# =============================================================================
# 4-QUARTER FORECAST OF STATE FINAL DEMAND (SFD), BY SECTOR
#   SFD = Household consumption (HFCE) + Private investment (private GFCF)
#         + Public final demand (government consumption + public investment)
# Seasonally adjusted chain volume measures, $m, quarterly 1985Q3-2026Q2
#
# TARGET VARIABLE (same as the Python notebook)
#   Every model forecasts quarterly growth, g_t = 100 x delta log(x_t), i.e.
#   100 x [log(x_t) - log(x_t-1)], approximately the % change on the quarter.
#   - Model A: each ARIMA is fitted to g_t itself (d = 0 imposed, differencing
#     done by the script). Series that can be zero or negative (discrepancy,
#     second-hand purchases, ...) cannot be logged and use the change in $m.
#   - Model B: the VECM equations already explain delta log of the block
#     variables (cointegration needs the log levels on the right-hand side).
#   - Growth forecasts are turned into $m only where adding up is needed
#     (MinT and the SFD sum), because Total = sum of parts holds in $m, not in
#     growth rates. Results are then reported back in 100 x delta log.
#   - Accuracy: RMSE of the growth error (actual - forecast 100 x delta log) and
#     RMSE of the log-level error 100 x (log actual - log forecast), which is
#     the error measure used in the Python backtest.
#   - The benchmark is the Python plain average: SFD grown at its own historical
#     average 100 x delta log (random walk with drift).
#
# WHAT THE SCRIPT DOES
#   Part 0  Setup: packages, file paths, horizon
#   Part 1  Read the three files (household, private, public)
#   Part 2  Shared tools: COVID dummies, MinT shrinkage, equation writers
#   Part 3  run_sector(): the full procedure for ONE sector
#             Stationarity: ADF + KPSS on every series, levels and differences
#             Benchmarks : historical mean and average growth of each variable
#             Model A    : ARIMA on Total + every component, reconciled with MinT
#             Model B    : VECM on the sector's cointegrated block + ARIMA for
#                          the other components, reconciled with MinT
#                          (VAR in differences if Johansen finds no cointegration)
#             Combination: average of A and B
#             Hold-out test on the last 4 quarters, final 4-quarter forecast,
#             ARIMA / VECM / Engle-Granger equations, MinT changes, diagnostics,
#             charts and an Excel workbook -- all labelled with the sector name
#   Part 4  Run the three sectors
#   Part 5  SFD: add the three sector forecasts for each quarter, hold-out
#           check against the ABS SFD, charts and an Excel workbook
# No machine learning models are used.
#
# Long-run (cointegrated) blocks used by Model B -- each passes Johansen and
# Engle-Granger at 5%:
#   Household: Essentials (food, rent, energy, health, insurance) and
#              Discretionary (clothing, furnishings, transport, recreation,
#              hotels/cafes/restaurants, alcohol)
#   Private  : Ownership transfer costs and household consumption (HFCE)
#              -- the housing-market / wealth effect. HFCE is fixed to the
#              household sector's forecast (conditional forecast), so the same
#              HFCE path is used everywhere.
#   Public   : State and local government consumption and investment
#              -- the state budget relation
# =============================================================================


# ---- PART 0. SETUP ----------------------------------------------------------

# Run once if a package is missing:
# install.packages(c("readxl","dplyr","tidyr","ggplot2","forecast","urca","vars","tseries","writexl"))

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forecast)
library(urca)
library(vars)
library(writexl)

# 'vars' loads MASS, whose select() breaks dplyr code: force the dplyr versions
select <- dplyr::select; filter <- dplyr::filter; mutate <- dplyr::mutate
rename <- dplyr::rename; relocate <- dplyr::relocate; transmute <- dplyr::transmute

set.seed(1)
H <- 4                                   # forecast horizon: 4 quarters

data_dir       <- Sys.getenv("SFD_DATA_DIR", ".")   # folder with the three input files (built by R/build_inputs.R)
household_path <- file.path(data_dir, "household.xlsx")
private_path   <- file.path(data_dir, "private.xlsx")
public_path    <- file.path(data_dir, "goverment.xlsx")       # government / public final demand

SFD_MODEL <- "Combination"               # model used for the headline SFD sum:
                                         # "MinT_A", "Model_B" or "Combination"

# TRUE: in the private VECM, household consumption (HFCE) is fixed to the
# household sector's forecast (same model as SFD_MODEL), so every part of the
# script uses the SAME HFCE path. Only transfer costs are forecast by the VECM,
# conditional on that path. FALSE: the private VECM forecasts HFCE itself.
PRIVATE_USES_HOUSEHOLD_FORECAST <- TRUE


# ---- PART 1. READ THE THREE FILES -------------------------------------------

# Find a column by a piece of its title (stops if not found exactly once)
find_in <- function(tbl, titles, pattern) {
  hit <- grep(pattern, titles, fixed = TRUE)
  if (length(hit) != 1) stop("Column not found exactly once: ", pattern)
  tbl[[hit]]
}

# --- 1a. Household consumption ------------------------------------------------
hh_raw <- read_excel(household_path, sheet = 1)
names(hh_raw)[1] <- "date"
hh_df <- hh_raw %>%
  mutate(date = as.Date(date),
         Transport = vehicles + `operation vehicules` + transport) %>%   # combine the 3
  relocate(Transport, .after = health) %>%
  select(-vehicles, -`operation vehicules`, -transport) %>%
  rename(any_of(c(
    Food = "food", Tobacco = "tobacco", Alcohol = "alcohol", Clothing = "cloting",
    Rent = "rent", Energy = "energy", Furnishings = "furnishing", Health = "health",
    Communications = "communication", Recreation = "recreation",
    Education = "education", Hospitality = "hospitality", Insurance = "insurance",
    Other = "other", Interstate = "expenditure interstate", Total = "final comsuption"
  )))
hh_df <- as.data.frame(hh_df)

# --- 1b. Private investment ---------------------------------------------------
pr_raw <- read_excel(private_path, sheet = 1)
pr_t   <- names(pr_raw)
# Non-dwelling construction is used as ONE component: its three parts are blank
# for 2012Q2-2022Q2 in the source (suppressed by the ABS).
pr_df <- data.frame(
  date            = as.Date(pr_raw[[1]]),
  Dwellings_new   = find_in(pr_raw, pr_t, "Dwellings - New and Used"),
  Dwellings_alter = find_in(pr_raw, pr_t, "Dwellings - Alterations and additions"),
  Transfer_costs  = find_in(pr_raw, pr_t, "Ownership transfer costs"),
  NonDwelling     = find_in(pr_raw, pr_t, "Non-dwelling construction - Total"),
  Machinery_new   = find_in(pr_raw, pr_t, "Machinery and equipment - New"),
  Machinery_2nd   = find_in(pr_raw, pr_t, "Machinery and equipment - Net purchase of second hand"),
  Biological      = find_in(pr_raw, pr_t, "Cultivated biological resources"),
  IP_products     = find_in(pr_raw, pr_t, "Intellectual property products"),
  Total           = find_in(pr_raw, pr_t, "Gross fixed capital formation: Chain"))

# --- 1c. Public final demand --------------------------------------------------
# Handles three layouts: ABS format without a date column, ABS format with a
# date column in column A, or a clean table (titles + dates + data).
pu_all <- read_excel(public_path, sheet = 1, col_names = FALSE, col_types = "text")
if (is.na(pu_all[[1]][1])) {                               # date column in column A
  date_num  <- suppressWarnings(as.numeric(pu_all[[1]]))
  data_rows <- which(!is.na(date_num))
  pu_dates  <- as.Date(date_num[data_rows], origin = "1899-12-30")
  pu_all    <- pu_all[, -1]
  pu_t      <- as.character(pu_all[1, ])
  pu_vals   <- as.data.frame(lapply(pu_all[data_rows, ], as.numeric))
} else {                                                   # dates from ABS metadata
  pu_t      <- as.character(pu_all[1, ])
  n_obs     <- as.numeric(pu_all[[1]][9])
  start_dt  <- as.Date(as.numeric(pu_all[[1]][7]), origin = "1899-12-30")
  pu_dates  <- seq(start_dt, by = "quarter", length.out = n_obs)
  pu_vals   <- as.data.frame(lapply(tail(pu_all, n_obs), as.numeric))
}
pu_df <- data.frame(
  date        = pu_dates,
  GG_Nat_FCE  = find_in(pu_vals, pu_t, "General government - National ;  Final consumption"),
  GG_SL_FCE   = find_in(pu_vals, pu_t, "General government - State and local ;  Final consumption"),
  PC_Cwth_INV = find_in(pu_vals, pu_t, "Public corporations - Commonwealth ;  Gross fixed"),
  PC_SL_INV   = find_in(pu_vals, pu_t, "Public corporations - State and local ;  Gross fixed"),
  GG_Nat_INV  = find_in(pu_vals, pu_t, "General government - National ;  Gross fixed"),
  GG_SL_INV   = find_in(pu_vals, pu_t, "General government - State and local ;  Gross fixed"),
  Total       = find_in(pu_vals, pu_t, "Public ;  Final demand"))
sfd_abs <- find_in(pu_vals, pu_t, "STATE FINAL DEMAND")    # official SFD, for checking
# GG = general government, PC = public corporations, Nat/Cwth = Commonwealth,
# SL = state and local, FCE = consumption, INV = investment (GFCF)

# One-off outlier: 1998Q1 State/local public-corporation investment is -$4.2bn,
# an asset sale (privatisation), not a fall in demand. Replace that quarter by
# the average of the quarters either side, in every public series.
out_idx <- which(pu_df$date == as.Date("1998-03-01"))
pu_outlier_tbl <- data.frame(date = pu_df$date[out_idx], series = names(pu_df)[-1],
                             original = unlist(pu_df[out_idx, -1]))
pu_df[, -1] <- lapply(pu_df[, -1], function(x) { x[out_idx] <- (x[out_idx - 1] + x[out_idx + 1]) / 2; x })
pu_outlier_tbl$replaced_by <- unlist(pu_df[out_idx, -1])

# --- 1d. Checks: no missing values, same quarters in all three files ----------
for (d in list(hh_df, pr_df, pu_df)) if (anyNA(d)) stop("Missing values in one of the files")
if (!identical(hh_df$date, pr_df$date) || !identical(hh_df$date, pu_df$date))
  stop("The three files do not cover exactly the same quarters")
gaps <- as.numeric(diff(hh_df$date))
if (any(gaps < 89 | gaps > 92)) stop("Dates are not consecutive quarters")


# ---- PART 2. SHARED TOOLS ---------------------------------------------------

dates <- hh_df$date
n     <- length(dates)
start_q <- c(as.numeric(format(dates[1], "%Y")), as.numeric(format(dates[1], "%m")) %/% 3)
to_ts   <- function(x) ts(x, start = start_q, frequency = 4)

# Target variable: quarterly growth, 100 x delta log (same as the Python notebook)
dlog      <- function(x) 100 * diff(log(x))                    # levels -> growth
dlog_path <- function(last, path) 100 * diff(log(c(last, path)))  # forecast levels -> growth path
from_dlog <- function(last, g) last * exp(cumsum(g) / 100)     # growth path -> levels

# COVID impulse dummies, 2020Q1-Q4 (quarters dated by their last month)
covid_dates <- as.Date(c("2020-03-01", "2020-06-01", "2020-09-01", "2020-12-01"))
D_covid <- sapply(covid_dates, function(d) as.numeric(dates == d))
colnames(D_covid) <- paste0("covid_", c("q1", "q2", "q3", "q4"))
active_cols <- function(X) X[, colSums(X != 0) > 0, drop = FALSE]

test_end   <- n - H                                    # hold-out: fit up to here
test_dates <- dates[(test_end + 1):n]
fc_dates   <- seq(dates[n], by = "quarter", length.out = H + 1)[-1]
fc_lab     <- format(fc_dates, "%Y-%m")

# Schafer-Strimmer shrinkage covariance (for MinT)
shrink_cov <- function(res) {
  res  <- res[complete.cases(res), , drop = FALSE]
  nn   <- nrow(res)
  covm <- crossprod(res) / nn
  tar  <- diag(diag(covm))
  corm <- cov2cor(covm)
  xs   <- scale(res, center = FALSE, scale = sqrt(diag(covm)))
  v    <- (1 / (nn * (nn - 1))) * (crossprod(xs^2) - (1 / nn) * (crossprod(xs))^2)
  diag(v) <- 0
  d    <- (corm - diag(nrow(corm)))^2
  lam  <- max(min(sum(v) / sum(d), 1), 0)
  W    <- lam * tar + (1 - lam) * covm
  attr(W, "lambda") <- lam
  W
}

# Equation writers
poly_txt <- function(coefs, sign, power) {
  if (length(coefs) == 0) return("")
  terms <- sapply(seq_along(coefs), function(i) {
    b <- if (power * i == 1) "B" else paste0("B^", power * i)
    sprintf(" %s %.4f%s", ifelse(sign * coefs[i] >= 0, "+", "-"), abs(coefs[i]), b)
  })
  paste0("(1", paste(terms, collapse = ""), ")")
}
arima_equation <- function(fit, name, tr = "dlog") {
  cf  <- coef(fit)
  ord <- c(arimaorder(fit), P = 0, D = 0, Q = 0)[c("p", "d", "q", "P", "D", "Q")]
  lhs <- if (tr == "dlog") paste0("100*dlog(", name, ")_t") else paste0("d(", name, ")_t")
  reg <- cf[names(cf) %in% c("intercept", "drift") | grepl("^covid", names(cf))]
  reg_txt <- sapply(names(reg), function(k) {
    v <- if (k == "drift") "t" else if (k == "intercept") "" else k
    sprintf("%s %.4f%s", ifelse(reg[k] >= 0, "+", "-"), abs(reg[k]), ifelse(v == "", "", paste0("*", v)))
  })
  eq1 <- paste(lhs, "=", sub("^\\+ ", "", paste(c(reg_txt, "+ eta_t"), collapse = " ")))
  ar  <- poly_txt(cf[grepl("^ar",  names(cf))], -1, 1)
  sar <- poly_txt(cf[grepl("^sar", names(cf))], -1, 4)
  ma  <- poly_txt(cf[grepl("^ma",  names(cf))],  1, 1)
  sma <- poly_txt(cf[grepl("^sma", names(cf))],  1, 4)
  dif <- if (ord["d"] > 0) paste0("(1 - B)", ifelse(ord["d"] > 1, paste0("^", ord["d"]), "")) else ""
  eq2 <- paste0(ar, sar, dif, " eta_t = ", ma, sma, if (ma == "" && sma == "") "" else " ", "e_t")
  data.frame(series = name,
             model = sprintf("ARIMA(%d,%d,%d)(%d,%d,%d)[4] %s", ord["p"], ord["d"], ord["q"],
                             ord["P"], ord["D"], ord["Q"], ifelse(tr == "dlog", "on 100*dlog (growth)", "on change in $m")),
             regression_equation = eq1, error_equation = trimws(eq2),
             sigma2 = signif(fit$sigma2, 4), AICc = round(fit$aicc, 1))
}
fmt_terms <- function(b, labels) {
  txt <- ifelse(labels == "", sprintf("%s %.4f", ifelse(b >= 0, "+", "-"), abs(b)),
                sprintf("%s %.4f*%s", ifelse(b >= 0, "+", "-"), abs(b), labels))
  sub("^- ", "-", sub("^\\+ ", "", paste(txt, collapse = " ")))
}
coef_tbl <- function(fit, part) {
  sm <- summary(fit)$coefficients
  data.frame(part = part, term = rownames(sm), estimate = round(sm[, 1], 5),
             std_error = round(sm[, 2], 5), t_value = round(sm[, 3], 2), p_value = round(sm[, 4], 4))
}
# MacKinnon (2010) Engle-Granger critical values (constant): b_inf + b1/T + b2/T^2
mackinnon <- list(
  `2` = rbind(`1pct` = c(-3.89644, -10.9519, -22.527), `5pct` = c(-3.33613, -6.1101, -6.823),
              `10pct` = c(-3.04445, -4.2412, -2.720)),
  `3` = rbind(`1pct` = c(-4.29374, -14.4354, -33.195), `5pct` = c(-3.74066, -8.5631, -10.852),
              `10pct` = c(-3.45218, -6.2143, -3.718)),
  `4` = rbind(`1pct` = c(-4.64332, -18.1031, -37.972), `5pct` = c(-4.09600, -11.2349, -11.175),
              `10pct` = c(-3.81020, -8.3931, -4.137)))

# --- Stationarity test for one series ------------------------------------------
# Series that are always positive are tested in logs, the others in levels.
# The COVID quarters are removed first (regression on the dummies), so the 2020
# outliers do not distort the tests. Two tests that point in opposite ways:
#   ADF  (urca::ur.df, lags by AIC): H0 = unit root (non-stationary)
#   KPSS (tseries::kpss.test)       : H0 = stationary
# Levels: constant + trend.  First differences: constant.  5% level.
stationarity_test <- function(x, name, sector, role = "component") {
  in_logs <- all(x > 0)
  y <- if (in_logs) log(x) else x
  covid_adjust <- function(v, X) as.numeric(residuals(lm(v ~ X)) + mean(v))
  one <- function(v, level) {
    a  <- ur.df(v, type = if (level) "trend" else "drift", lags = 8, selectlags = "AIC")
    tau <- as.numeric(a@teststat[1]); cv <- as.numeric(a@cval[1, "5pct"])
    k  <- suppressWarnings(tseries::kpss.test(v, null = if (level) "Trend" else "Level"))
    adf_stat <- tau < cv; kpss_stat <- k$p.value >= 0.05
    concl <- if (adf_stat && kpss_stat) "Stationary" else if (!adf_stat && !kpss_stat) "Non-stationary" else "Inconclusive"
    list(tau = tau, cv = cv, kpss = as.numeric(k$statistic), kpss_p = k$p.value, concl = concl)
  }
  L <- one(covid_adjust(y, D_covid), TRUE)
  D <- one(covid_adjust(diff(y), D_covid[-1, ]), FALSE)
  order <- if (L$concl == "Stationary") "I(0)" else if (D$concl == "Stationary") "I(1)" else
           if (D$concl == "Inconclusive") "I(1) likely" else "I(2)? / break"
  data.frame(sector = sector, series = name, role = role, scale = ifelse(in_logs, "log", "level"),
             level_ADF_tau = round(L$tau, 3), level_ADF_cv5 = L$cv, level_KPSS = round(L$kpss, 3),
             level_KPSS_p = round(L$kpss_p, 3), level_result = L$concl,
             diff_ADF_tau = round(D$tau, 3), diff_ADF_cv5 = D$cv, diff_KPSS = round(D$kpss, 3),
             diff_KPSS_p = round(D$kpss_p, 3), diff_result = D$concl, order_of_integration = order)
}

# --- "Average" benchmarks for one series ----------------------------------------
# Historical mean : forecast = average of all past values (flat line)
# Average growth  : forecast = last value grown at the series' average quarterly
#                   growth (log growth if always positive, else average change)
bench_average <- function(x, end) {
  y <- x[1:end]
  hist_mean <- rep(mean(y), H)
  avg_growth <- if (all(y > 0)) {
    g <- mean(diff(log(y))); y[end] * exp(g * (1:H))
  } else {
    y[end] + mean(diff(y)) * (1:H)
  }
  list(mean = hist_mean, growth = avg_growth)
}

pal <- c("Combination" = "#2a78d6", "Combination A+B" = "#2a78d6", "MinT (A)" = "#eb6834",
         "VECM-MinT (B)" = "#1baf7a", "Benchmark: average growth" = "#8a8984",
         "Plain average (SFD)" = "#7a4fc4",
         "Benchmark: historical mean" = "#b5b4ad")
sector_pal <- c("Household" = "#2a78d6", "Private" = "#eb6834", "Public" = "#1baf7a")
chart_theme <- theme_minimal(base_size = 11) +
  theme(legend.position = "top", panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))


# ---- PART 3. THE PROCEDURE FOR ONE SECTOR -----------------------------------
# cfg$name     : sector name used in titles, files and tables
# cfg$df       : date + components + Total
# cfg$groups   : Model B block -- list(VECM variable = components it adds up)
# cfg$external : other series in the VECM that are not components (or NULL)
# cfg$total_lab: description of the Total for chart titles
# cfg$cond     : optional fixed future path for a VECM variable (conditional
#                forecast): list(var = name, test = 4 values, final = 4 values), $m

run_sector <- function(cfg) {
  cat("\n\n#####################################################################\n")
  cat("#####  SECTOR:", cfg$name, "\n")
  cat("#####################################################################\n")
  df <- cfg$df
  categories <- setdiff(names(df), c("date", "Total"))
  # Chain volumes are not additive: keep the gap so Total = sum of parts exactly
  df$Discrepancy <- df$Total - rowSums(df[, categories])
  leaves  <- c(categories, "Discrepancy")
  all_ser <- c("Total", leaves)
  positive <- sapply(all_ser, function(s) all(df[[s]] > 0))   # logs if always > 0

  S_mat <- rbind(Total = rep(1, length(leaves)), diag(length(leaves)))
  dimnames(S_mat) <- list(all_ser, leaves)

  # --- Model A: ARIMA on every series + MinT
  # Each series is modelled in growth: 100 x delta log if it is always positive
  # (the Python target), or the change in $m if it can be <= 0. d = 0 and D = 0
  # are imposed because the script has already differenced the series, so the
  # ARMA is fitted to the growth rate itself. The COVID dummies are impulses in
  # the growth rate, i.e. the four 2020 quarters' growth is taken out of the
  # estimate of normal growth (as in the Python "ex-COVID" average).
  # The growth forecast is turned into $m (fc) only for MinT, because the
  # adding-up constraint holds in $m. Intervals for the $m path use the
  # variance of CUMULATIVE growth: sigma2 * sum_h (psi_0 + ... + psi_(h-j))^2.
  fit_arima <- function(s, end) {
    x   <- to_ts(df[[s]][1:end])
    pos <- positive[[s]]
    y   <- if (pos) 100 * diff(log(x)) else diff(x)          # target: growth
    X   <- active_cols(D_covid[2:end, , drop = FALSE])
    fit <- auto.arima(y, xreg = X, d = 0, D = 0)
    Xf  <- matrix(0, H, ncol(X), dimnames = list(NULL, colnames(X)))
    g   <- forecast(fit, xreg = Xf, h = H, level = c(80, 95))
    psi  <- c(1, ARMAtoMA(fit$model$phi, fit$model$theta, H - 1))
    se_c <- sqrt(fit$sigma2 * sapply(1:H, function(h) sum(cumsum(psi[1:h])^2)))
    cg   <- cumsum(as.numeric(g$mean))
    last <- df[[s]][end]
    to_lvl <- function(v) if (pos) last * exp(v / 100) else last + v
    z <- qnorm(c(0.90, 0.975))
    lower <- sapply(z, function(q) to_lvl(cg - q * se_c))
    upper <- sapply(z, function(q) to_lvl(cg + q * se_c))
    colnames(lower) <- colnames(upper) <- c("80%", "95%")
    prev <- df[[s]][1:(end - 1)]
    fit_g <- as.numeric(fitted(fit))
    fitted_level <- c(NA, if (pos) prev * exp(fit_g / 100) else prev + fit_g)
    list(fit = fit, growth = g, transform = if (pos) "dlog" else "diff",
         fitted_level = fitted_level,
         fc = list(mean = to_lvl(cg), lower = lower, upper = upper))
  }
  model_mint <- function(end) {
    fits <- lapply(all_ser, fit_arima, end = end); names(fits) <- all_ser
    yhat <- sapply(all_ser, function(s) as.numeric(fits[[s]]$fc$mean))
    res  <- sapply(all_ser, function(s) as.numeric(df[[s]][1:end] - fits[[s]]$fitted_level))
    W    <- shrink_cov(res[-(1:4), ])
    Winv <- solve(W)
    P    <- solve(t(S_mat) %*% Winv %*% S_mat) %*% t(S_mat) %*% Winv
    recon <- t(S_mat %*% P %*% t(yhat)); colnames(recon) <- all_ser
    list(total = recon[, "Total"], all = recon, base = yhat, P = P, fits = fits,
         lambda = attr(W, "lambda"))
  }

  # --- Model B: VECM on the cointegrated block
  vecm_data <- as.data.frame(lapply(cfg$groups, function(g) log(rowSums(df[, g, drop = FALSE]))))
  if (!is.null(cfg$external)) vecm_data <- cbind(vecm_data, log(cfg$external))

  # --- Stationarity tests: every series of the sector + the VECM block variables
  stat_tbl <- bind_rows(
    lapply(all_ser, function(s) stationarity_test(df[[s]], s, cfg$name,
                                                  role = if (s == "Total") "sector total" else "component")),
    lapply(names(vecm_data), function(v) stationarity_test(exp(vecm_data[[v]]), v, cfg$name, role = "VECM block")))
  cat("\n---", cfg$name, "stationarity (ADF + KPSS, 5%, COVID quarters removed) ---\n")
  print(stat_tbl %>% select(series, role, scale, level_result, diff_result, order_of_integration))

  # Conditional forecast: step the VECM/VAR forward one quarter at a time; the
  # conditioned variable is set to its given value, and the other variable is
  # adjusted for the conditioned variable's surprise using the residual
  # covariance (E[u_other | u_cond] = Sigma_oc / Sigma_cc * u_cond).
  cond_forecast <- function(vv, r, Y, end, Xf, cond_var, cond_log) {
    vn <- colnames(Y); K <- length(vn); ci <- match(cond_var, vn); oi <- setdiff(seq_len(K), ci)
    if (r > 0) {                                    # VECM as a VAR in log levels
      B <- vv$deterministic; for (i in 1:vv$p) B <- cbind(B, vv$A[[i]])
      Sig <- crossprod(vv$resid) / nrow(vv$resid)
      dm  <- vv$datamat; Zy <- dm[, !(colnames(dm) %in% colnames(vv$deterministic)), drop = FALSE]
      lasty <- c(Zy[nrow(Zy), ])
      out <- matrix(NA, H, K, dimnames = list(NULL, vn))
      for (h in 1:H) {
        f <- as.numeric(B %*% c(1, Xf[h, ], lasty[1:(K * vv$p)]))
        u_c <- cond_log[h] - f[ci]
        f[oi] <- f[oi] + Sig[oi, ci] / Sig[ci, ci] * u_c
        f[ci] <- cond_log[h]
        out[h, ] <- f; lasty <- c(f, lasty)
      }
      out
    } else {                                        # VAR in differences (growth)
      Bc  <- Bcoef(vv); Sig <- summary(vv)$covres; p <- vv$p
      dY  <- diff(Y); lastd <- c(t(dY[nrow(dY):(nrow(dY) - p + 1), , drop = FALSE]))
      cond_d <- diff(c(Y[end, ci], cond_log))
      lev <- Y[end, ]; out <- matrix(NA, H, K, dimnames = list(NULL, vn))
      for (h in 1:H) {
        z <- c(lastd[1:(K * p)], 1, Xf[h, ])
        f <- as.numeric(Bc %*% z)
        u_c <- cond_d[h] - f[ci]
        f[oi] <- f[oi] + Sig[oi, ci] / Sig[ci, ci] * u_c
        f[ci] <- cond_d[h]
        lev <- lev + f; out[h, ] <- lev; lastd <- c(f, lastd)
      }
      out
    }
  }

  model_vecm <- function(end, return_model = FALSE, cond = NULL) {
    Y <- as.matrix(vecm_data[1:end, ])
    X <- active_cols(D_covid[1:end, , drop = FALSE])
    K <- max(VARselect(Y, lag.max = 6, type = "const", exogen = X)$selection[["AIC(n)"]], 2)
    repeat {
      # ecdet = "none": unrestricted constant (the series trend in levels)
      jo <- ca.jo(Y, type = "trace", ecdet = "none", K = K, spec = "transitory", dumvar = X)
      cv <- rev(jo@cval[, "5pct"]); st <- rev(jo@teststat)
      r  <- min(sum(cumprod(st > cv)), ncol(Y) - 1)          # rank at 5% (can be 0)
      if (r > 0) {
        vv <- vec2var(jo, r = r)
      } else {                                               # no cointegration -> VAR in growth
        vv <- do.call(VAR, list(y = diff(Y), p = K - 1, type = "const", exogen = X[-1, , drop = FALSE]))
      }
      sp <- serial.test(vv, lags.pt = 12, type = "PT.asymptotic")$serial$p.value
      if (sp >= 0.05 || K >= 5) break
      K <- K + 1
    }
    Xf <- matrix(0, H, ncol(X), dimnames = list(NULL, colnames(X)))
    vn <- colnames(Y)
    fc <- lo95 <- hi95 <- matrix(NA, H, length(vn), dimnames = list(NULL, vn))
    if (r > 0) {
      pr <- predict(vv, n.ahead = H, ci = 0.95, dumvar = Xf)$fcst
      for (v in vn) { fc[, v] <- pr[[v]][, "fcst"]; lo95[, v] <- pr[[v]][, "lower"]; hi95[, v] <- pr[[v]][, "upper"] }
    } else {
      pr  <- predict(vv, n.ahead = H, dumvar = Xf)$fcst
      Ph  <- Phi(vv, nstep = H); Sig <- summary(vv)$covres
      for (j in seq_along(vn)) {
        v <- vn[j]
        fc[, v] <- Y[end, v] + cumsum(pr[[v]][, "fcst"])
        se <- sapply(1:H, function(h) {
          tot <- 0
          for (i in 0:(h - 1)) { Ci <- apply(Ph[, , 1:(i + 1), drop = FALSE], c(1, 2), sum)
                                 tot <- tot + (Ci %*% Sig %*% t(Ci))[j, j] }
          sqrt(tot)
        })
        lo95[, v] <- fc[, v] - qnorm(0.975) * se; hi95[, v] <- fc[, v] + qnorm(0.975) * se
      }
    }
    if (!is.null(cond)) {                            # conditional forecast
      fc <- cond_forecast(vv, r, Y, end, Xf, cond$var, log(cond$path))
      lo95[] <- NA; hi95[] <- NA                     # intervals not computed for conditional forecasts
    }
    out <- list(fc = exp(fc), lower95 = exp(lo95), upper95 = exp(hi95), K = K, r = r, serial_p = sp,
                type = paste0(if (r > 0) "VECM" else "VAR in differences (no cointegration)",
                              if (!is.null(cond)) paste0(", conditional on ", cond$var, " path") else ""))
    if (return_model) { out$jo <- jo; out$vv <- vv }
    out
  }

  # Model B: each block variable's VECM forecast replaces the ARIMA forecasts of
  # its components (scaled so the components add up to the VECM forecast); the
  # other components keep their ARIMA forecasts; then MinT with Model A weights.
  model_B <- function(mint, vecm) {
    yb <- mint$base
    for (g in names(cfg$groups)) {
      comp <- cfg$groups[[g]]
      yb[, comp] <- yb[, comp, drop = FALSE] * (vecm$fc[, g] / rowSums(yb[, comp, drop = FALSE]))
    }
    recon <- t(S_mat %*% mint$P %*% t(yb)); colnames(recon) <- all_ser
    list(total = recon[, "Total"], all = recon, base = yb)
  }

  # --- Hold-out test: fit up to 2025Q2, forecast the last 4 quarters
  actual    <- df$Total[(test_end + 1):n]
  test_mint <- model_mint(test_end)
  test_cond <- if (!is.null(cfg$cond)) list(var = cfg$cond$var, path = cfg$cond$test) else NULL
  fin_cond  <- if (!is.null(cfg$cond)) list(var = cfg$cond$var, path = cfg$cond$final) else NULL
  test_vecm <- model_vecm(test_end, cond = test_cond)
  test_B    <- model_B(test_mint, test_vecm)
  test_bench <- bench_average(df$Total, test_end)
  test_fc <- list(`MinT (A)`        = as.numeric(test_mint$total),
                  `VECM-MinT (B)`   = as.numeric(test_B$total),
                  `Combination A+B` = (as.numeric(test_mint$total) + as.numeric(test_B$total)) / 2,
                  `Benchmark: average growth` = test_bench$growth,
                  `Benchmark: historical mean` = test_bench$mean)
  models <- names(test_fc)
  holdout_tbl <- bind_rows(lapply(models, function(m) data.frame(
    sector = cfg$name, model = m, quarter = format(test_dates, "%Y-%m"), h = 1:H,
    actual = actual, forecast = test_fc[[m]]))) %>%
    group_by(model) %>%
    mutate(actual_dlog   = dlog(c(df$Total[test_end], actual)),            # 100 x delta log
           forecast_dlog = dlog_path(df$Total[test_end], forecast),
           dlog_error    = actual_dlog - forecast_dlog,                    # growth error, % points
           log_error     = 100 * (log(actual) - log(forecast))) %>%        # Python error measure
    ungroup() %>%
    mutate(error = actual - forecast, abs_error = abs(error), pct_error = 100 * error / actual)
  accuracy_tbl <- holdout_tbl %>% group_by(sector, model) %>%
    summarise(RMSE_dlog = sqrt(mean(dlog_error^2)), RMSE_logerr = sqrt(mean(log_error^2)),
              mean_dlog_bias = mean(dlog_error),
              RMSE = sqrt(mean(error^2)), MAE = mean(abs_error), MAPE = mean(abs(pct_error)),
              mean_error_bias = mean(error), actual_year = sum(actual), forecast_year = sum(forecast),
              .groups = "drop") %>%
    mutate(year_pct_error = 100 * (actual_year - forecast_year) / actual_year,
           ratio_vs_avg_growth = RMSE_logerr / RMSE_logerr[model == "Benchmark: average growth"]) %>%
    arrange(RMSE_logerr)
  cat("\n---", cfg$name, "hold-out accuracy (last 4 quarters) ---\n"); print(as.data.frame(accuracy_tbl))

  # --- Final forecast with all data
  final_mint <- model_mint(n)
  final_vecm <- model_vecm(n, return_model = TRUE, cond = fin_cond)
  final_B    <- model_B(final_mint, final_vecm)
  final_comb <- (as.numeric(final_mint$total) + as.numeric(final_B$total)) / 2
  last_actual <- df$Total[n]
  tfc <- final_mint$fits$Total$fc
  forecast_tbl <- data.frame(
    sector = cfg$name, quarter = fc_lab, date = fc_dates,
    MinT_A = as.numeric(final_mint$total), Model_B = as.numeric(final_B$total), Combination = final_comb,
    Bench_avg_growth = bench_average(df$Total, n)$growth, Bench_hist_mean = bench_average(df$Total, n)$mean,
    ARIMA_lower80 = as.numeric(tfc$lower[, "80%"]), ARIMA_upper80 = as.numeric(tfc$upper[, "80%"]),
    ARIMA_lower95 = as.numeric(tfc$lower[, "95%"]), ARIMA_upper95 = as.numeric(tfc$upper[, "95%"])) %>%
    mutate(MinT_A_dlog      = dlog_path(last_actual, MinT_A),           # 100 x delta log
           Model_B_dlog     = dlog_path(last_actual, Model_B),
           Combination_dlog = dlog_path(last_actual, Combination),
           Bench_dlog       = dlog_path(last_actual, Bench_avg_growth),
           ARIMA_Total_dlog = as.numeric(final_mint$fits$Total$growth$mean),   # before MinT
           ARIMA_Total_dlog_lo80 = as.numeric(final_mint$fits$Total$growth$lower[, "80%"]),
           ARIMA_Total_dlog_hi80 = as.numeric(final_mint$fits$Total$growth$upper[, "80%"]),
           Combination_qq_pct = 100 * (Combination / c(last_actual, head(Combination, -1)) - 1),
           Combination_yy_pct = 100 * (Combination / df$Total[(n - 3):n] - 1))
  cat("\n---", cfg$name, "final forecast, growth (100 x delta log) ---\n")
  print(forecast_tbl %>% select(quarter, MinT_A_dlog, Model_B_dlog, Combination_dlog, Bench_dlog) %>%
          mutate(across(-quarter, ~ round(.x, 2))))

  # Benchmarks for EVERY variable of the sector: hold-out (vs actual) and final
  bench_var_tbl <- bind_rows(lapply(all_ser, function(s) {
    bt <- bench_average(df[[s]], test_end); bf <- bench_average(df[[s]], n)
    act <- df[[s]][(test_end + 1):n]
    data.frame(sector = cfg$name, series = s, quarter = c(format(test_dates, "%Y-%m"), fc_lab),
               period = rep(c("hold-out", "forecast"), each = H),
               actual = c(act, rep(NA, H)),
               bench_avg_growth = c(bt$growth, bf$growth), bench_hist_mean = c(bt$mean, bf$mean),
               MinT = c(test_mint$all[, s], final_mint$all[, s]))
  }))
  bench_var_acc <- bench_var_tbl %>% filter(period == "hold-out") %>% group_by(sector, series) %>%
    summarise(RMSE_MinT = sqrt(mean((actual - MinT)^2)),
              RMSE_avg_growth = sqrt(mean((actual - bench_avg_growth)^2)),
              RMSE_hist_mean = sqrt(mean((actual - bench_hist_mean)^2)), .groups = "drop") %>%
    mutate(MinT_beats_avg_growth = RMSE_MinT < RMSE_avg_growth,
           MinT_beats_hist_mean = RMSE_MinT < RMSE_hist_mean)

  block_tbl <- bind_rows(lapply(colnames(final_vecm$fc), function(v) data.frame(
    sector = cfg$name, quarter = fc_lab, block_variable = v,
    VECM = round(final_vecm$fc[, v], 1),
    ARIMA_sum = if (v %in% names(cfg$groups)) round(rowSums(final_mint$base[, cfg$groups[[v]], drop = FALSE]), 1) else NA,
    VECM_lower95 = round(final_vecm$lower95[, v], 1), VECM_upper95 = round(final_vecm$upper95[, v], 1))))

  # --- ARIMA equations
  arima_eq_tbl <- bind_rows(lapply(all_ser, function(s) arima_equation(final_mint$fits[[s]]$fit, s,
                                                                       final_mint$fits[[s]]$transform)))
  arima_coef_tbl <- bind_rows(lapply(all_ser, function(s) {
    f <- final_mint$fits[[s]]$fit; se <- sqrt(diag(f$var.coef))
    data.frame(series = s, term = names(coef(f)), estimate = round(coef(f), 5),
               std_error = round(se[names(coef(f))], 5), t_value = round(coef(f) / se[names(coef(f))], 2))
  })); rownames(arima_coef_tbl) <- NULL
  cat("\n---", cfg$name, "ARIMA equation for the Total ---\n",
      arima_eq_tbl$model[1], "\n", arima_eq_tbl$regression_equation[1], "\n", arima_eq_tbl$error_equation[1], "\n")

  # --- Model B equations (VECM, or VAR in differences)
  if (final_vecm$r == 0) {
    vecm_coef_tbl <- bind_rows(lapply(names(final_vecm$vv$varresult), function(eqn) {
      sm <- summary(final_vecm$vv$varresult[[eqn]])$coefficients
      data.frame(equation = eqn, term = rownames(sm), estimate = round(sm[, 1], 5),
                 std_error = round(sm[, 2], 5), t_value = round(sm[, 3], 2), p_value = round(sm[, 4], 4))
    }))
    eq_txt <- sapply(names(final_vecm$vv$varresult), function(eqn) {
      b <- coef(final_vecm$vv$varresult[[eqn]])
      lab <- sub("^(.*)\\.l(\\d)$", "d\\1_{t-\\2}", names(b)); lab[lab == "const"] <- ""
      paste0("d", eqn, "_t = ", fmt_terms(b, lab), " + u_t")
    })
    vecm_eq_tbl <- data.frame(part = paste0("VAR equation: d", names(eq_txt)), equation = eq_txt)
  } else {
    vecm_ls <- cajorls(final_vecm$jo, r = final_vecm$r)
    beta <- vecm_ls$beta
    ect_txt <- sapply(seq_len(ncol(beta)), function(j) {
      b <- beta[, j]; b[abs(b) < 1e-10] <- 0
      lab <- sub("\\.l1$", "_{t-1}", rownames(beta))
      paste0("ECT", j, "_{t-1} = ", fmt_terms(b[b != 0], lab[b != 0]))
    })
    vc <- coef(vecm_ls$rlm)
    eq_txt <- sapply(colnames(vc), function(eqn) {
      lab <- rownames(vc)
      lab <- sub("^ect(\\d)$", "ECT\\1_{t-1}", lab)
      lab <- sub("^(.*)\\.dl(\\d)$", "d\\1_{t-\\2}", lab)
      lab[lab == "constant"] <- ""
      paste0("d", sub("\\.d$", "", eqn), "_t = ", fmt_terms(vc[, eqn], lab), " + u_t")
    })
    vecm_coef_tbl <- bind_rows(lapply(colnames(vc), function(eqn) {
      sm <- summary(vecm_ls$rlm)[[paste("Response", eqn)]]$coefficients
      data.frame(equation = eqn, term = rownames(sm), estimate = round(sm[, 1], 5),
                 std_error = round(sm[, 2], 5), t_value = round(sm[, 3], 2), p_value = round(sm[, 4], 4))
    }))
    vecm_eq_tbl <- data.frame(part = c(paste0("Cointegrating relation ", seq_along(ect_txt)),
                                       paste0("VECM equation: d", sub("\\.d$", "", colnames(vc)))),
                              equation = c(ect_txt, eq_txt))
  }
  rownames(vecm_coef_tbl) <- NULL
  cat("\n---", cfg$name, "Model B:", final_vecm$type, "| K =", final_vecm$K, "| rank r =", final_vecm$r, "---\n")
  cat(paste0("  ", vecm_eq_tbl$equation, "\n"), sep = "")

  # --- Engle-Granger two-step test on the block
  eg_y <- names(vecm_data)[1]; eg_x <- names(vecm_data)[-1]
  lr_fit <- lm(as.formula(paste(eg_y, "~", paste(c(eg_x, colnames(D_covid)), collapse = " + "))),
               data = data.frame(vecm_data, D_covid))
  z_eg   <- as.numeric(residuals(lr_fit))
  eg_adf <- ur.df(z_eg, type = "none", lags = 8, selectlags = "AIC")
  eg_tau <- as.numeric(eg_adf@teststat[1])
  mk     <- mackinnon[[as.character(ncol(vecm_data))]]
  eg_cv  <- mk[, 1] + mk[, 2] / length(z_eg) + mk[, 3] / length(z_eg)^2
  t_idx  <- 3:n
  d_all  <- lapply(vecm_data, diff)
  ecm_df <- data.frame(dy = d_all[[eg_y]][t_idx - 1], z_l1 = z_eg[t_idx - 1])
  for (v in names(vecm_data)) ecm_df[[paste0("d", v, "_l1")]] <- d_all[[v]][t_idx - 2]
  ecm_df  <- cbind(ecm_df, D_covid[t_idx, , drop = FALSE])
  ecm_fit <- lm(dy ~ ., data = ecm_df)
  lr_b <- coef(lr_fit); ecm_b <- coef(ecm_fit)
  lr_lab  <- ifelse(names(lr_b) == "(Intercept)", "", paste0(names(lr_b), "_t"))
  ecm_lab <- names(ecm_b)
  ecm_lab <- ifelse(ecm_lab == "(Intercept)", "", ecm_lab)
  ecm_lab <- sub("^z_l1$", "z_{t-1}", ecm_lab); ecm_lab <- sub("^d(.*)_l1$", "d\\1_{t-1}", ecm_lab)
  eg_eq_tbl <- data.frame(part = c("Engle-Granger long-run relation", "Engle-Granger error-correction model"),
                          equation = c(paste0(eg_y, "_t = ", fmt_terms(lr_b, lr_lab), " + z_t"),
                                       paste0("d", eg_y, "_t = ", fmt_terms(ecm_b, ecm_lab), " + e_t")))
  eg_test_tbl <- data.frame(
    item = c("Block variables", "Residual ADF tau", "EG 1% critical value", "EG 5% critical value",
             "EG 10% critical value", "Cointegrated at 5% (Engle-Granger)?", "ECM alpha", "ECM alpha t-value",
             "Johansen rank at 5%", "Model B type"),
    value = c(paste(names(vecm_data), collapse = ", "), round(eg_tau, 3), round(eg_cv["1pct"], 3),
              round(eg_cv["5pct"], 3), round(eg_cv["10pct"], 3), ifelse(eg_tau < eg_cv["5pct"], "Yes", "No"),
              round(ecm_b["z_l1"], 4), round(summary(ecm_fit)$coefficients["z_l1", 3], 2),
              final_vecm$r, final_vecm$type))
  eg_coef_tbl <- rbind(coef_tbl(lr_fit, "EG long-run regression"), coef_tbl(ecm_fit, "EG error-correction model"))
  rownames(eg_coef_tbl) <- NULL
  cat(sprintf("Engle-Granger: tau = %.2f vs 5%% cv %.2f -> %s\n", eg_tau, eg_cv["5pct"],
              ifelse(eg_tau < eg_cv["5pct"], "cointegrated", "not cointegrated")))

  # --- MinT changes and coherence
  mint_change_tbl <- bind_rows(lapply(seq_len(H), function(i) data.frame(
    quarter = fc_lab[i], series = all_ser, ARIMA_base = final_mint$base[i, ], MinT = final_mint$all[i, ],
    change = final_mint$all[i, ] - final_mint$base[i, ]))) %>%
    mutate(change_pct = round(100 * change / abs(ARIMA_base), 2), across(c(ARIMA_base, MinT, change), ~ round(.x, 1)))
  rownames(mint_change_tbl) <- NULL
  coherence_tbl <- data.frame(
    quarter = fc_lab,
    ARIMA_Total = round(final_mint$base[, "Total"], 1),
    ARIMA_sum_of_parts = round(rowSums(final_mint$base[, leaves]), 1),
    MinT_Total = round(final_mint$all[, "Total"], 1),
    MinT_sum_of_parts = round(rowSums(final_mint$all[, leaves]), 1))

  # --- Diagnostics
  fit_total <- final_mint$fits$Total$fit
  lb <- Box.test(residuals(fit_total), lag = 8, type = "Ljung-Box",
                 fitdf = sum(grepl("^(ar|ma|sar|sma)", names(coef(fit_total)))))
  st <- serial.test(final_vecm$vv, lags.pt = 12, type = "PT.asymptotic")
  diag_tbl <- data.frame(
    check = c("ARIMA(Total) model", "ARIMA(Total) Ljung-Box p (lag 8)", "MinT shrinkage lambda",
              "Model B type", "Model B lag order K", "Cointegration rank r (Johansen, 5%)",
              "Model B Portmanteau p (12 lags)"),
    value = c(arima_eq_tbl$model[1], round(lb$p.value, 3), round(final_mint$lambda, 3),
              final_vecm$type, final_vecm$K, final_vecm$r, round(st$serial$p.value, 3)))

  # --- Charts (titles and file names carry the sector name)
  fname <- tolower(cfg$name)

  # (a) every ARIMA forecast, with the MinT forecast
  arima_hist <- bind_rows(lapply(all_ser, function(s) data.frame(series = s, date = dates, value = df[[s]]))) %>%
    filter(date >= as.Date("2015-01-01"))
  arima_fc <- bind_rows(lapply(all_ser, function(s) {
    f <- final_mint$fits[[s]]$fc
    data.frame(series = s, date = fc_dates, ARIMA = as.numeric(f$mean), MinT = final_mint$all[, s],
               lo80 = as.numeric(f$lower[, "80%"]), hi80 = as.numeric(f$upper[, "80%"]),
               lo95 = as.numeric(f$lower[, "95%"]), hi95 = as.numeric(f$upper[, "95%"]))
  }))
  join <- data.frame(series = all_ser, date = dates[n], value = sapply(all_ser, function(s) df[[s]][n]))
  fc_long_a <- arima_fc %>% select(series, date, ARIMA, MinT) %>%
    pivot_longer(c(ARIMA, MinT), names_to = "forecast", values_to = "value") %>%
    bind_rows(mutate(join, forecast = "ARIMA"), mutate(join, forecast = "MinT"))
  band_a <- bind_rows(arima_fc %>% select(series, date, lo80, hi80, lo95, hi95),
                      data.frame(series = all_ser, date = dates[n], lo80 = join$value, hi80 = join$value,
                                 lo95 = join$value, hi95 = join$value))
  for (nm in c("arima_hist", "fc_long_a", "band_a")) {
    tmp <- get(nm); tmp$series <- factor(tmp$series, levels = all_ser); assign(nm, tmp)
  }
  p_arima <- ggplot() +
    geom_ribbon(data = band_a, aes(date, ymin = lo95, ymax = hi95), fill = "#cde2fb") +
    geom_ribbon(data = band_a, aes(date, ymin = lo80, ymax = hi80), fill = "#9ec5f4") +
    geom_line(data = arima_hist, aes(date, value), colour = "#0b0b0b", linewidth = 0.5) +
    geom_line(data = fc_long_a, aes(date, value, colour = forecast), linewidth = 0.7) +
    geom_point(data = filter(fc_long_a, date > dates[n]), aes(date, value, colour = forecast), size = 1) +
    scale_colour_manual(values = c(ARIMA = "#2a78d6", MinT = "#eb6834"), name = NULL) +
    facet_wrap(~ series, ncol = 3, scales = "free_y") +
    labs(title = paste0(cfg$name, " sector: ARIMA forecast of every component, and after MinT"),
         subtitle = "$m, chain volume, seasonally adjusted. Blue = ARIMA with 80/95% intervals; orange = after MinT.",
         x = NULL, y = NULL) +
    theme_minimal(base_size = 9) +
    theme(legend.position = "top", panel.grid.minor = element_blank(),
          strip.text = element_text(face = "bold", hjust = 0), plot.title = element_text(face = "bold", size = 13))
  ggsave(paste0(fname, "_arima_forecasts.png"), p_arima, width = 12,
         height = 2.2 * ceiling(length(all_ser) / 3) + 1.5, dpi = 140, bg = "white")

  # (b) final forecast of the sector Total
  hist_df <- data.frame(date = dates, value = df$Total) %>% filter(date >= as.Date("2015-01-01"))
  fc_long <- forecast_tbl %>% select(date, `MinT (A)` = MinT_A, `VECM-MinT (B)` = Model_B, Combination) %>%
    pivot_longer(-date, names_to = "model", values_to = "value")
  fc_long <- bind_rows(fc_long, data.frame(date = dates[n], model = unique(fc_long$model), value = last_actual))
  band <- data.frame(date = c(dates[n], fc_dates),
                     lo80 = c(last_actual, forecast_tbl$ARIMA_lower80), hi80 = c(last_actual, forecast_tbl$ARIMA_upper80),
                     lo95 = c(last_actual, forecast_tbl$ARIMA_lower95), hi95 = c(last_actual, forecast_tbl$ARIMA_upper95))
  p_fc <- ggplot() +
    geom_ribbon(data = band, aes(date, ymin = lo95, ymax = hi95), fill = "#cde2fb") +
    geom_ribbon(data = band, aes(date, ymin = lo80, ymax = hi80), fill = "#9ec5f4") +
    geom_line(data = hist_df, aes(date, value), colour = "#0b0b0b", linewidth = 0.7) +
    geom_line(data = fc_long, aes(date, value, colour = model), linewidth = 0.8) +
    geom_point(data = filter(fc_long, date > dates[n]), aes(date, value, colour = model), size = 1.6) +
    scale_colour_manual(values = pal, name = NULL) +
    labs(title = paste0(cfg$name, " sector: ", cfg$total_lab, " -- 4-quarter forecast"),
         subtitle = "$m, chain volume, seasonally adjusted. Shaded: ARIMA 80% and 95% intervals for the Total.",
         x = NULL, y = "$m") + chart_theme
  ggsave(paste0(fname, "_forecast.png"), p_fc, width = 10, height = 6, dpi = 150, bg = "white")

  # (c) hold-out test
  test_hist <- data.frame(date = dates, value = df$Total) %>% filter(date >= as.Date("2022-01-01"))
  plot_models <- setdiff(models, "Benchmark: historical mean")   # mean is far below: shown in tables only
  test_long <- bind_rows(holdout_tbl %>% filter(model %in% plot_models) %>%
                           transmute(date = test_dates[h], model, value = forecast),
                         data.frame(date = dates[test_end], model = plot_models, value = df$Total[test_end]))
  p_test <- ggplot() +
    annotate("rect", xmin = dates[test_end], xmax = dates[n], ymin = -Inf, ymax = Inf, fill = "grey93") +
    geom_line(data = test_hist, aes(date, value), colour = "#0b0b0b", linewidth = 0.8) +
    geom_point(data = filter(test_hist, date > dates[test_end]), aes(date, value), colour = "#0b0b0b", size = 2) +
    geom_line(data = test_long, aes(date, value, colour = model), linewidth = 0.8) +
    geom_point(data = filter(test_long, date > dates[test_end]), aes(date, value, colour = model), size = 1.8) +
    scale_colour_manual(values = pal, name = NULL) +
    labs(title = paste0(cfg$name, " sector: hold-out test, last 4 quarters vs actual"),
         subtitle = "Models fitted to data up to the June 2025 quarter. Black = actual. Grey = average-growth benchmark.",
         x = NULL, y = "$m") + chart_theme
  ggsave(paste0(fname, "_holdout.png"), p_test, width = 9, height = 5.5, dpi = 150, bg = "white")

  # --- Excel workbook for the sector
  round_df <- function(d) { d[] <- lapply(d, function(x) if (is.numeric(x)) round(x, 2) else x); d }
  sheets <- list(`Stationarity` = stat_tbl,
                 `Forecast` = round_df(forecast_tbl %>% select(-date)),
                 `Hold-out` = round_df(holdout_tbl), `Hold-out accuracy` = round_df(accuracy_tbl),
                 `MinT changes` = mint_change_tbl, `MinT coherence` = coherence_tbl,
                 `VECM block forecast` = block_tbl,
                 `ARIMA equations` = arima_eq_tbl, `ARIMA coefficients` = arima_coef_tbl,
                 `Model B equations` = vecm_eq_tbl, `Model B coefficients` = vecm_coef_tbl,
                 `Engle-Granger test` = eg_test_tbl, `Engle-Granger eqns` = eg_eq_tbl,
                 `Engle-Granger coefs` = eg_coef_tbl, Diagnostics = diag_tbl,
                 `Benchmark by variable` = round_df(bench_var_tbl),
                 `Benchmark accuracy` = round_df(bench_var_acc))
  if (!is.null(cfg$extra_sheets)) sheets <- c(sheets, cfg$extra_sheets)
  write_xlsx(sheets, paste0("forecast_results_", fname, ".xlsx"))

  list(name = cfg$name, df = df, forecast = forecast_tbl, holdout = holdout_tbl, test_fc = test_fc,
       accuracy = accuracy_tbl, diag = diag_tbl, eg = eg_test_tbl,
       stationarity = stat_tbl, bench_acc = bench_var_acc,
       plots = list(arima = p_arima, forecast = p_fc, holdout = p_test))
}


# ---- PART 4. RUN THE THREE SECTORS ------------------------------------------

# Household first: its forecast of HFCE is used by the private sector
hh_res <- run_sector(list(
  name = "Household", total_lab = "household final consumption expenditure",
  df = hh_df,
  groups = list(Essential     = c("Food", "Rent", "Energy", "Health", "Insurance"),
                Discretionary = c("Clothing", "Furnishings", "Transport", "Recreation",
                                  "Hospitality", "Alcohol")),
  external = NULL))

# HFCE path for the private VECM = household forecast from the SFD_MODEL model
holdout_name <- c(MinT_A = "MinT (A)", Model_B = "VECM-MinT (B)", Combination = "Combination A+B")[[SFD_MODEL]]
hfce_cond <- if (PRIVATE_USES_HOUSEHOLD_FORECAST) {
  list(var   = "HFCE",
       test  = hh_res$test_fc[[holdout_name]],          # household hold-out forecast
       final = hh_res$forecast[[SFD_MODEL]])            # household final forecast
} else NULL

pr_res <- run_sector(list(
  name = "Private", total_lab = "private investment (private GFCF)",
  df = pr_df,
  groups = list(Transfer_costs = "Transfer_costs"),
  external = data.frame(HFCE = hh_df$Total),            # housing / wealth effect
  cond = hfce_cond))

pu_res <- run_sector(list(
  name = "Public", total_lab = "public final demand",
  df = pu_df,
  groups = list(GG_SL_FCE = "GG_SL_FCE", GG_SL_INV = "GG_SL_INV"),
  external = NULL,
  extra_sheets = list(`Outlier 1998Q1` = pu_outlier_tbl)))

sectors <- list(Household = hh_res, Private = pr_res, Public = pu_res)

# Check: the HFCE used by the private VECM equals the household forecast
if (PRIVATE_USES_HOUSEHOLD_FORECAST) {
  cat("\nHFCE path used in the private VECM vs household forecast (", SFD_MODEL, "):\n")
  print(data.frame(quarter = fc_lab, household_forecast = round(hh_res$forecast[[SFD_MODEL]], 1),
                   used_in_private_VECM = round(hfce_cond$final, 1)))
}

# Show the sector charts in RStudio
for (s in sectors) { print(s$plots$arima); print(s$plots$forecast); print(s$plots$holdout) }


# ---- PART 5. STATE FINAL DEMAND = SUM OF THE THREE SECTORS -------------------

# --- 5a. Forecast for each quarter: every sector and the SFD sum, every model
sector_fc <- bind_rows(lapply(sectors, function(s) s$forecast)) %>%
  select(sector, quarter, date, MinT_A, Model_B, Combination, Bench_avg_growth, Bench_hist_mean)
# The three sector totals do not add up exactly to the ABS SFD (chain volumes are
# not additive, see the check below). So the SUM of the sector forecasts is
# turned into a growth path (100 x delta log, from the sum's own last value) and
# that growth is applied to the ABS SFD level. This removes the level gap, which
# would otherwise appear as fake growth in the first forecast quarter.
sum_actual   <- hh_df$Total + pr_df$Total + pu_df$Total      # sum of the sector totals, $m
last_sfd_abs <- sfd_abs[n]
fc_models    <- c("MinT_A", "Model_B", "Combination", "Bench_avg_growth")
sfd_fc <- sector_fc %>% group_by(quarter, date) %>%
  summarise(across(c(all_of(fc_models), Bench_hist_mean), sum), .groups = "drop") %>%
  arrange(date)
for (m in fc_models) {
  sfd_fc[[paste0(m, "_dlog")]] <- dlog_path(sum_actual[n], sfd_fc[[m]])          # 100 x delta log
  sfd_fc[[m]] <- from_dlog(last_sfd_abs, sfd_fc[[paste0(m, "_dlog")]])            # on the ABS level
}
sfd_fc <- sfd_fc %>%
  mutate(Plain_average_SFD_dlog = mean(dlog(sfd_abs)),               # the Python benchmark
         Plain_average_SFD = from_dlog(last_sfd_abs, Plain_average_SFD_dlog),
         sector = "SFD (sum)")
sfd_table <- bind_rows(sector_fc, sfd_fc) %>% arrange(date, factor(sector, levels = c(names(sectors), "SFD (sum)")))

# Wide table for the headline model: one column per sector + SFD + growth
sfd_wide <- sector_fc %>% select(sector, quarter, value = all_of(SFD_MODEL)) %>%
  pivot_wider(names_from = sector, values_from = value) %>%
  mutate(SFD_sum = Household + Private + Public,
         SFD_dlog = dlog_path(sum_actual[n], SFD_sum),               # 100 x delta log of SFD
         SFD = from_dlog(last_sfd_abs, SFD_dlog),                    # on the ABS SFD level
         SFD_qq_pct = 100 * (exp(SFD_dlog / 100) - 1),
         SFD_yy_pct = 100 * (SFD / sfd_abs[(n - 3):n] - 1),
         Household_share_pct = 100 * Household / SFD_sum,
         Private_share_pct = 100 * Private / SFD_sum,
         Public_share_pct = 100 * Public / SFD_sum)
# Contribution of each sector to SFD growth (% points): change in the sector / last quarter's sum
contrib <- sector_fc %>% select(sector, quarter, value = all_of(SFD_MODEL)) %>%
  group_by(sector) %>% arrange(quarter, .by_group = TRUE) %>%
  mutate(last = sectors[[first(sector)]]$df$Total[n],
         change = value - c(first(last), head(value, -1))) %>% ungroup() %>%
  left_join(sfd_wide %>% transmute(quarter, prev_sum = c(sum_actual[n], head(SFD_sum, -1))), by = "quarter") %>%
  mutate(contribution_pp = 100 * change / prev_sum) %>%
  select(quarter, sector, contribution_pp) %>%
  pivot_wider(names_from = sector, values_from = contribution_pp, names_glue = "{sector}_contrib_pp")
sfd_wide <- left_join(sfd_wide, contrib, by = "quarter")
sfd_wide[, -1] <- round(sfd_wide[, -1], 2)
cat("\n\n=========== STATE FINAL DEMAND forecast (", SFD_MODEL, ") ===========\n")
cat("SFD_dlog = 100 x delta log (quarterly growth); *_contrib_pp = sector contribution to it\n")
print(as.data.frame(sfd_wide))

# How well do the three sector totals add up to the official ABS SFD?
add_check <- data.frame(date = dates, ABS_SFD = sfd_abs,
                        Sum_of_sectors = sum_actual) %>%
  mutate(gap = ABS_SFD - Sum_of_sectors, gap_pct = 100 * gap / ABS_SFD)
cat("\nSum of sector totals vs ABS SFD, last 4 quarters:\n"); print(tail(add_check, 4))

# Stationarity of the official SFD, and all stationarity results together
all_stationarity <- bind_rows(c(unname(lapply(sectors, function(s) s$stationarity)),
                                list(stationarity_test(sfd_abs, "ABS State Final Demand", "SFD", role = "SFD total"))))
cat("\nStationarity summary (order of integration):\n")
print(as.data.frame(all_stationarity %>% select(sector, series, role, order_of_integration)))

# --- 5b. Hold-out test for SFD: sum of the sector hold-out forecasts vs ABS SFD
# Same treatment as the final forecast: sum of sectors -> growth path -> ABS level.
# The Python plain average (ABS SFD grown at its own mean 100 x delta log up to
# the hold-out start) is added as the benchmark every model is judged against.
sfd_holdout <- bind_rows(lapply(sectors, function(s) s$holdout)) %>%
  group_by(model, quarter, h) %>% summarise(forecast_sum = sum(forecast), .groups = "drop") %>%
  group_by(model) %>% arrange(h, .by_group = TRUE) %>%
  mutate(forecast_dlog = dlog_path(sum_actual[test_end], forecast_sum),
         forecast = from_dlog(sfd_abs[test_end], forecast_dlog)) %>% ungroup()
g_bar <- mean(dlog(sfd_abs[1:test_end]))
sfd_holdout <- bind_rows(sfd_holdout, data.frame(
    model = "Plain average (SFD)", quarter = format(test_dates, "%Y-%m"), h = 1:H,
    forecast_sum = NA, forecast_dlog = rep(g_bar, H), forecast = from_dlog(sfd_abs[test_end], rep(g_bar, H)))) %>%
  mutate(actual = sfd_abs[test_end + h],
         actual_dlog = 100 * (log(actual) - log(sfd_abs[test_end + h - 1])),
         dlog_error = actual_dlog - forecast_dlog,                    # growth error, % points
         log_error = 100 * (log(actual) - log(forecast)),             # Python error measure
         error = actual - forecast, pct_error = 100 * error / actual)
sfd_accuracy <- sfd_holdout %>% group_by(model) %>%
  summarise(RMSE_dlog = sqrt(mean(dlog_error^2)), RMSE_logerr = sqrt(mean(log_error^2)),
            mean_dlog_bias = mean(dlog_error),
            RMSE = sqrt(mean(error^2)), MAE = mean(abs(error)), MAPE = mean(abs(pct_error)),
            year_pct_error = 100 * (sum(actual) - sum(forecast)) / sum(actual), .groups = "drop") %>%
  mutate(ratio_vs_plain_average = RMSE_logerr / RMSE_logerr[model == "Plain average (SFD)"]) %>%
  arrange(RMSE_logerr)
# (unname + list so newer dplyr versions bind the rows instead of making list-columns)
all_accuracy <- bind_rows(c(unname(lapply(sectors, function(s) s$accuracy)),
                            list(sfd_accuracy %>% mutate(sector = "SFD (sum)"))))
cat("\nSFD hold-out accuracy (growth errors in % points; ratio < 1 beats the plain average):\n")
print(as.data.frame(sfd_accuracy))
cat("Note: 4 quarters only -- a sanity check, not a test. The Python backtest (78 origins) decides.\n")

# --- 5c. Charts
# (a) SFD history and forecast, by model
sfd_hist <- data.frame(date = dates, value = sfd_abs) %>% filter(date >= as.Date("2015-01-01"))
sfd_long <- sfd_fc %>% select(date, `MinT (A)` = MinT_A, `VECM-MinT (B)` = Model_B, Combination) %>%
  pivot_longer(-date, names_to = "model", values_to = "value") %>%
  bind_rows(data.frame(date = dates[n], model = c("MinT (A)", "VECM-MinT (B)", "Combination"), value = last_sfd_abs))
p_sfd <- ggplot() +
  geom_line(data = sfd_hist, aes(date, value), colour = "#0b0b0b", linewidth = 0.7) +
  geom_line(data = sfd_long, aes(date, value, colour = model), linewidth = 0.8) +
  geom_point(data = filter(sfd_long, date > dates[n]), aes(date, value, colour = model), size = 1.6) +
  scale_colour_manual(values = pal, name = NULL) +
  labs(title = "State Final Demand: 4-quarter forecast (sum of household, private and public)",
       subtitle = "$m, chain volume, seasonally adjusted. Black = ABS State Final Demand.",
       x = NULL, y = "$m") + chart_theme
print(p_sfd)
ggsave("sfd_forecast.png", p_sfd, width = 10, height = 6, dpi = 150, bg = "white")

# (b) Each forecast quarter split by sector (stacked bars, headline model)
stack_df <- sector_fc %>% select(sector, quarter, value = all_of(SFD_MODEL)) %>%
  mutate(sector = factor(sector, levels = c("Public", "Private", "Household")))
p_stack <- ggplot(stack_df, aes(quarter, value, fill = sector)) +
  geom_col(width = 0.6, colour = "white", linewidth = 0.5) +
  geom_text(aes(label = format(round(value), big.mark = ",")), position = position_stack(vjust = 0.5),
            colour = "white", size = 3.3) +
  geom_text(data = sfd_wide, aes(quarter, SFD_sum, label = paste0("Sum ", format(round(SFD_sum), big.mark = ","))),
            inherit.aes = FALSE, vjust = -0.6, size = 3.5, fontface = "bold") +
  scale_fill_manual(values = sector_pal, name = NULL, breaks = c("Household", "Private", "Public")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(title = "State Final Demand forecast by sector, each quarter",
       subtitle = paste0("$m, chain volume, seasonally adjusted. Model: ", SFD_MODEL, "."),
       x = NULL, y = "$m") + chart_theme
print(p_stack)
ggsave("sfd_by_sector.png", p_stack, width = 9, height = 6, dpi = 150, bg = "white")

# (c) Each sector's forecast on one page (small multiples, headline model)
multi_hist <- bind_rows(lapply(sectors, function(s) data.frame(sector = s$name, date = dates, value = s$df$Total))) %>%
  filter(date >= as.Date("2015-01-01"))
multi_fc <- bind_rows(
  sector_fc %>% select(sector, date, value = all_of(SFD_MODEL)),
  bind_rows(lapply(sectors, function(s) data.frame(sector = s$name, date = dates[n], value = s$df$Total[n]))))
p_multi <- ggplot() +
  geom_line(data = multi_hist, aes(date, value), colour = "#0b0b0b", linewidth = 0.6) +
  geom_line(data = multi_fc, aes(date, value, colour = sector), linewidth = 0.9) +
  geom_point(data = filter(multi_fc, date > dates[n]), aes(date, value, colour = sector), size = 1.6) +
  scale_colour_manual(values = sector_pal, guide = "none") +
  facet_wrap(~ sector, ncol = 1, scales = "free_y") +
  labs(title = "Forecast of each sector of State Final Demand",
       subtitle = paste0("$m, chain volume, seasonally adjusted. Black = actual; colour = forecast (", SFD_MODEL, ")."),
       x = NULL, y = "$m") + chart_theme +
  theme(strip.text = element_text(face = "bold", hjust = 0, size = 11))
print(p_multi)
ggsave("sfd_sectors_panel.png", p_multi, width = 9, height = 10, dpi = 150, bg = "white")

# (d) SFD hold-out test
sfd_test_hist <- data.frame(date = dates, value = sfd_abs) %>% filter(date >= as.Date("2022-01-01"))
sfd_plot_models <- setdiff(unique(sfd_holdout$model), "Benchmark: historical mean")
sfd_test_long <- bind_rows(sfd_holdout %>% filter(model %in% sfd_plot_models) %>%
                             transmute(date = test_dates[h], model, value = forecast),
                           data.frame(date = dates[test_end], model = sfd_plot_models, value = sfd_abs[test_end]))
p_sfd_test <- ggplot() +
  annotate("rect", xmin = dates[test_end], xmax = dates[n], ymin = -Inf, ymax = Inf, fill = "grey93") +
  geom_line(data = sfd_test_hist, aes(date, value), colour = "#0b0b0b", linewidth = 0.8) +
  geom_point(data = filter(sfd_test_hist, date > dates[test_end]), aes(date, value), colour = "#0b0b0b", size = 2) +
  geom_line(data = sfd_test_long, aes(date, value, colour = model), linewidth = 0.8) +
  geom_point(data = filter(sfd_test_long, date > dates[test_end]), aes(date, value, colour = model), size = 1.8) +
  scale_colour_manual(values = pal, name = NULL) +
  labs(title = "State Final Demand: hold-out test, last 4 quarters vs ABS SFD",
       subtitle = "Sum of the three sector forecasts, fitted up to the June 2025 quarter. Black = ABS SFD.",
       x = NULL, y = "$m") + chart_theme
print(p_sfd_test)
ggsave("sfd_holdout.png", p_sfd_test, width = 9, height = 5.5, dpi = 150, bg = "white")

# (e) SFD quarterly growth (100 x delta log): history and forecast, the modelled variable
g_hist <- data.frame(date = dates[-1], value = dlog(sfd_abs)) %>% filter(date >= as.Date("2015-01-01"))
g_fc <- sfd_fc %>% select(date, `MinT (A)` = MinT_A_dlog, `VECM-MinT (B)` = Model_B_dlog,
                          Combination = Combination_dlog, `Plain average (SFD)` = Plain_average_SFD_dlog) %>%
  pivot_longer(-date, names_to = "model", values_to = "value")
p_growth <- ggplot() +
  geom_hline(yintercept = 0, colour = "grey70") +
  geom_col(data = g_hist, aes(date, value), fill = "grey75", width = 70) +
  geom_line(data = g_fc, aes(date, value, colour = model), linewidth = 0.8) +
  geom_point(data = g_fc, aes(date, value, colour = model), size = 1.8) +
  scale_colour_manual(values = pal, name = NULL) +
  coord_cartesian(ylim = quantile(c(g_hist$value, g_fc$value), c(0.01, 0.99))) +
  labs(title = "State Final Demand: quarterly growth (100 x delta log), history and forecast",
       subtitle = "Grey bars = ABS SFD growth (COVID quarters clipped). Lines = forecast growth by model.",
       x = NULL, y = "% (100 x delta log)") + chart_theme
print(p_growth)
ggsave("sfd_growth_forecast.png", p_growth, width = 10, height = 5.5, dpi = 150, bg = "white")

# --- 5d. Excel workbook for SFD (+ a CSV of the growth forecasts for the Python notebook)
sfd_dlog_out <- sfd_fc %>% select(quarter, ends_with("_dlog"))
write.csv(sfd_dlog_out, "sfd_dlog_forecast.csv", row.names = FALSE)
cat("\nSFD growth forecasts (100 x delta log):\n"); print(as.data.frame(sfd_dlog_out %>% mutate(across(-quarter, ~ round(.x, 2)))))
round_df <- function(d) { d[] <- lapply(d, function(x) if (is.numeric(x)) round(x, 2) else x); d }
write_xlsx(list(`SFD growth forecast` = round_df(sfd_dlog_out),
                `Stationarity (all)` = all_stationarity,
                `Benchmark acc. by variable` = round_df(bind_rows(lapply(sectors, function(s) s$bench_acc))),
                `SFD forecast by sector` = sfd_wide,
                `All models by sector` = round_df(sfd_table %>% select(-date)),
                `Hold-out accuracy` = round_df(all_accuracy),
                `SFD hold-out` = round_df(sfd_holdout),
                `Sum vs ABS SFD` = round_df(add_check)),
           "forecast_results_sfd.xlsx")

cat("\nDone. Files written: forecast_results_household/private/public/sfd.xlsx, sfd_dlog_forecast.csv and the PNG charts.\n")

