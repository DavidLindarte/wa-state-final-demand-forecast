# =============================================================================
# BUILD THE THREE MODEL INPUT FILES FROM THE LATEST ABS RELEASE
#
# Source: ABS 5206.0 Australian National Accounts: National Income, Expenditure
#         and Product, Table 30 - State Final Demand, Detailed Components: WA
#         (seasonally adjusted, chain volume measures, $m). Licence: CC BY 4.0.
#
# Writes, in exactly the layout the forecasting script expects:
#   data/household.xlsx   household final consumption by purpose (short names)
#   data/private.xlsx     private gross fixed capital formation + private final demand
#   data/goverment.xlsx   government consumption/investment, public final demand, SFD
#
# Usage:
#   Rscript R/build_inputs.R                         # download the latest release
#   Rscript R/build_inputs.R path/to/5206030_SFD_WA.xlsx   # use a local copy
# =============================================================================
suppressPackageStartupMessages({ library(readxl); library(writexl) })

args     <- commandArgs(trailingOnly = TRUE)
raw_dir  <- "data/raw"
out_dir  <- "data"
dir.create(raw_dir, showWarnings = FALSE, recursive = TRUE)

# ---- 1. Get the ABS workbook -------------------------------------------------
if (length(args) >= 1) {
  abs_file <- args[1]
} else {
  # readabs keeps up with changes to the ABS website; it downloads the xlsx into raw_dir
  if (!requireNamespace("readabs", quietly = TRUE)) stop("Install readabs: install.packages('readabs')")
  invisible(readabs::read_abs(cat_no = "5206.0", tables = 30, path = raw_dir,
                              check_local = FALSE, retain_files = TRUE, show_progress_bars = FALSE))
  abs_file <- list.files(raw_dir, pattern = "^5206030.*\\.xlsx$", full.names = TRUE, ignore.case = TRUE)[1]
  if (is.na(abs_file)) stop("ABS table 5206030 was not downloaded")
}
message("Reading ", abs_file)

# ---- 2. Parse every Data sheet: 10 header rows, then dates + values ----------
read_abs_sheets <- function(path) {
  sheets <- grep("^Data", excel_sheets(path), value = TRUE)
  out <- list()
  for (sh in sheets) {
    raw <- suppressMessages(read_excel(path, sheet = sh, col_names = FALSE, col_types = "text"))
    hdr <- raw[1:10, ]; body <- raw[-(1:10), ]
    dates <- as.Date(as.numeric(body[[1]]), origin = "1899-12-30")
    for (j in 2:ncol(raw)) {
      out[[length(out) + 1]] <- data.frame(
        title = trimws(hdr[[j]][1]), series_type = hdr[[j]][3], series_id = hdr[[j]][10],
        date = dates, value = suppressWarnings(as.numeric(body[[j]])), stringsAsFactors = FALSE)
    }
  }
  d <- do.call(rbind, out)
  d[d$series_type == "Seasonally Adjusted" & !is.na(d$date), ]
}
abs_long <- read_abs_sheets(abs_file)

get_series <- function(title) {
  s <- abs_long[abs_long$title == title, c("date", "value")]
  if (nrow(s) == 0) stop("Series not found in ABS table: ", title)
  s[order(s$date), ]
}

# Build one wide table (date + one column per series), keeping quarters where all are present
build_table <- function(titles, col_names = titles) {
  tbl <- Reduce(function(a, b) merge(a, b, by = "date", all = TRUE),
                Map(function(t, nm) { s <- get_series(t); names(s)[2] <- nm; s }, titles, col_names))
  tbl <- tbl[complete.cases(tbl[, -1, drop = FALSE]) | rowSums(!is.na(tbl[, -1, drop = FALSE])) > 0, ]
  tbl[tbl$date >= as.Date("1985-09-01"), ]
}
cvm <- function(x) paste0(x, ": Chain volume measures ;")

# ---- 3. Household final consumption (short names used by the model script) ---
hh_map <- c(
  food = "Food", tobacco = "Cigarettes and tobacco", alcohol = "Alcoholic beverages",
  cloting = "Clothing and footwear", rent = "Rent and other dwelling services",
  energy = "Electricity, gas and other fuel", furnishing = "Furnishings and household equipment",
  health = "Health", vehicles = "Purchase of vehicles", `operation vehicules` = "Operation of vehicles",
  transport = "Transport services", communication = "Communications", recreation = "Recreation and culture",
  education = "Education services", hospitality = "Hotels, cafes and restaurants",
  insurance = "Insurance and other financial services", other = "Other goods and services",
  `expenditure interstate` = "Net expenditure interstate")
hh_titles <- c(cvm(paste0("Households ;  Final consumption expenditure - ", hh_map)),
               cvm("Households ;  Final consumption expenditure"))
household <- build_table(hh_titles, c(names(hh_map), "final comsuption"))
names(household)[1] <- "date"

# ---- 4. Private investment ------------------------------------------------------
pr_parts <- c("Dwellings - New and Used", "Dwellings - Alterations and additions", "Dwellings - Total",
              "Ownership transfer costs", "Non-dwelling construction - New building",
              "Non-dwelling construction - New engineering construction",
              "Non-dwelling construction - Net purchase of second hand assets",
              "Non-dwelling construction - Total", "Machinery and equipment - New",
              "Machinery and equipment - Net purchase of second hand assets",
              "Machinery and equipment - Total", "Cultivated biological resources",
              "Intellectual property products", "Business investment")
pr_titles <- c(cvm(paste0("Private ;  Gross fixed capital formation - ", pr_parts)),
               cvm("Private ;  Gross fixed capital formation"), cvm("Private ;  Final demand"))
private <- build_table(pr_titles); names(private)[1] <- "Date"

# ---- 5. Public final demand + SFD ---------------------------------------------
pu_titles <- cvm(c(
  "General government - National ;  Final consumption expenditure",
  "General government - State and local ;  Final consumption expenditure",
  "General government ;  Final consumption expenditure",
  "Public corporations - Commonwealth ;  Gross fixed capital formation",
  "Public corporations - State and local ;  Gross fixed capital formation",
  "Public corporations ;  Gross fixed capital formation",
  "General government - National ;  Gross fixed capital formation",
  "General government - State and local ;  Gross fixed capital formation",
  "General government ;  Gross fixed capital formation",
  "Public ;  Gross fixed capital formation", "Public ;  Final demand", "STATE FINAL DEMAND"))
public <- build_table(pu_titles); names(public)[1] <- "Date"

# ---- 6. Write + a small manifest for the dashboard ------------------------------
write_xlsx(list(Household = household), file.path(out_dir, "household.xlsx"))
write_xlsx(private, file.path(out_dir, "private.xlsx"))
# The model script reads this file in the ABS layout: cell A1 empty, series titles in row 1,
# Excel date serials in column A below it.
pub_out <- rbind(c(NA, names(public)[-1]),
                 cbind(as.character(as.numeric(public$Date - as.Date("1899-12-30"))),
                       as.matrix(format(public[, -1], scientific = FALSE, trim = TRUE))))
pub_out <- as.data.frame(pub_out, stringsAsFactors = FALSE)
write_xlsx(pub_out, file.path(out_dir, "goverment.xlsx"), col_names = FALSE)

last_q <- max(public$Date[!is.na(public[["STATE FINAL DEMAND: Chain volume measures ;"]])])
writeLines(jsonlite::toJSON(list(source_file = basename(abs_file), last_quarter = format(last_q, "%Y-%m"),
                                 rows = nrow(public), built_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)),
                            auto_unbox = TRUE, pretty = TRUE), file.path(out_dir, "inputs_manifest.json"))
message("Inputs written. Latest quarter with data: ", format(last_q, "%Y-%m"))
