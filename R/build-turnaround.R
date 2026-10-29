# Build the turnaround study data per hub and period (see R/turnaround.R).
# Data warehouse access (remote APDF, reporting list) only when the cache is missing;
# credentials from PRU_READ_USR / PRU_READ_PWD / PRU_READ_DBNAME.

source("R/turnaround.R")

# Location of apdf-<year>.zip and NM-flight-tables/ (set PBWG_DATA_DIR in ~/.Renviron)
data_dir <- Sys.getenv("PBWG_DATA_DIR")
if (!nzchar(data_dir) || !dir.exists(data_dir)) stop("Set PBWG_DATA_DIR to the shared data folder")
hubs     <- c("EGLL", "EHAM", "EDDF", "EDDM", "LFPG", "LSZH", "LTFM", "LIRF", "LEMD", "LPPT")

ta_all <- lapply(hubs, \(h) {
  p <- ta_params(
    hub      = h,
    start    = "2025-06-01",
    end      = "2025-09-01",   # exclusive
    apdf_zip = file.path(data_dir, "apdf-2025.zip"),
    nm_files = file.path(data_dir, "NM-flight-tables",
                         c("NM-FLT-2025-Q2.parquet", "NM-FLT-2025-Q3.parquet"))
  )
  ta_build(p)
}) |> setNames(hubs)
