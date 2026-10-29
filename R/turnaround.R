# Turnaround / delay propagation study: data preparation.
#
# Builds, for one European APDF hub and study period:
#   1. source milestone table (long, PBWG milestone convention) for every hub flight:
#      hub milestones (hub APDF) + remote-end milestones (remote APDF -> NM proxy -> blank)
#   2. legs dataset        (one row per hub flight, both ends: SOBT AOBT ATOT ALDT AIBT SIBT)
#   3. turnaround dataset  (one row per hub arrival, paired with the next movement of the airframe)
#   4. flight attributes   (delay codes, ATFM fields; carried, not used yet)
#   5. proxy parameters    (B_in, B_out, T_in) and a coverage log
#
# Remote end of a hub flight (one leg up/downstream only), in order of preference:
#   APDF  remote airport reports under APDF and SAM_ID links uniquely
#   NM    NM flight table (Model 1/3) linked by SAM_ID, proxies:
#           IN : SOBT = SIBT_hub - B_in - FLT_DUR_1 - TAXI_TIME_1 ; AOBT = AOBT_3 ; ATOT = AOBT_3 + TAXI_TIME_3
#           OUT: SIBT = SOBT_hub + TAXI_TIME_1 + FLT_DUR_1 + B_out ; ALDT = ARVT_3 ; AIBT = ARVT_3 + T_in
#   BLANK international, circular, no/duplicate SAM_ID, or no link (intentionally blank rows)
#
# Time zone: UTC throughout. All timestamps are forced to UTC on read (as_utc());
# the R session tz is never used for parsing, filtering or formatting.

library(DBI)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(arrow)

# ---- parameters ---------------------------------------------------------------

ta_params <- function(hub, start, end, apdf_zip, nm_files,
                      cutoff_per_day = 4, pad_days = 1,
                      # EUROCONTROL member / comprehensive agreement states (KISS: ICAO prefixes).
                      # E, L (incl. LL Israel), GC Canaries, GM Morocco, BI Iceland, BK Kosovo,
                      # UD Armenia, UG Georgia, UK Ukraine
                      eur_prefix = c("E", "L", "GC", "GM", "BI", "BK", "UD", "UG", "UK"),
                      out_dir = "data/turnaround",
                      src_dir = file.path("data-src", tools::file_path_sans_ext(basename(apdf_zip)))) {
  start <- ymd(start, tz = "UTC")
  end   <- ymd(end, tz = "UTC")   # exclusive
  list(
    hub = hub, start = start, end = end,
    pad_start = start - days(pad_days), pad_end = end + days(pad_days),
    period = paste0(format(start, "%Y%m%d", tz = "UTC"), "-",
                    format(end - days(1), "%Y%m%d", tz = "UTC")),
    apdf_zip = apdf_zip, nm_files = nm_files,
    cutoff_per_day = cutoff_per_day, eur_prefix = eur_prefix,
    out_dir = out_dir, src_dir = src_dir
  )
}

ta_file <- function(p, what, ext = "parquet") {
  file.path(p$out_dir, sprintf("TA-%s-%s-%s.%s", p$hub, what, p$period, ext))
}

# ---- helpers ------------------------------------------------------------------

as_utc    <- function(df) mutate(df, across(where(is.POSIXct), \(x) with_tz(x, "UTC")))
mins      <- function(a, b) as.numeric(difftime(a, b, units = "mins"))
in_window <- function(x, from, to) !is.na(x) & x >= from & x < to
is_eur    <- function(icao, p) {
  !is.na(icao) & (substr(icao, 1, 1) %in% p$eur_prefix | substr(icao, 1, 2) %in% p$eur_prefix)
}

# ---- APDF ---------------------------------------------------------------------

apdf_cols <- c(
  "APDS_ID", "IM_SAMAD_ID", "AP_C_FLTID", "AP_C_REG", "ARCTYP", "ADEP_ICAO", "ADES_ICAO",
  "SRC_AIRPORT", "SRC_PHASE", "SCHED_TIME_UTC", "BLOCK_TIME_UTC", "MVT_TIME_UTC",
  "AP_C_RWY", "AP_C_STND", "AP_C_FLTTYP",
  paste0("DLY", 1:5), paste0("DLY_TIME", 1:5)
)

apdf_std <- function(df) {
  df |>
    select(any_of(apdf_cols)) |>
    rename(SAM_ID = IM_SAMAD_ID, FLTID = AP_C_FLTID, REG = AP_C_REG, TYPE = ARCTYP,
           ADEP = ADEP_ICAO, ADES = ADES_ICAO, AIRPORT = SRC_AIRPORT, PHASE = SRC_PHASE,
           SCH_T = SCHED_TIME_UTC, BLK_T = BLOCK_TIME_UTC, RWY_T = MVT_TIME_UTC,
           RWY = AP_C_RWY, STND = AP_C_STND, FLTTYP = AP_C_FLTTYP) |>
    as_utc()
}

read_hub_apdf <- function(p) {
  members <- unzip(p$apdf_zip, list = TRUE)$Name
  m <- grep(paste0("^", p$hub, "_"), members, value = TRUE)
  if (length(m) != 1) stop("No unique APDF file for ", p$hub, " in ", p$apdf_zip)
  f <- file.path(p$src_dir, m)
  if (!file.exists(f)) unzip(p$apdf_zip, files = m, exdir = p$src_dir)
  open_dataset(f) |>
    select(any_of(apdf_cols)) |>
    collect() |>
    apdf_std() |>
    filter(in_window(RWY_T, p$pad_start, p$pad_end))
}

# ---- data warehouse (PRISME) ----------------------------------------------------

connect_prisme <- function() {
  # Oracle session tz only; the R session tz is left untouched
  Sys.setenv(ORA_SDTZ = "UTC", NLS_LANG = ".AL32UTF8")
  dbConnect(
    ROracle::Oracle(),
    username = Sys.getenv("PRU_READ_USR"),
    password = Sys.getenv("PRU_READ_PWD"),
    dbname   = Sys.getenv("PRU_READ_DBNAME")
  )
}

ora_date <- function(x) sprintf("TO_DATE('%s', 'YYYY-MM-DD')", format(x, "%Y-%m-%d", tz = "UTC"))

# All airports reporting under APDF in the study period (not hub specific; cached)
ta_reporting_airports <- function(p, con = NULL) {
  f <- file.path(p$out_dir, paste0("TA-APDF-reporting-airports-", p$period, ".csv"))
  if (file.exists(f)) return(read_csv(f, show_col_types = FALSE))
  if (is.null(con)) { con <- connect_prisme(); on.exit(dbDisconnect(con)) }
  # SRC_DATE_FROM is the monthly batch start
  sql <- paste0(
    "SELECT SRC_AIRPORT, SRC_PHASE, COUNT(*) AS N, ",
    "MIN(MVT_TIME_UTC) AS FIRST_MVT, MAX(MVT_TIME_UTC) AS LAST_MVT ",
    "FROM SWH_FCT.FAC_APDS_FLIGHT_IR691 ",
    "WHERE SRC_DATE_FROM >= ", ora_date(p$start), " AND SRC_DATE_FROM < ", ora_date(p$end),
    " GROUP BY SRC_AIRPORT, SRC_PHASE"
  )
  out <- dbGetQuery(con, sql) |> as_tibble() |> as_utc()
  write_csv(out, f)
  out
}

# Remote ends of hub legs at APDF airports `apts` (cached; only missing airports are queried)
ta_remote_apdf <- function(p, apts, con = NULL) {
  f      <- ta_file(p, "remote-APDF")
  f_apts <- ta_file(p, "remote-APDF-airports", "csv")
  # Read fully into memory (no memory map) so the cache can be rewritten on Windows
  cached <- if (file.exists(f)) as_tibble(as.data.frame(read_parquet(f, as_data_frame = FALSE))) else NULL
  done   <- if (file.exists(f_apts)) read_csv(f_apts, show_col_types = FALSE)$ICAO
            else if (!is.null(cached)) unique(cached$SRC_AIRPORT) else character()
  todo   <- setdiff(apts, done)
  if (length(todo) > 0) {
    if (is.null(con)) { con <- connect_prisme(); on.exit(dbDisconnect(con)) }
    sql <- paste0(
      "SELECT ", paste(apdf_cols, collapse = ", "),
      " FROM SWH_FCT.FAC_APDS_FLIGHT_IR691",
      " WHERE SRC_AIRPORT IN (", paste0("'", todo, "'", collapse = ", "), ")",
      " AND SRC_DATE_FROM >= TRUNC(", ora_date(p$pad_start), ", 'MM')",
      " AND SRC_DATE_FROM < ", ora_date(p$pad_end),
      " AND MVT_TIME_UTC >= ", ora_date(p$pad_start),
      " AND MVT_TIME_UTC < ", ora_date(p$pad_end),
      " AND (ADEP_ICAO = '", p$hub, "' OR ADES_ICAO = '", p$hub, "')"
    )
    new <- dbGetQuery(con, sql) |> as_tibble() |> as_utc()
    cached <- bind_rows(cached, new)
    tmp <- paste0(f, ".part")
    write_parquet(cached, tmp)
    gc()
    file.copy(tmp, f, overwrite = TRUE)
    unlink(tmp)
    write_csv(tibble(ICAO = union(done, todo)), f_apts)
  }
  apdf_std(cached)
}

# ---- NM flight table ------------------------------------------------------------

nm_cols <- c(
  "ID", "LOBT", "ADEP", "ADES", "AIRCRAFT_ID", "REGISTRATION", "AIRCRAFT_ADDRESS",
  "EOBT_1", "ARVT_1", "TAXI_TIME_1", "FLT_DUR_1", "AOBT_3", "ARVT_3", "TAXI_TIME_3", "FLT_DUR_3",
  "ATFM_DELAY", "MOST_PEN_REG", "REG_REASON", "IATA_DELAY_CODE", "ICAO_FLT_TYPE", "FLT_STATUS"
)

read_nm_hub <- function(p) {
  hub <- p$hub
  lapply(p$nm_files, \(f) {
    open_dataset(f) |>
      filter(ADEP == hub | ADES == hub) |>
      select(any_of(nm_cols)) |>
      collect()
  }) |>
    bind_rows() |>
    as_utc() |>
    filter(in_window(LOBT, p$pad_start - days(1), p$pad_end + days(1))) |>
    distinct()
}

# ---- build ----------------------------------------------------------------------

ta_build <- function(p, con = NULL) {
  dir.create(p$out_dir, showWarnings = FALSE, recursive = TRUE)
  n_days <- as.numeric(difftime(p$end, p$start, units = "days"))

  hub <- read_hub_apdf(p) |>
    mutate(LEG = if_else(PHASE == "ARR", "IN", "OUT"),
           REMOTE = if_else(LEG == "IN", ADEP, ADES))

  # Study airports: European remote ends with >= cutoff hub movements per day
  study <- hub |>
    filter(in_window(RWY_T, p$start, p$end), is_eur(REMOTE, p), REMOTE != p$hub) |>
    count(ICAO = REMOTE, PHASE) |>
    pivot_wider(names_from = PHASE, values_from = n, values_fill = 0) |>
    mutate(MVTS = ARR + DEP, PER_DAY = MVTS / n_days) |>
    filter(PER_DAY >= p$cutoff_per_day) |>
    arrange(desc(MVTS))

  reporting <- ta_reporting_airports(p, con)
  apdf_apts <- setdiff(unique(reporting$SRC_AIRPORT), p$hub)
  study <- study |> mutate(PER_DAY = round(PER_DAY, 1), APDF = ICAO %in% apdf_apts)
  write_csv(study, ta_file(p, "study-airports", "csv"))

  hub <- hub |>
    mutate(
      REMOTE_CLASS = case_when(
        is.na(REMOTE) | REMOTE == "" ~ "UNKNOWN",
        REMOTE == p$hub              ~ "CIRCULAR",
        REMOTE %in% study$ICAO       ~ "EUR-study",
        is_eur(REMOTE, p)            ~ "EUR-other",
        TRUE                         ~ "INTL"
      ),
      REMOTE_APDF = REMOTE %in% apdf_apts
    ) |>
    add_count(SAM_ID, name = "N_SAM") |>
    mutate(SAM_OK = !is.na(SAM_ID) & N_SAM == 1,
           KEY = if_else(SAM_OK, sprintf("%.0f", SAM_ID), sprintf("APDS%.0f", APDS_ID)),
           UID = paste(ADEP, ADES, KEY, sep = "-"))

  # Remote APDF: all reporting European remote ends of hub legs (study and below cutoff)
  need <- hub |> filter(REMOTE_CLASS %in% c("EUR-study", "EUR-other"), REMOTE_APDF) |>
    distinct(REMOTE) |> pull()
  remote <- ta_remote_apdf(p, need, con) |>
    filter(!is.na(SAM_ID)) |>
    mutate(LEG = if_else(PHASE == "DEP", "IN", "OUT")) |>   # remote DEP = hub inbound
    add_count(SAM_ID, LEG, name = "N")
  remote_dup <- remote |> filter(N > 1) |> distinct(SAM_ID) |> pull()
  remote <- remote |> filter(N == 1) |> select(-N)

  nm <- read_nm_hub(p) |> add_count(ID, name = "N")
  nm_dup <- nm |> filter(N > 1) |> distinct(ID) |> pull()
  nm <- nm |> filter(N == 1) |> select(-N)

  f <- hub |>
    left_join(
      remote |> select(SAM_ID, LEG, R_AIRPORT = AIRPORT, R_SCH = SCH_T, R_BLK = BLK_T,
                       R_RWY_T = RWY_T, R_RWY = RWY, R_STND = STND),
      by = c("SAM_ID", "LEG"), na_matches = "never"
    ) |>
    left_join(
      nm |> select(SAM_ID = ID, ICAO24 = AIRCRAFT_ADDRESS, EOBT_1, ARVT_1, TAXI_TIME_1, FLT_DUR_1,
                   AOBT_3, ARVT_3, TAXI_TIME_3, FLT_DUR_3, ATFM_DELAY, MOST_PEN_REG, REG_REASON,
                   IATA_DELAY_CODE, ICAO_FLT_TYPE, FLT_STATUS),
      by = "SAM_ID", na_matches = "never"
    ) |>
    mutate(
      ICAO24 = if_else(SAM_OK, toupper(ICAO24), NA_character_),
      APDF_OK = SAM_OK & REMOTE_APDF & !is.na(R_SCH) & (!is.na(R_BLK) | !is.na(R_RWY_T)),
      NM_OK   = SAM_OK & !is.na(AOBT_3) & !is.na(ARVT_3) & !is.na(FLT_DUR_1) & !is.na(TAXI_TIME_1),
      REMOTE_SRC = case_when(
        !REMOTE_CLASS %in% c("EUR-study", "EUR-other") ~ "BLANK",
        APDF_OK ~ "APDF",
        NM_OK   ~ "NM",
        TRUE    ~ "BLANK"
      ),
      MATCH = case_when(
        REMOTE_SRC != "BLANK"        ~ "sam_id",
        REMOTE_CLASS == "INTL"       ~ "none:intl",
        REMOTE_CLASS == "CIRCULAR"   ~ "none:circular",
        REMOTE_CLASS == "UNKNOWN"    ~ "none:unknown",
        is.na(SAM_ID)                ~ "none:no-sam",
        !SAM_OK | SAM_ID %in% c(remote_dup, nm_dup) ~ "none:dup-sam",
        TRUE                         ~ "none:no-link"
      )
    )

  # Proxy calibration on legs with both ends in APDF and an NM link
  cal <- f |>
    filter(APDF_OK, !is.na(FLT_DUR_1), !is.na(TAXI_TIME_1), !is.na(SCH_T)) |>
    mutate(
      B    = if_else(LEG == "OUT", mins(R_SCH, SCH_T), mins(SCH_T, R_SCH)) - TAXI_TIME_1 - FLT_DUR_1,
      T_IN = if_else(LEG == "OUT", mins(R_BLK, R_RWY_T), NA_real_)
    )
  q <- function(x) tibble(N = sum(!is.na(x)), MEDIAN = median(x, na.rm = TRUE),
                          P25 = quantile(x, .25, na.rm = TRUE), P75 = quantile(x, .75, na.rm = TRUE))
  params <- bind_rows(
    cal |> filter(LEG == "IN")  |> reframe(PARAM = "B_in",  q(B)),
    cal |> filter(LEG == "OUT") |> reframe(PARAM = "B_out", q(B)),
    cal |> filter(LEG == "OUT") |> reframe(PARAM = "T_in",  q(T_IN))
  ) |> mutate(HUB = p$hub, UNIT = "min", .before = 1)
  write_csv(params, ta_file(p, "proxy-params", "csv"))
  B_in  <- params$MEDIAN[params$PARAM == "B_in"]
  B_out <- params$MEDIAN[params$PARAM == "B_out"]
  T_in  <- params$MEDIAN[params$PARAM == "T_in"]

  # Remote-end milestones by source (SCH/BLK/RWY; meaning depends on LEG)
  apdf_src <- paste0("APDF-", f$R_AIRPORT)
  f <- f |>
    mutate(
      IS_APDF = REMOTE_SRC == "APDF", IS_NM = REMOTE_SRC == "NM", IN = LEG == "IN",
      RT_SCH = case_when(
        IS_APDF       ~ R_SCH,
        IS_NM & IN    ~ SCH_T - dminutes(B_in + FLT_DUR_1 + TAXI_TIME_1),
        IS_NM & !IN   ~ SCH_T + dminutes(TAXI_TIME_1 + FLT_DUR_1 + B_out)
      ),
      RT_BLK = case_when(IS_APDF ~ R_BLK, IS_NM & IN ~ AOBT_3, IS_NM & !IN ~ ARVT_3 + dminutes(T_in)),
      RT_RWY = case_when(IS_APDF ~ R_RWY_T, IS_NM & IN ~ AOBT_3 + dminutes(TAXI_TIME_3), IS_NM & !IN ~ ARVT_3),
      RS_SCH = case_when(IS_APDF ~ apdf_src, IS_NM ~ "NM-PROXY"),
      RS_BLK = case_when(IS_APDF ~ apdf_src, IS_NM & IN ~ "NM-M3", IS_NM ~ "NM-PROXY"),
      RS_RWY = case_when(IS_APDF ~ apdf_src, IS_NM & IN ~ "NM-PROXY", IS_NM ~ "NM-M3"),
      IN_PERIOD = in_window(RWY_T, p$start, p$end)
    )

  # 1. Source milestone table (long)
  mst_name <- function(kind, at_dep) {
    case_when(kind == "SCH" & at_dep ~ "SOBT", kind == "SCH" ~ "SIBT",
              kind == "BLK" & at_dep ~ "AOBT", kind == "BLK" ~ "AIBT",
              kind == "RWY" & at_dep ~ "ATOT", TRUE ~ "ALDT")
  }
  hub_src <- paste0("APDF-", p$hub)
  side <- function(kind, time, lbl, src, hub_side) {
    f |> transmute(
      UID, FLTID, REG, TYPE, ICAO24, ADEP, ADES,
      MST = mst_name(kind, if (hub_side) LEG == "OUT" else LEG == "IN"),
      MST_LBL = {{ lbl }}, TIME = {{ time }},
      LAT = NA_real_, LON = NA_real_, ALT = NA_real_,
      SOURCE = {{ src }}, SOURCE_ID = KEY, CONF = NA_character_,
      MATCH = if (hub_side) "hub" else MATCH,
      ROLE = if (hub_side) if_else(IN, "ARR", "DEP") else if_else(IN, "UP", "DN"),
      MST_TYPE = if_else(kind == "SCH", "SCH", "ACT")
    )
  }
  milestones <- bind_rows(
    side("SCH", SCH_T, NA_character_, hub_src, TRUE),
    side("BLK", BLK_T, STND, hub_src, TRUE),
    side("RWY", RWY_T, RWY, hub_src, TRUE),
    side("SCH", RT_SCH, NA_character_, RS_SCH, FALSE),
    side("BLK", RT_BLK, if_else(IS_APDF, R_STND, NA_character_), RS_BLK, FALSE),
    side("RWY", RT_RWY, if_else(IS_APDF, R_RWY, NA_character_), RS_RWY, FALSE)
  ) |>
    mutate(SOURCE_ID = if_else(is.na(SOURCE), NA_character_, SOURCE_ID)) |>
    arrange(UID, factor(MST, c("SOBT", "AOBT", "ATOT", "ALDT", "AIBT", "SIBT")))
  write_parquet(milestones, ta_file(p, "milestones"))

  # 2. Legs (one row per hub flight, both ends)
  legs <- milestones |>
    select(UID, MST, TIME) |>
    pivot_wider(names_from = MST, values_from = TIME) |>
    right_join(
      f |> transmute(UID, HUB = p$hub, LEG, ADEP, ADES, REMOTE, REMOTE_CLASS, REMOTE_APDF,
                     REMOTE_SRC, MATCH, FLTID, REG, TYPE, ICAO24, IN_PERIOD),
      by = "UID"
    ) |>
    select(UID, HUB, LEG, ADEP, ADES, REMOTE, REMOTE_CLASS, REMOTE_APDF, REMOTE_SRC, MATCH,
           FLTID, REG, TYPE, ICAO24, SOBT, AOBT, ATOT, ALDT, AIBT, SIBT, IN_PERIOD)
  write_parquet(legs, ta_file(p, "legs"))

  # 3. Turnarounds: each hub arrival in period + next hub movement of the same airframe
  seq <- f |>
    filter(!is.na(REG), REG != "", !is.na(BLK_T)) |>
    arrange(REG, BLK_T) |>
    group_by(REG) |>
    mutate(NEXT_UID = lead(UID), NEXT_LEG = lead(LEG)) |>
    ungroup()
  arr <- f |> transmute(ARR_UID = UID, ARR_FLTID = FLTID, TYPE, ORIGIN = ADEP,
                        ARR_CLASS = REMOTE_CLASS, ARR_SRC = REMOTE_SRC,
                        SIBT = SCH_T, ALDT = RWY_T, AIBT = BLK_T, ARR_RWY = RWY, ARR_STND = STND)
  dep <- f |> transmute(DEP_UID = UID, DEP_FLTID = FLTID, DEP_TYPE = TYPE, DESTINATION = ADES,
                        DEP_CLASS = REMOTE_CLASS, DEP_SRC = REMOTE_SRC,
                        SOBT = SCH_T, AOBT = BLK_T, ATOT = RWY_T, DEP_RWY = RWY, DEP_STND = STND)
  turnarounds <- seq |>
    filter(LEG == "IN", in_window(BLK_T, p$start, p$end)) |>
    transmute(
      TA_ID = paste(p$hub, REG, format(BLK_T, "%Y%m%d%H%M", tz = "UTC"), sep = "-"),
      HUB = p$hub, REG, ARR_UID = UID,
      DEP_UID = if_else(NEXT_LEG == "OUT", NEXT_UID, NA_character_),
      PAIR_QC = case_when(is.na(NEXT_LEG) ~ "no-next", NEXT_LEG == "IN" ~ "next-arr", TRUE ~ "next-dep")
    ) |>
    left_join(arr, by = "ARR_UID") |>
    left_join(dep, by = "DEP_UID") |>
    mutate(
      GROUND_ACT_MIN = mins(AOBT, AIBT),
      GROUND_SCH_MIN = mins(SOBT, SIBT),
      OVERNIGHT_UTC  = as_date(with_tz(AOBT, "UTC")) > as_date(with_tz(AIBT, "UTC")),
      TYPE_CHANGE    = !is.na(DEP_TYPE) & DEP_TYPE != TYPE
    ) |>
    select(-DEP_TYPE)
  write_parquet(turnarounds, ta_file(p, "turnarounds"))

  # 4. Flight attributes (not used in the core analysis)
  attributes <- f |>
    select(UID, FLTTYP, any_of(c(paste0("DLY", 1:5), paste0("DLY_TIME", 1:5))),
           ATFM_DELAY, MOST_PEN_REG, REG_REASON, IATA_DELAY_CODE, ICAO_FLT_TYPE, FLT_STATUS,
           TAXI_TIME_1, FLT_DUR_1, TAXI_TIME_3, FLT_DUR_3)
  write_parquet(attributes, ta_file(p, "flight-attributes"))

  # 5. Coverage log (flights in period)
  log <- f |>
    filter(IN_PERIOD) |>
    count(LEG, REMOTE_CLASS, REMOTE_SRC, MATCH, name = "N") |>
    mutate(HUB = p$hub, .before = 1)
  write_csv(log, ta_file(p, "coverage", "csv"))

  invisible(list(params = params, study = study, coverage = log, milestones = milestones,
                 legs = legs, turnarounds = turnarounds, attributes = attributes))
}
