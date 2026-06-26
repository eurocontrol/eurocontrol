#!/usr/bin/env Rscript
# ============================================================================
# Extract reference datasets from the Oracle DB for cross-language testing.
#
# Outputs go to  eurocontrol/data/reference/  (git-ignored).
# Set  COPY_TO_PYTHON <- TRUE  at the bottom to also copy into
# eurocontrolpy/tests/data/reference/ so Python tests can pick them up.
#
# Usage:
#   Rscript tests/extract_reference.R
# ============================================================================

library(eurocontrol)
library(arrow)
library(dplyr)
library(lubridate)
library(withr)

# ── Parameters ───────────────────────────────────────────────────────────────
# Keep the window SHORT so reference files stay small.
WEF       <- "2024-01-15 10:00:00"
TIL       <- "2024-01-15 10:30:00"
WEF_DATE  <- "2024-01-15"
TIL_DATE  <- "2024-01-16"
AIRAC     <- "517"

OUTPUT_DIR  <- file.path("data", "reference")
PYTHON_DIR  <- file.path("..", "eurocontrolpy", "tests", "data", "reference")

# ── Helpers ──────────────────────────────────────────────────────────────────

save_ref <- function(df, name) {
  path <- file.path(OUTPUT_DIR, paste0(name, ".parquet"))
  arrow::write_parquet(df, path)
  cat(sprintf("  ✓ %s  (%d rows × %d cols)\n", name, nrow(df), ncol(df)))
}

# ── Setup ────────────────────────────────────────────────────────────────────
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

withr::local_envvar(c(TZ = "UTC", ORA_SDTZ = "UTC", NLS_LANG = ".AL32UTF8"))
conn     <- withr::local_db_connection(db_connection(schema = "PRU_READ"))
conn_dev <- withr::local_db_connection(db_connection(schema = "PRU_DEV"))

cat("Extracting reference datasets …\n\n")

# ── 1. flights_tidy ─────────────────────────────────────────────────────────
flights <- flights_tidy(conn, wef = WEF, til = TIL) |> collect()
save_ref(flights, "flights_tidy")

# ── 2. adrr_flights_tidy ────────────────────────────────────────────────────
adrr <- adrr_flights_tidy(conn, wef = WEF_DATE, til = TIL_DATE) |> collect()
adrr <- head(adrr, 200)
save_ref(adrr, "adrr_flights_tidy")

# ── 3. airlines_tidy ────────────────────────────────────────────────────────
airlines <- airlines_tidy(conn) |> collect()
save_ref(airlines, "airlines_tidy")

# ── 4. apdf_tidy (raw + processed) ──────────────────────────────────────────
apdf_raw <- apdf_tbl(conn) |>
  filter(
    TO_DATE(WEF, "yyyy-mm-dd hh24:mi:ss") <= MVT_TIME_UTC,
    MVT_TIME_UTC < TO_DATE(TIL, "yyyy-mm-dd hh24:mi:ss"),
    TO_DATE(WEF, "yyyy-mm-dd hh24:mi:ss") <= SRC_DATE_FROM,
    SRC_DATE_FROM < TO_DATE(TIL, "yyyy-mm-dd hh24:mi:ss")
  ) |>
  collect()
save_ref(apdf_raw, "apdf_raw")

apdf <- apdf_tidy(conn, wef = WEF, til = TIL) |> collect()
save_ref(apdf, "apdf_tidy")

# ── 5. airspace_profiles_tidy ───────────────────────────────────────────────
asp <- airspace_profiles_tidy(conn, wef = WEF, til = TIL) |> collect()
save_ref(asp, "airspace_profiles_tidy")

# ── 6. flights_airspace_profiles_tidy ────────────────────────────────────────
# NOTE: the R function has a known FLT_UID column-name collision when
# airspace_profiles_tidy (which includes FLT_UID) is joined with
# flights_tidy (which also has FLT_UID). dbplyr suffixes them as .x/.y,
# then the downstream select(cols) fails.
# Workaround: build the equivalent result from the already-extracted data.
asp_ids <- asp |> dplyr::select(ID) |> dplyr::distinct()
fasp <- flights_tidy(conn, wef = (lubridate::as_datetime(WEF) - lubridate::dhours(28)) |>
                       format("%Y-%m-%d %H:%M:%S"),
                     til = (lubridate::as_datetime(TIL) + lubridate::dhours(24)) |>
                       format("%Y-%m-%d %H:%M:%S")) |>
  dplyr::inner_join(asp_ids, by = "ID", copy = TRUE) |>
  dplyr::distinct() |>
  collect()
save_ref(fasp, "flights_airspace_profiles_tidy")

# ── 7. point_profiles_tidy ──────────────────────────────────────────────────
bbox <- c(xmin = 5.0, xmax = 10.0, ymin = 47.0, ymax = 52.0)
ppt <- point_profiles_tidy(conn, wef = WEF, til = TIL, bbox = bbox) |> collect()
save_ref(ppt, "point_profiles_tidy")

# ── 8. generate_so6 ─────────────────────────────────────────────────────────
if (nrow(ppt) > 0) {
  so6 <- generate_so6(ppt)
  save_ref(so6, "generate_so6")
} else {
  cat("  ⚠ point_profiles_tidy returned 0 rows — skipping generate_so6\n")
}

# ── 9. export_airports ──────────────────────────────────────────────────────
airports <- export_airports(wef = WEF_DATE, til = TIL_DATE)
save_ref(airports, "export_airports")

# ── 10. Airspace geometries (requires PRU_DEV) ──────────────────────────────
cat("Extracting airspace geometries (PRU_DEV)...\n")

acc  <- acc_sf(conn_dev, cfmu_airac = AIRAC)
save_ref(sf::st_drop_geometry(acc), "acc_sf")

ansp <- ansp_sf(conn_dev, cfmu_airac = AIRAC)
save_ref(sf::st_drop_geometry(ansp), "ansp_sf")

es   <- es_sf(conn_dev, cfmu_airac = AIRAC)
save_ref(sf::st_drop_geometry(es), "es_sf")

firs <- fir_sf(conn_dev, cfmu_airac = AIRAC)
save_ref(sf::st_drop_geometry(firs), "fir_sf")

# ── 11. IATA season (no DB) ─────────────────────────────────────────────────
s24 <- season_iata(2024, "summer")
w24 <- season_iata(2024, "winter")
s25 <- season_iata(2025, "summer")

tibble::tibble(
  year   = c(2024L, 2024L, 2025L),
  season = c("summer", "winter", "summer"),
  start  = as.character(c(int_start(s24), int_start(w24), int_start(s25))),
  end    = as.character(c(int_end(s24),   int_end(w24),   int_end(s25)))
) |> save_ref("season_iata")

tibble::tibble(
  date   = c("2024-01-15", "2024-06-15", "2024-11-15", "2025-03-30"),
  season = c(
    iata_season_for_date("2024-01-15"),
    iata_season_for_date("2024-06-15"),
    iata_season_for_date("2024-11-15"),
    iata_season_for_date("2025-03-30")
  )
) |> save_ref("iata_season_for_date")

cat("\nAll reference datasets saved to:", normalizePath(OUTPUT_DIR), "\n")

# ── Toggle: copy to Python package ──────────────────────────────────────────
COPY_TO_PYTHON <- TRUE

if (COPY_TO_PYTHON) {
  dir.create(PYTHON_DIR, recursive = TRUE, showWarnings = FALSE)
  ref_files <- list.files(OUTPUT_DIR, pattern = "\\.parquet$", full.names = TRUE)
  file.copy(ref_files, PYTHON_DIR, overwrite = TRUE)
  cat(sprintf("Copied %d files to %s\n", length(ref_files), normalizePath(PYTHON_DIR)))
}
