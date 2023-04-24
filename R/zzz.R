.onLoad <- function(libname, pkgname) {
  if (rlang::is_installed("ROracle")) {
    requireNamespace("ROracle", quietly = TRUE)
  }
  else {
    warning("ROracle is NOT installed.",
            " Your setup won't work, please contact your valuable colleagues to help you on this.")
  }

  invisible()
}
