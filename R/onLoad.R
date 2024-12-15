.onLoad <- function(libname, pkgname) {
  op <- options()
  op.mapineqr <- list(
    mapineqr.api_spec_json = "https://mapineqfeatures.web.rug.nl/api.json",
    mapineqr.base_api_endpoint = "https://mapineqfeatures.web.rug.nl/functions/postgisftw.",
    mapineq.user_agent = "mapineqr R package"
  )
  toset <- !(names(op.mapineqr) %in% names(op))
  if (any(toset)) options(op.mapineqr[toset])

  invisible()
}
