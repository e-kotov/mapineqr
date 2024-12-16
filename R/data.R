#' Get univariate or bivariate data for a specific source
#'
#' Fetches univariate or bivariate data for a given source, year, NUTS level, and selected filters.
#'
#' @param x_source A `character` string specifying the source name for the x variable.
#' @param y_source (Optional) A `character` string specifying the source name for the y variable.
#' @param year A `character` or `integer` specifying the year.
#' @param level A `character` string specifying the NUTS level ("0", "1", "2", or "3").
#' @param x_filters A `named list` where the names are the filter fields for the x variable
#'   and the values are the selected values for those fields. Default is an empty list.
#' @param y_filters (Optional) A `named list` where the names are the filter fields for the y variable
#'   and the values are the selected values for those fields. Default is `NULL`.
#' @param limit An `integer` specifying the maximum number of results to return. Default is 2000.
#'
#' @return A `tibble` with the following columns:
#' 
#' **For univariate data** (when `y_source` is not provided):
#' 
#' * `best_year`: the best available year, closest to the requested year.
#' * `geo`: code for the NUTS region at the requested level.
#' * `geo_name`: name of the NUTS region at the requested level.
#' * `x`: the value of the univariate variable.
#' 
#' **For bivariate data** (when `y_source` is provided):
#' 
#' * `best_year`: the best available year, closest to the requested year (same for both x and y variables).
#' * `geo`: code for the NUTS region at the requested level.
#' * `geo_name`: name of the NUTS region at the requested level.
#' * `x`: the value of the x variable.
#' * `y`: the value of the y variable.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' # Univariate example
#' mi_data(
#'   x_source = "TGS00010",
#'   year = 2020,
#'   level = "2",
#'   x_filters = list(isced11 = "TOTAL", unit = "PC", age = "Y_GE15", freq = "A")
#' )
#'
#' # Bivariate example
#' mi_data(
#'   x_source = "TGS00010",
#'   y_source = "DEMO_R_MLIFEXP",
#'   year = 2020,
#'   level = "2",
#'   x_filters = list(isced11 = "TOTAL", unit = "PC", age = "Y_GE15", freq = "A"),
#'   y_filters = list(unit = "YR", age = "Y_LT1", freq = "A")
#' )
#' }
mi_data <- function(
  x_source,
  y_source = NULL,
  year,
  level,
  x_filters = list(),
  y_filters = NULL,
  limit = 2000
) {
  # Validate inputs
  checkmate::assert_string(x_source)
  checkmate::assert_character(level, len = 1)
  checkmate::assert_list(x_filters, types = c("character", "NULL"))
  checkmate::assert_integerish(year, null.ok = TRUE, max.len = 1)
  checkmate::assert_list(y_filters, types = c("character", "NULL"), null.ok = TRUE)
  if (!is.null(y_source)) checkmate::assert_string(y_source)
  
  # Build filter JSONs for X and Y
  x_conditions <- lapply(names(x_filters), function(name) {
    list(field = name, value = x_filters[[name]])
  })
  
  x_json <- list(
    source = x_source,
    conditions = x_conditions
  )
  x_json_string <- jsonlite::toJSON(x_json, auto_unbox = TRUE)
  
  # Check if it's bivariate (Y filters are provided)
  if (!is.null(y_source) && !is.null(y_filters)) {
    y_conditions <- lapply(names(y_filters), function(name) {
      list(field = name, value = y_filters[[name]])
    })
    y_json <- list(
      source = y_source,
      conditions = y_conditions
    )
    y_json_string <- jsonlite::toJSON(y_json, auto_unbox = TRUE)
  }
  
  # Build API endpoint
  base_api_endpoint <- getOption("mapineqr.base_api_endpoint")
  url_endpoint <- if (is.null(y_source)) {
    paste0(base_api_endpoint, "get_x_data/items.json")
  } else {
    paste0(base_api_endpoint, "get_xy_data/items.json")
  }
  
  # Prepare query parameters
  query_params <- list(
    `_level` = level,
    `_year` = as.character(year),
    `X_JSON` = x_json_string,
    `limit` = limit
  )
  
  # Add Y_JSON to query parameters if bivariate
  if (!is.null(y_source) && !is.null(y_filters)) {
    query_params$`Y_JSON` <- y_json_string
  }
  
  # Perform API request
  response <- httr2::request(url_endpoint) |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      "User-Agent" = getOption("mapineqr.user_agent")
    ) |>
    httr2::req_url_query(!!!query_params) |>
    httr2::req_method("GET") |>
    httr2::req_perform()
  
  # Parse response
  response_data <- httr2::resp_body_json(response, simplifyVector = TRUE) |> 
    tibble::as_tibble()
  
  return(response_data)
}
