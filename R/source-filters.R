#' Get column values for filtering a source
#'
#' Fetches the possible filtering values for a given source, year, and NUTS level.
#'
#' @param source_name A `character` string specifying the source name (f_resource).
#' @param year A `character` or `integer` specifying the year.
#' @param level A `character` string specifying the NUTS level ("0", "1", "2", or "3").
#' @param filters A `named list` where the names are the filter fields and
#'   the values are the selected values for those fields. Default is an empty list.
#' @inheritParams mi_sources
#'
#' @return A `tibble` with the fields, labels, and their possible values for filtering.
#' 
#' @export
#'
#' @examples
#' \donttest{
#' mi_source_filters(
#'   source_name = "DEMO_R_FIND2",
#'   year = 2020,
#'   level = "2",
#'   filters = list(unit = "YR")
#' )
#' }
mi_source_filters <- function(
  source_name,
  year,
  level,
  filters = list(),
  limit = 40
) {
  # Validate inputs
  checkmate::assert_string(source_name)
  checkmate::assert_character(level, len = 1)
  checkmate::assert_list(filters, types = c("character", "NULL"))
  checkmate::assert_integerish(year, null.ok = TRUE, max.len = 1)
  
  # Convert the named list to the required structure for the API
  selected <- if (length(filters) == 0) {
    list()
  } else {
    lapply(names(filters), function(name) {
      list(field = name, value = filters[[name]])
    })
  }

  # Build API endpoint
  base_api_endpoint <- getOption("mapineqr.base_api_endpoint")
  url_endpoint <- paste0(base_api_endpoint,
    "get_column_values_source_json/items.json")
  
  # Prepare JSON for source selections
  source_selections <- list(
    year = as.character(year),
    level = level,
    selected = selected
  )
  source_selections_json <- jsonlite::toJSON(
    source_selections,
    auto_unbox = TRUE
  )
  
  # Prepare query parameters
  query_params <- list(
    `_resource` = source_name,
    `source_selections` = source_selections_json,
    limit = limit
  )
  
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
    tibble::as_tibble() |> 
    tidyr::unnest(col = .data$field_values)
  
  return(response_data)
}
