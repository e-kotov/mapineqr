#' Get a list of available data sources
#' 
#' @param level a `character` string specifying the NUTS level ("0", "1", "2", or "3"). You can also always check valid NUTS levels using \code{\link{mi_nuts_levels}}.
#' @param year an `integer` of length 1, specifying the year. Optional.
#' @param limit an `integer` of length 1 specifying the maximum number of sources to return. Defaults to 1000.
#' 
#' @return a `tibble` of sources with the following columns:
#' 
#' * `source_name`: name of the data source
#' * `short_description`: short description of the data source
#' * `description`: description of the data source
#' 
#' @importFrom rlang .data
#' @export
#' 
#' @examples
#' 
#' \dontrun{
#' # get up to 10 sources for NUTS level 3
#' mi_sources("3", limit = 10)
#' 
#' # get all sources for NUTS level 3 and year 2020
#' mi_sources("3", year = 2020)
#' 
#' }
#' 
mi_sources <- function(
  level,
  year = NULL,
  limit = 1000
){

  checkmate::assert_choice(level, c("0", "1", "2", "3"))
  checkmate::assert_integerish(limit, lower = 1, max.len = 1)
  checkmate::assert_integerish(year, null.ok = TRUE, max.len = 1)

  base_api_endpoint <- getOption("mapineqr.base_api_endpoint")
  
  # select which API endpoint to use depending on whether year is provided
  if( is.null(year) ){
    url_endpoint <- paste0(base_api_endpoint,
      "get_source_by_nuts_level/items.json")
  } else if (!is.null(year)) {
    url_endpoint <- paste0(base_api_endpoint,
      "get_source_by_year_nuts_level/items.json")
  }

  # prepare query parameters
  query_params <- list(
    `_level` = level,
    `limit` = limit
  )
  if (!is.null(year)) {
    query_params$`_year` <- year
  }

  response <- httr2::request(url_endpoint) |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      "User-Agent" = getOption("mapineqr.user_agent")
    ) |>
    httr2::req_url_query(!!!query_params) |>
    httr2::req_method("GET") |>
    httr2::req_perform()

  response_data <- httr2::resp_body_json(response, simplifyVector = TRUE) |> 
    tibble::as_tibble() |> 
    dplyr::rename(
      description = .data$f_description,
      source_name = .data$f_resource,
      short_description = .data$f_short_description
    ) |> 
    dplyr::select(
      .data$source_name,
      .data$short_description,
      .data$description
    )
  
  return(response_data)
}

#' Get NUTS level and Year coverage for a specific source
#' 
#' Get the NUTS level and Year coverage for a specific data source.
#' 
#' @param source_name name of the data source
#' 
#' @return a `tibble` containing the following columns:
#' 
#' * `nuts_level`: NUTS level
#' * `year`: year
#' * `source_name`: name of the data source (mathces the `source_name` requested by the user)
#' * `short_description`: short description of the data source
#' * `description`: description of the data source
#' 
#' @importFrom rlang .data
#' @export
#' 
#' @examples
#' \dontrun{
#' mi_source_coverage("BD_HGNACE2_R3")
#' 
#' mi_source_coverage("ghs_smod")
#' }
mi_source_coverage <- function(
  source_name
){
  checkmate::assert_string(source_name)

  # get full source metadata list
  all_sources <- mi_nuts_levels() |> 
    purrr::map(~ mi_sources(level = .x)) |> 
    dplyr::bind_rows() |> 
    unique()
  
  # filter to the specific source
  source_metadata <- all_sources |>
    dplyr::filter(.data$source_name == !!source_name)

  # fail if no source found
  if(nrow(source_metadata) == 0){
    message("Invalid `source_name`, please find valid source names using mi_sources()")
    return(invisible(NULL))
  }

  base_api_endpoint <- getOption("mapineqr.base_api_endpoint")
  url_endpoint <- paste0(base_api_endpoint,
    "get_year_nuts_level_from_source/items.json")

  response <- httr2::request(url_endpoint) |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      "User-Agent" = getOption("mapineqr.user_agent")
    ) |>
    httr2::req_url_query(
      `_resource` = source_name
    ) |>
    httr2::req_method("GET") |>
    httr2::req_perform()
  
  response_data <- httr2::resp_body_json(response, simplifyVector = TRUE) |> 
    tibble::as_tibble() |> 
    dplyr::rename(nuts_level = .data$f_level, year = .data$f_year) |> 
    dplyr::mutate(source_name = source_name) |> 
    dplyr::left_join(source_metadata, by = "source_name") |> 
    dplyr::select(
      .data$nuts_level,
      .data$year,
      .data$source_name,
      .data$short_description,
      .data$description
    )

  return(response_data)
}
