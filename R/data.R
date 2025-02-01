#' Get univariate or bivariate data for a specific source
#'
#' Fetches univariate or bivariate data for a given source, year, NUTS level, and selected filters.
#'
#' @param x_source A `character` string specifying the source name for the x variable.
#' @param y_source (Optional) A `character` string specifying the source name for the y variable.
#' @param year A `character` or `integer` specifying the year.
#' @param level A `character` string specifying the NUTS level ("0", "1", "2", or "3").
#' @param x_filters A `named list` where the names are the filter fields for the x variable and the values are the selected values for those fields. Default is an empty list. To find out which filters to use, use \code{\link{mi_source_filters}} with the desired `source_name`.
#' @param y_filters (Optional) A `named list` where the names are the filter fields for the y variable and the values are the selected values for those fields. Default is `NULL`. To find out which filters to use, use \code{\link{mi_source_filters}} with the desired `source_name`.
#' @param limit An `integer` specifying the maximum number of results to return. Default is 2500. This default should be enough for most uses, as it is well above the number of NUTS 3 regions in the EU. The maximum limited by the API is 10000.
#'
#' @return A `tibble` with the following columns:
#' 
#' * `geo`: code for the (NUTS) region at the requested level.
#' * `geo_name`: name of the (NUTS) region at the requested level.
#' * `geo_source`: source (type) of the spatial units at the requested level.
#' * `geo_year`: year of the (NUTS) region at the requested level.
#' * `x_year`: The year of the predictor variable (X), included in bivariate requests.
#' * `y_year` (optional): The year of the outcome variable (Y), included in bivariate requests (only included when `y_source` is provided).
#' * `x`: the value of the univariate variable.
#' * `y` (optional): the value of the y variable (only included when `y_source` is provided).
#' 
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' \donttest{
#' # Univariate example
#' mi_data(
#'   x_source = "TGS00010",
#'   year = 2020,
#'   level = "2",
#'   x_filters = list(isced11 = "TOTAL", sex = "F")
#' )
#'
#' # Bivariate example
#' mi_data(
#'   x_source = "TGS00010",
#'   y_source = "DEMO_R_MLIFEXP",
#'   year = 2020,
#'   level = "2",
#'   x_filters = list(isced11 = "TOTAL", sex = "F"),
#'   y_filters = list(age = "Y2", sex = "F")
#' )
#' }
mi_data <- function(
  x_source,
  y_source = NULL,
  year,
  level,
  x_filters = list(),
  y_filters = NULL,
  limit = 2500
) {
  # Validate inputs
  checkmate::assert_string(x_source)
  checkmate::assert_character(level, len = 1)
  checkmate::assert_list(x_filters, types = c("character", "NULL"))
  checkmate::assert_integerish(year, null.ok = TRUE, max.len = 1)
  checkmate::assert_list(y_filters, types = c("character", "NULL"), null.ok = TRUE)
  checkmate::assert_number(limit, lower = 1, upper = 10000)
  if (!is.null(y_source)) checkmate::assert_string(y_source)
  
  # Build filter JSONs for X and Y
  x_conditions <- lapply(names(x_filters), function(name) {
    list(field = name, value = x_filters[[name]])
  })
  
  x_json <- list(
    source = x_source,
    conditions = x_conditions
  )
  # Minify JSON to remove extra whitespace/newlines
  x_json_string <- jsonlite::minify(
    jsonlite::toJSON(x_json, auto_unbox = TRUE)
  )

  # Check if it's bivariate (Y filters are provided)
  if (!is.null(y_source) && !is.null(y_filters)) {
    y_conditions <- lapply(names(y_filters), function(name) {
      list(field = name, value = y_filters[[name]])
    })
    y_json <- list(
      source = y_source,
      conditions = y_conditions
    )
    y_json_string <- jsonlite::minify(
      jsonlite::toJSON(y_json, auto_unbox = TRUE)
    )
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
    `limit` = limit
  )

  if (is.null(y_source)) {
    query_params$`_year` <- as.character(year)
  } else {
    query_params$`_predictor_year` <- as.character(year)
    query_params$`_outcome_year` <- as.character(year)
  }

  # Add JSON parameters as proper strings so that httr2 can URL encode them automatically
  query_params$`X_JSON` <- x_json_string
  if (!is.null(y_source) && !is.null(y_filters)) {
    query_params$`Y_JSON` <- y_json_string
  }
  
  # Perform API request
  request <- httr2::request(url_endpoint) |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      "User-Agent" = getOption("mapineqr.user_agent")
    ) |>
    httr2::req_url_query(!!!query_params) |>
    httr2::req_method("GET")

  response <- request |> httr2::req_perform()
  
  # Parse response
  response_data <- httr2::resp_body_json(response, simplifyVector = TRUE) |> 
    tibble::as_tibble()
  
  # Check for duplicate values within each geo for x and (if applicable) y.
  duplicate_issues <- response_data |>
    dplyr::group_by(.data$geo) |>
    dplyr::summarise(
      distinct_x = dplyr::n_distinct(.data$x),
      distinct_y = if ("y" %in% names(response_data)) dplyr::n_distinct(.data$y) else NA_integer_,
      .groups = "drop"
    )
  
  x_issue <- any(duplicate_issues$distinct_x > 1)
  y_issue <- if ("y" %in% names(response_data)) any(duplicate_issues$distinct_y > 1) else FALSE
  
  # Only perform additional filter checking if duplicate geos exist
  if (x_issue || y_issue) {
    # --- For the x variable ---
    missing_x_filters <- character(0)
    if (x_issue) {
      available_filters <- mi_source_filters(source_name = x_source, year = year, level = level)
      # Determine which filter fields have more than one option
      multi_option_fields <- available_filters |>
        dplyr::group_by(.data$field) |>
        dplyr::summarise(n_options = dplyr::n_distinct(.data$value), .groups = "drop") |>
        dplyr::filter(.data$n_options > 1) |>
        dplyr::pull(.data$field)
      # Only require filters for those fields with multiple options.
      missing_x_filters <- setdiff(multi_option_fields, names(x_filters))
    }
  
    # --- For the y variable (if applicable) ---
    missing_y_filters <- character(0)
    if (y_issue) {
      available_y_filters <- mi_source_filters(source_name = y_source, year = year, level = level)
      multi_option_y_fields <- available_y_filters |>
        dplyr::group_by(.data$field) |>
        dplyr::summarise(n_options = dplyr::n_distinct(.data$value), .groups = "drop") |>
        dplyr::filter(.data$n_options > 1) |>
        dplyr::pull(.data$field)
      missing_y_filters <- setdiff(multi_option_y_fields, names(y_filters))
    }
  
    # Only raise an error if any missing filter is found among fields with multiple options.
    if (length(missing_x_filters) > 0 || length(missing_y_filters) > 0) {
      msg <- "The API returned duplicate values for some geographic regions. This may indicate that not all necessary filters were specified."
      if (length(missing_x_filters) > 0) {
        msg <- paste0(
          msg,
          "\n\nFor the 'x' variable (source: '", x_source, "'):",
          "\n  The following filter fields (with multiple available options) were not specified: ",
          paste(missing_x_filters, collapse = ", "),
          "\nYou can review available filters by running:\n  mi_source_filters(source_name = '", x_source, "', year = ", year, ", level = '", level, "')"
        )
      }
      if (length(missing_y_filters) > 0) {
        msg <- paste0(
          msg,
          "\n\nFor the 'y' variable (source: '", y_source, "'):",
          "\n  The following filter fields (with multiple available options) were not specified: ",
          paste(missing_y_filters, collapse = ", "),
          "\nYou can review available filters by running:\n  mi_source_filters(source_name = '", y_source, "', year = ", year, ", level = '", level, "')"
        )
      }
      stop(msg)
    }
  }


  
  # Define expected columns based on whether y_source is specified
  if (is.null(y_source)) {
    expected_columns <- c("geo", "geo_name", "geo_source", "geo_year", "data_year", "x")
  } else {
    expected_columns <- c("geo", "geo_name", "geo_source", "geo_year", 
                          "predictor_year", "outcome_year", "x", "y")
  }

  # Check for missing expected columns
  missing_columns <- setdiff(expected_columns, colnames(response_data))

  if (length(missing_columns) > 0) {
    stop("The following expected columns are missing from the response: ", paste(missing_columns, collapse = ", "), ". The API may be down or might have changed. Please try again later. If the error persists, please open an issue on GitHub at <https://github.com/e-kotov/mapineqr/issues>.")
  }

  # Select and reorder columns using dplyr
  response_data <- response_data |> 
    dplyr::select(dplyr::all_of(expected_columns)) |> 
    dplyr::rename_with(~ dplyr::case_when(
      .x == "predictor_year" ~ "x_year",
      .x == "data_year" & !"predictor_year" %in% colnames(response_data) ~ "x_year",
      .x == "outcome_year" ~ "y_year",
      TRUE ~ .x
    ), .cols = dplyr::any_of(c("predictor_year", "outcome_year", "data_year")))
  
  return(response_data)
}
