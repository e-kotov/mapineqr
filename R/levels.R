#' Get a list of available NUTS levels
#' 
#' @return a `character` vector of valid NUTS levels that will be accepted by other functions.
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' mi_nuts_levels()
#' }
#' 
mi_nuts_levels <- function(){
  base_api_endpoint <- getOption("mapineqr.base_api_endpoint")
  url_endpoint <- paste0(base_api_endpoint,
    "get_levels/items.json")

  response <- httr2::request(url_endpoint) |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      "User-Agent" = getOption("mapineqr.user_agent")
    ) |>
    httr2::req_method("GET") |>
    httr2::req_perform()

  response_data <- httr2::resp_body_json(response) |> unlist()
  return(response_data)
}
