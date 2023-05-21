#' Send command
#'
#' Generalized command for triggering actions
#'
#' @param endpoint Endpoint of the API to call. Use "VAR" to represent variables
#' @param method Either "GET" or "POST"
#' @param requests A vector of pending calls
#' @param ... Other arguments needed by child functions
#'
#' @export
cmd <- function(endpoint = endpoint,
                method = method,
                requests = requests,
                ...) {
  expr <- substitute(
    send_request(method = x,
                 token = token,
                 base_url = base_url,
                 endpoint = y
                 ),
    list(x = method,
         y = endpoint)
  )

  requests <- expr |>
    add_request(requests)

  assign("requests", requests, parent.frame())
}

#' Orbit a ship
#'
#'@param ship_symbol A ship symbol
#'@param requests A vector of pending requests
#'
#' @export
orbit <- function(ship_select, requests) {
  endpoint <- "my/ships/SHIP_SELECT/orbit"

  requests <- purrr::map(ship_select, ~{
    endpoint <- gsub("SHIP_SELECT", .x, endpoint)
    cmd(endpoint, "POST", requests)
  }) |>
    unlist()

  return(requests)
}

#' Dock a ship
#'
#'@param ship_symbol A ship symbol
#'@param requests A vector of pending requests
#'
#' @export
dock <- function(ship_select, requests) {
  endpoint <- "my/ships/SHIP_SELECT/dock"

  requests <- purrr::map(ship_select, ~{
    endpoint <- gsub("SHIP_SELECT", .x, endpoint)
    cmd(endpoint, "POST", requests)
  }) |>
    unlist()

  return(requests)
}
