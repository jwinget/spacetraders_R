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

  additional_args <- match.call(expand.dots = FALSE)$...

  print(additional_args)

  if (is.null(additional_args)) {
    # No need to handle request body etc
    expr <- substitute(
      send_request(method = x,
                   token = token,
                   base_url = base_url,
                   endpoint = y
      ),
      list(x = method,
           y = endpoint
      )
    )
  }

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

#' Deliver contract cargo
#'
#'@param ship_symbol A ship symbol
#'@param requests A vector of pending requests
#'
#' @export
deliver <- function(ship_select, contract_select, cargo, requests) {
  endpoint <- glue::glue("my/contracts/{contract_select$contract_id}/deliver")

  purrr::map(ship_select, ~{
  request_body <- list(
    shipSymbol = .x,
    tradeSymbol = contract_select$deliver_symbol,
    units = cargo |>
      dplyr::filter(ship_symbol == .x &
               item_symbol == contract_select$deliver_symbol) |>
      dplyr::pull(item_units)
  )

    expr <- substitute(
    send_request(method = "POST",
                 token = token,
                 base_url = base_url,
                 endpoint = y,
                 body = x),
    list(x = request_body,
         y = endpoint)

  )
  print(expr)
  requests <<- c(requests, expr)
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

#' Extract resources
#'
#'@param ship_symbol A ship symbol
#'@param requests A vector of pending requests
#'
#' @export
extract <- function(ship_select, requests) {
  endpoint <- "my/ships/SHIP_SELECT/extract"

  requests <- purrr::map(ship_select, ~{
    endpoint <- gsub("SHIP_SELECT", .x, endpoint)
    cmd(endpoint, "POST", requests)
  }) |>
    unlist()

  return(requests)
}

#' Refuel a ship
#'
#'@param ship_symbol A ship symbol
#'@param requests A vector of pending requests
#'
#' @export
refuel <- function(ship_select, requests) {
  endpoint <- "my/ships/SHIP_SELECT/refuel"

  requests <- purrr::map(ship_select, ~{
    endpoint <- gsub("SHIP_SELECT", .x, endpoint)
    cmd(endpoint, "POST", requests)
  }) |>
    unlist()

  return(requests)
}

#' Sell cargo
#'
#'@param ship_symbol A ship symbol
#'@param requests A vector of pending requests
#'
#' @export
sell <- function(ship_select, cargo, requests) {
  endpoint <- "my/ships/SHIP_SELECT/sell"
  purrr::map(cargo, ~{
    request_body <- list(
      symbol = item_symbol,
      units = item_units
    )

    requests <- purrr::map(ship_select, ~{
      endpoint <- gsub("SHIP_SELECT", .x, endpoint)
      expr <- substitute(
        send_request(method = "POST",
                     token = token,
                     base_url = base_url,
                     endpoint = y,
                     body = x),
        list(x = request_body,
             y = endpoint)
      )
    }) |>
      unlist()

  }) |>
    unlist()

  return(requests)
}

#' Warp a ship
#'
#'@param ship_symbol A ship symbol
#'@param waypoint A waypoint symbol
#'@param requests A vector of pending requests
#'
#' @export
warp <- function(ship_select, waypoint_select, requests) {
  endpoint <- "my/ships/SHIP_SELECT/navigate"
  request_body <- list(
    waypointSymbol = waypoint_select
  )

  requests <- purrr::map(ship_select, ~{
    endpoint <- gsub("SHIP_SELECT", .x, endpoint)
    expr <- substitute(
      send_request(method = "POST",
                   token = token,
                   base_url = base_url,
                   endpoint = y,
                   body = x),
      list(x = request_body,
           y = endpoint)
    )
  }) |>
    unlist()

  return(requests)
}
