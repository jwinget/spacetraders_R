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
                requests = requests) {

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
    message(glue::glue("{.x} cleared for departure"))
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
    # Check ship cargo to see how many units should be delivered
    deliver_units <- cargo |>
      dplyr::filter(ship_symbol == .x &
                      item_symbol == contract_select$deliver_symbol) |>
      dplyr::pull(item_units)

    if(length(deliver_units > 0)) {
      message(glue::glue("{.x} is making a delivery!"))
      # Have some cargo to deliver
      # Warp to delivery waypoint
      # spacetraders::warp(.x, , contract_select$deliver_destination, requests)
      # Dock, refuel
      # Need to handle travel time here
      # Deliver cargo
      request_body <- list(
        shipSymbol = .x,
        tradeSymbol = contract_select$deliver_symbol,
        units = deliver_units
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
      requests <<- c(requests, expr)

      # Try to sell stuff

      # Orbit when done

    } else {
      message(glue::glue("{.x} does not have any contract cargo to deliver"))
    }
  })
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
    message(glue::glue("{.x} entering Docking Bay {sample(1:10, 1)}"))
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
    message(glue::glue("Extracting resources with {.x}"))
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
    message(glue::glue("Replenishing fuel cells on {.x}"))
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

  # Don't know why but my brain can't process this as a nested purrr operation
  for (i in seq_along(ship_select)) {
    message(glue::glue("Selling {ship_select[i]}'s cargo"))
    ship_cargo <- cargo |>
      dplyr::filter(ship_symbol == ship_select[i])

    endpoint <- gsub("SHIP_SELECT", ship_select[i], endpoint)

    sales <- purrr::map2(ship_cargo$item_symbol,
                ship_cargo$item_units, ~{
                  request_body <- list(
                    symbol = .x,
                    units = .y
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
                }) |>
      unlist()
    return(c(requests, sales))
  }
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
    message(glue::glue("{.x} warping to {waypoint_select}"))
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
