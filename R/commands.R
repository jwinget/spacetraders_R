#' Orbit a ship
#'
#'@param ship_symbol A ship symbol
#'@param requests A vector of pending requests
#'@param game A game object
#'
#' @export
orbit <- function(ship_symbol, requests) {

  endpoint <- glue::glue("my/ships/{ship_symbol}/orbit")

  requests <- add_request(
    expr <- rlang::expr(
      send_request(method = "POST",
                   token = token,
                   base_url = base_url,
                   endpoint = {{ endpoint }}
      )
    ),
    requests
  )

  return(requests)
}

#' Dock a ship
#'
#'@param ship_symbol A ship symbol
#'@param requests A vector of pending requests
#'@param game A game object
#'
#' @export
dock <- function(ship_symbol, requests) {

  endpoint <- glue::glue("my/ships/{ship_symbol}/dock")

  requests <- add_request(
    expr <- rlang::expr(
      send_request(method = "POST",
                   token = token,
                   base_url = base_url,
                   endpoint = {{ endpoint }}
      )
    ),
    requests
  )

  return(requests)
}
