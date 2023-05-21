#' Orbit a ship
#'
#'@param ship_symbol A ship symbol
#'@param requests A vector of pending requests
#'
#' @export
orbit <- function(ship_select, requests) {
  purrr::map(ship_select, ~{
    endpoint <- glue::glue("my/ships/{.x}/orbit")

    requests <<- add_request(
      expr <- substitute(
        send_request(method = "POST",
                     token = token,
                     base_url = base_url,
                     endpoint = x
        ),
        list(x = endpoint)
      ),
      requests
    )
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
  purrr::map(ship_select, ~{
    endpoint <- glue::glue("my/ships/{.x}/dock")

    requests <<- add_request(
      expr <- substitute(
        send_request(method = "POST",
                     token = token,
                     base_url = base_url,
                     endpoint = x
        ),
        list(x = endpoint)
      ),
      requests
    )
  })

  return(requests)
}
