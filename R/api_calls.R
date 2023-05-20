#' Parse Agent
#'
#' Parse agent information to database
#'
#' @export
parse_agent <- function(res, pool) {
  if (!is.null(res$error$message)) {
    message("Error fetching agent_response from the API")
  } else {
    agent <- tibble::tibble(
      account_ID = res$accountId,
      symbol = res$symbol,
      headquarters = res$headquarters,
      credits = res$credits,
      faction = res$startingFaction
    )

    dplyr::copy_to(pool, agent,
                   temporary = FALSE,
                   overwrite = TRUE)
  }
}

#' Parse Contracts
#'
#' Parse contract information to database
#'
#' @export
parse_contracts <- function(res, pool) {
  if (!is.null(res$error$message)) {
    message("Error fetching agent_response from the API")
  } else {
    contracts <- tibble::tibble(
      id <- res$id,
      faction <- res$factionSymbol,
      type <- res$type,
      deadline <- res$terms$deadline,
      payment_on_accept <- res$terms.payment.onAccepted,
      payment_on_fulfill <- res$term.payment.onFulfilled,
      deliver_symbol <- res$terms$deliver$tradeSymbol,
      deliver_destination <- res$terms$deliver$destinationSymbol,
      units_required <- res$terms$deliver$unitsRequired,
      units_fulfilled <- res$terms$deliver$unitsFulfilled,
      accepted <- res$accepted,
      fulfilled <- res$fulfilled,
      expiration <- res$expiration,
      accept_deadline <- res$deadlineToAccept
    )

    dplyr::copy_to(pool, contracts,
                   temporary = FALSE,
                   overwrite = TRUE)
  }
}

#' Parse Ships
#'
#' Parse ship information to database
#'
#' @export
parse_ships <- function(res, pool) {
  if (!is.null(res$error$message)) {
    message("Error fetching agent_response from the API")
  } else {
    # Parse that data
    ship_data <- tibble::tibble(
      ship_symbol = res$symbol,
      fuel_current = res$fuel$current,
      fuel_capacity = res$fuel$capacity,
      frame_name = res$frame$name,
    )

    cargo <- purrr::map2(res$symbol,
                res$cargo$inventory,
                ~{
                  tibble::tibble(
                    ship_symbol = rep(.x, times = length(.y$symbol)),
                    item_symbol = .y$symbol,
                    item_name = .y$name,
                    item_description = .y$description,
                    item_units = .y$units
                  )
                }) |>
      dplyr::bind_rows()

    navigation <- tibble::tibble(
      system = res$nav$systemSymbol,
      waypoint = res$nav$waypointSymbol,
      departure_waypoint = res$nav$route$departure$symbol,
      departure_type = res$nav$route$departure$type,
      destination_waypoint = res$nav$route$destination$symbol,
      destination_type = res$nav$route$destination$type,
      arrival_time = res$nav$route$arrival |>
        lubridate::as_datetime() |>
        lubridate::seconds(),
      departure_time = res$nav$route$departureTime |>
        lubridate::as_datetime() |>
        lubridate::seconds(),
      status = res$nav$status,
      flight_mode = res$nav$flightMode
      )

    dplyr::copy_to(pool,
                   df = cargo,
                   temporary = FALSE,
                   overwrite = TRUE)

    dplyr::copy_to(dest = pool,
                   df = navigation,
                   temporary = FALSE,
                   overwrite = TRUE)

    dplyr::copy_to(dest = pool,
                   df = ship_data,
                   temporary = FALSE,
                   overwrite = TRUE)
  }
}

#' Parse Systems
#'
#' Parse system information to database
#'
#' @export
parse_systems <- function(res, pool) {
  if (!is.null(res$error$message)) {
    message("Error fetching agent_response from the API")
  } else {
    #systems <- tibble::tibble(
    #
    #)
    #
    #dplyr::copy_to(pool, systems,
    #               temporary = FALSE)
  }
}
