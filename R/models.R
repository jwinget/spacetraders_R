#' Parse Agent
#'
#' Parse agent information to database
#'
#' @export
parse_agent <- function(res, pool) {
  if (!is.null(res$error$message)) {
    message("Error fetching agent from the API")
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
    message("Error fetching contracts from the API")
  } else {
    contract_id <- res$id
    faction <- res$faction
    type <- res$type
    deadline <- res$terms$deadline
    payment_on_accept <- res$terms$payment$onAccepted
    payment_on_fulfilled <- res$terms$payment$onFulfilled

    contracts <- purrr::map(res$terms$deliver, ~{
      tibble::tibble(
        contract_id = contract_id,
        faction = faction,
        type = type,
        deadline = deadline,
        payment_on_accept = payment_on_accept,
        payment_on_fulfilled = payment_on_fulfilled,
        deliver_symbol = .x$tradeSymbol,
        deliver_destination = .x$destinationSymbol,
        units_required = .x$unitsRequired,
        units_fulfilled = .x$unitsFulfilled
      )
    }) |>
      dplyr::bind_rows()

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
    message("Error fetching ships from the API")
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
      ship_symbol = res$symbol,
      fuel_current = res$fuel_current,
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
    message("Error fetching system from the API")
  } else {
    system_symbol <- res$symbol
    sector <- res$sectorSymbol
    system_type <- res$type
    this_system <-tibble::tibble(
        system_symbol = system_symbol,
        sector = sector,
        system_type = system_type,
        waypoint_symbol = res$waypoints$symbol,
        waypoint_type = res$waypoints$type
      )

    # Add these to the database if not present
    # "Append" doesn't seem to work in copy_to,
    # So we re-write the entire table :(

    systems <- dplyr::tbl(pool, "systems") |>
      dplyr::collect() |>
      dplyr::bind_rows(this_system) |>
      dplyr::distinct()

    dplyr::copy_to(pool, systems,
                   temporary = FALSE,
                   overwrite = TRUE)
  }
}

#' Parse Waypoints
#'
#' Parse waypoint information to database
#'
#' @export
parse_waypoints <- function(res, pool) {
  if (!is.null(res$error$message)) {
    message("Error fetching waypoints from the API")
  } else {
    waypoints <- tibble::tibble(
      system_symbol = res$systemSymbol,
      waypoint_symbol = res$symbol,
      faction = res$faction$symbol,
      type = res$type,
      charted = res$traits$symbol,
      name = res$traits$name,
      description = res$traits$description
    )

    dplyr::copy_to(pool, waypoints,
                   temporary = FALSE,
                   append = TRUE)
  }
}
