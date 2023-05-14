# Functions for spacetraders.io

#---- constants ----

#' Constants
#'
#' token.txt should contain your spacetraders.io agent token
#' base_url is set to v2 of the spacetraders.io api
#' @export
token <- readLines(here::here("token.txt"))
base_url <- "https://api.spacetraders.io/v2"

#---- utilities ----

#' Send request
#'
#' Sends a request to the spacetraders.io api
#'
#' @param method One of "GET" or "POST"
#' @param token API agent token
#' @param base_url base url of the api
#' @param endpoint API endpoint as a string
#' @param json_body Payload if needed, provided as a named list
#'
#' @export
send_request <- function(method, token, base_url, endpoint, json_body = NULL) {
  headers <- httr::add_headers(Authorization = glue::glue("Bearer {token}"))
  url <- glue::glue("{base_url}/{endpoint}")

  response <- switch(method,
                     GET = httr::GET(url, headers),
                     POST = httr::POST(url, headers, body = json_body, encode = "json"),
                     error = stop("Invalid method"))

  Sys.sleep(0.77) # Avoid hitting the limiter
  response |>
    httr::content("text") |>
    jsonlite::fromJSON()
}

#---- agent-management ----

#' New agent
#'
#' Create a new agent
#'
#' @param base_url base url of the api
#' @param symbol Agent symbol/name
#'
#' @export
new_agent <- function(base_url, symbol, faction = "COSMIC", overwrite = FALSE) {
  json_body <- list(symbol = symbol,
                    faction = faction)

  result <- send_request("POST",
               token, base_url, glue::glue("register"), json_body)

  token <- result$data$token

  if (file.exists("token.txt") & overwrite == FALSE) {
    break('"token.txt" is present in this directory and you have `overwrite` as FALSE.\nTo overwrite your existing agent, call this command with `overwrite = TRUE`')
  } else {
    write.csv(token, file = "token.txt")
  }
}

#' Agent info
#'
#' Returns information about your agent
#'
#' @param token API agent token
#' @param base_url base url of the api
#'
#' @export
agent_info <- function(token, base_url) {
  send_request("GET",
               token,
               base_url,
               glue::glue("my/agent"))
}

#' Ship info
#'
#' Returns information about your ships
#'
#' @param token API agent token
#' @param base_url base url of the api
#'
#' @export
ships <- function(token, base_url) {
  send_request("GET",
               token,
               base_url,
               glue::glue("my/ships"))
}

#' Ship cargo
#'
#' Get information about the cargo of a ship
#'
#' @param token API agent token
#' @param base_url base url of the api
#' @param ship_id A ship id string
#'
#' @export
ship_cargo <- function(token, base_url, ship_id) {
  send_request("GET",
               token,
               base_url,
               glue::glue("my/ships/{ship_id}"))
}

#' Contract info
#'
#' Returns information about your active contracts
#'
#' @param token API agent token
#' @param base_url base url of the api
#'
#' @export
contracts <- function(token, base_url) {
  send_request("GET",
               token,
               base_url,
               glue::glue("my/contracts"))
}

#---- scheduling ----
# WIP

#' API/Command scheduling
#'
#' Iterates no faster than once a second
#'
#' @export
op_cycle <- function(...) {
  # Get the time
  the_time <- Sys.time()

  # Do stuff every second
  d <- update_data(token, base_url, current_data)
  to_do <- process_data(token, base_url, d)
  execute_commands(token, base_url, d)

  # Wait until at least a second has elapsed
  now <- Sys.time()
  cycle_time <- now - the_time
  if (cycle_time < 1) {
    Sys.sleep(1 - cycle_time)
  }
}

#' Update unless error
#'
#' Try to query the API. Return updated data if the query succeds
#' otherwise keep original data
#' @param token API agent token
#' @param base_url base url of the api
#'
#' @export
update_unless_error <- function(method, token, base_url, endpoint, d = NULL, json_body = NULL) {
  res <- send_request(method, token, base_url, endpoint, json_body)

  if (res$error) {
    # Don't update
    return(d)
  } else {
    return(res$data)
  }
}


#' Query API
#'
#' Pull down data at once for processing
#'
#' @param token API agent token
#' @param base_url base url of the api
#'
#' @export
update_data <- function(token, base_url, current_data) {
  # This might be slowed by API limits, so don't run more than once every 10s
  agent_data %<-% update_unless_error(token,
                                    base_url,
                                    endpoint = "my/agent",
                                    d = current_data$agent)

  contract_data %<-% update_unless_error(token,
                                   base_url,
                                   endpoint = "my/contracts",
                                   d = current_data$contracts)

  ship_data %<-% update_unless_error(token,
                                   base_url,
                                   endpoint = "my/ships",
                                   d = current_data$ships)

  return(list(
    agent = agent_data,
    contracts = contract_data,
    ships = ship_data
  ))
}

#' Process data retrieved from API
#'
#' @param token API agent token
#' @param base_url base url of the api
#'
#' @export
process_data <- function(token, base_url, d) {
  # Contract status

  # Ship status
}

#---- navigation-and-operation ----

#' Dock a ship
#'
#' Docks a ship at its current location
#'
#' @param token API agent token
#' @param base_url base url of the api
#' @param ship_id A ship id string
#'
#' @export
dock <- function(token, base_url, ship_id) {
  send_request("POST",
               token,
               base_url,
               glue::glue("my/ships/{ship_id}/dock"))
}

#' Refuel a ship
#'
#' Refuels a ship at its current location. Ship must be docked first.
#'
#' @param token API agent token
#' @param base_url base url of the api
#' @param ship_id A ship id string
#'
#' @export
refuel <- function(token, base_url, ship_id) {
  send_request("POST",
               token,
               base_url,
               glue::glue("my/ships/{ship_id}/refuel"))
}

#' Orbit
#'
#' Orbit or undock a ship at its current location.
#'
#' @param token API agent token
#' @param base_url base url of the api
#' @param ship_id A ship id string
#'
#' @export
orbit <- function(token, base_url, ship_id) {
  send_request("POST",
               token,
               base_url,
               glue::glue("my/ships/{ship_id}/orbit"))
}

#' Extract resources
#'
#' Extract resources at the current location
#'
#' @param token API agent token
#' @param base_url base url of the api
#' @param ship_id A ship id string
#'
#' @export
extract <- function(token, base_url, ship_id) {
  send_request("POST",
               token,
               base_url,
               glue::glue("my/ships/{ship_id}/extract"))
  Sys.sleep(70)
}

#' Navigate
#'
#' Navigate ship to a waypoint
#'
#' @param token API agent token
#' @param base_url base url of the api
#' @param ship_id A ship id string
#' @param waypoint_id Destination waypoint id string
#'
#' @export
navigate <- function(token, base_url, ship_id, waypoint_id) {
  json_body <- list(waypointSymbol = waypoint_id)

  nav <- send_request("POST",
               token,
               base_url,
               glue::glue("my/ships/{ship_id}/navigate"),
               json_body)


  Sys.sleep(120)
}

#---- market ----

#' Get market info
#'
#' Retrieve information about the market at a waypoint
#'
#' @param token API agent token
#' @param base_url base url of the api
#' @param system_id A system id string
#' @param waypoint_id A waypoint id string
#'
#' @export
market <- function(token, base_url, system_id, waypoint_id) {
  send_request("GET", token, base_url, glue::glue("systems/{system_id}/waypoints/{waypoint_id}/market"))
}

#' Sell an item
#'
#' Sell an item at the current location
#'
#' @param token API agent token
#' @param base_url base url of the api
#' @param ship_id A ship id string
#' @param symbol The market symbol for the item to sell
#' @param units Number of units to sell. Integer.
#'
#' @export
sell <- function(token, base_url, ship_id, symbol, units) {
  json_body <- list(symbol = symbol,
                    units = units)

  send_request(method = "POST",
               token,
               base_url,
               glue::glue("my/ships/{ship_id}/sell"),
               json_body)
}

#' Deliver contract items
#'
#' Deliver contract items to current location
#'
#' @param token API agent token
#' @param base_url base url of the api
#' @param ship_id A ship id string
#' @param contract_id A contract id string
#' @param contract_symbol The market symbol for the item to sell
#' @param units Number of units to deliver. Integer.
#'
#' @export
deliver <- function(token, base_url, ship_id, contract_id, contract_symbol, units) {
  json_body <- list(shipSymbol = ship_id,
         tradeSymbol = contract_symbol,
         units = units)

  send_request("POST", token, base_url, glue::glue("my/contracts/{contract_id}/deliver"), json_body)
}

#---- multi-functions ----
#' Fly and dock
#'
#' Fly a ship to a waypoint and dock there
#'
#' @param token API agent token
#' @param base_url base url of the api
#' @param ship_id A ship id string
#' @param waypoint_id Destination waypoint id string
#'
#' @export
fly_and_dock <- function(token, base_url, ship_id, waypoint_id) {
  f <- future::future(
    navigate(token, base_url, ship_id, waypoint_id)
  )
  while(!future::resolved(f)) {
    Sys.sleep(1)
  }
  nav <- value(f)
  dock(token, base_url, ship_id)
}


#' Extract until full
#'
#' Extract resources at the current location until the ship's cargo is full
#'
#' @param token API agent token
#' @param base_url base url of the api
#' @param ship_id A ship id string
#'
#' @export
extract_until_full <- function(token, base_url, ship_id) {
  cargo <- ship_cargo(token, base_url, ship_id)$data$cargo
  message(glue::glue("Capacity: {cargo$capacity}, Units: {cargo$units}"))
  flush.console()

  if (cargo$capacity == cargo$units) {
    return(TRUE)
  } else {
    f <- future::future(
      extract(token, base_url, ship_id)
    )
    while (!future::resolved(f)) {
      Sys.sleep(1)
    }

    extract_until_full(token, base_url, ship_id)
  }
}

#' Unload cargo
#'
#' Deliver contract items and sell non-contract items
#'
#' @param token API agent token
#' @param base_url base url of the api
#' @param ship_id A ship id string
#'
#' @export
unload_cargo <- function(token, base_url, ship_id) {
  my_contracts <- contracts(token, base_url)$data
  to_deliver <- dplyr::bind_rows(my_contracts$terms$deliver)
  to_deliver$contract_id <- my_contracts$id

  ship_info <- ships(token, base_url)$data
  origin_waypoint <- ship_info$nav$waypointSymbol[ship_info$symbol == ship_id]

  cargo <- ship_cargo(token, base_url, ship_id)$data$cargo$inventory

  if(length(intersect(cargo$symbol, to_deliver$tradeSymbol)) >= 1) {
    # We have stuff to deliver
    message(glue::glue("Making deliveries!"))
    flush.console()
    d <- dplyr::left_join(to_deliver, cargo, by = c("tradeSymbol" = "symbol"))
    # Operate over each contract
    deliveries <- list(contract_id = d$contract_id,
                       contract_symbol = d$tradeSymbol,
                       waypoint_id = d$destinationSymbol,
                       units = d$units)
    purrr::pmap(deliveries, function(contract_id, contract_symbol, waypoint_id, units) {
      # Travel to drop-off
      if(!waypoint_id == origin_waypoint) {
        message(glue::glue("Flying to lovely {waypoint_id}"))
        flush.console()
        f <- future::future(navigate(token, base_url, ship_id, waypoint_id))
        while(!future::resolved(f)) {
          Sys.sleep(1)
        }
        nav <- future::value(f)
      }
      dock(token, base_url, ship_id)
      # Dock, fuel up, and deliver the goods
      message(glue::glue("Dropping off {contract_symbol} at {waypoint_id}"))
      flush.console()
      refuel(token, base_url, ship_id)
      deliver(token, base_url, ship_id, contract_id, contract_symbol, units)
    })
  }
  Sys.sleep(0.5)
  # Sell other stuff
  dock(token, base_url, ship_id)
  message(glue::glue("Selling stuff"))
  flush.console()
  cargo <- ship_cargo(token, base_url, ship_id)$data$cargo$inventory
  purrr::map2(cargo$symbol, cargo$units, ~{
    print(glue::glue("Selling {.x}"))
    sell(token, base_url, ship_id, .x, as.integer(.y))
  })
  Sys.sleep(0.5)
  # Orbit and return to original waypoint
  message(glue::glue("Returning to {origin_waypoint}"))
  flush.console()

  orbit <- future::future(orbit(token, base_url, ship_id)$data) |> future::value()

  current_location <- orbit$nav$waypointSymbol

  if(is.null(current_location) | is.null(origin_waypoint)) {
    # Travel
    f <- future::future(navigate(token, base_url, ship_id, origin_waypoint))
    while(!future::resolved(f)) {
      Sys.sleep(0.5)
    }
    nav <- future::value(f)
  } else if (!current_location == origin_waypoint) {
    message(glue::glue("{current_location} -> {origin_waypoint}"))
    flush.console()
    # Travel
    f <- future::future(navigate(token, base_url, ship_id, origin_waypoint))
    while(!future::resolved(f)) {
      Sys.sleep(0.5)
    }
    nav <- future::value(f)
  }

  message(glue::glue("Cargo run complete!"))
  flush.console()
}

#' Extraction loop
#'
#' An action loop to extract, deliver, and sell resources
#'
#' @param token API agent token
#' @param base_url base url of the api
#' @param ship_id Ship ID string
#' @param resource_waypoint Waypoint ID for resource belt
#'
#' @export
extraction_loop <- function(token, base_url, ship_id) {
  # Extract until full
  message(glue::glue("Extracting"))
  flush.console()
  extract_until_full(token, base_url, ship_id)

  # Deliver/sell cargo
  message(glue::glue("Delivering"))
  flush.console()
  unload_cargo(token, base_url, ship_id)
}


#' Run swarm
#'
#' Automate basic mining loop for all ships
#' @param token API agent token
#' @param base_url base url of the api
#' @param resource_waypoint Waypoint ID for resource belt
#'
#' @export
run_swarm <- function(token, base_url) {
  my_ships <- ships(token, base_url)$data$symbol
  future::plan("multisession", workers = length(my_ships)*2)
  while(TRUE) {
    furrr::future_map(my_ships, ~extraction_loop(token, base_url, .x))
  }
}
