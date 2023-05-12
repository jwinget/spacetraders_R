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
  
  response |>
    httr::content("text") |>
    jsonlite::fromJSON()
}

#---- agent-management ----

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
  total_capacity <- as.integer(cargo$capacity)
  current_load <- as.integer(cargo$units)
  
  while(current_load < total_capacity) {
    res <- extract(token, base_url, ship_id)
    if (!is.null(res$error$code)) {
      # Only update if the extraction worked
      # This tends to error out when the API is under load
      current_load <<- as.integer(res$data$cargo$units)
      message(glue::glue("Ship {ship_id} at {current_load} / {total_capacity} units"))
      flush.console()
      if(current_load == total_capacity) {
        return(TRUE)
      }
    }
    # This not only handles the laser cooldown,
    # but takes a little nap if the server is spitting errors
    Sys.sleep(70 + sample(1:3, 1, replace = TRUE)) # Update to use actual cooldown time
  }
  
  message(glue::glue("Ship {ship_id} full!"))
  flush.console()
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
navigate <- function(token, base_url, ship_id, waypoint_id) {
  json_body <- list(waypointSymbol = waypoint_id)
  
  send_request("POST",
               token,
               base_url,
               glue::glue("my/ships/{ship_id}/navigate"),
               json_body)
  
  Sys.sleep(120 + sample(1:5, 1, replace = TRUE))
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

#---- pastiche-functions ----
#' Unload cargo
#' 
#' Deliver contract items and sell non-contract items
#' 
#' @param token API agent token
#' @param base_url base url of the api
#' @param ship_id A ship id string
#' @param contract_id A contract id string
#' 
#' @export
unload_cargo <- function(token, base_url, ship_id) {
  # What does the contract need?
  contract <- contracts(token, base_url)$data
  contract_id <- contract$id[[1]]
  contract_symbol <- contract$terms$deliver[[1]]$tradeSymbol
  contract_destination <- contract$terms$deliver[[1]]$destinationSymbol
  
  # Ship cargo
  cargo <- tibble::as_tibble(ship_cargo(token,
                      base_url,
                      ship_id)$data$cargo$inventory)
  
  if (nrow(cargo) >= 1) {
    # Dock and sell non-contract goods
    dock(token, base_url, ship_id)
    
    to_sell <- cargo |>
        dplyr::filter(symbol != contract_symbol)
    
    for(i in seq_along(to_sell$symbol)) {
      sell(token, base_url, ship_id, to_sell$symbol[i], to_sell$units[i])
    }
    orbit(token, base_url, ship_id)
  }
  
  # Deliver contract cargo if needed
  if (contract_symbol %in% cargo$symbol) {
    # Navigate to destination
    navigate(token, base_url, ship_id, contract_destination)
    dock(token, base_url, ship_id)
    # Deliver
    available_units <- cargo$units[which(cargo$symbol == contract_symbol)]
    delivery <- deliver(token, base_url, ship_id, contract_id, contract_symbol, available_units)
    orbit(token, base_url, ship_id)
    # Head back to belt
    navigate(token, base_url, ship_id, "X1-DF55-17335A")
  }
}

#' Basic mining loop
#' 
#' Mine until cargo hold is full, then unload and repeat
basic_loop <- function(token, base_url, ship_id, contract_id, belt_waypoint = "X1-DF55-17335A") {
  contract_fulfilled <- FALSE
  while(contract_fulfilled == FALSE) {
    Sys.sleep(sample(1:10, 1, replace = TRUE)) # Stagger starts a little
    extract_until_full(token, base_url, ship_id)
    unload_cargo(token, base_url, ship_id, contract_id)
    navigate(token, base_url, ship_id, belt_waypoint)
    dock(token, base_url, ship_id)
    refuel(token, base_url, ship_id)
    orbit(token, base_url, ship_id)
  }
}

run_swarm <- function(token, base_url, contract_id) {
  drones <- ships(token, base_url)$data$symbol
  message(glue::glue("Starting up {paste(drones, collapse = ', ')}"))
  flush.console()
  future::plan("multisession", workers = length(drones))
  furrr::future_map(drones,
                    ~basic_loop(token = token,
                                base_url = base_url,
                                ship_id = .x,
                                contract_id = contract_id,
                                wait_nav = TRUE))
}
