#' Start up
#'
#' Initialize the basic environment needed for the game
#'
#' @param new_game Force a new game?
#' @param token An optional agent token as provided by the \code{register} endpoint. If not provided the function will try to read from the local database. If neither are found, will create a new agent.
#' @param symbol Your agent name
#' @param faction Faction to join. See https://docs.spacetraders.io/game-concepts/agents-and-factions
#' @export
game <- function(new_game = FALSE,
                 token = NULL,
                 symbol = NULL,
                 faction = "COSMIC",
                 pool = NULL) {
  base_url <- "https://api.spacetraders.io/v2"

  if(is.null(pool)) {
    pool <<- pool::dbPool(
      drv = RSQLite::SQLite(),
      dbname = here("db.sqlite")
    )
  }

  if (isTRUE(new_game)) {
    # Clear the database
    purrr::map(DBI::dbListTables(pool),
               ~DBI::dbRemoveTable(pool, .x))

  }

  if (is.null(token)) {
    if (DBI::dbExistsTable(pool, "credentials")) {
      token <- dplyr::tbl(pool, "credentials") |>
        dplyr::pull("token")

      # Get symbol and faction
      message("Token loaded from database")

      if (DBI::dbExistsTable(pool, "agent")) {
        message("Agent loaded from database")
        symbol <- dplyr::tbl(pool, "agent")$symbol
        faction <- dplyr::tbl(pool, "agent")$faction
      } else {
        "Fetching agent info"
        agent_json <- send_request(method = "GET",
                     token = token,
                     base_url = base_url,
                     endpoint = "my/agent"
        )
      }

    } else {
      # If not, initialize
      message("Requesting registration for a new agent.")
      if (is.null(symbol)) {
        symbol <- readline("What is your agent's name? ")
        faction <- readline("What faction should they join? ")
      }

      # Send request
      register_body <- list(
        symbol = symbol,
        faction = faction
      )
      register_json <- send_request("POST",
                                 token = NULL,
                                 base_url = base_url,
                                 endpoint = "register",
                                 body = register_body)

      if (is.null(register_json$data$accountId)) {
        message("Error registering")
        message(register_json$error$message)
      } else {
        token <- register_json$data$token
        writeLines(token, con = "token.txt")
        message(glue::glue("Token saved to token.txt"))
        # Write token to DB
        credentials <- tibble::tibble(token = token)
        dplyr::copy_to(pool, credentials,
                       temporary = FALSE)

        agent_json <- send_request(method = "GET",
                                   token = token,
                                   base_url = base_url,
                                   endpoint = "my/agent"
        )
      }

    }
  }

  # Make sure to write the agent info to db
  if (!DBI::dbExistsTable(pool, "agent")) {
    spacetraders::parse_agent(agent_json, pool)
  }

  return(
    list(
      "token" = token,
      "url" = base_url,
      "db" = pool
    )
  )
}

#' Send request
#'
#' Sends a request to the spacetraders.io api
#'
#' @param method One of "GET" or "POST"
#' @param token API agent token
#' @param base_url base url of the api
#' @param endpoint API endpoint as a string
#' @param body Payload if needed, provided as a named list
#'
#' @export
send_request <- function(method, token, base_url, endpoint, body = NULL) {
  headers <- httr::add_headers(Authorization = glue::glue("Bearer {token}"))
  url <- glue::glue("{base_url}/{endpoint}")

  response <- switch(method,
                     GET = httr::GET(url, headers),
                     POST = httr::POST(url, headers, body = body, encode = "json"),
                     error = stop("Invalid method"))

  res <- response |>
    httr::content("text") |>
    jsonlite::fromJSON()

  if (!is.null(res$error$message)) {
    message(glue::glue("Error: {res$error$message}"))
    return(NULL)
  }

  return(res)
}

#' Add request
#'
#' Add a request to the stack
#'
#' @param expr An unevaluated expression
#' @param requests A vector of requests
#'
#' @export
add_request <- function(expr, requests) {
  #message(glue::glue("Adding {deparse(expr)}"))
  flush.console()
  return(
    c(requests, expr)
  )
}

#' Process stack
#'
#' Execute the top request in the stack
#'
#' @param requests A vector of requests to process
#'
#' @export
process_stack <- function(requests = NULL, token, base_url, pool) {
  if (length(requests) == 0) {
    message("Refreshing database")

    # Get agent data
    requests <- add_request(
      agent_data <- rlang::expr(
        send_request(method = "GET",
                     token = token,
                     base_url = base_url,
                     endpoint = "my/agent"
                     )
        ),
      requests
    )

    # Get ship data
    requests <- add_request(
      ship_data <- rlang::expr(
        send_request(method = "GET",
                     token = token,
                     base_url = base_url,
                     endpoint = "my/ships"
        )
      ),
      requests
    )

    # Get system data for current locations
    ship_locations <- dplyr::tbl(pool, "navigation") |>
      dplyr::pull(system)

    endpoint <- "systems/LOCATION"
    requests <- purrr::map(ship_locations, ~{
      endpoint <- gsub("LOCATION", .x, endpoint)
      add_request(
        contract_data <- substitute(
          send_request(method = "GET",
                       token = token,
                       base_url = base_url,
                       endpoint = x
          ),
          list(x = endpoint)
        ),
        requests
      )
    }) |>
      unlist()

    # Get contract data
    requests <- add_request(
      contract_data <- rlang::expr(
        send_request(method = "GET",
                     token = token,
                     base_url = base_url,
                     endpoint = "my/contracts"
        )
      ),
      requests
    )
  return(requests)

  }

  response <- eval(requests[[1]])

  if(!is.null(response$error$message)) {
    shiny::showNotification(glue::glue("Error: {response$error$message}"))

  } else if (length(requests) > 0) {
  # Call appropriate db parse tool based on endpoint
    endpoint <- requests[[1]]$endpoint
    endpoint_map <- c(
      "my/agent" = "parse_agent",
      "my/ships" = "parse_ships",
      "my/contracts" = "parse_contracts"
    )

    if (endpoint %in% names(endpoint_map)) {
    expr_str <- glue::glue("{endpoint_map[[endpoint]]}({response[1]}, pool)")
    expr <- parse(text = expr_str)

    eval(expr)
    } else if (stringr::str_detect(endpoint, "system")) {
      #message(glue::glue("Parsing system information: {endpoint}"))

    expr_str <- glue::glue("parse_systems({response[1]}, pool)")
    expr <- parse(text = expr_str)

    eval(expr)
    } else {
      message(glue::glue("Triggering {endpoint}"))
    }
  }

  requests <- requests[-1]
  return(requests)
}
