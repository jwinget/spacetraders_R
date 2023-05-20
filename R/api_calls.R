#' Parse Agent
#'
#' Parse agent information to database
#'
#' @export
parse_agent <- function(res, pool) {
  if (!is.null(res$error$message)) {
    message("Error fetching agent_response from the API")
  } else {
    message("Updating agent")
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
    message("Updating contracts")
    #contracts <- tibble::tibble(
    #
    #)
    #
    #dplyr::copy_to(pool, contracts,
    #               temporary = FALSE)
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
    message("Updating ships")
    #ships <- tibble::tibble(
    #
    #)
    #
    #dplyr::copy_to(pool, ships,
    #               temporary = FALSE)
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
    message("Updating systems")
    #systems <- tibble::tibble(
    #
    #)
    #
    #dplyr::copy_to(pool, systems,
    #               temporary = FALSE)
  }
}
