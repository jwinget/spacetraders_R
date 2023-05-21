library(here)
library(shiny)
library(DBI)
library(pool)
library(reactable)

pool <- pool::dbPool(
  drv = RSQLite::SQLite(),
  dbname = here("db.sqlite")
)

ui <- fluidPage(
  titlePanel(
    textOutput("title_string"),
    windowTitle = "Spacetraders"
    ),
  textOutput("stack_size"),
  fluidRow(
    column(6,
           reactableOutput("ship_select_ui")
           ),
    column(6,
           reactableOutput("waypoint_select_ui")
           ),
  ),
  actionButton("orbit_button", "Orbit"),
  actionButton("dock_button", "Dock"),
  actionButton("refuel_button", "Refuel"),
  actionButton("warp_button", "Warp"),
  actionButton("extract_button", "Extract"),
  actionButton("deliver_button", "Deliver"),
  actionButton("sell_button", "Sell"),
  h3("Agent"),
  tableOutput("agent_table"),
  h3("Contract"),
  reactableOutput("contract_status"),
  h3("Ships"),
  tableOutput("ship_status"),
  h3("Navigation"),
  tableOutput("navigation_overview")
)

server <- function(input, output, session) {
  # Startup
  g <- spacetraders::game(pool = pool)
  base_url <- g$url
  token <- g$token

  requests <- c()

  # API tick
  timer <- reactiveTimer(1000)

  # Reactive values to store the stack and output
  stack <- reactiveVal(c())

  # Update the stack and output at each tick
  observeEvent(timer(), {
    stack(stack() |>
            spacetraders::process_stack(token = g$token,
                          base_url = g$url,
                          pool = pool))
  })

  #---- Inputs ----#
  observeEvent(input$orbit_button, {
    message(glue::glue("{selected_ships()} cleared for departure"))
    stack(spacetraders::orbit(selected_ships(), stack())
    )
  })

  observeEvent(input$dock_button, {
    message(glue::glue("{selected_ships()} entering Docking Bay {sample(1:10, 1)}"))
    stack(spacetraders::dock(selected_ships(), stack())
    )
  })

  observeEvent(input$refuel_button, {
    message(glue::glue("Repleneshing fuel cells on {selected_ships()}"))
    stack(spacetraders::refuel(selected_ships(), stack())
    )
  })

  observeEvent(input$warp_button, {
    message(glue::glue("{selected_ships()} warping to {selected_waypoint()}"))
    stack(spacetraders::warp(selected_ships(), selected_waypoint(), stack()))
  })

  observeEvent(input$extract_button, {
    message(glue::glue("Extracting resources with {selected_ships()}"))
    stack(spacetraders::extract(selected_ships(), stack())
    )
  })

  observeEvent(input$deliver_button, {
    message(glue::glue("Delivering contract resources from {selected_ships()}"))
    stack(spacetraders::deliver(selected_ships(), selected_contract(), cargo(), stack())
    )
  })

  observeEvent(input$sell_button, {
    message(glue::glue("Selling {selected_ships()}'s cargo"))
    cargo <- dplyr::tbl(pool, "cargo") |>
      dplyr::filter(ship_symbol %in% selected_ships()) |>
      dplyr::collect()

    stack(spacetraders::sell(selected_ships(), cargo, stack())
    )
  })

  #---- Outputs ----#
  agent <- reactive({
    invalidateLater(5000)
    dplyr::tbl(pool, "agent") |>
    dplyr::collect()
  })

  contracts <- reactive({
    invalidateLater(10000)
    dplyr::tbl(pool, "contracts") |>
      dplyr::select(contract_id,
                    deliver_symbol,
                    deliver_destination,
                    units_required,
                    units_fulfilled) |>
      dplyr::collect()
  })

  nav <- reactive({
    invalidateLater(10000)
    dplyr::tbl(pool, "navigation") |>
      dplyr::collect()
  })

  systems <- reactive({
    invalidateLater(10000)
    dplyr::tbl(pool, "systems") |>
      dplyr::collect()
  })

  ships <- reactive({
    invalidateLater(10000)
    ships <- dplyr::tbl(pool, "ship_data") |>
      dplyr::collect()
  })

  cargo <- reactive({
    invalidateLater(5000)
    cargo <- dplyr::tbl(pool, "cargo") |>
      dplyr::collect()
  })

  output$agent_table <- renderTable({
    agent()
  })

  output$contract_status <- renderReactable({
    reactable(contracts(),
              selection = "single",
              onClick = "select",
              defaultSelected = c(1))
  })

  selected_contract <- reactive(contracts()[getReactableState("contract_status", "selected"),])

  output$navigation_overview <- renderTable({
    nav()
  })

  output$stack_size <- renderText({
    glue::glue("Pending requests: {length(stack())}")
  })

  output$ship_select_ui <- renderReactable({
    ships() |>
      reactable(selection = "multiple", onClick = "select")
  })

  selected_ships <- reactive(ships()$ship_symbol[getReactableState("ship_select_ui", "selected")])

  output$waypoint_select_ui <- renderReactable({
    # Filter waypoints based on selected ship(s)
    if (is.null(selected_ships)) {
      selected_ships <- ships()$symbol
    }

    systems() |>
      reactable(selection = "single", onClick = "select")
  })

  selected_waypoint <- reactive(systems()$waypoint_symbol[getReactableState("waypoint_select_ui", "selected")])

  output$title_string <- renderText(
    glue::glue("{agent()$symbol} Expeditions")
  )

  onStop(function() {
    pool::poolClose(pool)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
