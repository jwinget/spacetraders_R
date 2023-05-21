library(here)
library(shiny)
library(DBI)
library(pool)

pool <- pool::dbPool(
  drv = RSQLite::SQLite(),
  dbname = here("db.sqlite")
)

ui <- fluidPage(
  titlePanel(
    textOutput("title_string"),
    windowTitle = "Spacetraders"
    ),
  uiOutput("ship_select_ui"),
  actionButton("orbit_button", "Orbit"),
  actionButton("dock_button", "Dock"),
  textOutput("stack_size"),
  tableOutput("agent_table"),
  tableOutput("contract_status"),
  tableOutput("ship_status"),
  tableOutput("navigation_overview")
)

server <- function(input, output, session) {
  # Startup
  g <- spacetraders::game(pool)
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
    message(glue::glue("{input$ship_select} cleared for departure"))
    stack(spacetraders::orbit(ship_symbol = input$ship_select,
                        requests = stack())
    )
  })

  observeEvent(input$dock_button, {
    message(glue::glue("{input$ship_select} entering Bay {sample(1:10, 1)}"))
    stack(spacetraders::dock(ship_symbol = input$ship_select,
                              requests = stack())
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
      dplyr::select(faction,
                    type,
                    deliver_symbol,
                    deliver_destination,
                    units_required,
                    units_fulfilled) |>
      dplyr::collect()
  })

  nav <- reactive({
    invalidateLater(4000)
    dplyr::tbl(pool, "navigation") |>
      dplyr::collect()
  })

  ships <- reactive({
    invalidateLater(10000)
    dplyr::tbl(pool, "ships") |>
      dplyr::collect()
  })

  output$agent_table <- renderTable({
    agent()
  })

  output$contract_status <- renderTable({
    contracts()
  })

  output$navigation_overview <- renderTable({
    nav()
  })

  output$stack_size <- renderText({
    glue::glue("Pending requests: {length(stack())}")
  })

  output$ship_select_ui <- renderUI({
    checkboxGroupInput("ship_select",
                "Select ship:",
                choices = ships()$ship_symbol)
  })

  output$ship_status <- renderTable({
    dplyr::tbl(pool, "ships") |>
      dplyr::collect()
  })

  output$title_string <- renderText(
    glue::glue("{agent()$symbol} Expeditions")
  )

  onStop(function() {
    pool::poolClose(pool)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
