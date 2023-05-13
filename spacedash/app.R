library(shiny)
library(bslib)

ui <- fluidPage(
  theme = bs_theme(
    bg = "#0d0d0d", fg = "#CBE86B", primary = "#FFF",
    base_font = font_google("Press Start 2P"),
    code_font = font_google("Press Start 2P"),
    "font-size-base" = "1rem", "enable-rounded" = FALSE
  ) |>
    bs_add_rules(
      '@import "https://unpkg.com/nes.css@latest/css/nes.min.css"'
    ),
  title = "SpaceDash",
  subtitle = "SpaceTraders.io dashboard",
  tagList(
    fluidRow(
      column(2,
             wellPanel(
               h2("Credits"),
               textOutput("credit_balance")
             )
            ),
      column(10,
             mainPanel(
               h2("Commands"),
               htmlOutput("ship_select_ui"),
               wellPanel(
                 h3("Navigation"),
                 textInput("waypoint", "WAYPOINT"),
                 actionButton("navigate", "GO"),
                 actionButton("flydock", "GO & DOCK")
               ),
               actionButton("dock", "DOCK"),
               actionButton("refuel", "REFUEL"),
               actionButton("orbit", "ORBIT"),
               actionButton("extract", "EXTRACT"),
               actionButton("unload", "UNLOAD"),
               actionButton("operate", "OPERATE ALL")
             )
            )
    ),
    fluidRow(
    mainPanel(
      h2("Ship status"),
      tableOutput("ship_status")
    )),
    fluidRow(
    mainPanel(
      h2("Contract status"),
      tableOutput("contracts")
    ))
  )
)

server <- function(input, output, session) {
  source(here::here("R", "funs.R"))

  future::plan("multisession")

  char_info <- reactive({
    invalidateLater(60006, session)
    agent_info(token, base_url)
  })

  output$credit_balance <- renderText({
    char_info()$data$credits})

  s_info <- reactive({
    invalidateLater(15005, session)
    ships(token, base_url)$data
  })

  ship_summary <- reactive({
    ship_icons <- c(
      "FRAME_FRIGATE" = '<img src="img/craft_speederB_SE.png"></img>',
      "FRAME_DRONE" = '<img src="img/craft_miner_SE.png"></img>'
    )

    ship_types <- s_info()$frame$symbol

    tibble::tibble(
      symbol = s_info()$symbol,
      #ship_type = ship_icons[ship_types],
      current_waypoint = s_info()$nav$waypointSymbol,
      waypoint_type = s_info()$nav$route$destination$type,
      status = s_info()$nav$status,
      fuel = s_info()$fuel$current,
      cargo_load = glue::glue("{round(100*s_info()$cargo$units / s_info()$cargo$capacity, 2)}%")
    )
  })

  output$ship_status <- renderTable({
    ship_summary()
    })

  contract_summary <- reactive({
    invalidateLater(2E5+25, session)
    my_contracts <- contracts(token, base_url)$data
    d <- tidyr::unnest(my_contracts$terms, cols = "deliver")
    tibble::tibble(
      id = my_contracts$id,
      deliver = d[["tradeSymbol"]],
      percent_complete = 100 * as.numeric(d[["unitsFulfilled"]]) / as.numeric(d[["unitsRequired"]])
    )
  })

  output$contracts <- renderTable({
    contract_summary()})

  # Ad-hoc commands
  output$ship_select_ui <- renderUI({
    selectInput("ship_select", "Select ship:", choices = ship_summary()$symbol)
  })

  observeEvent(input$dock, {
    message(glue::glue("Docking {input$ship_select}!"))
    dock(token, base_url, input$ship_select)
  })

  observeEvent(input$refuel, {
    message(glue::glue("Gassing up {input$ship_select}"))
    refuel(token, base_url, input$ship_select)
  })

  observeEvent(input$orbit, {
    message(glue::glue("{input$ship_select} cleared for departure"))
    orbit(token, base_url, input$ship_select)
  })

  observeEvent(input$extract, {
    message(glue::glue("{input$ship_select} getting to work"))
    extract_until_full(token, base_url, input$ship_select)
  })

  observeEvent(input$unload, {
    message(glue::glue("{input$ship_select} heading to market"))
    unload_cargo(token, base_url, input$ship_select)
  })

  observeEvent(input$operate, {
    message(glue::glue("All ships working against contract {contract_summary()$id[[1]]}"))
    run_swarm(token, base_url)
  })

  observeEvent(input$navigate, {
    message(glue::glue("{input$ship_select} navigating to {input$waypoint}"))
    ship <- input$ship_select
    waypoint <- input$waypoint
    navigate(token, base_url, ship, waypoint)
  })

  observeEvent(input$flydock, {
    message(glue::glue("{input$ship_select} navigating to {input$waypoint} and docking on arrival"))
    ship <- input$ship_select
    waypoint <- input$waypoint
    fly_and_dock(token, base_url, ship, waypoint)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
