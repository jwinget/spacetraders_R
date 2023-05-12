library(nessy)

ui <- cartridge(
  title = "SpaceDash",
  subtitle = "SpaceTraders.io dashboard",
  container_with_title(
    "Credits",
    textOutput("credit_balance")
  ),
  container_with_title(
    "Ship status",
    tableOutput("ship_status")
  ),
  container_with_title(
    "Contract status",
    tableOutput("contracts")
  ),
  container_with_title(
    "Commands",
    htmlOutput("ship_select_ui"),
    actionButton("dock", "DOCK"),
    actionButton("refuel", "REFUEL"),
    actionButton("orbit", "ORBIT"),
    actionButton("extract", "EXTRACT"),
    actionButton("unload", "UNLOAD"),
    actionButton("operate", "OPERATE ALL"),
    balloon_container(
      "Navigation",
      textInput("waypoint", "WAYPOINT"),
      actionButton("navigate", "GO")
    )
  )
)

server <- function(input, output, session) {
  source(here::here("R", "funs.R"))
  
  future::plan("multisession", workers = 5)
  
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
    
  ship_summary <- reactive({tibble::tibble(
      symbol = s_info()$symbol,
      current_waypoint = s_info()$nav$waypointSymbol,
      waypoint_type = s_info()$nav$route$destination$type,
      status = s_info()$nav$status,
      fuel = s_info()$fuel$current
    )
  })
  
  output$ship_status <- renderTable({
    ship_summary()})
  
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
    dispose_cargo(token, base_url, input$ship_select, contract_summary()$id[[1]])
  })

  observeEvent(input$operate, {
    message(glue::glue("All ships working against contract {contract_summary()$id[[1]]}"))
    run_swarm(token, base_url, contract_summary()$id[[1]])
  })
  
  observeEvent(input$navigate, {
    message(glue::glue("{input$ship_select} navigating to {input$waypoint}"))
    navigate(token, base_url, input$ship_select, input$waypoint)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
