library(here)
library(shiny)

ui <- fluidPage(
  titlePanel("Astral Expeditions"),
  fluidRow(
    column(2,
           textOutput("stack_size")
    ),
    column(8,
           tableOutput("agent_table"))
  )
)

server <- function(input, output, session) {
  pool <- pool::dbPool(
    drv = RSQLite::SQLite(),
    dbname = here("db.sqlite")
  )

  # Startup
  g <- spacetraders::game(pool)
  requests <- c()

  timer <- reactiveTimer(1000)
  # Reactive values to store the stack and output
  stack <- reactiveVal(c())
  output <- reactiveValues()

  # Update the stack and output at each tick
  observeEvent(timer(), {
    now <- Sys.time()
    stack(stack() |>
            spacetraders::process_stack(token = g$token,
                          base_url = g$url,
                          pool = pool))

    output <- spacetraders::update_ui(pool, stack(), output)
  })

  onStop(function() {
    pool::poolClose(pool)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
