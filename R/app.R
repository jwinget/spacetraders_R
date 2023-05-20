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

  output <- spacetraders::tick(game = g,
                 stack = requests,
                 input = input,
                 output = output)

  onStop(function() {
    pool::poolClose(pool)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
