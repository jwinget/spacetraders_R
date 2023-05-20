library(here)
library(shiny)

ui <- fluidPage(
  fluidRow(
    column(2,
           shiny::textOutput("stack_size")
    )
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
