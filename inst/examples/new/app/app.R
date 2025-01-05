library(shiny)
library(rhandsontable)

ui <- fluidPage(
  titlePanel("RHandsontable Test"),

  fluidRow(
    column(8,
           # The table output
           rHandsontableOutput("hot")
    ),
    column(4,
           # Debug/info panel
           h4("Selected Cell Info:"),
           verbatimTextOutput("selection_info"),

           h4("Menu Action Info:"),
           verbatimTextOutput("menu_info"),

           h4("Table Data Sample:"),
           verbatimTextOutput("data_info")
    )
  )
)

server <- function(input, output, session) {

  rv <- reactiveValues()

  # Create sample data
  df <- mtcars[1:5, 1:4]  # Using a subset for clarity

  # Define menu items
  menuItems <- list(
    reorder = list(
      name = "Reorder Columns",
      submenu = list(
        list(
          key = "reorder:asc",
          name = "Ascending Order",
          value = "reorder:asc"
        ),
        list(
          key = "reorder:desc",
          name = "Descending Order",
          value = "reorder:desc"
        )
      )
    ),
    separator = list(
      name = "---------"
    ),
    rename = list(
      name = "Rename Column",
      value = "rename"
    )
  )

  # Render the table
  output$hot <- renderRHandsontable({
    # Create a showcase example
    rhandsontable(mtcars) |>
      readonly_cols("A") |>
      readonly_rows(c(1,3)) |>
      style_cols("A", list(
        backgroundColor = "#E0FFE0"
      ))
  })

  # Display selection information
  output$selection_info <- renderPrint({
    req(input$hot_select)
    str(input$hot_select)
  })

  observeEvent(input$hot, {
    rv$data <- input$hot$data
    rv$params <- input$hot$params
    message("data")
  })

  # Display menu action information
  output$menu_info <- renderPrint({
    req(input$hot_dropdown_action)
    str(input$hot_dropdown_action)
  })

  # Display current table data
  output$data_info <- renderPrint({
    req(input$hot)
    # Just show the first few rows/columns for demonstration
    str(do.call(rbind.data.frame, input$hot$data))
  })

  # Handle menu actions
  observeEvent(input$hot_dropdown_action, {
    action <- input$hot_dropdown_action
    column <- action$column

    # Example of handling different menu actions
    switch(action$action,
           "reorder:asc" = {
             # Handle ascending reorder
             print(paste("Reorder", column, "ascending"))
           },
           "reorder:desc" = {
             # Handle descending reorder
             print(paste("Reorder", column, "descending"))
           },
           "rename" = {
             # Handle rename
             print(paste("Rename", column))
           }
    )
  })
}

shinyApp(ui, server)
