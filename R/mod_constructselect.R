#' constructselect UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_constructselect_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::uiOutput(ns("ui_item_select")),
    shiny::uiOutput(ns("ui_constructs"))
  )
}

#' constructselect Server Function
#'
#' @noRd
mod_constructselect_server <- function(input, output, session, data){
  ns <- session$ns

  rv <- reactiveValues(semaphor = 0,
                       my_items = dplyr::tibble(id = 1:length(names(data)),
                                                items = names(data),
                                                selected = FALSE),
                       my_constructs = dplyr::tibble()
  )


  output$ui_constructs <- renderUI({
    box(
      DT::DTOutput(ns("constructs")),
      shinyjs::disabled(actionButton(ns("remove_item"), "Remove"))
    )
  })

  # render result table ----
  output$constructs <- DT::renderDT({
    rv$my_constructs
  })

  # delete button
  observe({
    toggleState(id = "remove_item", condition =
                  (length(input$constructs_rows_selected) > 0 )
    )
  })

}

## To be copied in the UI
# mod_constructselect_ui("constructselect_ui_1")

## To be copied in the server
# callModule(mod_constructselect_server, "constructselect_ui_1", data)

