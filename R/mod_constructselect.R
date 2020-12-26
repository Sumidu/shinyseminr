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
 
  )
}
    
#' constructselect Server Function
#'
#' @noRd 
mod_constructselect_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_constructselect_ui("constructselect_ui_1")
    
## To be copied in the server
# callModule(mod_constructselect_server, "constructselect_ui_1")
 
