#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import shinyjs
#' @import shinyBS
#' @noRd
app_server <- function(input, output, session) {

  # List the first level callModules here
  # SRV: reactives ----

  # Interemediate Code ----
  measurements <- seminr::constructs(
    seminr::reflective("Image",       seminr::multi_items("IMAG", 1:5)),
    seminr::reflective("Expectation", seminr::multi_items("CUEX", 1:3)),
    seminr::reflective("Loyalty",     seminr::multi_items("CUSL", 1:3)),
    seminr::reflective("Complaints",  seminr::single_item("CUSCO"))
  )

  structure <- seminr::relationships(
    seminr::paths(from = c("Image", "Expectation"), to = c("Complaints", "Loyalty")
    )
  )

  data("mobi", package = "seminr")
  my_data <- mobi



  pls_model <- seminr::estimate_pls(data = mobi, measurements, structure)
  bs_model <- seminr::bootstrap_model(pls_model, seed = 1)

  res <- callModule(mod_multi_column_select_server, "multi_column_select_ui_1", my_data)


  # PLOTTING ----
  output$construct_code <- shiny::renderText({
    paste(res(), collapse = "\n")

  })

  output$construct_plot <- DiagrammeR::renderGrViz({

    seminr::dot_graph.measurement_model(res(), title = "Preview") %>% DiagrammeR::grViz()
    #plot_constructs(rv$my_constructs) %>% grViz()
  })

  #res <- callModule(mod_constructselect_server, "constructselect_ui_1", my_data)




  #print(res)
}
