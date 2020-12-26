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

  rv <- reactiveValues(semaphor = 0,
                       my_items = dplyr::tibble(id = 1:length(names(my_data)),
                                         items = names(my_data),
                                         selected = FALSE),
                       my_constructs = dplyr::tibble()
  )

  ## items ----
  items <- reactive({
    rv$semaphor
    rv$my_items
  })

  remaining_items <- reactive({
    items() %>% dplyr::filter(selected == FALSE)
  })


  # outputs ----
  output$item_select <- DT::renderDT({
    remaining_items()
  })

  # generate the main UI ----
  output$ui_item_select <- renderUI({
    shinydashboard::box(DT::DTOutput("item_select"),
        textInput("construct_name", "Name of Construct"),
        shinyBS::bsTooltip(id = "construct_name", title = "Please name your construct. Requires at least 3 characters."),
        disabled(actionButton("create_construct", "Create Construct")))
  })

  output$ui_constructs <- renderUI({
    box(
      DT::DTOutput("constructs"),
      disabled(actionButton("remove_item", "Remove"))
    )
  })


  # render result table ----
  output$constructs <- DT::renderDT({
    rv$my_constructs
  })


  # ensure button works ----
  observe({
    toggleState(id = "create_construct", condition =
                  (length(input$item_select_rows_selected) > 0 &&
                     nchar(input$construct_name) >= 3)
    )
  })

  # delete button
  observe({
    toggleState(id = "remove_item", condition =
                  (length(input$constructs_rows_selected) > 0 )
    )
  })


  observeEvent(input$remove_item, {
    # which are selected
    sels <- input$constructs_rows_selected

    # get their ids
    selected_items <- rv$my_constructs[sels, ]$id

    # make them available again
    rv$my_items <- rv$my_items %>% dplyr::mutate(selected = dplyr::case_when(id %in% selected_items ~ FALSE,
                                                               TRUE ~ selected))

    # remove from constructs
    rv$my_constructs <- rv$my_constructs %>% filter(id %nin% selected_items)
    rv$semaphor = rv$semamphor + 1
  })


  # create new construct ----
  observeEvent(input$create_construct, {
    # get row numbers
    sels <- input$item_select_rows_selected

    # get ids
    selected_items <- remaining_items()[sels,]$id

    rows <- dplyr::tibble(id = selected_items,
                   item = remaining_items()[sels, ]$items,
                   construct = input$construct_name)

    # change availability
    rv$my_items <- rv$my_items %>% dplyr::mutate(selected = dplyr::case_when(id %in% selected_items ~ TRUE,
                                                               TRUE ~ selected))

    # add to constructs
    rv$my_constructs <- rv$my_constructs %>% dplyr::bind_rows(rows)

    # clear input
    updateTextInput(session, "construct_name", value = "")
    rv$semaphor = rv$semamphor + 1


  })


  construct_gr <- reactive({
    res <- ""


    res
  })




  # PLOTTING ----
  output$grcode <- shiny::renderText({

    #plot_model(bs_model)
  })

  output$construct_plot <- DiagrammeR::renderGrViz({

    pls_model %>% seminr::dot_graph() %>% DiagrammeR::grViz()
    #plot_constructs(rv$my_constructs) %>% grViz()
  })

  callModule(mod_constructselect_server, "constructselect_ui_1", my_data)

}
