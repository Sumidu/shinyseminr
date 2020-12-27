#' multi_column_select UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param data the data frame to select from
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_multi_column_select_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinydashboard::box(title = "Select Constructs", solidHeader = T, width = 6, status = "primary",
      splitLayout(
      shiny::uiOutput(ns("ui_item_select")),
      shiny::uiOutput(ns("ui_constructs"))
      )
    )
  )
}

#' multi_column_select Server Function
#'
#' @noRd
mod_multi_column_select_server <- function(input, output, session, data){
  ns <- session$ns

  type_choices <- c("Reflective", "Composite (Mode A - correlation weights)", "Composite (Mode B - regression weights)")

  rv <- reactiveValues(semaphor = 0,
                       my_items = dplyr::tibble(id = 1:length(names(data)),
                                                items = names(data),
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


  # UI GEN ----

  # generate the main UI ----
  output$ui_item_select <- renderUI({
    div(
      h4("Select items for a construct"),
      DT::DTOutput(ns("item_select")),
      shiny::selectInput(ns("construct_type"), label = "Type of Construct",
                         choices = type_choices,
                         selected = "Reflective", multiple = FALSE),
      textInput(ns("construct_name"), "Name of Construct"),
      shinyBS::bsTooltip(id = ns("construct_name"), title = "Please name your construct. Requires at least 3 characters."),
      disabled(actionButton(ns("create_construct"), "Create Construct")))
  })

  output$ui_constructs <- renderUI({
    div(
      h4("Item-Construct association"),
      DT::DTOutput(ns("constructs")),
      shinyjs::disabled(actionButton(ns("remove_item"), "Remove"))
    )
  })

  # Buttons ----
  # ensure button works ----
  observe({
    toggleState(id = "create_construct", condition =
                  (length(input$item_select_rows_selected) > 0 && # more than one row selected
                     nchar(input$construct_name) >= 3) ## and construct name given
    )
  })

  # delete button
  observe({
    toggleState(id = "remove_item", condition =
                  (length(input$constructs_rows_selected) > 0 ) # more than one item exists
    )
  })


  # render result table ----
  output$constructs <- DT::renderDT({

    if (nrow(rv$my_constructs) == 0) {
      return(data.frame(id = NA, item = NA, construct = NA, type = NA))
    } else {
      return(rv$my_constructs)
    }
  })


  # item removed ----
  observeEvent(input$remove_item, {
    # which are selected
    sels <- input$constructs_rows_selected

    # get their ids
    selected_items <- rv$my_constructs[sels, ]$id


    # make them available again
    rv$my_items <- rv$my_items %>% dplyr::mutate(selected = dplyr::case_when(id %in% selected_items ~ FALSE,
                                                                             TRUE ~ selected))

    # remove from constructs
    rv$my_constructs <- rv$my_constructs %>% dplyr::filter(id %nin% selected_items)
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
                          construct = input$construct_name,
                          type = input$construct_type)

    # change availability
    rv$my_items <- rv$my_items %>% dplyr::mutate(selected = dplyr::case_when(id %in% selected_items ~ TRUE,
                                                                             TRUE ~ selected))

    # add to constructs
    rv$my_constructs <- rv$my_constructs %>% dplyr::bind_rows(rows)

    # clear input
    updateTextInput(session, "construct_name", value = "")
    rv$semaphor = rv$semamphor + 1

  })

  generate_constructs <- reactive({

    d <- rv$my_constructs
    if (is.null(d)) {
      return(seminr::constructs(seminr::reflective("dummy", c("dummy"))))
    }
    if ("construct" %in% names(d)) {
      construct_names <- d %>% dplyr::pull("construct") %>% unique()
    } else {
      return(seminr::constructs(seminr::reflective("dummy", c("dummy"))))
    }

    constructs <- list()
    if (nrow(d > 1)) {
      for (i in construct_names) {
        items <- d %>% dplyr::filter(construct == i)

        # reflective
        if (type_choices[1] %in% items$type) {
          item_list <- items %>% dplyr::pull(item) # get all item strings
          constructs[[i]] <- seminr::reflective(i, item_list)
        }
        if (type_choices[2] %in% items$type) {
          item_list <- items %>% dplyr::pull(item) # get all item strings
          constructs[[i]] <- seminr::composite(i, item_list)
        }
        if (type_choices[3] %in% items$type) {
          item_list <- items %>% dplyr::pull(item) # get all item strings
          constructs[[i]] <- seminr::composite(i, item_list, weights = seminr::regression_weights)
        }

      }
    }

    do.call(seminr::constructs, constructs)
  })

  return( reactive({generate_constructs()}))
}

## To be copied in the UI
# mod_multi_column_select_ui("multi_column_select_ui_1")

## To be copied in the server
# callModule(mod_multi_column_select_server, "multi_column_select_ui_1")

