#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here
    shinydashboard::dashboardPage(
      shinydashboard::dashboardHeader(title = "Structural Equation Modeling in seminr"),
      shinydashboard::dashboardSidebar(shinydashboard::sidebarMenu(
        shinydashboard::menuItem(
          "Constructs",
          tabName = "constructs",
          icon = icon("dashboard")
        ),
        shinydashboard::menuItem(
          "Relationships",
          tabName = "relationships", icon = icon("th")
          )
      )),
      # UI ----
      shinydashboard::dashboardBody(
                      shinydashboard::tabItems(
                      # First tab content
                        shinydashboard::tabItem(tabName = "constructs",
                              fluidRow(
                                mod_multi_column_select_ui("multi_column_select_ui_1"),
                                shinydashboard::box(width = 6, DiagrammeR::grVizOutput("construct_plot", height = 500)),
                                #mod_constructselect_ui("constructselect_ui_1"),
                                shinydashboard::box(width = 6, shiny::verbatimTextOutput("construct_code", placeholder = TRUE))
                              )),

                      # Second tab content
                      shinydashboard::tabItem(tabName = "relationships",
                              h2("Widgets tab content"))
                    ))
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){

  add_resource_path(
    'www', app_sys('app/www')
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'shinyseminr'
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    shinyjs::useShinyjs()
  )
}

