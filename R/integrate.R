#' area_picker
#'
#' @param df
#'
#' @export
#'
#' @examples
#' df <- echem_read(system.file('extdata/cv/cv_example.txt', package = 'osc'))
#' df <- area_picker(df)

peak_integrate <- function(df, x, y) {
  #requireNamespace("shiny", quietly = TRUE)
  #requireNamespace("miniUI", quietly = TRUE)
  input_name <- deparse(substitute(df))

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Select integration limits by dragging sliders below"),
    miniUI::miniContentPanel(
      shiny::plotOutput("plot", height = "100%")
    ),
    miniUI::miniContentPanel(
      shiny::fillRow(
        shiny::sliderInput("x1", "Integration limits", min(df$potential), max(df$potential), c(min(df$potential), max(df$potential)), sep = "", post = "V", dragRange = TRUE, width = "100%", step = 0.01),
        height = "50%"
      ),
      shiny::fillRow(
        shiny::selectInput("sweep", "Sweep", unique(df$sweep), width = "90%"),
        shiny::sliderInput("poly", "Background polynomial degree", 1, 5, 3, width = "90%", step = 1),
        shiny::sliderInput("span", "Span of fitting endpoints", 0.01, 0.25, 0.05, post = "V", width = "90%"),
        height = "50%")
    )
  )
  server <- function(input, output, session) {
    output$plot <- shiny::renderPlot({
      a <- area(df, sw = input$sweep, x1 = input$x1[1], x2 = input$x1[2], p = input$poly, span = input$span)
      plot_area(a)
    })
    shiny::observeEvent(input$done, {
      a <- area(df, sw = input$sweep, x1 = input$x1[1], x2 = input$x1[2], p = input$poly, span = input$span)
      area_params <- attr(a, "area")
      cat(paste0("\nArea of ", prettyNum(area_params$Q, digits = 3, format = "fg"), " C found between ", area_params$x1 , " V and ", area_params$x2 , " V.\n"))
      cat("The area() function below has been copied to the clipboard!\n")
      cat("Please paste it in your script for reproducibility.\n")
      res_string = paste0(input_name, " <- area(",input_name,", sw = ", input$sweep, ", x1 = ", input$x1[1], ", x2 = ", input$x1[2], ", p = ", input$poly, ", span = ", input$span, ")")
      cat(paste0("    ", res_string, "\n"))
      clipr::write_clip(res_string, return_new = FALSE)
      shiny::stopApp(returnValue = invisible(a))
    })
  }
  shiny::runGadget(ui, server)
}


#' Title
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
add_integrate <- function(df) {

}

#' Title
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
plot_integrate <- function(df) {

}
