#' Interactive smoothing
#'
#' Applies a Savitzky-Golay filter to smooth the data. The filter width can be selected interactively.
#'
#' @inheritParams peak_pick
#'
#' @return Returns a tibble with the original data and the result of the operation, as well as adding code for reproducibility to the clipboard.
#' @export
#'
#' @examples
#' library(tibble)
#' library(dplyr)
#' df <- tibble(x1 = 1:1000, y1 = sin(2*pi*(x1)/200)) %>%
#'   mutate(y2 = y1 + rnorm(n())/10)
#'
#' \dontrun{
#' peakr::peak_smooth(df, x1, y2)
#' }
#'
#' df %>% peakr::add_smooth(y2, fl = 95)
#'
#' df %>% peakr::add_smooth(y2, fl = 95) %>% peakr::plot_smooth(x1, y2)

peak_smooth <- function(df, x, y) {
  #requireNamespace("shiny", quietly = TRUE)
  #requireNamespace("miniUI", quietly = TRUE)
  input_name <- deparse(substitute(df))

  x <- rlang::enquo(x)
  y <- rlang::enquo(y)

  data <- generic_df(df, !!x, !!y)
  fl_max <- if (!length(data$x) %% 2) {length(data$x) - 1 } else {length(data$x)}

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Smooth data by adjusting parameters below"),
    miniUI::miniContentPanel(
      shiny::plotOutput("plot"),
      shiny::sliderInput("fl", "Filter length", 3, fl_max, 3, step = 2, sep = "", dragRange = FALSE, width = "100%")
    )
  )
  server <- function(input, output, session) {
    output$plot <- shiny::renderPlot({
      data %>% add_smooth(y, fl = input$fl) %>% plot_smooth(x, y)
    })
    shiny::observeEvent(input$done, {
      return_data <- df %>% add_smooth(!!y, fl = input$fl)

      message("\nSavitzky-Golay filter of length ", input$fl," has been added")
      message("The add_smooth() function below has been copied to the clipboard!")
      message("Please paste it in your script for reproducibility.")
      res_string = paste0(input_name, ' <- ', input_name,' %>% peakr::add_smooth(',rlang::quo_name(y),', fl = ',input$fl,')')
      message("\t", res_string)
      clipr::write_clip(res_string, return_new = FALSE)

      shiny::stopApp(returnValue = invisible(return_data))
    })
  }
  shiny::runGadget(ui, server)
}


#' Apply Savitzky-Golay filter
#'
#' @inheritParams peak_smooth
#' @param fl Filter length for the Savitzky-Golay filter. Must be an odd integer.
#'
#' @return Returns the original dataframe with an addition column containing the smoothed data
#' @export

add_smooth <- function(df, y, fl) {
  y <- rlang::enquo(y)
  y_smooth <- paste0(rlang::quo_name(y),"_smooth")
  df %>%
    dplyr::mutate(!!y_smooth := pracma::savgol(!!y, fl = fl))
}

#' Plot results of data smooting
#'
#' @inheritParams peak_smooth
#'
#' @return A plot displaying the orignal and smoothed data, as well as the residual
#' @export

plot_smooth <- function(df, x, y) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  y_smooth <- paste0(rlang::quo_name(y),"_smooth")

  df <- generic_df(df, !!x, !!y) %>%
    dplyr::mutate(y_smooth = df %>% dplyr::pull(!!y_smooth)) %>%
    dplyr::mutate(resid = y - y_smooth)

  g1 <- ggplot2::ggplot(df) +
    ggplot2::geom_line(ggplot2::aes_(~x, ~y), color = "grey20") +
    ggplot2::geom_line(ggplot2::aes_(~x, ~y_smooth), color = "red", size = 1) +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(), axis.title.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank())
  g2 <- ggplot2::ggplot(df, ggplot2::aes_(~x, ~resid)) +
    ggplot2::geom_line()

  patchwork::wrap_plots(g1, g2, ncol = 1, heights = c(4,1))
}
