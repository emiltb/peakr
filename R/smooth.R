#' Smoothing
#'
#' @param df
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
#' library(tidyverse)
#'
#' df <- tibble(x1 = 1:1000, y1 = sin(2*pi*(x1)/200)) %>%
#'   mutate(y2 = y1 + rnorm(n())/10)
#'
#' peakr::peak_smooth(df, x1, y2)
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


#' Title
#'
#' @param df
#'
#' @return
#' @importFrom rlang :=
#' @export
#'
#' @examples
add_smooth <- function(df, y, fl) {
  y <- rlang::enquo(y)
  y_smooth <- paste0(rlang::quo_name(y),"_smooth")
  df %>%
    dplyr::mutate(!!y_smooth := pracma::savgol(!!y, fl = fl))
}

#' Title
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
plot_smooth <- function(df, x, y) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  y_smooth <- paste0(rlang::quo_name(y),"_smooth")

  df <- generic_df(df, !!x, !!y) %>%
    mutate(y_smooth = df %>% pull(!!y_smooth)) %>%
    mutate(resid = y - y_smooth)

  g1 <- ggplot(df) +
    geom_line(aes(x, y), color = "grey20") +
    geom_line(aes(x, y_smooth), color = "red", size = 1) +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank())
  g2 <- ggplot(df, aes(x, resid)) +
    geom_line()

  patchwork::wrap_plots(g1, g2, ncol = 1, heights = c(4,1))
}
