#' area_picker
#'
#' @param df
#'
#' @export
#'
#' @examples
#' library(tidyverse)
#'
#' set.seed(123)
#' df <- tibble(x1 = seq(0.001, 10, 0.01), y1 = sin(2*x1)^4/(x1)) %>%
#'   mutate(y1 = y1 + rnorm(n(), mean = 0.01, sd = 0.1))
#'
#' peakr::peak_integrate(df, x1, y1)
#'
#' df <- df %>% peakr::add_integrate(x1, y1, x_low = 0.001, x_high = 3.3, p = 1, span = 0.05)
#'
#' df %>% peakr::plot_integrate(x1, y1)


peak_integrate <- function(df, x, y) {
  #requireNamespace("shiny", quietly = TRUE)
  #requireNamespace("miniUI", quietly = TRUE)
  input_name <- deparse(substitute(df))

  x <- rlang::enquo(x)
  y <- rlang::enquo(y)

  data <- generic_df(df, !!x, !!y)

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Select integration limits by dragging sliders below"),
    miniUI::miniContentPanel(
      shiny::plotOutput("plot", height = "100%")
    ),
    miniUI::miniContentPanel(
      shiny::fillRow(
        shiny::sliderInput("x1", "Integration limits", min(data$x), max(data$x), c(min(data$x), max(data$x)), step = 0.1, sep = "", dragRange = TRUE, width = "100%"),
        height = "50%"
      ),
      shiny::fillRow(
        shiny::sliderInput("poly", "Background polynomial degree", 1, 5, 3, width = "90%", step = 1),
        shiny::sliderInput("span", "Span of fitting endpoints", 0.01, 0.25, 0.05, width = "90%"),
        height = "50%")
    )
  )
  server <- function(input, output, session) {
    output$plot <- shiny::renderPlot({
      data %>% add_integrate(x, y, x_low = input$x1[1], x_high = input$x1[2], p = input$poly, span = input$span) %>% plot_integrate(x, y)
    })
    shiny::observeEvent(input$done, {
      return_data <- df %>% add_integrate(!!x, !!y, x_low = input$x1[1], x_high = input$x1[2], p = input$poly, span = input$span)
      params <- attr(return_data, "integrate")

      message("\nArea of ", params$integral," found between ", params$x_low, " and ",  params$x_high)
      message("The add_integrate() function below has been copied to the clipboard!")
      message("Please paste it in your script for reproducibility.")
      res_string = paste0(input_name, ' <- ', input_name,' %>% peakr::add_integrate(',rlang::quo_name(x),', ',rlang::quo_name(y),', x_low = ', params$x_low ,', x_high = ', params$x_high, ', p = ', params$p, ', span = ', params$span,')')
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
#' @export
#'
#' @examples
add_integrate <- function(df, x, y, x_low, x_high, p = 1, span = 0.05) {
  if (x_low > x_high) stop("x1 must be smaller than x2")
  if (span < 0) stop("span cannot be negative")

  x = rlang::enquo(x)
  y = rlang::enquo(y)

  data <- generic_df(df, !!x, !!y)

  background <- find_background(data, x_low, x_high, p, span)

  df <- df %>%
    dplyr::mutate(background = ifelse(dplyr::between(!!x, x_low, x_high), background$bg, NA))

  integral <- df %>%
    filter(!is.na(background)) %>%
    dplyr::mutate(y = !!y, x = !!x, subt = y - background) %>%
    dplyr::summarise(pracma::trapz(x, subt)) %>%
    as.numeric()

  attr(df, "integrate") <- list(
    integral = integral,
    x_low = x_low,
    x_high = x_high,
    p = p,
    span = span
  )
  return_data <- df
  return_data
}

#' Title
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
plot_integrate <- function(df, x, y) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)

  data <- generic_df(df, !!x, !!y)
  params <- attr(df, "integrate")

  data %>%
    mutate(background = df$background) %>%
    ggplot2::ggplot(ggplot2::aes(x, y)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(data = find_endpoints(data, params$x_low, params$x_high, params$span), aes(x, y), color = "red") +
    ggplot2::geom_line(data = . %>% filter(!is.na(background)), aes(x, background), color = "green") +
    geom_ribbon(data = . %>% filter(!is.na(background)), aes(x = x, ymin = background, ymax = y, fill = y - background > 0), alpha = 0.25) +
    theme(legend.position = "none") +
    annotate("text", x = Inf, y = Inf, hjust=1, vjust=1, label = attr(df, "integrate")$integral)
}

#' Title
#'
#' @param data
#' @param x_low
#' @param x_high
#' @param p
#' @param span
#'
#' @return
#' @export
#'
#' @examples
find_background <- function(data, x_low, x_high, p, span) {
  bg_data <- find_endpoints(data, x_low, x_high, span)
  bg_fit <- lm(y ~ poly(x,p), data = bg_data)

  list(bg = data %>% predict(bg_fit, .), bg_data = bg_data)
}

#' Title
#'
#' @param data
#' @param x_low
#' @param x_high
#' @param span
#'
#' @return
#' @export
#'
#' @examples
find_endpoints <- function(data, x_low, x_high, span) {
  seg1 <- data %>% dplyr::filter(dplyr::between(x, x_low - span, x_low + span))
  seg2 <- data %>% dplyr::filter(dplyr::between(x, x_high - span, x_high + span))

  dplyr::bind_rows(seg1, seg2)
}
