#' Peak integrator
#'
#' @param df Tibble containing the 2D data to integrate
#' @param x Column in df containing x-values
#' @param y Column in df containing y-values
#'
#' @return Returns a tibble with the original data and the result of the operation, as well as adding code for reproducibility to the clipboard.
#' @export
#'
#' @examples
#' library(tibble)
#' library(dplyr)
#' set.seed(123)
#' df <- tibble(x1 = seq(0.001, 10, 0.01), y1 = sin(2*x1)^4/(x1)) %>%
#'   mutate(y1 = y1 + rnorm(n(), mean = 0.01, sd = 0.1))
#'
#' \dontrun{
#' peakr::peak_integrate(df, x1, y1)
#' }
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


#' Add integral parameters to tibble
#'
#' Used for reproducibility between running the interactive gadget and running an analysis later on.
#'
#' @inheritParams peak_integrate
#' @param x_low Lower limit of integration
#' @param x_high Upper limit of integration
#' @param p Degree of polynomial used in background subtraction
#' @param span Span of the set of points used to fit the background
#'
#' @return Returns a tibble with the original data and integral parameters and results as the integrate attribute
#' @export
#'
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
    dplyr::filter(!is.na(background)) %>%
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

#' Plot results of peak integration
#'
#' @inheritParams peak_pick
#'
#' @return A plot displaying the area of integration
#' @export
#'
plot_integrate <- function(df, x, y) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)

  data <- generic_df(df, !!x, !!y)
  params <- attr(df, "integrate")

  data %>%
    dplyr::mutate(background = df$background) %>%
    ggplot2::ggplot(ggplot2::aes_(~x, ~y)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(data = find_endpoints(data, params$x_low, params$x_high, params$span), ggplot2::aes_(~x, ~y), color = "red") +
    ggplot2::geom_line(data = . %>% dplyr::filter(!is.na(background)), ggplot2::aes_(~x, ~background), color = "green") +
    ggplot2::geom_ribbon(data = . %>% dplyr::filter(!is.na(background)), ggplot2::aes_(x = ~x, ymin = ~background, ymax = ~y, fill = ~(y - background > 0)), alpha = 0.25) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::annotate("text", x = Inf, y = Inf, hjust=1, vjust=1, label = attr(df, "integrate")$integral)
}

#' Fit a p-degree polynomial to the selected endpoints
#'
#' @inheritParams add_integrate
#'
#' @return A list containing the fitted polynomial as well as the endpoints used for fitting.
#'
find_background <- function(df, x_low, x_high, p, span) {
  bg_data <- find_endpoints(df, x_low, x_high, span)
  bg_fit <- stats::lm(y ~ stats::poly(x,p), data = bg_data)

  list(bg = df %>% stats::predict(bg_fit, .), bg_data = bg_data)
}

#' Select the points to be used for background fitting
#'
#' @inheritParams add_integrate
#'
#' @return A tibble containing the endpoints to be used for background fitting
#'
find_endpoints <- function(df, x_low, x_high, span) {
  seg1 <- df %>% dplyr::filter(dplyr::between(x, x_low - span, x_low + span))
  seg2 <- df %>% dplyr::filter(dplyr::between(x, x_high - span, x_high + span))

  dplyr::bind_rows(seg1, seg2)
}
