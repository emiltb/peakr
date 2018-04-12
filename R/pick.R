#' Pick peaks in datasets interactively
#'
#' @description
#' `peak_pick()` is an interactive tool (a shinyGadget) for finding peaks in datasets (e.g. a spectrum). After loading the dataset and specifying the columns, you can easily select peaks by clicking on the dataset. The gadget will automatically try to determine the nearest peak to the click and select it. Peaks can be deselected by clicking again.
#'
#' When you are finished press "Done". The gadget will place a piece of code in your clipboard that you should paste into your script for reproducibility.
#'
#' @param data A tibble containing the data to pick peaks in
#' @param x Column containing the x-values
#' @param y Column containing the y-values
#' @param find Can be either "max" or "min". Defines what is selected when highlighting peaks. Defaults to "max".
#'
#' @return The original dataset with an additional column "peak", which indicates TRUE/FALSE if the given row is a peak.
#' @export
#'
#'
#' @examples
#' library(tibble)
#' library(dplyr)
#' set.seed(123)
#' df <- tibble(x = seq(0.001, 10, 0.01), y = sin(10*x)^4/(x)) %>%
#'   mutate(y = y + rnorm(n(), mean = 0.01, sd = 0.1))
#'
#' \dontrun{
#' peakr::peak_pick(df, x, y)
#' }
#'
#' df <- df %>% peakr::add_pick(c(14,48,80,112,143))
#'
#' # After adding peaks, the plot from the gadget can be reproduced using plot_pick()
#' df %>% peakr::plot_pick(x, y)

peak_pick <- function(data, x, y, find = "max") {
  input_name <- deparse(substitute(data))
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)

  if (find == "max") {
    selector <- max
    }
  else if (find == "min") {
    selector <- min
  } else {
    stop('find can only be "max" or "min"')
  }

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Select peaks by clicking on the figure below"),
    miniUI::miniContentPanel(
      shiny::plotOutput("plot1", height = "100%", dblclick = "plot1_dblclick", brush = shiny::brushOpts(id = "plot1_brush"))
    )
  )

  server <- function(input, output, session) {
    v <- shiny::reactiveValues(
      selectedData = data[0,]
    )

    shiny::observeEvent(input$plot1_dblclick, {
      X1 <- shiny::nearPoints(data, input$plot1_dblclick, maxpoints = 1, threshold = 25)

      if(nrow(X1) > 0) {
        row_to_add <- X1
        v$selectedData <- add_if_unique(v$selectedData, row_to_add, !! x, !! y)
      }
    })

    shiny::observeEvent(input$plot1_brush, {
      X1 <- shiny::brushedPoints(data, input$plot1_brush)

      row_to_add <- X1 %>% dplyr::filter((!!y) == selector(!! y))
      v$selectedData <- add_if_unique(v$selectedData, row_to_add, !! x, !! y)
      session$resetBrush("plot1_brush")
    })

    output$plot1 <- shiny::renderPlot({
      peak_indices <- match(v$selectedData %>% dplyr::pull(!! x), data %>% dplyr::pull(!! x))
      data %>% add_pick(peak_indices) %>% plot_pick(!! x, !! y)
    })

    shiny::observeEvent(input$done, {
      peak_indices <- match(v$selectedData %>% dplyr::pull(!! x), data %>% dplyr::pull(!! x))
      return_data <- data %>% add_pick(peak_indices)

      message(length(peak_indices)," peaks found in the dataset")
      message("The add_pick() function below has been copied to the clipboard!")
      message("Please paste it in your script for reproducibility.")
      peak_vec <- paste(peak_indices, collapse = ",")
      res_string = paste0(input_name, ' <- ', input_name,' %>% peakr::add_pick(c(', peak_vec ,'))')
      message("\t", res_string)
      clipr::write_clip(res_string, return_new = FALSE)

      shiny::stopApp(returnValue = invisible(return_data))
    })
  }
  shiny::runGadget(ui, server)
}

#' Add peak indicators at the given indices
#'
#' @inheritParams peak_pick
#' @param indices Indices of the data to mark as peaks
#'
#' @return tbl_df with an additional column indicating whether the row is a peak
#' @export
#'
#' @examples
#' library(tibble)
#' library(dplyr)
#' tibble(x1 = seq(0.1, 9, 0.01), y1 = sin(x1)) %>%
#'   add_pick(c(148,776))

add_pick <- function(data, indices) {
  return_data <- data %>% dplyr::mutate(peak = FALSE)
  return_data$peak[indices] <- TRUE
  return_data
}

#' Plot a dataset contain defined peaks
#'
#' @inheritParams peak_pick
#'
#' @return A ggplot
#' @export
#'
#' @examples
#' library(tibble)
#' library(dplyr)
#'
#' tibble(x1 = seq(0.1, 9, 0.01), y1 = sin(x1)) %>%
#'   add_pick(c(148,776)) %>%
#'   plot_pick(x1, y1)

plot_pick <- function(data, x, y) {
  x <- dplyr::enquo(x)
  y <- dplyr::enquo(y)

  y_range <- data %>% dplyr::pull(!! y) %>% range()
  nudge_dist <- (y_range[2] - y_range[1]) * 0.03

  ggplot2::ggplot(data, ggplot2::aes_string(rlang::quo_text(x), rlang::quo_text(y))) + ggplot2::geom_line() +
    ggplot2::geom_point(data = . %>% dplyr::filter(.$peak), color = "red") +
    ggplot2::geom_text(data = . %>% dplyr::filter(.$peak), ggplot2::aes_string(rlang::quo_text(x), rlang::quo_text(y), label = rlang::quo_text(x)), color = "red", nudge_y = nudge_dist)
}

