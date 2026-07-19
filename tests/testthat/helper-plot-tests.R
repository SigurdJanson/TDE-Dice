# Load required packages
library(testthat)
library(ggplot2)


#' Title
#' Test the structure incl. layers
#' @param plot
#' @param expectedGeom
#' @param expectedX
#' @param expectedY
#'
#' @returns
#' @export
#'
#' @examples
testPlotStructure <- function(plot, expectedGeom = NULL, expectedX = NULL, expectedY = NULL) {
  test_that("Plot is a ggplot object", {
    expect_true(is_ggplot(plot))
  })

  if (!is.null(expectedGeom)) {
    test_that("Plot contains expected geom layers", {
      geoms <- vapply(plot$layers, \(l) class(l$geom)[1], character(1))
      for(exp in expectedGeom) {
        expect_true(any(grepl(exp, geoms)),
                    info = paste("Expected geom:", exp))
      }
    })
  }

  if (!is.null(expectedX) || !is.null(expectedY)) {
    test_that("Plot has correct mapping", {
      mapping <- plot$mapping
      if (!is.null(expectedX)) {
        expect_equal(as_label(mapping$x), expectedX)
      }
      if (!is.null(expectedY)) {
        expect_equal(as_label(mapping$y), expectedY)
      }
    })
  }
}


testPlotData <- function(plot, columns, nRow) {
  test_that("Plot data is correctly transformed", {
    plot_data <- plot@data

    # Check if the data has the expected columns
    expect_true(all(columns %in% colnames(plot_data)))

    # Check number of data points
    expect_equal(nrow(plot_data), nRow)

    # Check if the data is scaled correctly (p * 100)
    expect_true(all(plot_data$p >= 0 & plot_data$p <= 100))
  })
}


testPlotAESDimensions <- function(plot, columns, nRow) {
  test_that("Plot data is correctly transformed", {
    plot_data <- ggplot2::layer_data(plot)

    # Check if the data has the expected columns
    expect_true(all(columns %in% colnames(plot_data)))

    # Check number of data points
    expect_equal(nrow(plot_data), nRow)

    # Check if the data is scaled correctly (p * 100)
    expect_true(all(plot_data$p >= 0 & plot_data$p <= 100))
  })
}


testPlotLabels <- function(plot, x, y, title = NULL, subtitle = NULL, caption = NULL) {
  test_that("plot has the expected labels", {
    # Check x and y labels
    expect_equal(plot$labels$x, x)
    expect_equal(plot$labels$y, y)

    if (!is.null(title))
      expect_equal(plot$labels$title, title)
    if (!is.null(subtitle))
      expect_equal(plot$labels$subtitle, subtitle)
    if (!is.null(caption))
      expect_equal(plot$labels$caption, caption)

    # Check legend title
    #expect_equal(plot$guides$color$title, title)
  })
}





