#' ggplot UI module
#'
#' @param id, character used to specify namespace,
#' see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI element
#' @export
ggplot_ui <- function(id) {
  ns <- shiny::NS(id)
  plotly::plotlyOutput(ns("plot"))
}

#' ggplot Server module
#'
#' This module produces the pvalue heatmap
#'
#' @param input,output,session standard \code{shiny} boilerplate
#' @param data data frame to plot
#' @param color point colors
#' @param color point shape
#' @export
ggplot_server <- function(input, output, session, data, color, group_colors, xlab = NULL, ylab = NULL) {
  output$plot <- plotly::renderPlotly({
    p <- data %>%
      dplyr::mutate(color = color) %>%
      ggplot(aes(Dim1, Dim2, color = color)) +
      geom_point() +
      theme_classic() +
      scale_color_manual(values = group_colors[1:length(unique(color))]) +
      theme(legend.position = "bottom") +
      stat_ellipse()
    if(!is.null(xlab)){
      p <- p + xlab(xlab)
    }
    if(is.null(ylab)){
      p <- p + ylab(ylab)
    }
    ggplotly(p)
  })
}
