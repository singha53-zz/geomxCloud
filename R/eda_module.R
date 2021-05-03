#' UI of Exploratory Data Analysis page
#' @export
#' @rdname eda
eda <- function() {
  shiny::fluidRow(shiny::uiOutput("eda"))
}

#' UI of Exploratory Data Analysis (EDA) page
#'
#' @param id, character used to specify namespace,
#' see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI element
#' @export
eda_ui <- function(id, catVars) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    shiny::fluidRow(shiny::column(6,
                                    shiny::radioButtons(
                                      inputId = ns("color"),
                                      label = "Color samples by first variable:",
                                      choices = catVars,
                                      selected = catVars[1],
                                      inline = TRUE
                                    )
                                  ),
                    shiny::column(6,
                                  shiny::radioButtons(
                                    inputId = ns("shape"),
                                    label = "Color samples by second variable:",
                                    choices = catVars,
                                    selected = catVars[1],
                                    inline = TRUE
                                  )
                    )
                    ),
    shiny::fluidRow(
      shiny::column(
        5,
        shiny::h4("PCA plot", align = "center"),
        ggplot_ui(ns("pca_plot"))
      ),
      shiny::column(
        7,
        shiny::h4("Association between sample annotations and principal components", align = "center"),
        geomxCloud::pvalue_heatmap_ui(ns("pca_pvalue_heatmap"))
      )
    ),
    shiny::fluidRow(
      shiny::column(
        5,
        shiny::h4("t-SNE plot", align = "center"),
        ggplot_ui(ns("tsne_plot"))
      ),
      shiny::column(
        7,
        shiny::h4("Association between sample annotations and t-SNE dimensions", align = "center"),
        geomxCloud::pvalue_heatmap_ui(ns("tsne_pvalue_heatmap"))
      )
    )
  )
}

#' Reactive values for eda_server module server-side processing
#'
#' @param input, output, session standard \code{shiny}
#'
#' @return list with following components
#' \describe{
#'   \item{ncomp}{reactive number indicating number of PCs}
#' }
#' @export
eda_ui_vars <- function(input, output, session) {
  return(
    list(
      color = shiny::reactive({
        input$color
      }),
      shape = shiny::reactive({
        input$shape
      })
    )
  )
}


#' EDA module server-side processings
#'
#' This module produces the EDA panel for a given dataset
#'
#' @param input,output,session standard \code{shiny} boilerplate
#' @param demo data frame with metadata
#' @param dataset data frame containing omic variables
#' @param response_var which variable to color by
#' @param eda_ui_vars list of one element ncomp(containing number of PCs)
#' @export
eda_server <- function(input, output, session, demo, pca, tsne, group_colors, eda_ui_vars) {
  ns <- session$ns

  ## Component plots
  pcs <- as.data.frame(as.matrix(pca$x[, 1:2, drop = FALSE]))
  colnames(pcs) <- c("Dim1", "Dim2")
  shiny::observeEvent(eda_ui_vars$color(), {
    shiny::observeEvent(eda_ui_vars$shape(), {
      if(eda_ui_vars$color() == eda_ui_vars$shape()){
        color <- factor(as.character(demo[, eda_ui_vars$color()]))
      } else {
        color <- factor(paste(as.character(demo[, eda_ui_vars$color()]),
                              as.character(demo[, eda_ui_vars$shape()]), sep="_"))
      }
      ## PCA
      shiny::callModule(
        module = ggplot_server,
        id = "pca_plot", data = pcs, color = color,
        group_colors = group_colors,
        xlab = paste0("PC1 (", signif(100 * summary(pca)[[6]]["Proportion of Variance", "PC1"], 3), ")%"),
        ylab = paste0("PC2 (", signif(100 * summary(pca)[[6]]["Proportion of Variance", "PC2"], 3), ")%")
      )

      ## t-SNE
      shiny::callModule(
        module = ggplot_server,
        id = "tsne_plot", data = tsne, color = color, group_colors = group_colors
      )
    })
  })

  # Heatmaps
  shiny::callModule(
    module = geomxCloud::pvalue_heatmap_server,
    id = "pca_pvalue_heatmap", demo = demo, pcs = pca$x[, 1:2, drop = FALSE]
  )
  shiny::callModule(
    module = geomxCloud::pvalue_heatmap_server,
    id = "tsne_pvalue_heatmap", demo = demo, pcs = tsne
  )
}
