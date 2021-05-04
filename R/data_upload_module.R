#' UI of Data Upload page
#'
#' @param id, character used to specify namespace,
#' see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI element
#' @export
data_upload_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shinyBS::bsButton(ns("verify_email"), label = "", icon = shiny::icon("exclamation-triangle"), style = "color:gray", size = "small"),
        shinyBS::bsPopover(id = ns("verify_email"), title = "Verified users only!",
                           content = "If you would like to use this production app, please send your email to asingh.analytics@gmail.com so that it can be verified!",
                           placement = "right",
                           trigger = "click",  options = NULL),
        h3("Verified Email"),
        shiny::textInput(ns("email"), "Enter a verified email address", "", placeholder = "Email"),
        h3("File upload"),
        fileInput(ns("omics_data"),
          label = "Spatial expression data",
          multiple = FALSE
        ),
        fileInput(inputId = ns("demo"), label = "Sample Annotations"),
        esquisse::filterDF_UI(ns("filtering")),
        uiOutput(ns("response_var")),
        uiOutput(ns("ref_var")),
        actionButton(ns("aws"), "Run Cloud Compute!",
          icon = icon("cloud"),
          style = "color: #fff;
                 background-color: #337ab7;
                 border-color: #2e6da4"
        ),
        uiOutput(ns("uploadErrorMsg")),
        h3(".RData File Upload"),
        fileInput(ns("rds"), label = ""),
        actionButton(ns("run"), "Visualize results!",
          icon = icon("play"),
          style = "color: #fff;
                 background-color: #337ab7;
                 border-color: #2e6da4"
        ),
        uiOutput(ns("awsErrorMsg"))
      ),
      shiny::mainPanel(
        fluidRow(
          valueBoxOutput(ns("samples")),
          valueBoxOutput(ns("variables")),
          valueBoxOutput(ns("classes"))
        ),
        fluidRow(
          # conditionalPanel(
          #   condition = "output.flag == true",
          shinyWidgets::checkboxGroupButtons(
            inputId = ns("vars"),
            label = "",
            choices = ""
          ),
          shinyWidgets::progressBar(
            id = ns("pbar"), value = 100,
            total = 100, display_pct = TRUE
          ),
          # ),
          DT::dataTableOutput(outputId = ns("table"))
        )
      )
    )
  )
}

#' Reactive values for data_upload_server module server-side processing
#'
#' @param input, output, session standard \code{shiny}
#'
#' @return list with following components
#' \describe{
#'   \item{response_var}{reactive vector of categories
#'   for the response variable}
#' }
#' @export
data_upload_ui_vars <- function(input, output, session) {
  return(
    list(
      sep = shiny::reactive({
        input$sep
      }),
      demo = shiny::reactive({
        input$demo
      }),
      response_var = shiny::reactive({
        input$response_var
      }),
      ref_var = shiny::reactive({
        input$ref_var
      }),
      omics_data = shiny::reactive({
        input$omics_data
      }),
      run = shiny::reactive({
        input$run
      }),
      heart_failure = shiny::reactive({
        input$heart_failure
      }),
      covid19 = shiny::reactive({
        input$covid19
      }),
      alexa = shiny::reactive({
        input$alexa
      }),
      modal = shiny::reactive({
        input$modal
      })
    )
  )
}

#' Data Upload module server-side processings
#'
#' This module produces the Data Upload panel for a given dataset
#'
#' @param input,output,session standard \code{shiny} boilerplate
#' @param demo data frame with metadata
#' @param dataset data frame containing omic variables
#' @param response factor containing cateogories for the response variable
#' @param metadata_vars list of reactive vars (cont_var, transform)
#' @param cont_var character selected continuous variable
#' @param transform boolean TRUE: log2-transformation
#' @export
data_upload_server <- function(input, output, session,
                               heart_failure_data, covid19_data, data_upload_ui_vars) {
  ns <- session$ns

  # output$uploadErrorMsg = renderUI({
  #   validate(
  #     need(data_upload_ui_vars$demo(), "Metadata is required with at least 1 categorical variable!"),
  #     need(data_upload_ui_vars$omics_data(), "At least one omics data is required!"),
  #     need(data_upload_ui_vars$response_var(), "A response variable is required!")
  #     # need(length(unique(data_upload_server_vars$get_demo_data()[, data_upload_ui_vars$response_var()])) > 1, "At least 2 categories required!")
  #   )
  # })
  #
  # # Demographics data upload
  # get_demo_data <- shiny::reactive({
  #   req(data_upload_ui_vars$demo())
  #   demo_data <- read.table(data_upload_ui_vars$demo()$datapath,
  #     header = TRUE, sep = data_upload_ui_vars$sep())
  #   demo_data
  # })
  #
  # # omics data upload
  # get_omics_data <- shiny::reactive({
  #   req(data_upload_ui_vars$omics_data())
  #   omics_data <- lapply(data_upload_ui_vars$omics_data()$datapath, read.table,
  #     header = TRUE,
  #     sep = data_upload_ui_vars$sep())
  #   names(omics_data) <- gsub(".csv|.tsv|.txt", "",
  #     data_upload_ui_vars$omics_data()$name)
  #   omics_data
  # })
  #
  #
  # # show column names of demo dataset
  # output$response_var <- shiny::renderUI({
  #   shiny::req(get_demo_data())
  #   keep_cols <- apply(get_demo_data(), 2, function(i) {
  #     ifelse(length(table(as.character(i))) < 9 &
  #         min(table(as.character(i))) > 1, TRUE, FALSE)
  #   })
  #   shiny::selectInput(ns("response_var"),
  #     "Select response variable",
  #     colnames(get_demo_data()[, keep_cols]))
  # })
  #
  # output$ref_var <- shiny::renderUI({
  #   shiny::selectInput(ns("ref_var"),
  #     "Select reference level",
  #     unique(get_demo_data()[, data_upload_ui_vars$response_var()]))
  # })
  #
  # response <- shiny::reactive({
  #   relevel(factor(as.character(get_demo_data()[,
  #     data_upload_ui_vars$response_var()])),
  #   ref = data_upload_ui_vars$ref_var())})
  #
  # determine which datasets to perform gene set enrichment analysis on?
  perform_pathway_analysis <- shiny::reactive({
    dataset_names <- sapply(names(get_omics_data()), function(i) {
      length(intersect(colnames(get_omics_data()[[i]]), unlist(kegg))) > 5
    })
    names(dataset_names)[dataset_names]
  })
  #
  # # if user wants to analyze the example heart failure data
  # output$heart_failure <- shiny::downloadHandler(
  #   filename = "heartFailureDatasets_omicsBioAnalytics.zip",
  #   content = function(file) {
  #     files <- NULL;
  #
  #     # loop through the sheets
  #     for (i in 1:length(heart_failure_data)) {
  #       #write each sheet to a csv file, save the name
  #       file_name <- paste0(names(heart_failure_data)[i], ".txt")
  #       write.table(heart_failure_data[[i]],
  #         file_name, sep = "\t",
  #         row.names = FALSE)
  #       files <- c(file_name, files)
  #     }
  #     # create the zip file
  #     zip(file, files)
  #   }
  # )
  #
  # # if user wants to analyze the example COVID-19 data
  # output$covid19 <- shiny::downloadHandler(
  #   filename = "COVID19Datasets_omicsBioAnalytics.zip",
  #   content = function(file) {
  #     files <- NULL;
  #
  #     #loop through the sheets
  #     for (i in 1:length(covid19_data)) {
  #       #write each sheet to a txt file, save the name
  #       file_name <- paste0(names(covid19_data)[i], ".txt")
  #       write.table(covid19_data[[i]], file_name, sep = "\t", row.names = FALSE)
  #       files <- c(file_name, files)
  #     }
  #     #create the zip file
  #     zip(file, files)
  #   }
  # )

  # data upload
  output$uploadErrorMsg <- renderUI({
    validate(
      need(isValidEmail(input$email), "A validate email address is required!"),
      need(input$demo, "Metadata is required with at least 1 categorical variable!"),
      need(input$omics_data, "At least one omics data is required!"),
      need(input$response_var, "A response variable is required!")
    )
  })

  output$awsErrorMsg <- renderUI({
    validate(
      need(input$rds, "An rds file is required!")
    )
  })


  # Demographics data upload
  get_demo_data <- shiny::reactive({
    req(input$demo)
    file_ext <- strsplit(input$demo$datapath, "\\.")[[1]][2]
    sep <- ifelse(file_ext == "txt", "\t", ",")

    demo <- read.table(input$demo$datapath,
      header = TRUE, sep = sep
    )
    demo[, colSums(is.na(demo)) == 0]
  })

  # Allow user to select columns
  observe({
    shinyWidgets::updateCheckboxGroupButtons(session,
      inputId = "vars",
      label = "Choose columns",
      choices = colnames(get_demo_data()),
      selected = colnames(get_demo_data())[1],
      checkIcon = list(
        yes = tags$i(
          class = "fa fa-check-square",
          style = "color: steelblue"
        ),
        no = tags$i(
          class = "fa fa-square-o",
          style = "color: steelblue"
        )
      )
    )
  })


  ## filter demo dataset
  vars <- reactiveValues(val = NULL)
  observeEvent(input$vars, {
    req(get_demo_data())
    vars$val <- input$vars
  })

  res_filter <- callModule(
    module = esquisse::filterDF,
    id = "filtering",
    data_table = get_demo_data,
    data_vars = reactive(vars$val),
    data_name = reactive("GeoMx")
  )

  # Print Sample Annotations
  output$table <- DT::renderDT(
    {
      res_filter$data_filtered()[, vars$val, drop = FALSE]
    },
    options = list(searching = FALSE, pageLength = 25, lengthMenu = c(15, 20, 35, 50), scrollX = T)
  )



  # omics data upload
  get_omics_data <- shiny::reactive({
    req(input$omics_data)
    file_ext <- strsplit(input$omics_data$datapath, "\\.")[[1]][2]
    sep <- ifelse(file_ext == "txt", "\t", ",")
    read.table(input$omics_data$datapath, header = TRUE, sep = sep, row.names = 1)
  })


  # Allow user to select the response variables used for analysis
  suppressWarnings(
    observeEvent(res_filter$data_filtered(), {
      output$response_var <- shiny::renderUI({
        shiny::req(res_filter$data_filtered())
        keep_cols <- apply(res_filter$data_filtered(), 2, function(i) {
          ifelse(length(table(as.character(i))) < 9 & length(table(as.character(i))) > 1 & min(table(as.character(i))) > 1, TRUE, FALSE)
        })
        shiny::selectInput(
          ns("response_var"),
          "Select response variable",
          colnames(res_filter$data_filtered()[, keep_cols])
        )
      })
    })
  )

  ## Reference level of response variable
  output$ref_var <- shiny::renderUI({
    req(input$response_var)
    shiny::selectInput(
      ns("ref_var"),
      "Select reference level",
      unique(res_filter$data_filtered()[, input$response_var])
    )
  })

  ## Response variable used for analysis
  response <- shiny::reactive({
    req(res_filter$data_filtered())
    req(input$response_var)
    req(input$ref_var)
    if (length(intersect(input$ref_var, unique(res_filter$data_filtered()[, input$response_var])))) {
      relevel(factor(as.character(res_filter$data_filtered()[, input$response_var])), ref = input$ref_var)
    }
  })

  # Display number of samples used for analysis
  output$samples <- renderValueBox({
    shiny::req(get_demo_data())

    valueBox(
      value = nrow(res_filter$data_filtered()),
      subtitle = "samples",
      icon = icon("vials"),
      color = "aqua"
    )
  })

  output$variables <- renderValueBox({
    shiny::req(get_demo_data())
    valueBox(
      value = nrow(get_omics_data()),
      subtitle = "Number of genes/proteins",
      icon = icon("table")
    )
  })

  output$classes <- renderValueBox({
    req(res_filter$data_filtered())
    req(response())
    classes <- table(response())
    valueBox(
      p(paste(paste(names(classes), classes, sep = "="), collapse = ", "), style = "word-wrap: break-word; display: inline-block; vertical-align: middle;"),
      "Class sizes",
      icon = icon("users")
    )
  })

  # Update progress bar when data is filtered
  observeEvent(res_filter$data_filtered(), {
    shinyWidgets::updateProgressBar(
      session = session, id = "pbar",
      value = nrow(res_filter$data_filtered()), total = nrow(get_demo_data())
    )
  })

  observeEvent(input$aws, {
    req(get_demo_data())
    req(get_omics_data())
    req(response())

    # find samples to keep
    keep_samples <- as.numeric(rownames(as.data.frame(res_filter$data_filtered()[, vars$val, drop = FALSE])))
    demo <- res_filter$data_filtered()[, vars$val, drop = FALSE]
    eset <- t(get_omics_data()[, keep_samples])
    response_var <- input$response_var
    res <- response()

    print(dim(demo))
    print(dim(eset))
    print(length(res))
    input_data <- list(demo = demo, eset = eset, response = res, response_var = response_var)
    # save datasets to temporary directory
    # tmp <- tempfile()
    # on.exit(unlink(tmp))
    folder <- strsplit(input$email, "@")[[1]][1]

    if(folder != "asingh.analytics"){
      showNotification("This email is not verified! \n Please email asingh.analytis@gmail with your email to verify your email address!",
                       type = "message", duration = 10
      )
    } else {

    saveRDS(input_data, file = paste0(tempdir(), "/", folder, "-geomx-analysis.rds"))

    ## upload data to S3
    shiny::withProgress(
      message = "Uploading data to the Cloud...",
      detail = "This may take some time...",
      value = 0.4,
      {
        upload <- tryCatch(
          {
            put_object(paste0(tempdir(), "/", folder, "-geomx-analysis.rds"),
              bucket = Sys.getenv("S3BUCKET"),
              show_progress = TRUE, multipart = TRUE
            )
          },
          error = function(err) NULL
        )
      }
    )

    if (is.null(upload)) {
      showNotification("The cloud infrastructure has not been setup. \n Please contact admin!",
        type = "message", duration = 10
      )
    } else {
      showNotification("Your analysis has started in the cloud! \n
                       You will receive an email with a link to download a R data file which you can upload below to visualize the results!",
        type = "message", duration = 10
      )
    }
    }
  })

  all_datasets <- reactiveValues(data = NULL)
  observeEvent(input$run, {
    req(input$rds)
    # if ( is.null(input$rds)) return(NULL)
    inFile <- input$rds$datapath
    print(inFile)
    readFile <- tryCatch(readRDS(inFile), error = function(err) NULL)

    if (is.null(readFile)) {
      showNotification("Please upload a correct file!",
        type = "message", duration = 10
      )
    } else {
      data <- readRDS(inFile)

      if (all(names(data) %in% c("input_data", "eda", "toptables", "enrichr_results"))) {
        all_datasets$val <- data
      } else {
        showNotification("The uploaded file does not have the correct specification!",
          type = "message", duration = 10
        )
      }
    }
  })

  # output$flag <- reactive({
  #   if (!is.null(get_demo_data())) {
  #     flag <- TRUE
  #   } else {
  #     flag <- FALSE
  #   }
  # })
  # outputOptions(output, "flag", suspendWhenHidden = FALSE)

  return(list(
    all_datasets = all_datasets,
    perform_pathway_analysis = perform_pathway_analysis
  ))
}

isValidEmail <- function(x) {
  grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x),
        ignore.case=TRUE)
}
