options(shiny.maxRequestSize=100*1024^2)
function(input, output, session) {
  ns <- session$ns

  # Homepage display
  output$author <- renderValueBox({
    valueBox(
      value = "Author",
      subtitle = "Amrit Singh",
      icon = icon("author"),
      color = "black",
      href = "https://www.amritsingh.ca"
    )
  })
  output$source <- renderValueBox({
    valueBox(
      value = "Source code",
      subtitle = "GeoMx Cloud",
      icon = icon("author"),
      color = "black",
      href = "https://github.com/singha53/geomxCloud"
    )
  })
  output$contest <- renderValueBox({
    valueBox(
      value = "Contest",
      subtitle = "NanoString Spatial Omics Hackathon",
      icon = icon("author"),
      color = "black",
      href = "https://nanostring.devpost.com/"
    )
  })

  # Do not show analysis sidemenu at startup!!
  output$analysisRan <- reactive({
    returnedValue = FALSE
    return(returnedValue)
  })
  outputOptions(output, "analysisRan", suspendWhenHidden = FALSE)

  ################################################################################
  #
  # Data Upload
  #
  ################################################################################
  data_upload_ui_vars <- callModule(module = geomxCloud::data_upload_ui_vars, "data_upload")
  data_upload_server_vars <- callModule(module = geomxCloud::data_upload_server,
    id = "data_upload",
    heart_failure_data = hf_datasets,
    covid19_data = covid19,
    data_upload_ui_vars = data_upload_ui_vars)

    # print(data_upload_server_vars$all_datasets$val)
    # Run analysis
    observeEvent(data_upload_server_vars$all_datasets$val, {
      ## datasets
      demo = data_upload_server_vars$all_datasets$val$input_data$demo
      response_var = data_upload_server_vars$all_datasets$val$input_data$response_var
      response = data_upload_server_vars$all_datasets$val$input_data$response
      ref_var = levels(data_upload_server_vars$all_datasets$val$input_data$res)[1]
      omics_data = list(mRNA = data_upload_server_vars$all_datasets$val$input_data$eset)
      pca <- list(mRNA = data_upload_server_vars$all_datasets$val$eda$pca)
      tsne <- list(mRNA = data_upload_server_vars$all_datasets$val$eda$tsne)
      toptables <- data_upload_server_vars$all_datasets$val$toptables
      enrichr_results <- data_upload_server_vars$all_datasets$val$enrichr_results

      withProgress(message = 'Running analysis...',
                   detail = 'This may take some time...', value = 0, {


                     ################################################################################
                     #
                     # Metadata Analysis
                     #
                     ################################################################################
                     metadata_ui_vars <- callModule(module = geomxCloud::metadata_ui_vars, "metadata")
                     callModule(module = geomxCloud::metadata_server,
                                id = "metadata",
                                response_var = response_var, demo = demo, response = response,
                                metadata_ui_vars = metadata_ui_vars)

                     ################################################################################
                     #
                     # Exploratory Data Analysis
                     #
                     ################################################################################
                     ### User interface
                     catVars <- colnames(demo)[apply(demo, 2, function(var){
                       length(table(var)) < 9
                     })]
                     output$eda = renderUI({
                       myTabs <- lapply(names(omics_data), function(datasetName){
                         tabPanel(datasetName,
                                  geomxCloud::eda_ui(paste0("eda", datasetName, sep = "_"), catVars = catVars)
                         )
                       })
                       do.call(tabsetPanel, myTabs)
                     })

                     ### Backend
                     lapply(names(omics_data),
                            function(datasetName) {
                              eda_ui_vars <- callModule(module = geomxCloud::eda_ui_vars, paste0("eda", datasetName, sep = "_"))
                              callModule(module = geomxCloud::eda_server,
                                         id = paste0("eda", datasetName, sep = "_"),
                                         demo = demo,
                                         pca = pca[[datasetName]],
                                         tsne = tsne[[datasetName]],
                                         group_colors = group_colors,
                                         eda_ui_vars = eda_ui_vars)
                            }
                     )

                     ################################################################################
                     #
                     # Differential Expression Analysis
                     #
                     ################################################################################
                     # set flag to only run geneset enrichment analysis of datasets with gene symbols
                     output$performPathwayAnalysis <- shiny::reactive({
                       shiny::req(omics_data)
                       datasetNames <- sapply(names(omics_data), function(i){
                         length(intersect(colnames(omics_data[[i]]), unlist(kegg))) > 5
                       })
                       names(datasetNames)[datasetNames]
                     })
                     shiny::outputOptions(output, "performPathwayAnalysis", suspendWhenHidden = FALSE)
                     ### User interface
                     output$dea = renderUI({
                       myTabs <- lapply(names(omics_data), function(datasetName){
                         tabPanel(datasetName,
                                  geomxCloud::dea_ui(paste0("dea", datasetName, sep = "_"),
                                                     datasetName = datasetName,
                                                     dataset = omics_data[[datasetName]],
                                                     response = response)
                         )
                       })
                       do.call(tabsetPanel, myTabs)
                     })

                     ### Backend
                     # determine which datasets to perform gene set enrichment analysis on?
                     perform_pathway_analysis <- shiny::reactive({
                       dataset_names <- sapply(names(omics_data), function(i){
                         length(intersect(colnames(omics_data[[i]]), unlist(kegg))) > 5
                       })
                       names(dataset_names)[dataset_names]
                     })

                     lapply(names(omics_data),
                            function(datasetName) {
                              dea_ui_vars <- callModule(module = geomxCloud::dea_ui_vars, paste0("dea", datasetName, sep = "_"))
                              callModule(module = geomxCloud::dea_server,
                                         id = paste0("dea", datasetName, sep = "_"),
                                         datasetName = datasetName,
                                         dataset = omics_data[[datasetName]],
                                         response = response,
                                         response_var = response_var,
                                         perform_pathway_analysis = perform_pathway_analysis(),
                                         group_colors = group_colors,
                                         enrichr_results = enrichr_results,
                                         toptables = toptables,
                                         dea_ui_vars = dea_ui_vars)
                            }
                     )

                     ################################################################################
                     #
                     # Biomarker Discovery Analysis
                     #
                     ################################################################################
                     # output$biomarker_discovery_analysis = renderUI({
                     #   geomxCloud::biomarker_discovery_analysis_ui("biomarker_discovery_analysis",
                     #     dataset_names = names(omics_data),
                     #     response = response,
                     #     response_var = response_var)
                     # })
                     # biomarker_discovery_analysis_ui_vars <- callModule(module = geomxCloud::biomarker_discovery_analysis_ui_vars,
                     #   "biomarker_discovery_analysis")
                     # callModule(module = geomxCloud::biomarker_discovery_analysis_server,
                     #   id = "biomarker_discovery_analysis",
                     #   datasets = omics_data,
                     #   response = response,
                     #   response_var = response_var,
                     #   group_colors = group_colors,
                     #   biomarker_discovery_analysis_ui_vars = biomarker_discovery_analysis_ui_vars)


                     observe({
                       showNotification("Click on the Analysis tab to begin.",
                                        type = "message", duration = 10)
                     })

                     # show analysis sidemenu when run analysis button is pressed
                     output$analysisRan <- reactive({
                       returnedValue = TRUE
                       return(returnedValue)
                     })
                     outputOptions(output, "analysisRan", suspendWhenHidden = FALSE)
                   })
    })





  ################################################################################
  #
  # Generate a report
  #
  ################################################################################
  report_ui_vars <- callModule(module = geomxCloud::report_ui_vars, "report")
  callModule(module = geomxCloud::report_server,
    id = "report",
    report_ui_vars = report_ui_vars)


  # delete temp files
  # session$onSessionEnded(function() {
  #   sapply(list.files(tempdir(), full.names = TRUE), file.remove)
  # })
}
