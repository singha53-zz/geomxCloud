generateTopTable = function(eset, design, coefNumber, test){
  if (test == "limma"){
    fit <- limma::eBayes(limma::lmFit(t(eset), design))
    top <- limma::topTable(fit, coef = coefNumber, adjust.method = "BH", n = nrow(fit), sort.by="none")
    top %>% dplyr::mutate(FeatureName = colnames(eset),
                          sig = -log10(P.Value)) %>%
      dplyr::arrange(P.Value)
  } else if (test == "vlimma") {
    v <- limma::voom(t(eset), design, plot=FALSE)
    fit <- limma::lmFit(v, design)
    fit <- limma::eBayes(fit)
    top <- limma::topTable(fit, coef = coefNumber, n = nrow(fit), adjust.method = "BH", sort.by="none")
    top %>% dplyr::mutate(FeatureName = colnames(eset),
                          sig = -log10(P.Value)) %>%
      dplyr::arrange(P.Value)
  } else {
    # assume OLS
    fit <- limma::lmFit(t(eset), design)
    fit$t <- fit$coef/fit$stdev.unscaled/fit$sigma
    fit$p.value <- 2 * pt(-abs(fit$t), df = fit$df.residual)
    top <- limma::topTable(fit, coef = coefNumber, adjust.method = "BH", n = nrow(fit), sort.by="none")
    top %>% dplyr::mutate(FeatureName = colnames(eset),
                          sig = -log10(P.Value)) %>%
      dplyr::arrange(P.Value)
  }
}
