## load libraries
library(dplyr)
library(enrichR)

args <- commandArgs(trailingOnly=TRUE)

## load data
input_data <- readRDS(paste0("data/", args))
phate <- read.csv("data/phate.csv")

# EDA Analysis
## PCA
library(M3C)
pca_result <- prcomp(
  x = input_data$eset,
  scale. = TRUE,
  center = TRUE,
  rank. = 5
)
tsne_result <- tsne(t(input_data$eset))$data
colnames(tsne_result) <- c("Dim1", "Dim2")
eda <- list(pca = pca_result, tsne = tsne_result, phate = phate)

# Differential Expression
source("code/functions.R")
## tests
if(any(log2(input_data$eset) < 0)){tests <- c("ols", "limma")}else{tests <- c("ols", "limma", "vlimma")}
## group comparisons
comparisons <- paste(levels(input_data$response)[1], setdiff(levels(input_data$response), levels(input_data$response)[1]), sep = " vs. ")
## design matrix
design <- model.matrix(~input_data$response)
## topTables
toptables <- lapply(comparisons, function(comparison){
  selectedCoef <- which(levels(input_data$response) == sapply(strsplit(comparison, " vs. "), function(i){ i[[2]]}))
  inner <- lapply(tests, function(test){
    generateTopTable(eset = input_data$eset, design = design, coefNumber = selectedCoef, test = test)
  })
  names(inner) <- tests
  inner
})
names(toptables) <- comparisons

# Geneset enrichment analyses
fdrs <- c(0.01, 0.05, 0.1)
pathwaydbs <- c("Jensen_DISEASES", "KEGG_2019_Human", "WikiPathways_2019_Human")

## enrichR results
enrichr_results <- lapply(comparisons, function(comparison){
  selectedCoef <- which(levels(input_data$response) == sapply(strsplit(comparison, " vs. "), function(i){ i[[2]]}))
  inner <- lapply(tests, function(test){
    top <- generateTopTable(eset = input_data$eset, design = design, coefNumber = selectedCoef, test = test)
    all <- lapply(fdrs, function(fdr){
      enrichr(top$FeatureName[top$adj.P.Val < fdr], pathwaydbs)
    })
    up <- lapply(fdrs, function(fdr){
      enrichr(top$FeatureName[top$adj.P.Val < fdr & top$logFC > 0], "LINCS_L1000_Chem_Pert_down")
    })
    down <- lapply(fdrs, function(fdr){
      enrichr(top$FeatureName[top$adj.P.Val < fdr & top$logFC < 0], "LINCS_L1000_Chem_Pert_up")
    })
    names(all) <- names(up) <- names(down) <- fdrs
    list(all = all, up = up, down = down)
  })
  names(inner) <- tests
  inner
})
names(enrichr_results) <- comparisons

## write out to results
output <- list(input_data  = input_data,
               eda = eda,
               toptables = toptables,
               enrichr_results = enrichr_results)

saveRDS(output, file = "output.rds")
