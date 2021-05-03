# install devtools to install CRAN packages
install.packages(c("remotes"))
require("remotes")
remotes::install_version("devtools", "1.11.0")

# Data manipulation
remotes::install_version("dplyr", version = "1.0.0", dependencies= TRUE)

# Exploratory Data Analysis
## M3C - t-SNE
# Differential Expression Analysis
## LIMMA
remotes::install_version("BiocManager", version = "1.30.10", dependencies= TRUE)
BiocManager::install(c("limma", "M3C"))

# Geneset enrichment analysis
## EnrichR
remotes::install_version("enrichR", version = "2.1", dependencies= TRUE)
# packageurl <- "http://cran.r-project.org/src/contrib/Archive/enrichR/enrichr_2.1.tar.gz"
# install.packages(packageurl, repos=NULL, type="source")
