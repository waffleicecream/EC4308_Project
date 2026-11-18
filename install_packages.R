# Install every package the project uses. Run once:  Rscript install_packages.R

options(repos = c(CRAN = "https://cloud.r-project.org"))

cran <- c("readxl", "dplyr", "tidyr", "lubridate", "ggplot2", "scales",
          "glmnet", "hdm", "randomForest", "xgboost", "remotes")
missing <- setdiff(cran, rownames(installed.packages()))
if (length(missing)) install.packages(missing)

# ic.glmnet() for the BIC LASSO is only on GitHub
if (!requireNamespace("HDeconometrics", quietly = TRUE))
  remotes::install_github("gabrielrvsc/HDeconometrics", upgrade = "never")

for (p in c(cran, "HDeconometrics"))
  cat(sprintf("%-15s %s\n", p, as.character(packageVersion(p))))
