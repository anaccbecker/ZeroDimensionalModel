# Packages and functions
source("source/packages.R", encoding = "UTF-8")
source("source/functions.R", encoding = "UTF-8")
Sys.setlocale("LC_TIME", "English")
.start <- as.Date("2012-01-01")
.end <- as.Date("2012-12-31")

# 0D model for single sector (A)
source("source/[A] readData.R", encoding = "UTF-8")
source("source/[A] model.R", encoding = "UTF-8")
source("source/[A] postProcessing.R", encoding = "UTF-8")
head(df_results_A)

# 0D model for sectors (B)
source("source/[B] readData.R", encoding = "UTF-8")
source("source/[B] model.R", encoding = "UTF-8")
source("source/[B] postProcessing.R", encoding = "UTF-8")


source("source/[B] Figures ENG/_all.R", encoding = "UTF-8")
head(df_results_B)

# Delft3D
source("source/[Delft] readData.R", encoding = "UTF-8")
source("source/comparison.R", encoding = "UTF-8")
