# Packages and functions
source("source/packages.R", encoding = "UTF-8")
source("source/functions.R", encoding = "UTF-8")

# 0D model for single sector (A)
source("source/[A] readData.R", encoding = "UTF-8")
source("source/[A] model.R", encoding = "UTF-8")
source("source/[A] postProcessing.R", encoding = "UTF-8")
head(df_results)

# 0D model for sectors (B)
source("source/[B] readData.R", encoding = "UTF-8")
source("source/[B] model.R", encoding = "UTF-8")
source("source/[B] postProcessing.R", encoding = "UTF-8")
source("source/[B] Figures ENG/_all.R", encoding = "UTF-8")
head(df_results)


