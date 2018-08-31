rm(list = ls()) 

# Packages ----
library(shiny)
library(shinydashboard)
library(data.table)
library(DT)
library(plotly)
library(readxl)

dashboard.naam <- "IPU Quickscan"

mappingFileToRead <- "mapping.xlsx"

csvFile <- "Quickscan VBHC.csv"

mappingAnswers <- as.data.table(read_xlsx(mappingFileToRead, sheet = "Blad2"))
domainQuestions <- as.data.table(read_xlsx(mappingFileToRead, sheet = "Blad1"))

stdPercentages <- c(1, 2, 3, 4, 5)
colors <- c("#E57373", "#FFAB91", "#FFE082", "#E6EE9C", "#A5D6A7")

quantiles <- c(0.05, 0.25, 0.5, 0.75, 0.95)
