library(shiny)
library(forestplot)
library(dplyr)
library(Cairo)
library(shinydashboard)
library(data.table)
library(shinycssloaders)
library(xtable)
library(shinyBS)
library(shinyLP)
library(shinyjs)
options(xtable.floating = T)
options(xtable.timestamp = "")

#============initial GWAS list
gwasList <- fread("initialgwasList.csv", sep = "#")
presnplist3 <- fread("exposureList.txt")
presnplist <- fread("www/dataResultsTable-res.csv")
#presnplist3 <- fread("www/exposures_MRbase2.txt", sep = "\t")

source("boxes+jumbotron.R", local = T)

source("dashboard_components.R", local = T)