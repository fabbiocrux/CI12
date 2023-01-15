# ================ Introduction à la Recherche Reproductible ================
#  But: Faire une script reprodctible d'analyse de donnes

# CONTENTS
#   0. Introduction
#   1. Reading the Excel/CSV Data
#   2. Transformer les donnes




## 1.1 Loading the libraries -----
library(tidyverse) # Data Science Tools
library(here) # Corresponding file path
library(readxl)  # Read a Excel File

here()
here::i_am("slides/day-03/02-Quantitative/01.")

## 1.2 Reading the Excel table ----
data <- read_excel(path = here("data/Data.xlsx" ))



