#**************TRABAJO PRACTICO N1 ANALISIS INTELIGENTE DEE DATOS**************
#*-----------------------------------------------------------------------------
#-VEIGA CRISTIAN - COMISION B
#------------------------------------------------------------------------------

#deshabilitar la notacion cientifica
options(scipen=999)
#limpio memoria
rm(list=ls())
gc()
#cargo librerias
library(readxl)
library(tidyverse)
library(dplyr)
#library(data.table)
library(ggplot2)
library(plotly)
library(summarytools)
library(treemap)
library(mice)

setwd("C:/Cursos/mcd/austral-mcd-aid/TP2")