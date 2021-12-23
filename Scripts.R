library(MASS)
library(ggcorrplot)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyverse)

column_2C <- read_csv("archive/column_2C_weka.csv", 
                    col_types = cols(pelvic_incidence = col_number(), 
                                           `pelvic_tilt numeric` = col_number(), 
                                           lumbar_lordosis_angle = col_number(), 
                                           sacral_slope = col_number(), pelvic_radius = col_number(), 
                                           degree_spondylolisthesis = col_number()))
column_3C <- read_csv("archive/column_3C_weka.csv", 
                      col_types = cols(pelvic_incidence = col_number(), 
                                             pelvic_tilt = col_number(), lumbar_lordosis_angle = col_number(), 
                                             sacral_slope = col_number(), pelvic_radius = col_number(), 
                                             degree_spondylolisthesis = col_number()))

colnames(column_2C) <- colnames(column_3C)

colnames(column_2C) <- colnames(column_3C)

column_2C<-rename(column_2C, "2C_Class" = "class")
column_3C<-rename(column_3C, "3C_Class" = "class")
ortho_df <- merge(column_2C, column_3C, by = c("pelvic_incidence", "pelvic_tilt", "lumbar_lordosis_angle", "sacral_slope", "pelvic_radius", "degree_spondylolisthesis"))

