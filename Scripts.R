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
ortho_df <- ortho_df %>% mutate("2C_Class" = factor(ortho_df$`2C_Class`, levels = c("Normal", "Abnormal")))
ortho_df <- ortho_df %>% mutate("3C_Class" = factor(ortho_df$`3C_Class`, levels = c("Normal", "Hernia", "Spondylolisthesis")))

summary(ortho_df)
summary(sapply(ortho_df,is.na))

ortho_df %>% select(-c(`2C_Class`, `3C_Class`)) %>% pairs()
model.matrix(~0+., ortho_df) %>%cor() %>% ggcorrplot(lab=T, lab_size=2.5,type = "lower")
# Visualization part

ortho_df2 <- ortho_df %>% pivot_longer(cols = c(pelvic_incidence, pelvic_tilt, pelvic_radius, sacral_slope, lumbar_lordosis_angle), names_to = "type_of_measure", values_to = "angles")
ortho_df2 <- ortho_df2 %>% select(type_of_measure, angles, everything())
ortho_df2 %>% ggplot(aes(angles, fill = type_of_measure)) + geom_density(alpha = 0.2)+ facet_grid(~`2C_Class`)
ortho_df2 %>% ggplot(aes(angles, fill = type_of_measure)) + geom_density(alpha = 0.2)+ facet_grid(~`3C_Class`)
ortho_df2 %>% ggplot(aes(y= angles, color = type_of_measure)) + geom_boxplot() + facet_grid(~`2C_Class`)
ortho_df2 %>% ggplot(aes(y= angles, color = type_of_measure)) + geom_boxplot() + facet_grid(~`3C_Class`)
ortho_df2 %>% ggplot(aes(x= angles,y = degree_spondylolisthesis,  color = type_of_measure)) + geom_point() + facet_grid(~`2C_Class`)
ortho_df2 %>% ggplot(aes(x= angles,y = degree_spondylolisthesis,  color = type_of_measure)) + geom_point() + facet_grid(~`3C_Class`)

# Normality test
ortho_df %>% select(where(is.numeric))%>% colnames() %>%
  set_names() %>%  map(~ ks.test(ortho_df[,.x], "pbinom")) %>%
  map_dfr(., tidy, .id = "parameter")