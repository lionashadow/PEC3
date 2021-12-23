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

column_2C<-rename(column_2C, "class_2" = "class")
column_3C<-rename(column_3C, "class_3" = "class")


ortho_df <- merge(column_2C, column_3C, by = c("pelvic_incidence", "pelvic_tilt", "lumbar_lordosis_angle", "sacral_slope", "pelvic_radius", "degree_spondylolisthesis"))
ortho_df <- ortho_df %>% mutate("class_2" = factor(ortho_df$`class_2`, levels = c("Normal", "Abnormal")))
ortho_df <- ortho_df %>% mutate("class_3" = factor(ortho_df$`class_3`, levels = c("Normal", "Hernia", "Spondylolisthesis")))
ortho_df2 <- ortho_df %>% pivot_longer(cols = c(pelvic_incidence, pelvic_tilt, pelvic_radius, sacral_slope, lumbar_lordosis_angle), names_to = "type_of_measure", values_to = "angles") %>% select(type_of_measure, angles, everything())
ortho_df3 <- ortho_df %>% select(where(is.numeric)) 

summary(ortho_df)
summary(sapply(ortho_df,is.na))

ortho_df %>% select(where(is.numeric)) %>% pairs()
model.matrix(~0. + pelvic_incidence+pelvic_tilt+pelvic_radius+sacral_slope+lumbar_lordosis_angle+ degree_spondylolisthesis + class_3, ortho_df) %>%cor() %>% ggcorrplot(lab=T, lab_size=2.5,type = "lower")
model.matrix(~0. + pelvic_incidence+pelvic_tilt+pelvic_radius+sacral_slope+lumbar_lordosis_angle+ degree_spondylolisthesis + class_3, ortho_df) %>%cor() %>% ggcorrplot(lab=T, lab_size=2.5,type = "lower")

# Visualization part


ortho_df2 %>% ggplot(aes(angles, fill = type_of_measure)) + geom_density(alpha = 0.2)+ facet_grid(~class_2)
ortho_df2 %>% ggplot(aes(angles, fill = type_of_measure)) + geom_density(alpha = 0.2)+ facet_grid(~class_3)
ortho_df2 %>% ggplot(aes(y= angles, color = type_of_measure)) + geom_boxplot() + facet_grid(~class_2)
ortho_df2 %>% ggplot(aes(y= angles, color = type_of_measure)) + geom_boxplot() + facet_grid(~class_3)
ortho_df2 %>% ggplot(aes(x= angles,y = degree_spondylolisthesis,  color = type_of_measure)) + geom_point() + facet_grid(~class_2)
ortho_df2 %>% ggplot(aes(x= angles,y = degree_spondylolisthesis,  color = type_of_measure)) + geom_point() + facet_grid(~class_3)
ortho_df2 %>% ggplot(aes(x =class_3, y= degree_spondylolisthesis, color = type_of_measure)) + geom_boxplot()

# Normality test

## Kolmogorov-Smirnov

ortho_df %>% select(where(is.numeric))%>% colnames() %>%
  set_names() %>%  map(~ ks.test(ortho_df[,.x], "pnorm")) %>%
  map_dfr(., tidy, .id = "parameter")
ks.test(ortho_df2$degree_spondylolisthesis[ortho_df2$class_3== "Hernia"], "pnorm")
## Shapiro

ortho_df %>% select(where(is.numeric))%>% colnames() %>%
  set_names() %>%  map(~ shapiro.test(ortho_df[,.x])) %>%
  map_dfr(., tidy, .id = "parameter")
shapiro.test(ortho_df2$degree_spondylolisthesis[ortho_df2$class_3== "Hernia"])
## QQplot
gqqplot(ortho_df2, "angles", facet.by = "class_3", color = "type_of_measure")
ggqqplot(ortho_df2, "angles", facet.by = "class_2", color = "type_of_measure")
ggqqplot(ortho_df2, "degree_spondylolisthesis", color = "class_3", facet.by  = "class_2")


