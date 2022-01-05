library(MASS)
library(ggplot2)
library(ggcorrplot)
library(ggpubr)
library(dplyr)
library(tidyverse)
library(broom)
library(cluster)
library(factoextra)


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

column_2C2<-rename(column_2C, "class" = "class_2")
column_3C2<-rename(column_3C, "class" = "class_3")
colnames(column_2C2) %>% 
  set_names() %>% 
  map(~(column_2C2[,.x]==column_3C2[,.x])) %>% as.data.frame() %>% summary()

column_2C<-rename(column_2C, "class_2" = "class")
column_3C<-rename(column_3C, "class_3" = "class")


ortho_df <- merge(column_2C, column_3C, by = c("pelvic_incidence", "pelvic_tilt", "lumbar_lordosis_angle", "sacral_slope", "pelvic_radius", "degree_spondylolisthesis"))
ortho_df <- ortho_df %>% mutate("class_2" = factor(ortho_df$`class_2`, levels = c("Normal", "Abnormal")))
ortho_df <- ortho_df %>% mutate("class_3" = factor(ortho_df$`class_3`, levels = c("Normal", "Hernia", "Spondylolisthesis")))
ortho_df2 <- ortho_df %>% pivot_longer(cols = c(pelvic_incidence, pelvic_tilt, pelvic_radius, sacral_slope, lumbar_lordosis_angle), names_to = "type_of_measure", values_to = "angles") %>% select(type_of_measure, angles, everything())
ortho_df3 <- ortho_df %>% filter(degree_spondylolisthesis<400) 
ortho_df4 <- ortho_df3 %>% pivot_longer(cols = c(pelvic_incidence, pelvic_tilt, pelvic_radius, sacral_slope, lumbar_lordosis_angle), names_to = "type_of_measure", values_to = "angles") %>% select(type_of_measure, angles, everything())
summary(ortho_df)
summary(sapply(ortho_df,is.na))

ortho_df %>% select(where(is.numeric)) %>% pairs()
model.matrix(~0. + pelvic_incidence+pelvic_tilt+pelvic_radius+sacral_slope+lumbar_lordosis_angle+ degree_spondylolisthesis + class_2, ortho_df) %>%cor() %>% ggcorrplot(lab=T, lab_size=2.5,type = "lower")
model.matrix(~0. + pelvic_incidence+pelvic_tilt+pelvic_radius+sacral_slope+lumbar_lordosis_angle+ degree_spondylolisthesis + class_3, ortho_df) %>%cor() %>% ggcorrplot(lab=T, lab_size=2.5,type = "lower")

# Visualization part
ggarrange(nrow = 3, ncol= 1)
pt <- ortho_df2 %>% ggplot(aes(angles, fill = type_of_measure)) + geom_density(alpha = 0.2)
ptc2 <- ortho_df2 %>% ggplot(aes(angles, fill = type_of_measure)) + geom_density(alpha = 0.2)+ facet_grid(~class_2)
ptc3 <- ortho_df2 %>% ggplot(aes(angles, fill = type_of_measure)) + geom_density(alpha = 0.2)+ facet_grid(~class_3)
pp <- list(pt,ptc2,ptc3)
ggarrange(nrow = 3, ncol= 1, plotlist = pp)
ortho_df2 %>% ggplot(aes(y= angles, color = type_of_measure)) + geom_boxplot() + facet_grid(~class_2)
ortho_df2 %>% ggplot(aes(y= angles, color = type_of_measure)) + geom_boxplot() + facet_grid(~class_3)
ortho_df2 %>% ggplot(aes(x= angles,y = degree_spondylolisthesis,  color = type_of_measure)) + geom_point() + facet_grid(~class_2)
ortho_df2 %>% ggplot(aes(x= angles,y = degree_spondylolisthesis,  color = type_of_measure)) + geom_point() + facet_grid(~class_3)
ortho_df2 %>% ggplot(aes(x =class_3, y= degree_spondylolisthesis, color = type_of_measure)) + geom_boxplot()
ortho_df2 %>% ggplot(aes(x =class_2, y= degree_spondylolisthesis, color = type_of_measure)) + geom_boxplot()
# Normality test

## Kolmogorov-Smirnov 

ortho_df %>% select(where(is.numeric))%>% colnames() %>%
  set_names() %>%  map(~ ks.test(ortho_df[,.x], "pnorm")) %>%
  map_dfr(., tidy, .id = "variable")
ks.test(ortho_df2$degree_spondylolisthesis[ortho_df2$class_3== "Hernia"], "pnorm")
## Shapiro

ortho_df %>% select(where(is.numeric))%>% colnames() %>%
  set_names() %>%  map(~ shapiro.test(ortho_df[,.x])) %>%
  map_dfr(., tidy, .id = "variable")
shapiro.test(ortho_df2$degree_spondylolisthesis[ortho_df2$class_3== "Hernia"])
## QQplot
gqqplot(ortho_df2, "angles", facet.by = "class_3", color = "type_of_measure")
ggqqplot(ortho_df2, "angles", facet.by = "class_2", color = "type_of_measure")
ggqqplot(ortho_df2, "degree_spondylolisthesis", color = "class_3", facet.by  = "class_2")


# LRM 

model_pi <- lm(pelvic_incidence ~ lumbar_lordosis_angle, data = ortho_df)
model_pi_k = coef(model_pi)

ortho_df %>% ggplot(aes(lumbar_lordosis_angle, pelvic_incidence, color = class_3)) + geom_point() + geom_smooth(method = "lm") + geom_abline(slope = model_pi_k[["lumbar_lordosis_angle"]], intercept = model_pi_k[["(Intercept)"]])
ortho_df %>% ggplot(aes(lumbar_lordosis_angle, pelvic_incidence, color = class_2)) + geom_point() + geom_smooth(method = "lm") + geom_abline(slope = model_pi_k[["lumbar_lordosis_angle"]], intercept = model_pi_k[["(Intercept)"]])

model_pi_mult <- lm(pelvic_incidence ~ lumbar_lordosis_angle + pelvic_radius+ degree_spondylolisthesis + class_2 + class_3, data = ortho_df)

step(model_pi_mult, direction = "both", trace = 1)

model_pi_mult <- lm(pelvic_incidence ~ lumbar_lordosis_angle + pelvic_radius+ degree_spondylolisthesis, data = ortho_df)

model_pi_mult_clean <- lm(formula = pelvic_incidence ~ lumbar_lordosis_angle + pelvic_radius + degree_spondylolisthesis, data = ortho_df)
predicted_pelvic_incidence <- data.frame(pelvic_incidence_pred = predict(model_pi_mult_clean, ortho_df), lumbar_lordosis_angle = ortho_df$lumbar_lordosis_angle, pelvic_radius= ortho_df$pelvic_radius, degree_spondylolisthesis = ortho_df$degree_spondylolisthesis)
ggplot(ortho_df, aes(lumbar_lordosis_angle, pelvic_incidence)) +
  geom_point(color = "blue") +
  geom_abline(slope = model_pi_k[["lumbar_lordosis_angle"]], intercept = model_pi_k[["(Intercept)"]]) +
  geom_line(data = predicted_pelvic_incidence, aes(lumbar_lordosis_angle, pelvic_incidence_pred), color = "darkgreen")

par(mfrow=c(1,2))
resid_lr <- rstandard(model_pi)
plot(fitted.values(model_pi), resid_lr)
abline(h=0)
resid_mr <- rstandard(model_pi_mult_clean)
plot(fitted.values(model_pi_mult_clean), resid_mr)
abline(h=0)

# ANOVA y Custering

## Valores médias y desviaciónes

### En todos grupos
ortho_df2 %>% group_by(type_of_measure) %>% summarise("Mean of angle" = mean(angles), "Standart deviation" = sd(angles))

ortho_df2 %>%  summarise("Mean of displacement" = mean(degree_spondylolisthesis), "SD of displacement" = sd(degree_spondylolisthesis))


ortho_df2 %>% ggplot(aes(y= angles, x = type_of_measure, fill = type_of_measure)) +
  geom_boxplot() + stat_summary(fun =mean, shape = 18, size = 1)

ortho_df2 %>% ggplot(aes(y= degree_spondylolisthesis, fill = "red", x = 0)) +
  geom_boxplot() +
  stat_summary(fun = mean, shape = 18, size =1)

### En grupos por 2 classificaciones
ortho_df2 %>% group_by(type_of_measure, class_2) %>%
  summarise("Mean of angle" = mean(angles), "Standart deviation" = sd(angles))
ortho_df2 %>% group_by(class_2) %>%
  summarise("Mean of displacement" = mean(degree_spondylolisthesis), "SD of displacement" = sd(degree_spondylolisthesis))

ortho_df2 %>% ggplot(aes(x =class_2, y= degree_spondylolisthesis, color = class_2)) +
  geom_boxplot() +
  stat_summary(fun.y = mean, geom = "point", shape = 18, size = 3)

ortho_df2 %>% ggplot(aes(y= angles, x = class_2, color = type_of_measure)) +
  geom_boxplot() +
  stat_summary(fun.y = mean, geom = "point", shape = 18, size = 3)

### En grupos por 3 classificaciones
ortho_df2 %>% group_by(type_of_measure, class_3) %>% 
  summarise("Mean of angle" = mean(angles), "Standart deviation" = sd(angles))

ortho_df2 %>% group_by(class_3) %>%  
  summarise("Mean of displacement" = mean(degree_spondylolisthesis), "SD of displacement" = sd(degree_spondylolisthesis))

ortho_df2 %>% ggplot(aes(x =class_3, y= degree_spondylolisthesis, color = class_3)) +
  geom_boxplot() +
  stat_summary(fun.y = mean, geom = "point", shape = 18, size = 3)

ortho_df2 %>% ggplot(aes(y= angles,x =class_3,color = type_of_measure)) +
  geom_boxplot() + 
  stat_summary(fun.y = mean, geom = "point", shape = 18, size = 3)

## Normality tests
### Kolmogorov-Smirnov

ortho_df %>% select(where(is.numeric))%>% colnames() %>%
  set_names() %>%  map(~ ks.test(ortho_df[,.], "pnorm")) %>%
  map_dfr(., tidy, .id = "variable")

### Shapiro
ortho_df %>% select(where(is.numeric))%>% colnames() %>%
  set_names() %>%  map(~ shapiro.test(ortho_df[,.x])) %>%
  map_dfr(., tidy, .id = "variable")

### QQplot
gqqplot(ortho_df2, "angles", facet.by = "class_3", color = "type_of_measure")
ggqqplot(ortho_df2, "angles", facet.by = "class_2", color = "type_of_measure")
ggqqplot(ortho_df2, "degree_spondylolisthesis", color = "class_3", facet.by  = "class_2")


### Homogenidad
ortho_df %>% select(where(is.numeric))%>% colnames() %>%
  set_names() %>%  map(~ fligner.test(ortho_df[,.] ~ class_2, data = ortho_df)) %>%
  map_dfr(., tidy, .id = "variable")

# Anova
anova_PI <- aov(pelvic_incidence~class_3, ortho_df)
anova_S <- aov(degree_spondylolisthesis~class_3, ortho_df)
anova_PR <- aov(pelvic_radius~class_3, ortho_df)
anova_LLA <- aov(lumbar_lordosis_angle~class_3, ortho_df)

par(mfrow=c(2,2))
plot(anova_PI)
par(mfrow=c(1,1))

plot(TukeyHSD(anova_PI))
par(mfrow=c(2,2))
plot(anova_S)
par(mfrow=c(1,1))

plot(TukeyHSD(anova_S))
par(mfrow=c(2,2))
plot(anova_PR)

par(mfrow=c(1,1))
plot(TukeyHSD(anova_PR))

par(mfrow=c(2,2))
plot(anova_LLA)
plot(TukeyHSD(anova_LLA))
par(mfrow=c(1,1))

## Cluster 

ortho_df %>% ggplot(aes(lumbar_lordosis_angle, pelvic_incidence, color = class_3)) +
  geom_point()
ortho_df %>% ggplot(aes(lumbar_lordosis_angle, pelvic_incidence, color = class_2)) +
  geom_point()

cluster3 <- kmeans(ortho_df[, c(1,5)], 3, nstart = 25)
table(cluster3$cluster, ortho_df$class_3)
summary(cluster3)
fviz_cluster(cluster3, data = ortho_df[, c(1:6)])
cluster2 <- kmeans(ortho_df[, c(1,5)], 2, nstart = 25)
table(cluster2$cluster, ortho_df$class_2)
fviz_cluster(cluster2, data = ortho_df[, c(1,3)])


cluster <- hclust(dist(ortho_df), method="complete")
plot(cluster)
pltree(agnes(ortho_df, method = "complete"))


ortho_df2 %>% 
  group_by(type_of_measure) %>%
  summarise("Mean of angle" = mean(angles),
            "Standart deviation" = sd(angles),"+CI" = confint(lm(angles~1, ortho_df))[,1], "-CI" = confint(lm(angles~1, ortho_df))[,2])

confint(lm(lumbar_lordosis_angle~1, ortho_df))


ortho_df_n <- ortho_df %>% 
  mutate(pelvic_incidence_n = (pelvic_incidence-min(ortho_df$pelvic_incidence))/(max(pelvic_incidence)-min(pelvic_incidence)),
         pelvic_tilt_n= (pelvic_tilt- min(ortho_df$pelvic_tilt))/(max(ortho_df$pelvic_tilt)-min(ortho_df$pelvic_tilt)),
         lumbar_lordosis_angle_n = (lumbar_lordosis_angle- min(ortho_df$lumbar_lordosis_angle))/(max(ortho_df$lumbar_lordosis_angle)- min(ortho_df$lumbar_lordosis_angle)), 
         sacral_slope_n = (sacral_slope-min(ortho_df$sacral_slope))/(max(ortho_df$sacral_slope)- min(ortho_df$sacral_slope)), 
         pelvic_radius_n =(pelvic_radius - min(ortho_df$pelvic_radius))/(max(ortho_df$pelvic_radius)-min(ortho_df$pelvic_radius)), 
         degree_spondylolisthesis_n =(degree_spondylolisthesis - min(ortho_df$degree_spondylolisthesis))/(max(ortho_df$degree_spondylolisthesis)-min(ortho_df$degree_spondylolisthesis))
         )

cluster3_n <- kmeans(ortho_df_n[, c(9,11)], 3, nstart = 25)
cluster3_n
table(cluster3$cluster, ortho_df_n$class_3)
summary(cluster3_n)
fviz_cluster(cluster3_n, data = ortho_df_n[, c(9,11)])



ortho_df2 %>% ggplot(aes(y= angles, x = type_of_measure, fill = type_of_measure)) +
  geom_boxplot() + 
  stat_summary(fun = mean,
                                shape = 18,
                                size = 1)+ 
  stat_summary(fun = mean,
               fun.min = function(x) {mean(x) - sd(x)},
               fun.max =function(x) {mean(x) + sd(x)},
               geom = "errorbar")+ 
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())



ax <- ortho_df2 %>% ggplot(aes(y= angles, x = type_of_measure, fill = type_of_measure)) +
  geom_boxplot() + stat_summary(fun = mean,
                                shape = 18,
                                size = 1) + 
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())
ax2 <- ortho_df2 %>% ggplot(aes(y= angles, x = type_of_measure), color = "black") +
  geom_boxplot(alpha = 0.3, aes(fill = type_of_measure)) +
  stat_summary(fun.y = mean,
               geom = "point",
               shape = 18, size = 4,
               aes(color = type_of_measure))+
  facet_wrap(~class_2)+ 
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

ax3 <- ortho_df2 %>% ggplot(aes(y= angles, x = type_of_measure), color = "black") +
  geom_boxplot(alpha = 0.3, aes(fill = type_of_measure)) +
  stat_summary(fun.y = mean, 
               geom = "point", shape = 18,
               size = 4, 
               aes(color = type_of_measure)) +
  facet_wrap(~class_3)+ 
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())
axds <- list(ax,ax2,ax3)
ggarrange(nrow = 3, ncol= 1, plotlist = axds)


dx <- ortho_df2 %>% ggplot(aes(y= degree_spondylolisthesis,
                               x = 0)) +
  geom_boxplot(fill = "red",
               alpha = 0.4) +
  stat_summary(fun = mean, shape = 18,
               size =1)+ 
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

dx2 <- ortho_df2 %>% ggplot(aes(x =0,
                                y= degree_spondylolisthesis,
                                fill= class_2),
                            color = black) +
  geom_boxplot(alpha = 0.6,
               aes(fill = class_2)) +
  stat_summary(fun.y = mean,
               geom = "point",
               shape = 18, size = 5, 
               aes(color = class_2))+
  facet_wrap(~class_2)+ 
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())


dx3 <- ortho_df2 %>% ggplot(aes(x = 0, y= degree_spondylolisthesis), color = black) +
  geom_boxplot(alpha = 0.4, aes(fill = class_3)) +
  stat_summary(fun.y = mean, geom = "point", shape = 18, size = 5, aes(color = class_3))+ facet_wrap(~class_3)+ 
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())
dxds <- list(dx,dx2,dx3)
ggarrange(nrow = 3, ncol= 1, plotlist = dxds)



ortho_df %>% group_by(class_2) %>%
  summarise("Mean PI" = mean(pelvic_incidence),
            "SD PI" = sd(pelvic_incidence), 
            "Mean PT" = mean(pelvic_tilt),
            "SD PT" = sd(pelvic_tilt), 
            "Mean LLA" = mean(lumbar_lordosis_angle),
            "SD LLA" = sd(lumbar_lordosis_angle),
            "Mean SS" = mean(sacral_slope),
            "SD SS" = sd(sacral_slope), 
            "Mean PR" = mean(pelvic_radius), 
            "SD PR" = sd(pelvic_radius),
            "Mean DS" = mean(degree_spondylolisthesis),
            "SD DS" = sd(degree_spondylolisthesis))


ortho_df %>% group_by(class_3) %>%
  summarise("Mean PI" = mean(pelvic_incidence),
            "SD PI" = sd(pelvic_incidence), 
            "Mean PT" = mean(pelvic_tilt),
            "SD PT" = sd(pelvic_tilt), 
            "Mean LLA" = mean(lumbar_lordosis_angle),
            "SD LLA" = sd(lumbar_lordosis_angle),
            "Mean SS" = mean(sacral_slope),
            "SD SS" = sd(sacral_slope), 
            "Mean PR" = mean(pelvic_radius), 
            "SD PR" = sd(pelvic_radius),
            "Mean DS" = mean(degree_spondylolisthesis),
            "SD DS" = sd(degree_spondylolisthesis))
stat_summary(stat_summary(fun = mean, geom = "point", shape = 18, size = 5, aes(y = degree_spondylolisthesis)))+
  stat_summary(aes(y = degree_spondylolisthesis),
               fun = mean,
               fun.min = function(x) {mean(x) - sd(x)},
               fun.max =function(x) {mean(x) + sd(x)},
               geom = "errorbar")

stat_summary(mapping = aes(x = 0, y=degree_spondylolisthesis), 
             color = "green",
             fun = mean,
             shape = 18,
             size = 1)+ 
  stat_summary(mapping = aes(x = 0, y=degree_spondylolisthesis), 
               color = "green",
               fun = mean,
               fun.min = function(x) {mean(x) - sd(x)},
               fun.max =function(x) {mean(x) + sd(x)},
               geom = "errorbar")

angles_ms <- ortho_df2 %>% ggplot(aes(y= angles, x = type_of_measure, color = type_of_measure)) +
  stat_summary(fun = mean,
               shape = 18,
               size = 1)+ 
  stat_summary(fun = mean,
               fun.min = function(x) {mean(x) - sd(x)},
               fun.max =function(x) {mean(x) + sd(x)},
               geom = "errorbar")+ 
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())
  
angles_ms2 <- ortho_df2 %>% ggplot(aes(y= angles, x = type_of_measure, color = type_of_measure)) +
  stat_summary(fun = mean,
               shape = 18,
               size = 1)+ 
  stat_summary(fun = mean,
               fun.min = function(x) {mean(x) - sd(x)},
               fun.max =function(x) {mean(x) + sd(x)},
               geom = "errorbar")+ 
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  facet_wrap(~class_2)

angles_ms3 <- ortho_df2 %>% ggplot(aes(y= angles, x = type_of_measure, color = type_of_measure)) +
  stat_summary(fun = mean,
               shape = 18,
               size = 1)+ 
  stat_summary(fun = mean,
               fun.min = function(x) {mean(x) - sd(x)},
               fun.max =function(x) {mean(x) + sd(x)},
               geom = "errorbar")+ 
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  facet_wrap(~class_3)

degree_ms <- ortho_df2 %>% ggplot(aes(y= degree_spondylolisthesis, x = 0)) +
  stat_summary(fun = mean,
               shape = 18,
               size = 1,
               color = "green")+ 
  stat_summary(fun = mean,
               fun.min = function(x) {mean(x) - sd(x)},
               fun.max =function(x) {mean(x) + sd(x)},
               color = "green",
               geom = "errorbar")+ 
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

degree_ms2 <- ortho_df2 %>% ggplot(aes(y= degree_spondylolisthesis, x = class_2, color = class_2)) +
  stat_summary(fun = mean,
               shape = 18,
               size = 1)+ 
  stat_summary(fun = mean,
               fun.min = function(x) {mean(x) - sd(x)},
               fun.max =function(x) {mean(x) + sd(x)},
               geom = "errorbar")+ 
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())  +
  facet_wrap(~class_2)

degree_ms3 <- ortho_df2 %>% ggplot(aes(y= degree_spondylolisthesis, x = class_3, color = class_3)) +
  stat_summary(fun = mean,
               shape = 18,
               size = 1)+ 
  stat_summary(fun = mean,
               fun.min = function(x) {mean(x) - sd(x)},
               fun.max =function(x) {mean(x) + sd(x)},
               geom = "errorbar")+ 
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  facet_wrap(~class_3)

ggarrange(plotlist= list(angles_ms,degree_ms, angles_ms2, degree_ms2, angles_ms3, degree_ms3) , nrow= 3, ncol = 2, legend = "bottom")




