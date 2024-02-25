---
title: "Modeling Guediawaye Census data "
author: "HEMA Aboubacar"
date: "2024-02-25"
output: 
  html_document:
    toc: true
    toc_depth: 3
    toc_float: true
    number_sections: true
    code_folding: show
    keep_md: true
---






```r
rm(list=ls())
```

## Importing library


```r
## Importing library
### List of required packages
required_packages <- c("tidyverse","janitor" ,"readr","dplyr","haven","sf", "flextable","sp", "factoextra", "FactoMineR","gtsummary", "sjPlot", "fastDummies","ggthemes","spdep","patchwork","corrr")

# Check if packages are installed
missing_packages <- setdiff(required_packages, installed.packages()[,"Package"])

### Install missing packages
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

### Load all packages
lapply(required_packages, library, character.only = TRUE)
```



```r
# Read shapefile data for 2013 and 2013


MPI_data_dr_2013 <- sf::read_sf(paste0(here::here(),"/output/output_data/MPI_data_dr_2013.shp"))
MPI_data_dr_2002 <- sf::read_sf(paste0(here::here(),"/output/output_data/MPI_data_dr_2002.shp"))
MPI_data_dr <- MPI_data_dr_2013 %>%
  plyr::rbind.fill(MPI_data_dr_2002) %>% 
  st_as_sf()
```


```r
names(MPI_data_dr)
```

```
##  [1] "id_dr"    "REGION"   "DEPT"     "NOM_ARR"  "NOM_CA"   "RGPH"    
##  [7] "homme"    "femme"    "hh_size"  "fertlty"  "Aucun"    "Primair" 
## [13] "Moyen"    "Secondr"  "Superir"  "Ltrcy_F"  "Ltrcy_A"  "Ltrcy_W" 
## [19] "Ltrcy_P"  "Ltrcy_M"  "Ltrcy_D"  "Ltrcy_S"  "rsdnt_p"  "rsdnt_b" 
## [25] "I_rps_s"  "I_sns_m"  "MPI_men"  "menage"   "mdn_cm_"  "mn_cm_g" 
## [31] "cm_homm"  "cm_femm"  "pct_cm_h" "pct_cm_f" "geometry"
```


# Regression modeling with Guediawaye Census data


```r
outcome = "MPI_men"
vars_to_remove = c("id_dr","REGION","DEPT","NOM_ARR","NOM_CA","RGPH","homme",    "femme","hh_size","Aucun","Superir","Ltrcy_P","Ltrcy_S","Ltrcy_D","rsdnt_p","rsdnt_b","I_rps_s","I_sns_m","MPI_men","mdn_cm_","mn_cm_g","cm_homm","cm_femm","pct_cm_h","geometry")

labels_var = c("Id dr","Region","Dept.","Nom Arr.","Nom CA","RGPH","Nombre d'homme","Nombre de femme","Taille ménage","Fertilité","Niveau d'instruction: Aucun","Niveau d'instruction: Primaire","Niveau d'instruction: Moyen","Niveau d'instruction: Secondaire", "Niveau d'instruction: Supérieur","Nombre de French dans le ménage","Nombre de Arabic dans le ménage","Nombre de Wolof dans le ménage","Nombre de Pulaar dans le ménage","Nombre de Sereer dans le ménage","Nombre de Mandingo dans le ménage","Nombre de Diola dans le ménage","Nombre de Soninke dans le ménage","Nombre de résident permanent","Nombre de Résident Absent","Nombre de repas sauté","Nombre de soins médicaux","MPI","Nombre de ménage dans le DR","Age médian du CM dans le DR","Age moyen du CM dans le DR","Nombre de CM de sexe masculin","Nombre de CM de sexe feminin","Proportion de CM de sexe masculin","Proportion de CM de sexe féminin","geometry")

label_to_remove = c("Id dr","Region","Dept.","Nom Arr.","Nom CA","RGPH","Nombre d'homme","Nombre de femme","Taille ménage","Niveau d'instruction: Aucun", "Niveau d'instruction: Supérieur","Nombre de Pulaar dans le ménage","Nombre de Sereer dans le ménage","Nombre de Diola dans le ménage","Nombre de résident permanent","Nombre de Résident Absent","Nombre de repas sauté","Nombre de soins médicaux","MPI","Age médian du CM dans le DR","Age moyen du CM dans le DR","Nombre de CM de sexe masculin","Nombre de CM de sexe feminin","Proportion de CM de sexe masculin","geometry")

predictors_2002 = setdiff(names(MPI_data_dr_2002),vars_to_remove)
label_predictors_2002 = append(setdiff(labels_var, label_to_remove), "densité de la population")
  
predictors_2013 =  setdiff(names(MPI_data_dr_2013),vars_to_remove)
label_predictors_2013 = append(setdiff(labels_var, label_to_remove), "densité de la population")

formula_2002 = paste0("log(",outcome,") ~ ",paste(predictors_2002[-length(predictors_2002)]," + ", collapse = ""),predictors_2002[length(predictors_2002)])

formula_2013 = paste0("log(",outcome,") ~ ",paste(predictors_2013[-length(predictors_2013)]," + ", collapse = ""),predictors_2013[length(predictors_2013)])


formula_2002 <- paste0(formula_2002, " + pop_density")
formula_2013 <- paste0(formula_2013, " + pop_density")
```

## Inspecting the outcome variable (MPI) with visualization


```r
mhv_map <- ggplot(MPI_data_dr_2013, aes(fill = MPI_men)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c(direction = -1) + 
  theme_void() + 
  labs(fill = "MPI ")

mhv_histogram <- ggplot(MPI_data_dr_2013, aes(x = MPI_men)) + 
  geom_histogram(alpha = 0.5, fill = "navy", color = "navy",
                 bins = 100) + 
  theme_minimal() + 
  scale_x_continuous(labels = scales::label_number_si(accuracy = 0.1)) + 
  labs(x = "MPI")

#grid.arrange(mhv_map, mhv_histogram, nrow = 1)
# mhv_map + mhv_histogram + labs(title = "MPI value charts for Guediawaye Census 2013")
gridExtra::grid.arrange(mhv_map, mhv_histogram, nrow = 1,
                        top = "MPI value charts for Guediawaye Census 2013")
```

![](05_Modelling-Senegal-Census-data_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
ggsave(paste0(here::here(),"/output/output_model/img/1_hv_histogram.png"), width = 8, height = 5)
```



```r
MPI_data_dr %>% 
  mutate(RGPH=ifelse(RGPH=="2013","Census 2013", "Census 2002")) %>% 
    
      ggplot(aes(fill = MPI_men)) + 
      
      geom_sf(color = NA) + 
  scale_fill_viridis_c(direction = -1) + 
  theme_void() + 
  labs(fill = "MPI")+
       
      facet_wrap(~RGPH) +
      
      # Adjusting the plot theme
      theme_map(base_size = 8) +
      theme(panel.background = element_rect(),
                #legend.background = element_blank(),
                axis.ticks = element_blank(),
                axis.text = element_blank(),
                legend.position = "bottom", 
                text = element_text(size = 8), 
                #panel.grid = element_line(color = "white", size = 0.8),
                legend.box.background = element_rect(),
                legend.box.margin = margin(6, 6, 6, 6),
                plot.background = element_rect(fill = "white"))+
      ggspatial::annotation_scale(
        location = "br",
        bar_cols = c("grey60", "white"),
        text_family = "ArcherPro Book"
      ) +
      ggspatial::annotation_north_arrow(
        location = "tl", which_north = "true",
        #pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
        #which_north = "grid",
      height = unit(1, "cm"),
      width = unit(1, "cm"),
      )
```

![](05_Modelling-Senegal-Census-data_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
ggsave(paste0(here::here(),"/output/output_model/img/2_mpi_rgph.png"), width = 8, height = 5)
```

```r
MPI_data_dr %>% 
  mutate(RGPH=ifelse(RGPH=="2013","Census 2013", "Census 2002")) %>%
  ggplot(aes(x = MPI_men)) + 
  geom_histogram(alpha = 0.5, fill = "navy", color = "navy",
                 bins = 100) + 
  theme_minimal() + 
  scale_x_continuous(labels = scales::label_number_si(accuracy = 0.1)) +
      xlab("MPI")+
      facet_wrap(~RGPH) +
      # Adjusting the plot theme
      theme_map(base_size = 8) +
      theme(panel.background = element_rect(),
                #legend.background = element_blank(),
                axis.ticks = element_blank(),
                axis.text = element_blank(),
                legend.position = "bottom", 
                text = element_text(size = 8), 
                #panel.grid = element_line(color = "white", size = 0.8),
                legend.box.background = element_rect(),
                legend.box.margin = margin(6, 6, 6, 6),
                plot.background = element_rect(fill = "white"))
```

![](05_Modelling-Senegal-Census-data_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
ggsave(paste0(here::here(),"/output/output_model/img/3_mpi_rgph_hist.png"), width = 8, height = 5)
```



```r
mhv_map_log <- ggplot(MPI_data_dr_2013, aes(fill = log(MPI_men))) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c(direction = -1) + 
  theme_void() + 
  labs(fill = "MPI\nvalue (log)")

mhv_histogram_log <- ggplot(MPI_data_dr_2013, aes(x = log(MPI_men))) + 
  geom_histogram(alpha = 0.5, fill = "navy", color = "navy",
                 bins = 100) + 
  theme_minimal() + 
  scale_x_continuous() + 
  labs(x = "MPI (log)")

gridExtra::grid.arrange(mhv_map_log, mhv_histogram_log, nrow = 1,
                        top = "Logged MPI value charts for Guediawaye Census 2013")
```

![](05_Modelling-Senegal-Census-data_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
ggsave(paste0(here::here(),"/output/output_model/img/4_hv_histogram_log.png"), width = 8, height = 5)
```



```r
MPI_data_dr %>% 
    
      ggplot(aes(fill = log(MPI_men))) + 
      
      geom_sf(color = NA) + 
  scale_fill_viridis_c(direction = -1) + 
  theme_void() + 
  labs(fill = "MPI\nvalue (log)")+
       
      facet_wrap(~RGPH) +
      
      # Adjusting the plot theme
      theme_map(base_size = 8) +
      theme(panel.background = element_rect(),
                #legend.background = element_blank(),
                axis.ticks = element_blank(),
                axis.text = element_blank(),
                legend.position = "bottom", 
                text = element_text(size = 8), 
                #panel.grid = element_line(color = "white", size = 0.8),
                legend.box.background = element_rect(),
                legend.box.margin = margin(6, 6, 6, 6),
                plot.background = element_rect(fill = "white"))+
      ggspatial::annotation_scale(
        location = "br",
        bar_cols = c("grey60", "white"),
        text_family = "ArcherPro Book"
      ) +
      ggspatial::annotation_north_arrow(
        location = "tl", which_north = "true",
        #pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
        #which_north = "grid",
      height = unit(1, "cm"),
      width = unit(1, "cm"),
      )
```

![](05_Modelling-Senegal-Census-data_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
ggsave(paste0(here::here(),"/output/output_model/img/5_log_mpi_rgph.png"), width = 8, height = 5)
```


## A first regression model


```r
library(sf)
library(units)

predictors = c("menage","Primair","Moyen","Secondr","Ltrcy_F","pct_cm_f","hh_size","Ltrcy_A","Ltrcy_W","Ltrcy_M","fertlty")

outcome = "MPI_men"
MPI_data_dr_2013_for_model<- MPI_data_dr_2013 %>%
  dplyr::select(MPI_men,predictors) %>% 
  mutate(pop_density = as.numeric(set_units(hh_size / st_area(.), "1/km2"))) %>% 
  dplyr::select(-hh_size)

##
MPI_data_dr_2002_for_model<- MPI_data_dr_2002 %>%
  dplyr::select(MPI_men,predictors) %>% 
  mutate(pop_density = as.numeric(set_units(hh_size / st_area(.), "1/km2"))) %>% 
  dplyr::select(-hh_size)
```


```r
# formula <- "log(MPI_men) ~ menage  + Primair + Moyen + Secondr + Ltrcy_F + pct_cm_f + pop_density + Ltrcy_A + Ltrcy_W + Ltrcy_M + fertlty"


### For 2002
model_2002 <- lm(formula = formula_2002, data = MPI_data_dr_2002_for_model)

### For 2013

model_2013 <- lm(formula = formula_2013, data = MPI_data_dr_2013_for_model)



stargazer::stargazer(model_2002, model_2013, type="html",

keep = predictors_2002,

covariate.labels = label_predictors_2002, out = (paste0(here::here(),"/output/output_model/1_reg_lineaire.html")))
```

```
## 
## <table style="text-align:center"><tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td colspan="2"><em>Dependent variable:</em></td></tr>
## <tr><td></td><td colspan="2" style="border-bottom: 1px solid black"></td></tr>
## <tr><td style="text-align:left"></td><td colspan="2">log(MPI_men)</td></tr>
## <tr><td style="text-align:left"></td><td>(1)</td><td>(2)</td></tr>
## <tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Fertilité</td><td>0.00004</td><td>0.0001</td></tr>
## <tr><td style="text-align:left"></td><td>(0.0001)</td><td>(0.0001)</td></tr>
## <tr><td style="text-align:left"></td><td></td><td></td></tr>
## <tr><td style="text-align:left">Niveau d'instruction: Primaire</td><td>-0.001</td><td>0.00004</td></tr>
## <tr><td style="text-align:left"></td><td>(0.001)</td><td>(0.0004)</td></tr>
## <tr><td style="text-align:left"></td><td></td><td></td></tr>
## <tr><td style="text-align:left">Niveau d'instruction: Moyen</td><td>-0.005<sup>***</sup></td><td>-0.004<sup>***</sup></td></tr>
## <tr><td style="text-align:left"></td><td>(0.001)</td><td>(0.001)</td></tr>
## <tr><td style="text-align:left"></td><td></td><td></td></tr>
## <tr><td style="text-align:left">Niveau d'instruction: Secondaire</td><td>-0.004<sup>**</sup></td><td>-0.005<sup>***</sup></td></tr>
## <tr><td style="text-align:left"></td><td>(0.002)</td><td>(0.001)</td></tr>
## <tr><td style="text-align:left"></td><td></td><td></td></tr>
## <tr><td style="text-align:left">Nombre de French dans le ménage</td><td>0.001</td><td>-0.001<sup>***</sup></td></tr>
## <tr><td style="text-align:left"></td><td>(0.001)</td><td>(0.0003)</td></tr>
## <tr><td style="text-align:left"></td><td></td><td></td></tr>
## <tr><td style="text-align:left">Nombre de Arabic dans le ménage</td><td>0.0003<sup>***</sup></td><td>0.001<sup>**</sup></td></tr>
## <tr><td style="text-align:left"></td><td>(0.0001)</td><td>(0.0004)</td></tr>
## <tr><td style="text-align:left"></td><td></td><td></td></tr>
## <tr><td style="text-align:left">Nombre de Wolof dans le ménage</td><td>0.002<sup>***</sup></td><td>0.0004</td></tr>
## <tr><td style="text-align:left"></td><td>(0.001)</td><td>(0.0004)</td></tr>
## <tr><td style="text-align:left"></td><td></td><td></td></tr>
## <tr><td style="text-align:left">Nombre de Mandingo dans le ménage</td><td>-0.018</td><td>-0.005</td></tr>
## <tr><td style="text-align:left"></td><td>(0.012)</td><td>(0.011)</td></tr>
## <tr><td style="text-align:left"></td><td></td><td></td></tr>
## <tr><td style="text-align:left">Nombre de Soninke dans le ménage</td><td>0.005<sup>***</sup></td><td>0.009<sup>***</sup></td></tr>
## <tr><td style="text-align:left"></td><td>(0.001)</td><td>(0.001)</td></tr>
## <tr><td style="text-align:left"></td><td></td><td></td></tr>
## <tr><td style="text-align:left">Nombre de ménage dans le DR</td><td>0.095</td><td>-0.241</td></tr>
## <tr><td style="text-align:left"></td><td>(0.279)</td><td>(0.161)</td></tr>
## <tr><td style="text-align:left"></td><td></td><td></td></tr>
## <tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>268</td><td>435</td></tr>
## <tr><td style="text-align:left">R<sup>2</sup></td><td>0.636</td><td>0.589</td></tr>
## <tr><td style="text-align:left">Adjusted R<sup>2</sup></td><td>0.620</td><td>0.579</td></tr>
## <tr><td style="text-align:left">Residual Std. Error</td><td>0.260 (df = 256)</td><td>0.276 (df = 423)</td></tr>
## <tr><td style="text-align:left">F Statistic</td><td>40.686<sup>***</sup> (df = 11; 256)</td><td>55.161<sup>***</sup> (df = 11; 423)</td></tr>
## <tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td colspan="2" style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
## </table>
```


```r
#library(corrr)

### 2002
dfw_estimates_2002 <- MPI_data_dr_2002_for_model%>%
  select(-MPI_men) %>%
  st_drop_geometry()

correlations <- correlate(dfw_estimates_2002, method = "pearson")
network_plot(correlations) + labs(title = "Network plot of correlations between model predictors for Guediawaye Census 2002")
```

![](05_Modelling-Senegal-Census-data_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

```r
ggsave(paste0(here::here(),"/output/output_model/img/6_corr_var.png"), width = 8, height = 5)

### 2013
dfw_estimates_2013 <- MPI_data_dr_2013_for_model%>%
  select(-MPI_men) %>%
  st_drop_geometry()

correlations <- correlate(dfw_estimates_2013, method = "pearson")
network_plot(correlations) + labs(title = "Network plot of correlations between model predictors for Guediawaye Census 2013")
```

![](05_Modelling-Senegal-Census-data_files/figure-html/unnamed-chunk-12-2.png)<!-- -->

```r
ggsave(paste0(here::here(),"/output/output_model/img/6_corr_var.png"), width = 8, height = 5)
```


```r
library(car)

vif(model_2013)
```

```
##     fertlty     Primair       Moyen     Secondr     Ltrcy_F     Ltrcy_A 
##    5.367791    6.364877    8.106134    6.016109    7.975205    1.753154 
##     Ltrcy_W     Ltrcy_M      menage    pct_cm_f pop_density 
##    1.482194    1.495710    3.871359    1.078805    1.310951
```

```r
vif(model_2002)
```

```
##     fertlty     Primair       Moyen     Secondr     Ltrcy_F     Ltrcy_A 
##   12.816729  296.905235   62.394454   23.511511  676.243716    4.316810 
##     Ltrcy_W     Ltrcy_M      menage    pct_cm_f pop_density 
##    2.813059    2.154716    3.245263    1.192264    5.093219
```

## Dimension reduction with principal components analysis



```r
pca_2013 <- prcomp(
  formula = ~., 
  data = dfw_estimates_2013, 
  scale. = TRUE, 
  center = TRUE
)

summary(pca_2013)
```

```
## Importance of components:
##                           PC1    PC2    PC3    PC4     PC5     PC6     PC7
## Standard deviation     2.1830 1.2702 1.1990 1.0093 0.86570 0.70864 0.63550
## Proportion of Variance 0.4332 0.1467 0.1307 0.0926 0.06813 0.04565 0.03671
## Cumulative Proportion  0.4332 0.5799 0.7106 0.8032 0.87133 0.91698 0.95369
##                            PC8     PC9    PC10    PC11
## Standard deviation     0.46543 0.35450 0.28978 0.28829
## Proportion of Variance 0.01969 0.01142 0.00763 0.00756
## Cumulative Proportion  0.97339 0.98481 0.99244 1.00000
```

```r
##
pca_2002 <- prcomp(
  formula = ~., 
  data = dfw_estimates_2002, 
  scale. = TRUE, 
  center = TRUE
)

summary(pca_2002)
```

```
## Importance of components:
##                          PC1    PC2    PC3     PC4    PC5     PC6     PC7
## Standard deviation     2.484 1.2669 1.1310 0.89828 0.6902 0.51195 0.42413
## Proportion of Variance 0.561 0.1459 0.1163 0.07335 0.0433 0.02383 0.01635
## Cumulative Proportion  0.561 0.7069 0.8232 0.89657 0.9399 0.96370 0.98005
##                            PC8     PC9    PC10    PC11
## Standard deviation     0.34446 0.24919 0.19411 0.03123
## Proportion of Variance 0.01079 0.00565 0.00343 0.00009
## Cumulative Proportion  0.99084 0.99649 0.99991 1.00000
```


```r
pca_2013_tibble <- pca_2013$rotation %>%
  as_tibble(rownames = "predictor")
pca_2013_tibble
```

```
## # A tibble: 11 × 12
##    predictor       PC1      PC2     PC3      PC4     PC5    PC6      PC7     PC8
##    <chr>         <dbl>    <dbl>   <dbl>    <dbl>   <dbl>  <dbl>    <dbl>   <dbl>
##  1 menage      0.411    0.0273   0.0207 -0.00165 -0.116   0.171 -0.100    0.860 
##  2 Primair     0.391    0.00136  0.292  -0.134   -0.0931  0.278 -0.182   -0.321 
##  3 Moyen       0.423    0.0591  -0.169  -0.0299  -0.123  -0.105  0.0880  -0.292 
##  4 Secondr     0.359    0.0776  -0.404   0.0669  -0.0637 -0.350  0.241    0.0562
##  5 Ltrcy_F     0.419   -0.0249  -0.140   0.0607   0.118  -0.310  0.179   -0.148 
##  6 pct_cm_f   -0.0501  -0.160    0.0499 -0.944    0.0273 -0.226  0.135    0.0901
##  7 Ltrcy_A     0.187   -0.242    0.385   0.127    0.804  -0.193 -0.00979  0.0681
##  8 Ltrcy_W     0.00824 -0.670   -0.121   0.0919  -0.249  -0.329 -0.598   -0.0156
##  9 Ltrcy_M     0.00631 -0.670   -0.169   0.0737  -0.0179  0.453  0.556   -0.0105
## 10 fertlty     0.397   -0.0175   0.210  -0.142   -0.0923  0.369 -0.195   -0.177 
## 11 pop_densi…  0.00230 -0.0771   0.682   0.178   -0.477  -0.355  0.370    0.0462
## # ℹ 3 more variables: PC9 <dbl>, PC10 <dbl>, PC11 <dbl>
```

```r
###
pca_2002_tibble <- pca_2002$rotation %>%
  as_tibble(rownames = "predictor")
pca_2002_tibble
```

```
## # A tibble: 11 × 12
##    predictor       PC1     PC2     PC3      PC4     PC5     PC6     PC7      PC8
##    <chr>         <dbl>   <dbl>   <dbl>    <dbl>   <dbl>   <dbl>   <dbl>    <dbl>
##  1 menage       0.317  -0.151   0.0760  1.53e-1  0.743  -0.386   0.0986 -0.334  
##  2 Primair      0.386  -0.0977 -0.0903  1.77e-2  0.0916  0.125   0.0829  0.551  
##  3 Moyen        0.366   0.0740  0.250   2.23e-1 -0.182   0.0868 -0.0602  0.0971 
##  4 Secondr      0.259   0.199   0.519   3.79e-1 -0.230   0.162  -0.0422 -0.401  
##  5 Ltrcy_F      0.394  -0.0113  0.0838  1.19e-1 -0.0357  0.120   0.0212  0.326  
##  6 pct_cm_f    -0.0469 -0.168  -0.612   7.50e-1 -0.0961  0.0674 -0.0988 -0.0935 
##  7 Ltrcy_A      0.342  -0.0715 -0.174  -2.34e-1 -0.311  -0.481  -0.664  -0.115  
##  8 Ltrcy_W      0.190   0.586  -0.273  -7.74e-6 -0.233  -0.446   0.540  -0.00999
##  9 Ltrcy_M      0.0267  0.707  -0.206  -4.56e-2  0.388   0.348  -0.425  -0.0105 
## 10 fertlty      0.373  -0.141  -0.163  -1.40e-1  0.140   0.135   0.0543  0.0967 
## 11 pop_density  0.316  -0.157  -0.315  -3.60e-1 -0.148   0.462   0.229  -0.526  
## # ℹ 3 more variables: PC9 <dbl>, PC10 <dbl>, PC11 <dbl>
```




```r
# For 2002
pca_2002_graph <- pca_2002_tibble %>%
  select(predictor:PC5) %>%
  pivot_longer(PC1:PC5, names_to = "component", values_to = "value") %>%
  ggplot(aes(x = value, y = predictor)) + 
  geom_col(fill = "darkgreen", color = "darkgreen", alpha = 0.5) + 
  facet_wrap(~component, nrow = 1) + 
  labs(y = NULL, x = "Value", title = "Census 2002") + 
  theme_minimal()
```



```r
# For 2013
pca_2013_graph <- pca_2013_tibble %>%
                  select(predictor:PC5) %>%
                  pivot_longer(PC1:PC5, names_to = "component", values_to = "value") %>%
                  ggplot(aes(x = value, y = predictor)) + 
                  geom_col(fill = "darkgreen", color = "darkgreen", alpha = 0.5) + 
                  facet_wrap(~component, nrow = 1) + 
                  labs(y = NULL, x = "Value", title = "Census 2013") + 
                  theme_minimal()
```




```r
gridExtra::grid.arrange(pca_2002_graph,pca_2013_graph, nrow = 1, top="Loadings for first five principal components  for Guediawaye ")
```

![](05_Modelling-Senegal-Census-data_files/figure-html/unnamed-chunk-18-1.png)<!-- -->

```r
ggsave(paste0(here::here(),"/output/output_model/img/7_hist_pca_rgph.png"), width = 8, height = 5)
```


```r
components_2013 <- predict(pca_2013, dfw_estimates_2013)

dfw_pca_2013 <- MPI_data_dr_2013_for_model%>%
  select(MPI_men) %>%
  cbind(components_2013) 

ggplot(dfw_pca_2013, aes(fill = PC1)) +
  geom_sf(color = NA) +
  labs(title = "Map of principal component 1 for Guediawaye Census 2013") +
  theme_void() +
  scale_fill_viridis_c(direction = -1)
```

![](05_Modelling-Senegal-Census-data_files/figure-html/unnamed-chunk-19-1.png)<!-- -->


```r
components_2002 <- predict(pca_2002, dfw_estimates_2002)

dfw_pca_2002 <- MPI_data_dr_2002_for_model%>%
  select(MPI_men) %>%
  cbind(components_2002) 

ggplot(dfw_pca_2002, aes(fill = PC1)) +
  geom_sf(color = NA) +
  labs(title = "Map of principal component 1 for Guediawaye Census 2002") +
  theme_void() +
  scale_fill_viridis_c(direction = -1)
```

![](05_Modelling-Senegal-Census-data_files/figure-html/unnamed-chunk-20-1.png)<!-- -->

```r
dfw_pca_2002$RGPH<-"Census 2002"
dfw_pca_2013$RGPH<-"Census 2013"
dfw_pca <- dfw_pca_2002 %>%
  plyr::rbind.fill(dfw_pca_2013) %>% 
  st_as_sf()


dfw_pca %>% 
    
      ggplot(aes(fill = PC1))  + 
      
      # Adding spatial features to the plot
      geom_sf(color = NA) +
       scale_fill_viridis_c(direction = -1)+
      facet_wrap(~RGPH) +
      
# Adjusting the plot theme
      theme_map(base_size = 8) +
      theme(panel.background = element_rect(),
                #legend.background = element_blank(),
                axis.ticks = element_blank(),
                axis.text = element_blank(),
                legend.position = "bottom", 
                text = element_text(size = 8), 
                #panel.grid = element_line(color = "white", size = 0.8),
                legend.box.background = element_rect(),
                legend.box.margin = margin(6, 6, 6, 6),
                plot.background = element_rect(fill = "white"))+
      ggspatial::annotation_scale(
        location = "br",
        bar_cols = c("grey60", "white"),
        text_family = "ArcherPro Book"
      ) +
      ggspatial::annotation_north_arrow(
        location = "tl", which_north = "true",
        #pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
        #which_north = "grid",
      height = unit(1, "cm"),
      width = unit(1, "cm"),
      )
```

![](05_Modelling-Senegal-Census-data_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

```r
ggsave(paste0(here::here(),"/output/output_model/img/8_pc1_rgph.png"), width = 8, height = 5)
```






```r
dfw_pca %>% 
    
      ggplot(aes(fill = PC2))  + 
      
      # Adding spatial features to the plot
      geom_sf(color = NA) +
       scale_fill_viridis_c(direction = -1)+
      facet_wrap(~RGPH) +
      
      # Adjusting the plot theme
      theme_map(base_size = 8) +
      theme(panel.background = element_rect(),
                #legend.background = element_blank(),
                axis.ticks = element_blank(),
                axis.text = element_blank(),
                legend.position = "bottom", 
                text = element_text(size = 8), 
                #panel.grid = element_line(color = "white", size = 0.8),
                legend.box.background = element_rect(),
                legend.box.margin = margin(6, 6, 6, 6),
                plot.background = element_rect(fill = "white"))+
      ggspatial::annotation_scale(
        location = "br",
        bar_cols = c("grey60", "white"),
        text_family = "ArcherPro Book"
      ) +
      ggspatial::annotation_north_arrow(
        location = "tl", which_north = "true",
        #pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
        #which_north = "grid",
      height = unit(1, "cm"),
      width = unit(1, "cm"),
      )
```

![](05_Modelling-Senegal-Census-data_files/figure-html/unnamed-chunk-22-1.png)<!-- -->

```r
ggsave(paste0(here::here(),"/output/output_model/img/8_pc2_rgph.png"), width = 8, height = 5)
```


```r
# For the model output
# Saving in the "dfw_pca"
sf::st_write(obj=dfw_pca, paste0(here::here(), "/output/output_model/shp/dfw_pca.shp"),  driver = "ESRI Shapefile", delete_layer = TRUE) 
```

```
## Deleting layer `dfw_pca' using driver `ESRI Shapefile'
## Writing layer `dfw_pca' to data source 
##   `C:/Users/ASUS/Desktop/projet_sn/Analysing-Senegal-Census-Data/output/output_model/shp/dfw_pca.shp' using driver `ESRI Shapefile'
## Writing 703 features with 13 fields and geometry type Polygon.
```



```r
### For 2002
pca_2002_formula <- paste0("log(MPI_men) ~ ", 
                      paste0('PC', 1:6, collapse = ' + '))

pca_2002_model <- lm(formula = pca_2002_formula, data = dfw_pca_2002)


### For 2013
pca_2013_formula <- paste0("log(MPI_men) ~ ", 
                      paste0('PC', 1:6, collapse = ' + '))

pca_2013_model <- lm(formula = pca_2013_formula, data = dfw_pca_2013)



stargazer::stargazer(pca_2002_model, pca_2013_model, type="text", out = (paste0(here::here(),"/output/output_model/2_reg_PCA.html")))
```

```
## 
## ===================================================================
##                                   Dependent variable:              
##                     -----------------------------------------------
##                                      log(MPI_men)                  
##                               (1)                     (2)          
## -------------------------------------------------------------------
## PC1                        -0.034***               -0.054***       
##                             (0.007)                 (0.007)        
##                                                                    
## PC2                        -0.078***               -0.029**        
##                             (0.013)                 (0.012)        
##                                                                    
## PC3                        -0.185***               0.190***        
##                             (0.014)                 (0.013)        
##                                                                    
## PC4                        -0.119***                 0.013         
##                             (0.018)                 (0.015)        
##                                                                    
## PC5                        0.209***                 -0.025         
##                             (0.023)                 (0.018)        
##                                                                    
## PC6                        -0.254***               0.161***        
##                             (0.032)                 (0.022)        
##                                                                    
## Constant                   -2.373***               -2.199***       
##                             (0.016)                 (0.015)        
##                                                                    
## -------------------------------------------------------------------
## Observations                  268                     435          
## R2                           0.617                   0.446         
## Adjusted R2                  0.608                   0.438         
## Residual Std. Error    0.264 (df = 261)        0.319 (df = 428)    
## F Statistic         70.003*** (df = 6; 261) 57.462*** (df = 6; 428)
## ===================================================================
## Note:                                   *p<0.1; **p<0.05; ***p<0.01
```

## Spatial regression




```r
MPI_data_dr_2002_for_model$residuals <- residuals(model_2002)

ggplot(MPI_data_dr_2002_for_model, aes(x = residuals)) + 
  geom_histogram(bins = 100, alpha = 0.5, color = "navy",
                 fill = "navy") + 
  labs(title = "Distribution of model residuals for Guediawaye Census 2002") +
  theme_minimal()
```

![](05_Modelling-Senegal-Census-data_files/figure-html/unnamed-chunk-25-1.png)<!-- -->


```r
MPI_data_dr_2013_for_model$residuals <- residuals(model_2013)

ggplot(MPI_data_dr_2013_for_model, aes(x = residuals)) + 
  geom_histogram(bins = 100, alpha = 0.5, color = "navy",
                 fill = "navy") + 
  labs(title = "Distribution of model residuals for Guediawaye Census 2013") +
  theme_minimal()
```

![](05_Modelling-Senegal-Census-data_files/figure-html/unnamed-chunk-26-1.png)<!-- -->



```r
MPI_data_dr_for_model <- MPI_data_dr_2002_for_model %>%
  dplyr::mutate(RGPH = "Census 2002") %>% 
  plyr::rbind.fill(MPI_data_dr_2013_for_model %>%
  dplyr::mutate(RGPH = "Census 2013"))

ggplot(MPI_data_dr_for_model, aes(x = residuals)) + 
      geom_histogram(bins = 100, alpha = 0.5, color = "navy",
                 fill = "navy") +
       
      facet_wrap(~RGPH) +
      
      # Adjusting the plot theme
      theme_map(base_size = 8) +
      theme(panel.background = element_rect(),
            legend.background = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            legend.position = "bottom", 
            text = element_text(size = 8), 
            panel.grid = element_line(color = "white", size = 0.8))
```

![](05_Modelling-Senegal-Census-data_files/figure-html/unnamed-chunk-27-1.png)<!-- -->

```r
ggsave(paste0(here::here(),"/output/output_model/3_reg_lineaire_residuals.png"), width = 8, height = 5)
```



```r
library(spdep)

wts_2002 <- MPI_data_dr_2002_for_model %>%
  poly2nb() %>%
  nb2listw()

moran.test(MPI_data_dr_2002_for_model$residuals, wts_2002)
```

```
## 
## 	Moran I test under randomisation
## 
## data:  MPI_data_dr_2002_for_model$residuals  
## weights: wts_2002    
## 
## Moran I statistic standard deviate = 2.1911, p-value = 0.01422
## alternative hypothesis: greater
## sample estimates:
## Moran I statistic       Expectation          Variance 
##       0.077202122      -0.003745318       0.001364795
```



```r
wts_2013 <- MPI_data_dr_2013_for_model %>%
  poly2nb() %>%
  nb2listw()

moran.test(MPI_data_dr_2013_for_model$residuals, wts_2013)
```

```
## 
## 	Moran I test under randomisation
## 
## data:  MPI_data_dr_2013_for_model$residuals  
## weights: wts_2013    
## 
## Moran I statistic standard deviate = 4.7117, p-value = 1.228e-06
## alternative hypothesis: greater
## sample estimates:
## Moran I statistic       Expectation          Variance 
##      0.1325997182     -0.0023041475      0.0008197714
```




```r
MPI_data_dr_2013_for_model$lagged_residuals <- lag.listw(wts_2013, MPI_data_dr_2013_for_model$residuals)

ggplot(MPI_data_dr_2013_for_model, aes(x = residuals, y = lagged_residuals)) + 
  theme_minimal() + 
  labs(title = "Moran scatterplot of residual spatial autocorrelation for Guediawaye Census 2013") +
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", color = "red")
```

![](05_Modelling-Senegal-Census-data_files/figure-html/unnamed-chunk-30-1.png)<!-- -->

```r
MPI_data_dr_2002_for_model$lagged_residuals <- lag.listw(wts_2002, MPI_data_dr_2002_for_model$residuals)

ggplot(MPI_data_dr_2002_for_model, aes(x = residuals, y = lagged_residuals)) + 
  theme_minimal() + 
  labs(title = "Moran scatterplot of residual spatial autocorrelation for Guediawaye Census 2002") +
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", color = "red")
```

![](05_Modelling-Senegal-Census-data_files/figure-html/unnamed-chunk-31-1.png)<!-- -->

```r
MPI_data_dr_for_model <- MPI_data_dr_2002_for_model %>%
  dplyr::mutate(RGPH = "Census 2002") %>% 
  plyr::rbind.fill(MPI_data_dr_2013_for_model %>%
  dplyr::mutate(RGPH = "Census 2013"))

ggplot(MPI_data_dr_for_model, aes(x = residuals, y = lagged_residuals)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", color = "red")+
      facet_wrap(~RGPH) +
      
      # Adjusting the plot theme
      theme_map(base_size = 8) +
      theme(panel.background = element_rect(),
            legend.background = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            legend.position = "bottom", 
            text = element_text(size = 8), 
            panel.grid = element_line(color = "white", size = 0.8))
```

![](05_Modelling-Senegal-Census-data_files/figure-html/unnamed-chunk-32-1.png)<!-- -->

```r
ggsave(paste0(here::here(),"/output/output_model/img/9_residuals_lagresiduals.png"), width = 8, height = 5)
```



# Geographically weighted regression

## Choosing a bandwidth for GWR


```r
library(GWmodel)
library(sf)

dfw_data_sp_2002 <- MPI_data_dr_2002_for_model%>%
  as_Spatial()

bw <- bw.gwr(
  formula = formula_2002, 
  data = dfw_data_sp_2002, 
  kernel = "bisquare",
  adaptive = TRUE
)
```

```
## Adaptive bandwidth: 173 CV score: 30.77821 
## Adaptive bandwidth: 115 CV score: 22.62838 
## Adaptive bandwidth: 78 CV score: 18.01934 
## Adaptive bandwidth: 56 CV score: 20.35081 
## Adaptive bandwidth: 92 CV score: 18.83631 
## Adaptive bandwidth: 69 CV score: 18.26553 
## Adaptive bandwidth: 83 CV score: 18.02488 
## Adaptive bandwidth: 74 CV score: 18.01254 
## Adaptive bandwidth: 72 CV score: 18.08195 
## Adaptive bandwidth: 75 CV score: 17.91883 
## Adaptive bandwidth: 76 CV score: 17.96413 
## Adaptive bandwidth: 74 CV score: 18.01254 
## Adaptive bandwidth: 75 CV score: 17.91883
```

```r
dfw_data_sp_2013 <- MPI_data_dr_2013_for_model%>%
  as_Spatial()

bw <- bw.gwr(
  formula = formula_2013, 
  data = dfw_data_sp_2013, 
  kernel = "bisquare",
  adaptive = TRUE
)
```

```
## Adaptive bandwidth: 276 CV score: 33.03686 
## Adaptive bandwidth: 179 CV score: 32.93194 
## Adaptive bandwidth: 117 CV score: 34.2892 
## Adaptive bandwidth: 215 CV score: 32.81678 
## Adaptive bandwidth: 240 CV score: 32.88297 
## Adaptive bandwidth: 202 CV score: 32.80238 
## Adaptive bandwidth: 191 CV score: 32.84514 
## Adaptive bandwidth: 205 CV score: 32.78626 
## Adaptive bandwidth: 211 CV score: 32.81625 
## Adaptive bandwidth: 205 CV score: 32.78626
```

```r
###
```


```r
### 2002
gw_model_2002 <- gwr.basic(
  formula = formula_2002, 
  data = dfw_data_sp_2002, 
  bw = bw,
  kernel = "bisquare",
  adaptive = TRUE
)

### 2013
gw_model_2013 <- gwr.basic(
  formula = formula_2013, 
  data = dfw_data_sp_2013, 
  bw = bw,
  kernel = "bisquare",
  adaptive = TRUE
)
```


```r
names(gw_model_2013)
```

```
## [1] "GW.arguments"  "GW.diagnostic" "lm"            "SDF"          
## [5] "timings"       "this.call"     "Ftests"
```

```r
##
names(gw_model_2002)
```

```
## [1] "GW.arguments"  "GW.diagnostic" "lm"            "SDF"          
## [5] "timings"       "this.call"     "Ftests"
```


```r
gw_model_2013_results <- gw_model_2013$SDF %>%
  st_as_sf() 

names(gw_model_2013_results)
```

```
##  [1] "Intercept"      "fertlty"        "Primair"        "Moyen"         
##  [5] "Secondr"        "Ltrcy_F"        "Ltrcy_A"        "Ltrcy_W"       
##  [9] "Ltrcy_M"        "menage"         "pct_cm_f"       "pop_density"   
## [13] "y"              "yhat"           "residual"       "CV_Score"      
## [17] "Stud_residual"  "Intercept_SE"   "fertlty_SE"     "Primair_SE"    
## [21] "Moyen_SE"       "Secondr_SE"     "Ltrcy_F_SE"     "Ltrcy_A_SE"    
## [25] "Ltrcy_W_SE"     "Ltrcy_M_SE"     "menage_SE"      "pct_cm_f_SE"   
## [29] "pop_density_SE" "Intercept_TV"   "fertlty_TV"     "Primair_TV"    
## [33] "Moyen_TV"       "Secondr_TV"     "Ltrcy_F_TV"     "Ltrcy_A_TV"    
## [37] "Ltrcy_W_TV"     "Ltrcy_M_TV"     "menage_TV"      "pct_cm_f_TV"   
## [41] "pop_density_TV" "Local_R2"       "geometry"
```

```r
###
gw_model_2002_results <- gw_model_2002$SDF %>%
  st_as_sf() 

names(gw_model_2002_results)
```

```
##  [1] "Intercept"      "fertlty"        "Primair"        "Moyen"         
##  [5] "Secondr"        "Ltrcy_F"        "Ltrcy_A"        "Ltrcy_W"       
##  [9] "Ltrcy_M"        "menage"         "pct_cm_f"       "pop_density"   
## [13] "y"              "yhat"           "residual"       "CV_Score"      
## [17] "Stud_residual"  "Intercept_SE"   "fertlty_SE"     "Primair_SE"    
## [21] "Moyen_SE"       "Secondr_SE"     "Ltrcy_F_SE"     "Ltrcy_A_SE"    
## [25] "Ltrcy_W_SE"     "Ltrcy_M_SE"     "menage_SE"      "pct_cm_f_SE"   
## [29] "pop_density_SE" "Intercept_TV"   "fertlty_TV"     "Primair_TV"    
## [33] "Moyen_TV"       "Secondr_TV"     "Ltrcy_F_TV"     "Ltrcy_A_TV"    
## [37] "Ltrcy_W_TV"     "Ltrcy_M_TV"     "menage_TV"      "pct_cm_f_TV"   
## [41] "pop_density_TV" "Local_R2"       "geometry"
```



```r
ggplot(gw_model_2002_results, aes(fill = Local_R2)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c(direction = -1) + 
  labs(title = "Local R-squared values from the GWR model for Guediawaye Census 2002",fill="R-squared") +
  theme_void()
```

![](05_Modelling-Senegal-Census-data_files/figure-html/unnamed-chunk-37-1.png)<!-- -->


```r
ggplot(gw_model_2013_results, aes(fill = Local_R2)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c(direction = -1) + 
  labs(title = "Local R-squared values from the GWR model for Guediawaye Census 2013",fill="R-squared") +
  theme_void()
```

![](05_Modelling-Senegal-Census-data_files/figure-html/unnamed-chunk-38-1.png)<!-- -->



```r
gw_model_results <- gw_model_2002_results %>%
  dplyr::mutate(RGPH = "Census 2002") %>% 
  plyr::rbind.fill(gw_model_2013_results %>%
  dplyr::mutate(RGPH = "Census 2013")) %>% 
  st_as_sf()


gw_model_results %>% 
    
      ggplot(aes(fill = Local_R2)) + 
      
      # Adding spatial features to the plot
      geom_sf() +
      
      scale_fill_viridis_c(direction = -1) +
       
      facet_wrap(~RGPH) +
      labs(fill="R-squared")+
# Adjusting the plot theme
      theme_map(base_size = 8) +
      theme(panel.background = element_rect(),
                #legend.background = element_blank(),
                axis.ticks = element_blank(),
                axis.text = element_blank(),
                legend.position = "bottom", 
                text = element_text(size = 8), 
                #panel.grid = element_line(color = "white", size = 0.8),
                legend.box.background = element_rect(),
                legend.box.margin = margin(6, 6, 6, 6),
                plot.background = element_rect(fill = "white"))+
      ggspatial::annotation_scale(
        location = "br",
        bar_cols = c("grey60", "white"),
        text_family = "ArcherPro Book"
      ) +
      ggspatial::annotation_north_arrow(
        location = "tl", which_north = "true",
        #pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
        #which_north = "grid",
      height = unit(1, "cm"),
      width = unit(1, "cm"),
      )
```

![](05_Modelling-Senegal-Census-data_files/figure-html/unnamed-chunk-39-1.png)<!-- -->

```r
ggsave(paste0(here::here(),"/output/output_model/img/10_rsquared_rgph.png"), width = 8, height = 5)
```



```r
# Saving the model output
# Saving in the "gw_model_results"
sf::st_write(obj=gw_model_results, paste0(here::here(), "/output/output_model/shp/gw_model_results.shp"),  driver = "ESRI Shapefile", delete_layer = TRUE) 
```

```
## Deleting layer `gw_model_results' using driver `ESRI Shapefile'
## Writing layer `gw_model_results' to data source 
##   `C:/Users/ASUS/Desktop/projet_sn/Analysing-Senegal-Census-Data/output/output_model/shp/gw_model_results.shp' using driver `ESRI Shapefile'
## Writing 703 features with 43 fields and geometry type Polygon.
```



```r
ggplot(gw_model_2002_results, aes(fill = menage)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c(direction = -1) + 
  labs(title = "Local parameter estimates for household members for Guediawaye Census 2002") +
  theme_void() + 
  labs(fill = "Local β for \nHH")
```

![](05_Modelling-Senegal-Census-data_files/figure-html/unnamed-chunk-41-1.png)<!-- -->


```r
ggplot(gw_model_2013_results, aes(fill = menage)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c(direction = -1) + 
  labs(title = "Local parameter estimates for household members for Guediawaye Census 2013") +
  theme_void() + 
  labs(fill = "Local β for \nHH")
```

![](05_Modelling-Senegal-Census-data_files/figure-html/unnamed-chunk-42-1.png)<!-- -->



```r
gw_model_results <- gw_model_2002_results %>%
  dplyr::mutate(RGPH = "Census 2002") %>% 
  plyr::rbind.fill(gw_model_2013_results %>%
  dplyr::mutate(RGPH = "Census 2013")) %>% 
  st_as_sf()


gw_model_results %>% 
    
      ggplot(aes(fill = menage)) + 
      
      # Adding spatial features to the plot
      geom_sf() +
      
      scale_fill_viridis_c(direction = -1) +
  labs(fill = "Local β for \nHH")+
      facet_wrap(~RGPH) +
      
# Adjusting the plot theme
      theme_map(base_size = 8) +
      theme(panel.background = element_rect(),
                #legend.background = element_blank(),
                axis.ticks = element_blank(),
                axis.text = element_blank(),
                legend.position = "bottom", 
                text = element_text(size = 8), 
                #panel.grid = element_line(color = "white", size = 0.8),
                legend.box.background = element_rect(),
                legend.box.margin = margin(6, 6, 6, 6),
                plot.background = element_rect(fill = "white"))+
      ggspatial::annotation_scale(
        location = "br",
        bar_cols = c("grey60", "white"),
        text_family = "ArcherPro Book"
      ) +
      ggspatial::annotation_north_arrow(
        location = "tl", which_north = "true",
        #pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
        #which_north = "grid",
      height = unit(1, "cm"),
      width = unit(1, "cm"),
      )
```

![](05_Modelling-Senegal-Census-data_files/figure-html/unnamed-chunk-43-1.png)<!-- -->

```r
ggsave(paste0(here::here(),"/output/output_model/img/11_local_beta_rgph.png"), width = 8, height = 5)
```


```r
ggplot(gw_model_2002_results, aes(fill = fertlty)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c(direction = -1) + 
  labs(title = "Local parameter estimates for population fertility for Guediawaye Census 2002") +
  theme_void() + 
  labs(fill = "Local β for \n household's fertility")
```

![](05_Modelling-Senegal-Census-data_files/figure-html/unnamed-chunk-44-1.png)<!-- -->


```r
ggplot(gw_model_2013_results, aes(fill = fertlty)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c(direction = -1) + 
  labs(title = "Local parameter estimates for population fertility for Guediawaye Census 2013") +
  theme_void() + 
  labs(fill = "Local β for \n household's fertility")
```

![](05_Modelling-Senegal-Census-data_files/figure-html/unnamed-chunk-45-1.png)<!-- -->



```r
gw_model_results <- gw_model_2002_results %>%
  dplyr::mutate(RGPH = "Census 2002") %>% 
  plyr::rbind.fill(gw_model_2013_results %>%
  dplyr::mutate(RGPH = "Census 2013")) %>% 
  st_as_sf()


purrr::map(predictors_2002, function(curr_var){ 
    
    gw_model_results %>% 
    
      ggplot(aes(fill = !!sym(curr_var))) + 
      
      # Adding spatial features to the plot
      geom_sf() +
      
      scale_fill_viridis_c(direction = -1) +
      #scale_fill_continuous(labels = function(x) format(x, scientific = TRUE))+
  labs(fill = paste0("Local β for \n household's ",curr_var))+
      facet_wrap(~RGPH) +
      
# Adjusting the plot theme
      theme_map(base_size = 8) +
      theme(panel.background = element_rect(),
                #legend.background = element_blank(),
                axis.ticks = element_blank(),
                axis.text = element_blank(),
                legend.position = "bottom", 
                text = element_text(size = 8), 
                #panel.grid = element_line(color = "white", size = 0.8),
                legend.box.background = element_rect(),
                legend.box.margin = margin(6, 6, 6, 6),
                plot.background = element_rect(fill = "white"))+
      ggspatial::annotation_scale(
        location = "br",
        bar_cols = c("grey60", "white"),
        text_family = "ArcherPro Book"
      ) +
      ggspatial::annotation_north_arrow(
        location = "tl", which_north = "true",
        #pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
        #which_north = "grid",
      height = unit(1, "cm"),
      width = unit(1, "cm"),
      )

ggsave(paste0(here::here(),"/output/output_model/img/12_beta_hh_",curr_var,"_rgph.png"), width = 8, height = 5)}, .progress = TRUE)
```

```
## [[1]]
## [1] "C:/Users/ASUS/Desktop/projet_sn/Analysing-Senegal-Census-Data/output/output_model/img/12_beta_hh_fertlty_rgph.png"
## 
## [[2]]
## [1] "C:/Users/ASUS/Desktop/projet_sn/Analysing-Senegal-Census-Data/output/output_model/img/12_beta_hh_Primair_rgph.png"
## 
## [[3]]
## [1] "C:/Users/ASUS/Desktop/projet_sn/Analysing-Senegal-Census-Data/output/output_model/img/12_beta_hh_Moyen_rgph.png"
## 
## [[4]]
## [1] "C:/Users/ASUS/Desktop/projet_sn/Analysing-Senegal-Census-Data/output/output_model/img/12_beta_hh_Secondr_rgph.png"
## 
## [[5]]
## [1] "C:/Users/ASUS/Desktop/projet_sn/Analysing-Senegal-Census-Data/output/output_model/img/12_beta_hh_Ltrcy_F_rgph.png"
## 
## [[6]]
## [1] "C:/Users/ASUS/Desktop/projet_sn/Analysing-Senegal-Census-Data/output/output_model/img/12_beta_hh_Ltrcy_A_rgph.png"
## 
## [[7]]
## [1] "C:/Users/ASUS/Desktop/projet_sn/Analysing-Senegal-Census-Data/output/output_model/img/12_beta_hh_Ltrcy_W_rgph.png"
## 
## [[8]]
## [1] "C:/Users/ASUS/Desktop/projet_sn/Analysing-Senegal-Census-Data/output/output_model/img/12_beta_hh_Ltrcy_M_rgph.png"
## 
## [[9]]
## [1] "C:/Users/ASUS/Desktop/projet_sn/Analysing-Senegal-Census-Data/output/output_model/img/12_beta_hh_menage_rgph.png"
## 
## [[10]]
## [1] "C:/Users/ASUS/Desktop/projet_sn/Analysing-Senegal-Census-Data/output/output_model/img/12_beta_hh_pct_cm_f_rgph.png"
```



# Classification and clustering of Guediawaye census data

## Geodemographic classification


```r
set.seed(1994)

dfw_kmeans_2013 <- dfw_pca_2013 %>%
  st_drop_geometry() %>%
  select(PC1:PC8) %>%
  kmeans(centers = 6)

table(dfw_kmeans_2013$cluster)
```

```
## 
##   1   2   3   4   5   6 
##  53  73 139  71  95   4
```

```r
##
dfw_kmeans_2002 <- dfw_pca_2002 %>%
  st_drop_geometry() %>%
  select(PC1:PC8) %>%
  kmeans(centers = 6)

table(dfw_kmeans_2002$cluster)
```

```
## 
##  1  2  3  4  5  6 
## 71 38  3  3 85 68
```




```r
dfw_clusters_2002 <- dfw_pca_2002 %>%
  mutate(cluster = as.character(dfw_kmeans_2002$cluster))

ggplot(dfw_clusters_2002, aes(fill = cluster)) + 
  geom_sf(size = 0.1) + 
  #scale_fill_brewer(palette = "Set1") + 
  scale_fill_brewer(palette = "Set3")+
  labs(title = "Map of geodemographic clusters for Guediawaye Census 2002") +
  theme_void() + 
  labs(fill = "Cluster ")
```

![](05_Modelling-Senegal-Census-data_files/figure-html/unnamed-chunk-48-1.png)<!-- -->



```r
dfw_clusters_2013 <- dfw_pca_2013 %>%
  mutate(cluster = as.character(dfw_kmeans_2013$cluster))

ggplot(dfw_clusters_2013, aes(fill = cluster)) + 
  geom_sf(size = 0.1) + 
  scale_fill_brewer(palette = "Set3")+
  labs(title = "Map of geodemographic clusters for Guediawaye Census 2013") +
  theme_void() + 
  labs(fill = "Cluster ")
```

![](05_Modelling-Senegal-Census-data_files/figure-html/unnamed-chunk-49-1.png)<!-- -->


```r
dfw_clusters <- dfw_clusters_2002 %>%
  dplyr::mutate(RGPH = "Census 2002") %>% 
  plyr::rbind.fill(dfw_clusters_2013 %>%
  dplyr::mutate(RGPH = "Census 2013")) %>% 
  st_as_sf()


dfw_clusters %>% 
    
      ggplot(aes(fill = cluster)) + 
      
  geom_sf(size = 0.1) + 
  scale_fill_brewer(palette = "Set3")+ 
      facet_wrap(~RGPH) +
      
# Adjusting the plot theme
      theme_map(base_size = 8) +
      theme(panel.background = element_rect(),
                #legend.background = element_blank(),
                axis.ticks = element_blank(),
                axis.text = element_blank(),
                legend.position = "bottom", 
                text = element_text(size = 8), 
                #panel.grid = element_line(color = "white", size = 0.8),
                legend.box.background = element_rect(),
                legend.box.margin = margin(6, 6, 6, 6),
                plot.background = element_rect(fill = "white"))+
      ggspatial::annotation_scale(
        location = "br",
        bar_cols = c("grey60", "white"),
        text_family = "ArcherPro Book"
      ) +
      ggspatial::annotation_north_arrow(
        location = "tl", which_north = "true",
        #pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
        #which_north = "grid",
      height = unit(1, "cm"),
      width = unit(1, "cm"),
      )
```

![](05_Modelling-Senegal-Census-data_files/figure-html/unnamed-chunk-50-1.png)<!-- -->

```r
ggsave(paste0(here::here(),"/output/output_model/img/13_cluster_rgph.png"), width = 8, height = 5)
```


```r
# Saving the model output
# Saving in the "dfw_clusters"
sf::st_write(obj=dfw_clusters, paste0(here::here(), "/output/output_model/shp/dfw_clusters_pca.shp"),  driver = "ESRI Shapefile", delete_layer = TRUE) 
```

```
## Writing layer `dfw_clusters_pca' to data source 
##   `C:/Users/ASUS/Desktop/projet_sn/Analysing-Senegal-Census-Data/output/output_model/shp/dfw_clusters_pca.shp' using driver `ESRI Shapefile'
## Writing 703 features with 14 fields and geometry type Polygon.
```




```r
library(plotly)

cluster_plot <- ggplot(dfw_clusters_2013, 
                       aes(x = PC1, y = PC2, color = cluster)) + 
  geom_point() + 
  scale_color_brewer(palette = "Set1") + 
  labs(title = "Interactive scatterplot of PC1 and PC2 colored by cluster for Guediawaye Census 2013") +
  theme_minimal()

ggplotly(cluster_plot) %>%
  layout(legend = list(orientation = "h", y = -0.15, 
                       x = 0.2, title = "Cluster"))
```

```{=html}
<div class="plotly html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-c9fb4393a882a12d9b84" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-c9fb4393a882a12d9b84">{"x":{"data":[{"x":[3.2450046121419733,2.5715118251503237,2.5266280720644896,5.7410435255660399,5.3069869715382767,2.2374826927994249,5.4541592074985328,2.6685355738251317,5.6040629879314681,4.7145302409722856,3.8858269323661276,4.4980593609904771,3.5127505562004413,3.9230272598493836,3.2360590246239185,3.5632033302652526,3.8622952357284062,3.8036316438819426,3.4757211582218783,2.4837541634695648,2.459616480197695,3.187097730200243,3.2724968822728111,5.268978299567225,2.8449305001390641,2.4993040130125781,4.5696190332447104,5.258983245555962,4.064359636958943,3.2598555770100379,4.124386079777123,2.6149431264429213,3.2366279615937441,3.8608096780771075,4.0181794546400553,5.0852993928803221,4.54389546261954,2.6963567324418429,5.3248467207168764,9.3322187640450522,7.3943762060924172,2.837974722093306,5.728564554481947,4.6633568436041299,3.885136448889928,6.2891263663397039,3.8990619889544811,3.3687120739404586,3.9397920352643547,2.5711782163360128,4.1225023989337695,3.1158902157706887,4.1038557254056371],"y":[0.64646426594049688,0.83764367483862412,0.61640202975422409,0.89094172988840292,-0.026098550931765128,0.93294341727733376,0.013596106305031559,0.77221639738727621,0.3759622623393602,0.73061787278919388,-0.96419536058099709,0.064840644350017657,0.28405944405958056,0.81178549360830499,0.75362409937947239,0.2861850935202398,0.06198198252215921,0.42647660649211666,-0.57140984740718326,0.47435721929439545,0.35339780673997268,0.68775309651117855,1.2454413863318026,0.89005671162975719,0.55283334381966642,1.0170634576213664,-0.016219377979509727,0.61640796881399484,0.20432047358723471,0.40168133633037828,0.10121887816145696,0.50674787708971436,0.84672245565627713,0.57642531168200062,-0.27999604380622567,0.076705003528376753,-0.74323742222716971,0.2704762543862822,0.12596496793056219,-2.1120133431348673,-0.15888436099287181,0.17265435745861435,1.0096212363171646,-0.72858895669607493,0.45519090797311518,0.52833460845648528,-1.0357574220430585,0.26716521447193092,0.47661425436777483,0.60991104920353434,1.0423482214000312,0.57255013267094146,-0.38256674557999382],"text":["PC1:  3.245004612<br />PC2:   0.646464266<br />cluster: 1","PC1:  2.571511825<br />PC2:   0.837643675<br />cluster: 1","PC1:  2.526628072<br />PC2:   0.616402030<br />cluster: 1","PC1:  5.741043526<br />PC2:   0.890941730<br />cluster: 1","PC1:  5.306986972<br />PC2:  -0.026098551<br />cluster: 1","PC1:  2.237482693<br />PC2:   0.932943417<br />cluster: 1","PC1:  5.454159207<br />PC2:   0.013596106<br />cluster: 1","PC1:  2.668535574<br />PC2:   0.772216397<br />cluster: 1","PC1:  5.604062988<br />PC2:   0.375962262<br />cluster: 1","PC1:  4.714530241<br />PC2:   0.730617873<br />cluster: 1","PC1:  3.885826932<br />PC2:  -0.964195361<br />cluster: 1","PC1:  4.498059361<br />PC2:   0.064840644<br />cluster: 1","PC1:  3.512750556<br />PC2:   0.284059444<br />cluster: 1","PC1:  3.923027260<br />PC2:   0.811785494<br />cluster: 1","PC1:  3.236059025<br />PC2:   0.753624099<br />cluster: 1","PC1:  3.563203330<br />PC2:   0.286185094<br />cluster: 1","PC1:  3.862295236<br />PC2:   0.061981983<br />cluster: 1","PC1:  3.803631644<br />PC2:   0.426476606<br />cluster: 1","PC1:  3.475721158<br />PC2:  -0.571409847<br />cluster: 1","PC1:  2.483754163<br />PC2:   0.474357219<br />cluster: 1","PC1:  2.459616480<br />PC2:   0.353397807<br />cluster: 1","PC1:  3.187097730<br />PC2:   0.687753097<br />cluster: 1","PC1:  3.272496882<br />PC2:   1.245441386<br />cluster: 1","PC1:  5.268978300<br />PC2:   0.890056712<br />cluster: 1","PC1:  2.844930500<br />PC2:   0.552833344<br />cluster: 1","PC1:  2.499304013<br />PC2:   1.017063458<br />cluster: 1","PC1:  4.569619033<br />PC2:  -0.016219378<br />cluster: 1","PC1:  5.258983246<br />PC2:   0.616407969<br />cluster: 1","PC1:  4.064359637<br />PC2:   0.204320474<br />cluster: 1","PC1:  3.259855577<br />PC2:   0.401681336<br />cluster: 1","PC1:  4.124386080<br />PC2:   0.101218878<br />cluster: 1","PC1:  2.614943126<br />PC2:   0.506747877<br />cluster: 1","PC1:  3.236627962<br />PC2:   0.846722456<br />cluster: 1","PC1:  3.860809678<br />PC2:   0.576425312<br />cluster: 1","PC1:  4.018179455<br />PC2:  -0.279996044<br />cluster: 1","PC1:  5.085299393<br />PC2:   0.076705004<br />cluster: 1","PC1:  4.543895463<br />PC2:  -0.743237422<br />cluster: 1","PC1:  2.696356732<br />PC2:   0.270476254<br />cluster: 1","PC1:  5.324846721<br />PC2:   0.125964968<br />cluster: 1","PC1:  9.332218764<br />PC2:  -2.112013343<br />cluster: 1","PC1:  7.394376206<br />PC2:  -0.158884361<br />cluster: 1","PC1:  2.837974722<br />PC2:   0.172654357<br />cluster: 1","PC1:  5.728564554<br />PC2:   1.009621236<br />cluster: 1","PC1:  4.663356844<br />PC2:  -0.728588957<br />cluster: 1","PC1:  3.885136449<br />PC2:   0.455190908<br />cluster: 1","PC1:  6.289126366<br />PC2:   0.528334608<br />cluster: 1","PC1:  3.899061989<br />PC2:  -1.035757422<br />cluster: 1","PC1:  3.368712074<br />PC2:   0.267165214<br />cluster: 1","PC1:  3.939792035<br />PC2:   0.476614254<br />cluster: 1","PC1:  2.571178216<br />PC2:   0.609911049<br />cluster: 1","PC1:  4.122502399<br />PC2:   1.042348221<br />cluster: 1","PC1:  3.115890216<br />PC2:   0.572550133<br />cluster: 1","PC1:  4.103855725<br />PC2:  -0.382566746<br />cluster: 1"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(228,26,28,1)","opacity":1,"size":5.6692913385826778,"symbol":"circle","line":{"width":1.8897637795275593,"color":"rgba(228,26,28,1)"}},"hoveron":"points","name":"1","legendgroup":"1","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[-1.0634094533451819,-0.067730685948234753,-0.33040353834835556,1.1912939706019161,1.7919218268120793,-0.72483437591358912,1.1211707432378117,1.7388274574385858,0.68799767338864137,0.41623236967383453,1.010401123581429,-1.0689756589309651,0.43082409902933411,2.3490486653580129,0.24368468604477561,0.20694152902060317,1.3288409527647507,1.2698458790139453,2.3846968744845469,1.5090267397986148,-0.10726202048527152,0.68716774521239121,-0.16029746435431536,0.28267746684303185,-0.12695413009783799,-0.31449895880887213,0.082899523195397798,-0.88881655372565782,0.37541002998115136,-0.2939155544787862,0.093877959730083657,-0.27845347855335484,0.11846909022903629,1.7857953881214166,2.3821200684679495,1.8028228280715435,-0.22212261186844645,-0.81321957591584226,-0.050199046822192041,0.42085457848711727,-0.17814261550749808,-0.72951918390341641,-0.28527025907898079,1.4823845120916497,1.2424229070975052,-0.81126966220935992,0.043359135587070392,-0.89803540547258476,0.029790593909320411,1.9447863728493371,1.2245636449105275,2.111690511205607,1.0871011547669007,1.2090645308391772,1.243595080762572,-0.086521859304350718,-0.31365434978348783,0.33976656169956321,0.33761314737956977,0.69060517852535785,0.37037399954893391,1.1074476774968014,-1.3322925282535563,-0.64408150401926978,1.820772845236879,1.6173234247126766,-0.12985500530127292,1.63799749117902,1.7323140899829028,-0.51956778688505956,0.67311675255934733,0.46651310814950209,-0.064362094063177666],"y":[0.5870869891185645,0.76325762756865101,0.61298092190475784,0.66380231000292489,0.69796132153632329,-0.15201037635776515,0.35513754801622804,0.2706219613205223,0.6084396993289295,-0.46584413498605859,0.25482829513581529,0.38664315593034077,0.41855994299673766,-0.11329398301655813,0.56321455044904567,0.78074766507463866,0.87132872538544448,0.68846566912634632,0.95144178591487671,0.81240024657959098,0.56994311282602328,-1.3809749055344598,-0.18271955064257817,-0.24040538806927728,-0.1765108458884157,-0.64644382025538139,0.1091771993452948,-1.2655638282627628,0.78489510546148733,0.75781420096838337,0.23061713778582166,0.33955206639743396,0.49409059002652878,0.24893032306558949,-0.013792371206938547,0.75163016907063196,0.8211304324301667,0.29987782686464798,0.58833921746944196,0.21363880094135357,0.06853003459472165,0.77421452419146941,0.3748341237778014,0.55473753693372474,0.68981837489278441,-0.22996718453808376,0.35113395780482198,0.58087133791504908,-1.5679366610690704,0.17940728629967576,-1.1965483725533501,0.63671255688414796,-1.5458496763824492,1.0123159220920528,1.1568967143054316,0.79993694291688533,-0.059467812083697877,-0.20606944730619217,0.01638851365534115,0.624408755100608,-0.18759717092827594,0.60103420661210782,-0.71057857186718199,0.32491652268834759,0.46977320611004647,0.73793282215783884,0.62998954321183187,0.48833002386749247,0.30991407658637843,0.13822649433318815,0.15706550753297452,0.7347749178661005,0.37605943723391783],"text":["PC1: -1.063409453<br />PC2:   0.587086989<br />cluster: 2","PC1: -0.067730686<br />PC2:   0.763257628<br />cluster: 2","PC1: -0.330403538<br />PC2:   0.612980922<br />cluster: 2","PC1:  1.191293971<br />PC2:   0.663802310<br />cluster: 2","PC1:  1.791921827<br />PC2:   0.697961322<br />cluster: 2","PC1: -0.724834376<br />PC2:  -0.152010376<br />cluster: 2","PC1:  1.121170743<br />PC2:   0.355137548<br />cluster: 2","PC1:  1.738827457<br />PC2:   0.270621961<br />cluster: 2","PC1:  0.687997673<br />PC2:   0.608439699<br />cluster: 2","PC1:  0.416232370<br />PC2:  -0.465844135<br />cluster: 2","PC1:  1.010401124<br />PC2:   0.254828295<br />cluster: 2","PC1: -1.068975659<br />PC2:   0.386643156<br />cluster: 2","PC1:  0.430824099<br />PC2:   0.418559943<br />cluster: 2","PC1:  2.349048665<br />PC2:  -0.113293983<br />cluster: 2","PC1:  0.243684686<br />PC2:   0.563214550<br />cluster: 2","PC1:  0.206941529<br />PC2:   0.780747665<br />cluster: 2","PC1:  1.328840953<br />PC2:   0.871328725<br />cluster: 2","PC1:  1.269845879<br />PC2:   0.688465669<br />cluster: 2","PC1:  2.384696874<br />PC2:   0.951441786<br />cluster: 2","PC1:  1.509026740<br />PC2:   0.812400247<br />cluster: 2","PC1: -0.107262020<br />PC2:   0.569943113<br />cluster: 2","PC1:  0.687167745<br />PC2:  -1.380974906<br />cluster: 2","PC1: -0.160297464<br />PC2:  -0.182719551<br />cluster: 2","PC1:  0.282677467<br />PC2:  -0.240405388<br />cluster: 2","PC1: -0.126954130<br />PC2:  -0.176510846<br />cluster: 2","PC1: -0.314498959<br />PC2:  -0.646443820<br />cluster: 2","PC1:  0.082899523<br />PC2:   0.109177199<br />cluster: 2","PC1: -0.888816554<br />PC2:  -1.265563828<br />cluster: 2","PC1:  0.375410030<br />PC2:   0.784895105<br />cluster: 2","PC1: -0.293915554<br />PC2:   0.757814201<br />cluster: 2","PC1:  0.093877960<br />PC2:   0.230617138<br />cluster: 2","PC1: -0.278453479<br />PC2:   0.339552066<br />cluster: 2","PC1:  0.118469090<br />PC2:   0.494090590<br />cluster: 2","PC1:  1.785795388<br />PC2:   0.248930323<br />cluster: 2","PC1:  2.382120068<br />PC2:  -0.013792371<br />cluster: 2","PC1:  1.802822828<br />PC2:   0.751630169<br />cluster: 2","PC1: -0.222122612<br />PC2:   0.821130432<br />cluster: 2","PC1: -0.813219576<br />PC2:   0.299877827<br />cluster: 2","PC1: -0.050199047<br />PC2:   0.588339217<br />cluster: 2","PC1:  0.420854578<br />PC2:   0.213638801<br />cluster: 2","PC1: -0.178142616<br />PC2:   0.068530035<br />cluster: 2","PC1: -0.729519184<br />PC2:   0.774214524<br />cluster: 2","PC1: -0.285270259<br />PC2:   0.374834124<br />cluster: 2","PC1:  1.482384512<br />PC2:   0.554737537<br />cluster: 2","PC1:  1.242422907<br />PC2:   0.689818375<br />cluster: 2","PC1: -0.811269662<br />PC2:  -0.229967185<br />cluster: 2","PC1:  0.043359136<br />PC2:   0.351133958<br />cluster: 2","PC1: -0.898035405<br />PC2:   0.580871338<br />cluster: 2","PC1:  0.029790594<br />PC2:  -1.567936661<br />cluster: 2","PC1:  1.944786373<br />PC2:   0.179407286<br />cluster: 2","PC1:  1.224563645<br />PC2:  -1.196548373<br />cluster: 2","PC1:  2.111690511<br />PC2:   0.636712557<br />cluster: 2","PC1:  1.087101155<br />PC2:  -1.545849676<br />cluster: 2","PC1:  1.209064531<br />PC2:   1.012315922<br />cluster: 2","PC1:  1.243595081<br />PC2:   1.156896714<br />cluster: 2","PC1: -0.086521859<br />PC2:   0.799936943<br />cluster: 2","PC1: -0.313654350<br />PC2:  -0.059467812<br />cluster: 2","PC1:  0.339766562<br />PC2:  -0.206069447<br />cluster: 2","PC1:  0.337613147<br />PC2:   0.016388514<br />cluster: 2","PC1:  0.690605179<br />PC2:   0.624408755<br />cluster: 2","PC1:  0.370374000<br />PC2:  -0.187597171<br />cluster: 2","PC1:  1.107447677<br />PC2:   0.601034207<br />cluster: 2","PC1: -1.332292528<br />PC2:  -0.710578572<br />cluster: 2","PC1: -0.644081504<br />PC2:   0.324916523<br />cluster: 2","PC1:  1.820772845<br />PC2:   0.469773206<br />cluster: 2","PC1:  1.617323425<br />PC2:   0.737932822<br />cluster: 2","PC1: -0.129855005<br />PC2:   0.629989543<br />cluster: 2","PC1:  1.637997491<br />PC2:   0.488330024<br />cluster: 2","PC1:  1.732314090<br />PC2:   0.309914077<br />cluster: 2","PC1: -0.519567787<br />PC2:   0.138226494<br />cluster: 2","PC1:  0.673116753<br />PC2:   0.157065508<br />cluster: 2","PC1:  0.466513108<br />PC2:   0.734774918<br />cluster: 2","PC1: -0.064362094<br />PC2:   0.376059437<br />cluster: 2"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(55,126,184,1)","opacity":1,"size":5.6692913385826778,"symbol":"circle","line":{"width":1.8897637795275593,"color":"rgba(55,126,184,1)"}},"hoveron":"points","name":"2","legendgroup":"2","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[-1.4533539881664745,-0.89949228392855618,-0.27839098847566884,-0.62731105277127364,-0.17201136376270473,0.12515542660676446,-1.2341332011226374,0.18628603308539837,-0.82911168866113205,-1.2034123269104211,-0.91431544240466633,-0.085619389411940666,-1.6701873341142393,-0.5256825965210179,-1.0609547485848554,-1.1542880595835083,-0.27751693915149461,-0.11951317937299452,-0.69304947296281938,-1.4393697119743025,-0.57988270414463028,-1.1982431347518061,-1.7164102177158562,0.12178050121068862,-1.0392267326859501,-0.70343986749768239,-1.0690585680343219,-1.2838615070055113,-1.3273166059592876,-1.1465985711812672,-1.0304853185926479,-0.28558629177188444,-1.5006256064057832,-1.4495336293492671,-1.9856424057897861,-0.6776309003029215,-1.0050872221020657,-1.2404239447574728,-0.18685692360777459,-0.35035058510776501,-1.7870181575911528,-0.82216570959808111,-0.36150363660926038,-0.0933689177474476,-0.44783963684210898,0.41440844919160563,-0.55323863655562666,-1.0484246281034879,-0.41963505159496634,-1.3336103485125854,0.28621130417187896,-0.65404788604581132,-0.81958056476366992,-0.12008301497510045,-0.25234692692325328,-1.0908354626531627,-0.54896701417652305,-0.85708979447398648,-0.74809670222383151,-1.3141595492732225,-0.96968509867012598,-0.87152187528899727,-2.0608676582980485,-0.84928693497724028,-0.47811175414694584,-0.72109726780422068,-0.21091629370687795,-0.2626188629373214,-2.0993802835833932,-2.3190462926837361,-1.7953270042219216,-1.4521688491855946,-0.99790209392635509,-1.1473946682233256,-1.3892015889515021,-1.3623943152488935,-0.9552139546045324,0.052937080712736555,-1.051062900883843,-1.122886819518967,-1.3011712176599428,-1.7024304513967825,-1.1282435316140202,-1.7204630518226045,-1.0707292285544046,-0.49158712553988682,-0.56220209652586639,0.19847988791073179,-1.3190910004192624,-2.0310285218355775,-0.03241524558572758,-0.83348827724243568,-0.14698160186477369,-1.5816101722469831,-0.89610257014223538,-0.92840504678434554,-2.2203730325675353,-0.43101584772769852,-1.9459924918302927,-0.53943796751242312,-1.0790122330630407,-1.1395860344121769,-0.85753469499551149,-1.6869190824789284,-0.49345670485549437,-1.6804815197609817,-2.0850398200804503,-1.1709553273195974,-0.6403224919928695,-1.7589051353934693,-0.63314074272950571,-1.9229708971678636,-1.4607740885500364,0.3655535528890726,-1.4173013915294532,-0.94288205890417431,-1.2450988116610353,-0.14787144570166597,-1.2190962018631855,-0.59238984171450637,-0.74731531757468395,-1.5342239900702268,-0.037909923544915534,-0.29289177701625541,-1.5041317058638428,-0.90347875429660696,-1.4052445164497513,-1.3948045671294023,-1.4926760264557297,-1.7586673455100232,-0.95354455802423665,-1.886181363553022,-1.4290427059686726,-0.90859140311268316,-1.1201849091086942,-1.9407563976232811,-1.9482176287784234,-1.9751890274587882,-0.08002182594509484],"y":[-0.12697022943414882,0.77861243020587467,0.45306430818592169,0.42594255884443427,0.63856947865725522,0.65704394857961368,0.3439088433689379,0.62794378215151625,0.511873339233757,0.70979624259686458,0.31923530387357069,0.32361654051377287,0.3589596206212754,0.59199020083785825,-0.43933424455879572,0.10187268559441248,-0.6353037925158278,0.61798850857056353,-0.27928121328375693,0.3084051943753558,-0.13099167515836238,0.18568805795814605,0.80325356642014456,0.28500374904074166,0.33692511365823924,0.0751453851677642,0.13869811856337744,-0.39950540619946601,0.43750233171171943,0.18031550134837571,0.66293850797174969,0.41851527727744775,0.30744650151755554,-1.2352083812265908,0.32294303471611407,0.052447209193004024,-0.21472982542641036,0.3258944776725925,-0.51296133187868631,-0.96387937763754894,0.20343329889111975,0.33811029702891976,-0.5633697605332072,0.23243953239567341,-0.033263159448213009,0.64438183034881424,0.42347120374172903,0.26917841474618964,0.2712425768438198,0.28993666879418184,0.65798575474363274,-0.0018958995360279923,0.0030924176808083265,0.66738757638633373,0.20361403253051302,-1.2718145370894915,-0.083183327967586873,0.36842940721212364,0.35700588568989167,0.39826588363030996,0.34850011695146405,-0.52092054348859451,-0.33814022399257282,0.65477467724715388,0.37391249169608731,-1.0402274991308438,-0.10073640078978427,0.14936624214812616,0.30627306757339995,0.027626735607015826,-0.26901240356478412,0.02643944723372,0.47859054836301096,0.41742998385638952,0.72075168379904764,0.31511429880584191,-0.051152346951818313,0.28307162894986465,-0.49091783051617288,0.35421867375229632,-0.76875919258814995,0.28889591777818402,0.38502272269091609,-0.34767839949570101,0.15574903843372395,-0.14649173273480148,-0.093316228732861758,0.054544812123536672,0.13124688439986104,0.0134058713190012,0.20948790630869235,0.44679658131825828,0.58939118699345328,0.12059047496150872,0.32976014361035427,0.25114577100769936,0.12841863573981208,0.2481493189373738,0.1306294991983751,0.1450353579476758,-0.20796537075953492,0.49534070638201527,-1.3046965083829791,0.48127424836901417,0.17348597836315902,0.39891901834757237,0.099616452545098946,0.26504257022007227,0.41653715430244131,0.60717609806852746,-1.4606222517782808,0.41503518029591557,0.43875831399662157,0.83203907155484802,0.25930807955496227,0.90345105647433388,0.56004593076547859,0.36220507204396013,0.27656322977054054,0.17973972368537824,0.32777531717945163,0.014831103423641578,0.046708308036168725,-0.13215961636420503,0.20960218635055561,0.4643866326525305,0.36354388098011997,0.01300472644816858,0.61195995720402585,0.3153454032097352,-0.053930842771473267,0.13677104948960167,0.29548355108577573,-1.8331645847277387,0.29463802200222755,-0.23946223563034738,0.47121083252522689,0.092797634147931782,0.40143339410103623],"text":["PC1: -1.453353988<br />PC2:  -0.126970229<br />cluster: 3","PC1: -0.899492284<br />PC2:   0.778612430<br />cluster: 3","PC1: -0.278390988<br />PC2:   0.453064308<br />cluster: 3","PC1: -0.627311053<br />PC2:   0.425942559<br />cluster: 3","PC1: -0.172011364<br />PC2:   0.638569479<br />cluster: 3","PC1:  0.125155427<br />PC2:   0.657043949<br />cluster: 3","PC1: -1.234133201<br />PC2:   0.343908843<br />cluster: 3","PC1:  0.186286033<br />PC2:   0.627943782<br />cluster: 3","PC1: -0.829111689<br />PC2:   0.511873339<br />cluster: 3","PC1: -1.203412327<br />PC2:   0.709796243<br />cluster: 3","PC1: -0.914315442<br />PC2:   0.319235304<br />cluster: 3","PC1: -0.085619389<br />PC2:   0.323616541<br />cluster: 3","PC1: -1.670187334<br />PC2:   0.358959621<br />cluster: 3","PC1: -0.525682597<br />PC2:   0.591990201<br />cluster: 3","PC1: -1.060954749<br />PC2:  -0.439334245<br />cluster: 3","PC1: -1.154288060<br />PC2:   0.101872686<br />cluster: 3","PC1: -0.277516939<br />PC2:  -0.635303793<br />cluster: 3","PC1: -0.119513179<br />PC2:   0.617988509<br />cluster: 3","PC1: -0.693049473<br />PC2:  -0.279281213<br />cluster: 3","PC1: -1.439369712<br />PC2:   0.308405194<br />cluster: 3","PC1: -0.579882704<br />PC2:  -0.130991675<br />cluster: 3","PC1: -1.198243135<br />PC2:   0.185688058<br />cluster: 3","PC1: -1.716410218<br />PC2:   0.803253566<br />cluster: 3","PC1:  0.121780501<br />PC2:   0.285003749<br />cluster: 3","PC1: -1.039226733<br />PC2:   0.336925114<br />cluster: 3","PC1: -0.703439867<br />PC2:   0.075145385<br />cluster: 3","PC1: -1.069058568<br />PC2:   0.138698119<br />cluster: 3","PC1: -1.283861507<br />PC2:  -0.399505406<br />cluster: 3","PC1: -1.327316606<br />PC2:   0.437502332<br />cluster: 3","PC1: -1.146598571<br />PC2:   0.180315501<br />cluster: 3","PC1: -1.030485319<br />PC2:   0.662938508<br />cluster: 3","PC1: -0.285586292<br />PC2:   0.418515277<br />cluster: 3","PC1: -1.500625606<br />PC2:   0.307446502<br />cluster: 3","PC1: -1.449533629<br />PC2:  -1.235208381<br />cluster: 3","PC1: -1.985642406<br />PC2:   0.322943035<br />cluster: 3","PC1: -0.677630900<br />PC2:   0.052447209<br />cluster: 3","PC1: -1.005087222<br />PC2:  -0.214729825<br />cluster: 3","PC1: -1.240423945<br />PC2:   0.325894478<br />cluster: 3","PC1: -0.186856924<br />PC2:  -0.512961332<br />cluster: 3","PC1: -0.350350585<br />PC2:  -0.963879378<br />cluster: 3","PC1: -1.787018158<br />PC2:   0.203433299<br />cluster: 3","PC1: -0.822165710<br />PC2:   0.338110297<br />cluster: 3","PC1: -0.361503637<br />PC2:  -0.563369761<br />cluster: 3","PC1: -0.093368918<br />PC2:   0.232439532<br />cluster: 3","PC1: -0.447839637<br />PC2:  -0.033263159<br />cluster: 3","PC1:  0.414408449<br />PC2:   0.644381830<br />cluster: 3","PC1: -0.553238637<br />PC2:   0.423471204<br />cluster: 3","PC1: -1.048424628<br />PC2:   0.269178415<br />cluster: 3","PC1: -0.419635052<br />PC2:   0.271242577<br />cluster: 3","PC1: -1.333610349<br />PC2:   0.289936669<br />cluster: 3","PC1:  0.286211304<br />PC2:   0.657985755<br />cluster: 3","PC1: -0.654047886<br />PC2:  -0.001895900<br />cluster: 3","PC1: -0.819580565<br />PC2:   0.003092418<br />cluster: 3","PC1: -0.120083015<br />PC2:   0.667387576<br />cluster: 3","PC1: -0.252346927<br />PC2:   0.203614033<br />cluster: 3","PC1: -1.090835463<br />PC2:  -1.271814537<br />cluster: 3","PC1: -0.548967014<br />PC2:  -0.083183328<br />cluster: 3","PC1: -0.857089794<br />PC2:   0.368429407<br />cluster: 3","PC1: -0.748096702<br />PC2:   0.357005886<br />cluster: 3","PC1: -1.314159549<br />PC2:   0.398265884<br />cluster: 3","PC1: -0.969685099<br />PC2:   0.348500117<br />cluster: 3","PC1: -0.871521875<br />PC2:  -0.520920543<br />cluster: 3","PC1: -2.060867658<br />PC2:  -0.338140224<br />cluster: 3","PC1: -0.849286935<br />PC2:   0.654774677<br />cluster: 3","PC1: -0.478111754<br />PC2:   0.373912492<br />cluster: 3","PC1: -0.721097268<br />PC2:  -1.040227499<br />cluster: 3","PC1: -0.210916294<br />PC2:  -0.100736401<br />cluster: 3","PC1: -0.262618863<br />PC2:   0.149366242<br />cluster: 3","PC1: -2.099380284<br />PC2:   0.306273068<br />cluster: 3","PC1: -2.319046293<br />PC2:   0.027626736<br />cluster: 3","PC1: -1.795327004<br />PC2:  -0.269012404<br />cluster: 3","PC1: -1.452168849<br />PC2:   0.026439447<br />cluster: 3","PC1: -0.997902094<br />PC2:   0.478590548<br />cluster: 3","PC1: -1.147394668<br />PC2:   0.417429984<br />cluster: 3","PC1: -1.389201589<br />PC2:   0.720751684<br />cluster: 3","PC1: -1.362394315<br />PC2:   0.315114299<br />cluster: 3","PC1: -0.955213955<br />PC2:  -0.051152347<br />cluster: 3","PC1:  0.052937081<br />PC2:   0.283071629<br />cluster: 3","PC1: -1.051062901<br />PC2:  -0.490917831<br />cluster: 3","PC1: -1.122886820<br />PC2:   0.354218674<br />cluster: 3","PC1: -1.301171218<br />PC2:  -0.768759193<br />cluster: 3","PC1: -1.702430451<br />PC2:   0.288895918<br />cluster: 3","PC1: -1.128243532<br />PC2:   0.385022723<br />cluster: 3","PC1: -1.720463052<br />PC2:  -0.347678399<br />cluster: 3","PC1: -1.070729229<br />PC2:   0.155749038<br />cluster: 3","PC1: -0.491587126<br />PC2:  -0.146491733<br />cluster: 3","PC1: -0.562202097<br />PC2:  -0.093316229<br />cluster: 3","PC1:  0.198479888<br />PC2:   0.054544812<br />cluster: 3","PC1: -1.319091000<br />PC2:   0.131246884<br />cluster: 3","PC1: -2.031028522<br />PC2:   0.013405871<br />cluster: 3","PC1: -0.032415246<br />PC2:   0.209487906<br />cluster: 3","PC1: -0.833488277<br />PC2:   0.446796581<br />cluster: 3","PC1: -0.146981602<br />PC2:   0.589391187<br />cluster: 3","PC1: -1.581610172<br />PC2:   0.120590475<br />cluster: 3","PC1: -0.896102570<br />PC2:   0.329760144<br />cluster: 3","PC1: -0.928405047<br />PC2:   0.251145771<br />cluster: 3","PC1: -2.220373033<br />PC2:   0.128418636<br />cluster: 3","PC1: -0.431015848<br />PC2:   0.248149319<br />cluster: 3","PC1: -1.945992492<br />PC2:   0.130629499<br />cluster: 3","PC1: -0.539437968<br />PC2:   0.145035358<br />cluster: 3","PC1: -1.079012233<br />PC2:  -0.207965371<br />cluster: 3","PC1: -1.139586034<br />PC2:   0.495340706<br />cluster: 3","PC1: -0.857534695<br />PC2:  -1.304696508<br />cluster: 3","PC1: -1.686919082<br />PC2:   0.481274248<br />cluster: 3","PC1: -0.493456705<br />PC2:   0.173485978<br />cluster: 3","PC1: -1.680481520<br />PC2:   0.398919018<br />cluster: 3","PC1: -2.085039820<br />PC2:   0.099616453<br />cluster: 3","PC1: -1.170955327<br />PC2:   0.265042570<br />cluster: 3","PC1: -0.640322492<br />PC2:   0.416537154<br />cluster: 3","PC1: -1.758905135<br />PC2:   0.607176098<br />cluster: 3","PC1: -0.633140743<br />PC2:  -1.460622252<br />cluster: 3","PC1: -1.922970897<br />PC2:   0.415035180<br />cluster: 3","PC1: -1.460774089<br />PC2:   0.438758314<br />cluster: 3","PC1:  0.365553553<br />PC2:   0.832039072<br />cluster: 3","PC1: -1.417301392<br />PC2:   0.259308080<br />cluster: 3","PC1: -0.942882059<br />PC2:   0.903451056<br />cluster: 3","PC1: -1.245098812<br />PC2:   0.560045931<br />cluster: 3","PC1: -0.147871446<br />PC2:   0.362205072<br />cluster: 3","PC1: -1.219096202<br />PC2:   0.276563230<br />cluster: 3","PC1: -0.592389842<br />PC2:   0.179739724<br />cluster: 3","PC1: -0.747315318<br />PC2:   0.327775317<br />cluster: 3","PC1: -1.534223990<br />PC2:   0.014831103<br />cluster: 3","PC1: -0.037909924<br />PC2:   0.046708308<br />cluster: 3","PC1: -0.292891777<br />PC2:  -0.132159616<br />cluster: 3","PC1: -1.504131706<br />PC2:   0.209602186<br />cluster: 3","PC1: -0.903478754<br />PC2:   0.464386633<br />cluster: 3","PC1: -1.405244516<br />PC2:   0.363543881<br />cluster: 3","PC1: -1.394804567<br />PC2:   0.013004726<br />cluster: 3","PC1: -1.492676026<br />PC2:   0.611959957<br />cluster: 3","PC1: -1.758667346<br />PC2:   0.315345403<br />cluster: 3","PC1: -0.953544558<br />PC2:  -0.053930843<br />cluster: 3","PC1: -1.886181364<br />PC2:   0.136771049<br />cluster: 3","PC1: -1.429042706<br />PC2:   0.295483551<br />cluster: 3","PC1: -0.908591403<br />PC2:  -1.833164585<br />cluster: 3","PC1: -1.120184909<br />PC2:   0.294638022<br />cluster: 3","PC1: -1.940756398<br />PC2:  -0.239462236<br />cluster: 3","PC1: -1.948217629<br />PC2:   0.471210833<br />cluster: 3","PC1: -1.975189027<br />PC2:   0.092797634<br />cluster: 3","PC1: -0.080021826<br />PC2:   0.401433394<br />cluster: 3"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(77,175,74,1)","opacity":1,"size":5.6692913385826778,"symbol":"circle","line":{"width":1.8897637795275593,"color":"rgba(77,175,74,1)"}},"hoveron":"points","name":"3","legendgroup":"3","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1.2699452418665713,1.5815417973265731,2.3854418400576343,2.6545510567545954,1.9686533855334558,2.2948185231737757,2.4524085683188694,1.4756674440736053,0.30870554177475307,2.360858780762765,1.7175317640006482,2.4080354504290664,2.3340815975310512,0.76277972922330695,3.3295710819982323,2.0613260042087864,2.2280287760982995,1.7209378048606692,1.0504379878903165,0.99591329396851858,2.1900634273880684,2.2401215968625281,1.5275969962226041,0.68147597669979287,1.9631092901468059,3.2032935554426438,1.5099873843223197,1.2421126463475276,1.3784438663280694,2.1572401942088475,0.60789216865662865,0.57337380936653215,1.0985148570395731,1.4098374337670658,0.76874271124089066,0.56832242856538595,0.53781204698927487,3.427864133271421,2.2857319289013005,2.621007196941286,1.1701325197406807,1.6619643904950983,1.245853065512629,0.55739515744335077,1.0136388851614961,1.9757283646024995,0.47078345478485673,0.67369548400681356,2.1059311983149711,1.3124732690433734,2.7274809531367348,1.3818899296193414,0.97829052689815321,1.234980571217086,2.4497807478502804,0.67269198291194787,0.54820444848688232,2.4337146876582079,1.9380096701208731,1.2218518915427874,1.9046006346514528,1.4316628971541256,1.0990723808745366,0.77568738324854369,3.0233074688641328,0.60901727255412819,1.6764263692586379,1.8173503133914004,0.78673364818156466,2.7533742114727771,1.7587112805037344],"y":[0.10738030471479415,-0.095075160590210539,0.67729143747736387,0.43147416365879321,0.76885784236901389,-0.14414621965462962,-0.12289480260418187,-0.074506564142195197,-0.75425613863400887,0.54237063161445198,-0.38273144015374944,0.28083749971625027,0.30363090610306886,-0.44512402207519847,-0.037860140308638365,0.22962077837727946,0.64872580543676472,0.76470854669291433,-0.094983430399349242,0.21691229142725399,0.11845712441856393,0.10049245143866499,0.61718640886607856,0.52626654992370758,-0.29184678459504904,-1.3297379658545783,0.2966730034714059,-0.15276730572975467,0.015909543035187032,-0.50376904435810788,-0.15560715081745211,0.2229331141004236,-0.30920340079728281,-0.28977275624797239,-0.27409063995407301,0.2450404793799007,0.078976793217883173,0.17702248503360615,0.017815682515471609,-0.57533504571852645,0.097512832464358074,0.34376886895212155,-1.3620759332899619,0.12930301522639584,-0.90907667463396913,0.20745339993673839,-1.2803715941996989,-0.49422058140337,0.23485617418701241,0.27903449669801395,-0.0084921501796331907,0.49922281123492679,0.14291009883205114,0.064829805258542678,-0.44723456066277878,-0.37001272819887021,-0.08584625299829321,-1.2639577963182584,-1.743217567351063,-1.6657591726572414,-2.3608241502954996,-3.9655542044009362,0.30012239660511297,0.60880430611334635,0.1749351505119196,0.086119190896103037,0.57898557151943753,-0.63470259852649102,-1.3135295880147719,-6.1799447609240614,-0.27247354675235436],"text":["PC1:  1.269945242<br />PC2:   0.107380305<br />cluster: 4","PC1:  1.581541797<br />PC2:  -0.095075161<br />cluster: 4","PC1:  2.385441840<br />PC2:   0.677291437<br />cluster: 4","PC1:  2.654551057<br />PC2:   0.431474164<br />cluster: 4","PC1:  1.968653386<br />PC2:   0.768857842<br />cluster: 4","PC1:  2.294818523<br />PC2:  -0.144146220<br />cluster: 4","PC1:  2.452408568<br />PC2:  -0.122894803<br />cluster: 4","PC1:  1.475667444<br />PC2:  -0.074506564<br />cluster: 4","PC1:  0.308705542<br />PC2:  -0.754256139<br />cluster: 4","PC1:  2.360858781<br />PC2:   0.542370632<br />cluster: 4","PC1:  1.717531764<br />PC2:  -0.382731440<br />cluster: 4","PC1:  2.408035450<br />PC2:   0.280837500<br />cluster: 4","PC1:  2.334081598<br />PC2:   0.303630906<br />cluster: 4","PC1:  0.762779729<br />PC2:  -0.445124022<br />cluster: 4","PC1:  3.329571082<br />PC2:  -0.037860140<br />cluster: 4","PC1:  2.061326004<br />PC2:   0.229620778<br />cluster: 4","PC1:  2.228028776<br />PC2:   0.648725805<br />cluster: 4","PC1:  1.720937805<br />PC2:   0.764708547<br />cluster: 4","PC1:  1.050437988<br />PC2:  -0.094983430<br />cluster: 4","PC1:  0.995913294<br />PC2:   0.216912291<br />cluster: 4","PC1:  2.190063427<br />PC2:   0.118457124<br />cluster: 4","PC1:  2.240121597<br />PC2:   0.100492451<br />cluster: 4","PC1:  1.527596996<br />PC2:   0.617186409<br />cluster: 4","PC1:  0.681475977<br />PC2:   0.526266550<br />cluster: 4","PC1:  1.963109290<br />PC2:  -0.291846785<br />cluster: 4","PC1:  3.203293555<br />PC2:  -1.329737966<br />cluster: 4","PC1:  1.509987384<br />PC2:   0.296673003<br />cluster: 4","PC1:  1.242112646<br />PC2:  -0.152767306<br />cluster: 4","PC1:  1.378443866<br />PC2:   0.015909543<br />cluster: 4","PC1:  2.157240194<br />PC2:  -0.503769044<br />cluster: 4","PC1:  0.607892169<br />PC2:  -0.155607151<br />cluster: 4","PC1:  0.573373809<br />PC2:   0.222933114<br />cluster: 4","PC1:  1.098514857<br />PC2:  -0.309203401<br />cluster: 4","PC1:  1.409837434<br />PC2:  -0.289772756<br />cluster: 4","PC1:  0.768742711<br />PC2:  -0.274090640<br />cluster: 4","PC1:  0.568322429<br />PC2:   0.245040479<br />cluster: 4","PC1:  0.537812047<br />PC2:   0.078976793<br />cluster: 4","PC1:  3.427864133<br />PC2:   0.177022485<br />cluster: 4","PC1:  2.285731929<br />PC2:   0.017815683<br />cluster: 4","PC1:  2.621007197<br />PC2:  -0.575335046<br />cluster: 4","PC1:  1.170132520<br />PC2:   0.097512832<br />cluster: 4","PC1:  1.661964390<br />PC2:   0.343768869<br />cluster: 4","PC1:  1.245853066<br />PC2:  -1.362075933<br />cluster: 4","PC1:  0.557395157<br />PC2:   0.129303015<br />cluster: 4","PC1:  1.013638885<br />PC2:  -0.909076675<br />cluster: 4","PC1:  1.975728365<br />PC2:   0.207453400<br />cluster: 4","PC1:  0.470783455<br />PC2:  -1.280371594<br />cluster: 4","PC1:  0.673695484<br />PC2:  -0.494220581<br />cluster: 4","PC1:  2.105931198<br />PC2:   0.234856174<br />cluster: 4","PC1:  1.312473269<br />PC2:   0.279034497<br />cluster: 4","PC1:  2.727480953<br />PC2:  -0.008492150<br />cluster: 4","PC1:  1.381889930<br />PC2:   0.499222811<br />cluster: 4","PC1:  0.978290527<br />PC2:   0.142910099<br />cluster: 4","PC1:  1.234980571<br />PC2:   0.064829805<br />cluster: 4","PC1:  2.449780748<br />PC2:  -0.447234561<br />cluster: 4","PC1:  0.672691983<br />PC2:  -0.370012728<br />cluster: 4","PC1:  0.548204448<br />PC2:  -0.085846253<br />cluster: 4","PC1:  2.433714688<br />PC2:  -1.263957796<br />cluster: 4","PC1:  1.938009670<br />PC2:  -1.743217567<br />cluster: 4","PC1:  1.221851892<br />PC2:  -1.665759173<br />cluster: 4","PC1:  1.904600635<br />PC2:  -2.360824150<br />cluster: 4","PC1:  1.431662897<br />PC2:  -3.965554204<br />cluster: 4","PC1:  1.099072381<br />PC2:   0.300122397<br />cluster: 4","PC1:  0.775687383<br />PC2:   0.608804306<br />cluster: 4","PC1:  3.023307469<br />PC2:   0.174935151<br />cluster: 4","PC1:  0.609017273<br />PC2:   0.086119191<br />cluster: 4","PC1:  1.676426369<br />PC2:   0.578985572<br />cluster: 4","PC1:  1.817350313<br />PC2:  -0.634702599<br />cluster: 4","PC1:  0.786733648<br />PC2:  -1.313529588<br />cluster: 4","PC1:  2.753374211<br />PC2:  -6.179944761<br />cluster: 4","PC1:  1.758711281<br />PC2:  -0.272473547<br />cluster: 4"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(152,78,163,1)","opacity":1,"size":5.6692913385826778,"symbol":"circle","line":{"width":1.8897637795275593,"color":"rgba(152,78,163,1)"}},"hoveron":"points","name":"4","legendgroup":"4","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[-1.4733690474470085,-1.5248544463661189,-2.216579643173358,-1.6760448226164406,-1.7355620548776418,-2.5313749685149896,-2.5485011982864396,-2.5521675873135119,-2.8741508837642558,-2.6286653351163674,-2.0791474189894208,-2.2777801664402744,-3.2547143942595449,-3.4656853235444807,-2.0719125428241241,-3.0096734391674262,-1.4004996405422798,-2.2867160167492422,-1.7253767414500689,-1.9442410613618848,-1.3872102676445059,-1.7042946299399249,-1.649628195858621,-2.9913774129169579,-2.0144548638760384,-2.0736619567733743,-1.8635012207087167,-1.763021681179636,-1.8660488093358278,-2.440652064614238,-2.8720461314326662,-1.478172375762721,-2.2404583080348837,-1.1897855966041906,-2.2950702692175575,-1.5610243852464838,-2.5915353082190524,-2.2897629444051342,-1.4199594511466891,-2.2331481035533174,-2.5348600556948173,-2.6357341027578172,-2.3304730505283286,-2.2731686126399984,-2.3818472454690234,-2.327675594309778,-2.3191823673049852,-2.1153362780623239,-3.4095147440707265,-1.752728737134013,-2.409906131342709,-3.9425040185059799,-1.4675961085635096,-3.1644699174227324,-1.5589198453475488,-3.1791549538479065,-4.6133667368245108,-3.9730520960489519,-2.3377177428229872,-3.4671739907093668,-1.9366602933061028,-4.6577828781139328,-1.9558993129405282,-2.1317614512512524,-2.4228257153169963,-2.8063737754747891,-2.2956823613217057,-2.2998008925730908,-0.84158039450235289,-2.1196637258105775,-2.7463595629730397,-1.8469401101816438,-3.1144892301927563,-2.9109374509826296,-2.0388071321688539,-2.6753678783905741,-1.8886721347654711,-2.2051632128881979,-2.1487664659529799,-2.5359654169171661,-2.0320699561682667,-2.1157796210865905,-2.2889266174759211,-3.3100346219606358,-3.1567335160607017,-1.5096542589893678,-2.1452162981449971,-1.6405325176557852,-3.5907964608926384,-2.869777781814665,-5.14842831121919,-2.5245564026175256,-1.8233152797659025,-4.3342531489973251,-2.8404803092606885],"y":[0.54506961892315231,0.61749369852311065,0.40467670315632581,-0.95043603291530676,0.7199111496588283,0.80431752445512983,0.25020532423107483,0.71940481063544681,0.14097083904496591,0.04852119379277893,0.36231892021491563,0.41099309148746943,0.7666108285161688,0.47432590182592727,0.92659755052528325,0.30865276460901253,-0.21879653675880367,0.33118503642189112,-0.13233185933638508,0.42417662301846693,0.48511694242340259,-1.301107585290759,0.32405946646471312,0.71824199288099599,0.40466585560259566,0.20359028913791408,-2.9469101352662026,0.12447570371950338,0.38798953280167764,0.57800969304289695,0.33266089114097086,-1.7735823862103439,-2.5507558653940903,0.72709211465937273,0.544300377275721,0.30879724316134544,0.34204558721328315,-0.99771373932096186,-0.31119308699145048,0.13608447328838921,-2.8212703801466934,0.15011657180121335,0.49947078628941005,0.04340268395597012,0.7180674115335246,0.38353277478973147,0.22682821042410339,-0.24296479826208894,0.33933610362613509,0.70662656034153315,0.61428805625691629,0.39143700983352986,0.29679079018657262,0.69034176537108216,0.42542869058810445,0.2362970268413821,0.92473642105006637,0.60265533761240819,-0.38492570918331392,0.089219906896168127,-2.6670609347112637,0.098908920474489737,0.1755772594550031,0.42973575470643977,0.26640825762579323,0.62675015371665999,0.30679447370950408,0.32352884038636592,-0.31973274801098511,-0.2863868284704516,0.24678687756226467,0.40841375731742163,0.36055541868059954,0.2827126996999158,0.36069412470757667,0.05380137967010979,0.67890559406132422,0.86308470800814208,-0.063360154644628436,0.20473802100430644,0.1521911681112316,-1.1481793973292054,0.4656195275779087,-0.11834718813610974,0.42652944913677648,-0.01567986663818427,0.62870092128707888,-3.3409356299507746,0.40338969642422673,-1.857779251270296,-0.083698033402681338,0.31853605843053023,-0.063774796499477149,-0.15026104109369726,0.61896089057150261],"text":["PC1: -1.473369047<br />PC2:   0.545069619<br />cluster: 5","PC1: -1.524854446<br />PC2:   0.617493699<br />cluster: 5","PC1: -2.216579643<br />PC2:   0.404676703<br />cluster: 5","PC1: -1.676044823<br />PC2:  -0.950436033<br />cluster: 5","PC1: -1.735562055<br />PC2:   0.719911150<br />cluster: 5","PC1: -2.531374969<br />PC2:   0.804317524<br />cluster: 5","PC1: -2.548501198<br />PC2:   0.250205324<br />cluster: 5","PC1: -2.552167587<br />PC2:   0.719404811<br />cluster: 5","PC1: -2.874150884<br />PC2:   0.140970839<br />cluster: 5","PC1: -2.628665335<br />PC2:   0.048521194<br />cluster: 5","PC1: -2.079147419<br />PC2:   0.362318920<br />cluster: 5","PC1: -2.277780166<br />PC2:   0.410993091<br />cluster: 5","PC1: -3.254714394<br />PC2:   0.766610829<br />cluster: 5","PC1: -3.465685324<br />PC2:   0.474325902<br />cluster: 5","PC1: -2.071912543<br />PC2:   0.926597551<br />cluster: 5","PC1: -3.009673439<br />PC2:   0.308652765<br />cluster: 5","PC1: -1.400499641<br />PC2:  -0.218796537<br />cluster: 5","PC1: -2.286716017<br />PC2:   0.331185036<br />cluster: 5","PC1: -1.725376741<br />PC2:  -0.132331859<br />cluster: 5","PC1: -1.944241061<br />PC2:   0.424176623<br />cluster: 5","PC1: -1.387210268<br />PC2:   0.485116942<br />cluster: 5","PC1: -1.704294630<br />PC2:  -1.301107585<br />cluster: 5","PC1: -1.649628196<br />PC2:   0.324059466<br />cluster: 5","PC1: -2.991377413<br />PC2:   0.718241993<br />cluster: 5","PC1: -2.014454864<br />PC2:   0.404665856<br />cluster: 5","PC1: -2.073661957<br />PC2:   0.203590289<br />cluster: 5","PC1: -1.863501221<br />PC2:  -2.946910135<br />cluster: 5","PC1: -1.763021681<br />PC2:   0.124475704<br />cluster: 5","PC1: -1.866048809<br />PC2:   0.387989533<br />cluster: 5","PC1: -2.440652065<br />PC2:   0.578009693<br />cluster: 5","PC1: -2.872046131<br />PC2:   0.332660891<br />cluster: 5","PC1: -1.478172376<br />PC2:  -1.773582386<br />cluster: 5","PC1: -2.240458308<br />PC2:  -2.550755865<br />cluster: 5","PC1: -1.189785597<br />PC2:   0.727092115<br />cluster: 5","PC1: -2.295070269<br />PC2:   0.544300377<br />cluster: 5","PC1: -1.561024385<br />PC2:   0.308797243<br />cluster: 5","PC1: -2.591535308<br />PC2:   0.342045587<br />cluster: 5","PC1: -2.289762944<br />PC2:  -0.997713739<br />cluster: 5","PC1: -1.419959451<br />PC2:  -0.311193087<br />cluster: 5","PC1: -2.233148104<br />PC2:   0.136084473<br />cluster: 5","PC1: -2.534860056<br />PC2:  -2.821270380<br />cluster: 5","PC1: -2.635734103<br />PC2:   0.150116572<br />cluster: 5","PC1: -2.330473051<br />PC2:   0.499470786<br />cluster: 5","PC1: -2.273168613<br />PC2:   0.043402684<br />cluster: 5","PC1: -2.381847245<br />PC2:   0.718067412<br />cluster: 5","PC1: -2.327675594<br />PC2:   0.383532775<br />cluster: 5","PC1: -2.319182367<br />PC2:   0.226828210<br />cluster: 5","PC1: -2.115336278<br />PC2:  -0.242964798<br />cluster: 5","PC1: -3.409514744<br />PC2:   0.339336104<br />cluster: 5","PC1: -1.752728737<br />PC2:   0.706626560<br />cluster: 5","PC1: -2.409906131<br />PC2:   0.614288056<br />cluster: 5","PC1: -3.942504019<br />PC2:   0.391437010<br />cluster: 5","PC1: -1.467596109<br />PC2:   0.296790790<br />cluster: 5","PC1: -3.164469917<br />PC2:   0.690341765<br />cluster: 5","PC1: -1.558919845<br />PC2:   0.425428691<br />cluster: 5","PC1: -3.179154954<br />PC2:   0.236297027<br />cluster: 5","PC1: -4.613366737<br />PC2:   0.924736421<br />cluster: 5","PC1: -3.973052096<br />PC2:   0.602655338<br />cluster: 5","PC1: -2.337717743<br />PC2:  -0.384925709<br />cluster: 5","PC1: -3.467173991<br />PC2:   0.089219907<br />cluster: 5","PC1: -1.936660293<br />PC2:  -2.667060935<br />cluster: 5","PC1: -4.657782878<br />PC2:   0.098908920<br />cluster: 5","PC1: -1.955899313<br />PC2:   0.175577259<br />cluster: 5","PC1: -2.131761451<br />PC2:   0.429735755<br />cluster: 5","PC1: -2.422825715<br />PC2:   0.266408258<br />cluster: 5","PC1: -2.806373775<br />PC2:   0.626750154<br />cluster: 5","PC1: -2.295682361<br />PC2:   0.306794474<br />cluster: 5","PC1: -2.299800893<br />PC2:   0.323528840<br />cluster: 5","PC1: -0.841580395<br />PC2:  -0.319732748<br />cluster: 5","PC1: -2.119663726<br />PC2:  -0.286386828<br />cluster: 5","PC1: -2.746359563<br />PC2:   0.246786878<br />cluster: 5","PC1: -1.846940110<br />PC2:   0.408413757<br />cluster: 5","PC1: -3.114489230<br />PC2:   0.360555419<br />cluster: 5","PC1: -2.910937451<br />PC2:   0.282712700<br />cluster: 5","PC1: -2.038807132<br />PC2:   0.360694125<br />cluster: 5","PC1: -2.675367878<br />PC2:   0.053801380<br />cluster: 5","PC1: -1.888672135<br />PC2:   0.678905594<br />cluster: 5","PC1: -2.205163213<br />PC2:   0.863084708<br />cluster: 5","PC1: -2.148766466<br />PC2:  -0.063360155<br />cluster: 5","PC1: -2.535965417<br />PC2:   0.204738021<br />cluster: 5","PC1: -2.032069956<br />PC2:   0.152191168<br />cluster: 5","PC1: -2.115779621<br />PC2:  -1.148179397<br />cluster: 5","PC1: -2.288926617<br />PC2:   0.465619528<br />cluster: 5","PC1: -3.310034622<br />PC2:  -0.118347188<br />cluster: 5","PC1: -3.156733516<br />PC2:   0.426529449<br />cluster: 5","PC1: -1.509654259<br />PC2:  -0.015679867<br />cluster: 5","PC1: -2.145216298<br />PC2:   0.628700921<br />cluster: 5","PC1: -1.640532518<br />PC2:  -3.340935630<br />cluster: 5","PC1: -3.590796461<br />PC2:   0.403389696<br />cluster: 5","PC1: -2.869777782<br />PC2:  -1.857779251<br />cluster: 5","PC1: -5.148428311<br />PC2:  -0.083698033<br />cluster: 5","PC1: -2.524556403<br />PC2:   0.318536058<br />cluster: 5","PC1: -1.823315280<br />PC2:  -0.063774796<br />cluster: 5","PC1: -4.334253149<br />PC2:  -0.150261041<br />cluster: 5","PC1: -2.840480309<br />PC2:   0.618960891<br />cluster: 5"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(255,127,0,1)","opacity":1,"size":5.6692913385826778,"symbol":"circle","line":{"width":1.8897637795275593,"color":"rgba(255,127,0,1)"}},"hoveron":"points","name":"5","legendgroup":"5","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1.4911354762817939,0.13277434029959229,-3.0479284977039294,0.0085174413139776951],"y":[-11.039171395158796,-13.46767575344863,-11.449619694352981,-5.1130299456540502],"text":["PC1:  1.491135476<br />PC2: -11.039171395<br />cluster: 6","PC1:  0.132774340<br />PC2: -13.467675753<br />cluster: 6","PC1: -3.047928498<br />PC2: -11.449619694<br />cluster: 6","PC1:  0.008517441<br />PC2:  -5.113029946<br />cluster: 6"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(255,255,51,1)","opacity":1,"size":5.6692913385826778,"symbol":"circle","line":{"width":1.8897637795275593,"color":"rgba(255,255,51,1)"}},"hoveron":"points","name":"6","legendgroup":"6","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":43.762557077625573,"r":7.3059360730593621,"b":40.182648401826491,"l":43.105022831050235},"font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724},"title":{"text":"Interactive scatterplot of PC1 and PC2 colored by cluster for Guediawaye Census 2013","font":{"color":"rgba(0,0,0,1)","family":"","size":17.534246575342465},"x":0,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-5.8724606649824018,10.056251117808264],"tickmode":"array","ticktext":["-5","0","5","10"],"tickvals":[-5,0,5,10],"categoryorder":"array","categoryarray":["-5","0","5","10"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.68949771689498},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176002,"zeroline":false,"anchor":"y","title":{"text":"PC1","font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-14.203331610437651,1.9810972433208243],"tickmode":"array","ticktext":["-10","-5","0"],"tickvals":[-10,-5,0],"categoryorder":"array","categoryarray":["-10","-5","0"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.68949771689498},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176002,"zeroline":false,"anchor":"x","title":{"text":"PC2","font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":true,"legend":{"bgcolor":null,"bordercolor":null,"borderwidth":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.68949771689498},"title":"Cluster","orientation":"h","y":-0.14999999999999999,"x":0.20000000000000001},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"240c1d597ca6":{"x":{},"y":{},"colour":{},"type":"scatter"}},"cur_data":"240c1d597ca6","visdat":{"240c1d597ca6":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```



```r
cluster_plot <- ggplot(dfw_clusters_2002, 
                       aes(x = PC1, y = PC2, color = cluster)) + 
  geom_point() + 
  scale_color_brewer(palette = "Set1") + 
  labs(title = "Interactive scatterplot of PC1 and PC2 colored by cluster for Guediawaye Census 2002") +
  theme_minimal()

ggplotly(cluster_plot) %>%
  layout(legend = list(orientation = "h", y = -0.15, 
                       x = 0.2, title = "Cluster"))
```

```{=html}
<div class="plotly html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-8bc471d399af9e9bc5e5" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-8bc471d399af9e9bc5e5">{"x":{"data":[{"x":[-1.2151159559093896,-3.0463853253707076,-1.3380828039152575,-1.5800379837440073,-1.9756169245255142,-1.8955821736030543,-0.6374079258786659,-1.5233988200619395,-2.5406878113370803,-0.67461528097239343,-1.2085096655987924,-1.4998028837032815,-1.9824179936371915,-1.2675140248796823,-2.3380457700901185,-1.5744736031178697,-1.4903886187993425,-0.62827706231109781,-1.5615696334372746,-2.4544272974223262,-0.56950450086527538,-1.4822745382868694,-0.90590731837773741,-1.4307373716424627,-1.6851927915449314,-0.67110653804821452,-0.99041808983538304,-0.7933171193551023,-2.310495799063208,-1.8736519229571114,-1.6112066102434521,-0.7527074456659224,-1.4565428187841696,-1.3327114741093975,-0.43245975991975871,-0.74859993253168178,-0.73504122688479734,-0.34508832456428973,-0.97865215459795429,-2.3589679773734527,-0.67905897945326521,-0.98307214305404411,-2.214463464887575,-1.3085630126439991,-2.4544644558167406,-1.3464794662636874,-0.97068914420039232,-0.93825548134518355,-0.99887432927570452,-1.7387393398162059,-0.27316800545759862,-3.1293106610042498,-0.49134816409903914,-2.4034839814397064,-1.0914165254849229,-0.89235054718629314,-0.5969980670291134,-2.0294859751232677,-0.4883513045385513,-2.5634524997105976,-1.0614282576909875,-0.88427718677519662,-0.41833239508653197,-1.7752174215873988,-0.89272757974326788,-3.2325733520608155,-1.6258956451254378,-1.6575952857138505,-2.0111821927392373,-1.2020289885465074,-0.79672049596219896],"y":[0.28702160801631321,0.11092930257704063,0.374713190444388,0.35687937737482711,0.58495611081715526,-0.26798866721586051,0.055919832693337085,0.78541308812467525,0.19848771462556897,0.17744181171485462,1.5350472045228021,0.16216549117898993,0.63191173628932429,-0.23843873925964959,0.073233782632935257,-0.29437497045827338,0.027096986842382091,-0.51936701191559043,0.2120675252221442,-0.20511282842912773,-0.33381352548137511,-0.67281162896779789,0.10760314234214283,-0.17887912796618405,-0.2716126714490194,-0.21430033792463829,-0.1374178854782804,-0.36390198014061287,0.57314904748583517,-0.21976564157196507,-0.19692291916329036,-0.23138846669702035,-0.22134909088367369,-0.12794739253500303,-0.27586401068602995,-0.17938640107550829,-0.19063576774968857,-0.075118695905418198,-0.19444972580429376,-0.31141316784472239,-0.28367389671575882,-0.13228195252188968,-0.22347402109073988,-0.15279368470442306,-0.45874555491621111,-0.50042009382444963,-0.47842484259265539,0.022916826115647753,-0.2457635310899573,-0.087894861282487441,0.17888678790129914,-0.19743860210279157,-0.17797414292475203,0.0025991454872257569,-0.39740242271406656,-0.3116984153122565,3.1086330324197147,-0.042685425253663638,-0.60374472859671668,-0.25946234829163445,-0.43783027163536642,-0.33981847840237411,-0.49609602717947143,-0.25360887517536662,-0.59765519129751565,-0.17090626186102229,0.13918548224733712,-0.34378767587916426,0.086659643742202752,-0.44355961412762612,-0.66625663704861526],"text":["PC1: -1.215115956<br />PC2:  0.287021608<br />cluster: 1","PC1: -3.046385325<br />PC2:  0.110929303<br />cluster: 1","PC1: -1.338082804<br />PC2:  0.374713190<br />cluster: 1","PC1: -1.580037984<br />PC2:  0.356879377<br />cluster: 1","PC1: -1.975616925<br />PC2:  0.584956111<br />cluster: 1","PC1: -1.895582174<br />PC2: -0.267988667<br />cluster: 1","PC1: -0.637407926<br />PC2:  0.055919833<br />cluster: 1","PC1: -1.523398820<br />PC2:  0.785413088<br />cluster: 1","PC1: -2.540687811<br />PC2:  0.198487715<br />cluster: 1","PC1: -0.674615281<br />PC2:  0.177441812<br />cluster: 1","PC1: -1.208509666<br />PC2:  1.535047205<br />cluster: 1","PC1: -1.499802884<br />PC2:  0.162165491<br />cluster: 1","PC1: -1.982417994<br />PC2:  0.631911736<br />cluster: 1","PC1: -1.267514025<br />PC2: -0.238438739<br />cluster: 1","PC1: -2.338045770<br />PC2:  0.073233783<br />cluster: 1","PC1: -1.574473603<br />PC2: -0.294374970<br />cluster: 1","PC1: -1.490388619<br />PC2:  0.027096987<br />cluster: 1","PC1: -0.628277062<br />PC2: -0.519367012<br />cluster: 1","PC1: -1.561569633<br />PC2:  0.212067525<br />cluster: 1","PC1: -2.454427297<br />PC2: -0.205112828<br />cluster: 1","PC1: -0.569504501<br />PC2: -0.333813525<br />cluster: 1","PC1: -1.482274538<br />PC2: -0.672811629<br />cluster: 1","PC1: -0.905907318<br />PC2:  0.107603142<br />cluster: 1","PC1: -1.430737372<br />PC2: -0.178879128<br />cluster: 1","PC1: -1.685192792<br />PC2: -0.271612671<br />cluster: 1","PC1: -0.671106538<br />PC2: -0.214300338<br />cluster: 1","PC1: -0.990418090<br />PC2: -0.137417885<br />cluster: 1","PC1: -0.793317119<br />PC2: -0.363901980<br />cluster: 1","PC1: -2.310495799<br />PC2:  0.573149047<br />cluster: 1","PC1: -1.873651923<br />PC2: -0.219765642<br />cluster: 1","PC1: -1.611206610<br />PC2: -0.196922919<br />cluster: 1","PC1: -0.752707446<br />PC2: -0.231388467<br />cluster: 1","PC1: -1.456542819<br />PC2: -0.221349091<br />cluster: 1","PC1: -1.332711474<br />PC2: -0.127947393<br />cluster: 1","PC1: -0.432459760<br />PC2: -0.275864011<br />cluster: 1","PC1: -0.748599933<br />PC2: -0.179386401<br />cluster: 1","PC1: -0.735041227<br />PC2: -0.190635768<br />cluster: 1","PC1: -0.345088325<br />PC2: -0.075118696<br />cluster: 1","PC1: -0.978652155<br />PC2: -0.194449726<br />cluster: 1","PC1: -2.358967977<br />PC2: -0.311413168<br />cluster: 1","PC1: -0.679058979<br />PC2: -0.283673897<br />cluster: 1","PC1: -0.983072143<br />PC2: -0.132281953<br />cluster: 1","PC1: -2.214463465<br />PC2: -0.223474021<br />cluster: 1","PC1: -1.308563013<br />PC2: -0.152793685<br />cluster: 1","PC1: -2.454464456<br />PC2: -0.458745555<br />cluster: 1","PC1: -1.346479466<br />PC2: -0.500420094<br />cluster: 1","PC1: -0.970689144<br />PC2: -0.478424843<br />cluster: 1","PC1: -0.938255481<br />PC2:  0.022916826<br />cluster: 1","PC1: -0.998874329<br />PC2: -0.245763531<br />cluster: 1","PC1: -1.738739340<br />PC2: -0.087894861<br />cluster: 1","PC1: -0.273168005<br />PC2:  0.178886788<br />cluster: 1","PC1: -3.129310661<br />PC2: -0.197438602<br />cluster: 1","PC1: -0.491348164<br />PC2: -0.177974143<br />cluster: 1","PC1: -2.403483981<br />PC2:  0.002599145<br />cluster: 1","PC1: -1.091416525<br />PC2: -0.397402423<br />cluster: 1","PC1: -0.892350547<br />PC2: -0.311698415<br />cluster: 1","PC1: -0.596998067<br />PC2:  3.108633032<br />cluster: 1","PC1: -2.029485975<br />PC2: -0.042685425<br />cluster: 1","PC1: -0.488351305<br />PC2: -0.603744729<br />cluster: 1","PC1: -2.563452500<br />PC2: -0.259462348<br />cluster: 1","PC1: -1.061428258<br />PC2: -0.437830272<br />cluster: 1","PC1: -0.884277187<br />PC2: -0.339818478<br />cluster: 1","PC1: -0.418332395<br />PC2: -0.496096027<br />cluster: 1","PC1: -1.775217422<br />PC2: -0.253608875<br />cluster: 1","PC1: -0.892727580<br />PC2: -0.597655191<br />cluster: 1","PC1: -3.232573352<br />PC2: -0.170906262<br />cluster: 1","PC1: -1.625895645<br />PC2:  0.139185482<br />cluster: 1","PC1: -1.657595286<br />PC2: -0.343787676<br />cluster: 1","PC1: -2.011182193<br />PC2:  0.086659644<br />cluster: 1","PC1: -1.202028989<br />PC2: -0.443559614<br />cluster: 1","PC1: -0.796720496<br />PC2: -0.666256637<br />cluster: 1"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(228,26,28,1)","opacity":1,"size":5.6692913385826778,"symbol":"circle","line":{"width":1.8897637795275593,"color":"rgba(228,26,28,1)"}},"hoveron":"points","name":"1","legendgroup":"1","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[-0.62878265503288955,1.2166677920206761,0.4020084707767434,-0.45924364815093432,1.1243429972045245,1.3668013827147842,0.20818074263432237,0.55867040301373194,-0.094554936962738478,-0.0061037419303873186,-0.38276600962851187,2.2328457424026618,0.61028240545721091,0.98779398531834961,-0.022211687096428914,0.58192373046461654,1.1461050756733759,1.7145601040110208,1.9470891288072238,2.5642033165929203,-0.29132505943944587,-0.22524158857518831,1.1884528734308721,-0.013102416989988375,0.97654752662595512,3.0844521532373048,2.8614313957224486,-0.73349974152420394,-0.065212439732008576,0.054388701679420198,0.29988553396770029,0.9062154587461051,1.1973972565731796,2.4695650690352551,2.0081757268430263,1.2341739858831118,0.62987758715906739,4.1892026864829059],"y":[0.28702645835150525,0.041876161517071409,0.1867832306594378,0.67958511093488738,-0.12572402332124968,0.82052434332372604,1.4063481696229223,1.5261444061037579,0.6868389159992111,0.96396359029749323,1.7166755624398555,1.6042270222003578,0.48939894816269164,0.2829939816083184,0.2620088265183923,0.54806478554591331,-0.20691935926088814,0.49725458872542877,-0.22124320780301493,-0.45245645542357316,2.9669630454388081,0.40051193891280867,0.39061728040219701,0.47900023227216504,-0.0026175120037805616,-0.31004744660531769,-0.31821724807097301,0.60182147325402013,0.67390785294243782,1.9247160604802278,1.2540002846460521,1.1517226070750373,1.4950281242198009,0.99449514432194497,1.0639754432451658,0.43945972125583699,0.19170809744523692,-0.029164795389663489],"text":["PC1: -0.628782655<br />PC2:  0.287026458<br />cluster: 2","PC1:  1.216667792<br />PC2:  0.041876162<br />cluster: 2","PC1:  0.402008471<br />PC2:  0.186783231<br />cluster: 2","PC1: -0.459243648<br />PC2:  0.679585111<br />cluster: 2","PC1:  1.124342997<br />PC2: -0.125724023<br />cluster: 2","PC1:  1.366801383<br />PC2:  0.820524343<br />cluster: 2","PC1:  0.208180743<br />PC2:  1.406348170<br />cluster: 2","PC1:  0.558670403<br />PC2:  1.526144406<br />cluster: 2","PC1: -0.094554937<br />PC2:  0.686838916<br />cluster: 2","PC1: -0.006103742<br />PC2:  0.963963590<br />cluster: 2","PC1: -0.382766010<br />PC2:  1.716675562<br />cluster: 2","PC1:  2.232845742<br />PC2:  1.604227022<br />cluster: 2","PC1:  0.610282405<br />PC2:  0.489398948<br />cluster: 2","PC1:  0.987793985<br />PC2:  0.282993982<br />cluster: 2","PC1: -0.022211687<br />PC2:  0.262008827<br />cluster: 2","PC1:  0.581923730<br />PC2:  0.548064786<br />cluster: 2","PC1:  1.146105076<br />PC2: -0.206919359<br />cluster: 2","PC1:  1.714560104<br />PC2:  0.497254589<br />cluster: 2","PC1:  1.947089129<br />PC2: -0.221243208<br />cluster: 2","PC1:  2.564203317<br />PC2: -0.452456455<br />cluster: 2","PC1: -0.291325059<br />PC2:  2.966963045<br />cluster: 2","PC1: -0.225241589<br />PC2:  0.400511939<br />cluster: 2","PC1:  1.188452873<br />PC2:  0.390617280<br />cluster: 2","PC1: -0.013102417<br />PC2:  0.479000232<br />cluster: 2","PC1:  0.976547527<br />PC2: -0.002617512<br />cluster: 2","PC1:  3.084452153<br />PC2: -0.310047447<br />cluster: 2","PC1:  2.861431396<br />PC2: -0.318217248<br />cluster: 2","PC1: -0.733499742<br />PC2:  0.601821473<br />cluster: 2","PC1: -0.065212440<br />PC2:  0.673907853<br />cluster: 2","PC1:  0.054388702<br />PC2:  1.924716060<br />cluster: 2","PC1:  0.299885534<br />PC2:  1.254000285<br />cluster: 2","PC1:  0.906215459<br />PC2:  1.151722607<br />cluster: 2","PC1:  1.197397257<br />PC2:  1.495028124<br />cluster: 2","PC1:  2.469565069<br />PC2:  0.994495144<br />cluster: 2","PC1:  2.008175727<br />PC2:  1.063975443<br />cluster: 2","PC1:  1.234173986<br />PC2:  0.439459721<br />cluster: 2","PC1:  0.629877587<br />PC2:  0.191708097<br />cluster: 2","PC1:  4.189202686<br />PC2: -0.029164795<br />cluster: 2"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(55,126,184,1)","opacity":1,"size":5.6692913385826778,"symbol":"circle","line":{"width":1.8897637795275593,"color":"rgba(55,126,184,1)"}},"hoveron":"points","name":"2","legendgroup":"2","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[31.602251978265834,11.062422147874997,11.696215693078841],"y":[-0.045592414254554736,-0.90465376559943556,-1.4316256665352705],"text":["PC1: 31.602251978<br />PC2: -0.045592414<br />cluster: 3","PC1: 11.062422148<br />PC2: -0.904653766<br />cluster: 3","PC1: 11.696215693<br />PC2: -1.431625667<br />cluster: 3"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(77,175,74,1)","opacity":1,"size":5.6692913385826778,"symbol":"circle","line":{"width":1.8897637795275593,"color":"rgba(77,175,74,1)"}},"hoveron":"points","name":"3","legendgroup":"3","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[-0.0094458951935649099,1.8789363563757921,0.82472361897464641],"y":[7.8586679615270993,12.317917982463772,10.292555737688152],"text":["PC1: -0.009445895<br />PC2:  7.858667962<br />cluster: 4","PC1:  1.878936356<br />PC2: 12.317917982<br />cluster: 4","PC1:  0.824723619<br />PC2: 10.292555738<br />cluster: 4"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(152,78,163,1)","opacity":1,"size":5.6692913385826778,"symbol":"circle","line":{"width":1.8897637795275593,"color":"rgba(152,78,163,1)"}},"hoveron":"points","name":"4","legendgroup":"4","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[0.64258481987630445,-0.32529379356434607,0.13737953417071827,0.22497301790519519,1.156418519274574,-0.29076831198923225,0.8060007035105996,1.0907394429205339,1.0852497698748742,-0.13491219672867361,0.3922001158636958,0.49098640933023413,1.050984080880806,1.0930863059121709,0.48036608083344778,0.24089042088495838,0.34036193905808382,0.074459083657677755,0.1468288727958344,-0.25693161254633012,0.55889940539357186,0.49756881337000919,0.75528525479841901,2.4617602869946751,-0.0735864573605437,0.20218703624427045,0.056139186058123264,1.1095948400095486,1.2399370397294687,1.1376288703371031,-0.062092972778198155,0.039426173237302152,-0.32576158442806602,0.38813417267633293,0.78174400910153408,0.7165587301464923,0.78051269414040103,0.10650347369307316,0.11159217234214325,-0.019440791707115219,-0.089401453328375874,0.12987171866037028,0.38277995249225544,-0.19279624576881663,0.14718005257820335,-0.20354474430753766,0.79004081497950318,1.1536497437677968,0.11211337296827491,0.091082602158325976,-0.043497466660487441,0.34292806383619506,0.69073547638557442,0.0018771716129827726,0.80132455122385493,-0.1798141455363293,-0.25395989197144248,-0.25754091249745759,0.55733052162563945,-0.45079441775591611,0.42436674308228028,-0.018748971632463113,-0.16911083381275721,0.53161298755138464,0.42030860451505297,-0.41608003369056767,-0.27335537314347041,-0.017069558647780939,-0.04086143268106103,0.59394058029581798,0.77882505526651569,0.28321420796961178,0.18489411766636527,0.16338126792024896,-0.52624950917339064,0.033218616241830909,-0.3691673741374743,1.5813913178342027,0.4105692620869239,0.67946066355150647,0.067353101291000361,4.3492189038484845,1.2907421984930878,1.8679503583636194,6.2220447081639909],"y":[0.13957025345743734,0.13416994362758089,1.1791288203513914,0.25864789077936756,-0.53092376129005869,-0.15005096529591805,-0.595546057110247,-0.34822538508880241,-0.42934718679736056,0.40644602769838106,-0.29963198804709446,-0.11859757790023853,0.11674644934867147,-0.29845796732450913,0.070914142487550125,1.3614169154942612,-0.03291676513745987,-0.15941364326877158,-0.26589887322338696,-0.065116026895662163,-0.36815519881674369,-0.2295986200794658,-0.35772930174701212,-0.10124025191423699,-0.031970635912825007,-0.29303813458134176,-0.02442503158893651,0.090295303817103253,-0.067077366500793739,0.1121248517371269,-0.027565041275196642,-0.57928887244721639,1.2447593689856038,-0.4023216713711153,-0.31641966522978748,-0.42833390364438284,-0.4139185912830291,-0.1456696265576411,-0.30498869712548327,-0.29096306097518609,-0.48053099016967504,-0.4164902486513537,0.49709622373271378,-0.24664868717042943,-0.046180143649067862,-0.34376611285781472,-0.43408179220919768,-0.67608041167086241,-0.49997164810694195,-0.40047697492424889,-0.26668261501944246,-0.4842010209055822,-0.16871721182907967,-0.056733326943433519,-0.39520285855855763,-0.31017576650812501,-0.37526346807103417,-0.40869081683640268,-0.53319065695159573,-0.036186110861176658,-0.6633487558062876,-0.28348218628132488,-0.29057835774367641,2.0750418802484663,0.2419568083770165,0.11647414895112335,-0.41857235089856037,-0.48799326918585423,-0.45406833664973006,-0.54425177829425375,-0.50639429368247313,-0.46384813555343074,-0.62912823374061133,-0.72522945629060698,-0.56214840873428806,-0.62094716050930421,-0.11034695508120047,-1.0641907650020754,-0.49966274352775347,-0.056139105694935243,-0.74654556649491388,-1.9170986054869528,-1.2263008358189094,-0.71788204270124589,-1.9893495886650578],"text":["PC1:  0.642584820<br />PC2:  0.139570253<br />cluster: 5","PC1: -0.325293794<br />PC2:  0.134169944<br />cluster: 5","PC1:  0.137379534<br />PC2:  1.179128820<br />cluster: 5","PC1:  0.224973018<br />PC2:  0.258647891<br />cluster: 5","PC1:  1.156418519<br />PC2: -0.530923761<br />cluster: 5","PC1: -0.290768312<br />PC2: -0.150050965<br />cluster: 5","PC1:  0.806000704<br />PC2: -0.595546057<br />cluster: 5","PC1:  1.090739443<br />PC2: -0.348225385<br />cluster: 5","PC1:  1.085249770<br />PC2: -0.429347187<br />cluster: 5","PC1: -0.134912197<br />PC2:  0.406446028<br />cluster: 5","PC1:  0.392200116<br />PC2: -0.299631988<br />cluster: 5","PC1:  0.490986409<br />PC2: -0.118597578<br />cluster: 5","PC1:  1.050984081<br />PC2:  0.116746449<br />cluster: 5","PC1:  1.093086306<br />PC2: -0.298457967<br />cluster: 5","PC1:  0.480366081<br />PC2:  0.070914142<br />cluster: 5","PC1:  0.240890421<br />PC2:  1.361416915<br />cluster: 5","PC1:  0.340361939<br />PC2: -0.032916765<br />cluster: 5","PC1:  0.074459084<br />PC2: -0.159413643<br />cluster: 5","PC1:  0.146828873<br />PC2: -0.265898873<br />cluster: 5","PC1: -0.256931613<br />PC2: -0.065116027<br />cluster: 5","PC1:  0.558899405<br />PC2: -0.368155199<br />cluster: 5","PC1:  0.497568813<br />PC2: -0.229598620<br />cluster: 5","PC1:  0.755285255<br />PC2: -0.357729302<br />cluster: 5","PC1:  2.461760287<br />PC2: -0.101240252<br />cluster: 5","PC1: -0.073586457<br />PC2: -0.031970636<br />cluster: 5","PC1:  0.202187036<br />PC2: -0.293038135<br />cluster: 5","PC1:  0.056139186<br />PC2: -0.024425032<br />cluster: 5","PC1:  1.109594840<br />PC2:  0.090295304<br />cluster: 5","PC1:  1.239937040<br />PC2: -0.067077367<br />cluster: 5","PC1:  1.137628870<br />PC2:  0.112124852<br />cluster: 5","PC1: -0.062092973<br />PC2: -0.027565041<br />cluster: 5","PC1:  0.039426173<br />PC2: -0.579288872<br />cluster: 5","PC1: -0.325761584<br />PC2:  1.244759369<br />cluster: 5","PC1:  0.388134173<br />PC2: -0.402321671<br />cluster: 5","PC1:  0.781744009<br />PC2: -0.316419665<br />cluster: 5","PC1:  0.716558730<br />PC2: -0.428333904<br />cluster: 5","PC1:  0.780512694<br />PC2: -0.413918591<br />cluster: 5","PC1:  0.106503474<br />PC2: -0.145669627<br />cluster: 5","PC1:  0.111592172<br />PC2: -0.304988697<br />cluster: 5","PC1: -0.019440792<br />PC2: -0.290963061<br />cluster: 5","PC1: -0.089401453<br />PC2: -0.480530990<br />cluster: 5","PC1:  0.129871719<br />PC2: -0.416490249<br />cluster: 5","PC1:  0.382779952<br />PC2:  0.497096224<br />cluster: 5","PC1: -0.192796246<br />PC2: -0.246648687<br />cluster: 5","PC1:  0.147180053<br />PC2: -0.046180144<br />cluster: 5","PC1: -0.203544744<br />PC2: -0.343766113<br />cluster: 5","PC1:  0.790040815<br />PC2: -0.434081792<br />cluster: 5","PC1:  1.153649744<br />PC2: -0.676080412<br />cluster: 5","PC1:  0.112113373<br />PC2: -0.499971648<br />cluster: 5","PC1:  0.091082602<br />PC2: -0.400476975<br />cluster: 5","PC1: -0.043497467<br />PC2: -0.266682615<br />cluster: 5","PC1:  0.342928064<br />PC2: -0.484201021<br />cluster: 5","PC1:  0.690735476<br />PC2: -0.168717212<br />cluster: 5","PC1:  0.001877172<br />PC2: -0.056733327<br />cluster: 5","PC1:  0.801324551<br />PC2: -0.395202859<br />cluster: 5","PC1: -0.179814146<br />PC2: -0.310175767<br />cluster: 5","PC1: -0.253959892<br />PC2: -0.375263468<br />cluster: 5","PC1: -0.257540912<br />PC2: -0.408690817<br />cluster: 5","PC1:  0.557330522<br />PC2: -0.533190657<br />cluster: 5","PC1: -0.450794418<br />PC2: -0.036186111<br />cluster: 5","PC1:  0.424366743<br />PC2: -0.663348756<br />cluster: 5","PC1: -0.018748972<br />PC2: -0.283482186<br />cluster: 5","PC1: -0.169110834<br />PC2: -0.290578358<br />cluster: 5","PC1:  0.531612988<br />PC2:  2.075041880<br />cluster: 5","PC1:  0.420308605<br />PC2:  0.241956808<br />cluster: 5","PC1: -0.416080034<br />PC2:  0.116474149<br />cluster: 5","PC1: -0.273355373<br />PC2: -0.418572351<br />cluster: 5","PC1: -0.017069559<br />PC2: -0.487993269<br />cluster: 5","PC1: -0.040861433<br />PC2: -0.454068337<br />cluster: 5","PC1:  0.593940580<br />PC2: -0.544251778<br />cluster: 5","PC1:  0.778825055<br />PC2: -0.506394294<br />cluster: 5","PC1:  0.283214208<br />PC2: -0.463848136<br />cluster: 5","PC1:  0.184894118<br />PC2: -0.629128234<br />cluster: 5","PC1:  0.163381268<br />PC2: -0.725229456<br />cluster: 5","PC1: -0.526249509<br />PC2: -0.562148409<br />cluster: 5","PC1:  0.033218616<br />PC2: -0.620947161<br />cluster: 5","PC1: -0.369167374<br />PC2: -0.110346955<br />cluster: 5","PC1:  1.581391318<br />PC2: -1.064190765<br />cluster: 5","PC1:  0.410569262<br />PC2: -0.499662744<br />cluster: 5","PC1:  0.679460664<br />PC2: -0.056139106<br />cluster: 5","PC1:  0.067353101<br />PC2: -0.746545566<br />cluster: 5","PC1:  4.349218904<br />PC2: -1.917098605<br />cluster: 5","PC1:  1.290742198<br />PC2: -1.226300836<br />cluster: 5","PC1:  1.867950358<br />PC2: -0.717882043<br />cluster: 5","PC1:  6.222044708<br />PC2: -1.989349589<br />cluster: 5"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(255,127,0,1)","opacity":1,"size":5.6692913385826778,"symbol":"circle","line":{"width":1.8897637795275593,"color":"rgba(255,127,0,1)"}},"hoveron":"points","name":"5","legendgroup":"5","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[-0.89894871769310336,0.24930810319020646,-1.2554999514319687,0.4816604137534653,-0.079411695345085659,-0.57578790542660196,-0.57860891208216969,-0.61452738037741028,0.15076637395659584,-0.77617676481598696,-0.38831355621223934,-0.39058543557697351,-1.0673012734982636,-0.28289634088000731,-0.99949342364659532,-1.2311713559756012,-1.4260095063510381,-1.4980447356409035,-0.026046091218072713,-0.65558451105844007,-1.1836478931979375,-0.15061383116681379,-1.6739390222874477,-1.0778150395741861,-0.94531193365560662,-0.88494464787737404,-0.86099427260913219,0.12905769833315447,0.50920452065781863,-0.31952253674587572,0.028596171691351757,-0.40285648809840635,-0.16827253765030817,-0.29566080214314472,-0.41406842342889721,-0.79245522274979363,-0.004282918391954188,-0.06912552944612893,0.055381327802940222,0.20565457989702898,0.31465143952293595,-0.27888968899548522,0.057833668882374498,-0.62821167585081528,-0.22190342683617076,-1.3295357735658848,-0.033243815000429791,0.27823656534722552,-0.47805457762406434,-1.3351828023668033,-0.79792556062574593,-0.9249594098591668,-1.4806020058688927,-0.081228605774419191,-0.67300581990588138,-0.18460226520321169,-0.45745562949041885,-0.97798193334602701,-0.46980553659397628,-0.4870162895958523,-0.70500245031921427,-0.42449342201765905,-1.2363219331973907,0.51754820143606006,-1.2273397810763442,-0.80291943678419642,0.94615402139613825,-0.037416100285605824],"y":[-0.14590695067055476,-0.057102002377478561,-0.2895509379721688,-0.46621227412906019,0.56623369068487051,-0.20560581313421653,-0.26764946722514849,-0.35403805246428355,0.24047388214839771,-0.46482438149410082,0.24887561526951407,0.25831427343921243,-0.70562990510954937,1.2294161309978053,-0.20719622495317047,-0.44485434372405208,-0.27080290192907908,-0.49587791188464636,-0.27317860114323672,-0.38639397950299792,-0.12090878850341383,-0.28635912569252747,-0.47475837463329479,-0.55101579719823068,-0.60788928923579377,-0.42460802757990773,-0.3396190122908963,-0.47035966094109166,-0.84366977425486356,-0.50078710106020152,-0.61087460359040091,-0.72954133837822632,-0.34364709818824002,-0.26506548708313449,0.58816415190480464,-0.41679152592999347,-0.3966570654931183,-0.83062470949425371,-0.68063333922869673,-0.7368000139407046,-0.91309100944802013,-0.44396717818742248,-0.75502792169724953,-0.31714215900920262,-0.61483797510230898,-0.41034916075381245,-0.74597837698360314,-0.29339469008193808,-0.95069466144733039,-0.5375135160988409,-0.54614544920485131,0.21965839659600456,0.043225781289364003,-0.9066511971761283,-0.49353812909382583,-0.758382493983628,-0.84219958591512412,-0.50194638307043205,-0.039569442727922899,-0.66805389362740497,-0.48727734135200279,-0.33725427980204198,-0.82159455829406935,-0.71119286142941407,-0.50860768516146282,-0.83115586752596715,-0.32833548086205555,-0.79198896579775402],"text":["PC1: -0.898948718<br />PC2: -0.145906951<br />cluster: 6","PC1:  0.249308103<br />PC2: -0.057102002<br />cluster: 6","PC1: -1.255499951<br />PC2: -0.289550938<br />cluster: 6","PC1:  0.481660414<br />PC2: -0.466212274<br />cluster: 6","PC1: -0.079411695<br />PC2:  0.566233691<br />cluster: 6","PC1: -0.575787905<br />PC2: -0.205605813<br />cluster: 6","PC1: -0.578608912<br />PC2: -0.267649467<br />cluster: 6","PC1: -0.614527380<br />PC2: -0.354038052<br />cluster: 6","PC1:  0.150766374<br />PC2:  0.240473882<br />cluster: 6","PC1: -0.776176765<br />PC2: -0.464824381<br />cluster: 6","PC1: -0.388313556<br />PC2:  0.248875615<br />cluster: 6","PC1: -0.390585436<br />PC2:  0.258314273<br />cluster: 6","PC1: -1.067301273<br />PC2: -0.705629905<br />cluster: 6","PC1: -0.282896341<br />PC2:  1.229416131<br />cluster: 6","PC1: -0.999493424<br />PC2: -0.207196225<br />cluster: 6","PC1: -1.231171356<br />PC2: -0.444854344<br />cluster: 6","PC1: -1.426009506<br />PC2: -0.270802902<br />cluster: 6","PC1: -1.498044736<br />PC2: -0.495877912<br />cluster: 6","PC1: -0.026046091<br />PC2: -0.273178601<br />cluster: 6","PC1: -0.655584511<br />PC2: -0.386393980<br />cluster: 6","PC1: -1.183647893<br />PC2: -0.120908789<br />cluster: 6","PC1: -0.150613831<br />PC2: -0.286359126<br />cluster: 6","PC1: -1.673939022<br />PC2: -0.474758375<br />cluster: 6","PC1: -1.077815040<br />PC2: -0.551015797<br />cluster: 6","PC1: -0.945311934<br />PC2: -0.607889289<br />cluster: 6","PC1: -0.884944648<br />PC2: -0.424608028<br />cluster: 6","PC1: -0.860994273<br />PC2: -0.339619012<br />cluster: 6","PC1:  0.129057698<br />PC2: -0.470359661<br />cluster: 6","PC1:  0.509204521<br />PC2: -0.843669774<br />cluster: 6","PC1: -0.319522537<br />PC2: -0.500787101<br />cluster: 6","PC1:  0.028596172<br />PC2: -0.610874604<br />cluster: 6","PC1: -0.402856488<br />PC2: -0.729541338<br />cluster: 6","PC1: -0.168272538<br />PC2: -0.343647098<br />cluster: 6","PC1: -0.295660802<br />PC2: -0.265065487<br />cluster: 6","PC1: -0.414068423<br />PC2:  0.588164152<br />cluster: 6","PC1: -0.792455223<br />PC2: -0.416791526<br />cluster: 6","PC1: -0.004282918<br />PC2: -0.396657065<br />cluster: 6","PC1: -0.069125529<br />PC2: -0.830624709<br />cluster: 6","PC1:  0.055381328<br />PC2: -0.680633339<br />cluster: 6","PC1:  0.205654580<br />PC2: -0.736800014<br />cluster: 6","PC1:  0.314651440<br />PC2: -0.913091009<br />cluster: 6","PC1: -0.278889689<br />PC2: -0.443967178<br />cluster: 6","PC1:  0.057833669<br />PC2: -0.755027922<br />cluster: 6","PC1: -0.628211676<br />PC2: -0.317142159<br />cluster: 6","PC1: -0.221903427<br />PC2: -0.614837975<br />cluster: 6","PC1: -1.329535774<br />PC2: -0.410349161<br />cluster: 6","PC1: -0.033243815<br />PC2: -0.745978377<br />cluster: 6","PC1:  0.278236565<br />PC2: -0.293394690<br />cluster: 6","PC1: -0.478054578<br />PC2: -0.950694661<br />cluster: 6","PC1: -1.335182802<br />PC2: -0.537513516<br />cluster: 6","PC1: -0.797925561<br />PC2: -0.546145449<br />cluster: 6","PC1: -0.924959410<br />PC2:  0.219658397<br />cluster: 6","PC1: -1.480602006<br />PC2:  0.043225781<br />cluster: 6","PC1: -0.081228606<br />PC2: -0.906651197<br />cluster: 6","PC1: -0.673005820<br />PC2: -0.493538129<br />cluster: 6","PC1: -0.184602265<br />PC2: -0.758382494<br />cluster: 6","PC1: -0.457455629<br />PC2: -0.842199586<br />cluster: 6","PC1: -0.977981933<br />PC2: -0.501946383<br />cluster: 6","PC1: -0.469805537<br />PC2: -0.039569443<br />cluster: 6","PC1: -0.487016290<br />PC2: -0.668053894<br />cluster: 6","PC1: -0.705002450<br />PC2: -0.487277341<br />cluster: 6","PC1: -0.424493422<br />PC2: -0.337254280<br />cluster: 6","PC1: -1.236321933<br />PC2: -0.821594558<br />cluster: 6","PC1:  0.517548201<br />PC2: -0.711192861<br />cluster: 6","PC1: -1.227339781<br />PC2: -0.508607685<br />cluster: 6","PC1: -0.802919437<br />PC2: -0.831155868<br />cluster: 6","PC1:  0.946154021<br />PC2: -0.328335481<br />cluster: 6","PC1: -0.037416100<br />PC2: -0.791988966<br />cluster: 6"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(255,255,51,1)","opacity":1,"size":5.6692913385826778,"symbol":"circle","line":{"width":1.8897637795275593,"color":"rgba(255,255,51,1)"}},"hoveron":"points","name":"6","legendgroup":"6","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":43.762557077625573,"r":7.3059360730593621,"b":40.182648401826491,"l":37.260273972602747},"font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724},"title":{"text":"Interactive scatterplot of PC1 and PC2 colored by cluster for Guediawaye Census 2002","font":{"color":"rgba(0,0,0,1)","family":"","size":17.534246575342465},"x":0,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-4.9743146185771483,33.343993244782169],"tickmode":"array","ticktext":["0","10","20","30"],"tickvals":[0,10,20,30],"categoryorder":"array","categoryarray":["0","10","20","30"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.68949771689498},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176002,"zeroline":false,"anchor":"y","title":{"text":"PC1","font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-2.7047129672214991,13.033281361020213],"tickmode":"array","ticktext":["0","4","8","12"],"tickvals":[0,4,8,12],"categoryorder":"array","categoryarray":["0","4","8","12"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.68949771689498},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176002,"zeroline":false,"anchor":"x","title":{"text":"PC2","font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":true,"legend":{"bgcolor":null,"bordercolor":null,"borderwidth":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.68949771689498},"title":"Cluster","orientation":"h","y":-0.14999999999999999,"x":0.20000000000000001},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"240c617058fb":{"x":{},"y":{},"colour":{},"type":"scatter"}},"cur_data":"240c617058fb","visdat":{"240c617058fb":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```


## Spatial clustering & regionalization


```r
library(spdep)

input_vars <- dfw_pca_2013 %>%
  select(PC1:PC8) %>%
  st_drop_geometry() %>%
  as.data.frame() 

skater_nbrs <- poly2nb(dfw_pca_2013, queen = TRUE)
costs <- nbcosts(skater_nbrs, input_vars)
skater_weights <- nb2listw(skater_nbrs, costs, style = "B")
```


```r
mst <- mstree(skater_weights)

regions <- skater(
  mst[,1:2], 
  input_vars, 
  ncuts = 5,
  crit = 10
)
```



```r
dfw_clusters_2013$region <- as.character(regions$group)

ggplot(dfw_clusters_2013, aes(fill = region)) + 
  geom_sf(size = 0.1) + 
  labs(title = "Map of contiguous regions derived with the SKATER algorithm for Guediawaye Census 2013") +
  scale_fill_brewer(palette = "Set3")+
  theme_void()
```

![](05_Modelling-Senegal-Census-data_files/figure-html/unnamed-chunk-56-1.png)<!-- -->

```r
input_vars <- dfw_pca_2002 %>%
  select(PC1:PC8) %>%
  st_drop_geometry() %>%
  as.data.frame() 

skater_nbrs <- poly2nb(dfw_pca_2002, queen = TRUE)
costs <- nbcosts(skater_nbrs, input_vars)
skater_weights <- nb2listw(skater_nbrs, costs, style = "B")
```


```r
mst <- mstree(skater_weights)

regions <- skater(
  mst[,1:2], 
  input_vars, 
  ncuts = 5,
  crit = 10
)
```



```r
dfw_clusters_2002$region <- as.character(regions$group)

ggplot(dfw_clusters_2002, aes(fill = region)) + 
  geom_sf(size = 0.1) + 
  labs(title = "Map of contiguous regions derived with the SKATER algorithm for Guediawaye Census 2002") +
  scale_fill_brewer(palette = "Set3")+ 
  theme_void()
```

![](05_Modelling-Senegal-Census-data_files/figure-html/unnamed-chunk-59-1.png)<!-- -->



```r
dfw_clusters <- dfw_clusters_2002 %>%
  dplyr::mutate(RGPH = "Census 2002") %>% 
  plyr::rbind.fill(dfw_clusters_2013 %>%
  dplyr::mutate(RGPH = "Census 2013")) %>% 
  st_as_sf()


dfw_clusters %>% 
    
      ggplot(aes(fill = region)) + 
      
  geom_sf(size = 0.1) + 
  scale_fill_brewer(palette = "Set3")+
      facet_wrap(~RGPH) +
      
      # Adjusting the plot theme
      theme_map(base_size = 8) +
      theme(panel.background = element_rect(),
            #legend.background = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            legend.position = "bottom", 
            text = element_text(size = 8), 
            #panel.grid = element_line(color = "white", size = 0.8),
            legend.box.background = element_rect(),
            legend.box.margin = margin(6, 6, 6, 6),
            plot.background = element_rect(fill = "white"))+
  ggspatial::annotation_scale(
    location = "br",
    bar_cols = c("grey60", "white"),
    text_family = "ArcherPro Book"
  ) +
  ggspatial::annotation_north_arrow(
    location = "tl", which_north = "true",
    #pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    #which_north = "grid",
  height = unit(1, "cm"),
  width = unit(1, "cm"),
  )
```

![](05_Modelling-Senegal-Census-data_files/figure-html/unnamed-chunk-60-1.png)<!-- -->

```r
ggsave(paste0(here::here(),"/output/output_model/img/14_dfw_pca_rgph.png"), width = 8, height = 5)
```


```r
# Saving the model output
# Saving in the "dfw_clusters"
sf::st_write(obj=dfw_clusters, paste0(here::here(), "/output/output_model/shp/dfw_clusters.shp"),  driver = "ESRI Shapefile", delete_layer = TRUE) 
```

```
## Writing layer `dfw_clusters' to data source 
##   `C:/Users/ASUS/Desktop/projet_sn/Analysing-Senegal-Census-Data/output/output_model/shp/dfw_clusters.shp' using driver `ESRI Shapefile'
## Writing 703 features with 15 fields and geometry type Polygon.
```

