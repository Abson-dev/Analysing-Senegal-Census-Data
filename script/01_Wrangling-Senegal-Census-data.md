---
title: "Wrangling Senegal Census data with tidyverse tools"
author: "IBRAHIM KASSOUM Habibou & HEMA Aboubacar"
date: "2024-01-13"
output: 
  html_document:
    toc: true
    toc_depth: 3
    toc_float: true
    number_sections: true
    code_folding: show
    keep_md: true
---



# Preambule: 


```r
# Clear the current workspace by removing all objects.
# This is useful to start with a clean slate and avoid potential conflicts or unexpected behavior.
rm(list=ls())
```

## Importing library

```r
## Importing library
### List of required packages
required_packages <- c("tidyverse","janitor" ,"readr","dplyr","haven","sf",
                       "flextable","sp", "factoextra", "FactoMineR","gtsummary", "sjPlot")

# Check if packages are installed
missing_packages <- setdiff(required_packages, installed.packages()[,"Package"])

### Install missing packages
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

### Load all packages
lapply(required_packages, library, character.only = TRUE)
```


## Importing the datasets an checking for duplicated row

### Importing the shapefile data


```r
# Read shapefile data for 2002 and 2013
sp_rgph_2002 <- sf::read_sf(paste0(here::here(),"/data/EAs_2013_2002_SHP/EAs_2002.shp"))
# Mutating the 'sp_rgph_2002' dataset to create a new 'id_dr' column based on conditions
sp_rgph_2002 <- sp_rgph_2002 %>% 
  mutate(id_dr = ifelse(NUMDEPT=="3", 
                      paste0(NUM_REG, NUMDEPT, NUM_ARRDT, "18", NUM_CA, IDDR, "O"), 
                      paste0("0", NUM_REG, NUMDEPT, NUM_ARRDT, "8", NUM_CA, IDDR, "O")))
sp_rgph_2013 <- sf::read_sf(paste0(here::here(),"/data/EAs_2013_2002_SHP/EAs_2013.shp"))

# Display the first 10 rows of the 2002 and 2013 shapefiles
head(sp_rgph_2002, 10L)
head(sp_rgph_2013, 10L)
```

### Importing the census datasets 

**For the 2002 census data**


```r
# Read and check for duplicated rows in 2002 datasets

# Read and check for duplicated rows in the "deces_2002" dataset
deces_2002 <- read_sav(paste0(here::here(),"/data/1. RGPH 2002/Requete PDBA2k2 Deces.sav"))
deces_2002 %>% janitor::get_dupes() 

# Read and check for duplicated rows in the "deces_complement" dataset
deces_2002_complement <- read_sav(paste0(here::here(),"/data/1. RGPH 2002/PDBA_rgph2002_Deces Complement Pikine.sav"))
deces_2002_complement %>% janitor::get_dupes() 

# Read and check for duplicated rows in the "individus_2002" dataset
individus_2002 <- haven::read_sav(paste0(here::here(),"/data/1. RGPH 2002/Requete PDBA2k2 Individus.v2.sav"))
individus_2002 %>% janitor::get_dupes() 

# Read and check for duplicated rows in the "menage_2002" dataset
menage_2002 <- haven::read_sav(paste0(here::here(),"/data/1. RGPH 2002/Requete PDBA2k2 menage.sav"))
menage_2002 %>% janitor::get_dupes()
menage_2002 %>% janitor::get_dupes(ID_MENAGE)

# Read and check for duplicated rows in the "menage_2002_complement" dataset
menage_2002_complement <- read_sav(paste0(here::here(),"/data/1. RGPH 2002/PDBA_rgph2002_menage_complement.sav"))
menage_2002_complement %>% janitor::get_dupes() 

# Read and check for duplicated rows in the "individus_2002_complement_pikine" dataset
individus_2002_complement_pikine <- read_sav(paste0(here::here(),"/data/1. RGPH 2002/PDBA_rgph2002_Individus Complement Pikine.sav"))
individus_2002_complement_pikine %>% janitor::get_dupes() 
```




```r
# Joining the 2002 dataset with the "deces_2002_complement" for deceased individuals
deces_2002 <- deces_2002_complement %>% 
  # Creating unique identifiers for each record in the dataset
  mutate(ID_MENAGE = paste0(QA01, QA02, QA03, QA04, QA05, QA06, "0", QA07, QA072_CLE, QA08, QA09, QA10),
         DR = paste0(QA01, QA02, QA03, QA04, QA05, QA06, "0", QA07, QA072_CLE)) %>%
  # Combining the dataset with the existing "deces_2002" dataset, filling missing values
  plyr::rbind.fill(deces_2002)

# Joining with the 2002 dataset for individual data
individus_2002 <- individus_2002_complement_pikine %>% 
  # Creating unique identifiers for each record in the dataset
  mutate(ID_MENAGE = paste0(QA01, QA02, QA03, QA04, QA05, QA06, "0", QA07, QA072_CLE, QA08, QA09, QA10),
         DR = paste0(QA01, QA02, QA03, QA04, QA05, QA06, "0", QA07, QA072_CLE)) %>%
  # Combining the dataset with the existing "individus_2002" dataset
  plyr::rbind.fill(individus_2002)

# Joining with the 2002 dataset for household data
menage_2002 <- menage_2002_complement %>% 
  # Creating unique identifiers for each record in the dataset
  mutate(ID_MENAGE = paste0(QA01, QA02, QA03, QA04, QA05, QA06, "0", QA07, QA072_CLE, QA08, QA09, QA10),
         DR = paste0(QA01, QA02, QA03, QA04, QA05, QA06, "0", QA07, QA072_CLE)) %>%
  # Combining the dataset with the existing "menage_2002" dataset
  plyr::rbind.fill(menage_2002)
```



**For the 2013 census dataset**


```r
# Read and check for duplicated rows in 2013 datasets

# Read and check for duplicated rows in the "deces_2013" dataset
deces_2013 <- read_sav(paste0(here::here(),"/data/4. RGPH 2013/Requete PDBA2k13 Deces_Guediawaye.sav"))
deces_2013 %>% janitor::get_dupes() 
```

```
## # A tibble: 0 × 21
## # ℹ 21 variables: ID_CONCESSION <chr>, ID_MENAGE <chr>, IDDR <chr>, A08 <chr>,
## #   A09 <chr>, A01 <dbl+lbl>, A02 <dbl>, A03 <dbl>, A04A <dbl>, A04B <dbl>,
## #   A05 <chr>, A06 <chr>, A07 <chr>, A07HAM <chr>, A10 <dbl+lbl>,
## #   A11 <dbl+lbl>, C01 <dbl+lbl>, cacr <dbl+lbl>, typ_pop <dbl+lbl>,
## #   coef2 <dbl>, dupe_count <int>
```

```r
# Read and check for duplicated rows in the "individus_2013" dataset
individus_2013 <- haven::read_sav(paste0(here::here(),"/data/4. RGPH 2013/Requete PDBA2k13 Indidivus GUEDIAWAYE.sav"))
individus_2013 %>% janitor::get_dupes() 
```

```
## # A tibble: 0 × 78
## # ℹ 78 variables: ID_CONCESSION <chr>, ID_MENAGE <chr>, B01 <chr>,
## #   B04 <dbl+lbl>, IDDR <chr>, A01 <dbl+lbl>, A02 <dbl>, A03 <dbl>, A04A <dbl>,
## #   A04B <dbl>, A05 <chr>, A06 <chr>, A07 <chr>, A07HAM <chr>, A08 <chr>,
## #   A09 <chr>, A10 <dbl+lbl>, A11 <dbl+lbl>, B06 <dbl+lbl>, B09 <dbl+lbl>,
## #   B09_AUTRES_PRECISER <chr>, B10 <dbl+lbl>, B11 <dbl+lbl>, B12 <dbl+lbl>,
## #   B12_AUTRES_PRECISER <chr>, B13 <dbl+lbl>, B13_AUTRES_PRECISER <chr>,
## #   B14 <dbl+lbl>, B15 <dbl+lbl>, B15_AUTRES_PRECISER <chr>, B16 <dbl+lbl>, …
```

```r
# Read and check for duplicated rows in the "menage_2013" dataset
menage_2013 <- haven::read_sav(paste0(here::here(),"/data/4. RGPH 2013/Requete PDBA2k13 menage.sav"))
menage_2013 %>% janitor::get_dupes() 
```

```
## # A tibble: 0 × 77
## # ℹ 77 variables: ID_CONCESSION <chr>, ID_MENAGE <chr>, A01 <dbl+lbl>,
## #   A02 <dbl>, A03 <dbl>, A04A <dbl>, IDDR <chr>, A04B <dbl>, A05 <chr>,
## #   A06 <chr>, Nbre_indiv <dbl>, A10 <dbl+lbl>, A11 <dbl+lbl>, E01 <dbl+lbl>,
## #   E02 <dbl>, E03 <dbl+lbl>, E04 <dbl+lbl>, E05 <dbl+lbl>, E06 <dbl+lbl>,
## #   E07 <dbl+lbl>, E08 <dbl+lbl>, E09 <dbl+lbl>, E10 <dbl+lbl>, E11 <dbl+lbl>,
## #   E12 <dbl+lbl>, E13_1 <dbl+lbl>, E13_2 <dbl+lbl>, E13_3 <dbl+lbl>,
## #   E13_4 <dbl+lbl>, E13_5 <dbl+lbl>, E13_6 <dbl+lbl>, E13_7 <dbl+lbl>, …
```

```r
menage_2013 %>% janitor::get_dupes(ID_MENAGE)
```

```
## # A tibble: 0 × 77
## # ℹ 77 variables: ID_MENAGE <chr>, dupe_count <int>, ID_CONCESSION <chr>,
## #   A01 <dbl+lbl>, A02 <dbl>, A03 <dbl>, A04A <dbl>, IDDR <chr>, A04B <dbl>,
## #   A05 <chr>, A06 <chr>, Nbre_indiv <dbl>, A10 <dbl+lbl>, A11 <dbl+lbl>,
## #   E01 <dbl+lbl>, E02 <dbl>, E03 <dbl+lbl>, E04 <dbl+lbl>, E05 <dbl+lbl>,
## #   E06 <dbl+lbl>, E07 <dbl+lbl>, E08 <dbl+lbl>, E09 <dbl+lbl>, E10 <dbl+lbl>,
## #   E11 <dbl+lbl>, E12 <dbl+lbl>, E13_1 <dbl+lbl>, E13_2 <dbl+lbl>,
## #   E13_3 <dbl+lbl>, E13_4 <dbl+lbl>, E13_5 <dbl+lbl>, E13_6 <dbl+lbl>, …
```



# Preparation of data on the two censuses (Wrangling)


```r
rm(list = c("deces_2002_complement","individus_2002_complement_pikine","menage_2002_complement","missing_packages","required_packages"))
```



```r
# Creating a variable "RGPH" to identify each survey 

# For 2002
## Add a new column "RGPH" with the value "2002" to the "individus_2002" dataset
individus_2002 <- 
  individus_2002 %>% 
  mutate(RGPH = "2002") %>% 
  mutate(id_dr = DR)

# For 2013
## Add a new column "RGPH" with the value "2013" to the "individus_2013" dataset
individus_2013 <- 
  individus_2013 %>% 
  mutate(RGPH = "2013") %>% 
  mutate(id_dr = IDDR)

## For RGPH 2013
individus_2002 <- individus_2002 %>%
 select(id_dr,ID_MENAGE, RGPH, everything())# 

## For RGPH 2002
individus_2013 <- individus_2013 %>%
 select(id_dr,ID_MENAGE, RGPH, everything())# 
```

# Study Variables

## Socio-demographic variables

### Sex/gender


```r
#For the RGPH 2002 dataset
menage_2002 <- menage_2002 %>% 
  left_join(
    individus_2002 %>% 
  group_by(ID_MENAGE) %>% 
  summarise(
    homme = sum(QB04 == 1, na.rm = TRUE),
    femme = sum(QB04 == 2, na.rm = TRUE),
    hh_size = homme + femme
  ) %>% 
  ungroup() %>% 
  mutate(
    homme = as.numeric(homme) %>%
           structure(label = "Nombre d'homme dans le ménage"),
    femme = as.numeric(femme) %>%
           structure(label = "Nombre de femmes dans le ménage"),
    hh_size = as.numeric(hh_size) %>%
           structure(label = "Taille du ménage")),
      
      # Merging by matching columns in both datasets
      by = c("ID_MENAGE"))

#1 = Male, 2 = Female
#For the RGPH 2013 dataset

#1 = Male, 2 = Female
menage_2013 <- menage_2013 %>% 
  left_join( 
    individus_2013 %>% 
  group_by(ID_MENAGE) %>% 
  summarise(
    homme = sum(B06 == 1, na.rm = TRUE),
    femme = sum(B06 == 2, na.rm = TRUE),
    hh_size = homme + femme
  ) %>% 
  ungroup() %>% 
  mutate(
    homme = as.numeric(homme) %>%
           structure(label = "Nombre d'homme dans le ménage"),
    femme = as.numeric(femme) %>%
           structure(label = "Nombre de femmes dans le ménage"),
    hh_size = as.numeric(hh_size) %>%
           structure(label = "Taille du ménage")),
      
      # Merging by matching columns in both datasets
      by = c("ID_MENAGE"))

gender_var = c("homme","femme","hh_size")
```


### Age




### Marital Status


```r
#For the RGPH 2002 dataset

     # 0         Monogame
     # 1     Polygame à 2
     # 2     Polygame à 3
     # 3 Polygame à 4 et+
     # 4      Célibataire
     # 5     Veuf / Veuve
     # 6          Divorcé
     # 7            Autre
menage_2002 <- menage_2002 %>% 
  left_join(
    individus_2002 %>% 
  group_by(ID_MENAGE) %>% 
  summarise(
    Monogame = sum(QB20 == 0, na.rm = TRUE),
    Polygame = sum(QB20 == 1 | QB20 == 2 | QB20 == 3 , na.rm = TRUE),
    Celibataire = sum(QB20 == 4, na.rm = TRUE),
    Veuf = sum(QB20 == 5, na.rm = TRUE),
    Divorce = sum(QB20 == 6, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  mutate(
    Monogame = as.numeric(Monogame) %>%
           structure(label = "Nombre de monogame dans le ménage"),
    Polygame = as.numeric(Polygame) %>%
           structure(label = "Nombre de polygame dans le ménage"),
    Celibataire = as.numeric(Celibataire) %>%
           structure(label = "Nombre de celibataire dans le ménage"),
    Veuf = as.numeric(Veuf) %>%
           structure(label = "Nombre de veuf/veuve dans le ménage"),
    Divorce = as.numeric(Divorce) %>%
           structure(label = "Nombre de divorce dans le ménage")
    ),
      
      # Merging by matching columns in both datasets
      by = c("ID_MENAGE"))
#For the RGPH 2013 dataset
    #  0                   Monogame
    #  1           Poly/1ère épouse
    #  2 Poly/2ié  épouse/2 épouses
    #  3  Poly/3iè épouse/3 épouses
    #  4 Poly/4iè épouse /4 épouses
    #  5 Poly/5iè épouse /5 épouses
    #  6                Célibataire
    #  7                 Veuf/Veuve
    #  8                 Divorcé(e)
    #  9   Union libre(concubinage)
    # 10                  Séparé(e)
menage_2013 <- menage_2013 %>% 
  left_join(
    individus_2013 %>% 
      group_by(ID_MENAGE) %>% 
      summarise(
        Monogame = sum(B41 == 0, na.rm = TRUE),
        Polygame = sum(B41 == 1 | B41 == 2 | B41 == 3 | B41 == 4 | B41 == 5 , na.rm = TRUE),
        Celibataire = sum(B41 == 6 | B41 == 9 , na.rm = TRUE),
        Veuf = sum(B41 == 7, na.rm = TRUE),
        Divorce = sum(B41 == 8 | B41 == 10 , na.rm = TRUE)
      ) %>% 
      ungroup() %>% 
      mutate(
        Monogame = as.numeric(Monogame) %>%
          structure(label = "Nombre de monogame dans le ménage"),
        Polygame = as.numeric(Polygame) %>%
          structure(label = "Nombre de polygame dans le ménage"),
        Celibataire = as.numeric(Celibataire) %>%
          structure(label = "Nombre de celibataire dans le ménage"),
        Veuf = as.numeric(Veuf) %>%
          structure(label = "Nombre de veuf/veuve dans le ménage"),
        Divorce = as.numeric(Divorce) %>%
          structure(label = "Nombre de divorce dans le ménage")
      ),
    
    # Merging by matching columns in both datasets
    by = c("ID_MENAGE"))
mat_status_var = c("Monogame","Polygame","Celibataire","Veuf","Divorce")
```


### Fertility


```r
#For the RGPH 2002 dataset
ferti_2002 = c("QB21M","QB21F","QB22M","QB22F","QB23M","QB23F")

menage_2002 <- menage_2002 %>% 
  left_join(
    individus_2002 %>% 
  
  mutate(
    fertility = rowSums(select(., .dots=all_of(ferti_2002)),na.rm = T)
  ) %>% 
    group_by(ID_MENAGE) %>% 
  
  summarise(
    fertility = sum(fertility,na.rm = TRUE) %>%
           structure(label = "Fertilité dans le ménage")
    ) , 
      
      # Merging by matching columns in both datasets
      by = c("ID_MENAGE"))
#For the RGPH 2013 dataset

ferti_2013 = c("B43","B44","B45","B46","B47","B48")
menage_2013 <- menage_2013 %>% 
  left_join(
    individus_2013 %>% 
  
  mutate(
    fertility = rowSums(select(., .dots=all_of(ferti_2013)),na.rm = T)
  ) %>% 
    group_by(ID_MENAGE) %>% 
  
  summarise(
    fertility = sum(fertility,na.rm = TRUE) %>%
           structure(label = "Fertilité dans le ménage")
    ) , 
      
      # Merging by matching columns in both datasets
      by = c("ID_MENAGE"))
fertility_var = c("fertility")
```


### Educational level


```r
#For the RGPH 2002 dataset
#QB15 | educ
     # 0      Aucun
     # 1   Primaire
     # 2      Moyen
     # 3 Secondaire
     # 4  Supérieur
     # 9   manquant
menage_2002 <- menage_2002 %>% 
  left_join(
    individus_2002 %>% 
      group_by(ID_MENAGE) %>% 
      summarise(
        Aucun = sum(educ == 0, na.rm = TRUE),
        Primaire = sum(educ == 1, na.rm = TRUE),
        Moyen = sum(educ == 2, na.rm = TRUE),
        Secondaire = sum(educ == 3, na.rm = TRUE),
        Superieur = sum(educ == 4, na.rm = TRUE)
      ) %>% 
      ungroup() %>% 
      mutate(
        Aucun = as.numeric(Aucun) %>%
          structure(label = "Nombre de Aucun dans le ménage"),
        Primaire = as.numeric(Primaire) %>%
          structure(label = "Nombre de Primaire dans le ménage"),
        Moyen = as.numeric(Moyen) %>%
          structure(label = "Nombre de Moyen dans le ménage"),
        Secondaire = as.numeric(Secondaire) %>%
          structure(label = "Nombre de Secondaire dans le ménage"),
        Superieur = as.numeric(Superieur) %>%
          structure(label = "Nombre de Supérieur dans le ménage")
      ),
    
    # Merging by matching columns in both datasets
    by = c("ID_MENAGE"))
#For the RGPH 2013 dataset

individus_2013 <- individus_2013 %>% 
  
  mutate(educ = case_when(
    
    B32 %in% seq(1,3,1)  ~ 1, # If B32 is in pre-school levels, set it to "prescolaire" for c("Petite section","Moyenne section", "Grnde section")
    
    B32 %in% seq(4,9,1) ~ 1, # If B32 corresponds to elementary levels, set it to "elementaire" for c("Cours d'initiation (CI)", "Cours préparatoire (CP)", "Cours élémentaire 1ère année (CE1)", "Cours élémentaire 2ème année (CE2)", "Cours moyen 1ère année (CM1)", "Cours moyen 2ème année (CM2)"
    
    B32 %in% seq(10,13,1) ~ 2, # If B32 corresponds to middle school levels, set it to "moyen" for c("Sixième (6ième)", "Cinquième (5ième)", "Quatrième (4ième)", "Troisième (3ième)")
    
    B32 %in% seq(14,16,1) ~ 3, # If B32 corresponds to high school levels, set it to "secondaire" for c("Seconde", "Première", "Terminale")
    
    B32 %in% seq(17,24,1) ~ 4, # If B32 corresponds to higher education levels, set it to "superieur" c("1ère année", "2ème année", "Licence/3ième année", "Maitrise/4ième année", "DEA/5ième année", "6ième année", "7ième année", "8ième année et +")
    TRUE ~ as.numeric(B32) %>% 
  structure(label = Hmisc::label(individus_2013$B32)))) # Apply labels to the B32 column

menage_2013 <- menage_2013 %>% 
  left_join(
    individus_2013 %>% 
      group_by(ID_MENAGE) %>% 
      summarise(
        Aucun = sum(educ == 0, na.rm = TRUE),
        Primaire = sum(educ == 1, na.rm = TRUE),
        Moyen = sum(educ == 2, na.rm = TRUE),
        Secondaire = sum(educ == 3, na.rm = TRUE),
        Superieur = sum(educ == 4, na.rm = TRUE)
      ) %>% 
      ungroup() %>% 
      mutate(
        Aucun = as.numeric(Aucun) %>%
          structure(label = "Nombre de Aucun dans le ménage"),
        Primaire = as.numeric(Primaire) %>%
          structure(label = "Nombre de Primaire dans le ménage"),
        Moyen = as.numeric(Moyen) %>%
          structure(label = "Nombre de Moyen dans le ménage"),
        Secondaire = as.numeric(Secondaire) %>%
          structure(label = "Nombre de Secondaire dans le ménage"),
        Superieur = as.numeric(Superieur) %>%
          structure(label = "Nombre de Supérieur dans le ménage")
      ),
    
    # Merging by matching columns in both datasets
    by = c("ID_MENAGE"))

educ_var = c("Aucun","Primaire","Moyen","Secondaire","Superieur")
```


### Literacy level


```r
#For the RGPH 2002 dataset
#QB16A,QB16B,QB16C,QB16D,QB16E,QB16F,QB16G,QB16H,QB16I,QB16J
#1-None, 2-French, 3-Arabic, 4-Wolof, 5-Pulaar, 6-Sereer, 7-Mandingo, 8-Diola, 9 -Soninke, 10-Other Language.
menage_2002 <- menage_2002 %>% 
  left_join(
    individus_2002 %>% 
      group_by(ID_MENAGE) %>% 
      summarise(
        Literacy_None = sum(QB16A == 1, na.rm = TRUE),
        Literacy_French = sum(QB16B == 1, na.rm = TRUE),
        Literacy_Arabic = sum(QB16C == 1, na.rm = TRUE),
        Literacy_Wolof = sum(QB16D == 1, na.rm = TRUE),
        Literacy_Pulaar = sum(QB16E == 1, na.rm = TRUE),
        Literacy_Sereer = sum(QB16F == 1, na.rm = TRUE),
        Literacy_Mandingo = sum(QB16G == 1, na.rm = TRUE),
        Literacy_Diola = sum(QB16H == 1, na.rm = TRUE),
        Literacy_Soninke = sum(QB16I == 1, na.rm = TRUE),
        Literacy_Other = sum(QB16J == 1, na.rm = TRUE)
      ) %>% 
      ungroup() %>% 
      mutate(
        Literacy_None = as.numeric(Literacy_None) %>%
          structure(label = "Nombre de Aucun dans le ménage"),
        Literacy_French = as.numeric(Literacy_French) %>%
          structure(label = "Nombre de French dans le ménage"),
        Literacy_Arabic = as.numeric(Literacy_Arabic) %>%
          structure(label = "Nombre de Arabic dans le ménage"),
        Literacy_Wolof = as.numeric(Literacy_Wolof) %>%
          structure(label = "Nombre de Wolof dans le ménage"),
        Literacy_Pulaar = as.numeric(Literacy_Pulaar) %>%
          structure(label = "Nombre de Pulaar dans le ménage"),
        Literacy_Sereer = as.numeric(Literacy_Sereer) %>%
          structure(label = "Nombre de Sereer dans le ménage"),
        Literacy_Mandingo = as.numeric(Literacy_Mandingo) %>%
          structure(label = "Nombre de Mandingo dans le ménage"),
        Literacy_Diola = as.numeric(Literacy_Diola) %>%
          structure(label = "Nombre de Diola dans le ménage"),
        Literacy_Soninke = as.numeric(Literacy_Soninke) %>%
          structure(label = "Nombre de Soninke dans le ménage"),
        Literacy_Other = as.numeric(Literacy_Other) %>%
          structure(label = "Nombre de Other dans le ménage")
      ),
    
    # Merging by matching columns in both datasets
    by = c("ID_MENAGE"))
#For the RGPH 2013 dataset
#B34_FR,B34_AR,B34_WO,B34_PU,B34_SE,B34_MA,B34_DI,B34_SO
menage_2013 <- menage_2013 %>% 
  left_join(
    individus_2013 %>% 
      group_by(ID_MENAGE) %>% 
      summarise(
        Literacy_French = sum(B34_FR == 1, na.rm = TRUE),
        Literacy_Arabic = sum(B34_AR == 1, na.rm = TRUE),
        Literacy_Wolof = sum(B34_WO == 1, na.rm = TRUE),
        Literacy_Pulaar = sum(B34_PU == 1, na.rm = TRUE),
        Literacy_Sereer = sum(B34_SE == 1, na.rm = TRUE),
        Literacy_Mandingo = sum(B34_MA == 1, na.rm = TRUE),
        Literacy_Diola = sum(B34_DI == 1, na.rm = TRUE),
        Literacy_Soninke = sum(B34_SO == 1, na.rm = TRUE)
      ) %>% 
      ungroup() %>% 
      mutate(
        Literacy_French = as.numeric(Literacy_French) %>%
          structure(label = "Nombre de French dans le ménage"),
        Literacy_Arabic = as.numeric(Literacy_Arabic) %>%
          structure(label = "Nombre de Arabic dans le ménage"),
        Literacy_Wolof = as.numeric(Literacy_Wolof) %>%
          structure(label = "Nombre de Wolof dans le ménage"),
        Literacy_Pulaar = as.numeric(Literacy_Pulaar) %>%
          structure(label = "Nombre de Pulaar dans le ménage"),
        Literacy_Sereer = as.numeric(Literacy_Sereer) %>%
          structure(label = "Nombre de Sereer dans le ménage"),
        Literacy_Mandingo = as.numeric(Literacy_Mandingo) %>%
          structure(label = "Nombre de Mandingo dans le ménage"),
        Literacy_Diola = as.numeric(Literacy_Diola) %>%
          structure(label = "Nombre de Diola dans le ménage"),
        Literacy_Soninke = as.numeric(Literacy_Soninke) %>%
          structure(label = "Nombre de Soninke dans le ménage")
      ),
    
    # Merging by matching columns in both datasets
    by = c("ID_MENAGE"))
literacy_var = c("Literacy_French","Literacy_Arabic","Literacy_Wolof","Literacy_Pulaar","Literacy_Sereer","Literacy_Mandingo","Literacy_Diola","Literacy_Soninke")
```

### Migration variable


```r
#For the RGPH 2002 dataset
#the place of residence 5 years ago was also asked for residents present, absent or for visitors.

     # 1 Resident present
     # 2  Resident absent
     # 3         Visiteur
menage_2002 <- menage_2002 %>% 
  left_join(
    individus_2002 %>% 
      group_by(ID_MENAGE) %>% 
      summarise(
        resident_pres = sum(QB08 == 1, na.rm = TRUE),
        resident_abs = sum(QB08 == 2, na.rm = TRUE),
        resident_vis = sum(QB08 == 3, na.rm = TRUE)
      ) %>% 
      ungroup() %>% 
      mutate(
        resident_pres = as.numeric(resident_pres) %>%
          structure(label = "Nombre de Résident Présent dans le ménage"),
        resident_abs = as.numeric(resident_abs) %>%
          structure(label = "Nombre de Résident Absent dans le ménage"),
        resident_vis = as.numeric(resident_vis) %>%
          structure(label = "Nombre de Visiteur dans le ménage")
      ),
    
    # Merging by matching columns in both datasets
    by = c("ID_MENAGE"))
#For the RGPH 2013 dataset

     # 1 Résident Présent
     # 2  Résident Absent
     # 3         Visiteur
menage_2013 <- menage_2013 %>% 
  left_join(
    individus_2013 %>% 
      group_by(ID_MENAGE) %>% 
      summarise(
        resident_pres = sum(B11 == 1, na.rm = TRUE),
        resident_abs = sum(B11 == 2, na.rm = TRUE),
        resident_vis = sum(B11 == 3, na.rm = TRUE)
      ) %>% 
      ungroup() %>% 
      mutate(
        resident_pres = as.numeric(resident_pres) %>%
          structure(label = "Nombre de Résident Présent dans le ménage"),
        resident_abs = as.numeric(resident_abs) %>%
          structure(label = "Nombre de Résident Absent dans le ménage"),
        resident_vis = as.numeric(resident_vis) %>%
          structure(label = "Nombre de Visiteur dans le ménage")
      ),
    
    # Merging by matching columns in both datasets
    by = c("ID_MENAGE"))
mig_var = c("resident_pres","resident_abs","resident_vis")
```

### Maternal death variable



## Socio-economic variables


### Employed variable



### Occupational status variable



### Income variable

Two identical questions asked for the two censuses are: (1) during the last 12 months, has your household skipped a meal because of lack of income? (2) During the last 12 months, has it happened that a sick member of your household could not receive medical care because of lack of income? The answers to these questions were either 1. Yes or 2. No


```r
#For the RGPH 2002 dataset, base menage
#E16: Repas sauté
     # 1   Oui
     # 2   Non
     # 9    nd
#E17:Soins médicaux
     # 1            Oui
     # 2            Non
     # 3 Non Applicable
     # 9             nd
menage_2002 <- menage_2002 %>% 
  dplyr::mutate(I_repas_saute = ifelse(E16==1,1,0) %>% structure(label=Hmisc::label(individus_2002$E16)),
                I_soins_medicaux = ifelse(E17==1,1,0) %>% structure(label=Hmisc::label(individus_2002$E17)))

#For the RGPH 2013 dataset
menage_2013 <- menage_2013 %>% 
  dplyr::mutate(I_repas_saute = ifelse(E18==1,1,0) %>% structure(label=Hmisc::label(individus_2002$E18)),
                I_soins_medicaux = ifelse(E20==1,1,0) %>% structure(label=Hmisc::label(individus_2002$E20)))
income_var = c("I_repas_saute","I_soins_medicaux")
```

## Household variables

### Capital goods variable


```r
#For the RGPH 2002 dataset
#OK
# 1. Radio, 2. Television, 3. Video/VCD/DVD, 4. Refrigerator/freezer, 5. Telephone, 6. Improved fireplace, 7. Air conditioner, 8. Sewing machine, 9. Water heater, 10. Stove

#For the RGPH 2013 dataset
```

### Access to basic equipment


```r
#For the RGPH 2002 dataset
#OK
# 1. Main mode of household waste disposal, 2. Main mode of waste water disposal, 3. Main mode of lighting, 4. Main fuel for cooking
#For the RGPH 2013 dataset
```

### Household size


```r
#For the RGPH 2002 dataset
#OK
#For the RGPH 2013 dataset
```

### Head of household


```r
## In the following we merge the "menage_2002" with the individual dataset
menage_2002 <- menage_2002 %>% 
  left_join(
    
    individus_2002 %>% 
      
      # Filtering to get only individuals marked as "Chef de menage" in QB03
      filter(QB03== 1) %>% #"Chef de menage"
      
      # Creating the "niveau_instruction_cm" variable from QB15 column
      mutate(niveau_instruction_cm = QB15 %>% structure(label=paste0(Hmisc::label(individus_2002$QB15), " CM")),
      
             # Creating the "sexe_cm" variable from QB04 column
          sexe_cm = QB04 %>% structure(label= paste0(Hmisc::label(individus_2002$QB04), " Sexe CM")),
          
          # Creating the "age_cm" variable from QB06 column
          age_cm = QB06 %>% structure(label=paste0(Hmisc::label(individus_2002$QB06), "Age CM"))) %>% 
      # Selecting specific columns from the filtered dataset
      select(ID_MENAGE, niveau_instruction_cm, sexe_cm, age_cm) ,
      
      # Merging by matching columns in both datasets
      by = c("ID_MENAGE")# = "ID_MENAGE", "id_dr" = "DR"
             
    
  )


## In the following we merge the "menage_2013" with the individual dataset
menage_2013 <- menage_2013 %>% 
  left_join(
    
    individus_2013 %>% 
      
      # Filtering to get only individuals marked as "Chef de menage" in B04
      filter(B04==1) %>% #"Chef de ménage"
      
      # Renaming the B32 column to "niveau_instruction_cm"
      mutate(niveau_instruction_cm = B32 %>% structure(label=paste0(Hmisc::label(individus_2013$B32)," CM")), 
      
      # Creating the "sexe_cm" variable from B06 column
          sexe_cm = B06 %>% structure(label= paste0(Hmisc::label(individus_2013$B06), "Sexe CM")),
          
          # Creating the "age_cm" variable from QB08 column
          age_cm = B08 %>% structure(label=paste0(Hmisc::label(individus_2013$B08), "Age CM")))%>% 
      # Selecting specific columns from the filtered dataset
      select(ID_MENAGE, niveau_instruction_cm, sexe_cm, age_cm),
      
      # Merging by matching columns in both datasets
      by = c("ID_MENAGE")# = "ID_MENAGE", "id_dr" = "IDDR"
    
  )
```

### Household composition















*Calculation of indices on household-related component variables (Age of household heads, by gender between the two censuses)*


```r
# Select the CM (Chef de ménage) and compute the average age

# Select relevant columns (B04, B06, B08, RGPH) from the "individus_2013" dataset
individus_2013 %>% 
  select(B04, B06, B08, RGPH) %>%

  # Filter to select only records where "B04" (Chef de ménage) equals 1
  # This is done to identify the household head
  filter(B04== 1) %>% # Household head (B04 == 1) "Chef de ménage"

  # Rename columns for clarity
  rename(
    sexe_cm = B06,        # Renamed B06 to "sexe_cm"
    lien_cm = B04,        # Renamed B04 to "lien_cm"
    age_cm = B08,         # Renamed B08 to "age_cm"
  ) %>% 

  # Combine this filtered data with a similar operation on the "individus_2002" dataset
  bind_rows(
    individus_2002 %>% 
      select(QB03, QB04, QB06, RGPH) %>%
    
    # Filter to select only records where "QB03" (Chef de ménage) equals "Chef de menage"
    filter(QB03 == 1 ) %>% # Household head (QB03 == 1) "Chef de menage"
      
    # Rename columns for consistency
    rename(
      sexe_cm = QB04,        # Renamed QB04 to "sexe_cm"
      lien_cm = QB03,        # Renamed QB03 to "lien_cm"
      age_cm = QB06,         # Renamed QB06 to "age_cm"
    ) 
    
  ) %>% 

  # Remove the "lien_cm" (link to household head) column
  select(-lien_cm) %>% 
  mutate( sexe_cm = ifelse(sexe_cm ==2, 0, 1)) %>% 
  
  # Generate a summary table using "tbl_summary" for the specified columns
  tbl_summary(
    by = RGPH,  # Group by "RGPH" column
    label = list(sexe_cm ~ "Sexe du chef de ménage (Masculin = 1, Féminin = 0)",
                 age_cm ~ "Age du chef de ménage (en année)"),
    statistic = list(all_continuous() ~ "{mean}" 
                     #all_categorical() ~ "{p}"
                     ),
    digits = everything() ~ c(0,0),
    missing = "no"
  ) %>% 

  # Modify the table header to provide a descriptive label
  modify_header(label ~ "Caractéristique du Chef de ménage entre les deux recensements") %>%
  
  # Add difference statistics to the table
  add_difference() 
```

```{=html}
<div id="zjzcfthjrs" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#zjzcfthjrs table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#zjzcfthjrs thead, #zjzcfthjrs tbody, #zjzcfthjrs tfoot, #zjzcfthjrs tr, #zjzcfthjrs td, #zjzcfthjrs th {
  border-style: none;
}

#zjzcfthjrs p {
  margin: 0;
  padding: 0;
}

#zjzcfthjrs .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#zjzcfthjrs .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#zjzcfthjrs .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#zjzcfthjrs .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#zjzcfthjrs .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#zjzcfthjrs .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#zjzcfthjrs .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#zjzcfthjrs .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#zjzcfthjrs .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#zjzcfthjrs .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#zjzcfthjrs .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#zjzcfthjrs .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#zjzcfthjrs .gt_spanner_row {
  border-bottom-style: hidden;
}

#zjzcfthjrs .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#zjzcfthjrs .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#zjzcfthjrs .gt_from_md > :first-child {
  margin-top: 0;
}

#zjzcfthjrs .gt_from_md > :last-child {
  margin-bottom: 0;
}

#zjzcfthjrs .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#zjzcfthjrs .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#zjzcfthjrs .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#zjzcfthjrs .gt_row_group_first td {
  border-top-width: 2px;
}

#zjzcfthjrs .gt_row_group_first th {
  border-top-width: 2px;
}

#zjzcfthjrs .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#zjzcfthjrs .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#zjzcfthjrs .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#zjzcfthjrs .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#zjzcfthjrs .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#zjzcfthjrs .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#zjzcfthjrs .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#zjzcfthjrs .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#zjzcfthjrs .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#zjzcfthjrs .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#zjzcfthjrs .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#zjzcfthjrs .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#zjzcfthjrs .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#zjzcfthjrs .gt_left {
  text-align: left;
}

#zjzcfthjrs .gt_center {
  text-align: center;
}

#zjzcfthjrs .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#zjzcfthjrs .gt_font_normal {
  font-weight: normal;
}

#zjzcfthjrs .gt_font_bold {
  font-weight: bold;
}

#zjzcfthjrs .gt_font_italic {
  font-style: italic;
}

#zjzcfthjrs .gt_super {
  font-size: 65%;
}

#zjzcfthjrs .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#zjzcfthjrs .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#zjzcfthjrs .gt_indent_1 {
  text-indent: 5px;
}

#zjzcfthjrs .gt_indent_2 {
  text-indent: 10px;
}

#zjzcfthjrs .gt_indent_3 {
  text-indent: 15px;
}

#zjzcfthjrs .gt_indent_4 {
  text-indent: 20px;
}

#zjzcfthjrs .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Caractéristique du Chef de ménage entre les deux recensements">Caractéristique du Chef de ménage entre les deux recensements</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;2002&lt;/strong&gt;, N = 34,226&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>2002</strong>, N = 34,226<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;2013&lt;/strong&gt;, N = 43,188&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>2013</strong>, N = 43,188<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Difference&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;2&lt;/sup&gt;&lt;/span&gt;"><strong>Difference</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>2</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;95% CI&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;2,3&lt;/sup&gt;&lt;/span&gt;"><strong>95% CI</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>2,3</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;p-value&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;2&lt;/sup&gt;&lt;/span&gt;"><strong>p-value</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>2</sup></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Sexe du chef de ménage (Masculin = 1, Féminin = 0)</td>
<td headers="stat_1" class="gt_row gt_center">26,309 (77%)</td>
<td headers="stat_2" class="gt_row gt_center">29,419 (68%)</td>
<td headers="estimate" class="gt_row gt_center">8.8%</td>
<td headers="ci" class="gt_row gt_center">8.1%, 9.4%</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Age du chef de ménage (en année)</td>
<td headers="stat_1" class="gt_row gt_center">47</td>
<td headers="stat_2" class="gt_row gt_center">49</td>
<td headers="estimate" class="gt_row gt_center">-1.3</td>
<td headers="ci" class="gt_row gt_center">-1.5, -1.1</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="6"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> n (%); Mean</td>
    </tr>
    <tr>
      <td class="gt_footnote" colspan="6"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>2</sup></span> Two sample test for equality of proportions; Welch Two Sample t-test</td>
    </tr>
    <tr>
      <td class="gt_footnote" colspan="6"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>3</sup></span> CI = Confidence Interval</td>
    </tr>
  </tfoot>
</table>
</div>
```



# Multivariate calculation for poverty indices (MPI)

## Education dimension

### Cleaning of the "education" variable




### Education indicators computation

*Years of schooling indicator*


```r
edu_var = c("Primaire","Moyen","Secondaire","Superieur")
#educ_var = c("Aucun","Primaire","Moyen","Secondaire","Superieur")
# For the 2002 individus data  

menage_2002 <- 
  menage_2002 %>% 
  
# Calculate a binary variable 'school_attendance' for each household.
# If the sum of education levels is greater than 0, it means at least one individual in the household is attending school (0), 
# otherwise, it means no one is attending school (1).
  mutate(years_schooling_dim = rowSums(select(., .dots=all_of(edu_var)),na.rm = T),
    years_schooling_dim = 
           as.factor(ifelse(years_schooling_dim > 0, 0, 1)) %>% structure(label = "Education: years of schooling"))
  
# For the 2013 individus data  

menage_2013 <- 
  menage_2013 %>% 
  
# Calculate a binary variable 'school_attendance' for each household.
# If the sum of education levels is greater than 0, it means at least one individual in the household is attending school (0), 
# otherwise, it means no one is attending school (1).
  mutate(years_schooling_dim = rowSums(select(., .dots=all_of(edu_var)),na.rm = T),
    years_schooling_dim = 
           as.factor(ifelse(years_schooling_dim > 0, 0, 1)) %>% structure(label = "Education: years of schooling"))
```

*School Attendance indicator*


```r
# Processing for the year 2002
menage_2002 <- 
  menage_2002 %>% 
  left_join(
    # Joining the data with summarized statistics per household
    individus_2002 %>%
    group_by(ID_MENAGE) %>%
    summarise(
      # Calculating the number of children (lien_cm=="children" and age < 13) with no school attendance 
      
      school_attendance = sum(educ == 0 & age5 > 13 & QB03 == 3, na.rm = TRUE),
      
      #The total number of (lien_cm=="children") in the household
      children_hh = sum(QB03 == 3,na.rm = TRUE)
    ) %>%
    ungroup() %>%
      
    # Computing a dimension for school attendance in the household
    mutate(
      school_attendance_dim = as.factor(ifelse(school_attendance == children_hh, 1, 0))%>% structure(label = "Education: school attendance")
    ) %>%
      
    # Selecting relevant columns for the join
    select(ID_MENAGE, school_attendance_dim),
    by = c("ID_MENAGE")
  )

# Processing for the year 2013
menage_2013 <- 
  menage_2013 %>% 
  left_join(
    # Joining the data with summarized statistics per household
    individus_2013 %>%
    group_by(ID_MENAGE) %>%
    summarise(
      
      # Calculating the number of children (lien_cm=="children" and age < 13) with no school attendance  (NA for B32)
      school_attendance = sum(is.na(B32) & B08 > 13 & B04 == 3, na.rm = TRUE),
      
      # Computing a dimension for school attendance in the household
      children_hh = sum(B04 == 3,na.rm = TRUE)
    ) %>%
    ungroup() %>%
      
    # Computing a dimension for school attendance in the household
    mutate(
      school_attendance_dim = as.factor(ifelse(school_attendance == children_hh, 1, 0)) %>% structure(label = "Education: school attendance")
    ) %>%
      
    # Selecting relevant columns for the join
    select(ID_MENAGE, school_attendance_dim),
    by = c("ID_MENAGE")
  )
```


## Standard of Living dimension

### Cleaning the "Standard of Living" variables


```r
# Renaming variables so that they can have the same name in the two
# datasets

# Renaming variables so that they can have the same name in the two datasets

## For the RGPH 2002
menage_2002 <- menage_2002 %>% 
  
  # Create and rename variables in the "menage_2002" dataset
  mutate(
       # Create a new variable "nb_indiv" with labels from the "menage_2002" dataset
    #nb_indiv = Nb_indiv %>% structure(label=Hmisc::label(menage_2002$Nb_indiv)),
    
    # Create a new variable "id_dr" with labels from the "menage_2002" dataset
    id_dr = DR %>% structure(label=Hmisc::label(menage_2002$DR)),
    
    # Create a new variable "type_log" as a factor with labels from the "menage_2002" dataset
    type_log = as.factor(E01) %>% structure(label=Hmisc::label(menage_2002$E01)),
    
    # Create a new variable "piece_occup" as a factor with labels from the "menage_2002" dataset
    piece_occup = as.factor(E02) %>% structure(label=Hmisc::label(menage_2002$E02)),
    
    # Create a new variable "stat_occupation" as a factor with labels from the "menage_2002" dataset
    stat_occupation = as.factor(E03) %>% structure(label=Hmisc::label(menage_2002$E03)),
    
    # Create a new variable "type_aisance" as a factor with labels from the "menage_2002" dataset
    type_aisance = as.factor(E04) %>% structure(label=Hmisc::label(menage_2002$E04)),
    
    # Create a new variable "approv_eau" as a factor with labels from the "menage_2002" dataset
    approv_eau = as.factor(E05) %>% structure(label=Hmisc::label(menage_2002$E05)),
    
    # Create a new variable "eclairage" as a factor with labels from the "menage_2002" dataset
    eclairage = as.factor(E06) %>% structure(label=Hmisc::label(menage_2002$E06)),
    
    # Create a new variable "combustible_cuisine" as a factor with labels from the "menage_2002" dataset
    combustible_cuisine = as.factor(E07) %>% structure(label=Hmisc::label(menage_2002$E07)),
    
    # Create new variables "nature_mur," "nature_toit," and "nature_sol" as factors with labels from the "menage_2002" dataset
    nature_mur = as.factor(E08) %>% structure(label=Hmisc::label(menage_2002$E08)),
    nature_toit = as.factor(E09) %>% structure(label=Hmisc::label(menage_2002$E09)),
    nature_sol = as.factor(E10) %>% structure(label=Hmisc::label(menage_2002$E10)),
    
    # Create new variables "radio," "televiseur," "video," and "refri_conge" as factors with labels from the "menage_2002" dataset
    radio = as.factor(E11B) %>% structure(label=Hmisc::label(menage_2002$E11B)),
    televiseur = as.factor(E11C) %>% structure(label=Hmisc::label(menage_2002$E11C)),
    video = as.factor(E11D) %>% structure(label=Hmisc::label(menage_2002$E11D)),
    refri_conge = as.factor(E11E) %>% structure(label=Hmisc::label(menage_2002$E11E)),
    
    # Create new variables "telephone," "rechaud_cuis," "foyer_ameli," and "climatisateur" as factors with labels from the "menage_2002" dataset
    telephone = as.factor(E11F) %>% structure(label=Hmisc::label(menage_2002$E11F)),
    rechaud_cuis = as.factor(E11G) %>% structure(label=Hmisc::label(menage_2002$E11G)),
    foyer_ameli = as.factor(E11H) %>% structure(label=Hmisc::label(menage_2002$E11H)),
    climatisateur = as.factor(E11I) %>% structure(label=Hmisc::label(menage_2002$E11I)),
    
    # Create new variables "machine_coudre," "voiture," "mobylette," "bicyclette," "caleche_charette," and "pirogue" as factors with labels from the "menage_2002" dataset
    machine_coudre = as.factor(E11J) %>% structure(label=Hmisc::label(menage_2002$E11J)),
    voiture = as.factor(E12B) %>% structure(label=Hmisc::label(menage_2002$E12B)),
    mobylette = as.factor(E12C) %>% structure(label=Hmisc::label(menage_2002$E12C)),
    bicyclette = as.factor(E12E) %>% structure(label=Hmisc::label(menage_2002$E12E)),
    caleche_charette = as.factor(E12F) %>% structure(label=Hmisc::label(menage_2002$E12F)),
    pirogue = as.factor(E12G) %>% structure(label=Hmisc::label(menage_2002$E12G)),
    
    
# Create a new variable "houe_charrue" as a factor with labels from the "menage_2002" dataset
houe_charrue = as.factor(E13B) %>% structure(label=Hmisc::label(menage_2002$E13B)),

# Create a new variable "caleche_char" as a factor with labels from the "menage_2002" dataset
caleche_char = as.factor(E13C) %>% structure(label=Hmisc::label(menage_2002$E13C)),

# Create a new variable "animaux_traite" as a factor with labels from the "menage_2002" dataset
animaux_traite = as.factor(E13D) %>% structure(label=Hmisc::label(menage_2002$E13D)),

# Create a new variable "tracteur" as a factor with labels from the "menage_2002" dataset
tracteur = as.factor(E13E) %>% structure(label=Hmisc::label(menage_2002$E13E)),

# Create a new variable "voiture_cam" as a factor with labels from the "menage_2002" dataset
voiture_cam = as.factor(E13F) %>% structure(label=Hmisc::label(menage_2002$E13F)),

# Create a new variable "mobylette_bicycl" as a factor with labels from the "menage_2002" dataset
mobylette_bicycl = as.factor(E13G) %>% structure(label=Hmisc::label(menage_2002$E13G)),

# Create a new variable "refriger_congelat" as a factor with labels from the "menage_2002" dataset
refriger_congelat = as.factor(E13I) %>% structure(label=Hmisc::label(menage_2002$E13I)),

# Create a new variable "machine_coudre" as a factor with labels from the "menage_2002" dataset
machine_coudre = as.factor(E13J) %>% structure(label=Hmisc::label(menage_2002$E13J)),

# Create a new variable "materiel_musique" as a factor with labels from the "menage_2002" dataset
materiel_musique = as.factor(E13K) %>% structure(label=Hmisc::label(menage_2002$E13K)),

# Create a new variable "chaise_bache" as a factor with labels from the "menage_2002" dataset
chaise_bache = as.factor(E13L) %>% structure(label=Hmisc::label(menage_2002$E13L)),

# Create a new variable "telephone_tele" as a factor with labels from the "menage_2002" dataset
telephone_tele = as.factor(E13M) %>% structure(label=Hmisc::label(menage_2002$E13M)),

# Create a new variable "photocopieuse" as a factor with labels from the "menage_2002" dataset
photocopieuse = as.factor(E13N) %>% structure(label=Hmisc::label(menage_2002$E13N)),

# Create a new variable "ordinateur" as a factor with labels from the "menage_2002" dataset
ordinateur = as.factor(E13O) %>% structure(label=Hmisc::label(menage_2002$E13O)),

# Create a new variable "moulin" as a factor with labels from the "menage_2002" dataset
moulin = as.factor(E13P) %>% structure(label=Hmisc::label(menage_2002$E13P)),

# Create a new variable "appareil_photo" as a factor with labels from the "menage_2002" dataset
appareil_photo = as.factor(E13Q) %>% structure(label=Hmisc::label(menage_2002$E13Q)),

# Create a new variable "terrain_bat" as a factor with labels from the "menage_2002" dataset
terrain_bat = as.factor(E13R) %>% structure(label=Hmisc::label(menage_2002$E13R)),

# Create a new variable "evacut_ordures" as a factor with labels from the "menage_2002" dataset
evacut_ordures = as.factor(E14) %>% structure(label=Hmisc::label(menage_2002$E14)),

# Create a new variable "evacut_eaux_usees" as a factor with labels from the "menage_2002" dataset
evacut_eaux_usees = as.factor(E15) %>% structure(label=Hmisc::label(menage_2002$E15)),

# Create a new variable "repas_saute" as a factor with labels from the "menage_2002" dataset
repas_saute = as.factor(E16) %>% structure(label=Hmisc::label(menage_2002$E16)),

# Create a new variable "soins_medicaux" as a factor with labels from the "menage_2002" dataset
soins_medicaux = as.factor(E17) %>% structure(label=Hmisc::label(menage_2002$E17))
  )


# Mutate and rename variables in the "menage_2013" dataset
menage_2013 <- menage_2013 %>% 
  
  mutate(
# Create a new variable "id_dr" as a factor with labels from the "menage_2013" dataset
id_dr = IDDR %>% structure(label=Hmisc::label(menage_2013$IDDR)),

# Create a new variable "nb_indiv" as a factor with labels from the "menage_2013" dataset
#nb_indiv = Nbre_indiv %>% structure(label=Hmisc::label(menage_2013$Nbre_indiv)),

# Create a new variable "type_log" as a factor with labels from the "menage_2013" dataset
type_log = as.factor(E01) %>% structure(label=Hmisc::label(menage_2013$E01)),

# Create a new variable "piece_occup" as a factor with labels from the "menage_2013" dataset
piece_occup = as.factor(E02) %>% structure(label=Hmisc::label(menage_2013$E02)),

# Create a new variable "stat_occupation" as a factor with labels from the "menage_2013" dataset
stat_occupation = as.factor(E03) %>% structure(label=Hmisc::label(menage_2013$E03)),

# Create a new variable "type_aisance" as a factor with labels from the "menage_2013" dataset
type_aisance = as.factor(E08) %>% structure(label=Hmisc::label(menage_2013$E08)),

# Create a new variable "approv_eau" as a factor with labels from the "menage_2013" dataset
approv_eau = as.factor(E09) %>% structure(label=Hmisc::label(menage_2013$E09)),

# Create a new variable "eclairage" as a factor with labels from the "menage_2013" dataset
eclairage = as.factor(E11) %>% structure(label=Hmisc::label(menage_2013$E11)),

# Create a new variable "combustible_cuisine" as a factor with labels from the "menage_2013" dataset
combustible_cuisine = as.factor(E12) %>% structure(label=Hmisc::label(menage_2013$E12)),

# Create a new variable "nature_mur" as a factor with labels from the "menage_2013" dataset
nature_mur = as.factor(E05) %>% structure(label=Hmisc::label(menage_2013$E05)),

# Create a new variable "nature_toit" as a factor with labels from the "menage_2013" dataset
nature_toit = as.factor(E06) %>% structure(label=Hmisc::label(menage_2013$E06)),

# Create a new variable "nature_sol" as a factor with labels from the "menage_2013" dataset
nature_sol = as.factor(E07) %>% structure(label=Hmisc::label(menage_2013$E07)),

# Create a new variable "radio" as a factor with labels from the "menage_2013" dataset
radio = as.factor(E13_1) %>% structure(label=Hmisc::label(menage_2013$E13_1)),

# Create a new variable "televiseur" as a factor with labels from the "menage_2013" dataset
televiseur = as.factor(E13_2) %>% structure(label=Hmisc::label(menage_2013$E13_2)),

# Create a new variable "video" as a factor with labels from the "menage_2013" dataset
video = as.factor(E13_3) %>% structure(label=Hmisc::label(menage_2013$E13_3)),

# Create a new variable "refri_conge" as a factor with labels from the "menage_2013" dataset
refri_conge = as.factor(E13_4) %>% structure(label=Hmisc::label(menage_2013$E13_4)),

# Create a new variable "telephone" as a factor with labels from the "menage_2013" dataset
telephone = as.factor(E13_6) %>% structure(label=Hmisc::label(menage_2013$E13_6)),

# Create a new variable "rechaud_cuis" as a factor with labels from the "menage_2013" dataset
rechaud_cuis = as.factor(E13_18) %>% structure(label=Hmisc::label(menage_2013$E13_18)),

# Create a new variable "foyer_ameli" as a factor with labels from the "menage_2013" dataset
foyer_ameli = as.factor(E13_7) %>% structure(label=Hmisc::label(menage_2013$E13_7)),

# Create a new variable "climatisateur" as a factor with labels from the "menage_2013" dataset
climatisateur = as.factor(E13_8) %>% structure(label=Hmisc::label(menage_2013$E13_8)),

# Create a new variable "machine_coudre" as a factor with labels from the "menage_2013" dataset
machine_coudre = as.factor(E13_9) %>% structure(label=Hmisc::label(menage_2013$E13_9)),
    # Create a new variable "voiture" as a factor with labels from the "menage_2013" dataset
voiture = as.factor(E15_5) %>% structure(label=Hmisc::label(menage_2013$E15_5)),

# Create a new variable "mobylette" as a factor with labels from the "menage_2013" dataset
mobylette = as.factor(E15_6) %>% structure(label=Hmisc::label(menage_2013$E15_6)),

# Create a new variable "bicyclette" as a factor with labels from the "menage_2013" dataset
bicyclette = as.factor(E14_3) %>% structure(label=Hmisc::label(menage_2013$E14_3)),

# Create a new variable "caleche_charette" as a factor with labels from the "menage_2013" dataset
caleche_charette = as.factor(E14_4) %>% structure(label=Hmisc::label(menage_2013$E14_4)),

# Create a new variable "pirogue" as a factor with labels from the "menage_2013" dataset
pirogue = as.factor(E15_7) %>% structure(label=Hmisc::label(menage_2013$E15_7)),

# Create a new variable "houe_charrue" as a factor with labels from the "menage_2013" dataset
houe_charrue = as.factor(E15_1) %>% structure(label=Hmisc::label(menage_2013$E15_1)),

# Create a new variable "caleche_char" as a factor with labels from the "menage_2013" dataset
caleche_char = as.factor(E15_2) %>% structure(label=Hmisc::label(menage_2013$E15_2)),

# Create a new variable "animaux_traite" as a factor with labels from the "menage_2013" dataset
animaux_traite = as.factor(E15_3) %>% structure(label=Hmisc::label(menage_2013$E15_3)),

# Create a new variable "tracteur" as a factor with labels from the "menage_2013" dataset
tracteur = as.factor(E15_4) %>% structure(label=Hmisc::label(menage_2013$E15_4)),

# Create a new variable "voiture_cam" as a factor with labels from the "menage_2013" dataset
voiture_cam = as.factor(E15_5) %>% structure(label=Hmisc::label(menage_2013$E15_5)),

# Create a new variable "mobylette_bicycl" as a factor with labels from the "menage_2013" dataset
mobylette_bicycl = as.factor(E15_6) %>% structure(label=Hmisc::label(menage_2013$E15_6)),

# Create a new variable "refriger_congelat" as a factor with labels from the "menage_2013" dataset
refriger_congelat = as.factor(E15_8) %>% structure(label=Hmisc::label(menage_2013$E15_8)),

# Create a new variable "machine_coudre" as a factor with labels from the "menage_2013" dataset
machine_coudre = as.factor(E15_9) %>% structure(label=Hmisc::label(menage_2013$E15_9)),

# Create a new variable "materiel_musique" as a factor with labels from the "menage_2013" dataset
materiel_musique = as.factor(E15_10) %>% structure(label=Hmisc::label(menage_2013$E15_10)),

# Create a new variable "chaise_bache" as a factor with labels from the "menage_2013" dataset
chaise_bache = as.factor(E15_11) %>% structure(label=Hmisc::label(menage_2013$E15_11)),

# Create a new variable "telephone_tele" as a factor with labels from the "menage_2013" dataset
telephone_tele = as.factor(E15_12) %>% structure(label=Hmisc::label(menage_2013$E15_12)),

# Create a new variable "photocopieuse" as a factor with labels from the "menage_2013" dataset
photocopieuse = as.factor(E15_13) %>% structure(label=Hmisc::label(menage_2013$E15_13)),

# Create a new variable "ordinateur" as a factor with labels from the "menage_2013" dataset
ordinateur = as.factor(E15_14) %>% structure(label=Hmisc::label(menage_2013$E15_14)),

# Create a new variable "moulin" as a factor with labels from the "menage_2013" dataset
moulin = as.factor(E15_15) %>% structure(label=Hmisc::label(menage_2013$E15_15)),

# Create a new variable "appareil_photo" as a factor with labels from the "menage_2013" dataset
appareil_photo = as.factor(E15_16) %>% structure(label=Hmisc::label(menage_2013$E15_16)),

# Create a new variable "terrain_bat" as a factor with labels from the "menage_2013" dataset
terrain_bat = as.factor(E15_17) %>% structure(label=Hmisc::label(menage_2013$E15_17)),

# Create a new variable "evacut_ordures" as a factor with labels from the "menage_2013" dataset
evacut_ordures = as.factor(E16) %>% structure(label=Hmisc::label(menage_2013$E16)),

# Create a new variable "evacut_eaux_usees" as a factor with labels from the "menage_2013" dataset
evacut_eaux_usees = as.factor(E17) %>% structure(label=Hmisc::label(menage_2013$E17)),

# Create a new variable "repas_saute" as a factor with labels from the "menage_2013" dataset
repas_saute = as.factor(E18) %>% structure(label=Hmisc::label(menage_2013$E18)),

# Create a new variable "soins_medicaux" as a factor with labels from the "menage_2013" dataset
soins_medicaux = as.factor(E20) %>% structure(label=Hmisc::label(menage_2013$E20))
    )
```


### Standard of living indicators computation

**Data source**: 

* *For the Sanitation*: [link](https://www.who.int/data/gho/data/indicators/indicator-details/GHO/population-using-improved-sanitation-facilities-(-))

* *For water*:
[link](https://data.unicef.org/topic/water-and-sanitation/drinking-water/)
 

```r
# For RGPH 2002
menage_2002 <- menage_2002 %>% 
  # Creating a dimension for cooking fuel, checking if it's wood or charcoal
  # Bois=1, Charbon = 2 and Autre=5
  mutate(cooking_fuel_dim = as.factor(ifelse(combustible_cuisine == 1 | combustible_cuisine == 2| combustible_cuisine == 5, 1, 0)) %>% structure(label = "Standard of living: cooking fuel"), 
         
  # Creating a dimension for sanitation, checking for specific types (public edifice, nature, other)
  # Edicule public==4, Dans la nature==4, Autre==6
         sanitation_dim = as.factor(ifelse(type_aisance %in% seq(4, 6), 1, 0)) %>% structure(label = "Standard of living: sanitation"),
         
  # Creating a dimension for drinking water, considering specific sources (indoor well, outdoor well, watercourse, other)
  # Puits interieur = 1, Puits exterieur = 2, cours d'eau = 7 et autre = 8
         drinking_water_dim = as.factor(ifelse(approv_eau %in% c(1, 2, 7, 8), 1, 0)) %>% structure(label = "Standard of living: drinking water"),
         
  # Creating a dimension for electricity, considering different sources (storm lamp, candle, wood, other)
  # Lampe tempête = 6, Bougie = 7, Bois=8, Autre= 9,
         electricity_dim = as.factor(ifelse(eclairage %in% c(6, 7, 8, 9), 1, 0))%>% structure(label = "Standard of living: electricity"),
         
  # Creating a dimension for housing quality, checking specific types of construction materials for walls, floor, and roof
        # For nature_mur, 3 is "Banco", 4 is "Paille/tige" and 5 is 'autre".
         # For nature_sol, 3 is 'Argile/banco", 4 is Sable and 5 is "autre".
         # For nature_toit, 4 is "Chaume" and 5 is "autre"
         housing_dim = as.factor(ifelse(nature_mur %in% c(3, 4, 5) & nature_sol %in% c(3, 4, 5) & nature_toit %in% c(4, 5), 1, 0))%>% structure(label = "Standard of living: housing"),
         
  # Creating a dimension for household assets, checking if various assets are absent in the household
  
         assets_dim = as.factor(ifelse(radio == 0 & televiseur == 0 & telephone == 0 & animaux_traite == 0 & bicyclette == 0 & mobylette == 0 & mobylette_bicycl == 0 & 
                           refri_conge == 0 & refriger_congelat == 0 & voiture == 0 & voiture_cam == 0, 1, 0)) %>% structure(label = "Standard of living: assets")
  )



# For RGPH 2013
menage_2013 <- menage_2013 %>% 
  # Creating a dimension for cooking fuel, checking if it's wood or charcoal
  # Bois=1, Charbon = 2, Bouse de vache=5 and Autre=8
  mutate(cooking_fuel_dim = as.factor(ifelse(combustible_cuisine %in% c(1,2,5,8), 1, 0))%>% structure(label = "Standard of living: cooking fuel"), 
         
  # Creating a dimension for sanitation, checking for specific types (public edifice, nature, other)
  # Non couverte =22, Edicule public==31, Dans la nature==41, Autre==88
         sanitation_dim = as.factor(ifelse(type_aisance %in% c(22, 31, 41, 88), 1, 0))%>% structure(label = "Standard of living: sanitation"),
         
  # Creating a dimension for drinking water, considering specific sources (indoor well, outdoor well, watercourse, other)
  # Puits non protégés = 6, Eau de pluie = 9, Source, cour d'eau = 10 and autre = 98
         drinking_water_dim = as.factor(ifelse(approv_eau %in% c(6, 9, 10, 98), 1, 0))%>% structure(label = "Standard of living: drinking water"),
         
  # Creating a dimension for electricity, considering different sources (storm lamp, candle, wood, other)
  # Lampe à pétrole artisanale=5, Lampe tempête = 6, Bougie = 7, Bois=8, Lampe rechargeable= 9, Autre=98
         electricity_dim = as.factor(ifelse(eclairage %in% c(5, 6, 7, 8, 9, 98), 1, 0))%>% structure(label = "Standard of living: electricity"),
         
  # Creating a dimension for housing quality, checking specific types of construction materials for walls, floor, and roof
        # For nature_mur, 5 is Bois, 6 is "Banco", 7 is "Banco+ enduit ciment",  8 is 'Paille/Tige" and 9 is "Autre".
         # For nature_sol, 3 is 'Argile/banco", 4 is Sable and 7 is "Bois ciré" and 8 is "autre".
         # For nature_toit, 0 is "Autre" and 4 is "Chaume/paille"
         housing_dim = as.factor(ifelse(nature_mur %in% c(5, 6, 7, 8, 9) & nature_sol %in% c(3, 4, 7, 8) & nature_toit %in% c(0, 4), 1, 0))%>% structure(label = "Standard of living: housing"),
         
  # Creating a dimension for household assets, checking if various assets are absent in the household
  
         assets_dim = as.factor(ifelse(radio == 0 & televiseur == 0 & telephone == 0 & telephone_tele == 0 & animaux_traite == 0 & bicyclette == 0 & mobylette == 0 & mobylette_bicycl == 0 & 
                           refri_conge == 0 & refriger_congelat == 0 & voiture == 0 & voiture_cam == 0, 1, 0))%>% structure(label = "Standard of living: assets")
  )
```




*Additionnal cleaning*


```r
# For the RGPH 2013 dataset, recode the "soins_medicaux" variable
# This code modifies the "soins_medicaux" variable, replacing "2" with "0" if present

menage_2013 <- menage_2013 %>%
  mutate(soins_medicaux = as.numeric(soins_medicaux),
            soins_medicaux = 
                dplyr::recode(soins_medicaux,"2" = 0),
            soins_medicaux = as.factor(soins_medicaux) %>% structure(label=Hmisc::label(menage_2002$E17))) 

# For the RGPH 2002 dataset, recode the "soins_medicaux" variable
# This code modifies the "soins_medicaux" variable, replacing "2" with "0", "3" with NA, and "9" with NA
menage_2002 <- menage_2002 %>%
  mutate(soins_medicaux = as.numeric(soins_medicaux),
           soins_medicaux = dplyr::recode(soins_medicaux, "2" = 0, "3" = NULL, "9" = NULL), 
         soins_medicaux = as.factor(soins_medicaux) %>% structure(label=Hmisc::label(menage_2002$E17)))
```

## Health dimension

### Cleaning of the "Health" variables and indicators computation


```r
# For the RGPH 2002 dataset, determine if there was a death in the household in the last twelve months
# This code calculates whether there was a death in the household based on specific columns
deces_2002 <- deces_2002 %>% 
  mutate(
    est_decedee = ifelse(is.na(C03) & is.na(C04) & is.na(C05) & is.na(GROUPE_AGE_DECES), 0, 1) %>% structure(label = "y-a-t-il eu décès dans le ménage au cours des douze derniers mois?")
  ) %>% 
  group_by(ID_MENAGE) %>% 
  summarise(
    
    # Count the number of death in the household
    deces = sum(est_decedee, na.rm = TRUE),
    
    # Count the number of death in the household below the age of 5
    deces_under_five = sum(est_decedee==1 & C04 <=5,
                                       na.rm = TRUE)) %>% 
  ungroup() %>% 
  # Create a variable for an under 18 death in the household
  mutate(deces_under_five_dim = as.factor(ifelse(deces_under_five > 0, 1, ifelse(is.na(deces_under_five), 0, 0)) %>%
                                            structure(label = "Y a-t-il eu un décés d'un enfant de moins de cinq ans dans le ménage ?")),
         deces_under_five = deces_under_five %>%
                                            structure(label = "Nombre de décés de moins de cinq ans")) 

# For the RGPH 2013 dataset, calculate the number of deaths in each household
# This code groups the data by household and calculates the number of deaths in each household
deces_2013 <- deces_2013 %>% 
  group_by(ID_MENAGE) %>% 
  summarise(deces = sum(C01, na.rm = TRUE)
    
    # Count the number of death in the household below the age of 18
  ) %>% 
  ungroup() %>% 
  # Create the variable for the number of death in the household
  mutate(deces = as.numeric(deces) %>%
           structure(label = "Nombre de décés dans le ménage")) 
```




```r
# For the RGPH 2013 dataset, select specific columns and merge with "deces_2013" data
# This code selects the "id_dr," "ID_MENAGE," and columns 77 to the last column in "menage_2013"
# It then performs a left join with "deces_2013" based on the "ID_MENAGE" key

# Selecting specific columns from the 'menage_2013' dataset
menage_2013 <- menage_2013 %>% 
  select(id_dr, ID_MENAGE, everything()) %>%
  
# Performing a left join with the 'deces_2013' dataset based on 'ID_MENAGE'
  left_join(deces_2013, by = "ID_MENAGE") %>%
  
# Adding a new column 'RGPH' with the value "2013" and setting a label
  mutate(RGPH = "2013" %>% structure(label = "RGPH"),
  
# Adding a label to the 'deces' column
         deces = deces %>% structure(label = "Nombre de décès dans le ménage au cours des 12 derniers mois"))

# For the RGPH 2002 dataset, select specific columns and merge with "deces_2013" data
# This code selects the "id_dr," "ID_MENAGE," and columns 71 to the last column in "menage_2002"
# It then performs a left join with "deces_2013" based on the "ID_MENAGE" key

# Selecting specific columns from the 'menage_2002' dataset
menage_2002 <- menage_2002 %>% 
  select(id_dr, ID_MENAGE, num_men, everything()) %>%
  
# Performing a left join with the 'deces_2002' dataset based on 'ID_MENAGE'
  left_join(deces_2002, by = "ID_MENAGE") %>%
  
# Adding a new column 'RGPH' with the value "2002" and setting a label
  mutate(RGPH = "2002" %>% structure(label = "RGPH"),
  
# Adding a new column 'ID_MENAGE_corrige' by checking if 'num_men' is missing and using 'ID_MENAGE' if so, then setting a label
         ID_MENAGE_corrige = ifelse(is.na(num_men), ID_MENAGE, num_men) %>% structure(label = "ID MENAGE")) %>%
  
# Selecting and removing specific columns
  select(-c(ID_MENAGE, num_men)) %>%
  
# Renaming the 'ID_MENAGE_corrige' column to 'ID_MENAGE'
  rename(ID_MENAGE = ID_MENAGE_corrige) %>%
  
# Adding a new column 'deces' by checking if it's missing and setting it to 0, with a label
  mutate(deces = ifelse(is.na(deces), 0, deces) %>% structure(label = "Nombre de décès dans le ménage au cours des 12 derniers mois"),
         
         # Replace Na by 0
         
         deces_under_five = ifelse(is.na(deces_under_five), 0, deces_under_five),
         
         deces_under_five_dim = as.factor(ifelse(is.na(deces_under_five_dim), 0, deces_under_five_dim)) %>% structure(label = "Health: child mortality"))
```






```r
# Filtering the 'menage_2013' dataset to exclude specific values in the 'id_dr' column
menage_2013 <- menage_2013 %>% 
  filter(!(id_dr %in% c("014301140124", "014301140125")))
```


# Exporting the datasets



```r
## For RGPH 2013
individus_2002 <- individus_2002 %>%
 select(id_dr,ID_MENAGE, RGPH, everything())# 

## For RGPH 2002
individus_2013 <- individus_2013 %>%
 select(id_dr,ID_MENAGE, RGPH, everything())# 
# Saving in SPSS format
haven::write_sav(individus_2002,paste0(here::here(), "/output/output_data/individus_2002.sav"))
# Saving in SPSS format
haven::write_sav(individus_2013,paste0(here::here(), "/output/output_data/individus_2013.sav"))
```



```r
# Exporting the "menage_2013" and "menage_2002" datasets

# For the "menage_2002"
# Saving in SPSS format
haven::write_sav(menage_2002,paste0(here::here(), "/output/output_data/menage_2002.sav"))

# Saving in R format
saveRDS(menage_2002,file=paste0(here::here(), "/output/output_data/menage_2002.rds"))

# For the "menage_2013"
# Saving in SPSS format
haven::write_sav(menage_2013,paste0(here::here(), "/output/output_data/menage_2013.sav"))

# Saving in R format
saveRDS(menage_2013,file=paste0(here::here(), "/output/output_data/menage_2013.rds"))


# For the "sp_rgph_2002"
# Saving in the sf format
sf::st_write(obj=sp_rgph_2002, paste0(here::here(), "/output/output_data/EAs_2002_new.shp"),  driver = "ESRI Shapefile", delete_layer = TRUE) # we change the id_dr
```


```r
## Removing all objects
rm(list=ls())
```
