# Master en science actuarielles

## John GABARY, Août 2023

Bienvenue sur le dépôt GitHub dédié au travail en lien avec mon mémoire, dans le cadre de mon cursus académique en sciences actuarielle, sous la supervision du Prof. Julien Trufin.

#### NOTE : Une grande partie du code a été construite en utilisant le référentiel GitHub du Prof. Arthur Charpentier disponible à l'adresse : https://github.com/freakonometrics/autocalibration/.

# OPTIMISATION DE LA TARIFICATION EN ASSURANCE GRÂCE À L’AUTOCALIBRATION : ÉTUDE COMPARATIVE DES MODÈLES GLM, GAM ET GBM
================
John GABARY
Juillet 2023

- [Fonctions](#fonctions)
  - [Multiplot](#multiplot)
  - [Plot_freq](#plot_freq)
  - [Complot](#complot)
- [Exploration initiale des données](#exploration-initiale-des-données)
  - [Données manquantes](#données-manquantes)
  - [Structure du portefeuille](#structure-du-portefeuille)
  - [Identifiant de police (Policy
    id)](#identifiant-de-police-policy-id)
  - [Exposition au risque (Exposure)](#exposition-au-risque-exposure)
  - [Nombre de sinistres et Fréquences de
    sinstres](#nombre-de-sinistres-et-fréquences-de-sinstres)
  - [Prétraitement initial des
    données](#prétraitement-initial-des-données)
  - [Analyse des données (statistiques
    descriptives)](#analyse-des-données-statistiques-descriptives)
  - [Corrélation](#corrélation)
- [Sépartion du jeu de données](#sépartion-du-jeu-de-données)
- [Modélisation](#modélisation)
  - [Modèle Linéaire Généralisé (GLM) & Modèle Additif Généralisé
    (GAM)](#modèle-linéaire-généralisé-glm--modèle-additif-généralisé-gam)
    - [Selection](#selection)
    - [Comparaison GLM, GLM avec discrétisation, et
      GAM](#comparaison-glm-glm-avec-discrétisation-et-gam)
  - [GBM tuning](#gbm-tuning)
    - [Importances relatives et dépendance
      partielle](#importances-relatives-et-dépendance-partielle)
    - [Interactions](#interactions)
    - [Comparaison GLM & GAM & GBM](#comparaison-glm--gam--gbm)
- [Regréssion locale](#regréssion-locale)
  - [Diagnostic of the prediction](#diagnostic-of-the-prediction)
- [ditribution of premium](#ditribution-of-premium)
- [Régression locale](#régression-locale)
  - [Diagnostic de la regression](#diagnostic-de-la-regression)
- [Correction des primes
  (Autocallibration)](#correction-des-primes-autocallibration)
- [Diagnostic autocalibration](#diagnostic-autocalibration)
  - [Comparaison versions corrigées](#comparaison-versions-corrigées)
- [Graphique quantiles simples](#graphique-quantiles-simples)
- [Double lift](#double-lift)
- [Déviance poisson](#déviance-poisson)
- [Comparaison GLM, GAM et GBM (pour après
  autocalibration)](#comparaison-glm-gam-et-gbm-pour-après-autocalibration)
  - [GLM](#glm)
  - [GLM](#glm-1)
  - [GBM](#gbm)
- [Autocalibration (Uniquement basée sur les profils
  risqués)](#autocalibration-uniquement-basée-sur-les-profils-risqués)
- [Diagnostic autocalibration](#diagnostic-autocalibration-1)
  - [Comparaison versions corrigées](#comparaison-versions-corrigées-1)
- [Graphique quantiles simples](#graphique-quantiles-simples-1)
- [Double lift](#double-lift-1)
- [Déviance poisson](#déviance-poisson-1)

<center>
![](..\ULB.png)
</center>

# Fonctions

## Multiplot

``` r
 # Charger les bibliothèques nécessaires
library(ggplot2)
library(dplyr)
library(gridExtra)


# Fonction pour afficher deux graphiques en un
multiPlotD <- function(data, Variable, frname) {
  # Titres des graphiques
  t1 <- "Exposition totale"
  t2 <- "Fréquence observée"
  # Renommer la colonne avec le nom fourni (Variable)
  data <- rename(data, "Variable" := all_of(Variable))
  
  # Regroupement et résumé des données
  out_sum <- data %>%
    group_by(Variable) %>% 
    summarize(
      Exp = sum(Exposure),
      Claims_freq = sum(ClaimNb) / sum(Exposure),
      StDev = sqrt(sum(ClaimNb)) / sum(Exposure)
    )
  # Palette de couleurs pour les graphiques
  clrpal <- c("#236B8E", "#4682B4", "#6CA6CD", "#8AB6CD", "#AED4E6", "#4C72B0", "#55A868", "#C44E52", "#8172B3", "#CCB974", "#64B5CD", "#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD", "#8C564B")
  # Graphique 1 : Barres empilées
  p1 <- ggplot(out_sum, aes(x = Variable, y = Exp, fill = Variable)) +
    geom_bar(stat = "identity") +
    labs(x = frname, y = "Exposition au risque (en année)", title = t1) +
    theme_bw() +
    theme(
      plot.title = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(size = 10)
    ) +
    scale_fill_manual(values = clrpal)
  # Graphique 2 : Points, lignes et barres d'erreur
  p2 <- ggplot(out_sum, aes(x = Variable, ymin = Claims_freq - StDev, ymax = Claims_freq + StDev, group = 1)) +
    geom_point(aes(y = Claims_freq), colour = clrpal[1], size = 2) +
    geom_line(aes(y = Claims_freq), colour = clrpal[2]) +
    geom_line(aes(y = pf_freq), colour = "gray80") +
    geom_errorbar(color = clrpal[3]) +
    labs(x = frname, y = "Fréquence", title = t2) +
    theme_bw() +
    theme(
      plot.title = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(size = 10)
    ) +
    scale_colour_manual(values = clrpal)
  # Afficher les deux graphiques côte à côte
  grid.arrange(p1, p2, ncol = 2)
}
# Fonction multiPlot sans les détails
multiPlot <- function(data, Variable, frname) {
  t1 <- "Exposition totale"
  t2 <- "Fréquence observée"
  data <- rename(data, "Variable" := all_of(Variable))
  out_sum <- data %>%
    group_by(Variable) %>% 
    summarize(
      Exp = sum(Exposure),
      Claims_freq = sum(ClaimNb) / sum(Exposure),
      StDev = sqrt(sum(ClaimNb)) / sum(Exposure)
    )
    # Palette de couleurs pour les graphiques
  clrpal <- c("#236B8E", "#4682B4", "#6CA6CD", "#8AB6CD", "#AED4E6", "#4C72B0", "#55A868", "#C44E52", "#8172B3", "#CCB974", "#64B5CD", "#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD", "#8C564B")
  p1 <- ggplot(out_sum, aes(x = Variable, y = Exp, fill = Variable)) +
    geom_bar(stat = "identity") +
    labs(x = frname, y = "Exposition au risque (en année)", title = t1) +
    theme_bw() +
    theme(
      plot.title = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(size = 10)
    ) +
    scale_fill_gradient(low = clrpal[1], high = clrpal[5])
  p2 <- ggplot(out_sum, aes(x = Variable, y = Claims_freq, group = 1)) +
    geom_point(colour = clrpal[1], size = 2) +
    geom_line(aes(y = Claims_freq), colour = clrpal[2]) +
    geom_line(aes(y = pf_freq), colour = "gray80") +
    geom_errorbar(aes(ymin = Claims_freq - StDev, ymax = Claims_freq + StDev), color = clrpal[3]) +
    labs(x = frname, y = "Fréquence", title = t2) +
    theme_bw() +
    theme(
      plot.title = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(size = 10)
    ) +
    scale_fill_gradient(low = clrpal[1], high = clrpal[2])
  
  grid.arrange(p1, p2, ncol = 2)
}
```

## Plot_freq

for …

``` r
# Fonction pour afficher le graphique de fréquence avec un modèle unique
plot_freqv <- function(data_test, xvar, title, model, mdlvariant) {
  out <- data_test %>% 
    group_by(!!sym(xvar)) %>% 
    summarize(
      obs = sum(ClaimNb) / sum(Exposure),
      pred = sum(!!sym(mdlvariant)) / sum(Exposure)
    )
  ggplot(out, aes(x = !!sym(xvar), group = 1)) + 
    geom_point(aes(y = pred, colour = model)) +
    geom_point(aes(y = obs, colour = "observed"), size = 0.8) +
    geom_line(aes(y = pred, colour = model)) +
    geom_line(aes(y = obs, colour = "observed"), linetype = "solid") + 
    labs(x = xvar, y = "fréquence", title = title) +
    theme_bw() +
    theme(
      plot.title = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(size = 10)
    ) +
    scale_colour_manual(values = clrpal)
}
# Fonction pour afficher le graphique de fréquence avec deux modèles
plot_freq0 <- function(data_test, xvar, title, model1, model2) {
  out <- data_test %>% 
    group_by(!!sym(xvar)) %>% 
    summarize(
      obs = sum(ClaimNb) / sum(Exposure),
      pred1 = sum(!!sym(paste("", model1, sep = ""))) / sum(Exposure),
      pred2 = sum(!!sym(paste("", model2, sep = ""))) / sum(Exposure)
    )
  ggplot(out, aes(x = !!sym(xvar), group = 1)) +
    geom_point(aes(y = obs, colour = "observed"), size = 0.8) +
    geom_line(aes(y = obs, colour = "observed"), linetype = "solid") +
    geom_point(aes(y = pred1, colour = model1), size = 0.8) +
    geom_line(aes(y = pred1, colour = model1)) +
    geom_point(aes(y = pred2, colour = model2), size = 0.8) +
    geom_line(aes(y = pred2, colour = model2)) +
    labs(x = xvar, y = "fréquence", title = title) +
    theme_bw() +
    theme(
      plot.title = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(size = 10)
    ) +
    scale_colour_manual(values = clrpal)
}
# Fonction pour afficher le graphique de fréquence avec trois modèles
plot_freq1 <- function(data_test, xvar, title, model1, model2, model3) {
  out <- data_test %>% 
    group_by(!!sym(xvar)) %>% 
    summarize(
      obs = sum(ClaimNb) / sum(Exposure),
      pred1 = sum(!!sym(paste("", model1, sep = ""))) / sum(Exposure),
      pred2 = sum(!!sym(paste("", model2, sep = ""))) / sum(Exposure),
      pred3 = sum(!!sym(paste("", model3, sep = ""))) / sum(Exposure)
    )
  ggplot(out, aes(x = !!sym(xvar), group = 1)) +
    geom_point(aes(y = obs, colour = "observed"), size = 0.8) +
    geom_line(aes(y = obs, colour = "observed"), linetype = "solid") +
    geom_point(aes(y = pred1, colour = model1), size = 0.8) +
    geom_line(aes(y = pred1, colour = model1)) +
    geom_point(aes(y = pred2, colour = model2), size = 0.8) +
    geom_line(aes(y = pred2, colour = model2)) +
    geom_point(aes(y = pred3, colour = model3), size = 0.8) +
    geom_line(aes(y = pred3, colour = model3)) +
    labs(x = xvar, y = "fréquence", title = title) +
    theme_bw() +
    theme(
      plot.title = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(size = 10)
    ) +
    scale_colour_manual(values = clrpal)
}
# Fonction pour afficher le graphique de fréquence avec quatre modèles
plot_freq2 <- function(data_test, xvar, title, model1, model2, model3, model4) {
  out <- data_test %>% 
    group_by(!!sym(xvar)) %>% 
    summarize(
      obs = sum(ClaimNb) / sum(Exposure),
      pred1 = sum(!!sym(paste("", model1, sep = ""))) / sum(Exposure),
      pred2 = sum(!!sym(paste("", model2, sep = ""))) / sum(Exposure),
      pred3 = sum(!!sym(paste("", model3, sep = ""))) / sum(Exposure),
      pred4 = sum(!!sym(paste("", model4, sep = ""))) / sum(Exposure)
    )
  ggplot(out, aes(x = !!sym(xvar), group = 1)) +
    geom_point(aes(y = obs, colour = "observed"), size = 0.8) +
    geom_line(aes(y = obs, colour = "observed"), linetype = "solid") +
    geom_point(aes(y = pred1, colour = model1), size = 0.8) +
    geom_line(aes(y = pred1, colour = model1)) +
    geom_point(aes(y = pred2, colour = model2), size = 0.8) +
    geom_line(aes(y = pred2, colour = model2)) +
    geom_point(aes(y = pred3, colour = model3), size = 0.8) +
    geom_line(aes(y = pred3, colour = model3)) +
    geom_point(aes(y = pred4, colour = model4), size = 0.8) +
    geom_line(aes(y = pred4, colour = model4)) +
    labs(x = xvar, y = "fréquence", title = title) +
    theme_bw() +
    theme(
      plot.title = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(size = 10)
    ) +
    scale_colour_manual(values = clrpal)
}
```

``` r
# Fonction pour prétraiter les variables catégorielles pour un modèle GLM
GLMprocess <- function(dat) {
  dat %>% mutate(
    VehPowerGLM = as.factor(pmin(VehPower, 9)),
    VehAgeGLM = cut(VehAge, breaks = c(-Inf, 0, 10, Inf), labels = c("1", "2", "3")),
    DrivAgeGLM = cut(DrivAge, breaks = c(-Inf, 20, 25, 30, 40, 50, 70, Inf), labels = c("1", "2", "3", "4", "5", "6", "7")),
    BonusMalusGLM = as.integer(pmin(BonusMalus, 150)),
    DensityGLM = as.numeric(Density),
    VehAgeGLM = relevel(VehAgeGLM, ref = "2"),   
    DrivAgeGLM = relevel(DrivAgeGLM, ref = "5")
  )
}
# Fonction pour prétraiter les variables numériques pour un modèle numérique
Numprocess <- function(dat) {
  dat %>% 
    select("Exposure", "VehPower", "VehAge", "DrivAge", "BonusMalus", "VehBrand", "VehGas", "Area", "Density", "Region") %>% 
    mutate(
      VehGas = as.numeric(VehGas),
      VehBrand = as.numeric(VehBrand),
      Region = as.numeric(Region),
      VehPower = as.numeric(VehPower),
      BonusMalus = as.integer(pmin(BonusMalus, 150)),
      Area = as.numeric(Area),
      VehAge = as.numeric(VehAge),   
      DrivAge = as.numeric(DrivAge)
    )
}
```

``` r
# Remplir le tibble avec les valeurs souhaitées
comp_tab <- tibble(
  Model = c("Model1", "Model2", "Model3"),  # Noms des modèles
  "In sample loss (training set)" = c(0.1, 0.2, 0.15),    # In sample loss (training set) pour chaque modèle
  "Out sample loss (validation set)" = c(0.25, 0.3, 0.28), # Out sample loss (validation set) pour chaque modèle
  "Out sample loss (test set)" = c(0.18, 0.22, 0.20),      # Out sample loss (test set) pour chaque modèle
  AIC = c(5, 7, 6),                      # AIC pour chaque modèle
  BIC = c(10, 12, 11)                    # BIC pour chaque modèle
)
# Afficher le tibble avec les noms de colonnes
# print(comp_tab)
```

``` r
# Fonction pour calculer la déviance de Poisson
PoissonDeviance <- function(y, ypred) {
  PoissonDeviance = 2 * (sum(ypred) - sum(y) + sum(log((y / ypred)^(y)))) / length(ypred)
}
# Fonction pour transformer le jeu de données en un jeu de données uniforme (Exposure = 1)
unif <- function(dat) {
  dat %>% mutate(Exposure = 1)
}
```

## Complot

``` r
# Fonction pour créer les graphiques de fréquence pour 2 modèles
comp_plots0 <- function(dat, model1, model2){
  # VehPower
  p1 <- plot_freq0(dat, "VehPower", "Puissance du véhicule", model1, model2)
  # DrivAge
  p2 <- plot_freq0(dat, "DrivAge", "Âge du conducteur", model1, model2)
  # VehAge
  p3 <- plot_freq0(dat, "VehAge", "Âge du véhicule", model1, model2)
  # Density
  p4 <- plot_freq0(dat, "BonusMalus", "Bonus Malus", model1, model2)
  # Afficher les graphiques en 2 colonnes
  gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2)
}
# Fonction pour créer les graphiques de fréquence pour 3 modèles
comp_plots1 <- function(dat, model1, model2, model3){
  # VehPower
  p1 <- plot_freq1(dat, "VehPower", "Puissance du véhicule", model1, model2, model3)
  # DrivAge
  p2 <- plot_freq1(dat, "DrivAge", "Âge du conducteur", model1, model2, model3)
  # VehAge
  p3 <- plot_freq1(dat, "VehAge", "Âge du véhicule", model1, model2, model3)
  # Density
  p4 <- plot_freq1(dat, "BonusMalus", "Bonus Malus", model1, model2, model3)
  # Afficher les graphiques en 2 colonnes
  gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2)
}
# Fonction pour créer les graphiques de fréquence pour 4 modèles
comp_plots2 <- function(dat, model1, model2, model3, model4){
  # VehPower
  p1 <- plot_freq2(dat, "VehPower", "Puissance du véhicule", model1, model2, model3, model4)
  # DrivAge
  p2 <- plot_freq2(dat, "DrivAge", "Âge du conducteur", model1, model2, model3, model4)
  # VehAge
  p3 <- plot_freq2(dat, "VehAge", "Âge du véhicule", model1, model2, model3, model4)
  # Density
  p4 <- plot_freq2(dat, "BonusMalus", "Bonus Malus", model1, model2, model3, model4)
  # Afficher les graphiques en 2 colonnes
  gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2)
}
```

Chargement du jeu de données `freMTPL2freq` du package `CASdatasets`

``` r
# Charger le jeu de données
load("D:/OneDrive/Bureau/Documents_M2/Ass_Non_Vie/Project/CASdatasets_1.0-11/CASdatasets/data/freMTPL2freq.rda")

# Afficher la structure des données
str(freMTPL2freq)
```

    ## 'data.frame':    678013 obs. of  12 variables:
    ##  $ IDpol     : num  1 3 5 10 11 13 15 17 18 21 ...
    ##  $ ClaimNb   : 'table' num [1:678013(1d)] 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Exposure  : num  0.1 0.77 0.75 0.09 0.84 0.52 0.45 0.27 0.71 0.15 ...
    ##  $ VehPower  : int  5 5 6 7 7 6 6 7 7 7 ...
    ##  $ VehAge    : int  0 0 2 0 0 2 2 0 0 0 ...
    ##  $ DrivAge   : int  55 55 52 46 46 38 38 33 33 41 ...
    ##  $ BonusMalus: int  50 50 50 50 50 50 50 68 68 50 ...
    ##  $ VehBrand  : Factor w/ 11 levels "B1","B10","B11",..: 4 4 4 4 4 4 4 4 4 4 ...
    ##  $ VehGas    : chr  "Regular" "Regular" "Diesel" "Diesel" ...
    ##  $ Area      : Factor w/ 6 levels "A","B","C","D",..: 4 4 2 2 2 5 5 3 3 2 ...
    ##  $ Density   : int  1217 1217 54 76 76 3003 3003 137 137 60 ...
    ##  $ Region    : Factor w/ 21 levels "Alsace","Aquitaine",..: 21 21 18 2 2 16 16 13 13 17 ...

# Exploration initiale des données

## Données manquantes

``` r
# Trouver les indices des valeurs manquantes dans le jeu de données
indices_manquants <- which(is.na(freMTPL2freq))

# Résumé du jeu de données
# Afficher un résumé des statistiques descriptives du jeu de données
summary(freMTPL2freq)
```

    ##      IDpol            ClaimNb         Exposure           VehPower     
    ##  Min.   :      1   n.vars :1       Min.   :0.002732   Min.   : 4.000  
    ##  1st Qu.:1157951   n.cases:36102   1st Qu.:0.180000   1st Qu.: 5.000  
    ##  Median :2272152                   Median :0.490000   Median : 6.000  
    ##  Mean   :2621857                   Mean   :0.528750   Mean   : 6.455  
    ##  3rd Qu.:4046274                   3rd Qu.:0.990000   3rd Qu.: 7.000  
    ##  Max.   :6114330                   Max.   :2.010000   Max.   :15.000  
    ##                                                                       
    ##      VehAge           DrivAge        BonusMalus        VehBrand     
    ##  Min.   :  0.000   Min.   : 18.0   Min.   : 50.00   B12    :166024  
    ##  1st Qu.:  2.000   1st Qu.: 34.0   1st Qu.: 50.00   B1     :162736  
    ##  Median :  6.000   Median : 44.0   Median : 50.00   B2     :159861  
    ##  Mean   :  7.044   Mean   : 45.5   Mean   : 59.76   B3     : 53395  
    ##  3rd Qu.: 11.000   3rd Qu.: 55.0   3rd Qu.: 64.00   B5     : 34753  
    ##  Max.   :100.000   Max.   :100.0   Max.   :230.00   B6     : 28548  
    ##                                                     (Other): 72696  
    ##     VehGas          Area          Density     
    ##  Length:678013      A:103957   Min.   :    1  
    ##  Class :character   B: 75459   1st Qu.:   92  
    ##  Mode  :character   C:191880   Median :  393  
    ##                     D:151596   Mean   : 1792  
    ##                     E:137167   3rd Qu.: 1658  
    ##                     F: 17954   Max.   :27000  
    ##                                               
    ##                          Region      
    ##  Centre                     :160601  
    ##  Rhone-Alpes                : 84752  
    ##  Provence-Alpes-Cotes-D'Azur: 79315  
    ##  Ile-de-France              : 69791  
    ##  Bretagne                   : 42122  
    ##  Nord-Pas-de-Calais         : 40275  
    ##  (Other)                    :201157

## Structure du portefeuille

Nous commençons par fournir des statistiques descriptives et
exploratoires des données. Cela comprend d’abord la structure du
portefeuille en termes de volumes et de statistiques clés.

## Identifiant de police (Policy id)

Tout d’abord, la variable *id* est liée à un identifiant unique de la
police. Nous pouvons vérifier que chaque police apparaît une seule fois
dans le jeu de données.

``` r
# Vérifier que chaque police apparaît une seule fois dans le jeu de données
longueur_unique_id <- length(unique(freMTPL2freq$id))
longueur_totale <- nrow(freMTPL2freq)
verification_id_unique <- longueur_unique_id == longueur_totale
sprintf("Chaque police apparaît-elle une seule fois ? : %s", verification_id_unique)
```

    ## [1] "Chaque police apparaît-elle une seule fois ? : FALSE"

## Exposition au risque (Exposure)

``` r
# Création des graphiques pour visualiser la distribution de l'exposition au risque (Exposure) et du nombre de sinistres (ClaimNb)
p1 <- ggplot(freMTPL2freq, aes(Exposure)) +
  geom_histogram(color = "#236B8E", fill = "#236B8E", bins = 30) +
  labs(x = "Exposition au risque", y = "Effectif") +
  theme_bw() +
  theme(plot.title = element_text(size = 12, face = "bold.italic")  )
p2 <- ggplot(freMTPL2freq, aes(x = "Exposition au risque", y = Exposure)) +
  geom_boxplot(color = "#236B8E", fill = "white") +
  labs(x = "Exposition au risque", y = "Fréquence") +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold.italic") )
p3 <- ggplot(freMTPL2freq, aes(ClaimNb)) +
  geom_histogram(color = "#236B8E", fill = "#236B8E", bins = 30) +
  labs(x = "Nombre de sinistres", y = "Effectif") +
  theme_minimal() +
  theme( plot.title = element_text(size = 12, face = "bold.italic")  )
# Afficher les graphiques en 3 colonnes
grid.arrange(p3, p1, p2, ncol = 3)
```

![](Mémoire_Actuariat_John_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

## Nombre de sinistres et Fréquences de sinstres

``` r
# Calculer la fréquence des sinistres du portefeuille
pf_freq <- sum(freMTPL2freq$ClaimNb) / sum(freMTPL2freq$Exposure)
# Fréquence des sinistres du portefeuille (estimation homogène)
sprintf("Fréquence des sinistres du portefeuille :  %s", round(pf_freq, 5))
```

    ## [1] "Fréquence des sinistres du portefeuille :  0.1007"

``` r
# Grouper par le nombre de sinistres et calculer le nombre total de polices dans chaque groupe
portf_gs <- freMTPL2freq %>%
  group_by(ClaimNb) %>%
  summarize(Nombre_de_polices = n()) %>%
  mutate(Proportion = round(Nombre_de_polices*100 /sum(Nombre_de_polices),2))
# Renommer les colonnes du tableau
names(portf_gs) <- c("Nombre de sinistres", "Nombre total de sinistres", "Proportion (en %)")
# Afficher le tableau
knitr::kable(portf_gs)
```

| Nombre de sinistres | Nombre total de sinistres | Proportion (en %) |
|--------------------:|--------------------------:|------------------:|
|                   0 |                    643953 |             94.98 |
|                   1 |                     32178 |              4.75 |
|                   2 |                      1784 |              0.26 |
|                   3 |                        82 |              0.01 |
|                   4 |                         7 |              0.00 |
|                   5 |                         2 |              0.00 |
|                   6 |                         1 |              0.00 |
|                   8 |                         1 |              0.00 |
|                   9 |                         1 |              0.00 |
|                  11 |                         3 |              0.00 |
|                  16 |                         1 |              0.00 |

Les variables catégorielles ici sont répertoriées comme des facteurs.
Nous avons converti le type de la variable “ClaimNb” qui était de type
tableau en type numérique pour faciliter l’analyse ultérieure.

## Prétraitement initial des données

``` r
# Convertir la variable VehGas en facteur
freMTPL2freq <- freMTPL2freq %>%
  mutate(VehGas = factor(VehGas),
         ClaimNb = as.integer(ClaimNb),
         Density = round(log(Density), 3))
```

## Analyse des données (statistiques descriptives)

``` r
# Afficher la structure des données
str(freMTPL2freq)
```

    ## 'data.frame':    678013 obs. of  12 variables:
    ##  $ IDpol     : num  1 3 5 10 11 13 15 17 18 21 ...
    ##  $ ClaimNb   : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Exposure  : num  0.1 0.77 0.75 0.09 0.84 0.52 0.45 0.27 0.71 0.15 ...
    ##  $ VehPower  : int  5 5 6 7 7 6 6 7 7 7 ...
    ##  $ VehAge    : int  0 0 2 0 0 2 2 0 0 0 ...
    ##  $ DrivAge   : int  55 55 52 46 46 38 38 33 33 41 ...
    ##  $ BonusMalus: int  50 50 50 50 50 50 50 68 68 50 ...
    ##  $ VehBrand  : Factor w/ 11 levels "B1","B10","B11",..: 4 4 4 4 4 4 4 4 4 4 ...
    ##  $ VehGas    : Factor w/ 2 levels "Diesel","Regular": 2 2 1 1 1 2 2 1 1 1 ...
    ##  $ Area      : Factor w/ 6 levels "A","B","C","D",..: 4 4 2 2 2 5 5 3 3 2 ...
    ##  $ Density   : num  7.1 7.1 3.99 4.33 4.33 ...
    ##  $ Region    : Factor w/ 21 levels "Alsace","Aquitaine",..: 21 21 18 2 2 16 16 13 13 17 ...

``` r
# Calculer la fréquence des sinistres du portefeuille
pf_freq <- sum(freMTPL2freq$ClaimNb) / sum(freMTPL2freq$Exposure)
# Afficher les graphiques exploratoires pour les variables catégorielles
M1 <- multiPlot(freMTPL2freq, "DrivAge", "Âge du conducteur")
```

![](Mémoire_Actuariat_John_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
M2 <- multiPlot(freMTPL2freq, "VehAge", "Âge du véhicule")
```

![](Mémoire_Actuariat_John_files/figure-gfm/unnamed-chunk-12-2.png)<!-- -->

``` r
M3 <- multiPlot(freMTPL2freq, "VehPower", "Puissance du véhicule")
```

![](Mémoire_Actuariat_John_files/figure-gfm/unnamed-chunk-12-3.png)<!-- -->

``` r
M4 <- multiPlotD(freMTPL2freq, "VehBrand", "Marque de véhicule")
```

![](Mémoire_Actuariat_John_files/figure-gfm/unnamed-chunk-12-4.png)<!-- -->

``` r
M5 <- multiPlotD(freMTPL2freq, "VehGas", "Type de carburant")
```

![](Mémoire_Actuariat_John_files/figure-gfm/unnamed-chunk-12-5.png)<!-- -->

``` r
M6 <- multiPlotD(freMTPL2freq, "Area", "Zone de résidence")
```

![](Mémoire_Actuariat_John_files/figure-gfm/unnamed-chunk-12-6.png)<!-- -->

``` r
M7 <- multiPlot(freMTPL2freq, "Density", "Densité")
```

![](Mémoire_Actuariat_John_files/figure-gfm/unnamed-chunk-12-7.png)<!-- -->

``` r
# Afficher les graphiques exploratoires en plusieurs colonnes
grid.arrange(M1, M2, M3, M4, ncol = 2)
```

![](Mémoire_Actuariat_John_files/figure-gfm/unnamed-chunk-12-8.png)<!-- -->

``` r
grid.arrange(M5, M6, M7, ncol = 2)
```

![](Mémoire_Actuariat_John_files/figure-gfm/unnamed-chunk-12-9.png)<!-- -->

La variable `Exposition` est un peu étrange - il semble étrange d’avoir
des polices dont la durée est supérieure à un an (sans contexte
spécifique). Dans le cadre de notre analyse nous avons jugé préférable
de traiter ces valeurs anormales et potentiellement erronées.

``` r
# Filtrer les polices avec une exposition supérieure à 1 an
etran <- freMTPL2freq %>% filter(Exposure > 1)
p1 <- ggplot(etran, aes(Exposure)) +
  geom_histogram(bins = 40, color = "#236B8E", fill = "#AED4E6") +
  ggtitle("Distribution des expositions > 1") +
  theme_bw() +
  theme(
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 9),
    plot.title = element_text(size = 9))
p2 <- ggplot(etran, aes(ClaimNb)) +
  geom_histogram(bins = 40, color = "#236B8E", fill = "#AED4E6") +
  ggtitle("Nombre de sinistres (pour Exposure > 1)") +
  theme_bw() +
  theme(
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 9),
    plot.title = element_text(size = 9))
# Afficher les graphiques côte à côte
grid.arrange(p1, p2, ncol = 2)
```

![](Mémoire_Actuariat_John_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

## Corrélation

``` r
# Calculer la matrice de corrélation de Spearman
M <- round(cor(Numprocess(freMTPL2freq), method = "spearman"), 2)
corrplot::corrplot(M, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45, hclust.method = "ward.D")
```

![](Mémoire_Actuariat_John_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
# Calculer la matrice de corrélation de Pearson
M <- round(cor(Numprocess(freMTPL2freq), method = "pearson"), 2)
corrplot::corrplot(M, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45, hclust.method = "ward.D")
```

![](Mémoire_Actuariat_John_files/figure-gfm/unnamed-chunk-14-2.png)<!-- -->

Ces étapes permettent d’effectuer une exploration préliminaire des
données et d’identifier des aspects importants tels que la fréquence des
sinistres dans le portefeuille et les corrélations entre les variables.
Les graphiques et les analyses de corrélation fournissent une meilleure
compréhension des caractéristiques du jeu de données et aident à
orienter les analyses et les modélisations ultérieures.

# Sépartion du jeu de données

``` r
# Séparation du jeu de données en ensembles d'apprentissage, de test et de validation
ids <- partition(freMTPL2freq$ClaimNb, p = c(train = 0.6, valid = 0.2, test = 0.2), seed = seed)
dl = freMTPL2freq[ids$train,]
dt = freMTPL2freq[ids$test,]
dv = freMTPL2freq[ids$valid,]

# Exclusion des polices d'assurance avec une durée d'exposition supérieure à un an.
datalearning <- dl[dl$Exposure <= 1,]
datatest <- dt[dt$Exposure <= 1,]
datavalid <- dv[dv$Exposure <= 1,]
#En filtrant les polices avec une exposition inférieure ou égale à un an, nous nous concentrons sur les données les plus pertinentes pour l'analyse. Cela nous permet d'éviter toute distorsion due aux valeurs atypiques dans la variable Exposition et de travailler sur des données plus représentatives du portefeuille d'assurance automobile.

# Uniformisation de l'exposition pour chaque ensemble (la variable Exposure est mise à 1)
datalearning_1 <- unif(datalearning)
datatest_1 <- unif(datatest)
datavalid_1 <- unif(datavalid)

# Calcul des proportions de sinistres par rapport à l'exposition dans chaque ensemble
prop_claim_datalearning <- sum(datalearning$ClaimNb) / sum(datalearning$Exposure)
prop_claim_datatest <- sum(datatest$ClaimNb) / sum(datatest$Exposure)
prop_claim_datavalid <- sum(datavalid$ClaimNb) / sum(datavalid$Exposure)

prop_claim_datalearning
```

    ## [1] 0.100329

``` r
prop_claim_datatest
```

    ## [1] 0.104051

``` r
prop_claim_datavalid
```

    ## [1] 0.09964636

Les proportions de sinistres par rapport à l’exposition dans chaque
ensemble sont respectivement :

- Ensemble d’apprentissage : `prop_claim_datalearning`
- Ensemble de test : `prop_claim_datatest`
- Ensemble de validation : `prop_claim_datavalid`

Ces proportions peuvent être utiles pour évaluer la fréquence des
sinistres dans chaque ensemble et s’assurer de leur représentativité
dans l’ensemble des données.

# Modélisation

## Modèle Linéaire Généralisé (GLM) & Modèle Additif Généralisé (GAM)

``` r
# Modèle GLM de base
Model_0 <- glm(ClaimNb ~ 1 + offset(log(Exposure)), family = poisson, data = datalearning)

# Modèle GLM avec des variables explicatives
Model_glm <- glm(ClaimNb ~ VehPower + DrivAge, family = poisson, data = datalearning)

# Modèle GAM avec des variables explicatives lissées
Model_gam <- gam(ClaimNb ~ s(DrivAge) + s(VehPower), family = poisson, data = datalearning)

# Graphique prédiction GLM
x_tilde <- datalearning
x_tilde$ClaimNb <- predict(Model_glm, x_tilde, type = "response")
wireframe(ClaimNb ~ VehPower + DrivAge, data = x_tilde,
          xlab = "VehPower",
          ylab = "DrivAge",
          main = "Predicted count with GLM",
          drape = TRUE,
          colorkey = TRUE)
```

![](Mémoire_Actuariat_John_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
# Graphique prédiction GAM
x_tilde <- datalearning
x_tilde$ClaimNb <- predict(Model_gam, x_tilde, type = "response")
wireframe(ClaimNb ~ VehPower + DrivAge, data = x_tilde,
          xlab = "VehPower",
          ylab = "DrivAge",
          main = "Predicted count with GAM",
          drape = TRUE,
          colorkey = TRUE)
```

![](Mémoire_Actuariat_John_files/figure-gfm/unnamed-chunk-16-2.png)<!-- -->

``` r
# Modèle GLM avec des variables explicatives transformées
Model_GLM. <- glm(ClaimNb ~ VehPowerGLM + DrivAgeGLM, family = poisson, data = GLMprocess(datalearning))

# Graphique prédiction GLM avec des variables transformées
x_tilde <- GLMprocess(datalearning)
x_tilde$ClaimNb <- predict(Model_GLM., x_tilde, type = "response")
wireframe(ClaimNb ~ VehPowerGLM + DrivAgeGLM, data = x_tilde,
          xlab = "VehPower",
          ylab = "DrivAge",
          main = "Predicted count with GLM",
          drape = TRUE,
          colorkey = TRUE)
```

![](Mémoire_Actuariat_John_files/figure-gfm/unnamed-chunk-16-3.png)<!-- -->

### Selection

``` r
Model_0 = glm(ClaimNb~ 1 + offset(log(Exposure)), family=poisson,data=datalearning)

# Modèle GLM de base (GLM)
Model_GLM = glm(ClaimNb ~ VehPower + VehAge + DrivAge + BonusMalus + Density + VehBrand + VehGas + Area + Region + offset(log(Exposure)), family = poisson, data = datalearning)
datalearning$GLM <- predict(Model_GLM, newdata = datalearning, type = "response")
datatest$GLM <- predict(Model_GLM, newdata = datatest, type = "response")
datavalid$GLM <- predict(Model_GLM, newdata = datavalid, type = "response")

comp_tab[1, ] <- list("GLM", round(PoissonDeviance(datalearning$ClaimNb, datalearning$GLM), 6),
                     round(PoissonDeviance(datavalid$ClaimNb, datavalid$GLM), 6),
                     round(PoissonDeviance(datatest$ClaimNb, datatest$GLM), 6), AIC(Model_GLM), BIC(Model_GLM))

# Modèle GLM avec discrétisation (GLM avec discrétisation)
Model_GLM_ = glm(ClaimNb ~ VehPowerGLM + VehAgeGLM + BonusMalusGLM + VehBrand + VehGas + DensityGLM + Region + Area + DrivAgeGLM + offset(log(Exposure)), family = poisson, data = GLMprocess(datalearning))
datalearning$GLM_ <- predict(Model_GLM_, newdata = GLMprocess(datalearning), type = "response")
datatest$GLM_ <- predict(Model_GLM_, newdata = GLMprocess(datatest), type = "response")
datavalid$GLM_ <- predict(Model_GLM_, newdata = GLMprocess(datavalid), type = "response")

comp_tab[2, ] <- list("GLM (avec discrétisation)", round(PoissonDeviance(datalearning$ClaimNb, datalearning$GLM_), 6),
                     round(PoissonDeviance(datavalid$ClaimNb, datavalid$GLM_), 6),
                     round(PoissonDeviance(datatest$ClaimNb, datatest$GLM_), 6), AIC(Model_GLM_), BIC(Model_GLM_))

# Modèle GAM (GAM)
Model_GAM = gam(ClaimNb ~ s(VehPower) + s(VehAge) + s(DrivAge) + s(BonusMalus) + s(Density) + VehBrand + VehGas + Area + Region + offset(log(Exposure)), family = poisson, data = datalearning)
datalearning$GAM <- predict(Model_GAM, newdata = datalearning, type = "response")
datatest$GAM <- predict(Model_GAM, newdata = datatest, type = "response")
datavalid$GAM <- predict(Model_GAM, newdata = datavalid, type = "response")

comp_tab[3, ] <- list("GAM", round(PoissonDeviance(datalearning$ClaimNb, datalearning$GAM), 6),
                     round(PoissonDeviance(datatest$ClaimNb, datatest$GAM), 6),
                     round(PoissonDeviance(datavalid$ClaimNb, datavalid$GAM), 6), AIC(Model_GAM), BIC(Model_GAM))

# Résumé des modèles
# summary(Model_GLM)
# summary(Model_GLM_)
# summary(Model_GAM)

# Affichage du tableau comparatif
knitr::kable(comp_tab)
```

| Model                     | In sample loss (training set) | Out sample loss (validation set) | Out sample loss (test set) |      AIC |      BIC |
|:--------------------------|------------------------------:|---------------------------------:|---------------------------:|---------:|---------:|
| GLM                       |                      0.318888 |                         0.317176 |                   0.328850 | 170861.3 | 171319.7 |
| GLM (avec discrétisation) |                      0.313163 |                         0.310890 |                   0.321766 | 168556.6 | 169124.1 |
| GAM                       |                      0.316126 |                         0.326025 |                   0.314636 | 169797.6 | 170570.9 |

### Comparaison GLM, GLM avec discrétisation, et GAM

``` r
comp_plots1(datatest, "GLM", "GLM_", "GAM")
```

![](Mémoire_Actuariat_John_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

## GBM tuning

``` r
# set.seed(seed)
# tuning_grid = expand.grid(interaction.depth = 1:10, n.minobsinnode =1, shrinkage= 0.01)
# features <- names(datalearning)[4:12]
# formul <- as.formula(paste("ClaimNb ~",paste(features[!features %in% "datalearning"], collapse = " + "), paste(" + offset(log(Exposure)) ")))
# gbm.res = list()
# library(progress) # package needed for progress bar
# pb <- progress_bar$new(total = nrow(tuning_grid))
# for (nb.it in 1:nrow(tuning_grid)){
# pb$tick()
# set.seed(seed)
# gbm.res[[nb.it]] <- gbm3::gbmt(formul,
# data=datalearning,
# var_monotone=rep(0,length=9),
# distribution=gbm_dist("Poisson"),
# train_params = training_params(
# num_trees = 15000,
# shrinkage=tuning_grid[nb.it,]$shrinkage,
# interaction_depth=tuning_grid[nb.it,]$interaction.depth,
# bag_fraction = 0.5,
# num_train = 1*nrow(datalearning),
# min_num_obs_in_node = tuning_grid[nb.it,]$n.minobsinnode),
# keep_gbm_data = TRUE,
# par_details = gbmParallel(num_threads = 10),
# is_verbose = TRUE,
# cv_folds = 10)
# Sys.sleep(1/nrow(tuning_grid)) }
# save(gbm.res, file = "GBM_tuning")
```

``` r
# Définition de la grille de recherche des hyperparamètres
tuning_grid = expand.grid(interaction.depth = 1:10, n.minobsinnode = 1, shrinkage = 0.01)

# Chargement des résultats de tuning GBM préalablement sauvegardés (pour gagner du temps)
load("GBM_tuning")

# Visualisation des performances des modèles GBM avec différentes valeurs d'hyperparamètres
par(mfrow = c(1, 3))
plot(gbmt_performance(gbm.res[[10]], method = "cv"))
nb.trees.opt = vector(length = nrow(tuning_grid))
for (nb.it in 1:nrow(tuning_grid)) {
  nb.trees.opt[nb.it] <- print(gbmt_performance(gbm.res[[nb.it]], method = "cv"))}
```

    ## The best cross-validation iteration was 14999.
    ## The best cross-validation iteration was 14978.
    ## The best cross-validation iteration was 14657.
    ## The best cross-validation iteration was 13874.
    ## The best cross-validation iteration was 7221.
    ## The best cross-validation iteration was 5081.
    ## The best cross-validation iteration was 4779.
    ## The best cross-validation iteration was 3802.
    ## The best cross-validation iteration was 3453.
    ## The best cross-validation iteration was 3601.

``` r
plot(nb.trees.opt, xlab = "Interaction depth", ylab = "Optimal number of trees")
pred.error = vector(length = nrow(tuning_grid))
for (nb.it in 1:nrow(tuning_grid)) {
  pred.error[nb.it] <- gbm.res[[nb.it]]$cv_error[nb.trees.opt[nb.it]]}
plot(pred.error, xlab = "Interaction depth", ylab = "Prediction error", col = "blue")
```

![](Mémoire_Actuariat_John_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
par(mfrow = c(1, 1))
```

``` r
# Création du data.frame pour les graphiques
df <- data.frame(
  Interaction_Depth = 1:nrow(tuning_grid),
  Optimal_Number_of_Trees = nb.trees.opt,
  Prediction_Error = pred.error)

# Graphique 1 - Nombre optimal d'arbres
p1 <- ggplot(df, aes(x = Interaction_Depth, y = Optimal_Number_of_Trees)) +
  geom_line(colour = clrpal[1]) +
  geom_point(colour = clrpal[1], size = 3) +
  geom_hline(yintercept = min(nb.trees.opt), colour = "red", linetype = "dashed") +
  labs(x = "Interaction Depth", y = "Optimal Number of Trees") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10)
  ) +
  scale_x_continuous(breaks = seq(1, nrow(tuning_grid), 1))

# Graphique 2 - Erreur de prédiction
p2 <- ggplot(df, aes(x = Interaction_Depth, y = Prediction_Error)) +
  geom_line(colour = clrpal[2]) +
  geom_point(colour = clrpal[2], size = 3) +
  geom_hline(yintercept = min(pred.error), colour = "red", linetype = "dashed") +
  labs(x = "Interaction Depth", y = "Prediction Error") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10)) +
  scale_x_continuous(breaks = seq(1, nrow(tuning_grid), 1))

# Affichage côte à côte des graphiques
grid.arrange(p1, p2, ncol = 2)
```

![](Mémoire_Actuariat_John_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

``` r
# GBM Final
features <- names(datalearning)[4:12]
formul <- as.formula(paste("ClaimNb ~",paste(features[!features %in% "datalearning"], collapse = " + "), paste(" + offset(log(Exposure)) ")))
nb.trees.fin <- nb.trees.opt[which(pred.error == min(pred.error))]
best.iter <- which(pred.error == min(pred.error))
set.seed(seed)
GBM <- gbmt(formul,
data=datalearning,
var_monotone=rep(0,length=9),
distribution=gbm_dist("Poisson"),
train_params = training_params(
num_trees = nb.trees.fin,
shrinkage=0.01,
interaction_depth=best.iter,
bag_fraction = 0.5,
num_train = 1*nrow(datalearning),
min_num_obs_in_node = 1),
keep_gbm_data = TRUE,
par_details = gbmParallel(num_threads = 2),
is_verbose = FALSE)
```

### Importances relatives et dépendance partielle

``` r
# Afficher le résumé de l'importance des variables
summary(GBM)
```

![](Mémoire_Actuariat_John_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

    ##                   var   rel_inf
    ## BonusMalus BonusMalus 27.161341
    ## VehAge         VehAge 22.302340
    ## DrivAge       DrivAge 10.577316
    ## Region         Region 10.212314
    ## VehBrand     VehBrand 10.138920
    ## Density       Density  7.090605
    ## VehPower     VehPower  5.953481
    ## VehGas         VehGas  3.307851
    ## Area             Area  3.255833

``` r
# Configuration du tracé en plusieurs colonnes
par(mfrow = c(2, 4))

# Parcours des variables et création des graphiques
for (i in 1:length(features)) {
  # Création du graphique avec plot()
  plot(GBM, var_index = i, num_trees = nb.trees.fin, type = "response")
  
  # Modification des paramètres esthétiques
  par(mar = c(4, 4, 2, 1))  # Ajuster les marges
}
```

![](Mémoire_Actuariat_John_files/figure-gfm/unnamed-chunk-23-2.png)<!-- -->

``` r
# Rétablir la configuration par défaut
par(mfrow = c(1, 1))
```

![](Mémoire_Actuariat_John_files/figure-gfm/unnamed-chunk-23-3.png)<!-- -->

### Interactions

``` r
# Création d'une matrice vide de dimension 9x9
# res = matrix(NA, 9, 9)

# Boucles pour évaluer les interactions entre les variables
# for (i in 1:8){
  # for (j in (i + 1):9){
    # Utilisation de la fonction interact pour évaluer l'interaction entre les variables i et j dans le modèle GBM
    # res[i, j] = interact(gbm_fit_obj = GBM, data = datalearning, var_indices = c(i, j), nb.trees.fin)
  # }
# }

# Remplissage de la diagonale de la matrice avec des zéros (car l'interaction d'une variable avec elle-même est nulle)
# diag(res) = 0

# Définition des noms de lignes et de colonnes de la matrice avec les noms des variables (features)
# row.names(res) = features
# colnames(res) = row.names(res)

# Conversion de la matrice en un format long (melt) pour une visualisation plus facile dans un graphique
# interact_melt <- data.table::melt(res, na.rm = TRUE)

load("res_interact")

# Création d'un graphique de type heatmap pour visualiser les interactions entre les variables
ggplot(data = interact_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#AED4E6", high = "blue", name = "Friedman's\nH-statistic") +
  xlab(" ") +
  ylab(" ") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
        plot.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 10)) +
  coord_fixed()
```

![](Mémoire_Actuariat_John_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

### Comparaison GLM & GAM & GBM

``` r
# Prédiction de la variable GBM sur le jeu de données datalearning à l'aide du modèle GBM
datalearning$GBM <- predict(GBM, newdata = datalearning, n.trees = nb.trees.fin, type = "response") * datalearning$Exposure

# Prédiction de la variable GBM sur le jeu de données datatest à l'aide du modèle GBM
datatest$GBM <- predict(GBM, newdata = datatest, n.trees = nb.trees.fin, type = "response") * datatest$Exposure

# Prédiction de la variable GBM sur le jeu de données datavalid à l'aide du modèle GBM
datavalid$GBM <- predict(GBM, newdata = datavalid, n.trees = nb.trees.fin, type = "response") * datavalid$Exposure

# Mise à jour de la ligne 4 de la table comp_tab avec les déviations de Poisson pour GBM
comp_tab[4, ] <- list("GBM", round(PoissonDeviance(datalearning$ClaimNb, datalearning$GBM), 6),
                   round(PoissonDeviance(datatest$ClaimNb, datatest$GBM), 6),
                   round(PoissonDeviance(datavalid$ClaimNb, datavalid$GBM), 6), NaN, NaN)

# Définition des couleurs pour les graphiques de comparaison
clrpal <- c("blue3", "lightgreen", "lightblue", "gray5", "gray77", "gray50", "#6CA6CD", "#8AB6CD", "#1F77B4", "#4682B4", "#4C72B0")

# Affichage des graphiques de comparaison en utilisant la fonction comp_plots2
comp_plots2(datatest, "GLM", "GLM_", "GAM", "GBM")
```

![](Mémoire_Actuariat_John_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

``` r
# Affichage de la table comp_tab sous forme de tableau en utilisant la fonction knitr::kable
knitr::kable(comp_tab)
```

| Model                     | In sample loss (training set) | Out sample loss (validation set) | Out sample loss (test set) |      AIC |      BIC |
|:--------------------------|------------------------------:|---------------------------------:|---------------------------:|---------:|---------:|
| GLM                       |                      0.318888 |                         0.317176 |                   0.328850 | 170861.3 | 171319.7 |
| GLM (avec discrétisation) |                      0.313163 |                         0.310890 |                   0.321766 | 168556.6 | 169124.1 |
| GAM                       |                      0.316126 |                         0.326025 |                   0.314636 | 169797.6 | 170570.9 |
| GBM                       |                      0.298511 |                         0.313775 |                   0.303663 |      NaN |      NaN |

# Regréssion locale

## Diagnostic of the prediction

``` r
# Définition de la fonction desc(y) qui calcule la moyenne, le 10ème et le 90ème percentile d'un vecteur y
desc = function(y){
  m = c(mean(y), quantile(y, c(.1, .9)))
  m = round(m, 4)
  names(m) = c("mean", "10%", "90%")
  m
}

# Calcul des statistiques pour les variables GLM, GAM et GBM dans le jeu de données datavalid
desc(datavalid$GLM)
```

    ##   mean    10%    90% 
    ## 0.0531 0.0064 0.1023

``` r
desc(datavalid$GAM)
```

    ##   mean    10%    90% 
    ## 0.0530 0.0063 0.1050

``` r
desc(datavalid$GBM)
```

    ##   mean    10%    90% 
    ## 0.0528 0.0061 0.0992

``` r
# Calcul de la moyenne des prédictions du modèle Model_0 pour le jeu de données datavalid
mean(predict(Model_0, newdata = datavalid, type = "response"))
```

    ## [1] 0.05303401

``` r
# Calcul des statistiques pour les variables GLM, GAM et GBM dans le jeu de données datatest
desc(datatest$GLM)
```

    ##   mean    10%    90% 
    ## 0.0530 0.0063 0.1024

``` r
desc(datatest$GAM)
```

    ##   mean    10%    90% 
    ## 0.0531 0.0063 0.1050

``` r
desc(datatest$GBM)
```

    ##   mean    10%    90% 
    ## 0.0530 0.0060 0.0999

``` r
# Calcul de la moyenne des prédictions du modèle Model_0 pour le jeu de données datatest
mean(predict(Model_0, newdata = datatest, type = "response"))
```

    ## [1] 0.05292278

``` r
# Calcul des statistiques pour les variables GLM, GAM et GBM dans le jeu de données datalearning
desc(datalearning$GLM)
```

    ##   mean    10%    90% 
    ## 0.0529 0.0063 0.1020

``` r
desc(datalearning$GAM)
```

    ##   mean    10%    90% 
    ## 0.0529 0.0063 0.1045

``` r
desc(datalearning$GBM)
```

    ##   mean    10%    90% 
    ## 0.0528 0.0060 0.0994

``` r
# Calcul de la moyenne des prédictions du modèle Model_0 pour le jeu de données datalearning
mean(predict(Model_0, newdata = datalearning, type = "response"))
```

    ## [1] 0.05291906

# ditribution of premium

``` r
# Définir le thème des graphiques
theme_set(theme_bw())

# Définir les couleurs à utiliser
clrpal <- c("#236B8E", "gray37", "#AED4E6", "gray77", "#64B5CD", "gray50", "#6CA6CD", "#8AB6CD", "#1F77B4", "#4682B4", "gray15", "#4C72B0")

# Créer l'histogramme pour la variable GLM
p1 <- ggplot(datavalid, aes(x = GLM)) +
  geom_histogram(color = "black", fill = clrpal[1]) +
  labs(x = "Fréquence (GLM)", title = "") +
  xlim(0, 0.3) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10))

# Créer l'histogramme pour la variable GAM
p2 <- ggplot(datavalid, aes(x = GAM)) +
  geom_histogram( color = "black", fill = clrpal[3]) +
  labs(x = "Fréquence (GAM)", title = "") +
  xlim(0, 0.3) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10))

# Créer l'histogramme pour la variable GBM
p3 <- ggplot(datavalid, aes(x = GBM)) +
  geom_histogram(color = "black", fill = clrpal[12]) +
  labs(x = "Fréquence (GBM)", title = "") +
  xlim(0, 0.3) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10))

# Afficher les histogrammes côte à côte
library(gridExtra)
grid.arrange(p1, p2, p3, ncol = 3)
```

![](Mémoire_Actuariat_John_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

``` r
# Charger la bibliothèque locfit
library(locfit)

# Séquence des valeurs alpha pour la validation croisée
alphas <- seq(0.01, 0.5, by = 0.001)

# Calculer les graphiques de validation croisée pour le modèle GLM
A_1 <- lcvplot(x = datavalid$GLM, y = datavalid$ClaimNb, weights = datavalid[,"Exposure"], kern = "rect", deg = 0, alpha = alphas)

# Calculer les graphiques de validation croisée pour le modèle GAM
A_2 <- lcvplot(x = datavalid$GAM, y = datavalid$ClaimNb, weights = datavalid[,"Exposure"], kern = "rect", deg = 0, alpha = alphas)

# Calculer les graphiques de validation croisée pour le modèle GBM
A_3 <- lcvplot(x = datavalid$GBM, y = datavalid$ClaimNb, weights = datavalid[,"Exposure"], kern = "rect", deg = 0, alpha = alphas)

# Diviser la fenêtre graphique en 4 pour afficher les graphiques côte à côte
par(mfrow = c(1, 4))

# Afficher le graphique de validation croisée pour le modèle GLM
plot(A_1, xlab = "Fitted DF", ylab = A_1$cri)

# Afficher le graphique de validation croisée pour le modèle GAM
plot(A_2, xlab = "Fitted DF", ylab = A_2$cri)

# Afficher le graphique de validation croisée pour le modèle GBM
plot(A_3, xlab = "Fitted DF", ylab = A_3$cri)

# Remettre la configuration de la fenêtre graphique à la normale
par(mfrow = c(1, 1))
```

![](Mémoire_Actuariat_John_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

# Régression locale

``` r
# Sous-échantillonner datavalid pour ne contenir que les 10 000 premières observations
# datavalid <- datavalid[1:10000,]

# Définir les valeurs alpha pour chaque modèle
alphas = 0.05
alphas_ = 0.05
alphas__ = 0.05

# Ajustement locfit pour le modèle GLM
fit_loc_GLM = locfit.raw(x=datavalid$GLM, y=datavalid[,"ClaimNb"], weights=datavalid[,"Exposure"], kern="rect", deg=0, alpha=alphas)

# Ajustement locfit pour le modèle GAM
fit_loc_GAM = locfit.raw(x=datavalid$GAM, y=datavalid[,"ClaimNb"], weights=datavalid[,"Exposure"], kern="rect", deg=0, alpha=alphas_)

# Ajustement locfit pour le modèle GBM
fit_loc_GBM = locfit.raw(x=datavalid$GBM, y=datavalid[,"ClaimNb"], weights=datavalid[,"Exposure"], kern="rect", deg=0, alpha=alphas__)
```

## Diagnostic de la regression

``` r
# Diviser la fenêtre graphique en une seule rangée avec quatre colonnes
par(mfrow=c(1,4))

# Tracer le graphique pour fit_loc_GLM avec les données colorées en clrpal[1]
# Définir les limites de l'axe x de 0 à 0.2 et l'axe y de 0 à 0.2
plot(fit_loc_GLM, col=clrpal[1], xlim=c(0,.2), xlab="Fréquence (GLM)", ylab="", ylim=c(0,.2))
# Tracer une ligne diagonale en pointillés avec une pente de 1 et une interception de 0 pour faciliter la comparaison
abline(a=0, b=1, lwd=.4)

# Tracer le graphique pour fit_loc_GAM avec les données colorées en clrpal[3]
# Définir les limites de l'axe x de 0 à 0.2 et l'axe y de 0 à 0.2
plot(fit_loc_GAM, col=clrpal[3], xlim=c(0,.2), xlab="Fréquence (GAM)", ylab="", ylim=c(0,.2))
# Tracer une ligne diagonale en pointillés avec une pente de 1 et une interception de 0 pour faciliter la comparaison
abline(a=0, b=1, lwd=.4)

# Tracer le graphique pour fit_loc_GBM avec les données colorées en clrpal[2]
# Définir les limites de l'axe x de 0 à 0.2 et l'axe y de 0 à 0.2
plot(fit_loc_GBM, col=clrpal[2], xlim=c(0,.2), xlab="Fréquence (GBM)", ylab="", ylim=c(0,.2))
# Tracer une ligne diagonale en pointillés avec une pente de 1 et une interception de 0 pour faciliter la comparaison
abline(a=0, b=1, lwd=.4)
```

![](Mémoire_Actuariat_John_files/figure-gfm/unnamed-chunk-30-1.png)<!-- -->

``` r
# Définir le thème du graphique en noir et blanc
theme_set(theme_bw())

# Définir les couleurs à utiliser dans les graphiques
clrpal <- c("#236B8E", "gray37", "#AED4E6", "gray77", "#64B5CD")

# Calculer les quantiles de fréquence pour chaque modèle (GLM, GAM et GBM) à l'aide d'une boucle
vx <- seq(0, .2, length = 251)
q_GLM <- q_GAM <- q_GBM <- rep(NA, length(vx))

for (i in 1:length(vx)) {
  q_GLM[i] <- mean(datavalid$GLM <= vx[i])
  q_GAM[i] <- mean(datavalid$GAM <= vx[i])
  q_GBM[i] <- mean(datavalid$GBM <= vx[i])
}

# Créer des data frames pour chaque modèle avec les quantiles de fréquence et les prédictions de locfit
df_GLM <- data.frame(Quantile = q_GLM, vy_GLM = predict(fit_loc_GLM, newdata = vx) - vx, vy_GLM. = predict(fit_loc_GLM, newdata = vx)/vx)
df_GAM <- data.frame(Quantile = q_GAM, vy_GAM = predict(fit_loc_GAM, newdata = vx) - vx, vy_GAM. = predict(fit_loc_GAM, newdata = vx)/vx)
df_GBM <- data.frame(Quantile = q_GBM, vy_GBM = predict(fit_loc_GBM, newdata = vx) - vx, vy_GBM. = predict(fit_loc_GBM, newdata = vx)/vx)

# Tracer le premier graphique à l'aide de ggplot2 avec les données de df_GLM
p1 <- ggplot(df_GLM, aes(x = Quantile, y = vy_GLM)) +
  geom_line(color = clrpal[1]) +
  labs(x = "Quantile de fréquence (GLM)", y = "", title = "") +
  xlim(0, 1) +
  ylim(-0.1, 0.1) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.4)

# Tracer le deuxième graphique à l'aide de ggplot2 avec les données de df_GAM
p2 <- ggplot(df_GAM, aes(x = Quantile, y = vy_GAM)) +
  geom_line(color = clrpal[3]) +
  labs(x = "Quantile de fréquence (GAM)", y = "", title = "") +
  xlim(0, 1) +
  ylim(-0.1, 0.1) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.4)

# Tracer le troisième graphique à l'aide de ggplot2 avec les données de df_GBM
p3 <- ggplot(df_GBM, aes(x = Quantile, y = vy_GBM)) +
  geom_line(color = clrpal[5]) +
  labs(x = "Quantile de fréquence (GBM)", y = "", title = "") +
  xlim(0, 1) +
  ylim(-0.1, 0.1) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.4)

# Afficher les graphiques côte à côte en utilisant la fonction grid.arrange de gridExtra
library(gridExtra)
grid.arrange(p1, p2, p3, ncol = 3)
```

![](Mémoire_Actuariat_John_files/figure-gfm/unnamed-chunk-31-1.png)<!-- -->

``` r
# Tracer le premier graphique à l'aide de ggplot2 avec les données de df_GLM
p1 <- ggplot(df_GLM, aes(x = Quantile, y = vy_GLM.)) +
  geom_line(color = clrpal[1]) +
  labs(x = "Quantile de fréquence (GLM)", y = "", title = "") +
  xlim(0, 1) +
  ylim(0.5, 1.6) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", size = 0.4)

# Tracer le deuxième graphique à l'aide de ggplot2 avec les données de df_GAM
p2 <- ggplot(df_GAM, aes(x = Quantile, y = vy_GAM.)) +
  geom_line(color = clrpal[3]) +
  labs(x = "Quantile de fréquence (GAM)", y = "", title = "") +
  xlim(0, 1) +
  ylim(0.5, 1.6) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", size = 0.4)

# Tracer le troisième graphique à l'aide de ggplot2 avec les données de df_GBM
p3 <- ggplot(df_GBM, aes(x = Quantile, y = vy_GBM.)) +
  geom_line(color = clrpal[5]) +
  labs(x = "Quantile de fréquence (GBM)", y = "", title = "") +
  xlim(0, 1) +
  ylim(0.5, 1.6) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", size = 0.4)

# Afficher les graphiques côte à côte en utilisant la fonction grid.arrange de gridExtra
library(gridExtra)
grid.arrange(p1, p2, p3, ncol = 3)
```

![](Mémoire_Actuariat_John_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->

# Correction des primes (Autocallibration)

``` r
# Utilise les modèles de locfit pour effectuer des prédictions sur les données de 'datavalid' avec les variables GLM, GAM et GBM.
p_GLM = predict(fit_loc_GLM, newdata = datavalid$GLM)
p_GAM = predict(fit_loc_GAM, newdata = datavalid$GAM)
p_GBM = predict(fit_loc_GBM, newdata = datavalid$GBM)

# Ajuste les valeurs de 'GLM_BC', 'GAM_BC' et 'GBM_BC' en multipliant les valeurs respectives de 'GLM', 'GAM' et 'GBM' par les prédictions précédemment calculées.
datavalid$GLM_BC = datavalid$GLM * (p_GLM / datavalid$GLM)
datavalid$GAM_BC = datavalid$GAM * (p_GAM / datavalid$GAM)
datavalid$GBM_BC = datavalid$GBM * (p_GBM / datavalid$GBM)

# Répète le processus précédent pour les données de 'datatest'.
p_GLM = predict(fit_loc_GLM, newdata = datatest$GLM)
p_GAM = predict(fit_loc_GAM, newdata = datatest$GAM)
p_GBM = predict(fit_loc_GBM, newdata = datatest$GBM)

datatest$GLM_BC = datatest$GLM * (p_GLM / datatest$GLM)
datatest$GAM_BC = datatest$GAM * (p_GAM / datatest$GAM)
datatest$GBM_BC = datatest$GBM * (p_GBM / datatest$GBM)

# Répète le processus précédent pour les données de 'datalearning'.
p_GLM = predict(fit_loc_GLM, newdata = datalearning$GLM)
p_GAM = predict(fit_loc_GAM, newdata = datalearning$GAM)
p_GBM = predict(fit_loc_GBM, newdata = datalearning$GBM)

datalearning$GLM_BC = datalearning$GLM * (p_GLM / datalearning$GLM)
datalearning$GAM_BC = datalearning$GAM * (p_GAM / datalearning$GAM)
datalearning$GBM_BC = datalearning$GBM * (p_GBM / datalearning$GBM)

# Crée des vecteurs vides pour stocker les quantiles des variables GLM_BC, GAM_BC et GBM_BC.
q_GLM_BC = q_GAM_BC = q_GBM_BC = rep(NA, length(vx))
```

``` r
# Crée des vecteurs vides pour stocker les quantiles des variables GLM_BC, GAM_BC et GBM_BC.
q_GLM_BC = q_GAM_BC = q_GBM_BC = rep(NA, length(vx))

# Boucle pour calculer les quantiles pour chaque valeur de vx.
for(i in 1:length(vx)){
  # Calcule la proportion d'observations de la variable 'GLM_BC' inférieures ou égales à vx[i].
  q_GLM_BC[i] = mean(datavalid$GLM_BC <= vx[i])

  # Calcule la proportion d'observations de la variable 'GAM_BC' inférieures ou égales à vx[i].
  q_GAM_BC[i] = mean(datavalid$GAM_BC <= vx[i])

  # Calcule la proportion d'observations de la variable 'GBM_BC' inférieures ou égales à vx[i].
  q_GBM_BC[i] = mean(datavalid$GBM_BC <= vx[i])
}
```

# Diagnostic autocalibration

``` r
# Obtient un résumé statistique de la variable 'GLM_BC' dans l'ensemble de données 'datavalid'.
desc(datavalid$GLM_BC)
```

    ##   mean    10%    90% 
    ## 0.0498 0.0206 0.0814

``` r
# Obtient un résumé statistique de la variable 'GAM_BC' dans l'ensemble de données 'datavalid'.
desc(datavalid$GAM_BC)
```

    ##   mean    10%    90% 
    ## 0.0496 0.0189 0.0839

``` r
# Obtient un résumé statistique de la variable 'GBM_BC' dans l'ensemble de données 'datavalid'.
desc(datavalid$GBM_BC)
```

    ##   mean    10%    90% 
    ## 0.0483 0.0170 0.0791

``` r
# Obtient un résumé statistique de la variable 'GLM_BC' dans l'ensemble de données 'datatest'.
desc(datatest$GLM_BC)
```

    ##   mean    10%    90% 
    ## 0.0498 0.0206 0.0814

``` r
# Obtient un résumé statistique de la variable 'GAM_BC' dans l'ensemble de données 'datatest'.
desc(datatest$GAM_BC)
```

    ##   mean    10%    90% 
    ## 0.0497 0.0189 0.0839

``` r
# Obtient un résumé statistique de la variable 'GBM_BC' dans l'ensemble de données 'datatest'.
desc(datatest$GBM_BC)
```

    ##   mean    10%    90% 
    ## 0.0484 0.0170 0.0796

``` r
# Obtient un résumé statistique de la variable 'GLM_BC' dans l'ensemble de données 'datalearning'.
desc(datalearning$GLM_BC)
```

    ##   mean    10%    90% 
    ## 0.0497 0.0206 0.0813

``` r
# Obtient un résumé statistique de la variable 'GAM_BC' dans l'ensemble de données 'datalearning'.
desc(datalearning$GAM_BC)
```

    ##   mean    10%    90% 
    ## 0.0496 0.0189 0.0839

``` r
# Obtient un résumé statistique de la variable 'GBM_BC' dans l'ensemble de données 'datalearning'.
desc(datalearning$GBM_BC)
```

    ##   mean    10%    90% 
    ## 0.0483 0.0170 0.0792

``` r
# Pour l'ensemble de données 'datavalid', on utilise la fonction locfit.raw pour ajuster un modèle local non-paramétrique
# en utilisant 'GLM_BC' comme variable explicative et 'ClaimNb' comme variable à prédire, avec pondération par 'Exposure'.
# Ajustements locaux (local linear regression) pour le modèle GLM_BC
fit_loc_GLM_BC = locfit.raw(x = datavalid$GLM_BC,
                            y = datavalid[,"ClaimNb"], 
                            weights = datavalid[,"Exposure"], 
                            kern = "rect", deg = 0, alpha = alphas)

# Ajustements locaux (local linear regression) pour le modèle GAM_BC
fit_loc_GAM_BC = locfit.raw(x = datavalid$GAM_BC,
                            y = datavalid[,"ClaimNb"],
                            weights = datavalid[,"Exposure"],
                            kern = "rect", deg = 0, alpha = alphas_)

# Ajustements locaux (local linear regression) pour le modèle GBM_BC
fit_loc_GBM_BC = locfit.raw(x = datavalid$GBM_BC,
                            y = datavalid[,"ClaimNb"],
                            weights = datavalid[,"Exposure"],
                            kern = "rect", deg = 0, alpha = alphas__)
```

## Comparaison versions corrigées

``` r
# Définir le thème
theme_set(theme_bw())

# Définir les couleurs
clrpal <- c("#236B8E", "gray37", "#AED4E6", "gray77", "#64B5CD", "gray50", "#6CA6CD", "#8AB6CD", "#1F77B4", "#4682B4", "gray15", "#4C72B0")

# Calculer les quantiles pour chaque modèle
vu <- (0:200) / 200
idss = sample(1:nrow(datavalid), size = 10000)

# Créer des data frames pour chaque modèle
df_GLM <- data.frame(Quantile = vu[3:199], Premium_GLM = quantile(datavalid$GLM, probs = vu)[3:199], Premium_BC_GLM = quantile(datavalid$GLM_BC, probs = vu)[3:199])
df_GAM <- data.frame(Quantile = vu[3:199], Premium_GAM = quantile(datavalid$GAM, probs = vu)[3:199], Premium_BC_GAM = quantile(datavalid$GAM_BC, probs = vu)[3:199])
df_GBM <- data.frame(Quantile = vu[3:199], Premium_GBM = quantile(datavalid$GAM, probs = vu)[3:199], Premium_BC_GBM = quantile(datavalid$GBM_BC, probs = vu)[3:199])

# Créer le ggplot pour chaque modèle
p1 <- ggplot(df_GLM, aes(x = Premium_GLM, y = Premium_BC_GLM)) +
  geom_line(color = clrpal[11]) +
  geom_point(data = datatest[idss,], aes(x = GLM, y = GLM_BC), pch = 1, col = "lightblue", cex = 0.4) +
  labs(x = "Prime (GLM)", y = "Prime BC (GLM)", title = "") +
  xlim(0, 0.2) +
  ylim(0, 0.2) +
  theme_bw() +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black", size = 0.4)

p2 <- ggplot(df_GAM, aes(x = Premium_GAM, y = Premium_BC_GAM)) +
  geom_line(color = clrpal[11]) +
  geom_point(data = datavalid[idss,], aes(x = GAM, y = GAM_BC), pch = 1, col = "lightblue", cex = 0.4) +
  labs(x = "Prime (GAM)", y = "Prime BC (GAM)", title = "") +
  xlim(0, 0.2) +
  ylim(0, 0.2) +
  theme_bw() +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black", size = 0.4)

p3 <- ggplot(df_GBM, aes(x = Premium_GBM, y = Premium_BC_GBM)) +
  geom_line(color = clrpal[11]) +
  geom_point(data = datavalid[idss,], aes(x = GBM, y = GBM_BC), pch = 1, col = "lightblue", cex = 0.4) +
  labs(x = "Prime (GBM)", y = "Prime BC (GBM)", title = "") +
  xlim(0, 0.2) +
  ylim(0, 0.2) +
  theme_bw() +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black", size = 0.4)

# Afficher les graphiques côte à côte
library(gridExtra)
grid.arrange(p1, p2, p3, ncol = 3)
```

![](Mémoire_Actuariat_John_files/figure-gfm/unnamed-chunk-37-1.png)<!-- -->

``` r
# Créer le graphique ggplot pour chaque modèle
p1 <- ggplot(data = datatest[idss,], aes(x = GLM, y = GLM_BC), pch = 1, col = "lightblue", cex = 0.4) +
  geom_line(color = clrpal[11]) +
  labs(x = "Prime (GLM)", y = "Prime BC (GLM)", title = "") +
  xlim(0, 0.2) +
  ylim(0, 0.2) +
  theme_bw() +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black", size = 0.4)

p2 <- ggplot(data = datatest[idss,], aes(x = GAM, y = GAM_BC), pch = 1, col = "lightblue", cex = 0.4) +
  geom_line(color = clrpal[11]) +
  labs(x = "Prime (GAM)", y = "Prime BC (GAM)", title = "") +
  xlim(0, 0.2) +
  ylim(0, 0.2) +
  theme_bw() +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black", size = 0.4)

p3 <- ggplot(data = datatest[idss,], aes(x = GBM, y = GBM_BC), pch = 1, col = "lightblue", cex = 0.4) +
  geom_line(color = clrpal[11]) +
  labs(x = "Prime (GBM)", y = "Prime BC (GBM)", title = "") +
  xlim(0, 0.2) +
  ylim(0, 0.2) +
  theme_bw() +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black", size = 0.4)

# Afficher les graphiques côte à côte
library(gridExtra)
grid.arrange(p1, p2, p3, ncol = 3)
```

![](Mémoire_Actuariat_John_files/figure-gfm/unnamed-chunk-38-1.png)<!-- -->

``` r
# Définir le thème
theme_set(theme_bw())

# Définir les couleurs
clrpal <- c("#236B8E", "gray37", "#AED4E6", "gray77", "#64B5CD", "gray50", "#6CA6CD", "#8AB6CD", "#1F77B4", "#4682B4", "gray15", "#4C72B0")

# Tracer les histogrammes côte à côte
p1 <- ggplot(datavalid, aes(x = GLM_BC)) +
  geom_histogram(binwidth = 0.001, color = clrpal[1], fill = clrpal[1]) +
  labs(x = "Fréquence (GLM)", title = "") +
  xlim(0, 0.3) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10))

p2 <- ggplot(datavalid, aes(x = GAM_BC)) +
  geom_histogram(binwidth = 0.001, color = clrpal[3], fill = clrpal[3]) +
  labs(x = "Fréquence (GAM)", title = "") +
  xlim(0, 0.3) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10))

p3 <- ggplot(datavalid, aes(x = GBM_BC)) +
  geom_histogram(binwidth = 0.001, color = "black", fill = clrpal[12]) +
  labs(x = "Fréquence (GBM)", title = "") +
  xlim(0, 0.3) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10))

# Afficher les histogrammes côte à côte
library(gridExtra)
grid.arrange(p1, p2, p3, ncol = 3)
```

![](Mémoire_Actuariat_John_files/figure-gfm/unnamed-chunk-39-1.png)<!-- -->

``` r
# Diviser la fenêtre graphique en 1 ligne et 4 colonnes
par(mfrow=c(1,4))

# Tracer le graphique pour GLM BC
plot(fit_loc_GLM_BC, xlim=c(0,.2), xlab="Prime BC (GLM)", ylab="", ylim=c(0,.2))
lines(fit_loc_GLM, col=scales::alpha(clrpal[1], .4))
abline(a=0, b=1, lwd=.4)

# Tracer le graphique pour GAM BC
plot(fit_loc_GAM_BC, xlim=c(0,.2), xlab="Prime BC (GAM)", ylab="", ylim=c(0,.2))
lines(fit_loc_GAM, col=scales::alpha(clrpal[2], .4))
abline(a=0, b=1, lwd=.4)

# Tracer le graphique pour GBM BC
plot(fit_loc_GBM_BC, xlim=c(0,.2), xlab="Prime BC (GBM)", ylab="", ylim=c(0,.2))
lines(fit_loc_GBM, col=scales::alpha(clrpal[3], .4))
abline(a=0, b=1, lwd=.4)

# Remettre la configuration par défaut pour la fenêtre graphique
par(mfrow=c(1,1))
```

![](Mémoire_Actuariat_John_files/figure-gfm/unnamed-chunk-40-1.png)<!-- -->

``` r
# Définir le thème
theme_set(theme_bw())

# Définir les couleurs
clrpal <- c("#236B8E", "gray37", "#AED4E6", "gray77", "#64B5CD")

# Calculer les quantiles pour chaque modèle
vx <- seq(0, .2, length = 251)
q_GLM_BC <- q_GAM_BC <- q_GBM_BC <- rep(NA, length(vx))

for (i in 1:length(vx)) {
  q_GLM_BC[i] <- mean(datavalid$GLM_BC <= vx[i])
  q_GAM_BC[i] <- mean(datavalid$GAM_BC <= vx[i])
  q_GBM_BC[i] <- mean(datavalid$GBM_BC <= vx[i])
}

# Créer des data frames pour chaque modèle
df_GLM <- data.frame(Quantile = q_GLM, Quantile. = q_GLM_BC, vy_GLM_BC = predict(fit_loc_GLM_BC, newdata = vx) - vx, vy_GLM_BC. = predict(fit_loc_GLM_BC, newdata = vx)/vx, vy_GLM = predict(fit_loc_GLM, newdata = vx) - vx, vy_GLM. = predict(fit_loc_GLM, newdata = vx)/vx)
df_GAM <- data.frame(Quantile = q_GAM, Quantile. = q_GAM_BC, vy_GAM_BC = predict(fit_loc_GAM_BC, newdata = vx) - vx, vy_GAM_BC. = predict(fit_loc_GAM_BC, newdata = vx)/vx, vy_GAM = predict(fit_loc_GAM, newdata = vx) - vx, vy_GAM. = predict(fit_loc_GAM, newdata = vx)/vx)
df_GBM <- data.frame(Quantile = q_GBM, Quantile. = q_GBM_BC, vy_GBM_BC = predict(fit_loc_GBM_BC, newdata = vx) - vx, vy_GBM_BC. = predict(fit_loc_GBM_BC, newdata = vx)/vx, vy_GBM = predict(fit_loc_GBM, newdata = vx) - vx, vy_GBM. = predict(fit_loc_GBM, newdata = vx)/vx)

# Tracer les graphiques en utilisant ggplot2
p1 <- ggplot(df_GLM, aes(x = Quantile., y = vy_GLM_BC)) +
  geom_line(color = clrpal[1], size = 1.1) +
  geom_line(data = df_GLM, aes(x = Quantile, y = vy_GLM), color = clrpal[1]) +
  labs(x = "Quantile de fréquence (GLM)", y = "", title = "") +
  xlim(0, 1) +
  ylim(-0.1, 0.1) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.4)

p2 <- ggplot(df_GAM, aes(x = Quantile., y = vy_GAM_BC)) +
  geom_line(color = clrpal[3], size = 1.1) +
  geom_line(data = df_GAM, aes(x = Quantile, y = vy_GAM), color = clrpal[3]) +
  labs(x = "Quantile de fréquence (GAM)", y = "", title = "") +
  xlim(0, 1) +
  ylim(-0.1, 0.1) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.4)

p3 <- ggplot(df_GBM, aes(x = Quantile., y = vy_GBM_BC)) +
  geom_line(color = clrpal[5], size = 1.1) +
  geom_line(data = df_GBM, aes(x = Quantile, y = vy_GBM), color = clrpal[5]) +
  labs(x = "Quantile de fréquence (GBM)", y = "", title = "") +
  xlim(0, 1) +
  ylim(-0.1, 0.1) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.4)

# Afficher les graphiques côte à côte
library(gridExtra)
grid.arrange(p1, p2, p3, ncol = 3)
```

![](Mémoire_Actuariat_John_files/figure-gfm/unnamed-chunk-41-1.png)<!-- -->

``` r
# Définir le thème
theme_set(theme_bw())

# Définir les couleurs
clrpal <- c("#236B8E", "gray37", "#AED4E6", "gray77", "#64B5CD")

# Calculer les quantiles pour chaque modèle
vx <- seq(0, .2, length = 251)
q_GLM_BC <- q_GAM_BC <- q_GBM_BC <- rep(NA, length(vx))

for (i in 1:length(vx)) {
  q_GLM_BC[i] <- mean(datavalid$GLM_BC <= vx[i])
  q_GAM_BC[i] <- mean(datavalid$GAM_BC <= vx[i])
  q_GBM_BC[i] <- mean(datavalid$GBM_BC <= vx[i])
}

# Créer des data frames pour chaque modèle
df_GLM <- data.frame(Quantile = q_GLM, Quantile. = q_GLM_BC, vy_GLM_BC = predict(fit_loc_GLM_BC, newdata = vx) - vx, vy_GLM_BC. = predict(fit_loc_GLM_BC, newdata = vx)/vx, vy_GLM = predict(fit_loc_GLM, newdata = vx) - vx, vy_GLM. = predict(fit_loc_GLM, newdata = vx)/vx)
df_GAM <- data.frame(Quantile = q_GAM, Quantile. = q_GAM_BC, vy_GAM_BC = predict(fit_loc_GAM_BC, newdata = vx) - vx, vy_GAM_BC. = predict(fit_loc_GAM_BC, newdata = vx)/vx, vy_GAM = predict(fit_loc_GAM, newdata = vx) - vx, vy_GAM. = predict(fit_loc_GAM, newdata = vx)/vx)
df_GBM <- data.frame(Quantile = q_GBM, Quantile. = q_GBM_BC, vy_GBM_BC = predict(fit_loc_GBM_BC, newdata = vx) - vx, vy_GBM_BC. = predict(fit_loc_GBM_BC, newdata = vx)/vx, vy_GBM = predict(fit_loc_GBM, newdata = vx) - vx, vy_GBM. = predict(fit_loc_GBM, newdata = vx)/vx)

# Tracer les graphiques en utilisant ggplot2
p1 <- ggplot(df_GLM, aes(x = Quantile., y = vy_GLM_BC.)) +
  geom_line(color = clrpal[1], size = 1.1) +
  geom_line(data = df_GLM, aes(x = Quantile, y = vy_GLM.), color = clrpal[1]) +
  labs(x = "Quantile de fréquence (GLM)", y = "", title = "") +
  xlim(0, 1) +
  ylim(0.5, 1.6) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", size = 0.4)

p2 <- ggplot(df_GAM, aes(x = Quantile., y = vy_GAM_BC.)) +
  geom_line(color = clrpal[3], size = 1.1) +
  geom_line(data = df_GAM, aes(x = Quantile, y = vy_GAM.), color = clrpal[3]) +
  labs(x = "Quantile de fréquence (GAM)", y = "", title = "") +
  xlim(0, 1) +
  ylim(0.5, 1.6) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", size = 0.4)

p3 <- ggplot(df_GBM, aes(x = Quantile., y = vy_GBM_BC.)) +
  geom_line(color = clrpal[5], size = 1.1) +
  geom_line(data = df_GBM, aes(x = Quantile, y = vy_GBM.), color = clrpal[5]) +
  labs(x = "Quantile de fréquence (GBM)", y = "", title = "") +
  xlim(0, 1) +
  ylim(0.5, 1.6) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", size = 0.4)

# Afficher les graphiques côte à côte
library(gridExtra)
grid.arrange(p1, p2, p3, ncol = 3)
```

![](Mémoire_Actuariat_John_files/figure-gfm/unnamed-chunk-42-1.png)<!-- -->

``` r
# Créer des data frames pour chaque modèle
df_GLM_BC <- data.frame(Quantile = q_GLM_BC, vy_GLM_BC = predict(fit_loc_GLM_BC, newdata = vx))
df_GLM <- data.frame(Quantile = q_GLM, vy_GLM = predict(fit_loc_GLM, newdata = vx), vy_GLM. = predict(fit_loc_GLM, newdata = vx)/vx, vx = vx)
df_GAM_BC <- data.frame(Quantile = q_GAM_BC, vy_GAM_BC = predict(fit_loc_GAM_BC, newdata = vx))
df_GAM <- data.frame(Quantile = q_GAM, vy_GAM = predict(fit_loc_GAM, newdata = vx), vy_GAM. = predict(fit_loc_GAM, newdata = vx)/vx, vx = vx)
df_GBM_BC <- data.frame(Quantile = q_GBM_BC, vy_GBM_BC = predict(fit_loc_GBM_BC, newdata = vx))
df_GBM <- data.frame(Quantile = q_GBM, vy_GBM = predict(fit_loc_GBM, newdata = vx), vy_GBM. = predict(fit_loc_GBM, newdata = vx)/vx, vx = vx)

# Définir le thème
theme_set(theme_bw())

# Tracer les graphiques en utilisant ggplot2
p1 <- ggplot(df_GLM_BC, aes(x = Quantile, y = vy_GLM_BC)) +
  geom_line(color = clrpal[1], size = 1.1) +
  geom_line(data = df_GLM, aes(x = Quantile, y = vx), color = "gray25") +
  geom_line(data = df_GLM, aes(x = Quantile, y = vy_GLM), color = "green4") +
  labs(x = "Quantile de prime (GLM)", y = "", title = "") +
  xlim(0, 1) +
  ylim(0, 0.2) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray25", size = 0.4)

p2 <- ggplot(df_GAM_BC, aes(x = Quantile, y = vy_GAM_BC)) +
  geom_line(color = clrpal[1], size = 1.1) +
  geom_line(data = df_GAM, aes(x = Quantile, y = vx), color = "gray25") +
  geom_line(data = df_GAM, aes(x = Quantile, y = vy_GAM), color = "green4") +
  labs(x = "Quantile de prime (GAM)", y = "", title = "") +
  xlim(0, 1) +
  ylim(0, 0.2) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray25", size = 0.4)

p3 <- ggplot(df_GBM_BC, aes(x = Quantile, y = vy_GBM_BC)) +
  geom_line(color = clrpal[1], size = 1.1) +
  geom_line(data = df_GBM, aes(x = Quantile, y = vx), color = "gray25") +
  geom_line(data = df_GBM, aes(x = Quantile, y = vy_GBM), color = "green4") +
  labs(x = "Quantile de prime (boosting)", y = "", title = "") +
  xlim(0, 1) +
  ylim(0, 0.2) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray25", size = 0.4)

# Afficher les graphiques côte à côte
library(gridExtra)
grid.arrange(p1, p2, p3, ncol = 3)
```

![](Mémoire_Actuariat_John_files/figure-gfm/unnamed-chunk-43-1.png)<!-- -->

# Graphique quantiles simples

``` r
# Trier l'ensemble de données en fonction des valeurs de GLM
datatest <- datatest[order(datatest$GLM), ]

# Nombre de sous-ensembles
K <- 10

# Calculer la valeur moyenne prédite de l'ensemble de test pour la normalisation
avg_predicted_value <- mean(datatest$GLM)

# Répartir les données en classes de même taille en fonction des sous-ensembles utilisant GLM
datatest$Subset_GLM <- cut(seq(nrow(datatest)), breaks = K, labels = FALSE)

# Calculer la valeur moyenne des pertes attendues et des pertes réelles (ClaimNb) dans chaque sous-ensemble en utilisant GLM
lift_data <- datatest %>%
  group_by(Subset_GLM) %>%
  summarize(
    Mean_Observed = mean(ClaimNb) / avg_predicted_value,
    Mean_Predicted_GLM = mean(GLM) / avg_predicted_value,
    Mean_Predicted_GLM_BC = mean(GLM_BC) / avg_predicted_value
  )

# Tracer la courbe de lift en utilisant ggplot2
library(ggplot2)

# Palette de couleurs
clrpal <- c("#236B8E", "gray37", "#AED4E6")

p1 <- ggplot(data = lift_data, aes(x = Subset_GLM)) +
  geom_line(aes(y = Mean_Observed, color = "Observations")) +
  geom_line(aes(y = Mean_Predicted_GLM, color = "Modèle sans BC (GLM)")) +
  geom_line(aes(y = Mean_Predicted_GLM_BC, color = "Modèle BC (GLM_BC)")) +
  labs(x = "Déciles", y = "Lift Normalisé", title = "") +
  scale_color_manual(values = clrpal) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 8)
  )
```

``` r
# Trier l'ensemble de données en fonction des valeurs de GAM
datatest <- datatest[order(datatest$GAM), ]

# Nombre de sous-ensembles
K <- 10

# Calculer la valeur moyenne prédite de l'ensemble de test pour la normalisation
avg_predicted_value <- mean(datatest$GAM)

# Répartir les données en classes de même taille en fonction des sous-ensembles utilisant GAM
datatest$Subset_GAM <- cut(seq(nrow(datatest)), breaks = K, labels = FALSE)

# Calculer la valeur moyenne des pertes attendues et des pertes réelles (ClaimNb) dans chaque sous-ensemble en utilisant GAM
lift_data <- datatest %>%
  group_by(Subset_GAM) %>%
  summarize(
    Mean_Observed = mean(ClaimNb) / avg_predicted_value,
    Mean_Predicted_GAM = mean(GAM) / avg_predicted_value,
    Mean_Predicted_GAM_BC = mean(GAM_BC) / avg_predicted_value
  )

# Tracer la courbe de lift en utilisant ggplot2
library(ggplot2)

# Palette de couleurs
clrpal <- c("#236B8E", "gray37", "#AED4E6")

p2 <- ggplot(data = lift_data, aes(x = Subset_GAM)) +
  geom_line(aes(y = Mean_Observed, color = "Observations")) +
  geom_line(aes(y = Mean_Predicted_GAM, color = "Modèle sans BC (GAM)")) +
  geom_line(aes(y = Mean_Predicted_GAM_BC, color = "Modèle BC (GAM_BC)")) +
  labs(x = "Déciles", y = "Lift Normalisé", title = "Courbe de Lift simple") +
  scale_color_manual(values = clrpal) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 17, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 8)
  )
```

``` r
# Trier l'ensemble de données en fonction des valeurs de GBM
datatest <- datatest[order(datatest$GBM), ]

# Nombre de sous-ensembles
K <- 10

# Calculer la valeur moyenne prédite de l'ensemble de test pour la normalisation
avg_predicted_value <- mean(datatest$GBM)

# Répartir les données en classes de même taille en fonction des sous-ensembles utilisant GBM
datatest$Subset_GBM <- cut(seq(nrow(datatest)), breaks = K, labels = FALSE)

# Calculer la valeur moyenne des pertes attendues et des pertes réelles (ClaimNb) dans chaque sous-ensemble en utilisant GBM
lift_data <- datatest %>%
  group_by(Subset_GBM) %>%
  summarize(
    Mean_Observed = mean(ClaimNb) / avg_predicted_value,
    Mean_Predicted_GBM = mean(GBM) / avg_predicted_value,
    Mean_Predicted_GBM_BC = mean(GBM_BC) / avg_predicted_value
  )

# Tracer la courbe de lift en utilisant ggplot2
library(ggplot2)

# Palette de couleurs
clrpal <- c("#236B8E", "gray37", "#AED4E6")

p3 <- ggplot(data = lift_data, aes(x = Subset_GBM)) +
  geom_line(aes(y = Mean_Observed, color = "Observations")) +
  geom_line(aes(y = Mean_Predicted_GBM, color = "Modèle sans BC (GBM)")) +
  geom_line(aes(y = Mean_Predicted_GBM_BC, color = "Modèle BC (GBM_BC)")) +
  labs(x = "Déciles", y = "Lift Normalisé", title = "") +
  scale_color_manual(values = clrpal) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 8)
  )
grid.arrange(p1, p2, p3, ncol = 3)
```

![](Mémoire_Actuariat_John_files/figure-gfm/unnamed-chunk-46-1.png)<!-- -->

# Double lift

``` r
# Trier l'ensemble de données en fonction du ratio entre les primes après et avant l'autocalibration
datatest$Ratio <- datatest$GLM_BC / datatest$GLM
datatest <- datatest[order(datatest$Ratio), ]

# Nombre de sous-ensembles
K <- 10

# Calculer la valeur moyenne prédite de l'ensemble de test pour la normalisation
avg_predicted_value_GLM <- mean(datatest$GLM)
avg_predicted_value_GLM_BC <- mean(datatest$GLM_BC)
avg_claim_nb <- mean(datatest$ClaimNb)

# Répartir les données en classes de même taille en fonction du ratio
datatest$Subset_GLM <- cut(seq(nrow(datatest)), breaks = K, labels = FALSE)

# Calculer la valeur moyenne prédite avant et après l'autocalibration, ainsi que la valeur moyenne observée dans chaque sous-ensemble
lift_data <- datatest %>%
  group_by(Subset_GLM) %>%
  summarize(
    Mean_Observed = mean(ClaimNb) / avg_claim_nb,
    Mean_Predicted_GLM = mean(GLM) / avg_predicted_value_GLM,
    Mean_Predicted_GLM_BC = mean(GLM_BC) / avg_predicted_value_GLM_BC
  )

# Tracer les Courbes de Lift Double en utilisant ggplot2
library(ggplot2)

# Palette de couleurs
clrpal <- c("#236B8E", "gray37", "#AED4E6")

p1 <- ggplot(data = lift_data, aes(x = Subset_GLM)) +
  geom_line(aes(y = Mean_Observed, color = "Observations")) +
  geom_line(aes(y = Mean_Predicted_GLM, color = "Modèle sans BC (GLM)")) +
  geom_line(aes(y = Mean_Predicted_GLM_BC, color = "Modèle BC (GLM_BC)")) +
  labs(x = "Déciles", y = "Lift Normalisé", title = "") +
  scale_color_manual(values = clrpal) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 8)
  )
```

``` r
# Trier l'ensemble de données en fonction du ratio entre les primes après et avant l'autocalibration
datatest$Ratio <- datatest$GAM_BC / datatest$GAM
datatest <- datatest[order(datatest$Ratio), ]

# Nombre de sous-ensembles
K <- 10

# Calculer la valeur moyenne prédite de l'ensemble de test pour la normalisation
avg_predicted_value_GAM <- mean(datatest$GAM)
avg_predicted_value_GAM_BC <- mean(datatest$GAM_BC)
avg_claim_nb <- mean(datatest$ClaimNb)

# Répartir les données en classes de même taille en fonction du ratio
datatest$Subset_GAM <- cut(seq(nrow(datatest)), breaks = K, labels = FALSE)

# Calculer la valeur moyenne prédite avant et après l'autocalibration, ainsi que la valeur moyenne observée dans chaque sous-ensemble
lift_data <- datatest %>%
  group_by(Subset_GAM) %>%
  summarize(
    Mean_Observed = mean(ClaimNb) / avg_claim_nb,
    Mean_Predicted_GAM = mean(GAM) / avg_predicted_value_GAM,
    Mean_Predicted_GAM_BC = mean(GAM_BC) / avg_predicted_value_GAM_BC
  )

# Tracer les Courbes de Lift Double en utilisant ggplot2
library(ggplot2)

# Palette de couleurs
clrpal <- c("#236B8E", "gray37", "#AED4E6")

p2 <- ggplot(data = lift_data, aes(x = Subset_GAM)) +
  geom_line(aes(y = Mean_Observed, color = "Observations")) +
  geom_line(aes(y = Mean_Predicted_GAM, color = "Modèle sans BC (GAM)")) +
  geom_line(aes(y = Mean_Predicted_GAM_BC, color = "Modèle BC (GAM_BC)")) +
  labs(x = "Déciles", y = "Lift Normalisé", title = "Courbe de Lift Double") +
  scale_color_manual(values = clrpal) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 17, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 8)
  )
```

``` r
# Trier l'ensemble de données en fonction du ratio entre les primes après et avant l'autocalibration
datatest$Ratio <- datatest$GBM_BC / datatest$GBM
datatest <- datatest[order(datatest$Ratio), ]

# Nombre de sous-ensembles
K <- 10

# Calculer la valeur moyenne prédite de l'ensemble de test pour la normalisation
avg_predicted_value_GBM <- mean(datatest$GBM)
avg_predicted_value_GBM_BC <- mean(datatest$GBM_BC)
avg_claim_nb <- mean(datatest$ClaimNb)

# Répartir les données en classes de même taille en fonction du ratio
datatest$Subset_GBM <- cut(seq(nrow(datatest)), breaks = K, labels = FALSE)

# Calculer la valeur moyenne prédite avant et après l'autocalibration, ainsi que la valeur moyenne observée dans chaque sous-ensemble
lift_data <- datatest %>%
  group_by(Subset_GBM) %>%
  summarize(
    Mean_Observed = mean(ClaimNb) / avg_claim_nb,
    Mean_Predicted_GBM = mean(GBM) / avg_predicted_value_GBM,
    Mean_Predicted_GBM_BC = mean(GBM_BC) / avg_predicted_value_GBM_BC
  )

# Tracer les Courbes de Lift Double en utilisant ggplot2
library(ggplot2)

# Palette de couleurs
clrpal <- c("#236B8E", "gray37", "#AED4E6")

p3 <- ggplot(data = lift_data, aes(x = Subset_GBM)) +
  geom_line(aes(y = Mean_Observed, color = "Observations")) +
  geom_line(aes(y = Mean_Predicted_GBM, color = "Modèle sans BC (GBM)")) +
  geom_line(aes(y = Mean_Predicted_GBM_BC, color = "Modèle BC (GBM_BC)")) +
  labs(x = "Déciles", y = "Lift Normalisé", title = "") +
  scale_color_manual(values = clrpal) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
  )
grid.arrange(p1, p2, p3, ncol = 3)
```

![](Mémoire_Actuariat_John_files/figure-gfm/unnamed-chunk-49-1.png)<!-- -->

# Déviance poisson

``` r
comp_tab <-NULL
comp_tab <- tibble(
  model_use = character(),
  dev1 = numeric(),
  dev2 = numeric(),
  dev3 = numeric())
names(comp_tab) <- c("Model","In sample loss (%)","Out sample loss test (%)", "Out sample loss datavalid", "Data test frequency")
```

``` r
comp_tab[1, ] <- list("GLM", round(PoissonDeviance(datalearning$ClaimNb, datalearning$GLM), 6),
                   round(PoissonDeviance(datatest$ClaimNb, datatest$GLM), 6),
                   round(PoissonDeviance(datavalid$ClaimNb, datavalid$GLM), 6))

comp_tab[2, ] <- list("GAM", round(PoissonDeviance(datalearning$ClaimNb, datalearning$GAM), 6),
                   round(PoissonDeviance(datatest$ClaimNb, datatest$GAM), 6),
                   round(PoissonDeviance(datavalid$ClaimNb, datavalid$GAM), 6))

comp_tab[3, ] <- list("GBM", round(PoissonDeviance(datalearning$ClaimNb, datalearning$GBM), 6),
                   round(PoissonDeviance(datatest$ClaimNb, datatest$GBM), 6),
                   round(PoissonDeviance(datavalid$ClaimNb, datavalid$GBM), 6))

knitr::kable(comp_tab)
```

| Model | In sample loss (%) | Out sample loss test (%) | Out sample loss datavalid |
|:------|-------------------:|-------------------------:|--------------------------:|
| GLM   |           0.318888 |                 0.328850 |                  0.317176 |
| GAM   |           0.316126 |                 0.326025 |                  0.314636 |
| GBM   |           0.298511 |                 0.313775 |                  0.303663 |

``` r
# sum(datavalid$ClaimNb)/sum(datavalid$Exposure)
comp_tab[4, ] <- list("GLM_BC", round(PoissonDeviance(datalearning$ClaimNb, datalearning$GLM_BC), 6),
                   round(PoissonDeviance(datatest$ClaimNb, datatest$GLM_BC), 6),
                   round(PoissonDeviance(datavalid$ClaimNb, datavalid$GLM_BC), 6))

comp_tab[5, ] <- list("GAM_BC", round(PoissonDeviance(datalearning$ClaimNb, datalearning$GAM_BC), 6),
                   round(PoissonDeviance(datatest$ClaimNb, datatest$GAM_BC), 6),
                   round(PoissonDeviance(datavalid$ClaimNb, datavalid$GAM_BC), 6))

comp_tab[6, ] <- list("GBM_BC", round(PoissonDeviance(datalearning$ClaimNb, datalearning$GBM_BC), 6),
                   round(PoissonDeviance(datatest$ClaimNb, datatest$GBM_BC), 6),
                   round(PoissonDeviance(datavalid$ClaimNb, datavalid$GBM_BC), 6))

knitr::kable(comp_tab)
```

| Model  | In sample loss (%) | Out sample loss test (%) | Out sample loss datavalid |
|:-------|-------------------:|-------------------------:|--------------------------:|
| GLM    |           0.318888 |                 0.328850 |                  0.317176 |
| GAM    |           0.316126 |                 0.326025 |                  0.314636 |
| GBM    |           0.298511 |                 0.313775 |                  0.303663 |
| GLM_BC |           0.308292 |                 0.317000 |                  0.306812 |
| GAM_BC |           0.306449 |                 0.314967 |                  0.304791 |
| GBM_BC |           0.294816 |                 0.307164 |                  0.297389 |

``` r
# Charger les bibliothèques
library(ggplot2)

# Définir la matrice des déviations Out sample loss test avec et sans BC
dev <- matrix(
  comp_tab$`Out sample loss test (%)`,
  nrow = 3,
  byrow = FALSE,
  dimnames = list(c("GLM", "GAM", "GBM"), c("Sans BC", "Avec BC"))
)

# Créer le graphique de comparaison des déviations Out sample loss test avec et sans BC
ggplot(data = as.data.frame(dev), aes(x = rownames(dev))) +
   geom_point(aes(y = `Sans BC`, color = "Sans autocalibration"), size = 4) +
  geom_point(aes(y = `Avec BC`, color = "Avec autocalibration"), size = 4) +
  labs(x = "Modèles", y = "Déviances", title = "Comparaison des déviances") +
  scale_fill_manual(values = c("#236B8E", "#AED4E6")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 8))
```

![](Mémoire_Actuariat_John_files/figure-gfm/unnamed-chunk-53-1.png)<!-- -->

# Comparaison GLM, GAM et GBM (pour après autocalibration)

## GLM

``` r
comp_plots0(datatest, "GLM", "GLM_BC")
```

![](Mémoire_Actuariat_John_files/figure-gfm/unnamed-chunk-54-1.png)<!-- -->

## GLM

``` r
comp_plots0(datatest, "GAM", "GAM_BC")
```

![](Mémoire_Actuariat_John_files/figure-gfm/unnamed-chunk-55-1.png)<!-- -->

## GBM

``` r
comp_plots0(datatest, "GBM", "GBM_BC")
```

![](Mémoire_Actuariat_John_files/figure-gfm/unnamed-chunk-56-1.png)<!-- -->

# Autocalibration (Uniquement basée sur les profils risqués)

``` r
# Calculer les quantiles de la variable GLM
quantiles_GLM <- quantile(datavalid$GLM, c(0.25, 0.75))
# Sélectionnez les observations des derniers quantiles pour GLM
last_quantile_indices_GLM <- which(datavalid$GLM <= quantiles_GLM[1] | datavalid$GLM >= quantiles_GLM[2])

# Calculer les quantiles de la variable GLM
quantiles_GAM <- quantile(datavalid$GAM, c(0.25, 0.75))
# Sélectionnez les observations des derniers quantiles pour GAM
last_quantile_indices_GAM <- which(datavalid$GAM <= quantiles_GAM[1] | datavalid$GAM >= quantiles_GAM[2])

# Calculer les quantiles de la variable GLM
quantiles_GBM <- quantile(datavalid$GBM, c(0.25, 0.75))
# Sélectionnez les observations des derniers quantiles pour GBM
last_quantile_indices_GBM <- which(datavalid$GBM <= quantiles_GBM[1] | datavalid$GBM >= quantiles_GBM[2])
```

``` r
# Charger la bibliothèque locfit
library(locfit)

# Séquence des valeurs alpha pour la validation croisée
alphas <- seq(0.01, 0.5, by = 0.001)

# Calculer les graphiques de validation croisée pour le modèle GLM
A_1 <- lcvplot(x = datavalid$GLM[last_quantile_indices_GLM], y = datavalid[last_quantile_indices_GLM, "ClaimNb"], weights = datavalid[last_quantile_indices_GLM,"Exposure"], kern = "rect", deg = 0, alpha = alphas)

# Calculer les graphiques de validation croisée pour le modèle GAM
A_2 <- lcvplot(x = datavalid$GAM[last_quantile_indices_GAM], y = datavalid[last_quantile_indices_GAM, "ClaimNb"], weights = datavalid[last_quantile_indices_GAM,"Exposure"], kern = "rect", deg = 0, alpha = alphas)

# Calculer les graphiques de validation croisée pour le modèle GBM
A_3 <- lcvplot(x = datavalid$GBM[last_quantile_indices_GBM], y = datavalid[last_quantile_indices_GBM, "ClaimNb"], weights = datavalid[last_quantile_indices_GBM,"Exposure"], kern = "rect", deg = 0, alpha = alphas)

# Diviser la fenêtre graphique en 4 pour afficher les graphiques côte à côte
par(mfrow = c(1, 4))

# Afficher le graphique de validation croisée pour le modèle GLM
plot(A_1, xlab = "Fitted DF", ylab = A_1$cri)

# Afficher le graphique de validation croisée pour le modèle GAM
plot(A_2, xlab = "Fitted DF", ylab = A_2$cri)

# Afficher le graphique de validation croisée pour le modèle GBM
plot(A_3, xlab = "Fitted DF", ylab = A_3$cri)

# Remettre la configuration de la fenêtre graphique à la normale
par(mfrow = c(1, 1))
```

![](Mémoire_Actuariat_John_files/figure-gfm/unnamed-chunk-58-1.png)<!-- -->

``` r
# Appliquez l'autocalibration aux premiers et derniers quantiles pour GLM
fit_loc_GLM <- locfit.raw(
  x = datavalid$GLM[last_quantile_indices_GLM],
  y = datavalid[last_quantile_indices_GLM, "ClaimNb"],
  weights = datavalid[last_quantile_indices_GLM, "Exposure"],
  kern = "rect",
  deg = 0,
  alpha = 0.04 # Vous devez définir correctement alphas selon vos besoins
)

# Appliquez l'autocalibration aux premiers et derniers quantiles pour GAM
fit_loc_GAM <- locfit.raw(
  x = datavalid$GAM[last_quantile_indices_GAM],
  y = datavalid[last_quantile_indices_GAM, "ClaimNb"],
  weights = datavalid[last_quantile_indices_GAM, "Exposure"],
  kern = "rect",
  deg = 0,
  alpha = 0.03 # Vous devez définir correctement alphas_ selon vos besoins
)

# Appliquez l'autocalibration auxpremiers et derniers quantiles pour GBM
fit_loc_GBM <- locfit.raw(
  x = datavalid$GBM[last_quantile_indices_GBM],
  y = datavalid[last_quantile_indices_GBM, "ClaimNb"],
  weights = datavalid[last_quantile_indices_GBM, "Exposure"],
  kern = "rect",
  deg = 0,
  alpha = 0.04 # Vous devez définir correctement alphas__ selon vos besoins
)

# Utilise les modèles de locfit pour effectuer des prédictions sur les données de 'datavalid' avec les variables GLM, GAM et GBM.
p_GLM = predict(fit_loc_GLM, newdata = datavalid$GLM)
p_GAM = predict(fit_loc_GAM, newdata = datavalid$GAM)
p_GBM = predict(fit_loc_GBM, newdata = datavalid$GBM)

# Ajuste les valeurs de 'GLM_BC', 'GAM_BC' et 'GBM_BC' en multipliant les valeurs respectives de 'GLM', 'GAM' et 'GBM' par les prédictions précédemment calculées.
datavalid$GLM_BC = datavalid$GLM * (p_GLM / datavalid$GLM)
datavalid$GAM_BC = datavalid$GAM * (p_GAM / datavalid$GAM)
datavalid$GBM_BC = datavalid$GBM * (p_GBM / datavalid$GBM)

# Répète le processus précédent pour les données de 'datatest'.
p_GLM = predict(fit_loc_GLM, newdata = datatest$GLM)
p_GAM = predict(fit_loc_GAM, newdata = datatest$GAM)
p_GBM = predict(fit_loc_GBM, newdata = datatest$GBM)

datatest$GLM_BC = datatest$GLM * (p_GLM / datatest$GLM)
datatest$GAM_BC = datatest$GAM * (p_GAM / datatest$GAM)
datatest$GBM_BC = datatest$GBM * (p_GBM / datatest$GBM)

# Répète le processus précédent pour les données de 'datalearning'.
p_GLM = predict(fit_loc_GLM, newdata = datalearning$GLM)
p_GAM = predict(fit_loc_GAM, newdata = datalearning$GAM)
p_GBM = predict(fit_loc_GBM, newdata = datalearning$GBM)

datalearning$GLM_BC = datalearning$GLM * (p_GLM / datalearning$GLM)
datalearning$GAM_BC = datalearning$GAM * (p_GAM / datalearning$GAM)
datalearning$GBM_BC = datalearning$GBM * (p_GBM / datalearning$GBM)
```

# Diagnostic autocalibration

``` r
# Obtient un résumé statistique de la variable 'GLM_BC' dans l'ensemble de données 'datavalid'.
desc(datavalid$GLM_BC)
```

    ##   mean    10%    90% 
    ## 0.0537 0.0220 0.0817

``` r
# Obtient un résumé statistique de la variable 'GAM_BC' dans l'ensemble de données 'datavalid'.
desc(datavalid$GAM_BC)
```

    ##   mean    10%    90% 
    ## 0.0530 0.0196 0.0860

``` r
# Obtient un résumé statistique de la variable 'GBM_BC' dans l'ensemble de données 'datavalid'.
desc(datavalid$GBM_BC)
```

    ##   mean    10%    90% 
    ## 0.0525 0.0179 0.0789

``` r
# Obtient un résumé statistique de la variable 'GLM_BC' dans l'ensemble de données 'datatest'.
desc(datatest$GLM_BC)
```

    ##   mean    10%    90% 
    ## 0.0538 0.0219 0.0819

``` r
# Obtient un résumé statistique de la variable 'GAM_BC' dans l'ensemble de données 'datatest'.
desc(datatest$GAM_BC)
```

    ##   mean    10%    90% 
    ## 0.0531 0.0196 0.0861

``` r
# Obtient un résumé statistique de la variable 'GBM_BC' dans l'ensemble de données 'datatest'.
desc(datatest$GBM_BC)
```

    ##   mean    10%    90% 
    ## 0.0526 0.0179 0.0797

``` r
# Obtient un résumé statistique de la variable 'GLM_BC' dans l'ensemble de données 'datalearning'.
desc(datalearning$GLM_BC)
```

    ##   mean    10%    90% 
    ## 0.0536 0.0219 0.0816

``` r
# Obtient un résumé statistique de la variable 'GAM_BC' dans l'ensemble de données 'datalearning'.
desc(datalearning$GAM_BC)
```

    ##   mean    10%    90% 
    ## 0.0530 0.0196 0.0859

``` r
# Obtient un résumé statistique de la variable 'GBM_BC' dans l'ensemble de données 'datalearning'.
desc(datalearning$GBM_BC)
```

    ##   mean    10%    90% 
    ## 0.0525 0.0179 0.0790

## Comparaison versions corrigées

``` r
# Définir le thème
theme_set(theme_bw())

# Définir les couleurs
clrpal <- c("#236B8E", "gray37", "#AED4E6", "gray77", "#64B5CD", "gray50", "#6CA6CD", "#8AB6CD", "#1F77B4", "#4682B4", "gray15", "#4C72B0")

# Calculer les quantiles pour chaque modèle
vu <- (0:200) / 200
idss = sample(1:nrow(datavalid), size = 10000)

# Créer des data frames pour chaque modèle
df_GLM <- data.frame(Quantile = vu[3:199], Premium_GLM = quantile(datavalid$GLM, probs = vu)[3:199], Premium_BC_GLM = quantile(datavalid$GLM_BC, probs = vu)[3:199])
df_GAM <- data.frame(Quantile = vu[3:199], Premium_GAM = quantile(datavalid$GAM, probs = vu)[3:199], Premium_BC_GAM = quantile(datavalid$GAM_BC, probs = vu)[3:199])
df_GBM <- data.frame(Quantile = vu[3:199], Premium_GBM = quantile(datavalid$GAM, probs = vu)[3:199], Premium_BC_GBM = quantile(datavalid$GBM_BC, probs = vu)[3:199])

# Créer le ggplot pour chaque modèle
p1 <- ggplot(df_GLM, aes(x = Premium_GLM, y = Premium_BC_GLM)) +
  geom_line(color = clrpal[11]) +
  geom_point(data = datatest[idss,], aes(x = GLM, y = GLM_BC), pch = 1, col = "lightblue", cex = 0.4) +
  labs(x = "Prime (GLM)", y = "Prime BC (GLM)", title = "") +
  xlim(0, 0.2) +
  ylim(0, 0.2) +
  theme_bw() +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black", size = 0.4)

p2 <- ggplot(df_GAM, aes(x = Premium_GAM, y = Premium_BC_GAM)) +
  geom_line(color = clrpal[11]) +
  geom_point(data = datavalid[idss,], aes(x = GAM, y = GAM_BC), pch = 1, col = "lightblue", cex = 0.4) +
  labs(x = "Prime (GAM)", y = "Prime BC (GAM)", title = "") +
  xlim(0, 0.2) +
  ylim(0, 0.2) +
  theme_bw() +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black", size = 0.4)

p3 <- ggplot(df_GBM, aes(x = Premium_GBM, y = Premium_BC_GBM)) +
  geom_line(color = clrpal[11]) +
  geom_point(data = datavalid[idss,], aes(x = GBM, y = GBM_BC), pch = 1, col = "lightblue", cex = 0.4) +
  labs(x = "Prime (GBM)", y = "Prime BC (GBM)", title = "") +
  xlim(0, 0.2) +
  ylim(0, 0.2) +
  theme_bw() +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black", size = 0.4)

# Afficher les graphiques côte à côte
library(gridExtra)
grid.arrange(p1, p2, p3, ncol = 3)
```

![](Mémoire_Actuariat_John_files/figure-gfm/unnamed-chunk-61-1.png)<!-- -->

``` r
# Créer le graphique ggplot pour chaque modèle
p1 <- ggplot(data = datatest[idss,], aes(x = GLM, y = GLM_BC), pch = 1, col = "lightblue", cex = 0.4) +
  geom_line(color = clrpal[11]) +
  labs(x = "Prime (GLM)", y = "Prime BC (GLM)", title = "") +
  xlim(0, 0.2) +
  ylim(0, 0.2) +
  theme_bw() +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black", size = 0.4)

p2 <- ggplot(data = datatest[idss,], aes(x = GAM, y = GAM_BC), pch = 1, col = "lightblue", cex = 0.4) +
  geom_line(color = clrpal[11]) +
  labs(x = "Prime (GAM)", y = "Prime BC (GAM)", title = "") +
  xlim(0, 0.2) +
  ylim(0, 0.2) +
  theme_bw() +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black", size = 0.4)

p3 <- ggplot(data = datatest[idss,], aes(x = GBM, y = GBM_BC), pch = 1, col = "lightblue", cex = 0.4) +
  geom_line(color = clrpal[11]) +
  labs(x = "Prime (GBM)", y = "Prime BC (GBM)", title = "") +
  xlim(0, 0.2) +
  ylim(0, 0.2) +
  theme_bw() +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black", size = 0.4)

# Afficher les graphiques côte à côte
library(gridExtra)
grid.arrange(p1, p2, p3, ncol = 3)
```

![](Mémoire_Actuariat_John_files/figure-gfm/unnamed-chunk-62-1.png)<!-- -->

``` r
# Définir le thème
theme_set(theme_bw())

# Définir les couleurs
clrpal <- c("#236B8E", "gray37", "#AED4E6", "gray77", "#64B5CD", "gray50", "#6CA6CD", "#8AB6CD", "#1F77B4", "#4682B4", "gray15", "#4C72B0")

# Tracer les histogrammes côte à côte
p1 <- ggplot(datavalid, aes(x = GLM_BC)) +
  geom_histogram(binwidth = 0.001, color = clrpal[1], fill = clrpal[1]) +
  labs(x = "Fréquence (GLM)", title = "") +
  xlim(0, 0.3) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10))

p2 <- ggplot(datavalid, aes(x = GAM_BC)) +
  geom_histogram(binwidth = 0.001, color = clrpal[3], fill = clrpal[3]) +
  labs(x = "Fréquence (GAM)", title = "") +
  xlim(0, 0.3) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10))

p3 <- ggplot(datavalid, aes(x = GBM_BC)) +
  geom_histogram(binwidth = 0.001, color = "black", fill = clrpal[12]) +
  labs(x = "Fréquence (GBM)", title = "") +
  xlim(0, 0.3) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10))

# Afficher les histogrammes côte à côte
library(gridExtra)
grid.arrange(p1, p2, p3, ncol = 3)
```

![](Mémoire_Actuariat_John_files/figure-gfm/unnamed-chunk-63-1.png)<!-- -->

``` r
# Définir le thème
theme_set(theme_bw())

# Définir les couleurs
clrpal <- c("#236B8E", "gray37", "#AED4E6", "gray77", "#64B5CD")

# Calculer les quantiles pour chaque modèle
vx <- seq(0, .2, length = 251)
q_GLM_BC <- q_GAM_BC <- q_GBM_BC <- rep(NA, length(vx))

for (i in 1:length(vx)) {
  q_GLM_BC[i] <- mean(datavalid$GLM_BC <= vx[i])
  q_GAM_BC[i] <- mean(datavalid$GAM_BC <= vx[i])
  q_GBM_BC[i] <- mean(datavalid$GBM_BC <= vx[i])
}

# Créer des data frames pour chaque modèle
df_GLM <- data.frame(Quantile = q_GLM, Quantile. = q_GLM_BC, vy_GLM_BC = predict(fit_loc_GLM_BC, newdata = vx) - vx, vy_GLM_BC. = predict(fit_loc_GLM_BC, newdata = vx)/vx, vy_GLM = predict(fit_loc_GLM, newdata = vx) - vx, vy_GLM. = predict(fit_loc_GLM, newdata = vx)/vx)
df_GAM <- data.frame(Quantile = q_GAM, Quantile. = q_GAM_BC, vy_GAM_BC = predict(fit_loc_GAM_BC, newdata = vx) - vx, vy_GAM_BC. = predict(fit_loc_GAM_BC, newdata = vx)/vx, vy_GAM = predict(fit_loc_GAM, newdata = vx) - vx, vy_GAM. = predict(fit_loc_GAM, newdata = vx)/vx)
df_GBM <- data.frame(Quantile = q_GBM, Quantile. = q_GBM_BC, vy_GBM_BC = predict(fit_loc_GBM_BC, newdata = vx) - vx, vy_GBM_BC. = predict(fit_loc_GBM_BC, newdata = vx)/vx, vy_GBM = predict(fit_loc_GBM, newdata = vx) - vx, vy_GBM. = predict(fit_loc_GBM, newdata = vx)/vx)

# Tracer les graphiques en utilisant ggplot2
p1 <- ggplot(df_GLM, aes(x = Quantile., y = vy_GLM_BC)) +
  geom_line(color = clrpal[1], size = 1.1) +
  geom_line(data = df_GLM, aes(x = Quantile, y = vy_GLM), color = clrpal[1]) +
  labs(x = "Quantile de fréquence (GLM)", y = "", title = "") +
  xlim(0, 1) +
  ylim(-0.1, 0.1) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.4)

p2 <- ggplot(df_GAM, aes(x = Quantile., y = vy_GAM_BC)) +
  geom_line(color = clrpal[3], size = 1.1) +
  geom_line(data = df_GAM, aes(x = Quantile, y = vy_GAM), color = clrpal[3]) +
  labs(x = "Quantile de fréquence (GAM)", y = "", title = "") +
  xlim(0, 1) +
  ylim(-0.1, 0.1) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.4)

p3 <- ggplot(df_GBM, aes(x = Quantile., y = vy_GBM_BC)) +
  geom_line(color = clrpal[5], size = 1.1) +
  geom_line(data = df_GBM, aes(x = Quantile, y = vy_GBM), color = clrpal[5]) +
  labs(x = "Quantile de fréquence (GBM)", y = "", title = "") +
  xlim(0, 1) +
  ylim(-0.1, 0.1) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.4)

# Afficher les graphiques côte à côte
library(gridExtra)
grid.arrange(p1, p2, p3, ncol = 3)
```

![](Mémoire_Actuariat_John_files/figure-gfm/unnamed-chunk-64-1.png)<!-- -->

``` r
# Définir le thème
theme_set(theme_bw())

# Définir les couleurs
clrpal <- c("#236B8E", "gray37", "#AED4E6", "gray77", "#64B5CD")

# Calculer les quantiles pour chaque modèle
vx <- seq(0, .2, length = 251)
q_GLM_BC <- q_GAM_BC <- q_GBM_BC <- rep(NA, length(vx))

for (i in 1:length(vx)) {
  q_GLM_BC[i] <- mean(datavalid$GLM_BC <= vx[i])
  q_GAM_BC[i] <- mean(datavalid$GAM_BC <= vx[i])
  q_GBM_BC[i] <- mean(datavalid$GBM_BC <= vx[i])
}

# Créer des data frames pour chaque modèle
df_GLM <- data.frame(Quantile = q_GLM, Quantile. = q_GLM_BC, vy_GLM_BC = predict(fit_loc_GLM_BC, newdata = vx) - vx, vy_GLM_BC. = predict(fit_loc_GLM_BC, newdata = vx)/vx, vy_GLM = predict(fit_loc_GLM, newdata = vx) - vx, vy_GLM. = predict(fit_loc_GLM, newdata = vx)/vx)
df_GAM <- data.frame(Quantile = q_GAM, Quantile. = q_GAM_BC, vy_GAM_BC = predict(fit_loc_GAM_BC, newdata = vx) - vx, vy_GAM_BC. = predict(fit_loc_GAM_BC, newdata = vx)/vx, vy_GAM = predict(fit_loc_GAM, newdata = vx) - vx, vy_GAM. = predict(fit_loc_GAM, newdata = vx)/vx)
df_GBM <- data.frame(Quantile = q_GBM, Quantile. = q_GBM_BC, vy_GBM_BC = predict(fit_loc_GBM_BC, newdata = vx) - vx, vy_GBM_BC. = predict(fit_loc_GBM_BC, newdata = vx)/vx, vy_GBM = predict(fit_loc_GBM, newdata = vx) - vx, vy_GBM. = predict(fit_loc_GBM, newdata = vx)/vx)

# Tracer les graphiques en utilisant ggplot2
p1 <- ggplot(df_GLM, aes(x = Quantile., y = vy_GLM_BC.)) +
  geom_line(color = clrpal[1], size = 1.1) +
  geom_line(data = df_GLM, aes(x = Quantile, y = vy_GLM.), color = clrpal[1]) +
  labs(x = "Quantile de fréquence (GLM)", y = "", title = "") +
  xlim(0, 1) +
  ylim(0.5, 1.6) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", size = 0.4)

p2 <- ggplot(df_GAM, aes(x = Quantile., y = vy_GAM_BC.)) +
  geom_line(color = clrpal[3], size = 1.1) +
  geom_line(data = df_GAM, aes(x = Quantile, y = vy_GAM.), color = clrpal[3]) +
  labs(x = "Quantile de fréquence (GAM)", y = "", title = "") +
  xlim(0, 1) +
  ylim(0.5, 1.6) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", size = 0.4)

p3 <- ggplot(df_GBM, aes(x = Quantile., y = vy_GBM_BC.)) +
  geom_line(color = clrpal[5], size = 1.1) +
  geom_line(data = df_GBM, aes(x = Quantile, y = vy_GBM.), color = clrpal[5]) +
  labs(x = "Quantile de fréquence (GBM)", y = "", title = "") +
  xlim(0, 1) +
  ylim(0.5, 1.6) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", size = 0.4)

# Afficher les graphiques côte à côte
library(gridExtra)
grid.arrange(p1, p2, p3, ncol = 3)
```

![](Mémoire_Actuariat_John_files/figure-gfm/unnamed-chunk-65-1.png)<!-- -->

``` r
# Créer des data frames pour chaque modèle
df_GLM_BC <- data.frame(Quantile = q_GLM_BC, vy_GLM_BC = predict(fit_loc_GLM_BC, newdata = vx))
df_GLM <- data.frame(Quantile = q_GLM, vy_GLM = predict(fit_loc_GLM, newdata = vx), vy_GLM. = predict(fit_loc_GLM, newdata = vx)/vx, vx = vx)
df_GAM_BC <- data.frame(Quantile = q_GAM_BC, vy_GAM_BC = predict(fit_loc_GAM_BC, newdata = vx))
df_GAM <- data.frame(Quantile = q_GAM, vy_GAM = predict(fit_loc_GAM, newdata = vx), vy_GAM. = predict(fit_loc_GAM, newdata = vx)/vx, vx = vx)
df_GBM_BC <- data.frame(Quantile = q_GBM_BC, vy_GBM_BC = predict(fit_loc_GBM_BC, newdata = vx))
df_GBM <- data.frame(Quantile = q_GBM, vy_GBM = predict(fit_loc_GBM, newdata = vx), vy_GBM. = predict(fit_loc_GBM, newdata = vx)/vx, vx = vx)

# Définir le thème
theme_set(theme_bw())

# Tracer les graphiques en utilisant ggplot2
p1 <- ggplot(df_GLM_BC, aes(x = Quantile, y = vy_GLM_BC)) +
  geom_line(color = clrpal[1], size = 1.1) +
  geom_line(data = df_GLM, aes(x = Quantile, y = vx), color = "gray25") +
  geom_line(data = df_GLM, aes(x = Quantile, y = vy_GLM), color = "green4") +
  labs(x = "Quantile de prime (GLM)", y = "", title = "") +
  xlim(0, 1) +
  ylim(0, 0.2) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray25", size = 0.4)

p2 <- ggplot(df_GAM_BC, aes(x = Quantile, y = vy_GAM_BC)) +
  geom_line(color = clrpal[1], size = 1.1) +
  geom_line(data = df_GAM, aes(x = Quantile, y = vx), color = "gray25") +
  geom_line(data = df_GAM, aes(x = Quantile, y = vy_GAM), color = "green4") +
  labs(x = "Quantile de prime (GAM)", y = "", title = "") +
  xlim(0, 1) +
  ylim(0, 0.2) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray25", size = 0.4)

p3 <- ggplot(df_GBM_BC, aes(x = Quantile, y = vy_GBM_BC)) +
  geom_line(color = clrpal[1], size = 1.1) +
  geom_line(data = df_GBM, aes(x = Quantile, y = vx), color = "gray25") +
  geom_line(data = df_GBM, aes(x = Quantile, y = vy_GBM), color = "green4") +
  labs(x = "Quantile de prime (boosting)", y = "", title = "") +
  xlim(0, 1) +
  ylim(0, 0.2) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray25", size = 0.4)

# Afficher les graphiques côte à côte
library(gridExtra)
grid.arrange(p1, p2, p3, ncol = 3)
```

![](Mémoire_Actuariat_John_files/figure-gfm/unnamed-chunk-66-1.png)<!-- -->

# Graphique quantiles simples

``` r
# Trier l'ensemble de données en fonction des valeurs de GLM
datatest <- datatest[order(datatest$GLM), ]

# Nombre de sous-ensembles
K <- 10

# Calculer la valeur moyenne prédite de l'ensemble de test pour la normalisation
avg_predicted_value <- mean(datatest$GLM)

# Répartir les données en classes de même taille en fonction des sous-ensembles utilisant GLM
datatest$Subset_GLM <- cut(seq(nrow(datatest)), breaks = K, labels = FALSE)

# Calculer la valeur moyenne des pertes attendues et des pertes réelles (ClaimNb) dans chaque sous-ensemble en utilisant GLM
lift_data <- datatest %>%
  group_by(Subset_GLM) %>%
  summarize(
    Mean_Observed = mean(ClaimNb) / avg_predicted_value,
    Mean_Predicted_GLM = mean(GLM) / avg_predicted_value,
    Mean_Predicted_GLM_BC = mean(GLM_BC) / avg_predicted_value,
    Init_lift = abs(mean(ClaimNb) / avg_predicted_value - mean(GLM) / avg_predicted_value),
    Act_lift = mean(ClaimNb) / avg_predicted_value - mean(GLM_BC) / avg_predicted_value
  )
tibble(Lift_Avant_AC_GLM = sum(lift_data$Init_lift), Lift_Après_AC_GLM = sum(lift_data$Act_lift))
```

    ## # A tibble: 1 × 2
    ##   Lift_Avant_AC_GLM Lift_Après_AC_GLM
    ##               <dbl>             <dbl>
    ## 1              2.78             0.214

``` r
# Tracer la courbe de lift en utilisant ggplot2
library(ggplot2)

# Palette de couleurs
clrpal <- c("#236B8E", "gray37", "#AED4E6")

p1 <- ggplot(data = lift_data, aes(x = Subset_GLM)) +
  geom_line(aes(y = Mean_Observed, color = "Observations")) +
  geom_line(aes(y = Mean_Predicted_GLM, color = "Modèle sans BC (GLM)")) +
  geom_line(aes(y = Mean_Predicted_GLM_BC, color = "Modèle BC (GLM_BC)")) +
  geom_point(aes(y = Mean_Observed, color = "Observations")) +
  geom_point(aes(y = Mean_Predicted_GLM, color = "Modèle sans BC (GLM)")) +
  geom_point(aes(y = Mean_Predicted_GLM_BC, color = "Modèle BC (GLM_BC)")) +
  labs(x = "Déciles", y = "Lift Normalisé", title = "") +
  scale_y_continuous(limits = c(0, 4)) +
  scale_x_continuous(breaks = 1:10) +
  scale_color_manual(values = clrpal) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 8)
  )
```

``` r
# Trier l'ensemble de données en fonction des valeurs de GAM
datatest <- datatest[order(datatest$GAM), ]

# Nombre de sous-ensembles
K <- 10

# Calculer la valeur moyenne prédite de l'ensemble de test pour la normalisation
avg_predicted_value <- mean(datatest$GAM)

# Répartir les données en classes de même taille en fonction des sous-ensembles utilisant GAM
datatest$Subset_GAM <- cut(seq(nrow(datatest)), breaks = K, labels = FALSE)

# Calculer la valeur moyenne des pertes attendues et des pertes réelles (ClaimNb) dans chaque sous-ensemble en utilisant GAM
lift_data <- datatest %>%
  group_by(Subset_GAM) %>%
  summarize(
    Mean_Observed = mean(ClaimNb) / avg_predicted_value,
    Mean_Predicted_GAM = mean(GAM) / avg_predicted_value,
    Mean_Predicted_GAM_BC = mean(GAM_BC) / avg_predicted_value,
    Init_lift = abs(mean(ClaimNb) / avg_predicted_value - mean(GAM) / avg_predicted_value),
    Act_lift = abs(mean(ClaimNb) / avg_predicted_value - mean(GAM_BC) / avg_predicted_value)
  )

tibble(Lift_Avant_AC_GAM = sum(lift_data$Init_lift), Lift_Après_AC_GAM = sum(lift_data$Act_lift))
```

    ## # A tibble: 1 × 2
    ##   Lift_Avant_AC_GAM Lift_Après_AC_GAM
    ##               <dbl>             <dbl>
    ## 1              2.68             0.745

``` r
# Tracer la courbe de lift en utilisant ggplot2
library(ggplot2)

# Palette de couleurs
clrpal <- c("#236B8E", "gray37", "#AED4E6")

p2 <- ggplot(data = lift_data, aes(x = Subset_GAM)) +
  geom_line(aes(y = Mean_Observed, color = "Observations")) +
  geom_line(aes(y = Mean_Predicted_GAM, color = "Modèle sans BC (GAM)")) +
  geom_line(aes(y = Mean_Predicted_GAM_BC, color = "Modèle BC (GAM_BC)")) +
  geom_point(aes(y = Mean_Observed, color = "Observations")) +
  geom_point(aes(y = Mean_Predicted_GAM, color = "Modèle sans BC (GAM)")) +
  geom_point(aes(y = Mean_Predicted_GAM_BC, color = "Modèle BC (GAM_BC)")) +
  labs(x = "Déciles", y = "Lift Normalisé", title = "") +
  scale_y_continuous(limits = c(0, 4)) +
  scale_x_continuous(breaks = 1:10) +
  scale_color_manual(values = clrpal) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 8)
  )
```

``` r
# Trier l'ensemble de données en fonction des valeurs de GBM
datatest <- datatest[order(datatest$GBM), ]

# Nombre de sous-ensembles
K <- 10

# Calculer la valeur moyenne prédite de l'ensemble de test pour la normalisation
avg_predicted_value <- mean(datatest$GBM)

# Répartir les données en classes de même taille en fonction des sous-ensembles utilisant GBM
datatest$Subset_GBM <- cut(seq(nrow(datatest)), breaks = K, labels = FALSE)

# Calculer la valeur moyenne des pertes attendues et des pertes réelles (ClaimNb) dans chaque sous-ensemble en utilisant GBM
lift_data <- datatest %>%
  group_by(Subset_GBM) %>%
  summarize(
    Mean_Observed = mean(ClaimNb) / avg_predicted_value,
    Mean_Predicted_GBM = mean(GBM) / avg_predicted_value,
    Mean_Predicted_GBM_BC = mean(GBM_BC) / avg_predicted_value,
    Init_lift = abs(mean(ClaimNb) / avg_predicted_value - mean(GBM) / avg_predicted_value),
    Act_lift = abs(mean(ClaimNb) / avg_predicted_value - mean(GBM_BC) / avg_predicted_value)
  )

tibble(Lift_Avant_AC_GBM = sum(lift_data$Init_lift), Lift_Après_AC_GBM = sum(lift_data$Act_lift))
```

    ## # A tibble: 1 × 2
    ##   Lift_Avant_AC_GBM Lift_Après_AC_GBM
    ##               <dbl>             <dbl>
    ## 1              1.97              1.01

``` r
# Tracer la courbe de lift en utilisant ggplot2
library(ggplot2)

# Palette de couleurs
clrpal <- c("#236B8E", "gray37", "#AED4E6")

p3 <- ggplot(data = lift_data, aes(x = Subset_GBM)) +
  geom_line(aes(y = Mean_Observed, color = "Observations")) +
  geom_line(aes(y = Mean_Predicted_GBM, color = "Modèle sans BC (GAM)")) +
  geom_line(aes(y = Mean_Predicted_GBM_BC, color = "Modèle BC (GAM_BC)")) +
  geom_point(aes(y = Mean_Observed, color = "Observations")) +
  geom_point(aes(y = Mean_Predicted_GBM, color = "Modèle sans BC (GAM)")) +
  geom_point(aes(y = Mean_Predicted_GBM_BC, color = "Modèle BC (GAM_BC)")) +
  labs(x = "Déciles", y = "Lift Normalisé", title = "") +
  scale_y_continuous(limits = c(0, 4)) +
  scale_x_continuous(breaks = 1:10) +
  scale_color_manual(values = clrpal) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 8)
  )
grid.arrange(p1, p2, p3, ncol = 3)
```

![](Mémoire_Actuariat_John_files/figure-gfm/unnamed-chunk-69-1.png)<!-- -->

# Double lift

``` r
# Trier l'ensemble de données en fonction du ratio entre les primes après et avant l'autocalibration
datatest$Ratio <- datatest$GLM_BC / datatest$GLM
datatest <- datatest[order(datatest$Ratio), ]

# Nombre de sous-ensembles
K <- 10

# Calculer la valeur moyenne prédite de l'ensemble de test pour la normalisation
avg_predicted_value_GLM <- mean(datatest$GLM)
avg_predicted_value_GLM_BC <- mean(datatest$GLM_BC)
avg_claim_nb <- mean(datatest$ClaimNb)

# Répartir les données en classes de même taille en fonction du ratio
datatest$Subset_GLM <- cut(seq(nrow(datatest)), breaks = K, labels = FALSE)

# Calculer la valeur moyenne prédite avant et après l'autocalibration, ainsi que la valeur moyenne observée dans chaque sous-ensemble
lift_data <- datatest %>%
  group_by(Subset_GLM) %>%
  summarize(
    Mean_Observed = mean(ClaimNb) / avg_claim_nb,
    Mean_Predicted_GLM = mean(GLM) / avg_predicted_value_GLM,
    Mean_Predicted_GLM_BC = mean(GLM_BC) / avg_predicted_value_GLM_BC
  )

# Tracer les Courbes de Lift Double en utilisant ggplot2
library(ggplot2)

# Palette de couleurs
clrpal <- c("#236B8E", "gray37", "#AED4E6")

p1 <- ggplot(data = lift_data, aes(x = Subset_GLM)) +
  geom_line(aes(y = Mean_Observed, color = "Observations")) +
  geom_line(aes(y = Mean_Predicted_GLM, color = "Modèle sans BC (GLM)")) +
  geom_line(aes(y = Mean_Predicted_GLM_BC, color = "Modèle BC (GLM_BC)")) +
  geom_point(aes(y = Mean_Observed, color = "Observations")) +
  geom_point(aes(y = Mean_Predicted_GLM, color = "Modèle sans BC (GLM)")) +
  geom_point(aes(y = Mean_Predicted_GLM_BC, color = "Modèle BC (GLM_BC)")) +
  labs(x = "Déciles", y = "Lift Normalisé", title = "") +
  scale_y_continuous(limits = c(0, 2.5)) +
  scale_x_continuous(breaks = 1:10) +
  scale_color_manual(values = clrpal) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 8)
  )
```

``` r
# Trier l'ensemble de données en fonction du ratio entre les primes après et avant l'autocalibration
datatest$Ratio <- datatest$GAM_BC / datatest$GAM
datatest <- datatest[order(datatest$Ratio), ]

# Nombre de sous-ensembles
K <- 10

# Calculer la valeur moyenne prédite de l'ensemble de test pour la normalisation
avg_predicted_value_GAM <- mean(datatest$GAM)
avg_predicted_value_GAM_BC <- mean(datatest$GAM_BC)
avg_claim_nb <- mean(datatest$ClaimNb)

# Répartir les données en classes de même taille en fonction du ratio
datatest$Subset_GAM <- cut(seq(nrow(datatest)), breaks = K, labels = FALSE)

# Calculer la valeur moyenne prédite avant et après l'autocalibration, ainsi que la valeur moyenne observée dans chaque sous-ensemble
lift_data <- datatest %>%
  group_by(Subset_GAM) %>%
  summarize(
    Mean_Observed = mean(ClaimNb) / avg_claim_nb,
    Mean_Predicted_GAM = mean(GAM) / avg_predicted_value_GAM,
    Mean_Predicted_GAM_BC = mean(GAM_BC) / avg_predicted_value_GAM_BC
  )

# Tracer les Courbes de Lift Double en utilisant ggplot2
library(ggplot2)

# Palette de couleurs
clrpal <- c("#236B8E", "gray37", "#AED4E6")

p2 <- ggplot(data = lift_data, aes(x = Subset_GAM)) +
  geom_line(aes(y = Mean_Observed, color = "Observations")) +
  geom_line(aes(y = Mean_Predicted_GAM, color = "Modèle sans BC (GAM)")) +
  geom_line(aes(y = Mean_Predicted_GAM_BC, color = "Modèle BC (GAM_BC)")) +
  geom_point(aes(y = Mean_Observed, color = "Observations")) +
  geom_point(aes(y = Mean_Predicted_GAM, color = "Modèle sans BC (GAM)")) +
  geom_point(aes(y = Mean_Predicted_GAM_BC, color = "Modèle BC (GAM_BC)")) +
  labs(x = "Déciles", y = "Lift Normalisé", title = "") +
  scale_y_continuous(limits = c(0, 2.5)) +
  scale_x_continuous(breaks = 1:10) +
  scale_color_manual(values = clrpal) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 8)
  )
```

``` r
# Trier l'ensemble de données en fonction du ratio entre les primes après et avant l'autocalibration
datatest$Ratio <- datatest$GBM_BC / datatest$GBM
datatest <- datatest[order(datatest$Ratio), ]

# Nombre de sous-ensembles
K <- 10

# Calculer la valeur moyenne prédite de l'ensemble de test pour la normalisation
avg_predicted_value_GBM <- mean(datatest$GBM)
avg_predicted_value_GBM_BC <- mean(datatest$GBM_BC)
avg_claim_nb <- mean(datatest$ClaimNb)

# Répartir les données en classes de même taille en fonction du ratio
datatest$Subset_GBM <- cut(seq(nrow(datatest)), breaks = K, labels = FALSE)

# Calculer la valeur moyenne prédite avant et après l'autocalibration, ainsi que la valeur moyenne observée dans chaque sous-ensemble
lift_data <- datatest %>%
  group_by(Subset_GBM) %>%
  summarize(
    Mean_Observed = mean(ClaimNb) / avg_claim_nb,
    Mean_Predicted_GBM = mean(GBM) / avg_predicted_value_GBM,
    Mean_Predicted_GBM_BC = mean(GBM_BC) / avg_predicted_value_GBM_BC
  )

# Tracer les Courbes de Lift Double en utilisant ggplot2
library(ggplot2)

# Palette de couleurs
clrpal <- c("#236B8E", "gray37", "#AED4E6")

p3 <- ggplot(data = lift_data, aes(x = Subset_GBM)) +
  geom_line(aes(y = Mean_Observed, color = "Observations")) +
  geom_line(aes(y = Mean_Predicted_GBM, color = "Modèle sans BC (GBM)")) +
  geom_line(aes(y = Mean_Predicted_GBM_BC, color = "Modèle BC (GBM_BC)")) +
  geom_point(aes(y = Mean_Observed, color = "Observations")) +
  geom_point(aes(y = Mean_Predicted_GBM, color = "Modèle sans BC (GBM)")) +
  geom_point(aes(y = Mean_Predicted_GBM_BC, color = "Modèle BC (GBM_BC)")) +
  labs(x = "Déciles", y = "Lift Normalisé", title = "") +
  scale_y_continuous(limits = c(0, 2.5)) +
  scale_x_continuous(breaks = 1:10) +
  scale_color_manual(values = clrpal) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
  )
grid.arrange(p1, p2, p3, ncol = 3)
```

![](Mémoire_Actuariat_John_files/figure-gfm/unnamed-chunk-72-1.png)<!-- -->

# Déviance poisson

``` r
comp_tab <-NULL
comp_tab <- tibble(
  model_use = character(),
  dev1 = numeric(),
  dev2 = numeric(),
  dev3 = numeric())
names(comp_tab) <- c("Model","In sample loss (%)","Out sample loss test (%)", "Out sample loss datavalid", "Data test frequency")
```

``` r
comp_tab[1, ] <- list("GLM", round(PoissonDeviance(datalearning$ClaimNb, datalearning$GLM), 6),
                   round(PoissonDeviance(datatest$ClaimNb, datatest$GLM), 6),
                   round(PoissonDeviance(datavalid$ClaimNb, datavalid$GLM), 6))

comp_tab[2, ] <- list("GAM", round(PoissonDeviance(datalearning$ClaimNb, datalearning$GAM), 6),
                   round(PoissonDeviance(datatest$ClaimNb, datatest$GAM), 6),
                   round(PoissonDeviance(datavalid$ClaimNb, datavalid$GAM), 6))

comp_tab[3, ] <- list("GBM", round(PoissonDeviance(datalearning$ClaimNb, datalearning$GBM), 6),
                   round(PoissonDeviance(datatest$ClaimNb, datatest$GBM), 6),
                   round(PoissonDeviance(datavalid$ClaimNb, datavalid$GBM), 6))

knitr::kable(comp_tab)
```

| Model | In sample loss (%) | Out sample loss test (%) | Out sample loss datavalid |
|:------|-------------------:|-------------------------:|--------------------------:|
| GLM   |           0.318888 |                 0.328850 |                  0.317176 |
| GAM   |           0.316126 |                 0.326025 |                  0.314636 |
| GBM   |           0.298511 |                 0.313775 |                  0.303663 |

``` r
# sum(datavalid$ClaimNb)/sum(datavalid$Exposure)
comp_tab[4, ] <- list("GLM_BC", round(PoissonDeviance(datalearning$ClaimNb, datalearning$GLM_BC), 6),
                   round(PoissonDeviance(datatest$ClaimNb, datatest$GLM_BC), 6),
                   round(PoissonDeviance(datavalid$ClaimNb, datavalid$GLM_BC), 6))

comp_tab[5, ] <- list("GAM_BC", round(PoissonDeviance(datalearning$ClaimNb, datalearning$GAM_BC), 6),
                   round(PoissonDeviance(datatest$ClaimNb, datatest$GAM_BC), 6),
                   round(PoissonDeviance(datavalid$ClaimNb, datavalid$GAM_BC), 6))

comp_tab[6, ] <- list("GBM_BC", round(PoissonDeviance(datalearning$ClaimNb, datalearning$GBM_BC), 6),
                   round(PoissonDeviance(datatest$ClaimNb, datatest$GBM_BC), 6),
                   round(PoissonDeviance(datavalid$ClaimNb, datavalid$GBM_BC), 6))

knitr::kable(comp_tab)
```

| Model  | In sample loss (%) | Out sample loss test (%) | Out sample loss datavalid |
|:-------|-------------------:|-------------------------:|--------------------------:|
| GLM    |           0.318888 |                 0.328850 |                  0.317176 |
| GAM    |           0.316126 |                 0.326025 |                  0.314636 |
| GBM    |           0.298511 |                 0.313775 |                  0.303663 |
| GLM_BC |           0.309105 |                 0.317672 |                  0.307628 |
| GAM_BC |           0.306350 |                 0.314664 |                  0.304618 |
| GBM_BC |           0.294559 |                 0.307047 |                  0.297504 |

``` r
# Charger les bibliothèques
library(ggplot2)

# Définir la matrice des déviations Out sample loss test avec et sans BC
dev <- matrix(
  comp_tab$`Out sample loss test (%)`,
  nrow = 3,
  byrow = FALSE,
  dimnames = list(c("GLM", "GAM", "GBM"), c("Sans BC", "Avec BC"))
)

# Créer le graphique de comparaison des déviations Out sample loss test avec et sans BC
ggplot(data = as.data.frame(dev), aes(x = rownames(dev))) +
   geom_point(aes(y = `Sans BC`, color = "Sans autocalibration"), size = 4) +
  geom_point(aes(y = `Avec BC`, color = "Avec autocalibration"), size = 4) +
  labs(x = "Modèles", y = "Déviances", title = "Comparaison des déviances") +
  scale_fill_manual(values = c("#236B8E", "#AED4E6")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 8))
```

![](Mémoire_Actuariat_John_files/figure-gfm/unnamed-chunk-76-1.png)<!-- -->
