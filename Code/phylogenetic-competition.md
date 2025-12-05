Untitled
================

\##Loading

``` r
library(openxlsx)
library(data.table)
library(devtools)
```

    ## Loading required package: usethis

``` r
library(vegan)
```

    ## Loading required package: permute

    ## 
    ## Attaching package: 'permute'

    ## The following object is masked from 'package:devtools':
    ## 
    ##     check

``` r
library(lmerTest)
```

    ## Loading required package: lme4

    ## Warning: package 'lme4' was built under R version 4.5.2

    ## Loading required package: Matrix

    ## 
    ## Attaching package: 'lmerTest'

    ## The following object is masked from 'package:lme4':
    ## 
    ##     lmer

    ## The following object is masked from 'package:stats':
    ## 
    ##     step

``` r
library(performance)
library(emmeans)
```

    ## Welcome to emmeans.
    ## Caution: You lose important information if you filter this package's results.
    ## See '? untidy'

    ## 
    ## Attaching package: 'emmeans'

    ## The following object is masked from 'package:devtools':
    ## 
    ##     test

``` r
library(multcomp)
```

    ## Loading required package: mvtnorm

    ## Loading required package: survival

    ## Loading required package: TH.data

    ## Warning: package 'TH.data' was built under R version 4.5.2

    ## Loading required package: MASS

    ## 
    ## Attaching package: 'TH.data'

    ## The following object is masked from 'package:MASS':
    ## 
    ##     geyser

``` r
library(goeveg)
```

    ## This is GoeVeg 0.7.9 - build: 2025-09-02

``` r
library(viridis)
```

    ## Loading required package: viridisLite

``` r
library(glmmTMB)
```

    ## Warning in check_dep_version(dep_pkg = "TMB"): package version mismatch: 
    ## glmmTMB was built with TMB package version 1.9.17
    ## Current TMB package version is 1.9.18
    ## Please re-install glmmTMB from source or restore original 'TMB' package (see '?reinstalling' for more information)

``` r
library(DHARMa)
```

    ## This is DHARMa 0.4.7. For overview type '?DHARMa'. For recent changes, type news(package = 'DHARMa')

``` r
#devtools::install_github("jinyizju/V.PhyloMaker2")
library(V.PhyloMaker2) #manually install if in lib
```

    ## Loading required package: ape

``` r
#if (!require("BiocManager", quietly = TRUE)) install.packages("BiocManager")
#BiocManager::install("ggtree")
library(ggtree) #manually install if in lib
```

    ## ggtree v4.0.1 Learn more at https://yulab-smu.top/contribution-tree-data/
    ## 
    ## Please cite:
    ## 
    ## Guangchuang Yu.  Data Integration, Manipulation and Visualization of
    ## Phylogenetic Trees (1st edition). Chapman and Hall/CRC. 2022,
    ## doi:10.1201/9781003279242, ISBN: 9781032233574

    ## 
    ## Attaching package: 'ggtree'

    ## The following object is masked from 'package:ape':
    ## 
    ##     rotate

    ## The following object is masked from 'package:Matrix':
    ## 
    ##     expand

``` r
library(ape)
library(picante)
```

    ## Loading required package: nlme

    ## 
    ## Attaching package: 'nlme'

    ## The following object is masked from 'package:ggtree':
    ## 
    ##     collapse

    ## The following object is masked from 'package:lme4':
    ## 
    ##     lmList

``` r
#remove.packages("geiger")
#remotes::install_version("geiger", version = "2.0.9", repos = "http://cran.us.r-project.org")
#devtools::install_github("eliotmiller/metricTester")
library(geiger)
```

    ## 
    ## Attaching package: 'geiger'

    ## The following object is masked from 'package:goeveg':
    ## 
    ##     sem

``` r
library(metricTester)
library(interactions)
#BiocManager::install("ComplexHeatmap")
library(ComplexHeatmap) #manually install if in lib
```

    ## Loading required package: grid

    ## ========================================
    ## ComplexHeatmap version 2.26.0
    ## Bioconductor page: http://bioconductor.org/packages/ComplexHeatmap/
    ## Github page: https://github.com/jokergoo/ComplexHeatmap
    ## Documentation: http://jokergoo.github.io/ComplexHeatmap-reference
    ## 
    ## If you use it in published research, please cite either one:
    ## - Gu, Z. Complex Heatmap Visualization. iMeta 2022.
    ## - Gu, Z. Complex heatmaps reveal patterns and correlations in multidimensional 
    ##     genomic data. Bioinformatics 2016.
    ## 
    ## 
    ## The new InteractiveComplexHeatmap package can directly export static 
    ## complex heatmaps into an interactive Shiny app with zero effort. Have a try!
    ## 
    ## This message can be suppressed by:
    ##   suppressPackageStartupMessages(library(ComplexHeatmap))
    ## ========================================

``` r
library(tidyverse)
```

    ## Warning: package 'ggplot2' was built under R version 4.5.2

    ## Warning: package 'readr' was built under R version 4.5.2

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.6
    ## ✔ forcats   1.0.1     ✔ stringr   1.6.0
    ## ✔ ggplot2   4.0.1     ✔ tibble    3.3.0
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.2.0

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::between()     masks data.table::between()
    ## ✖ dplyr::collapse()    masks nlme::collapse(), ggtree::collapse()
    ## ✖ tidyr::expand()      masks ggtree::expand(), Matrix::expand()
    ## ✖ dplyr::filter()      masks stats::filter()
    ## ✖ dplyr::first()       masks data.table::first()
    ## ✖ lubridate::hour()    masks data.table::hour()
    ## ✖ lubridate::isoweek() masks data.table::isoweek()
    ## ✖ dplyr::lag()         masks stats::lag()
    ## ✖ dplyr::last()        masks data.table::last()
    ## ✖ lubridate::mday()    masks data.table::mday()
    ## ✖ lubridate::minute()  masks data.table::minute()
    ## ✖ lubridate::month()   masks data.table::month()
    ## ✖ tidyr::pack()        masks Matrix::pack()
    ## ✖ lubridate::quarter() masks data.table::quarter()
    ## ✖ lubridate::second()  masks data.table::second()
    ## ✖ dplyr::select()      masks MASS::select()
    ## ✖ purrr::transpose()   masks data.table::transpose()
    ## ✖ tidyr::unpack()      masks Matrix::unpack()
    ## ✖ lubridate::wday()    masks data.table::wday()
    ## ✖ lubridate::week()    masks data.table::week()
    ## ✖ dplyr::where()       masks ape::where()
    ## ✖ lubridate::yday()    masks data.table::yday()
    ## ✖ lubridate::year()    masks data.table::year()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
MeanPlusSe<-function(x) mean(x)+plotrix::std.error(x)
myCol <- viridis_pal(option = "G", begin = 0.175, end = 0.775, direction = -1)(4)
myCol[5:8] <- viridis_pal(option = "A", begin = 0.35, end = .775, direction = -1)(4)
scales::show_col(myCol)
```

![](phylogenetic-competition_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

COOP2_FH_P7 seems to be an outlier

Phylogenetic analysis:

1.  Load data into a common data frame

2.  Load name key

3.  Remove unknowns from raw data and name key

4.  Check for Family matches in the appendix of V.PhyloMaker2 -
    confirmed 9/23/22

    1.  Check that all species are in the GBOTB.extended.WP.tre and list
        related species accordingly

    2.  Make tree using relatives; Senario.3 and BuildNodes.1 -
        confirmed 10/01/22

    3.  Check for genera that are not be monophyletic - confirmed
        10/01/22 5. Make a phylogentic tree ultimetic

``` r
COOP1Data<-read.xlsx("../Data/COOP1_weedsHighRes.xlsx")
COOP2Data<-read.xlsx("../Data/COOP2_weedsHighRes.xlsx")
COOP3Data<-read.xlsx("../Data/COOP3_weedsHighRes.xlsx")
COOP4Data<-read.xlsx("../Data/COOP4_weedsHighRes.xlsx")

COOP1Data$Block<-as.numeric(COOP1Data$Block)

FullData<-bind_rows(COOP1Data,COOP2Data,COOP3Data,COOP4Data)
#write.xlsx(FullData,"FullData.xlsx")
```

# Prep

**Outcomes:**

*Data frames*

- Full data includes cover crops and their biomass

- Weed data removed the six cover crop species to focus only on weed
  communities

*Modeling data*

- Abundance data: …SpClean, …SpClean_Sum, …SpClean_Wint

- Phylogenetics: …tree_boundDicot,…tree_boundDicot_Sum,
  …tree_boundDicot_Wint

``` r
# Abundance data----

#Replace all NAs with 0
FullData<-FullData %>% 
    mutate(across(.cols = -c(Trial,Site,Year,Block,Plot,
                           CoverCrop,CoverCropBiomass),
                .fns = ~replace_na(.,0)))

#Reset true NAs (COOP2_FH P14) and bind with 0-corrected data
#Created "experiment" variable
#Set factors and their labels
FullData<-FullData %>% 
    filter(Trial == "COOP2", Site == "Farm Hub", Plot == "14") %>%
    mutate(across(.cols = -c(Trial,Site,Year,Block,Plot,CoverCrop,CoverCropBiomass),
                  .fns = ~replace(.,.==0,NA))) %>%
    rbind(FullData %>%
            filter(!(Trial == "COOP2" & Site == "Farm Hub" & Plot == "14"))) %>% 
    mutate(Experiment = case_when(
              Trial %in% c("COOP1", "COOP3") ~ "Summer cover crops",
              Trial %in% c("COOP2", "COOP4") ~ "Winter cover crops"),
           across(.cols = c(Experiment,Trial,Site,Block,Year,CoverCrop),
                  .fns = factor)) %>%
    select(Experiment,Trial,Site,Year,Block,Plot,
           CoverCrop,CoverCropBiomass, everything()) %>% 
    arrange(Trial, Site,Plot)

FullData$CoverCrop<-factor(FullData$CoverCrop,
                            levels = c("Tilled","CAN","CR",
                                      "HV","HVxCR","Buckwheat",
                                      "SH","SS","SSxSH"),
                            labels = c("Tilled","Canola","Cereal rye",
                                      "Hairy vetch","HVxCR","Buckwheat",
                                      "Sunn hemp","S. sudangrass", "SSxSH"))

table(FullData$CoverCrop,FullData$Block,FullData$Trial,FullData$Site)
```

    ## , ,  = COOP1,  = Farm Hub
    ## 
    ##                
    ##                 1 2 3 4
    ##   Tilled        1 1 1 1
    ##   Canola        0 0 0 0
    ##   Cereal rye    0 0 0 0
    ##   Hairy vetch   0 0 0 0
    ##   HVxCR         0 0 0 0
    ##   Buckwheat     1 1 1 1
    ##   Sunn hemp     1 1 1 1
    ##   S. sudangrass 1 1 1 1
    ##   SSxSH         1 1 1 1
    ## 
    ## , ,  = COOP2,  = Farm Hub
    ## 
    ##                
    ##                 1 2 3 4
    ##   Tilled        1 1 1 1
    ##   Canola        1 1 1 1
    ##   Cereal rye    1 1 1 1
    ##   Hairy vetch   1 1 1 1
    ##   HVxCR         1 1 1 1
    ##   Buckwheat     0 0 0 0
    ##   Sunn hemp     0 0 0 0
    ##   S. sudangrass 0 0 0 0
    ##   SSxSH         0 0 0 0
    ## 
    ## , ,  = COOP3,  = Farm Hub
    ## 
    ##                
    ##                 1 2 3 4
    ##   Tilled        1 1 1 1
    ##   Canola        0 0 0 0
    ##   Cereal rye    0 0 0 0
    ##   Hairy vetch   0 0 0 0
    ##   HVxCR         0 0 0 0
    ##   Buckwheat     1 1 1 1
    ##   Sunn hemp     1 1 1 1
    ##   S. sudangrass 1 1 1 1
    ##   SSxSH         1 1 1 1
    ## 
    ## , ,  = COOP4,  = Farm Hub
    ## 
    ##                
    ##                 1 2 3 4
    ##   Tilled        1 1 1 1
    ##   Canola        1 1 1 1
    ##   Cereal rye    1 1 1 1
    ##   Hairy vetch   1 1 1 1
    ##   HVxCR         1 1 1 1
    ##   Buckwheat     0 0 0 0
    ##   Sunn hemp     0 0 0 0
    ##   S. sudangrass 0 0 0 0
    ##   SSxSH         0 0 0 0
    ## 
    ## , ,  = COOP1,  = Musgrave
    ## 
    ##                
    ##                 1 2 3 4
    ##   Tilled        1 1 1 1
    ##   Canola        0 0 0 0
    ##   Cereal rye    0 0 0 0
    ##   Hairy vetch   0 0 0 0
    ##   HVxCR         0 0 0 0
    ##   Buckwheat     1 1 1 1
    ##   Sunn hemp     1 1 1 1
    ##   S. sudangrass 1 1 1 1
    ##   SSxSH         1 1 1 1
    ## 
    ## , ,  = COOP2,  = Musgrave
    ## 
    ##                
    ##                 1 2 3 4
    ##   Tilled        1 1 1 1
    ##   Canola        1 1 1 1
    ##   Cereal rye    1 1 1 1
    ##   Hairy vetch   1 1 1 1
    ##   HVxCR         1 1 1 1
    ##   Buckwheat     0 0 0 0
    ##   Sunn hemp     0 0 0 0
    ##   S. sudangrass 0 0 0 0
    ##   SSxSH         0 0 0 0
    ## 
    ## , ,  = COOP3,  = Musgrave
    ## 
    ##                
    ##                 1 2 3 4
    ##   Tilled        1 1 1 1
    ##   Canola        0 0 0 0
    ##   Cereal rye    0 0 0 0
    ##   Hairy vetch   0 0 0 0
    ##   HVxCR         0 0 0 0
    ##   Buckwheat     1 1 1 1
    ##   Sunn hemp     1 1 1 1
    ##   S. sudangrass 1 1 1 1
    ##   SSxSH         1 1 1 1
    ## 
    ## , ,  = COOP4,  = Musgrave
    ## 
    ##                
    ##                 1 2 3 4
    ##   Tilled        1 1 1 1
    ##   Canola        1 1 1 1
    ##   Cereal rye    1 1 1 1
    ##   Hairy vetch   1 1 1 1
    ##   HVxCR         1 1 1 1
    ##   Buckwheat     0 0 0 0
    ##   Sunn hemp     0 0 0 0
    ##   S. sudangrass 0 0 0 0
    ##   SSxSH         0 0 0 0

``` r
#Removing cover crops from the weed data to focus only on weed communities
##Confirmed that the cover crop columns are not weeds and can be removed
WeedData <- FullData

WeedDataTmp<-WeedData %>%
  select(Trial,Site,Year,Block,Plot,
         CoverCrop,
         buckwheat,sun.hemp,sorghum.sudangrass,
         canola,cereal.rye,hairy.vetch,
         CoverCropBiomass) %>% 
  mutate(CoverCropBiomass2=
           buckwheat+sun.hemp+sorghum.sudangrass+
           canola+cereal.rye+hairy.vetch,
         CoverCropBiomass2=round(CoverCropBiomass2,2))
WeedDataTmp[which((round(WeedDataTmp$CoverCropBiomass,2) == WeedDataTmp$CoverCropBiomass2) == FALSE),]
```

    ##  [1] Trial              Site               Year               Block             
    ##  [5] Plot               CoverCrop          buckwheat          sun.hemp          
    ##  [9] sorghum.sudangrass canola             cereal.rye         hairy.vetch       
    ## [13] CoverCropBiomass   CoverCropBiomass2 
    ## <0 rows> (or 0-length row.names)

``` r
WeedData <- WeedData %>% 
  select(-c(buckwheat,sun.hemp,sorghum.sudangrass,
         canola,cereal.rye,hairy.vetch))

# Species for phylo----

# "_weed" removes the cover crop species
# Data to make phylogentic trees
NameKey<-read.xlsx("../Data/NameKey.xlsx")
SpeciesList<-NameKey[!is.na(NameKey$species),2:6] #five unknown spp. removed
FamilyList<-read.csv("../Data/FamilyList.csv")

# Six cover crops removed to focus only on weed communities
NameKey_weed<-NameKey %>% 
  filter(!CommonName %in% c("buckwheat","sun.hemp","sorghum.sudangrass",
         "canola","cereal.rye","hairy.vetch"))
SpeciesList_weed<-NameKey_weed[!is.na(NameKey_weed$species),2:6]
```

``` r
# Full----
##  Checking if common names match between abundance data and phylo data- looks good
sort(colnames(FullData[9:80]))[which(sort(colnames(FullData[9:74])) %in% sort(NameKey$CommonName) == FALSE)]
```

    ## character(0)

``` r
##  joining volunteer buckwheat, volunteer c. rye, and volunteer canola to their species to allow for the pivots
### Inspecting the plots where the volunteers occur to confirm that they are not also the CC treatment - good
FullData %>% 
  select("buckwheat","volunteer.buckwheat",
         "canola", "volunteer.canola",
         "cereal.rye", "volunteer.cereal.rye") %>% 
  filter(volunteer.buckwheat | volunteer.canola | volunteer.cereal.rye > 0)
```

    ##   buckwheat volunteer.buckwheat canola volunteer.canola cereal.rye
    ## 1         0                0.00      0             0.00       0.00
    ## 2         0                0.00      0             0.00       0.00
    ## 3         0                0.57      0             0.00       0.00
    ## 4         0                0.00      0             4.01       0.00
    ## 5         0                0.00      0             0.11       0.00
    ## 6         0                0.00      0             0.07     614.15
    ## 7         0                0.00      0             2.14     413.44
    ##   volunteer.cereal.rye
    ## 1                 3.33
    ## 2                34.30
    ## 3                 0.00
    ## 4                 0.00
    ## 5                 0.00
    ## 6                 0.00
    ## 7                 0.00

``` r
FullDataTmp.NoVol<-FullData %>% 
  mutate(buckwheat = buckwheat + volunteer.buckwheat,
         canola = canola + volunteer.canola,
         cereal.rye = cereal.rye + volunteer.cereal.rye,
         volunteer.buckwheat = NULL,
         volunteer.canola = NULL,
         volunteer.cereal.rye = NULL)

##  Pivoting the abundance data for the join
FullDataTmp<-pivot_longer(FullDataTmp.NoVol,
             cols = -c(Experiment,Trial,Site,Year,Block,Plot,
                       CoverCrop,CoverCropBiomass),
             names_to = "Species",
             values_to = "Biomass_g0.5m2")

##  logical test of pivot_long made a dataframe of the correct size- it looks good!
nrow(FullDataTmp) == ncol(select(FullDataTmp.NoVol,-c(Experiment,Trial,Site,Year,Block,Plot,CoverCrop,CoverCropBiomass)))*160
```

    ## [1] TRUE

``` r
##  join and remove unknowns
FullDataTmp2 <- left_join(FullDataTmp,NameKey,by = c("Species" = "CommonName"))
FullDataTmp3 <- FullDataTmp2 %>% 
  select(-c(Species,genus,family,species.relative,genus.relative)) %>% 
  filter(!species %in% NA) %>% 
  mutate(species=str_replace(species," ","_"))

FullDataTmp4<-pivot_wider(FullDataTmp3,names_from = "species",values_from = "Biomass_g0.5m2")

#Confirming pivot_wider
##Values - confirmed
rowSums(FullDataTmp4[,9:72]) %in%
rowSums(FullDataTmp.NoVol %>% 
  select(-c(starts_with("unknown") | ends_with("spp.") | ends_with("spp"))) %>% 
  select(-c(Experiment,Trial,Site,Year,Block,Plot,CoverCrop,CoverCropBiomass)))
```

    ##   [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ##  [16] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ##  [31] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ##  [46] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ##  [61] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ##  [76] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ##  [91] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [106] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [121] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [136] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [151] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE

``` r
#Creation of abundance data with Latin names
FullSp<-FullDataTmp4 %>% 
  mutate(rowname=paste(Experiment,Trial,Site,Year,Block,Plot,CoverCrop,sep = "_")) %>% 
  select(-c(Experiment,Trial,Site,Year,Block,Plot,CoverCrop,CoverCropBiomass)) %>% 
  column_to_rownames(var = "rowname")

#Removed missing sample
FullSpClean<-FullSp[-c(which(rownames(FullSp)== "Winter cover crops_COOP2_Farm Hub_2021_3_14_Tilled")),]

FullSpClean_Wint<-FullSpClean[rownames(FullSpClean) %like% "Winter cover crops",]
FullSpClean_Wint<-FullSpClean_Wint[,which(colSums(FullSpClean_Wint)>0)]

FullSpClean_Sum<-FullSpClean[rownames(FullSpClean) %like% "Summer cover crops",]
FullSpClean_Sum<-FullSpClean_Sum[,which(colSums(FullSpClean_Sum)>0)]

# _Weed----
#Checking if common names match between abundance data and phylo data- looks good
sort(colnames(WeedData[9:74]))[which(sort(colnames(WeedData[9:74])) %in% sort(NameKey_weed$CommonName) == FALSE)]
```

    ## character(0)

``` r
#Pivoting the abundance data for the join
WeedDataTmp<-pivot_longer(WeedData,
             cols = -c(Experiment,Trial,Site,Year,Block,Plot,CoverCrop,CoverCropBiomass),
             names_to = "Species",
             values_to = "Biomass_g0.5m2")

#logical test of pivot_long made a dataframe of the correct size- it looks good!
nrow(WeedDataTmp) == ncol(select(WeedData,-c(Experiment,Trial,Site,Year,Block,Plot,CoverCrop,CoverCropBiomass)))*160
```

    ## [1] TRUE

``` r
#join and remove unknowns
WeedDataTmp2 <- left_join(WeedDataTmp,NameKey_weed,by = c("Species" = "CommonName"))
WeedDataTmp3 <- WeedDataTmp2 %>% 
  select(-c(Species,genus,family,species.relative,genus.relative)) %>% 
  filter(!species %in% NA) %>% 
  mutate(species=str_replace(species," ","_"))

WeedDataTmp4<-pivot_wider(WeedDataTmp3,names_from = "species",values_from = "Biomass_g0.5m2")

#Confirming pivot_wider
##Values - confirmed
rowSums(WeedDataTmp4[,9:69]) %in%
rowSums(WeedData %>% 
  select(-c(starts_with("unknown") | ends_with("spp.") | ends_with("spp"))) %>% 
  select(-c(Experiment,Trial,Site,Year,Block,Plot,CoverCrop,CoverCropBiomass)))
```

    ##   [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ##  [16] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ##  [31] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ##  [46] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ##  [61] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ##  [76] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ##  [91] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [106] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [121] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [136] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [151] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE

``` r
#Names compaired to phylo tree - confirmed
##tree_boundDicot$tip.label %in% colnames(WeedDataTmp4[,9:69])

#Creation of abundance data with Latin names
WeedSp<-WeedDataTmp4 %>% 
  mutate(rowname=paste(Experiment,Trial,Site,Year,Block,Plot,CoverCrop,sep = "_")) %>% 
  select(-c(Experiment,Trial,Site,Year,Block,Plot,CoverCrop,CoverCropBiomass)) %>% 
  column_to_rownames(var = "rowname")

#Removed missing sample
WeedSpClean<-WeedSp[-c(which(rownames(WeedSp)== "Winter cover crops_COOP2_Farm Hub_2021_3_14_Tilled")),]

WeedSpClean_Wint<-WeedSpClean[rownames(WeedSpClean) %like% "Winter cover crops",]
WeedSpClean_Wint<-WeedSpClean_Wint[,which(colSums(WeedSpClean_Wint)>0)]

WeedSpClean_Sum<-WeedSpClean[rownames(WeedSpClean) %like% "Summer cover crops",]
WeedSpClean_Sum<-WeedSpClean_Sum[,which(colSums(WeedSpClean_Sum)>0)]
```

``` r
# Creation----
## Full----
Full.relativesBound <- bind.relative(sp.list = SpeciesList,
                                tree = GBOTB.extended.WP,
                                nodes = nodes.info.1.WP)
```

    ## Warning in bind.relative(sp.list = SpeciesList, tree = GBOTB.extended.WP, :
    ## Duplicated species detected and removed.

    ## [1] "Brassica napus"       "Fagopyrum esculentum" "Secale cereale"

``` r
Full.tree_bound <- phylo.maker(sp.list = Full.relativesBound$species.list,
                        tree = Full.relativesBound$phylo,
                        nodes = Full.relativesBound$nodes.info,
                        scenarios="S3")

is.binary(Full.tree_bound$scenario.3)
```

    ## [1] FALSE

``` r
Full.tree_boundDicot<-multi2di(Full.tree_bound$scenario.3)
is.ultrametric(Full.tree_boundDicot)
```

    ## [1] TRUE

``` r
## _weed----
#No binding of relatives
#tree_unbound <- phylo.maker(sp.list = SpeciesList_weed,
#                            tree = GBOTB.extended.WP,
#                            nodes = nodes.info.1.WP, scenarios = "S3")

#ggtree(tree_unbound$scenario.3)+
#  geom_tiplab()+
#  scale_x_continuous(expand = expansion(mult = 0.5))+
#  labs(title="Unbound tree")

#Bond relatives - helps specify distances. I will use
relativesBound <- bind.relative(sp.list = SpeciesList_weed,
                                tree = GBOTB.extended.WP,
                                nodes = nodes.info.1.WP)

tree_bound<-phylo.maker(sp.list = relativesBound$species.list,
                        tree = relativesBound$phylo,
                        nodes = relativesBound$nodes.info, scenarios="S3")

#The tree has a polytom in the Poa, which is randomly solved using multi2di()
is.binary(tree_bound$scenario.3)
```

    ## [1] FALSE

``` r
#Corrected tree is ultrametric, which fulfills models that assume constant variance and equal means at the tip
tree_boundDicot<-multi2di(tree_bound$scenario.3)
is.binary(tree_boundDicot)
```

    ## [1] TRUE

``` r
is.ultrametric(tree_boundDicot)
```

    ## [1] TRUE

``` r
# Visualization----
## Full----
### Example
Full.tree <- phylo.maker(sp.list = SpeciesList,
                 tree = GBOTB.extended.WP,
                 nodes = nodes.info.1.WP, scenarios = "S3")
```

    ## [1] "Duplicated species detected and removed."
    ## [1] "Brassica napus"       "Fagopyrum esculentum" "Secale cereale"

``` r
Full.groupInfoFamily<-split(
  str_replace(SpeciesList$species," ","_"),
  SpeciesList$family)

### Clean
Full.tree_boundDicot<-groupOTU(Full.tree_boundDicot,Full.groupInfoFamily)

ggtree(Full.tree_boundDicot) + 
  geom_tiplab(aes(color = group)) +
  scale_x_continuous(expand = expansion(mult = 0.5)) +
  labs(title="Bound ultrametric tree colored by familes",
       color = "Family")
```

![](phylogenetic-competition_files/figure-gfm/phylo%20trees-1.png)<!-- -->

``` r
### Winter cover crop trial
Full.tree_boundDicot.Wint <- drop.tip(phy = Full.tree_boundDicot,
                                 tip = setdiff(Full.tree_boundDicot$tip.label,
                                               colnames(FullSpClean_Wint)))

Full.tree_boundDicot.Wint<-groupOTU(Full.tree_boundDicot.Wint,Full.groupInfoFamily)

ggtree(Full.tree_boundDicot.Wint, layout = "fan") + 
  geom_tiplab(aes(color = group)) +
  scale_x_continuous(expand = expansion(mult = 0.5)) +
  theme(legend.position = "none")
```

    ## Scale for y is already present.
    ## Adding another scale for y, which will replace the existing scale.

    ## Warning: Unknown or uninitialised column: `subgroup`.

    ## Warning: Unknown or uninitialised column: `subgroup`.
    ## Unknown or uninitialised column: `subgroup`.
    ## Unknown or uninitialised column: `subgroup`.

![](phylogenetic-competition_files/figure-gfm/phylo%20trees-2.png)<!-- -->

``` r
  #labs(title="Winter cover crop trial",
       #color = "Family")

###  Summer cover crop trial
Full.tree_boundDicot.Sum <- drop.tip(phy = Full.tree_boundDicot,
                                 tip = setdiff(Full.tree_boundDicot$tip.label,
                                               colnames(FullSpClean_Sum)))

Full.tree_boundDicot.Sum<-groupOTU(Full.tree_boundDicot.Sum,Full.groupInfoFamily)

ggtree(Full.tree_boundDicot.Sum) + 
  geom_tiplab(aes(color = group)) +
  scale_x_continuous(expand = expansion(mult = 0.5)) +
  labs(title="Summer cover crop trial",
       color = "Family")
```

![](phylogenetic-competition_files/figure-gfm/phylo%20trees-3.png)<!-- -->

``` r
## _weed----
### Both experiments
groupInfoFamily<-split(
  str_replace(SpeciesList_weed$species," ","_"),
  SpeciesList_weed$family)

tree_boundDicot<-groupOTU(tree_boundDicot,groupInfoFamily)

ggtree(tree_boundDicot) + 
  geom_tiplab(aes(color = group)) +
  scale_x_continuous(expand = expansion(mult = 0.5)) +
  labs(title="Bound ultrametric tree colored by familes",
       color = "Family")
```

![](phylogenetic-competition_files/figure-gfm/phylo%20trees-4.png)<!-- -->

``` r
### Winter cover crop trial
tree_boundDicot.Wint <- drop.tip(phy = tree_boundDicot,
                                 tip = setdiff(tree_boundDicot$tip.label,
                                               colnames(WeedSpClean_Wint)))

tree_boundDicot.Wint<-groupOTU(tree_boundDicot.Wint,groupInfoFamily)

ggtree(tree_boundDicot.Wint) + 
  geom_tiplab(aes(color = group)) +
  scale_x_continuous(expand = expansion(mult = 0.5)) +
  labs(title="Winter cover crop trial",
       color = "Family")
```

![](phylogenetic-competition_files/figure-gfm/phylo%20trees-5.png)<!-- -->

``` r
###  Summer cover crop trial
tree_boundDicot.Sum <- drop.tip(phy = tree_boundDicot,
                                tip = setdiff(tree_boundDicot$tip.label,
                                              colnames(WeedSpClean_Sum)))

tree_boundDicot.Sum<-groupOTU(tree_boundDicot.Sum,groupInfoFamily)

ggtree(tree_boundDicot.Sum) + 
  geom_tiplab(aes(color = group)) +
  scale_x_continuous(expand = expansion(mult = 0.5)) +
  labs(title="Summer cover crop trial",
       color = "Family")
```

![](phylogenetic-competition_files/figure-gfm/phylo%20trees-6.png)<!-- -->

## Analysis

### Alpha Diversity

#### Calculation

``` r
# Calculation----
##  Full----
FullDist <- cophenetic(Full.tree_boundDicot)
Full.inter.mpd <- modifiedMPD(FullSpClean, FullDist,
                              abundance.weighted="interspecific")

Full.inter.mpd <- replace_na(Full.inter.mpd,0)
AlphaAnalysisTmp.Full <- data.frame(Trt = rownames(FullSpClean),Full.inter.mpd)

### Adding treatment info
AlphaAnalysis.Full <- AlphaAnalysisTmp.Full %>% 
  separate(col = Trt,
           into = c("Experiment","Trial","Site","Year", "Block","Plot","CoverCrop"),
           sep="_") %>% 
  mutate(SiteYear = paste(Site,Trial,sep = "_"),
         across(.cols = c(Experiment,Trial,Site,Block,Year,CoverCrop,SiteYear),
                .fns = factor))

AlphaAnalysis.Full$CoverCrop <- factor(AlphaAnalysis.Full$CoverCrop,
                                  levels = c("Tilled","Canola","Cereal rye",
                                             "Hairy vetch","HVxCR","Buckwheat",
                                             "Sunn hemp","S. sudangrass", "SSxSH"))
###  Adding cover crop biomass
AlphaAnalysis.Full$Plot<-as.numeric(AlphaAnalysis.Full$Plot)
AlphaAnalysis.Full<-left_join(AlphaAnalysis.Full,FullDataTmp4[,1:8])
```

    ## Joining with `by = join_by(Experiment, Trial, Site, Year, Block, Plot,
    ## CoverCrop)`

``` r
##  Weed----
WeedDist <- cophenetic(tree_boundDicot)
inter.mpd <- modifiedMPD(WeedSpClean, WeedDist, abundance.weighted="interspecific")

###  The NAs are samples with 0 or 1 weed species, this would be a distance of 0
####  Confirmation
WeedSpClean2 <- WeedSpClean
WeedSpClean2[WeedSpClean2>0] <-1
which(rowSums(WeedSpClean2) <=1) %in% which(paste(inter.mpd) == "NA")
```

    ##  [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [16] TRUE TRUE TRUE TRUE

``` r
####  Replace NA with 0
inter.mpd <- replace_na(inter.mpd,0)
AlphaAnalysisTmp <- data.frame(Trt = rownames(WeedSpClean),inter.mpd)

### Adding treatment info
AlphaAnalysis <- AlphaAnalysisTmp %>% 
  separate(col = Trt,
           into = c("Experiment","Trial","Site","Year", "Block","Plot","CoverCrop"),
           sep="_") %>% 
  mutate(SiteYear = paste(Site,Trial,sep = "_"),
         across(.cols = c(Experiment,Trial,Site,Block,Year,CoverCrop,SiteYear),
                .fns = factor))

AlphaAnalysis$CoverCrop <- factor(AlphaAnalysis$CoverCrop,
                                  levels = c("Tilled","Canola","Cereal rye",
                                             "Hairy vetch","HVxCR","Buckwheat",
                                             "Sunn hemp","S. sudangrass", "SSxSH"))
###  Adding cover crop biomass
AlphaAnalysis$Plot<-as.numeric(AlphaAnalysis$Plot)
AlphaAnalysisWeed<-left_join(AlphaAnalysis,WeedDataTmp4[,1:8])
```

    ## Joining with `by = join_by(Experiment, Trial, Site, Year, Block, Plot,
    ## CoverCrop)`

``` r
# Data frame creation----
##  Full----
###  All treatments
AlphaAnalysis.Full <- AlphaAnalysis.Full %>%
  select(Experiment,Trial,SiteYear,Site,Year,Block,Plot,
         CoverCrop,CoverCropBiomass,everything())

AlphaAnalysisWint.Full <- AlphaAnalysis.Full %>% 
  filter(Experiment == "Winter cover crops")
AlphaAnalysisWint.Full <- droplevels(AlphaAnalysisWint.Full)

AlphaAnalysisSum.Full <- AlphaAnalysis.Full %>% 
  filter(Experiment == "Summer cover crops")
AlphaAnalysisSum.Full <- droplevels(AlphaAnalysisSum.Full)
AlphaAnalysisSum.Full$CoverCrop <- factor(AlphaAnalysisSum.Full$CoverCrop,
                                          levels = c("Tilled","Buckwheat","S. sudangrass",
                                                     "Sunn hemp","SSxSH"))

###  Cover crops only
AlphaAnalysisWint_CC.Full <- AlphaAnalysisWint.Full %>% 
  filter(CoverCrop != "Tilled")
AlphaAnalysisWint_CC.Full <- droplevels(AlphaAnalysisWint_CC.Full)

AlphaAnalysisSum_CC.Full <- AlphaAnalysisSum.Full %>% 
  filter(CoverCrop != "Tilled")
AlphaAnalysisSum_CC.Full <- droplevels(AlphaAnalysisSum_CC.Full)

##  Weed----
###  All treatments
AlphaAnalysisWeed <- AlphaAnalysisWeed %>%
  select(Experiment,Trial,SiteYear,Site,Year,Block,Plot,
         CoverCrop,CoverCropBiomass,everything())

AlphaAnalysisWint <- AlphaAnalysisWeed %>% 
  filter(Experiment == "Winter cover crops")
AlphaAnalysisWint <- droplevels(AlphaAnalysisWint)

AlphaAnalysisSum <- AlphaAnalysisWeed %>% 
  filter(Experiment == "Summer cover crops")
AlphaAnalysisSum <- droplevels(AlphaAnalysisSum)
AlphaAnalysisSum$CoverCrop <- factor(AlphaAnalysisSum$CoverCrop,
                                     levels = c("Tilled","Buckwheat",
                                                "S. sudangrass","Sunn hemp","SSxSH"))

###  Cover crops only
AlphaAnalysisWint_CC <- AlphaAnalysisWint %>% 
  filter(CoverCrop != "Tilled")
AlphaAnalysisWint_CC <- droplevels(AlphaAnalysisWint_CC)

AlphaAnalysisSum_CC <- AlphaAnalysisSum %>% 
  filter(CoverCrop != "Tilled")
AlphaAnalysisSum_CC <- droplevels(AlphaAnalysisSum_CC)

##  Combination -only with data that excluded the control----
### Winter
AlphaAnalysisWint_CC.Full$CC.Pres <- "Yes"
AlphaAnalysisWint_CC.Full <- rename(AlphaAnalysisWint_CC.Full,
                                    inter.mpd = Full.inter.mpd)
AlphaAnalysisWint_CC$CC.Pres <- "No"
CC_DirrectionDataWint <- bind_rows(AlphaAnalysisWint_CC.Full,
                                   AlphaAnalysisWint_CC)
CC_DirrectionDataWint$CC.Pres <- as.factor(CC_DirrectionDataWint$CC.Pres)

### Summer

AlphaAnalysisSum_CC.Full$CC.Pres <- "Yes"
AlphaAnalysisSum_CC.Full <- rename(AlphaAnalysisSum_CC.Full,
                                    inter.mpd = Full.inter.mpd)
AlphaAnalysisSum_CC$CC.Pres <- "No"
CC_DirrectionDataSum <- bind_rows(AlphaAnalysisSum_CC.Full,
                                   AlphaAnalysisSum_CC)
CC_DirrectionDataSum$CC.Pres <- as.factor(CC_DirrectionDataSum$CC.Pres)
```

Confirmation that using a regional tree *does not* affect interspecific
MPD. What I did to test was to creat trees from subsets of the data and
an individual sample and did not find that the interspecific MPD was
different.

Some edits to the data frame WeedSpFHClean had to be made in 2025 to get
the chunck to run, just specifying -1. Not sure if this will affect the
test…

#### Modeling

All modeling was done using data separated by winter and summer cash
crop trial because the tilled control represented different weed
communities *and* the cover crop species were different so its imposible
to have a trial\*treatment interaction.

1.  **Comparison** between the control and the treatments (Use all data)

2.  Effect of cover crop **biomass** (control dropped to avoid
    singularity)

3.  Do cover crops have a **directional** effect? (control dropped to
    avoid singularity, null model used)

``` r
#glmer(gaussian(link = "log")) used because it had the best fit and the most logical results

# Winter----
ContMod1_Wint <- lmer(inter.mpd ~ Site*CoverCrop+(1|SiteYear/Block),
             data=AlphaAnalysisWint)
```

    ## boundary (singular) fit: see help('isSingular')

``` r
summary(ContMod1_Wint)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: inter.mpd ~ Site * CoverCrop + (1 | SiteYear/Block)
    ##    Data: AlphaAnalysisWint
    ## 
    ## REML criterion at convergence: 823.8
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -1.9772 -0.6585  0.1112  0.7018  1.5146 
    ## 
    ## Random effects:
    ##  Groups         Name        Variance Std.Dev.
    ##  Block:SiteYear (Intercept)    0      0.00   
    ##  SiteYear       (Intercept) 1254     35.41   
    ##  Residual                   6344     79.65   
    ## Number of obs: 79, groups:  Block:SiteYear, 16; SiteYear, 4
    ## 
    ## Fixed effects:
    ##                                   Estimate Std. Error       df t value Pr(>|t|)
    ## (Intercept)                        234.634     39.188    7.430   5.987 0.000437
    ## SiteMusgrave                       -48.779     54.365    6.912  -0.897 0.399753
    ## CoverCropCanola                     26.424     41.254   67.065   0.641 0.524008
    ## CoverCropCereal rye               -105.176     41.254   67.065  -2.549 0.013080
    ## CoverCropHairy vetch               -82.753     41.254   67.065  -2.006 0.048897
    ## CoverCropHVxCR                     -75.481     41.254   67.065  -1.830 0.071743
    ## SiteMusgrave:CoverCropCanola        -4.313     57.339   67.039  -0.075 0.940271
    ## SiteMusgrave:CoverCropCereal rye    98.326     57.339   67.039   1.715 0.091002
    ## SiteMusgrave:CoverCropHairy vetch   31.685     57.339   67.039   0.553 0.582382
    ## SiteMusgrave:CoverCropHVxCR         12.629     57.339   67.039   0.220 0.826340
    ##                                      
    ## (Intercept)                       ***
    ## SiteMusgrave                         
    ## CoverCropCanola                      
    ## CoverCropCereal rye               *  
    ## CoverCropHairy vetch              *  
    ## CoverCropHVxCR                    .  
    ## SiteMusgrave:CoverCropCanola         
    ## SiteMusgrave:CoverCropCereal rye  .  
    ## SiteMusgrave:CoverCropHairy vetch    
    ## SiteMusgrave:CoverCropHVxCR          
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) StMsgr CvrCrC CvrCCr CvrCHv CCHVCR SM:CCC SM:CCr SM:CCv
    ## SiteMusgrav -0.721                                                        
    ## CoverCrpCnl -0.562  0.405                                                 
    ## CvrCrpCrlry -0.562  0.405  0.534                                          
    ## CvrCrpHryvt -0.562  0.405  0.534  0.534                                   
    ## CvrCrpHVxCR -0.562  0.405  0.534  0.534  0.534                            
    ## StMsgrv:CCC  0.404 -0.546 -0.719 -0.384 -0.384 -0.384                     
    ## StMsgr:CCCr  0.404 -0.546 -0.384 -0.719 -0.384 -0.384  0.518              
    ## StMsgr:CCHv  0.404 -0.546 -0.384 -0.384 -0.719 -0.384  0.518  0.518       
    ## StMs:CCHVCR  0.404 -0.546 -0.384 -0.384 -0.384 -0.719  0.518  0.518  0.518
    ## optimizer (nloptwrap) convergence code: 0 (OK)
    ## boundary (singular) fit: see help('isSingular')

``` r
plot(ContMod1_Wint)
```

![](phylogenetic-competition_files/figure-gfm/compairason-1.png)<!-- -->

``` r
#check_model(ContMod1_Wint)
anova(ContMod1_Wint)
```

    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##                Sum Sq Mean Sq NumDF  DenDF F value   Pr(>F)   
    ## Site             1795  1794.9     1  2.012  0.2829 0.647680   
    ## CoverCrop      116429 29107.3     4 67.023  4.5883 0.002463 **
    ## Site:CoverCrop  27881  6970.2     4 67.023  1.0987 0.364476   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
ContMod2_Wint <- glmmTMB(inter.mpd ~ Site*CoverCrop+(1|SiteYear/Block),
                 family = tweedie(),
                 data = AlphaAnalysisWint)
simulateResiduals(ContMod2_Wint,plot = TRUE) #no indication of improved fit
```

![](phylogenetic-competition_files/figure-gfm/compairason-2.png)<!-- -->

    ## Object of Class DHARMa with simulated residuals based on 250 simulations with refit = FALSE . See ?DHARMa::simulateResiduals for help. 
    ##  
    ## Scaled residual values: 0.592 0.544 0.084 0.86 0.828 0.892 0.92 0.512 0.005266911 0.6 0.628 0.116 0.924 0.28 0.0162995 0.01823786 0.556 0.556 0.904 0.652 ...

``` r
car::Anova(ContMod2_Wint,type = 3)
```

    ## Analysis of Deviance Table (Type III Wald chisquare tests)
    ## 
    ## Response: inter.mpd
    ##                   Chisq Df Pr(>Chisq)    
    ## (Intercept)    706.0353  1    < 2e-16 ***
    ## Site             0.6399  1    0.42375    
    ## CoverCrop        8.8187  4    0.06579 .  
    ## Site:CoverCrop   2.4782  4    0.64854    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
ContMod3_Wint <- update(ContMod2_Wint, family = gaussian(link = "log"), start = list(beta = rep(0,10)))
simulateResiduals(ContMod3_Wint,plot = TRUE) #no indication of improved fit
```

![](phylogenetic-competition_files/figure-gfm/compairason-3.png)<!-- -->

    ## Object of Class DHARMa with simulated residuals based on 250 simulations with refit = FALSE . See ?DHARMa::simulateResiduals for help. 
    ##  
    ## Scaled residual values: 0.608 0.532 0.076 0.924 0.864 0.896 0.932 0.496 0.016 0.576 0.564 0.072 0.94 0.26 0.024 0.024 0.604 0.508 0.94 0.74 ...

``` r
car::Anova(ContMod3_Wint,type = 3) #Will use
```

    ## Analysis of Deviance Table (Type III Wald chisquare tests)
    ## 
    ## Response: inter.mpd
    ##                    Chisq Df Pr(>Chisq)    
    ## (Intercept)    1300.4759  1    < 2e-16 ***
    ## Site              1.1236  1    0.28914    
    ## CoverCrop        16.7623  4    0.00215 ** 
    ## Site:CoverCrop    4.2002  4    0.37958    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Summer----
ContMod1_Sum <- lmer(inter.mpd ~ Site*CoverCrop+(1|SiteYear/Block),
             data=AlphaAnalysisSum)
summary(ContMod1_Sum)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: inter.mpd ~ Site * CoverCrop + (1 | SiteYear/Block)
    ##    Data: AlphaAnalysisSum
    ## 
    ## REML criterion at convergence: 824.2
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.7478 -0.3431  0.1835  0.5175  1.8775 
    ## 
    ## Random effects:
    ##  Groups         Name        Variance Std.Dev.
    ##  Block:SiteYear (Intercept)  417.4   20.43   
    ##  SiteYear       (Intercept) 3836.8   61.94   
    ##  Residual                   4905.3   70.04   
    ## Number of obs: 80, groups:  Block:SiteYear, 16; SiteYear, 4
    ## 
    ## Fixed effects:
    ##                                     Estimate Std. Error      df t value
    ## (Intercept)                          152.227     50.831   3.041   2.995
    ## SiteMusgrave                          38.713     71.885   3.041   0.539
    ## CoverCropBuckwheat                   -47.867     35.019  56.000  -1.367
    ## CoverCropS. sudangrass               -48.960     35.019  56.000  -1.398
    ## CoverCropSunn hemp                   -27.026     35.019  56.000  -0.772
    ## CoverCropSSxSH                       -37.587     35.019  56.000  -1.073
    ## SiteMusgrave:CoverCropBuckwheat       52.958     49.524  56.000   1.069
    ## SiteMusgrave:CoverCropS. sudangrass   53.250     49.524  56.000   1.075
    ## SiteMusgrave:CoverCropSunn hemp       31.568     49.524  56.000   0.637
    ## SiteMusgrave:CoverCropSSxSH           71.270     49.524  56.000   1.439
    ##                                     Pr(>|t|)  
    ## (Intercept)                           0.0569 .
    ## SiteMusgrave                          0.6271  
    ## CoverCropBuckwheat                    0.1771  
    ## CoverCropS. sudangrass                0.1676  
    ## CoverCropSunn hemp                    0.4435  
    ## CoverCropSSxSH                        0.2877  
    ## SiteMusgrave:CoverCropBuckwheat       0.2895  
    ## SiteMusgrave:CoverCropS. sudangrass   0.2869  
    ## SiteMusgrave:CoverCropSunn hemp       0.5264  
    ## SiteMusgrave:CoverCropSSxSH           0.1557  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) StMsgr CvrCrB CvCS.s CvrCSh CCSSSH SM:CCB SM:CCs SM:CCh
    ## SiteMusgrav -0.707                                                        
    ## CvrCrpBckwh -0.344  0.244                                                 
    ## CvrCrpS.sdn -0.344  0.244  0.500                                          
    ## CvrCrpSnnhm -0.344  0.244  0.500  0.500                                   
    ## CvrCrpSSxSH -0.344  0.244  0.500  0.500  0.500                            
    ## StMsgrv:CCB  0.244 -0.344 -0.707 -0.354 -0.354 -0.354                     
    ## StMsg:CCS.s  0.244 -0.344 -0.354 -0.707 -0.354 -0.354  0.500              
    ## StMsgr:CCSh  0.244 -0.344 -0.354 -0.354 -0.707 -0.354  0.500  0.500       
    ## StMs:CCSSSH  0.244 -0.344 -0.354 -0.354 -0.354 -0.707  0.500  0.500  0.500

``` r
plot(ContMod1_Sum)
```

![](phylogenetic-competition_files/figure-gfm/compairason-4.png)<!-- -->

``` r
anova(ContMod1_Sum)
```

    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##                 Sum Sq Mean Sq NumDF DenDF F value Pr(>F)
    ## Site            7597.1  7597.1     1     2  1.5488 0.3394
    ## CoverCrop       7017.0  1754.3     4    56  0.3576 0.8377
    ## Site:CoverCrop 11904.1  2976.0     4    56  0.6067 0.6595

``` r
ContMod2_Sum <- glmmTMB(inter.mpd ~ Site*CoverCrop+(1|SiteYear/Block),
                 family = tweedie(),
                 data = AlphaAnalysisSum)
simulateResiduals(ContMod2_Sum,plot = TRUE) #no indication of improved fit
```

![](phylogenetic-competition_files/figure-gfm/compairason-5.png)<!-- -->

    ## Object of Class DHARMa with simulated residuals based on 250 simulations with refit = FALSE . See ?DHARMa::simulateResiduals for help. 
    ##  
    ## Scaled residual values: 0.428 0.376 0.005397167 0.164 0.176 0.408 0.18 0.796 0.02034513 0.05559735 0.124 0.752 0.372 0.256 0.336 0.08757011 0.08567733 0.192 0.176 0.248 ...

``` r
car::Anova(ContMod2_Sum,type = 3)
```

    ## Analysis of Deviance Table (Type III Wald chisquare tests)
    ## 
    ## Response: inter.mpd
    ##                   Chisq Df Pr(>Chisq)    
    ## (Intercept)    210.9352  1     <2e-16 ***
    ## Site             0.6040  1     0.4371    
    ## CoverCrop        2.7511  4     0.6003    
    ## Site:CoverCrop   2.2865  4     0.6832    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
ContMod3_Sum <- update(ContMod2_Sum, family = gaussian(link = "log"), start = list(beta = rep(0,10)))
simulateResiduals(ContMod3_Sum,plot = TRUE) #no indication of improved fit
```

![](phylogenetic-competition_files/figure-gfm/compairason-6.png)<!-- -->

    ## Object of Class DHARMa with simulated residuals based on 250 simulations with refit = FALSE . See ?DHARMa::simulateResiduals for help. 
    ##  
    ## Scaled residual values: 0.4 0.292 0.064 0.152 0.196 0.356 0.172 0.872 0.072 0.064 0.128 0.66 0.384 0.24 0.332 0.08 0.104 0.184 0.172 0.18 ...

``` r
car::Anova(ContMod3_Sum,type = 3) #Will use
```

    ## Analysis of Deviance Table (Type III Wald chisquare tests)
    ## 
    ## Response: inter.mpd
    ##                   Chisq Df Pr(>Chisq)    
    ## (Intercept)    226.7552  1     <2e-16 ***
    ## Site             0.8649  1     0.3524    
    ## CoverCrop        2.6035  4     0.6262    
    ## Site:CoverCrop   2.5241  4     0.6403    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Viz----
##  Winter
interMPDLettersWint<-cld(emmeans(ContMod3_Wint,~CoverCrop),
                     sort=T,Letters="abcde",adjust="none", reversed= TRUE)
```

    ## NOTE: Results may be misleading due to involvement in interactions

``` r
WintFilteringFig<-
left_join(AlphaAnalysisWint,interMPDLettersWint) %>% 
ggplot(.,aes(CoverCrop,inter.mpd,fill=CoverCrop))+
  geom_blank(aes(y=mean(inter.mpd)*1.5))+
  stat_summary(geom = "bar",fun = "mean")+
  stat_summary(geom = "errorbar",fun.data = "mean_se",width = .1)+
  stat_summary(geom="text",fun = "MeanPlusSe",
               aes(label=trimws(.group)),size=6,vjust = -0.5)+
  scale_fill_viridis(discrete=TRUE,direction = -1,option = "A",
                     begin = .25,end = .9, alpha=.975)+
  scale_x_discrete(labels = c("Tilled","Canola","Cereal rye\n(CR)",
                              "Hairy vetch\n(HV)","CR x HV"))+
  guides(size = "none")+
  labs(x = NULL, y = "Interspecific MPD", fill ="Cover crop")+
  theme_bw(base_size = 16)+
  theme(legend.position = "none")+
  facet_grid(~"Winter cover crops")
```

    ## Joining with `by = join_by(CoverCrop)`

``` r
##  Summer
interMPDLettersSum<-cld(emmeans(ContMod3_Sum,~CoverCrop),
                     sort=T,Letters="abcde",adjust="none", reversed= TRUE)
```

    ## NOTE: Results may be misleading due to involvement in interactions

``` r
SummerFilteringFig<-
left_join(AlphaAnalysisSum,interMPDLettersSum) %>% 
ggplot(.,aes(CoverCrop,inter.mpd,fill=CoverCrop))+
  geom_blank(aes(y=mean(inter.mpd)*1.25))+
  stat_summary(geom = "bar",fun = "mean")+
  stat_summary(geom = "errorbar",fun.data = "mean_se",width = .1)+
  stat_summary(geom="text",fun = "MeanPlusSe",
               aes(label=trimws(.group)),size=6,vjust = -0.5)+
  scale_fill_viridis(discrete=TRUE,direction = -1,option = "A",
                     begin = .25,end = .9, alpha=.975)+
  scale_x_discrete(labels = c("Tilled","Buckwheat","Sorghum\nsudangrass\n(SS)",
                              "Sunn hemp\n(SH)", "SS x SH"))+
  guides(size = "none")+
  labs(x = "Treatment", y = "Interspecific MPD", fill ="Cover crop")+
  theme_bw(base_size = 16)+
  theme(legend.position = "none")+
  facet_grid(~"Summer cover crops")
```

    ## Joining with `by = join_by(CoverCrop)`

``` r
#pdf("Figures (450dpi)/FilteringFig.pdf",width = 6, height = 10)
gridExtra::grid.arrange(WintFilteringFig,SummerFilteringFig)
```

![](phylogenetic-competition_files/figure-gfm/compairason-7.png)<!-- -->

``` r
#dev.off()
```

``` r
#lmer used because it was the only model to fit Stephan Perry from CSCU approved *fit* 11/14/22

# Winter----
BioMod1_Wint <- lmer(inter.mpd ~ Site*CoverCropBiomass*CoverCrop+(1|SiteYear/Block),
             data=AlphaAnalysisWint_CC)
```

    ## boundary (singular) fit: see help('isSingular')

``` r
plot(BioMod1_Wint) #okay fit
```

![](phylogenetic-competition_files/figure-gfm/biomass-1.png)<!-- -->

``` r
anova(BioMod1_Wint,type=3)
```

    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##                                 Sum Sq Mean Sq NumDF  DenDF F value    Pr(>F)
    ## Site                                 4       4     1 39.298  0.0006 0.9803034
    ## CoverCropBiomass                  1124    1124     1 46.209  0.1907 0.6644036
    ## CoverCrop                        95864   31955     3 47.580  5.4183 0.0027396
    ## Site:CoverCropBiomass              928     928     1 46.209  0.1574 0.6933643
    ## Site:CoverCrop                   39511   13170     3 47.580  2.2332 0.0965038
    ## CoverCropBiomass:CoverCrop      121160   40387     3 47.170  6.8480 0.0006346
    ## Site:CoverCropBiomass:CoverCrop  33742   11247     3 47.170  1.9071 0.1412772
    ##                                    
    ## Site                               
    ## CoverCropBiomass                   
    ## CoverCrop                       ** 
    ## Site:CoverCropBiomass              
    ## Site:CoverCrop                  .  
    ## CoverCropBiomass:CoverCrop      ***
    ## Site:CoverCropBiomass:CoverCrop    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
test(emtrends(BioMod1_Wint, pairwise~CoverCrop|CoverCropBiomass,
                       var = "CoverCropBiomass"))$emtrends
```

    ## NOTE: Results may be misleading due to involvement in interactions

    ## CoverCropBiomass = 358:
    ##  CoverCrop   CoverCropBiomass.trend    SE   df t.ratio p.value
    ##  Canola                       0.157 0.183 47.8   0.860  0.3940
    ##  Cereal rye                  -0.242 0.360 47.5  -0.674  0.5038
    ##  Hairy vetch                 -0.888 0.279 43.6  -3.181  0.0027
    ##  HVxCR                        0.746 0.296 47.8   2.522  0.0151
    ## 
    ## Results are averaged over the levels of: Site 
    ## Degrees-of-freedom method: kenward-roger

``` r
BioMod2_Wint <- glmmTMB(inter.mpd ~ Site*CoverCropBiomass*CoverCrop+(1|SiteYear/Block),
                 family = tweedie(),
                 data = AlphaAnalysisWint_CC)
```

    ## Warning in (function (start, objective, gradient = NULL, hessian = NULL, :
    ## NA/NaN function evaluation

``` r
simulateResiduals(BioMod2_Wint,plot = TRUE) #worst fit than lmer
```

![](phylogenetic-competition_files/figure-gfm/biomass-2.png)<!-- -->

    ## Object of Class DHARMa with simulated residuals based on 250 simulations with refit = FALSE . See ?DHARMa::simulateResiduals for help. 
    ##  
    ## Scaled residual values: 0.612 0.164 0.772 0.768 0.968 0.956 0.564 0.01817145 0.576 0.084 0.844 0.612 0.04489534 0.02835806 0.516 0.916 0.528 0.988 0.416 0.908 ...

``` r
car::Anova(BioMod2_Wint,type = 3) #Test terms doesn't fit
```

    ## Analysis of Deviance Table (Type III Wald chisquare tests)
    ## 
    ## Response: inter.mpd
    ##                                 Chisq Df Pr(>Chisq)
    ## (Intercept)                       NaN  1        NaN
    ## Site                              NaN  1        NaN
    ## CoverCropBiomass                  NaN  1        NaN
    ## CoverCrop                         NaN  3        NaN
    ## Site:CoverCropBiomass             NaN  1        NaN
    ## Site:CoverCrop                    NaN  3        NaN
    ## CoverCropBiomass:CoverCrop        NaN  3        NaN
    ## Site:CoverCropBiomass:CoverCrop   NaN  3        NaN

``` r
#BioMod3_Wint <- update(BioMod2_Wint,               #No fit
#                       family = gaussian(link = "log"),
#                       start = 0)


# Summer----
BioMod1_Sum <- lmer(inter.mpd ~ Site*CoverCropBiomass*CoverCrop+(1|SiteYear/Block),
             data=AlphaAnalysisSum_CC)
```

    ## boundary (singular) fit: see help('isSingular')

``` r
plot(BioMod1_Sum) #okay fit
```

![](phylogenetic-competition_files/figure-gfm/biomass-3.png)<!-- -->

``` r
anova(BioMod1_Sum,type=3) #No effect
```

    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##                                 Sum Sq Mean Sq NumDF  DenDF F value  Pr(>F)  
    ## Site                             36369   36369     1  6.268  6.2987 0.04426 *
    ## CoverCropBiomass                   246     246     1 45.899  0.0426 0.83736  
    ## CoverCrop                        16539    5513     3 35.059  0.9548 0.42484  
    ## Site:CoverCropBiomass             4941    4941     1 45.899  0.8557 0.35979  
    ## Site:CoverCrop                   18547    6182     3 35.059  1.0707 0.37404  
    ## CoverCropBiomass:CoverCrop       12822    4274     3 29.722  0.7402 0.53652  
    ## Site:CoverCropBiomass:CoverCrop  11138    3713     3 29.722  0.6430 0.59345  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
test(emtrends(BioMod1_Sum, pairwise~CoverCrop|CoverCropBiomass,
                       var = "CoverCropBiomass"))$emtrends #No effect
```

    ## NOTE: Results may be misleading due to involvement in interactions

    ## CoverCropBiomass = 185:
    ##  CoverCrop     CoverCropBiomass.trend    SE   df t.ratio p.value
    ##  Buckwheat                     0.6966 1.020 47.6   0.683  0.4978
    ##  S. sudangrass                 0.0112 0.274 31.1   0.041  0.9678
    ##  Sunn hemp                    -0.5456 0.454 32.5  -1.201  0.2384
    ##  SSxSH                         0.0709 0.394 40.3   0.180  0.8581
    ## 
    ## Results are averaged over the levels of: Site 
    ## Degrees-of-freedom method: kenward-roger

``` r
BioMod2_Sum <- glmmTMB(inter.mpd ~ Site*CoverCropBiomass*CoverCrop+(1|SiteYear/Block),
                 family = tweedie(),
                 data = AlphaAnalysisSum_CC)
```

    ## Warning in (function (start, objective, gradient = NULL, hessian = NULL, :
    ## NA/NaN function evaluation

``` r
simulateResiduals(BioMod2_Sum,plot = TRUE) #worst fit than lmer
```

![](phylogenetic-competition_files/figure-gfm/biomass-4.png)<!-- -->

    ## Object of Class DHARMa with simulated residuals based on 250 simulations with refit = FALSE . See ?DHARMa::simulateResiduals for help. 
    ##  
    ## Scaled residual values: 0.852 0.628 0.1423955 0.184 0.7 0.412 0.3346785 0.04735687 0.972 0.592 0.204 0.448 0.4299252 0.122646 0.668 0.292 0.736 0.003176124 0.508 0.648 ...

``` r
car::Anova(BioMod2_Sum,type = 3) #Test terms doesn't fit
```

    ## Analysis of Deviance Table (Type III Wald chisquare tests)
    ## 
    ## Response: inter.mpd
    ##                                 Chisq Df Pr(>Chisq)
    ## (Intercept)                       NaN  1        NaN
    ## Site                              NaN  1        NaN
    ## CoverCropBiomass                  NaN  1        NaN
    ## CoverCrop                         NaN  3        NaN
    ## Site:CoverCropBiomass             NaN  1        NaN
    ## Site:CoverCrop                    NaN  3        NaN
    ## CoverCropBiomass:CoverCrop        NaN  3        NaN
    ## Site:CoverCropBiomass:CoverCrop   NaN  3        NaN

``` r
#BioMod3_Sum <- update(BioMod2_Sum,               #No fit
#                       family = gaussian(link = "log"),
#                       start = 0)

# Viz----
##  Winter
### Results don't change with unit corrected model
AlphaAnalysisWint_CCTmp <- AlphaAnalysisWint_CC %>% 
  mutate(CoverCropBiomass = CoverCropBiomass*2) #converted to g/m2

BioMod1_Wintb <- lmer(inter.mpd ~ Site*CoverCropBiomass*CoverCrop+(1|SiteYear/Block),
             data=AlphaAnalysisWint_CCTmp)
```

    ## boundary (singular) fit: see help('isSingular')

``` r
plot(BioMod1_Wintb) #okay fit
```

![](phylogenetic-competition_files/figure-gfm/biomass-5.png)<!-- -->

``` r
anova(BioMod1_Wintb,type=3)
```

    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##                                 Sum Sq Mean Sq NumDF  DenDF F value    Pr(>F)
    ## Site                                 4       4     1 39.298  0.0006 0.9803034
    ## CoverCropBiomass                  1124    1124     1 46.209  0.1907 0.6644036
    ## CoverCrop                        95864   31955     3 47.580  5.4183 0.0027396
    ## Site:CoverCropBiomass              928     928     1 46.209  0.1574 0.6933643
    ## Site:CoverCrop                   39511   13170     3 47.580  2.2332 0.0965038
    ## CoverCropBiomass:CoverCrop      121160   40387     3 47.170  6.8480 0.0006346
    ## Site:CoverCropBiomass:CoverCrop  33742   11247     3 47.170  1.9071 0.1412772
    ##                                    
    ## Site                               
    ## CoverCropBiomass                   
    ## CoverCrop                       ** 
    ## Site:CoverCropBiomass              
    ## Site:CoverCrop                  .  
    ## CoverCropBiomass:CoverCrop      ***
    ## Site:CoverCropBiomass:CoverCrop    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
test(emtrends(BioMod1_Wintb, pairwise~CoverCrop|CoverCropBiomass,
                       var = "CoverCropBiomass"))$emtrends
```

    ## NOTE: Results may be misleading due to involvement in interactions

    ## CoverCropBiomass = 716:
    ##  CoverCrop   CoverCropBiomass.trend     SE   df t.ratio p.value
    ##  Canola                      0.0786 0.0914 47.8   0.860  0.3940
    ##  Cereal rye                 -0.1211 0.1800 47.5  -0.674  0.5038
    ##  Hairy vetch                -0.4439 0.1400 43.6  -3.181  0.0027
    ##  HVxCR                       0.3731 0.1480 47.8   2.522  0.0151
    ## 
    ## Results are averaged over the levels of: Site 
    ## Degrees-of-freedom method: kenward-roger

``` r
WintCovCrop.labs <- c("Canola","Cereal rye (CR)",
                      "Hairy vetch (HV)","CR x HV")
names(WintCovCrop.labs) <- c("Canola", "Cereal rye",
                             "Hairy vetch","HVxCR")

f_labelsWint <- data.frame(CoverCrop = c("Canola", "Cereal rye",
                               "Hairy vetch", "HVxCR"),
                       label = c("italic('P') == 0.40","italic('P') == 0.50",
                                 "italic('P') < 0.01", "italic('P') < 0.05"))

WintRegMPDFig<-
  emmip(BioMod1_Wintb,CoverCrop~CoverCropBiomass,
      at=list(CoverCropBiomass =c(0,300,500,850,1000,1200,1400,1500)),
      type = "response",plotit = FALSE) %>%
    mutate(CoverCrop = tvar) %>% 
    filter(!(CoverCrop == "Canola" & CoverCropBiomass > 1400 |
             CoverCrop == "Cereal rye" & CoverCropBiomass < 500 |
             CoverCrop == "Hairy vetch" & CoverCropBiomass > 850 |
             CoverCrop == "HVxCR" & CoverCropBiomass < 300 |
             CoverCrop == "HVxCR" & CoverCropBiomass > 1200)) %>% 
  ggplot(. , aes(CoverCropBiomass,yvar,color=CoverCrop))+
    geom_line(linewidth = 1.5)+
    geom_point(data = AlphaAnalysisWint_CCTmp,
               aes(CoverCropBiomass, inter.mpd,
                   color=CoverCrop), size = 2.25, alpha = .75)+
    geom_text(x=-Inf,y=Inf,aes(label = label),
              color = "black",size = 4.75, data = f_labelsWint,parse = TRUE,
              hjust = -0.25, vjust = 1.5)+
    scale_x_continuous(labels = scales::comma)+
    scale_y_continuous(expand = expansion(mult = 0.14))+
    scale_color_manual(values = myCol[1:4])+
    theme_bw(base_size = 16)+
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45,hjust = 1))+
    labs(x = NULL,
         y = "Interspecific MPD", color = "Cover crop")+
    facet_grid(Experiment~CoverCrop,scales = "free_x",
               labeller = labeller(CoverCrop= WintCovCrop.labs))
```

    ## NOTE: Results may be misleading due to involvement in interactions

``` r
WintRegMPDFig
```

![](phylogenetic-competition_files/figure-gfm/biomass-6.png)<!-- -->

``` r
## Summer
AlphaAnalysisSum_CCTmp <- AlphaAnalysisSum_CC %>% 
  mutate(CoverCropBiomass = CoverCropBiomass*2)

BioMod1_Sumb <- lmer(inter.mpd ~ Site*CoverCropBiomass*CoverCrop+(1|SiteYear/Block),
             data=AlphaAnalysisSum_CCTmp)
```

    ## boundary (singular) fit: see help('isSingular')

``` r
plot(BioMod1_Sumb) #results don't change
```

![](phylogenetic-competition_files/figure-gfm/biomass-7.png)<!-- -->

``` r
anova(BioMod1_Sumb,type=3) #results don't change
```

    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##                                 Sum Sq Mean Sq NumDF  DenDF F value  Pr(>F)  
    ## Site                             36369   36369     1  6.268  6.2987 0.04426 *
    ## CoverCropBiomass                   246     246     1 45.899  0.0426 0.83736  
    ## CoverCrop                        16539    5513     3 35.059  0.9548 0.42484  
    ## Site:CoverCropBiomass             4941    4941     1 45.899  0.8557 0.35979  
    ## Site:CoverCrop                   18547    6182     3 35.059  1.0707 0.37404  
    ## CoverCropBiomass:CoverCrop       12822    4274     3 29.722  0.7402 0.53652  
    ## Site:CoverCropBiomass:CoverCrop  11138    3713     3 29.722  0.6430 0.59345  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
test(emtrends(BioMod1_Sumb, pairwise~CoverCrop|CoverCropBiomass,
                       var = "CoverCropBiomass"))$emtrends
```

    ## NOTE: Results may be misleading due to involvement in interactions

    ## CoverCropBiomass = 370:
    ##  CoverCrop     CoverCropBiomass.trend    SE   df t.ratio p.value
    ##  Buckwheat                    0.34832 0.510 47.6   0.683  0.4978
    ##  S. sudangrass                0.00558 0.137 31.1   0.041  0.9678
    ##  Sunn hemp                   -0.27279 0.227 32.5  -1.201  0.2384
    ##  SSxSH                        0.03547 0.197 40.3   0.180  0.8581
    ## 
    ## Results are averaged over the levels of: Site 
    ## Degrees-of-freedom method: kenward-roger

``` r
SumCovCrop.labs <- c("Buckwheat","Sorghum sudangrass (SS)",
                      "Sunn hemp (SH)","SS x SH")
names(SumCovCrop.labs) <- c("Buckwheat","S. sudangrass",
                            "Sunn hemp","SSxSH")
SumCovCrop.labs <- factor(SumCovCrop.labs,
                          levels = c("Buckwheat","Sorghum sudangrass (SS)",
                                     "Sunn hemp (SH)","SS x SH"))
f_labelsSum <- data.frame(CoverCrop = c("Buckwheat", "S. sudangrass",
                               "Sunn hemp", "SSxSH"),
                       label = c("italic('P') == 0.50","italic('P') == 0.97",
                                 "italic('P') == 0.24", "italic('P') == 0.86"))

SumRegMPDFig<-
  emmip(BioMod1_Sumb,CoverCrop~CoverCropBiomass,
        at=list(CoverCropBiomass =c(0,50,100,200,400,500,900)),
        type = "response",plotit = FALSE) %>%
    mutate(CoverCrop = tvar) %>% 
    filter(!(CoverCrop == "Buckwheat" & CoverCropBiomass < 50 |
             CoverCrop == "Buckwheat" & CoverCropBiomass > 500 |
             CoverCrop == "Sunn hemp" & CoverCropBiomass < 100 |
             CoverCrop == "Sunn hemp" & CoverCropBiomass > 500 |
             CoverCrop == "S. sudangrass" & CoverCropBiomass < 100 |
             CoverCrop == "SSxSH" & CoverCropBiomass < 200)) %>% 
    ggplot(. , aes(CoverCropBiomass,yvar,color=CoverCrop))+
      geom_line(linewidth = 1.5)+
      geom_point(data = AlphaAnalysisSum_CCTmp,
                 aes(CoverCropBiomass, inter.mpd,
                     color=CoverCrop), size = 2.25, alpha = .75)+
      geom_text(x=-Inf,y=Inf,aes(label = label),
                color = "black",size = 4.75, data = f_labelsSum,parse = TRUE,
                hjust = -0.25, vjust = 1.5)+
      scale_y_continuous(expand = expansion(mult = 0.23))+
      scale_x_continuous(labels = scales::comma)+
      scale_color_manual(values = myCol[5:8])+
      theme_bw(base_size = 16)+
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 45,hjust = 1))+
      labs(x = expression("Cover crop biomass (g"~m^-2*')'),
           y = "Interspecific MPD", color = "Cover crop")+
      facet_grid(Experiment~
                   factor(CoverCrop,
                          levels = c("Buckwheat","S. sudangrass",
                                     "Sunn hemp","SSxSH"),
                          labels = c("Buckwheat","Sorghum sudangrass (SS)",
                                     "Sunn hemp (SH)","SS x SH")),
                 scales = "free_x",
                 labeller = labeller(CoverCrop= SumCovCrop.labs))
```

    ## NOTE: Results may be misleading due to involvement in interactions

``` r
SumRegMPDFig
```

![](phylogenetic-competition_files/figure-gfm/biomass-8.png)<!-- -->

``` r
#pdf("Figures (450dpi)/RegFig.pdf",width = 10.6, height = 7)
ggpubr::ggarrange(WintRegMPDFig,SumRegMPDFig,nrow = 2,align = "hv")
```

![](phylogenetic-competition_files/figure-gfm/biomass-9.png)<!-- -->

``` r
#dev.off()


 # Checks----
## Confirmation that estimates are - looks good
emmip(BioMod1_Wint,CoverCrop~CoverCropBiomass,
      at=list(CoverCropBiomass =c(0,300,500,850,1000,1200,1400,1500)),
      type = "response")+
  facet_wrap(~CoverCrop)
```

    ## NOTE: Results may be misleading due to involvement in interactions

![](phylogenetic-competition_files/figure-gfm/biomass-10.png)<!-- -->

``` r
AlphaAnalysisWint_CCTmp2 <- 
  AlphaAnalysisWint_CC %>% 
  filter(CoverCrop == "Hairy vetch")
AlphaAnalysisWint_CCTmp2 <- droplevels(AlphaAnalysisWint_CCTmp2)

BioMod1_WintTmp2 <- lmer(inter.mpd ~ Site*CoverCropBiomass+(1|SiteYear),
             data=AlphaAnalysisWint_CCTmp2)
emmip(BioMod1_WintTmp2,~CoverCropBiomass,
      at=list(CoverCropBiomass =c(0,300,500,850,1000,1200,1400,1500)),
      type = "response")
```

    ## NOTE: Results may be misleading due to involvement in interactions

![](phylogenetic-competition_files/figure-gfm/biomass-11.png)<!-- -->

Data: CC_DirrectionDataWint and CC_DirrectionDataSum

``` r
#Will use lmer models

# Winter----
DirMod1_Wint <- lmer(inter.mpd ~ Site*CoverCrop*CC.Pres +
                       (1|SiteYear/Block),
             data=CC_DirrectionDataWint)
```

    ## boundary (singular) fit: see help('isSingular')

``` r
plot(DirMod1_Wint) #okay fit
```

![](phylogenetic-competition_files/figure-gfm/dirrection-1.png)<!-- -->

``` r
anova(DirMod1_Wint)
```

    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##                        Sum Sq Mean Sq NumDF DenDF F value    Pr(>F)    
    ## Site                      198     198     1     2  0.0392  0.861414    
    ## CoverCrop               46869   15623     3   110  3.0927  0.029995 *  
    ## CC.Pres                199811  199811     1   110 39.5549 6.629e-09 ***
    ## Site:CoverCrop          23459    7820     3   110  1.5480  0.206203    
    ## Site:CC.Pres             2841    2841     1   110  0.5623  0.454925    
    ## CoverCrop:CC.Pres       65477   21826     3   110  4.3207  0.006398 ** 
    ## Site:CoverCrop:CC.Pres   5872    1957     3   110  0.3875  0.762252    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
DirMod2_Wint <- glmmTMB(inter.mpd ~ Site*CoverCrop*CC.Pres +
                          (1|SiteYear/Block),
                 family = tweedie(),
                 data = CC_DirrectionDataWint)
simulateResiduals(DirMod2_Wint,plot = TRUE) #worst fit than lmer
```

    ## qu = 0.75, log(sigma) = -3.138724 : outer Newton did not converge fully.

![](phylogenetic-competition_files/figure-gfm/dirrection-2.png)<!-- -->

    ## Object of Class DHARMa with simulated residuals based on 250 simulations with refit = FALSE . See ?DHARMa::simulateResiduals for help. 
    ##  
    ## Scaled residual values: 0.564 0.52 0.616 0.556 0.592 0.672 0.58 0.548 0.6 0.516 0.608 0.54 0.52 0.508 0.592 0.612 0.328 0.64 0.54 0.5 ...

``` r
car::Anova(DirMod2_Wint,type = 3) #seems off
```

    ## Analysis of Deviance Table (Type III Wald chisquare tests)
    ## 
    ## Response: inter.mpd
    ##                            Chisq Df Pr(>Chisq)    
    ## (Intercept)            1251.3869  1    < 2e-16 ***
    ## Site                      0.9289  1    0.33515    
    ## CoverCrop                 8.8477  3    0.03139 *  
    ## CC.Pres                   0.1195  1    0.72956    
    ## Site:CoverCrop            2.6726  3    0.44490    
    ## Site:CC.Pres              0.4650  1    0.49528    
    ## CoverCrop:CC.Pres         4.9200  3    0.17775    
    ## Site:CoverCrop:CC.Pres    0.9534  3    0.81253    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
DirMod3_Wint <- update(DirMod2_Wint,               
                      family = gaussian(link = "log"),
                       start = list(beta = rep(0,16)))
simulateResiduals(DirMod3_Wint,plot = TRUE) #worst fit than lmer
```

![](phylogenetic-competition_files/figure-gfm/dirrection-3.png)<!-- -->

    ## Object of Class DHARMa with simulated residuals based on 250 simulations with refit = FALSE . See ?DHARMa::simulateResiduals for help. 
    ##  
    ## Scaled residual values: 0.616 0.532 0.684 0.54 0.544 0.7 0.652 0.48 0.676 0.472 0.648 0.508 0.468 0.524 0.564 0.704 0.256 0.708 0.556 0.488 ...

``` r
car::Anova(DirMod3_Wint,type = 3)
```

    ## Analysis of Deviance Table (Type III Wald chisquare tests)
    ## 
    ## Response: inter.mpd
    ##                            Chisq Df Pr(>Chisq)    
    ## (Intercept)            3505.1251  1  < 2.2e-16 ***
    ## Site                      2.2389  1  0.1345773    
    ## CoverCrop                19.2520  3  0.0002425 ***
    ## CC.Pres                   0.3333  1  0.5637489    
    ## Site:CoverCrop            4.5141  3  0.2110380    
    ## Site:CC.Pres              1.1746  1  0.2784523    
    ## CoverCrop:CC.Pres        11.9161  3  0.0076763 ** 
    ## Site:CoverCrop:CC.Pres    1.8965  3  0.5941658    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Summer----
DirMod1_Sum <- lmer(inter.mpd ~ Site*CoverCrop+CC.Pres +
                       (1|SiteYear/Block),
             data=CC_DirrectionDataSum)
```

    ## boundary (singular) fit: see help('isSingular')

``` r
plot(DirMod1_Sum) #okay fit
```

![](phylogenetic-competition_files/figure-gfm/dirrection-4.png)<!-- -->

``` r
anova(DirMod1_Sum)
```

    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##                Sum Sq Mean Sq NumDF DenDF F value    Pr(>F)    
    ## Site            13196   13196     1     2  2.7001  0.242058    
    ## CoverCrop       69623   23208     3   117  4.7485  0.003672 ** 
    ## CC.Pres        170712  170712     1   117 34.9294 3.447e-08 ***
    ## Site:CoverCrop   9849    3283     3   117  0.6717  0.571048    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
DirMod2_Sum <- glmmTMB(inter.mpd ~ Site*CoverCrop*CC.Pres +
                          (1|SiteYear/Block),
                 family = tweedie(),
                 data = CC_DirrectionDataSum)
simulateResiduals(DirMod2_Sum,plot = TRUE) #worst fit than lmer
```

![](phylogenetic-competition_files/figure-gfm/dirrection-5.png)<!-- -->

    ## Object of Class DHARMa with simulated residuals based on 250 simulations with refit = FALSE . See ?DHARMa::simulateResiduals for help. 
    ##  
    ## Scaled residual values: 0.204 0.632 0.604 0.664 0.644 0.14 0.568 0.612 0.56 0.2 0.588 0.676 0.588 0.708 0.604 0.088 0.484 0.808 0.548 0.5 ...

``` r
car::Anova(DirMod2_Sum,type = 3) #seems off
```

    ## Analysis of Deviance Table (Type III Wald chisquare tests)
    ## 
    ## Response: inter.mpd
    ##                           Chisq Df Pr(>Chisq)    
    ## (Intercept)            354.2473  1  < 2.2e-16 ***
    ## Site                     4.2737  1  0.0387060 *  
    ## CoverCrop                0.5050  3  0.9177952    
    ## CC.Pres                 10.9797  1  0.0009212 ***
    ## Site:CoverCrop           0.4214  3  0.9357821    
    ## Site:CC.Pres             3.0751  1  0.0795013 .  
    ## CoverCrop:CC.Pres        3.2085  3  0.3605874    
    ## Site:CoverCrop:CC.Pres   0.7010  3  0.8729606    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
DirMod3_Sum <- update(DirMod2_Sum,               
                      family = gaussian(link = "log"),
                      start = list(beta = rep(0,16)))
simulateResiduals(DirMod3_Sum,plot = TRUE) #worst fit than lmer
```

![](phylogenetic-competition_files/figure-gfm/dirrection-6.png)<!-- -->

    ## Object of Class DHARMa with simulated residuals based on 250 simulations with refit = FALSE . See ?DHARMa::simulateResiduals for help. 
    ##  
    ## Scaled residual values: 0.16 0.604 0.568 0.72 0.72 0.124 0.596 0.584 0.624 0.144 0.556 0.704 0.552 0.724 0.572 0.092 0.54 0.88 0.572 0.388 ...

``` r
car::Anova(DirMod3_Sum,type = 3)
```

    ## Analysis of Deviance Table (Type III Wald chisquare tests)
    ## 
    ## Response: inter.mpd
    ##                           Chisq Df Pr(>Chisq)    
    ## (Intercept)            425.9185  1  < 2.2e-16 ***
    ## Site                     6.1055  1  0.0134758 *  
    ## CoverCrop                0.5845  3  0.8999617    
    ## CC.Pres                 13.8856  1  0.0001943 ***
    ## Site:CoverCrop           0.6070  3  0.8948369    
    ## Site:CC.Pres             4.6971  1  0.0302133 *  
    ## CoverCrop:CC.Pres        3.6956  3  0.2962599    
    ## Site:CoverCrop:CC.Pres   0.9760  3  0.8070661    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Viz----
##  Winter
pairs(emmeans(DirMod1_Wint,~CC.Pres|CoverCrop))
```

    ## NOTE: Results may be misleading due to involvement in interactions

    ## CoverCrop = Canola:
    ##  contrast estimate   SE df t.ratio p.value
    ##  No - Yes     -6.6 25.1 98  -0.263  0.7933
    ## 
    ## CoverCrop = Cereal rye:
    ##  contrast estimate   SE df t.ratio p.value
    ##  No - Yes    -96.5 25.1 98  -3.840  0.0002
    ## 
    ## CoverCrop = Hairy vetch:
    ##  contrast estimate   SE df t.ratio p.value
    ##  No - Yes    -82.8 25.1 98  -3.294  0.0014
    ## 
    ## CoverCrop = HVxCR:
    ##  contrast estimate   SE df t.ratio p.value
    ##  No - Yes   -130.2 25.1 98  -5.182  <.0001
    ## 
    ## Results are averaged over the levels of: Site 
    ## Degrees-of-freedom method: kenward-roger

``` r
cld(emmeans(DirMod1_Wint,~CC.Pres|CoverCrop),
    Letters="abcde",adjust="none", reversed= TRUE)
```

    ## NOTE: Results may be misleading due to involvement in interactions

    ## CoverCrop = Canola:
    ##  CC.Pres emmean   SE   df lower.CL upper.CL .group
    ##  Yes        241 20.5 15.7    197.5      285  a    
    ##  No         235 20.5 15.7    190.9      278  a    
    ## 
    ## CoverCrop = Cereal rye:
    ##  CC.Pres emmean   SE   df lower.CL upper.CL .group
    ##  Yes        251 20.5 15.7    207.1      294  a    
    ##  No         154 20.5 15.7    110.6      198   b   
    ## 
    ## CoverCrop = Hairy vetch:
    ##  CC.Pres emmean   SE   df lower.CL upper.CL .group
    ##  Yes        226 20.5 15.7    182.5      270  a    
    ##  No         143 20.5 15.7     99.7      187   b   
    ## 
    ## CoverCrop = HVxCR:
    ##  CC.Pres emmean   SE   df lower.CL upper.CL .group
    ##  Yes        271 20.5 15.7    227.7      315  a    
    ##  No         141 20.5 15.7     97.5      185   b   
    ## 
    ## Results are averaged over the levels of: Site 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## significance level used: alpha = 0.05 
    ## NOTE: If two or more means share the same grouping symbol,
    ##       then we cannot show them to be different.
    ##       But we also did not show them to be the same.

``` r
DirrectionFigWint<-
  emmip(DirMod1_Wint,CoverCrop~CC.Pres, CIs = TRUE,
      dotarg = list(size=4),
      linearg = list(linewidth = 1.25,alpha = 0.75,linetype = "solid"),
      CIarg = list(lwd = 2, alpha = 0.55))+
  aes(shape = CoverCrop)+
  labs(y = "Interspecific MPD",
       x = NULL,
       color = "Cover crop", shape = "Cover crop")+
  scale_color_viridis(discrete=TRUE,direction = -1,
                      option = "A",begin = .15,end = .85)+
  theme_bw(base_size = 16)+
  facet_grid(~"Winter cover crops")
```

    ## NOTE: Results may be misleading due to involvement in interactions

    ## Warning: Duplicated aesthetics after name standardisation: linewidth

``` r
##  Summer
pairs(emmeans(DirMod1_Sum,~CC.Pres|CoverCrop))
```

    ## CoverCrop = Buckwheat:
    ##  contrast estimate   SE  df t.ratio p.value
    ##  No - Yes      -73 12.4 105  -5.910  <.0001
    ## 
    ## CoverCrop = S. sudangrass:
    ##  contrast estimate   SE  df t.ratio p.value
    ##  No - Yes      -73 12.4 105  -5.910  <.0001
    ## 
    ## CoverCrop = Sunn hemp:
    ##  contrast estimate   SE  df t.ratio p.value
    ##  No - Yes      -73 12.4 105  -5.910  <.0001
    ## 
    ## CoverCrop = SSxSH:
    ##  contrast estimate   SE  df t.ratio p.value
    ##  No - Yes      -73 12.4 105  -5.910  <.0001
    ## 
    ## Results are averaged over the levels of: Site 
    ## Degrees-of-freedom method: kenward-roger

``` r
cld(emmeans(DirMod1_Sum,~CC.Pres|CoverCrop),
    Letters="abcde",adjust="none", reversed= TRUE)
```

    ## CoverCrop = Buckwheat:
    ##  CC.Pres emmean   SE   df lower.CL upper.CL .group
    ##  Yes        241 21.6 4.39      183      299  a    
    ##  No         168 21.6 4.39      110      226   b   
    ## 
    ## CoverCrop = S. sudangrass:
    ##  CC.Pres emmean   SE   df lower.CL upper.CL .group
    ##  Yes        190 21.6 4.39      132      248  a    
    ##  No         117 21.6 4.39       59      175   b   
    ## 
    ## CoverCrop = Sunn hemp:
    ##  CC.Pres emmean   SE   df lower.CL upper.CL .group
    ##  Yes        244 21.6 4.39      186      302  a    
    ##  No         171 21.6 4.39      113      229   b   
    ## 
    ## CoverCrop = SSxSH:
    ##  CC.Pres emmean   SE   df lower.CL upper.CL .group
    ##  Yes        246 21.6 4.39      188      304  a    
    ##  No         173 21.6 4.39      115      231   b   
    ## 
    ## Results are averaged over the levels of: Site 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## significance level used: alpha = 0.05 
    ## NOTE: If two or more means share the same grouping symbol,
    ##       then we cannot show them to be different.
    ##       But we also did not show them to be the same.

``` r
DirrectionFigSum <- 
emmip(DirMod1_Sum,CoverCrop~CC.Pres, CIs = TRUE,
      dotarg = list(size=4),
      linearg = list(linewidth = 1.25,alpha = 0.75,linetype = "solid"),
      CIarg = list(lwd = 2, alpha = 0.55))+
  aes(shape = CoverCrop)+
  labs(y = "Interspecific MPD",
       x = "Calculated with cover crop?",
       color = "Cover crop", shape = "Cover crop")+
  scale_color_viridis(discrete=TRUE,direction = -1,
                      option = "A",begin = .15,end = .85)+
  theme_bw(base_size = 16)+
  facet_grid(~"Summer cover crops")
```

    ## Warning: Duplicated aesthetics after name standardisation: linewidth

``` r
#pdf("Figures (450dpi)/DirrectionOLDFig.pdf",width = 6.66, height = 6.64)
ggpubr::ggarrange(DirrectionFigWint,DirrectionFigSum,align = "hv",nrow = 2)
```

![](phylogenetic-competition_files/figure-gfm/dirrection-7.png)<!-- -->

``` r
#dev.off()
```

I need to prove that cover crop spp. were not causing dispersion by
virtue of their species being different than the resident weed
community, rather competitive exclusion of photogenically related weeds.

FullSpClean is in latin names and has cleaned the missing plot (n =
159). It includes also includes cover crops as a speices in the matrix.

``` r
# Prep----
##  Data frame of tilled control for inter.MPD calculation
TilledData <-
  WeedSpClean %>% 
    rownames_to_column() %>% 
    filter(str_detect(rowname,"_Tilled")) %>% 
    column_to_rownames(var = "rowname")

##  Data frame of tilled control for merging with cover crop biomass
TilledDataWide <-
  WeedSpClean %>% 
    rownames_to_column() %>% 
    filter(str_detect(rowname,"_Tilled")) %>% 
    separate(rowname,
             into = c("Experiment","Trial","Site",
                      "Year","Block","Plot","CoverCrop"),
             sep = "_")

##  Data frame of weeds of the tilled control with the biomass of the cover crops in their corresponding site/year/block 
CC_BiomassData <-
  FullSpClean %>% 
  select("Brassica_napus","Crotalaria_juncea","Fagopyrum_esculentum",
         "Secale_cereale","Sorghum_X_drummondii","Vicia_villosa") %>% 
  rownames_to_column() %>% 
  separate(rowname,
             into = c("Experiment","Trial","Site",
                      "Year","Block","Plot","CoverCrop"),
             sep = "_")

TilledDataPlusTmp <- left_join(TilledDataWide,CC_BiomassData,
          by = c("Experiment","Trial","Site",
                 "Year","Block")) #n = 155 because of missing tilled plot 

##  Git rid of duplicate cover crop and weed spp. 
##  Kept cover crop treatment ID, remember the weeds are those of the tilled treatments!
TilledDataPlus<-TilledDataPlusTmp %>% 
  mutate(Brassica_napus = Brassica_napus.x + Brassica_napus.y,
         Fagopyrum_esculentum = Fagopyrum_esculentum.x + Fagopyrum_esculentum.y,
         Secale_cereale = Secale_cereale.x + Secale_cereale.y) %>% 
  rename(Plot_Tilled = Plot.x,
         CoverCrop = CoverCrop.y) %>% 
  select(-c(Brassica_napus.x,Brassica_napus.y,
            Fagopyrum_esculentum.x,Fagopyrum_esculentum.y,
            Secale_cereale.x,Secale_cereale.y,
            CoverCrop.x,Plot.y)) %>% 
  mutate(TreatmentID = paste(Experiment,Trial,Site,
                      Year,Block,Plot_Tilled,CoverCrop,sep = "_"),
         .keep = "unused") %>% 
  column_to_rownames(var = "TreatmentID")

# inter.MPD calculation----
##  Weed tree used for TilledData
WeedDist <- cophenetic(tree_boundDicot) #no change from above
Weed.inter.mpd_Tilled <- modifiedMPD(TilledData, WeedDist,
                         abundance.weighted="interspecific")
Weed.inter.mpd_Tilled <- replace_na(Weed.inter.mpd_Tilled,0)
Weed.AlphaAnalysis_TilledTmp <- data.frame(Trt = rownames(TilledData),
                               Weed.inter.mpd_Tilled)

Weed.AlphaAnalysis_Tilled <- Weed.AlphaAnalysis_TilledTmp %>% 
  separate(col = Trt,
           into = c("Experiment","Trial","Site","Year", "Block","Plot","CoverCrop"),
           sep="_") %>% 
  mutate(SiteYear = paste(Site,Trial,sep = "_"),
         CoverCrop = NULL,
         across(.cols = c(Experiment,Trial,Site,Block,Year,SiteYear),
                .fns = factor))

##  Full tree used for TilledDataPlus
FullDist <- cophenetic(Full.tree_boundDicot) #no change from above
Full.inter.mpd_Tilled <- modifiedMPD(TilledDataPlus, FullDist,
                              abundance.weighted="interspecific")
Full.inter.mpd_Tilled <- replace_na(Full.inter.mpd_Tilled,0)
Full.AlphaAnalysis_TilledTmp <- data.frame(Trt = rownames(TilledDataPlus),
                                           Full.inter.mpd_Tilled)

Full.AlphaAnalysis_Tilled <- Full.AlphaAnalysis_TilledTmp %>% 
  separate(col = Trt,
           into = c("Experiment","Trial","Site","Year", "Block","Plot","CoverCrop"),
           sep="_") %>% 
  mutate(SiteYear = paste(Site,Trial,sep = "_"),
         across(.cols = c(Experiment,Trial,Site,Block,Year,CoverCrop,SiteYear),
                .fns = factor))

##  Combination of dataframes
### n = 155, very good
ProofDataTmp<-
  left_join(Full.AlphaAnalysis_Tilled,Weed.AlphaAnalysis_Tilled, 
          by = c("Experiment","Trial","Site","Year","Block","Plot","SiteYear"))

ProofData <-
  ProofDataTmp %>% 
    pivot_longer(cols = c(Weed.inter.mpd_Tilled,Full.inter.mpd_Tilled),
                 names_to = "CC.Pres", values_to = "inter.mpd") %>% 
    mutate(CC.Pres = recode(CC.Pres, Weed.inter.mpd_Tilled = "No",
                            Full.inter.mpd_Tilled = "Yes"),
           across(.cols = c(CC.Pres),.fns = factor)) %>% 
    filter(CoverCrop != "Tilled")

ProofData_Wint <- ProofData %>% filter(Experiment == "Winter cover crops")
ProofData_Wint <- droplevels(ProofData_Wint)

ProofData_Sum <- ProofData %>% filter(Experiment == "Summer cover crops")
ProofData_Sum <- droplevels(ProofData_Sum)
ProofData_Sum$CoverCrop <- factor(ProofData_Sum$CoverCrop,
                                  levels = c("Buckwheat","S. sudangrass","Sunn hemp","SSxSH"))

# Modeling----
##  Winter----
ProofModWint_lmer <- lmer(inter.mpd ~ Site*CoverCrop*CC.Pres + 
                        (1|SiteYear/Block),
                      data=ProofData_Wint)
plot(ProofModWint_lmer) #okay fit
```

![](phylogenetic-competition_files/figure-gfm/Proof-1.png)<!-- -->

``` r
anova(ProofModWint_lmer)
```

    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##                        Sum Sq Mean Sq NumDF  DenDF F value    Pr(>F)    
    ## Site                      204     204     1  2.027  0.2745  0.651976    
    ## CoverCrop               20986    6995     3 91.000  9.4247 1.739e-05 ***
    ## CC.Pres                 15112   15112     1 91.000 20.3602 1.912e-05 ***
    ## Site:CoverCrop           9707    3236     3 91.000  4.3592  0.006467 ** 
    ## Site:CC.Pres            50108   50108     1 91.000 67.5093 1.363e-12 ***
    ## CoverCrop:CC.Pres       20986    6995     3 91.000  9.4247 1.739e-05 ***
    ## Site:CoverCrop:CC.Pres   9707    3236     3 91.000  4.3592  0.006467 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
ProofModWint_log <- glmmTMB(inter.mpd ~ Site*CoverCrop*CC.Pres + 
                          (1|SiteYear/Block),
                        family = gaussian(link = "log"),
                        data=ProofData_Wint)
simulateResiduals(ProofModWint_log,plot = TRUE) #worst
```

![](phylogenetic-competition_files/figure-gfm/Proof-2.png)<!-- -->

    ## Object of Class DHARMa with simulated residuals based on 250 simulations with refit = FALSE . See ?DHARMa::simulateResiduals for help. 
    ##  
    ## Scaled residual values: 0.752 0.088 0.732 0.532 0.76 0.868 0.696 0.536 0.512 0.62 0.52 0.696 0.464 0.756 0.536 0.488 0.708 0.476 0.744 0.632 ...

``` r
ProofModWint_Twe <- update(ProofModWint_log,family = tweedie(link = "log"))
simulateResiduals(ProofModWint_Twe,plot = TRUE) #worst
```

![](phylogenetic-competition_files/figure-gfm/Proof-3.png)<!-- -->

    ## Object of Class DHARMa with simulated residuals based on 250 simulations with refit = FALSE . See ?DHARMa::simulateResiduals for help. 
    ##  
    ## Scaled residual values: 0.708 0.032 0.712 0.556 0.712 0.856 0.696 0.52 0.488 0.6 0.636 0.804 0.576 0.824 0.604 0.484 0.732 0.46 0.752 0.576 ...

``` r
#ProofModWint_Gamma <- update(ProofModWint_log,family = Gamma())
#simulateResiduals(ProofModWint_Gamma,plot = TRUE) #worst

##  Summer----
ProofModSum_lmer <- lmer(inter.mpd ~ Site*CoverCrop*CC.Pres + 
                        (1|SiteYear/Block),
                      data=ProofData_Sum)
plot(ProofModSum_lmer) #okay fit
```

![](phylogenetic-competition_files/figure-gfm/Proof-4.png)<!-- -->

``` r
anova(ProofModSum_lmer)
```

    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##                        Sum Sq Mean Sq NumDF DenDF F value    Pr(>F)    
    ## Site                     2877    2877     1     2  1.5399   0.34045    
    ## CoverCrop               20163    6721     3    98  3.5978   0.01625 *  
    ## CC.Pres                 67804   67804     1    98 36.2962 2.973e-08 ***
    ## Site:CoverCrop          10738    3579     3    98  1.9160   0.13198    
    ## Site:CC.Pres              683     683     1    98  0.3658   0.54671    
    ## CoverCrop:CC.Pres       20163    6721     3    98  3.5978   0.01625 *  
    ## Site:CoverCrop:CC.Pres  10738    3579     3    98  1.9160   0.13198    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
ProofModSum_log <- glmmTMB(inter.mpd ~ Site*CoverCrop*CC.Pres + 
                          (1|SiteYear/Block),
                        family = gaussian(link = "log"),
                        data=ProofData_Sum)
simulateResiduals(ProofModSum_log,plot = TRUE) #worst
```

![](phylogenetic-competition_files/figure-gfm/Proof-5.png)<!-- -->

    ## Object of Class DHARMa with simulated residuals based on 250 simulations with refit = FALSE . See ?DHARMa::simulateResiduals for help. 
    ##  
    ## Scaled residual values: 0.06 0.068 0.056 0.6 0.068 0.444 0.04 0.58 0.9 0.868 0.88 0.744 0.864 0.756 0.888 0.78 0.024 0.424 0.024 0.08 ...

``` r
ProofModSum_Twe <- update(ProofModSum_log,family = tweedie(link = "log"))
simulateResiduals(ProofModSum_Twe,plot = TRUE) #worst
```

![](phylogenetic-competition_files/figure-gfm/Proof-6.png)<!-- -->

    ## Object of Class DHARMa with simulated residuals based on 250 simulations with refit = FALSE . See ?DHARMa::simulateResiduals for help. 
    ##  
    ## Scaled residual values: 0.032 0.08 0.036 0.52 0.04 0.388 0.04 0.52 0.888 0.772 0.844 0.724 0.88 0.62 0.84 0.616 0.004 0.408 0.012 0.044 ...

``` r
#ProofModSum_Gamma <- update(ProofModSum_log,family = Gamma())
#simulateResiduals(ProofModSum_Gamma,plot = TRUE) #worst

# Viz----
##  Winter----
pairs(emmeans(ProofModWint_lmer,~CC.Pres|CoverCrop))
```

    ## NOTE: Results may be misleading due to involvement in interactions

    ## CoverCrop = Canola:
    ##  contrast estimate   SE df t.ratio p.value
    ##  No - Yes     21.9 9.97 91   2.201  0.0303
    ## 
    ## CoverCrop = Cereal rye:
    ##  contrast estimate   SE df t.ratio p.value
    ##  No - Yes    -32.6 9.97 91  -3.271  0.0015
    ## 
    ## CoverCrop = Hairy vetch:
    ##  contrast estimate   SE df t.ratio p.value
    ##  No - Yes    -31.1 9.97 91  -3.124  0.0024
    ## 
    ## CoverCrop = HVxCR:
    ##  contrast estimate   SE df t.ratio p.value
    ##  No - Yes    -48.2 9.97 91  -4.830  <.0001
    ## 
    ## Results are averaged over the levels of: Site 
    ## Degrees-of-freedom method: kenward-roger

``` r
cld(emmeans(ProofModWint_lmer,~CC.Pres|CoverCrop),
    Letters="abcde",adjust="none", reversed= TRUE)
```

    ## NOTE: Results may be misleading due to involvement in interactions

    ## CoverCrop = Canola:
    ##  CC.Pres emmean   SE   df lower.CL upper.CL .group
    ##  No         211 10.7 5.14      183      238  a    
    ##  Yes        189 10.7 5.14      161      216   b   
    ## 
    ## CoverCrop = Cereal rye:
    ##  CC.Pres emmean   SE   df lower.CL upper.CL .group
    ##  Yes        243 10.7 5.14      216      271  a    
    ##  No         211 10.7 5.14      183      238   b   
    ## 
    ## CoverCrop = Hairy vetch:
    ##  CC.Pres emmean   SE   df lower.CL upper.CL .group
    ##  Yes        242 10.7 5.14      215      269  a    
    ##  No         211 10.7 5.14      183      238   b   
    ## 
    ## CoverCrop = HVxCR:
    ##  CC.Pres emmean   SE   df lower.CL upper.CL .group
    ##  Yes        259 10.7 5.14      232      286  a    
    ##  No         211 10.7 5.14      183      238   b   
    ## 
    ## Results are averaged over the levels of: Site 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## significance level used: alpha = 0.05 
    ## NOTE: If two or more means share the same grouping symbol,
    ##       then we cannot show them to be different.
    ##       But we also did not show them to be the same.

``` r
ProofFigWint<-
  emmip(ProofModWint_lmer,CoverCrop~CC.Pres, CIs = TRUE,
      dotarg = list(size=4),
      linearg = list(size = 1.25,alpha = 0.75,linetype = "solid"),
      CIarg = list(lwd = 2, alpha = 0.55))+
  aes(shape = CoverCrop)+
  labs(y = "Interspecific MPD\nof control weed community",
       x = NULL,
       color = "Cover crop", shape = "Cover crop")+
  scale_color_viridis(discrete=TRUE,direction = -1,
                      option = "A",begin = .15,end = .85)+
  theme_bw(base_size = 16)+
  facet_grid(~"Winter cover crops")
```

    ## NOTE: Results may be misleading due to involvement in interactions

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `linewidth` instead.
    ## ℹ The deprecated feature was likely used in the emmeans package.
    ##   Please report the issue at <https://github.com/rvlenth/emmeans/issues>.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## Warning: Duplicated aesthetics after name standardisation: linewidth

``` r
##  Summer----
pairs(emmeans(ProofModSum_lmer,~CC.Pres|CoverCrop))
```

    ## NOTE: Results may be misleading due to involvement in interactions

    ## CoverCrop = Buckwheat:
    ##  contrast estimate   SE df t.ratio p.value
    ##  No - Yes   -59.58 15.3 98  -3.899  0.0002
    ## 
    ## CoverCrop = S. sudangrass:
    ##  contrast estimate   SE df t.ratio p.value
    ##  No - Yes    -4.51 15.3 98  -0.295  0.7687
    ## 
    ## CoverCrop = Sunn hemp:
    ##  contrast estimate   SE df t.ratio p.value
    ##  No - Yes   -70.53 15.3 98  -4.616  <.0001
    ## 
    ## CoverCrop = SSxSH:
    ##  contrast estimate   SE df t.ratio p.value
    ##  No - Yes   -49.51 15.3 98  -3.240  0.0016
    ## 
    ## Results are averaged over the levels of: Site 
    ## Degrees-of-freedom method: kenward-roger

``` r
cld(emmeans(ProofModSum_lmer,~CC.Pres|CoverCrop),
    Letters="abcde",adjust="none", reversed= TRUE)
```

    ## NOTE: Results may be misleading due to involvement in interactions

    ## CoverCrop = Buckwheat:
    ##  CC.Pres emmean   SE   df lower.CL upper.CL .group
    ##  Yes        231 20.2 3.56      172      290  a    
    ##  No         172 20.2 3.56      113      230   b   
    ## 
    ## CoverCrop = S. sudangrass:
    ##  CC.Pres emmean   SE   df lower.CL upper.CL .group
    ##  Yes        176 20.2 3.56      117      235  a    
    ##  No         172 20.2 3.56      113      230  a    
    ## 
    ## CoverCrop = Sunn hemp:
    ##  CC.Pres emmean   SE   df lower.CL upper.CL .group
    ##  Yes        242 20.2 3.56      183      301  a    
    ##  No         172 20.2 3.56      113      230   b   
    ## 
    ## CoverCrop = SSxSH:
    ##  CC.Pres emmean   SE   df lower.CL upper.CL .group
    ##  Yes        221 20.2 3.56      162      280  a    
    ##  No         172 20.2 3.56      113      230   b   
    ## 
    ## Results are averaged over the levels of: Site 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## significance level used: alpha = 0.05 
    ## NOTE: If two or more means share the same grouping symbol,
    ##       then we cannot show them to be different.
    ##       But we also did not show them to be the same.

``` r
ProofFigSum<-
  emmip(ProofModSum_lmer,CoverCrop~CC.Pres, CIs = TRUE,
      dotarg = list(size=4),
      linearg = list(size = 1.25,alpha = 0.75,linetype = "solid"),
      CIarg = list(lwd = 2, alpha = 0.55))+
  aes(shape = CoverCrop)+
  labs(y = "Interspecific MPD\nof control weed community",
       x = "Calculated with cover crop?",
       color = "Cover crop", shape = "Cover crop")+
  scale_color_viridis(discrete=TRUE,direction = -1,
                      option = "A",begin = .15,end = .85)+
  theme_bw(base_size = 16)+
  facet_grid(~"Sumer cover crops")
```

    ## NOTE: Results may be misleading due to involvement in interactions

    ## Warning: Duplicated aesthetics after name standardisation: linewidth

``` r
  #theme(strip.text.x = element_blank())

ggpubr::ggarrange(ProofFigWint, ProofFigSum,align = "hv",nrow = 2)
```

![](phylogenetic-competition_files/figure-gfm/Proof-7.png)<!-- -->

But are cover crops further away from their weed communties than those
of the tilled control?

``` r
# Prep----
##  Winter----
### Distance of cover crop to their weed communities
DirrectionData.Cov_Wint <-
  CC_DirrectionDataWint  %>% 
    pivot_wider(names_from = CC.Pres,
                values_from = inter.mpd) %>% 
    mutate(inter.mpdDiff_CovDiff = Yes - No) %>% 
    rename("Yes_Cov" = "Yes", "No_Cov" = "No")

### Distance of cover crop to the weed communities of the tilled control
DirrectionData.Tilled_Wint <-
  ProofData_Wint %>% 
    pivot_wider(names_from = CC.Pres,
                values_from = inter.mpd) %>% 
    mutate(inter.mpdDiff_TilledDiff = Yes - No) %>% 
    rename("Yes_Till" = "Yes", "No_Till" = "No") 
DirrectionData.Tilled_Wint$Plot<-as.numeric(DirrectionData.Tilled_Wint$Plot)

sort(names(DirrectionData.Cov_Wint))
```

    ##  [1] "Block"                 "CoverCrop"             "CoverCropBiomass"     
    ##  [4] "Experiment"            "inter.mpdDiff_CovDiff" "No_Cov"               
    ##  [7] "Plot"                  "Site"                  "SiteYear"             
    ## [10] "Trial"                 "Year"                  "Yes_Cov"

``` r
sort(names(DirrectionData.Tilled_Wint))
```

    ##  [1] "Block"                    "CoverCrop"               
    ##  [3] "Experiment"               "inter.mpdDiff_TilledDiff"
    ##  [5] "No_Till"                  "Plot"                    
    ##  [7] "Site"                     "SiteYear"                
    ##  [9] "Trial"                    "Year"                    
    ## [11] "Yes_Till"

``` r
### Removed block that was missing the tilled control with left_join
DiffData_WintTmp<-left_join(DirrectionData.Tilled_Wint,DirrectionData.Cov_Wint,
                           by = c("Experiment", "Trial", "Site",
                                  "Year", "Block", "CoverCrop", "SiteYear"))

DiffData_Wint <- DiffData_WintTmp %>% 
  select("Experiment", "Trial", "Site",
         "Year", "Block", "CoverCrop", "SiteYear",
         "inter.mpdDiff_TilledDiff","inter.mpdDiff_CovDiff") %>%
  mutate(inter.mpdDiff = inter.mpdDiff_CovDiff - inter.mpdDiff_TilledDiff)

##  Summer----
### Distance of cover crop to their weed communities
DirrectionData.Cov_Sum <-
  CC_DirrectionDataSum  %>% 
    pivot_wider(names_from = CC.Pres,
                values_from = inter.mpd) %>% 
    mutate(inter.mpdDiff_CovDiff = Yes - No) %>% 
    rename("Yes_Cov" = "Yes", "No_Cov" = "No")

### Distance of cover crop to the weed communities of the tilled control
DirrectionData.Tilled_Sum <-
  ProofData_Sum %>% 
    pivot_wider(names_from = CC.Pres,
                values_from = inter.mpd) %>% 
    mutate(inter.mpdDiff_TilledDiff = Yes - No) %>% 
    rename("Yes_Till" = "Yes", "No_Till" = "No") 
DirrectionData.Tilled_Sum$Plot<-as.numeric(DirrectionData.Tilled_Sum$Plot)

sort(names(DirrectionData.Cov_Sum))
```

    ##  [1] "Block"                 "CoverCrop"             "CoverCropBiomass"     
    ##  [4] "Experiment"            "inter.mpdDiff_CovDiff" "No_Cov"               
    ##  [7] "Plot"                  "Site"                  "SiteYear"             
    ## [10] "Trial"                 "Year"                  "Yes_Cov"

``` r
sort(names(DirrectionData.Tilled_Sum))
```

    ##  [1] "Block"                    "CoverCrop"               
    ##  [3] "Experiment"               "inter.mpdDiff_TilledDiff"
    ##  [5] "No_Till"                  "Plot"                    
    ##  [7] "Site"                     "SiteYear"                
    ##  [9] "Trial"                    "Year"                    
    ## [11] "Yes_Till"

``` r
### Removed block that was missing the tilled control with left_join
DiffData_SumTmp<-left_join(DirrectionData.Tilled_Sum,DirrectionData.Cov_Sum,
                           by = c("Experiment", "Trial", "Site",
                                  "Year", "Block", "CoverCrop", "SiteYear"))

DiffData_Sum <- DiffData_SumTmp %>% 
  select("Experiment", "Trial", "Site",
         "Year", "Block", "CoverCrop", "SiteYear",
         "inter.mpdDiff_TilledDiff","inter.mpdDiff_CovDiff") %>%
  mutate(inter.mpdDiff = inter.mpdDiff_CovDiff - inter.mpdDiff_TilledDiff)

# Modeling----
DiffModWint_lmer <- lmer(inter.mpdDiff ~ Site*CoverCrop + 
                        (1|SiteYear/Block),
                      data=DiffData_Wint)
```

    ## boundary (singular) fit: see help('isSingular')

``` r
plot(DiffModWint_lmer) #okay fit
```

![](phylogenetic-competition_files/figure-gfm/Redemption-1.png)<!-- -->

``` r
anova(DiffModWint_lmer)
```

    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##                Sum Sq Mean Sq NumDF  DenDF F value Pr(>F)
    ## Site            48984   48984     1  1.748  5.3255 0.1653
    ## CoverCrop       21086    7029     3 49.773  0.7641 0.5196
    ## Site:CoverCrop  27294    9098     3 49.773  0.9891 0.4056

``` r
test(emmeans(DiffModWint_lmer, "CoverCrop")) #The non-sig values will change a little but trends are always the same
```

    ## NOTE: Results may be misleading due to involvement in interactions

    ##  CoverCrop   emmean   SE   df t.ratio p.value
    ##  Canola        27.3 25.1 20.1   1.087  0.2897
    ##  Cereal rye    70.0 25.1 20.1   2.787  0.0113
    ##  Hairy vetch   47.5 25.1 20.1   1.891  0.0731
    ##  HVxCR         74.1 25.1 20.1   2.948  0.0079
    ## 
    ## Results are averaged over the levels of: Site 
    ## Degrees-of-freedom method: kenward-roger

``` r
#convergence issues
#glmmTMB(inter.mpdDiff ~ Site*CoverCrop + (1|SiteYear/Block),
      #family = gaussian(link = "log"), data=DiffData_Wint, start = -200)

DiffModSum_lmer <- lmer(inter.mpdDiff ~ Site*CoverCrop + 
                        (1|SiteYear/Block),
                      data=DiffData_Sum)
summary(DiffModSum_lmer)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: inter.mpdDiff ~ Site * CoverCrop + (1 | SiteYear/Block)
    ##    Data: DiffData_Sum
    ## 
    ## REML criterion at convergence: 680.7
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -1.3334 -0.5672 -0.1472  0.4828  3.2630 
    ## 
    ## Random effects:
    ##  Groups         Name        Variance Std.Dev.
    ##  Block:SiteYear (Intercept) 1864.4   43.18   
    ##  SiteYear       (Intercept)  546.9   23.39   
    ##  Residual                   6753.8   82.18   
    ## Number of obs: 64, groups:  Block:SiteYear, 16; SiteYear, 4
    ## 
    ## Fixed effects:
    ##                                     Estimate Std. Error      df t value
    ## (Intercept)                           84.000     36.752   6.833   2.286
    ## SiteMusgrave                         -68.440     51.975   6.833  -1.317
    ## CoverCropS. sudangrass               -35.281     41.091  42.000  -0.859
    ## CoverCropSunn hemp                   -38.639     41.091  42.000  -0.940
    ## CoverCropSSxSH                        -4.974     41.091  42.000  -0.121
    ## SiteMusgrave:CoverCropS. sudangrass  -20.725     58.111  42.000  -0.357
    ## SiteMusgrave:CoverCropSunn hemp       26.686     58.111  42.000   0.459
    ## SiteMusgrave:CoverCropSSxSH          -30.342     58.111  42.000  -0.522
    ##                                     Pr(>|t|)  
    ## (Intercept)                           0.0571 .
    ## SiteMusgrave                          0.2304  
    ## CoverCropS. sudangrass                0.3954  
    ## CoverCropSunn hemp                    0.3524  
    ## CoverCropSSxSH                        0.9042  
    ## SiteMusgrave:CoverCropS. sudangrass   0.7231  
    ## SiteMusgrave:CoverCropSunn hemp       0.6484  
    ## SiteMusgrave:CoverCropSSxSH           0.6043  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) StMsgr CvCS.s CvrCSh CCSSSH SM:CCs SM:CCh
    ## SiteMusgrav -0.707                                          
    ## CvrCrpS.sdn -0.559  0.395                                   
    ## CvrCrpSnnhm -0.559  0.395  0.500                            
    ## CvrCrpSSxSH -0.559  0.395  0.500  0.500                     
    ## StMsg:CCS.s  0.395 -0.559 -0.707 -0.354 -0.354              
    ## StMsgr:CCSh  0.395 -0.559 -0.354 -0.707 -0.354  0.500       
    ## StMs:CCSSSH  0.395 -0.559 -0.354 -0.354 -0.707  0.500  0.500

``` r
plot(DiffModSum_lmer) #okay fit
```

![](phylogenetic-competition_files/figure-gfm/Redemption-2.png)<!-- -->

``` r
anova(DiffModSum_lmer)
```

    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##                 Sum Sq Mean Sq NumDF DenDF F value Pr(>F)
    ## Site           26145.0 26145.0     1     2  3.8712 0.1880
    ## CoverCrop      16879.3  5626.4     3    42  0.8331 0.4833
    ## Site:CoverCrop  7654.8  2551.6     3    42  0.3778 0.7695

``` r
test(emmeans(DiffModSum_lmer, "CoverCrop"))
```

    ## NOTE: Results may be misleading due to involvement in interactions

    ##  CoverCrop     emmean SE   df t.ratio p.value
    ##  Buckwheat      49.78 26 6.83   1.916  0.0980
    ##  S. sudangrass   4.14 26 6.83   0.159  0.8782
    ##  Sunn hemp      24.48 26 6.83   0.942  0.3782
    ##  SSxSH          29.63 26 6.83   1.140  0.2925
    ## 
    ## Results are averaged over the levels of: Site 
    ## Degrees-of-freedom method: kenward-roger

``` r
#convergence issues
#glmmTMB(inter.mpdDiff ~ Site*CoverCrop + (1|SiteYear/Block),
#      family = gaussian(link = "log"), data=DiffData_Sum, start = -300)

# Viz----
DiffFigWint <-
  plot(emmeans(DiffModWint_lmer, "CoverCrop"),
     horizontal = FALSE,plotit = FALSE) %>% 
  ggplot(data=.,aes(CoverCrop,the.emmean,color=CoverCrop))+
    geom_hline(yintercept=0, linetype="dashed", 
                color = "black", size=1.5)+
    geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                  color="black",linewidth = 1.5,width = .65)+
    #geom_linerange(aes(ymin = lower.CL, ymax = upper.CL),
    #               linewidth = 4,alpha = .45)+
    geom_point(color = "darkred", size = 4.75)+ #orginally size = 3, color = "black"
    geom_text(aes(label = c("italic('P') == 0.29","italic('P') < 0.05",
                            "italic('P') == 0.07","italic('P') < 0.01"),
                  y = upper.CL+20),
              color = "black",size = 5,parse = TRUE)+
    scale_color_manual(values = myCol[1:4])+
    scale_x_discrete(labels = c("Canola","Cereal rye\n(CR)",
                              "Hairy vetch\n(HV)","CR x HV"))+
    scale_y_continuous(expand = expansion(mult = 0.1))+
    labs(y="Relativized\nInterspecific MPD",
         x = "Cover crop")+
    theme_bw(base_size = 16)+
    theme(legend.position = "none")+
    facet_grid(~"Winter cover crops")
```

    ## NOTE: Results may be misleading due to involvement in interactions

``` r
DiffFigSum <-
  plot(emmeans(DiffModSum_lmer, "CoverCrop"),
     horizontal = FALSE,plotit = FALSE) %>% 
  ggplot(data=.,aes(CoverCrop,the.emmean,color=CoverCrop))+
    geom_hline(yintercept=0, linetype="dashed",
               color = "black", size=1.5)+
    geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                  color="black",linewidth = 1.5, width = .65)+
    #geom_linerange(aes(ymin = lower.CL, ymax = upper.CL),
    #               linewidth = 4,alpha = .45)+
    geom_point(color = "darkred", size = 4.75)+ #orginally size = 3, color = "black"
    geom_text(aes(label = c("italic('P') == 0.10","italic('P') == 0.88",
                            "italic('P') == 0.38","italic('P') == 0.29"),
                  y = upper.CL+20),
              color = "black",size = 5,parse = TRUE)+
    scale_color_manual(values = myCol[5:8])+
    scale_x_discrete(labels = c("Buckwheat","Sorghum\nsudangrass\n(SS)",
                                "Sunn hemp\n(SH)", "SS x SH"))+
    scale_y_continuous(expand = expansion(mult = 0.1))+
    labs(y="Relativized\nInterspecific MPD",
         x = "Cover crop")+
    theme_bw(base_size = 16)+
    theme(legend.position = "none")+
    facet_grid(~"Summer cover crops")
```

    ## NOTE: Results may be misleading due to involvement in interactions

``` r
#pdf("Figures (450dpi)/Dirrection/DirrectionNEWFig_talk.pdf",width = 5.60, height = 6.60)
ggpubr::ggarrange(DiffFigWint,NULL,DiffFigSum,
                  nrow = 3,align = "hv",heights = c(1,-.15,1))
```

![](phylogenetic-competition_files/figure-gfm/Redemption-3.png)<!-- -->

``` r
#dev.off()
```

### Suppression

``` r
# Tweedie(link = log) was used

# Data frame creation----
SupressionData <- WeedData[,1:8]
SupressionData$SiteYear = as.factor(paste(WeedData$Site,WeedData$Trial,sep = "_"))
SupressionData$WeedBiomass <- rowSums(WeedData[,-c(1:8)])
SupressionData<-SupressionData[-c(which(SupressionData$Experiment == "Winter cover crops" &
                                        SupressionData$Trial == "COOP2" &
                                        SupressionData$Site == "Farm Hub" &
                                        SupressionData$Plot == "14")),]

SupressionDataWint<-SupressionData %>%
  filter(Experiment == "Winter cover crops")
SupressionDataWint <- droplevels(SupressionDataWint)

SupressionDataSum<-SupressionData %>%
  filter(Experiment == "Summer cover crops")
SupressionDataSum <- droplevels(SupressionDataSum)
SupressionDataSum$CoverCrop <- factor(SupressionDataSum$CoverCrop,
                                      levels = c("Tilled","Buckwheat","S. sudangrass",
                                                 "Sunn hemp", "SSxSH"))

# Winter----
SupMod1Wint <- lmer(WeedBiomass ~ Site*CoverCrop + (1|SiteYear/Block),
                  data = SupressionDataWint)
```

    ## boundary (singular) fit: see help('isSingular')

``` r
plot(SupMod1Wint)
```

![](phylogenetic-competition_files/figure-gfm/comparison-1.png)<!-- -->

``` r
anova(SupMod1Wint)
```

    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##                Sum Sq Mean Sq NumDF DenDF F value    Pr(>F)    
    ## Site            12575 12574.5     1    69 13.0867  0.000562 ***
    ## CoverCrop      114563 28640.7     4    69 29.8073 2.102e-14 ***
    ## Site:CoverCrop  12663  3165.7     4    69  3.2946  0.015712 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
SupMod2Wint <- glmmTMB(WeedBiomass ~ Site*CoverCrop + (1|SiteYear/Block),
                       family = tweedie(),data = SupressionDataWint) #Convergence issues
simulateResiduals(SupMod2Wint,plot = TRUE) #Perfect
```

![](phylogenetic-competition_files/figure-gfm/comparison-2.png)<!-- -->

    ## Object of Class DHARMa with simulated residuals based on 250 simulations with refit = FALSE . See ?DHARMa::simulateResiduals for help. 
    ##  
    ## Scaled residual values: 0.94 0.176 0.568 0.228 0.772 0.62 1 0.076 0.952 0.68 0.156 0.592 0.124 0.18 0.04024942 0.028 0.68 0.156 0.272 0.72 ...

``` r
car::Anova(SupMod2Wint, type = 3)
```

    ## Analysis of Deviance Table (Type III Wald chisquare tests)
    ## 
    ## Response: WeedBiomass
    ##                   Chisq Df Pr(>Chisq)    
    ## (Intercept)    277.3740  1  < 2.2e-16 ***
    ## Site             1.8563  1      0.173    
    ## CoverCrop       78.5165  4  3.591e-16 ***
    ## Site:CoverCrop  26.9507  4  2.034e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#write.xlsx(car::Anova(SupMod2Wint, type = 3),rowNames = TRUE,"~/downloads/SupMod2Wint.xlsx")

SupMod3Wint <- update(SupMod2Wint,family = gaussian(link = "log"), start = list(beta = rep(0,10)))
simulateResiduals(SupMod3Wint,plot = TRUE) #bad fit
```

![](phylogenetic-competition_files/figure-gfm/comparison-3.png)<!-- -->

    ## Object of Class DHARMa with simulated residuals based on 250 simulations with refit = FALSE . See ?DHARMa::simulateResiduals for help. 
    ##  
    ## Scaled residual values: 1 0.396 0.684 0.332 0.568 0.724 0.996 0.384 0.616 0.836 0.44 0.52 0.348 0.132 0.504 0.04 0.676 0.408 0.368 0.832 ...

``` r
## Summer----
SupMod1Sum <- lmer(WeedBiomass ~ Site*CoverCrop + (1|SiteYear/Block),
                  data = SupressionDataSum)
```

    ## boundary (singular) fit: see help('isSingular')

``` r
plot(SupMod1Sum)
```

![](phylogenetic-competition_files/figure-gfm/comparison-4.png)<!-- -->

``` r
anova(SupMod1Sum)
```

    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##                Sum Sq Mean Sq NumDF DenDF F value    Pr(>F)    
    ## Site             1180  1180.0     1     2  1.7534    0.3165    
    ## CoverCrop      120326 30081.6     4    68 44.6992 < 2.2e-16 ***
    ## Site:CoverCrop  22957  5739.3     4    68  8.5282 1.226e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
SupMod2Sum <- glmmTMB(WeedBiomass ~ Site*CoverCrop + (1|SiteYear/Block),
                       family = tweedie(),data = SupressionDataSum)
simulateResiduals(SupMod2Sum,plot = TRUE)
```

![](phylogenetic-competition_files/figure-gfm/comparison-5.png)<!-- -->

    ## Object of Class DHARMa with simulated residuals based on 250 simulations with refit = FALSE . See ?DHARMa::simulateResiduals for help. 
    ##  
    ## Scaled residual values: 0.96 0.084 0.908 0.208 0.476 0.148 0.48 0.32 0.612 0.012 0.6 0.904 0.84 0.056 0.184 0.656 0.06 0.06 0.516 0.692 ...

``` r
car::Anova(SupMod2Sum, type = 3)
```

    ## Analysis of Deviance Table (Type III Wald chisquare tests)
    ## 
    ## Response: WeedBiomass
    ##                   Chisq Df Pr(>Chisq)    
    ## (Intercept)    316.2145  1  < 2.2e-16 ***
    ## Site             4.6864  1   0.030402 *  
    ## CoverCrop      117.1984  4  < 2.2e-16 ***
    ## Site:CoverCrop  14.6986  4   0.005369 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#write.xlsx(car::Anova(SupMod2Sum, type = 3),rowNames = TRUE,"~/downloads/SupMod2Sum.xlsx")

SupMod3Sum <- update(SupMod2Sum,family = gaussian(link = "log"))
simulateResiduals(SupMod3Sum,plot = TRUE) #not better
```

    ## qu = 0.25, log(sigma) = -2.719162 : outer Newton did not converge fully.

    ## qu = 0.25, log(sigma) = -3.023676 : outer Newton did not converge fully.

![](phylogenetic-competition_files/figure-gfm/comparison-6.png)<!-- -->

    ## Object of Class DHARMa with simulated residuals based on 250 simulations with refit = FALSE . See ?DHARMa::simulateResiduals for help. 
    ##  
    ## Scaled residual values: 0.6 0.044 0.612 0.2 0.396 0.208 0.464 0.252 0.528 0.036 0.568 0.644 0.56 0.044 0.236 0.532 0.152 0.044 0.444 0.54 ...

``` r
car::Anova(SupMod2Sum, type = 3) #seems off
```

    ## Analysis of Deviance Table (Type III Wald chisquare tests)
    ## 
    ## Response: WeedBiomass
    ##                   Chisq Df Pr(>Chisq)    
    ## (Intercept)    316.2145  1  < 2.2e-16 ***
    ## Site             4.6864  1   0.030402 *  
    ## CoverCrop      117.1984  4  < 2.2e-16 ***
    ## Site:CoverCrop  14.6986  4   0.005369 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
## Viz----
### Winter 1100 x 400
SupLettersWint<-cld(emmeans(SupMod2Wint,~CoverCrop|Site),
                     sort=T,Letters="abcde",adjust="none", reversed= TRUE)
WinterSuppressionFig<-
  left_join(SupressionDataWint,SupLettersWint) %>% 
    ggplot(.,aes(CoverCrop,WeedBiomass*20,fill=CoverCrop))+
      geom_blank(aes(y=mean(WeedBiomass*20)*6.5))+
      stat_summary(geom = "bar",fun = "mean")+
      stat_summary(geom = "errorbar",fun.data = "mean_se",width = .1)+
      stat_summary(geom="text",fun = "MeanPlusSe",
                   aes(label=trimws(.group)),size=7,vjust = -0.5)+
      scale_fill_viridis(discrete = TRUE,option = "A",begin = .25,end = .9,
                         alpha=.975,direction = -1,
                         labels = c("Tilled","Canola","Cereal rye (CR)",
                                    "Hairy vetch (HV)","HV x CR"))+
      scale_x_discrete(labels = c("Tilled","Canola","Cereal\nrye\n(CR)",
                                "Hairy\nvetch\n(HV)","CR x HV"))+
      scale_y_continuous(labels = scales::comma)+
      theme_bw(base_size = 16)+
      theme(legend.position = "none")+
      guides(size = "none")+
      labs(x = NULL,
           y = expression("Weed biomass (kg"~ha^-1*')'),
          fill ="Treatment")+
      facet_grid(Experiment~Site)
```

    ## Joining with `by = join_by(Site, CoverCrop)`

``` r
### Summer 1250 x 400
SupLettersSum<-cld(emmeans(SupMod2Sum,~CoverCrop|Site),
                     sort=T,Letters="abcde",adjust="none", reversed= TRUE)
SummerSuppressionFig<-
  left_join(SupressionDataSum,SupLettersSum) %>% 
    ggplot(.,aes(CoverCrop,WeedBiomass*20,fill=CoverCrop))+
      geom_blank(aes(y=mean(WeedBiomass*20)*5.75))+
      stat_summary(geom = "bar",fun = "mean")+
      stat_summary(geom = "errorbar",fun.data = "mean_se",width = .1)+
      stat_summary(geom="text",fun = "MeanPlusSe",
                   aes(label=trimws(.group)),size=7,vjust = -0.5)+
      scale_fill_viridis(discrete=TRUE,direction = -1,
                         option = "A",begin = .25,end = .9)+
      scale_x_discrete(labels=c("Tilled","Buckwheat", "Sorghum\nsudangrass\n(SS)",
                                "Sunn\nhemp\n(SH)","SS x SH"))+
      scale_y_continuous(labels = scales::comma)+
      theme_bw(base_size = 16)+
      theme(legend.position = "none")+
      guides(size = "none")+
      labs(x = "Treatment",
           y = expression("Weed biomass (kg"~ha^-1*')'),
           fill ="Treatment")+
      facet_grid(Experiment~Site)+
      theme(strip.text.x = element_blank())
```

    ## Joining with `by = join_by(Site, CoverCrop)`

``` r
#pdf("Figures (450dpi)/WeedSuppression/SuppressionFig.pdf",width = 11, height = 10)
ggpubr::ggarrange(WinterSuppressionFig,NULL,SummerSuppressionFig,
                  nrow = 3,align = "hv",
                  heights = c(1,-0.05,1))
```

![](phylogenetic-competition_files/figure-gfm/comparison-7.png)<!-- -->

``` r
#dev.off()

### Table
SuppressionTabelDataTmp <- rbind(
  left_join(SupressionDataWint,SupLettersWint),
  left_join(SupressionDataSum,SupLettersSum))
```

    ## Joining with `by = join_by(Site, CoverCrop)`

    ## Joining with `by = join_by(Site, CoverCrop)`

``` r
SuppressionTabelData <- 
  SuppressionTabelDataTmp %>% 
    mutate(WeedBiomass = WeedBiomass*20) %>% 
    group_by(Experiment,Site,CoverCrop) %>% 
    summarise(meanWeed = scales::comma(round(mean(WeedBiomass,na.rm = TRUE),0)),
              seWeed = round(plotrix::std.error(WeedBiomass,na.rm = TRUE),0),
              letter = trimws(nth(.group,1)))
```

    ## `summarise()` has grouped output by 'Experiment', 'Site'. You can override
    ## using the `.groups` argument.

``` r
#write.xlsx(SuppressionTabelData,"Figures (450dpi)/SuppressionTable.xlsx")

SuppressionTabelDataTmp %>% 
    mutate(WeedBiomass = WeedBiomass*20) %>% 
    group_by(Experiment,CoverCrop) %>% 
    summarise(meanWeed = scales::comma(round(mean(WeedBiomass,na.rm = TRUE),0)),
              sdWeed = round(sd(WeedBiomass,na.rm = TRUE),0))
```

    ## `summarise()` has grouped output by 'Experiment'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 10 × 4
    ## # Groups:   Experiment [2]
    ##    Experiment         CoverCrop     meanWeed sdWeed
    ##    <fct>              <fct>         <chr>     <dbl>
    ##  1 Winter cover crops Tilled        2,053      1377
    ##  2 Winter cover crops Canola        172         189
    ##  3 Winter cover crops Cereal rye    66          236
    ##  4 Winter cover crops Hairy vetch   561         752
    ##  5 Winter cover crops HVxCR         33           48
    ##  6 Summer cover crops Tilled        2,101      1382
    ##  7 Summer cover crops Buckwheat     36           48
    ##  8 Summer cover crops S. sudangrass 83           88
    ##  9 Summer cover crops Sunn hemp     681         790
    ## 10 Summer cover crops SSxSH         193         199

``` r
### Reduction
SupressionData %>% 
  group_by(Experiment,Site,CoverCrop) %>% 
  summarise(meanWeed = mean(WeedBiomass,na.rm = TRUE)) %>% 
  mutate(Control=nth(meanWeed,1)) %>% 
  select(Experiment,CoverCrop,Control,meanWeed) %>% 
  mutate(PercentRed = (Control-meanWeed)/Control,
         PercentRed2 = round(PercentRed,2))
```

    ## `summarise()` has grouped output by 'Experiment', 'Site'. You can override
    ## using the `.groups` argument.
    ## Adding missing grouping variables: `Site`

    ## # A tibble: 20 × 7
    ## # Groups:   Experiment, Site [4]
    ##    Site     Experiment         CoverCrop Control meanWeed PercentRed PercentRed2
    ##    <fct>    <fct>              <fct>       <dbl>    <dbl>      <dbl>       <dbl>
    ##  1 Farm Hub Summer cover crops Tilled      149.   149.         0            0   
    ##  2 Farm Hub Summer cover crops Buckwheat   149.     3.09       0.979        0.98
    ##  3 Farm Hub Summer cover crops Sunn hemp   149.    51.5        0.655        0.65
    ##  4 Farm Hub Summer cover crops S. sudan…   149.     2.22       0.985        0.99
    ##  5 Farm Hub Summer cover crops SSxSH       149.    13.0        0.913        0.91
    ##  6 Musgrave Summer cover crops Tilled       61.0   61.0        0            0   
    ##  7 Musgrave Summer cover crops Buckwheat    61.0    0.526      0.991        0.99
    ##  8 Musgrave Summer cover crops Sunn hemp    61.0   16.7        0.727        0.73
    ##  9 Musgrave Summer cover crops S. sudan…    61.0    6.05       0.901        0.9 
    ## 10 Musgrave Summer cover crops SSxSH        61.0    6.28       0.897        0.9 
    ## 11 Farm Hub Winter cover crops Tilled      134.   134.         0            0   
    ## 12 Farm Hub Winter cover crops Canola      134.    11.4        0.915        0.92
    ## 13 Farm Hub Winter cover crops Cereal r…   134.     6.17       0.954        0.95
    ## 14 Farm Hub Winter cover crops Hairy ve…   134.    54.8        0.591        0.59
    ## 15 Farm Hub Winter cover crops HVxCR       134.     2.82       0.979        0.98
    ## 16 Musgrave Winter cover crops Tilled       75.1   75.1        0            0   
    ## 17 Musgrave Winter cover crops Canola       75.1    5.85       0.922        0.92
    ## 18 Musgrave Winter cover crops Cereal r…    75.1    0.445      0.994        0.99
    ## 19 Musgrave Winter cover crops Hairy ve…    75.1    1.23       0.984        0.98
    ## 20 Musgrave Winter cover crops HVxCR        75.1    0.464      0.994        0.99

``` r
SupressionData %>% 
  group_by(Year,Experiment,Site,Block,CoverCrop) %>% 
  summarise(meanWeed = mean(WeedBiomass,na.rm = TRUE)) %>% 
  mutate(Control=nth(meanWeed,1)) %>% 
  select(Experiment,CoverCrop,Control,meanWeed) %>% 
  mutate(PercentRed = (Control-meanWeed)/Control) %>% 
  filter(CoverCrop != "Tilled") %>% 
  ungroup() %>% 
  summarise(meanPer = mean(PercentRed))
```

    ## `summarise()` has grouped output by 'Year', 'Experiment', 'Site', 'Block'. You
    ## can override using the `.groups` argument.
    ## Adding missing grouping variables: `Year`, `Site`, `Block`

    ## # A tibble: 1 × 1
    ##   meanPer
    ##     <dbl>
    ## 1   0.582

biomass reg.- didn’t use

``` r
# Data frame creation----
SupressionDataWint_CC <- SupressionDataWint
SupressionDataWint_CC <- SupressionDataWint_CC %>% 
  mutate(CoverCropBiomass=CoverCropBiomass*2, #Conversion to g/m2
         WeedBiomass = WeedBiomass*2) #Conversion to g/m2

SupressionDataSum_CC <- SupressionDataSum
SupressionDataSum_CC <- SupressionDataSum_CC %>% 
  mutate(CoverCropBiomass=CoverCropBiomass*2, #Conversion to g/m2
         WeedBiomass = WeedBiomass*2) #Conversion to g/m2

# Wint----
SupBiomassMod1_Wint<-lmer(WeedBiomass ~ Site*CoverCropBiomass + (1|SiteYear/Block),
                          data = SupressionDataWint_CC)
```

    ## boundary (singular) fit: see help('isSingular')

``` r
plot(SupBiomassMod1_Wint)
```

![](phylogenetic-competition_files/figure-gfm/regerssion-1.png)<!-- -->

``` r
anova(SupBiomassMod1_Wint)
```

    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##                       Sum Sq Mean Sq NumDF  DenDF F value    Pr(>F)    
    ## Site                   32002   32002     1  6.786  6.5838   0.03823 *  
    ## CoverCropBiomass      336341  336341     1 73.793 69.1951 3.299e-12 ***
    ## Site:CoverCropBiomass   6031    6031     1 73.793  1.2407   0.26896    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#This warning occurs when the optimizer visits a region of parameter space that is invalid. It is not a problem as long as the optimizer has left that region of parameter space upon convergence, which is indicated by an absence of the model convergence warnings described above.
SupBiomassMod2_Wint <- glmmTMB(WeedBiomass ~ Site*CoverCropBiomass+(1|SiteYear/Block),
                               family = tweedie(), data = SupressionDataWint_CC)
```

    ## Warning in (function (start, objective, gradient = NULL, hessian = NULL, :
    ## NA/NaN function evaluation

``` r
plot(simulateResiduals(SupBiomassMod2_Wint), quantreg = FALSE)
```

![](phylogenetic-competition_files/figure-gfm/regerssion-2.png)<!-- -->

``` r
car::Anova(SupBiomassMod2_Wint,type = 3)
```

    ## Analysis of Deviance Table (Type III Wald chisquare tests)
    ## 
    ## Response: WeedBiomass
    ##                          Chisq Df Pr(>Chisq)    
    ## (Intercept)           293.4817  1  < 2.2e-16 ***
    ## Site                    2.7545  1    0.09698 .  
    ## CoverCropBiomass       51.4527  1  7.334e-13 ***
    ## Site:CoverCropBiomass   4.0186  1    0.04500 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
test(emtrends(SupBiomassMod2_Wint, "Site",var = "CoverCropBiomass"))
```

    ##  Site     CoverCropBiomass.trend       SE  df z.ratio p.value
    ##  Farm Hub               -0.00346 0.000482 Inf  -7.173  <.0001
    ##  Musgrave               -0.00483 0.000525 Inf  -9.214  <.0001

``` r
SupBiomassMod3_Wint <- update(SupBiomassMod2_Wint,
                              family = gaussian(link = "log"), start = list(beta=rep(0,4)))
```

    ## Warning in finalizeTMB(TMBStruc, obj, fit, h, data.tmb.old): Model convergence
    ## problem; non-positive-definite Hessian matrix. See vignette('troubleshooting')

``` r
#simulateResiduals(SupBiomassMod3_Wint, plot = TRUE) #Convergence issue and bad fit

# Sum----
SupBiomassMod1_Sum<-lmer(WeedBiomass ~ Site*CoverCropBiomass + (1|SiteYear/Block),
                          data = SupressionDataSum_CC)
```

    ## boundary (singular) fit: see help('isSingular')

``` r
plot(SupBiomassMod1_Sum)
```

![](phylogenetic-competition_files/figure-gfm/regerssion-3.png)<!-- -->

``` r
anova(SupBiomassMod1_Sum)
```

    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##                       Sum Sq Mean Sq NumDF  DenDF F value   Pr(>F)    
    ## Site                   16529   16529     1  2.368  3.6672 0.175402    
    ## CoverCropBiomass      319892  319892     1 74.518 70.9738 1.94e-12 ***
    ## Site:CoverCropBiomass  52117   52117     1 74.518 11.5631 0.001084 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
SupBiomassMod2_Sum <- glmmTMB(WeedBiomass ~ Site*CoverCropBiomass + (1|SiteYear/Block),
                               family = tweedie(), data = SupressionDataSum_CC)
```

    ## Warning in (function (start, objective, gradient = NULL, hessian = NULL, :
    ## NA/NaN function evaluation

``` r
simulateResiduals(SupBiomassMod2_Sum, plot = TRUE)
```

![](phylogenetic-competition_files/figure-gfm/regerssion-4.png)<!-- -->

    ## Object of Class DHARMa with simulated residuals based on 250 simulations with refit = FALSE . See ?DHARMa::simulateResiduals for help. 
    ##  
    ## Scaled residual values: 0.332 0.388 0.244 0.32 0.704 0.268 0.14 0.536 0.156 0.192 0.752 0.296 0.376 0.312 0.316 0.116 0.116 0.504 0.672 0.352 ...

``` r
car::Anova(SupBiomassMod2_Sum,type = 3)
```

    ## Analysis of Deviance Table (Type III Wald chisquare tests)
    ## 
    ## Response: WeedBiomass
    ##                          Chisq Df Pr(>Chisq)    
    ## (Intercept)           105.2618  1  < 2.2e-16 ***
    ## Site                    2.3681  1     0.1238    
    ## CoverCropBiomass       39.4019  1   3.45e-10 ***
    ## Site:CoverCropBiomass   1.4396  1     0.2302    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
test(emtrends(SupBiomassMod2_Sum, "Site",var = "CoverCropBiomass"))
```

    ##  Site     CoverCropBiomass.trend       SE  df z.ratio p.value
    ##  Farm Hub               -0.00498 0.000794 Inf  -6.277  <.0001
    ##  Musgrave               -0.00352 0.000916 Inf  -3.845  0.0001

``` r
#SupBiomassMod3_Sum <- update(SupBiomassMod2_Sum, family = gaussian(link = "log"), start = 0)
#simulateResiduals(SupBiomassMod3_Sum, plot = TRUE) #really bad


# Viz----
##  Winter
SupressionDataWint_Mean <- SupressionDataWint %>% 
  group_by(Site,CoverCrop) %>% 
  summarise(CoverCropBiomass_Mean = mean(CoverCropBiomass*2,na.rm = TRUE),
            seCoverCropBiomass = plotrix::std.error(CoverCropBiomass*2),
            WeedBiomass_Mean = mean(WeedBiomass*2,na.rm = TRUE),
            seWeedBiomass = plotrix::std.error(WeedBiomass*2))
```

    ## `summarise()` has grouped output by 'Site'. You can override using the
    ## `.groups` argument.

``` r
WintRegLet<-left_join(SupLettersWint,SupressionDataWint_Mean) #from comp. model
```

    ## Joining with `by = join_by(CoverCrop, Site)`

``` r
WintRegFig <-
  emmip(SupBiomassMod2_Wint,~CoverCropBiomass|Site,
        at=list(CoverCropBiomass = seq(from = 1, to = 1300, by = 10)),
        linearg = list(size = 1,color="darkgrey"),
        type = "response") +
    geom_errorbar(data=SupressionDataWint_Mean, orientation = "y",
                  aes(x = CoverCropBiomass_Mean, y = WeedBiomass_Mean,
                      xmin=CoverCropBiomass_Mean - seCoverCropBiomass,
                      xmax=CoverCropBiomass_Mean + seCoverCropBiomass,
                      color=CoverCrop,height = 0),show.legend=FALSE)+
    geom_errorbar(data=SupressionDataWint_Mean, orientation = "x",
                  aes(x = CoverCropBiomass_Mean, y = WeedBiomass_Mean,
                      ymin=WeedBiomass_Mean-seWeedBiomass,
                      ymax=WeedBiomass_Mean+seWeedBiomass,
                      color=CoverCrop,width = 0),show.legend=FALSE)+
    geom_point(data = SupressionDataWint_Mean,
               aes(x = CoverCropBiomass_Mean, y = WeedBiomass_Mean,
                   fill = CoverCrop, color = CoverCrop, shape = CoverCrop),
               size = 5.75, group = "CoverCrop")+
    ggrepel::geom_text_repel(data = WintRegLet,
              aes(CoverCropBiomass_Mean,WeedBiomass_Mean,label = trimws(.group)),
              vjust = -.45, hjust = -0.45,force=.3,point.padding=1,
              segment.color = NA,size = 8.15)+  
    scale_fill_manual(values = c("black",myCol[1:4]),
                      labels = c("Tilled control","Canola","Cereal rye (CR)",
                                 "Hairy vetch (HV)","CR x HV"))+
    scale_color_manual(values = c("black",myCol[1:4]),
                       labels = c("Tilled control","Canola","Cereal rye (CR)",
                                 "Hairy vetch (HV)","CR x HV"))+
    scale_shape_manual(values = c(1,22,21,25,23),
                       labels = c("Tilled control","Canola","Cereal rye (CR)",
                                 "Hairy vetch (HV)","CR x HV"))+
    scale_y_continuous(expand = expansion(mult = c(0.07, 0.015)))+
    scale_x_continuous(labels = scales::comma)+
    labs(y=expression("Weed biomass (g"~m^-2*')'),
         x=expression("Cover crop biomass (g"~m^-2*')'),
         color = "Treatment",fill = "Treatment",
         shape = "Treatment")+
    theme_bw(base_size = 20)+
    #ggh4x::facet_grid2("Winter cover crops" ~ Site, scales = "free_y", independent = "y")+
    theme(axis.title.x = element_blank(),
          legend.spacing.y = unit(.15, 'cm'))+
    guides(fill = guide_legend(byrow = TRUE))
```

    ## Warning in geom_errorbar(data = SupressionDataWint_Mean, orientation = "y", :
    ## Ignoring unknown aesthetics: height

``` r
##  Summer
SupressionDataSum_Mean <- SupressionDataSum %>% 
  group_by(Site,CoverCrop) %>% 
  summarise(CoverCropBiomass_Mean = mean(CoverCropBiomass*2,na.rm = TRUE),
            seCoverCropBiomass = plotrix::std.error(CoverCropBiomass*2),
            WeedBiomass_Mean = mean(WeedBiomass*2,na.rm = TRUE),
            seWeedBiomass = plotrix::std.error(WeedBiomass*2))
```

    ## `summarise()` has grouped output by 'Site'. You can override using the
    ## `.groups` argument.

``` r
SumRegLet<-left_join(SupLettersSum,SupressionDataSum_Mean) #from comp. model
```

    ## Joining with `by = join_by(CoverCrop, Site)`

``` r
SummerRegFig <-
  emmip(SupBiomassMod2_Sum,~CoverCropBiomass|Site,
        at=list(CoverCropBiomass = 1:600),
        linearg = list(size = 1,color = "darkgrey"),
        type = "response") +
    geom_errorbar(data=SupressionDataSum_Mean, orientation = "y",
                  aes(x = CoverCropBiomass_Mean, y = WeedBiomass_Mean,
                      xmin=CoverCropBiomass_Mean - seCoverCropBiomass,
                      xmax=CoverCropBiomass_Mean + seCoverCropBiomass,
                      color=CoverCrop,height = 0),show.legend=FALSE)+
    geom_errorbar(data=SupressionDataSum_Mean, orientation = "x",
                  aes(x = CoverCropBiomass_Mean, y = WeedBiomass_Mean,
                      ymin=WeedBiomass_Mean-seWeedBiomass,
                      ymax=WeedBiomass_Mean+seWeedBiomass,
                      color=CoverCrop,width = 0),show.legend=FALSE)+
    geom_point(data = SupressionDataSum_Mean,
               aes(x = CoverCropBiomass_Mean, y = WeedBiomass_Mean,
                   fill = CoverCrop, color = CoverCrop, shape = CoverCrop),
               size = 5.75, group = "CoverCrop")+
    ggrepel::geom_text_repel(data = SumRegLet,
              aes(CoverCropBiomass_Mean,WeedBiomass_Mean,label = trimws(.group)),
              vjust = -.45, hjust = -0.45,force=.5,
              segment.color = NA,size = 8.15)+
    scale_fill_manual(values = c("black",myCol[5:8]),
                      labels = c("Tilled control","Buckwheat","S. sudangrass (SS)",
                                 "Sunn hemp (SH)", "SS x SH"))+
    scale_color_manual(values = c("black",myCol[5:8]),
                       labels = c("Tilled control","Buckwheat","S. sudangrass (SS)",
                                 "Sunn hemp (SH)", "SS x SH"))+
    scale_shape_manual(values = c(1,22,21,25,23),
                       labels = c("Tilled control","Buckwheat","S. sudangrass (SS)",
                                 "Sunn hemp (SH)", "SS x SH"))+
    scale_y_continuous(expand = expansion(mult = c(0.055, 0.015)))+
    labs(y=expression("Weed biomass (g"~m^-2*')'),
         x=expression("Cover crop biomass (g"~m^-2*')'),
         color = "Treatment",fill = "Treatment",
         shape = "Treatment")+
    theme_bw(base_size = 20)+
    #ggh4x::facet_grid2("Summer cover crops" ~ Site, scales = "free_y", independent = "y")+ #seems to to work anymore?
    theme(strip.text.x = element_blank(),
          legend.spacing.y = unit(.15, 'cm'))+
   guides(fill = guide_legend(byrow = TRUE))
```

    ## Warning in geom_errorbar(data = SupressionDataSum_Mean, orientation = "y", :
    ## Ignoring unknown aesthetics: height

``` r
#pdf("Figures (450dpi)/WeedSuppression/RegressionFig.pdf",width = 10.5, height = 7.5)
ggpubr::ggarrange(WintRegFig,NULL,SummerRegFig,
                  nrow = 3,align = "hv",
                  heights = c(1,-0.13,1))
```

![](phylogenetic-competition_files/figure-gfm/regerssion-5.png)<!-- -->

``` r
#dev.off()



# Collinearity check----
##  Moderate correlation confirms my use of two models
## emmeans is totally off in the models that have CoverCrop*CoverCropBiomass
SupBiomassMod2_WintTmp <- lm(WeedBiomass~ Site*CoverCrop+CoverCropBiomass, data = SupressionDataWint_CC)

plot(emmeans(SupBiomassMod2_WintTmp,~CoverCrop*Site, type = "response"))
```

![](phylogenetic-competition_files/figure-gfm/regerssion-6.png)<!-- -->

``` r
summary(SupBiomassMod2_WintTmp)
```

    ## 
    ## Call:
    ## lm(formula = WeedBiomass ~ Site * CoverCrop + CoverCropBiomass, 
    ##     data = SupressionDataWint_CC)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -168.69  -14.06   -1.83    7.50  345.55 
    ## 
    ## Coefficients:
    ##                                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                        268.31114   23.17752  11.576  < 2e-16 ***
    ## SiteMusgrave                      -118.12864   31.73713  -3.722 0.000403 ***
    ## CoverCropCanola                   -201.59126   42.11616  -4.787 9.53e-06 ***
    ## CoverCropCereal rye               -180.33437   57.21937  -3.152 0.002415 ** 
    ## CoverCropHairy vetch              -131.58647   36.01096  -3.654 0.000503 ***
    ## CoverCropHVxCR                    -210.09281   45.85676  -4.582 2.03e-05 ***
    ## CoverCropBiomass                    -0.06488    0.04084  -1.589 0.116752    
    ## SiteMusgrave:CoverCropCanola       109.68303   44.15870   2.484 0.015467 *  
    ## SiteMusgrave:CoverCropCereal rye    81.39352   46.91222   1.735 0.087268 .  
    ## SiteMusgrave:CoverCropHairy vetch   14.25332   44.17894   0.323 0.747968    
    ## SiteMusgrave:CoverCropHVxCR        105.94888   44.37894   2.387 0.019755 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 61.32 on 68 degrees of freedom
    ## Multiple R-squared:  0.6829, Adjusted R-squared:  0.6362 
    ## F-statistic: 14.64 on 10 and 68 DF,  p-value: 1.864e-13

``` r
check_collinearity(SupBiomassMod2_WintTmp)
```

    ## Model has interaction terms. VIFs might be inflated.
    ##   Try to center the variables used for the interaction, or check
    ##   multicollinearity among predictors of a model without interaction terms.

    ## # Check for Multicollinearity
    ## 
    ## Moderate Correlation
    ## 
    ##              Term  VIF     VIF 95% CI adj. VIF Tolerance Tolerance 95% CI
    ##              Site 5.29 [ 3.85,  7.46]     2.30      0.19     [0.13, 0.26]
    ##  CoverCropBiomass 5.07 [ 3.69,  7.14]     2.25      0.20     [0.14, 0.27]
    ## 
    ## High Correlation
    ## 
    ##            Term   VIF     VIF 95% CI adj. VIF Tolerance Tolerance 95% CI
    ##       CoverCrop 59.85 [41.64, 86.23]     1.67      0.02     [0.01, 0.02]
    ##  Site:CoverCrop 62.35 [43.37, 89.83]     7.90      0.02     [0.01, 0.02]

``` r
SupBiomassMod2_SumTmp <- glmmTMB(WeedBiomass~ Site+CoverCrop+CoverCropBiomass+(1|SiteYear/Block),
                               family = tweedie(), data = SupressionDataSum_CC)
```

    ## Warning in (function (start, objective, gradient = NULL, hessian = NULL, :
    ## NA/NaN function evaluation

``` r
check_collinearity(SupBiomassMod2_SumTmp)
```

    ## # Check for Multicollinearity
    ## 
    ## Low Correlation
    ## 
    ##  Term  VIF    VIF 95% CI adj. VIF Tolerance Tolerance 95% CI
    ##  Site 1.03 [1.00, 15.91]     1.02      0.97     [0.06, 1.00]
    ## 
    ## Moderate Correlation
    ## 
    ##              Term  VIF    VIF 95% CI adj. VIF Tolerance Tolerance 95% CI
    ##         CoverCrop 7.10 [5.04, 10.22]     2.66      0.14     [0.10, 0.20]
    ##  CoverCropBiomass 6.94 [4.93,  9.99]     2.63      0.14     [0.10, 0.20]

# Life cycle

``` r
library(FD)
```

    ## Loading required package: ade4

    ## Loading required package: geometry

``` r
# Cleaning COOP data to just show weeds in the Tilled control
WeedSpCleanTilled <- WeedSpClean[which(rownames(WeedSpClean) %like% "Tilled"),]
WeedSpCleanTilled <- WeedSpCleanTilled[,which(colSums(WeedSpCleanTilled)>0)]

# Loading and cleaning weed communities for the tilled control

# Dummy variable----
TraitsWeed_Dummy <- read.xlsx("../Data/TraitsCOOPWeeds.xlsx",sheet = 1)
TraitsWeed_Dummy <- TraitsWeed_Dummy %>% column_to_rownames("Species")

TraitsWeed_Dummy <- as.matrix(TraitsWeed_Dummy)
WeedSpCleanTilled <- as.matrix(WeedSpCleanTilled)
TraitsWeed_Dummy<-TraitsWeed_Dummy[order(match(rownames(TraitsWeed_Dummy),
                                   colnames(WeedSpCleanTilled))),]

CWM_DummyRaw <- functcomp(TraitsWeed_Dummy,WeedSpCleanTilled,CWM.type = "all") #Confirmed that this properly deals with species that have Wint/Sum emergence
CWM_DummyFull <- CWM_DummyRaw %>% 
  select(ends_with("_1")) %>% 
  rename_with(~gsub("_1", "", .x, fixed = TRUE)) %>% 
  rownames_to_column() %>% 
  separate(rowname,
           into = c("Experiment","Trial","Site",
                    "Year","Block","Plot","CoverCrop"),
           sep = "_") %>% 
  mutate(across(.cols = c(Experiment,Trial,Site,Year,Block,CoverCrop),
                .fns = factor))

CWM_DummyEmergence <- CWM_DummyFull %>% 
  select(-c(Annual, Biennial, Perennial)) %>%
  pivot_longer(cols = c(Winter,Summer),
               names_to = "Emergence",
               values_to = "Percent_Emergence")

CWM_DummyLifeCycle <- CWM_DummyFull %>% 
  select(-c(Winter,Summer)) %>%
  pivot_longer(cols = c(Annual, Biennial, Perennial),
               names_to = "LifeCycle",
               values_to = "Percent_LifeCycle")

#Summaries
CWM_DummyFull %>% 
  group_by(Experiment,Site) %>% 
  summarise(across(.cols = c(Summer,Winter,Annual,Biennial,Perennial),
                   ~scales::percent(mean(.x,na.rm=TRUE))))
```

    ## `summarise()` has grouped output by 'Experiment'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 4 × 7
    ## # Groups:   Experiment [2]
    ##   Experiment         Site     Summer Winter Annual Biennial Perennial
    ##   <fct>              <fct>    <chr>  <chr>  <chr>  <chr>    <chr>    
    ## 1 Summer cover crops Farm Hub 95%    21%    100%   0%       0%       
    ## 2 Summer cover crops Musgrave 99%    28%    69%    0%       31%      
    ## 3 Winter cover crops Farm Hub 48%    85%    94%    19%      7%       
    ## 4 Winter cover crops Musgrave 56%    86%    92%    4%       13%

``` r
ggplot(CWM_DummyEmergence,aes(Site,Percent_Emergence,
                              fill=Emergence))+
  stat_summary(geom = "bar",fun = "mean",position = "dodge")+
  stat_summary(geom = "errorbar",fun.data = "mean_se",width=.1,
               position = position_dodge(.9))+
  facet_grid(~Experiment)
```

![](phylogenetic-competition_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
ggplot(CWM_DummyLifeCycle,aes(Site,Percent_LifeCycle,
                              fill=LifeCycle))+
  stat_summary(geom = "bar",fun = "mean",position = "dodge")+
  stat_summary(geom = "errorbar",fun.data = "mean_se",width=.1,
               position = position_dodge(.9))+
  facet_grid(~Experiment)
```

![](phylogenetic-competition_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

# Permanova

``` r
cols <- c("grey",myCol[5:8])
cols2 <- c("grey",myCol[1:4])

# Winter
#Row sums of zero removed-non imputed for NMDS and PerManova b/c multiple plots naturally at 0
WeedSpClean_Wint.PerM<-WeedSpClean_Wint[rowSums(WeedSpClean_Wint) != 0, ]
WeedEnvClean_Wint.PerM <- data.frame(Trt = rownames(WeedSpClean_Wint.PerM))
WeedEnvClean_Wint.PerM <- WeedEnvClean_Wint.PerM %>% 
  separate(col = Trt,
           into = c("Experiment","Trial","Site","Year", "Block","Plot","CoverCrop"),
           sep="_")

WeedEnvClean_Wint.PerM$Site <- as.factor(WeedEnvClean_Wint.PerM$Site)
WeedEnvClean_Wint.PerM$CoverCrop <- factor(WeedEnvClean_Wint.PerM$CoverCrop,
                                           levels = c("Tilled","Canola","Cereal rye",
                                                      "Hairy vetch","HVxCR"),
                                           labels = c("Till","Can","CR","HV","CRxHV"))

WeedSpClean_Wint.PerM.FH <- WeedSpClean_Wint.PerM %>% 
  rownames_to_column(var = "RowNames") %>%
  filter(grepl("Farm Hub", RowNames)) %>% 
  column_to_rownames(var = "RowNames")
WeedSpClean_Wint.PerM.FH<-WeedSpClean_Wint.PerM.FH[,colSums(WeedSpClean_Wint.PerM.FH) != 0]
WeedEnvClean_Wint.PerM.FH <- WeedEnvClean_Wint.PerM %>% 
  filter(grepl("Farm Hub", Site))

WeedSpClean_Wint.PerM.Mus <- WeedSpClean_Wint.PerM %>% 
  rownames_to_column(var = "RowNames") %>%
  filter(grepl("Mus", RowNames)) %>% 
  column_to_rownames(var = "RowNames")
WeedSpClean_Wint.PerM.Mus<-WeedSpClean_Wint.PerM.Mus[,colSums(WeedSpClean_Wint.PerM.Mus) != 0]
WeedEnvClean_Wint.PerM.Mus <- WeedEnvClean_Wint.PerM %>% 
  filter(grepl("Mus", Site))

# Summer
#Row sums of zero removed - for NMDS and PerManova
WeedSpClean_Sum.PerM<-WeedSpClean_Sum[rowSums(WeedSpClean_Sum) != 0, ]
WeedEnvClean_Sum.PerM <- data.frame(Trt = rownames(WeedSpClean_Sum.PerM))
WeedEnvClean_Sum.PerM <- WeedEnvClean_Sum.PerM %>% 
  separate(col = Trt,
           into = c("Experiment","Trial","Site","Year", "Block","Plot","CoverCrop"),
           sep="_")
WeedEnvClean_Sum.PerM$Site <- as.factor(WeedEnvClean_Sum.PerM$Site)
WeedEnvClean_Sum.PerM$CoverCrop <- factor(WeedEnvClean_Sum.PerM$CoverCrop,
                                          levels = c("Tilled","Buckwheat","S. sudangrass",
                                                     "Sunn hemp","SSxSH"),
                                          labels = c("Till","BW","SS","SH","SSxSH"))

WeedSpClean_Sum.PerM.FH <- WeedSpClean_Sum.PerM %>% 
  rownames_to_column(var = "RowNames") %>%
  filter(grepl("Farm Hub", RowNames)) %>% 
  column_to_rownames(var = "RowNames")
WeedSpClean_Sum.PerM.FH<-WeedSpClean_Sum.PerM.FH[,colSums(WeedSpClean_Sum.PerM.FH) != 0]
WeedEnvClean_Sum.PerM.FH <- WeedEnvClean_Sum.PerM %>% 
  filter(grepl("Farm Hub", Site))

WeedSpClean_Sum.PerM.Mus <- WeedSpClean_Sum.PerM %>% 
  rownames_to_column(var = "RowNames") %>%
  filter(grepl("Mus", RowNames)) %>% 
  column_to_rownames(var = "RowNames")
WeedSpClean_Sum.PerM.Mus<-WeedSpClean_Sum.PerM.Mus[,colSums(WeedSpClean_Sum.PerM.Mus) != 0]
WeedEnvClean_Sum.PerM.Mus <- WeedEnvClean_Sum.PerM %>% 
  filter(grepl("Mus", Site))
```

``` r
#Ploting
#pdf("Figures (450dpi)/nmds.pdf",width = 6.15, height = 6.64)
m <- matrix(c(1,2,3,4,5,5),nrow = 3,ncol = 2,byrow = TRUE)
layout(mat = m,heights = c(0.4,0.4,0.2))
par(oma = c(8,1,1,1), mfrow = c(2, 2), mar = c(4, 4, 4, 4))

#FH -winter
nmds_Wint.FH<-metaMDS(WeedSpClean_Wint.PerM.FH)
```

    ## Square root transformation
    ## Wisconsin double standardization
    ## Run 0 stress 0.1592958 
    ## Run 1 stress 0.1778739 
    ## Run 2 stress 0.1868368 
    ## Run 3 stress 0.1592958 
    ## ... New best solution
    ## ... Procrustes: rmse 2.284124e-05  max resid 7.250395e-05 
    ## ... Similar to previous best
    ## Run 4 stress 0.1746006 
    ## Run 5 stress 0.1835827 
    ## Run 6 stress 0.1830916 
    ## Run 7 stress 0.1879677 
    ## Run 8 stress 0.1810746 
    ## Run 9 stress 0.1820266 
    ## Run 10 stress 0.1824829 
    ## Run 11 stress 0.1589249 
    ## ... New best solution
    ## ... Procrustes: rmse 0.01993279  max resid 0.1106036 
    ## Run 12 stress 0.1678079 
    ## Run 13 stress 0.1824832 
    ## Run 14 stress 0.1832482 
    ## Run 15 stress 0.176128 
    ## Run 16 stress 0.167336 
    ## Run 17 stress 0.1906578 
    ## Run 18 stress 0.1778739 
    ## Run 19 stress 0.1706457 
    ## Run 20 stress 0.1817176 
    ## *** Best solution was not repeated -- monoMDS stopping criteria:
    ##     20: stress ratio > sratmax

``` r
ordiplot(nmds_Wint.FH, type = "n")
points(nmds_Wint.FH, cex = 1, pch = c(1,22,21,25,23)[WeedEnvClean_Wint.PerM.FH$CoverCrop],
       bg = cols2[WeedEnvClean_Wint.PerM.FH$CoverCrop],
       col = cols2[WeedEnvClean_Wint.PerM.FH$CoverCrop])
ordiellipse(nmds_Wint.FH, groups = WeedEnvClean_Wint.PerM.FH$CoverCrop, draw = "polygon",
         label = TRUE, kind = "sd")
ordispider(nmds_Wint.FH, groups = WeedEnvClean_Wint.PerM.FH$CoverCrop, draw = "polygon",
         lty = "dotted",col = cols2[WeedEnvClean_Wint.PerM.FH$CoverCrop],
         label = TRUE, kind = "sd")
```

    ## Warning in match.fun(FUN)(...): "draw" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "kind" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "draw" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "kind" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "draw" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "kind" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "draw" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "kind" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "draw" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "kind" is not a graphical parameter

    ## Warning in strwidth("m", cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strwidth("m", cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strheight("x", cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strheight("x", cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strwidth(labels[i], cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strwidth(labels[i], cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strheight(labels[i], cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strheight(labels[i], cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strwidth("m", cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strwidth("m", cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strheight("x", cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strheight("x", cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strwidth(labels[i], cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strwidth(labels[i], cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strheight(labels[i], cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strheight(labels[i], cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strwidth("m", cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strwidth("m", cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strheight("x", cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strheight("x", cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strwidth(labels[i], cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strwidth(labels[i], cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strheight(labels[i], cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strheight(labels[i], cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strwidth("m", cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strwidth("m", cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strheight("x", cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strheight("x", cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strwidth(labels[i], cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strwidth(labels[i], cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strheight(labels[i], cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strheight(labels[i], cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strwidth("m", cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strwidth("m", cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strheight("x", cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strheight("x", cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strwidth(labels[i], cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strwidth(labels[i], cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strheight(labels[i], cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strheight(labels[i], cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in match.fun(FUN)(...): "draw" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "kind" is not a graphical parameter

    ## Warning in text.default(...): "draw" is not a graphical parameter

    ## Warning in text.default(...): "kind" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "draw" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "kind" is not a graphical parameter

    ## Warning in text.default(...): "draw" is not a graphical parameter

    ## Warning in text.default(...): "kind" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "draw" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "kind" is not a graphical parameter

    ## Warning in text.default(...): "draw" is not a graphical parameter

    ## Warning in text.default(...): "kind" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "draw" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "kind" is not a graphical parameter

    ## Warning in text.default(...): "draw" is not a graphical parameter

    ## Warning in text.default(...): "kind" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "draw" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "kind" is not a graphical parameter

    ## Warning in text.default(...): "draw" is not a graphical parameter

    ## Warning in text.default(...): "kind" is not a graphical parameter

``` r
title(main = "Winter experiment: Farm hub")

#Mus -Winter
nmds_Wint.Mus<-metaMDS(WeedSpClean_Wint.PerM.Mus)
```

    ## Wisconsin double standardization
    ## Run 0 stress 0.2442452 
    ## Run 1 stress 0.2442012 
    ## ... New best solution
    ## ... Procrustes: rmse 0.1227252  max resid 0.3206513 
    ## Run 2 stress 0.2363263 
    ## ... New best solution
    ## ... Procrustes: rmse 0.1024122  max resid 0.4129705 
    ## Run 3 stress 0.2411793 
    ## Run 4 stress 0.2675861 
    ## Run 5 stress 0.234014 
    ## ... New best solution
    ## ... Procrustes: rmse 0.1237047  max resid 0.3058719 
    ## Run 6 stress 0.2324525 
    ## ... New best solution
    ## ... Procrustes: rmse 0.03509274  max resid 0.1223162 
    ## Run 7 stress 0.2440909 
    ## Run 8 stress 0.2634745 
    ## Run 9 stress 0.2340455 
    ## Run 10 stress 0.2421463 
    ## Run 11 stress 0.2405523 
    ## Run 12 stress 0.2299219 
    ## ... New best solution
    ## ... Procrustes: rmse 0.08184566  max resid 0.4594799 
    ## Run 13 stress 0.2586336 
    ## Run 14 stress 0.2408468 
    ## Run 15 stress 0.2302377 
    ## ... Procrustes: rmse 0.02886271  max resid 0.1278647 
    ## Run 16 stress 0.239693 
    ## Run 17 stress 0.2311328 
    ## Run 18 stress 0.2409024 
    ## Run 19 stress 0.2333654 
    ## Run 20 stress 0.2333655 
    ## *** Best solution was not repeated -- monoMDS stopping criteria:
    ##     19: stress ratio > sratmax
    ##      1: scale factor of the gradient < sfgrmin

``` r
ordiplot(nmds_Wint.Mus, type = "n")
points(nmds_Wint.Mus, cex = 1, pch = c(1,22,21,25,23)[WeedEnvClean_Wint.PerM.Mus$CoverCrop],
       bg = cols2[WeedEnvClean_Wint.PerM.Mus$CoverCrop],
       col = cols2[WeedEnvClean_Wint.PerM.Mus$CoverCrop])
ordiellipse(nmds_Wint.Mus, groups = WeedEnvClean_Wint.PerM.Mus$CoverCrop, draw = "polygon",
         label = TRUE, kind = "sd")
ordispider(nmds_Wint.Mus, groups = WeedEnvClean_Wint.PerM.Mus$CoverCrop, draw = "polygon",
         lty = "dotted",col = cols2[WeedEnvClean_Wint.PerM.Mus$CoverCrop],
         label = TRUE, kind = "sd")
```

    ## Warning in match.fun(FUN)(...): "draw" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "kind" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "draw" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "kind" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "draw" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "kind" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "draw" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "kind" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "draw" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "kind" is not a graphical parameter

    ## Warning in strwidth("m", cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strwidth("m", cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strheight("x", cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strheight("x", cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strwidth(labels[i], cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strwidth(labels[i], cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strheight(labels[i], cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strheight(labels[i], cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strwidth("m", cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strwidth("m", cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strheight("x", cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strheight("x", cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strwidth(labels[i], cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strwidth(labels[i], cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strheight(labels[i], cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strheight(labels[i], cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strwidth("m", cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strwidth("m", cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strheight("x", cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strheight("x", cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strwidth(labels[i], cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strwidth(labels[i], cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strheight(labels[i], cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strheight(labels[i], cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strwidth("m", cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strwidth("m", cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strheight("x", cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strheight("x", cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strwidth(labels[i], cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strwidth(labels[i], cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strheight(labels[i], cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strheight(labels[i], cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strwidth("m", cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strwidth("m", cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strheight("x", cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strheight("x", cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strwidth(labels[i], cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strwidth(labels[i], cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strheight(labels[i], cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strheight(labels[i], cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in match.fun(FUN)(...): "draw" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "kind" is not a graphical parameter

    ## Warning in text.default(...): "draw" is not a graphical parameter

    ## Warning in text.default(...): "kind" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "draw" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "kind" is not a graphical parameter

    ## Warning in text.default(...): "draw" is not a graphical parameter

    ## Warning in text.default(...): "kind" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "draw" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "kind" is not a graphical parameter

    ## Warning in text.default(...): "draw" is not a graphical parameter

    ## Warning in text.default(...): "kind" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "draw" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "kind" is not a graphical parameter

    ## Warning in text.default(...): "draw" is not a graphical parameter

    ## Warning in text.default(...): "kind" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "draw" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "kind" is not a graphical parameter

    ## Warning in text.default(...): "draw" is not a graphical parameter

    ## Warning in text.default(...): "kind" is not a graphical parameter

``` r
title(main = "Winter experiment: Musgrave")

#FH Summer
nmds_Sum.FH<-metaMDS(WeedSpClean_Sum.PerM.FH)
```

    ## Square root transformation
    ## Wisconsin double standardization
    ## Run 0 stress 0.1502587 
    ## Run 1 stress 0.1623466 
    ## Run 2 stress 0.1561949 
    ## Run 3 stress 0.1386979 
    ## ... New best solution
    ## ... Procrustes: rmse 0.04923415  max resid 0.2800473 
    ## Run 4 stress 0.1493727 
    ## Run 5 stress 0.1510915 
    ## Run 6 stress 0.1411913 
    ## Run 7 stress 0.1399067 
    ## Run 8 stress 0.1416505 
    ## Run 9 stress 0.1527625 
    ## Run 10 stress 0.1546555 
    ## Run 11 stress 0.141191 
    ## Run 12 stress 0.1386976 
    ## ... New best solution
    ## ... Procrustes: rmse 0.0001763335  max resid 0.0004564791 
    ## ... Similar to previous best
    ## Run 13 stress 0.141191 
    ## Run 14 stress 0.1399066 
    ## Run 15 stress 0.1696702 
    ## Run 16 stress 0.1386462 
    ## ... New best solution
    ## ... Procrustes: rmse 0.00666997  max resid 0.02777312 
    ## Run 17 stress 0.1386465 
    ## ... Procrustes: rmse 0.0005572233  max resid 0.001490128 
    ## ... Similar to previous best
    ## Run 18 stress 0.1398529 
    ## Run 19 stress 0.1631787 
    ## Run 20 stress 0.1416502 
    ## *** Best solution repeated 1 times

``` r
ordiplot(nmds_Sum.FH, type = "n")
points(nmds_Sum.FH, cex = 1, pch = c(1,22,21,25,23)[WeedEnvClean_Sum.PerM.FH$CoverCrop],
       bg = cols[WeedEnvClean_Sum.PerM.FH$CoverCrop],
       col = cols[WeedEnvClean_Sum.PerM.FH$CoverCrop])
ordiellipse(nmds_Sum.FH, groups = WeedEnvClean_Sum.PerM.FH$CoverCrop, draw = "polygon",
         label = TRUE, kind = "sd")
ordispider(nmds_Sum.FH, groups = WeedEnvClean_Sum.PerM.FH$CoverCrop, draw = "polygon",
         lty = "dotted",col = cols[WeedEnvClean_Sum.PerM.FH$CoverCrop],
         label = TRUE, kind = "sd")
```

    ## Warning in match.fun(FUN)(...): "draw" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "kind" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "draw" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "kind" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "draw" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "kind" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "draw" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "kind" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "draw" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "kind" is not a graphical parameter

    ## Warning in strwidth("m", cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strwidth("m", cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strheight("x", cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strheight("x", cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strwidth(labels[i], cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strwidth(labels[i], cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strheight(labels[i], cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strheight(labels[i], cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strwidth("m", cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strwidth("m", cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strheight("x", cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strheight("x", cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strwidth(labels[i], cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strwidth(labels[i], cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strheight(labels[i], cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strheight(labels[i], cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strwidth("m", cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strwidth("m", cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strheight("x", cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strheight("x", cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strwidth(labels[i], cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strwidth(labels[i], cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strheight(labels[i], cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strheight(labels[i], cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strwidth("m", cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strwidth("m", cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strheight("x", cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strheight("x", cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strwidth(labels[i], cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strwidth(labels[i], cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strheight(labels[i], cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strheight(labels[i], cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strwidth("m", cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strwidth("m", cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strheight("x", cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strheight("x", cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strwidth(labels[i], cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strwidth(labels[i], cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strheight(labels[i], cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strheight(labels[i], cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in match.fun(FUN)(...): "draw" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "kind" is not a graphical parameter

    ## Warning in text.default(...): "draw" is not a graphical parameter

    ## Warning in text.default(...): "kind" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "draw" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "kind" is not a graphical parameter

    ## Warning in text.default(...): "draw" is not a graphical parameter

    ## Warning in text.default(...): "kind" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "draw" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "kind" is not a graphical parameter

    ## Warning in text.default(...): "draw" is not a graphical parameter

    ## Warning in text.default(...): "kind" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "draw" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "kind" is not a graphical parameter

    ## Warning in text.default(...): "draw" is not a graphical parameter

    ## Warning in text.default(...): "kind" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "draw" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "kind" is not a graphical parameter

    ## Warning in text.default(...): "draw" is not a graphical parameter

    ## Warning in text.default(...): "kind" is not a graphical parameter

``` r
title(main = "Summer experiment: Farm hub")

#Mus Summer
nmds_Sum.Mus<-metaMDS(WeedSpClean_Sum.PerM.Mus[-3,], distance = "bray") #Results vary with distance metric chosen
```

    ## Square root transformation
    ## Wisconsin double standardization
    ## Run 0 stress 0.1782472 
    ## Run 1 stress 0.193622 
    ## Run 2 stress 0.1864736 
    ## Run 3 stress 0.2034275 
    ## Run 4 stress 0.2042382 
    ## Run 5 stress 0.1861512 
    ## Run 6 stress 0.1842851 
    ## Run 7 stress 0.186803 
    ## Run 8 stress 0.1842852 
    ## Run 9 stress 0.1938171 
    ## Run 10 stress 0.2170884 
    ## Run 11 stress 0.214576 
    ## Run 12 stress 0.1782472 
    ## ... New best solution
    ## ... Procrustes: rmse 3.368472e-05  max resid 0.0001381875 
    ## ... Similar to previous best
    ## Run 13 stress 0.1782472 
    ## ... Procrustes: rmse 7.963751e-05  max resid 0.0002395034 
    ## ... Similar to previous best
    ## Run 14 stress 0.1853583 
    ## Run 15 stress 0.1827315 
    ## Run 16 stress 0.2099294 
    ## Run 17 stress 0.2076218 
    ## Run 18 stress 0.1873813 
    ## Run 19 stress 0.2042448 
    ## Run 20 stress 0.2051345 
    ## *** Best solution repeated 2 times

``` r
ordiplot(nmds_Sum.Mus, type = "n")
points(nmds_Sum.Mus, cex = 1, pch = c(1,22,21,25,23)[WeedEnvClean_Sum.PerM.Mus[-3,]$CoverCrop],
       bg = cols[WeedEnvClean_Sum.PerM.Mus[-3,]$CoverCrop],
       col = cols[WeedEnvClean_Sum.PerM.Mus[-3,]$CoverCrop])
ordiellipse(nmds_Sum.Mus, groups = WeedEnvClean_Sum.PerM.Mus[-3,]$CoverCrop, draw = "polygon",
         label = TRUE, kind = "sd")
ordispider(nmds_Sum.Mus, groups = WeedEnvClean_Sum.PerM.Mus[-3,]$CoverCrop, draw = "polygon",
         lty = "dotted",col = cols[WeedEnvClean_Sum.PerM.Mus[-3,]$CoverCrop],
         label = TRUE, kind = "sd")
```

    ## Warning in match.fun(FUN)(...): "draw" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "kind" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "draw" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "kind" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "draw" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "kind" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "draw" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "kind" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "draw" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "kind" is not a graphical parameter

    ## Warning in strwidth("m", cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strwidth("m", cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strheight("x", cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strheight("x", cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strwidth(labels[i], cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strwidth(labels[i], cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strheight(labels[i], cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strheight(labels[i], cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strwidth("m", cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strwidth("m", cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strheight("x", cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strheight("x", cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strwidth(labels[i], cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strwidth(labels[i], cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strheight(labels[i], cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strheight(labels[i], cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strwidth("m", cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strwidth("m", cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strheight("x", cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strheight("x", cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strwidth(labels[i], cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strwidth(labels[i], cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strheight(labels[i], cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strheight(labels[i], cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strwidth("m", cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strwidth("m", cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strheight("x", cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strheight("x", cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strwidth(labels[i], cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strwidth(labels[i], cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strheight(labels[i], cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strheight(labels[i], cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strwidth("m", cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strwidth("m", cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strheight("x", cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strheight("x", cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strwidth(labels[i], cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strwidth(labels[i], cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in strheight(labels[i], cex = cex[i], ...): "draw" is not a graphical
    ## parameter

    ## Warning in strheight(labels[i], cex = cex[i], ...): "kind" is not a graphical
    ## parameter

    ## Warning in match.fun(FUN)(...): "draw" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "kind" is not a graphical parameter

    ## Warning in text.default(...): "draw" is not a graphical parameter

    ## Warning in text.default(...): "kind" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "draw" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "kind" is not a graphical parameter

    ## Warning in text.default(...): "draw" is not a graphical parameter

    ## Warning in text.default(...): "kind" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "draw" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "kind" is not a graphical parameter

    ## Warning in text.default(...): "draw" is not a graphical parameter

    ## Warning in text.default(...): "kind" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "draw" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "kind" is not a graphical parameter

    ## Warning in text.default(...): "draw" is not a graphical parameter

    ## Warning in text.default(...): "kind" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "draw" is not a graphical parameter

    ## Warning in match.fun(FUN)(...): "kind" is not a graphical parameter

    ## Warning in text.default(...): "draw" is not a graphical parameter

    ## Warning in text.default(...): "kind" is not a graphical parameter

``` r
title(main = "Summer experiment: Musgrave")

par(fig = c(0, 1, 0, 1), oma = c(2, 0, 4, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend(x = "bottom",inset = 0,
        legend = c("Tilled control",
                   "Canola","Cereal rye","Hairy vetch","CR x HV",
                   "Tilled control",
                   "Buckwheat","S. sudangrass","Sunn hemp","SSxSH"),
       pch = c(1,22,21,25,23,1,22,21,25,23),
       pt.bg = c("black",myCol[1:4],"black",myCol[5:8]),
       col=c("black",myCol[1:4],"black",myCol[5:8]), cex= 1, ncol=2,
       xpd = TRUE)
```

![](phylogenetic-competition_files/figure-gfm/ploting-1.png)<!-- -->

``` r
#Figure for 615 x 664
#dev.off()
```

Anderson and Walsh (2014) Note that if design is balanced, then
PERMANOVA is okay even in the face of heterogeneous dispersion *as long
as as the data is balanced*.

Results (3/20/23) - Bray-Curtis is nice because it is semi-metric,
focuses on shared species (although a case can be made for double zeros
here) - I used a log transformation to minimize the effect of dominant
species on permanova results. To deal with zeros I added 1.

- Plots with zero weeds makes it impossible to constrain permutations.
  But there wasn’t much variation within sites so this should be a
  minimal issue.
- To get homogeneous variation, i had to remove an outlier in the summer
  experiment. NMDS confirmed that it was way off

A test for homogeneity of multivariate dispersions (PERMDISP) in the
space of the chosen dissimilarity measure can be done, either to
accompany PERMANOVA or in its own right.This test compares within group
spread among groups using the average value of the distances from
individual observations to their own group centroid (Anderson 2017). If
it is unsignificant, then we have confirmation that centroid differences
drive PERMANOVA results. Not sure if it’s *valid with different sample
sizes*.

In old - Euclidean permanova with constrains gives expected results -
RDA, everything is significant

``` r
#Winter prep####
##Sites
bd_Sites.Wint<- betadisper(vegdist(WeedSpClean_Wint.PerM, method = "bray"),
                      WeedEnvClean_Wint.PerM$Site)
boxplot(bd_Sites.Wint)
```

![](phylogenetic-competition_files/figure-gfm/Testing-1.png)<!-- -->

``` r
anova(bd_Sites.Wint)
```

    ## Analysis of Variance Table
    ## 
    ## Response: Distances
    ##           Df   Sum Sq   Mean Sq F value Pr(>F)
    ## Groups     1 0.000339 0.0003395  0.0951 0.7587
    ## Residuals 73 0.260542 0.0035691

``` r
permutest(bd_Sites.Wint)
```

    ## 
    ## Permutation test for homogeneity of multivariate dispersions
    ## Permutation: free
    ## Number of permutations: 999
    ## 
    ## Response: Distances
    ##           Df   Sum Sq   Mean Sq      F N.Perm Pr(>F)
    ## Groups     1 0.000339 0.0003395 0.0951    999   0.78
    ## Residuals 73 0.260542 0.0035691

``` r
##CC
bd_CC.Wint<- betadisper(vegdist(WeedSpClean_Wint.PerM, method = "bray"),
                   WeedEnvClean_Wint.PerM$CoverCrop)
boxplot(bd_CC.Wint)
```

![](phylogenetic-competition_files/figure-gfm/Testing-2.png)<!-- -->

``` r
anova(bd_CC.Wint)
```

    ## Analysis of Variance Table
    ## 
    ## Response: Distances
    ##           Df  Sum Sq   Mean Sq F value Pr(>F)
    ## Groups     4 0.04624 0.0115611  1.8328 0.1322
    ## Residuals 70 0.44155 0.0063078

``` r
permutest(bd_CC.Wint)
```

    ## 
    ## Permutation test for homogeneity of multivariate dispersions
    ## Permutation: free
    ## Number of permutations: 999
    ## 
    ## Response: Distances
    ##           Df  Sum Sq   Mean Sq      F N.Perm Pr(>F)
    ## Groups     4 0.04624 0.0115611 1.8328    999  0.135
    ## Residuals 70 0.44155 0.0063078

``` r
##Interaction
bd_Int.Wint<- betadisper(vegdist(WeedSpClean_Wint.PerM, method = "bray"),
                         as.factor(WeedEnvClean_Wint.PerM$Site):
                           as.factor(WeedEnvClean_Wint.PerM$CoverCrop))
boxplot(bd_Int.Wint)
```

![](phylogenetic-competition_files/figure-gfm/Testing-3.png)<!-- -->

``` r
anova(bd_Int.Wint)
```

    ## Analysis of Variance Table
    ## 
    ## Response: Distances
    ##           Df Sum Sq  Mean Sq F value Pr(>F)
    ## Groups     9 0.1624 0.018044  1.0815 0.3885
    ## Residuals 65 1.0845 0.016685

``` r
permutest(bd_Int.Wint)
```

    ## 
    ## Permutation test for homogeneity of multivariate dispersions
    ## Permutation: free
    ## Number of permutations: 999
    ## 
    ## Response: Distances
    ##           Df Sum Sq  Mean Sq      F N.Perm Pr(>F)
    ## Groups     9 0.1624 0.018044 1.0815    999  0.396
    ## Residuals 65 1.0845 0.016685

``` r
#Summer prep####
##Sites
bd_Sites.Sum<- betadisper(vegdist(WeedSpClean_Sum.PerM[-23,], method = "bray"),
                      WeedEnvClean_Sum.PerM[-23,]$Site)
boxplot(bd_Sites.Sum)
```

![](phylogenetic-competition_files/figure-gfm/Testing-4.png)<!-- -->

``` r
anova(bd_Sites.Sum)
```

    ## Analysis of Variance Table
    ## 
    ## Response: Distances
    ##           Df   Sum Sq   Mean Sq F value Pr(>F)
    ## Groups     1 0.002979 0.0029787   0.823 0.3671
    ## Residuals 77 0.278682 0.0036192

``` r
permutest(bd_Sites.Sum)
```

    ## 
    ## Permutation test for homogeneity of multivariate dispersions
    ## Permutation: free
    ## Number of permutations: 999
    ## 
    ## Response: Distances
    ##           Df   Sum Sq   Mean Sq     F N.Perm Pr(>F)
    ## Groups     1 0.002979 0.0029787 0.823    999  0.369
    ## Residuals 77 0.278682 0.0036192

``` r
##CC - outlier had to be removed for equal spread
bd_CC.Sum<- betadisper(vegdist(WeedSpClean_Sum.PerM[-23,], method = "bray"),
                   WeedEnvClean_Sum.PerM[-23,]$CoverCrop)
boxplot(bd_CC.Sum)
```

![](phylogenetic-competition_files/figure-gfm/Testing-5.png)<!-- -->

``` r
anova(bd_CC.Sum)
```

    ## Analysis of Variance Table
    ## 
    ## Response: Distances
    ##           Df   Sum Sq   Mean Sq F value Pr(>F)  
    ## Groups     4 0.033854 0.0084634  2.3078 0.0659 .
    ## Residuals 74 0.271379 0.0036673                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
permutest(bd_CC.Sum)
```

    ## 
    ## Permutation test for homogeneity of multivariate dispersions
    ## Permutation: free
    ## Number of permutations: 999
    ## 
    ## Response: Distances
    ##           Df   Sum Sq   Mean Sq      F N.Perm Pr(>F)  
    ## Groups     4 0.033854 0.0084634 2.3078    999  0.065 .
    ## Residuals 74 0.271379 0.0036673                       
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
##Interaction
bd_Int.Sum<- betadisper(vegdist(WeedSpClean_Sum.PerM[-23,], method = "bray"),
                         as.factor(WeedEnvClean_Sum.PerM[-23,]$Site):
                           as.factor(WeedEnvClean_Sum.PerM[-23,]$CoverCrop))
boxplot(bd_Int.Sum)
```

![](phylogenetic-competition_files/figure-gfm/Testing-6.png)<!-- -->

``` r
anova(bd_Int.Sum)
```

    ## Analysis of Variance Table
    ## 
    ## Response: Distances
    ##           Df  Sum Sq  Mean Sq F value Pr(>F)
    ## Groups     9 0.11203 0.012448  0.7122 0.6958
    ## Residuals 69 1.20598 0.017478

``` r
permutest(bd_Int.Sum)
```

    ## 
    ## Permutation test for homogeneity of multivariate dispersions
    ## Permutation: free
    ## Number of permutations: 999
    ## 
    ## Response: Distances
    ##           Df  Sum Sq  Mean Sq      F N.Perm Pr(>F)
    ## Groups     9 0.11203 0.012448 0.7122    999  0.728
    ## Residuals 69 1.20598 0.017478

``` r
# PerManova tests####
##  Winter
adonis2(log(WeedSpClean_Wint.PerM+1) ~ Site*CoverCrop, data = WeedEnvClean_Wint.PerM) #both
```

    ## Permutation test for adonis under reduced model
    ## Permutation: free
    ## Number of permutations: 999
    ## 
    ## adonis2(formula = log(WeedSpClean_Wint.PerM + 1) ~ Site * CoverCrop, data = WeedEnvClean_Wint.PerM)
    ##          Df SumOfSqs      R2      F Pr(>F)    
    ## Model     9   10.923 0.34415 3.7898  0.001 ***
    ## Residual 65   20.817 0.65585                  
    ## Total    74   31.740 1.00000                  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#adonis2(log(WeedSpClean_Wint.PerM.FH+1) ~ CoverCrop, data = WeedEnvClean_Wint.PerM.FH)
#adonis2(log(WeedSpClean_Wint.PerM.Mus+1) ~ CoverCrop, data = WeedEnvClean_Wint.PerM.Mus)

##  Suummer
adonis2(log(WeedSpClean_Sum.PerM[-23,]+1) ~ Site*CoverCrop, data = WeedEnvClean_Sum.PerM[-23,]) #both
```

    ## Permutation test for adonis under reduced model
    ## Permutation: free
    ## Number of permutations: 999
    ## 
    ## adonis2(formula = log(WeedSpClean_Sum.PerM[-23, ] + 1) ~ Site * CoverCrop, data = WeedEnvClean_Sum.PerM[-23, ])
    ##          Df SumOfSqs      R2      F Pr(>F)    
    ## Model     9   10.359 0.31997 3.6073  0.001 ***
    ## Residual 69   22.017 0.68003                  
    ## Total    78   32.376 1.00000                  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#adonis2(log(WeedSpClean_Sum.PerM.Mus[-3,]+1) ~ CoverCrop, data = WeedEnvClean_Sum.PerM.Mus[-3,])
#adonis2(log(WeedSpClean_Sum.PerM.FH+1) ~ CoverCrop, data = WeedEnvClean_Sum.PerM.FH)

#devtools::install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
library(pairwiseAdonis)
```

    ## Loading required package: cluster

``` r
pairwise.adonis(log(WeedSpClean_Sum.PerM[-23,]+1),WeedEnvClean_Sum.PerM[-23,]$CoverCrop)
```

    ##            pairs Df SumsOfSqs   F.Model         R2 p.value p.adjusted sig
    ## 1       SS vs SH  1 0.8417324 2.1972756 0.07043165   0.017       0.17    
    ## 2       SS vs BW  1 0.8514415 1.9366830 0.06260151   0.023       0.23    
    ## 3    SS vs SSxSH  1 0.2814846 0.6861659 0.02311399   0.766       1.00    
    ## 4     SS vs Till  1 1.0927622 2.8689040 0.09002205   0.004       0.04   .
    ## 5       SH vs BW  1 1.7678071 4.5802321 0.13245232   0.001       0.01   *
    ## 6    SH vs SSxSH  1 0.4829292 1.3507223 0.04308425   0.188       1.00    
    ## 7     SH vs Till  1 0.6304371 1.9151611 0.06000788   0.042       0.42    
    ## 8    BW vs SSxSH  1 1.2509362 3.0347225 0.09186463   0.002       0.02   .
    ## 9     BW vs Till  1 1.7904415 4.6643570 0.13455772   0.001       0.01   *
    ## 10 SSxSH vs Till  1 0.9677081 2.7226745 0.08320452   0.003       0.03   .

``` r
pairwise.adonis(WeedSpClean_Wint.PerM,WeedEnvClean_Wint.PerM$CoverCrop)
```

    ##            pairs Df SumsOfSqs  F.Model         R2 p.value p.adjusted sig
    ## 1    Till vs Can  1 1.5837990 4.224948 0.12716192   0.001       0.01   *
    ## 2     Till vs HV  1 1.1541642 2.852331 0.09245107   0.003       0.03   .
    ## 3     Till vs CR  1 1.6737200 4.144977 0.12894633   0.001       0.01   *
    ## 4  Till vs CRxHV  1 1.6529580 4.094531 0.13168011   0.001       0.01   *
    ## 5      Can vs HV  1 1.3017195 3.168797 0.09850529   0.001       0.01   *
    ## 6      Can vs CR  1 1.0561340 2.576075 0.08158313   0.002       0.02   .
    ## 7   Can vs CRxHV  1 0.9037729 2.203752 0.07296287   0.008       0.08    
    ## 8       HV vs CR  1 1.0320769 2.340297 0.07713494   0.003       0.03   .
    ## 9    HV vs CRxHV  1 0.8675309 1.961476 0.06772706   0.006       0.06    
    ## 10   CR vs CRxHV  1 0.3367256 0.762842 0.02747708   0.765       1.00

# Ploting examples

``` r
ht_opt("DENDROGRAM_PADDING" = unit(1,"mm"),
       "DIMNAME_PADDING" = unit(2,"mm"), 
       "TITLE_PADDING" = unit(4,"mm"))

# Prep----
WeedSpCleanSimp<-WeedSpClean %>% 
  rownames_to_column(var = "TreatmentID") %>% 
  separate(TreatmentID,
           into = c("Experiment","Trial","Site",
                    "Year","Block","Plot","CoverCrop"),
           sep = "_") %>% 
  group_by(Experiment,CoverCrop) %>% 
  summarise(across(-matches(c("Experiment","Trial","Site","Year",
                              "Block","Plot","CoverCrop")),
                   ~ mean(.x, na.rm = TRUE))) %>% 
  ungroup()
```

    ## `summarise()` has grouped output by 'Experiment'. You can override using the
    ## `.groups` argument.

``` r
WeedSpCleanSimp <- as.data.frame(WeedSpCleanSimp)
rownames(WeedSpCleanSimp) <- paste(WeedSpCleanSimp$Experiment,WeedSpCleanSimp$CoverCrop,sep = "_")
WeedSpCleanSimp$Experiment <- NULL
WeedSpCleanSimp$CoverCrop <- NULL

AlphaAnalysisDist <- AlphaAnalysis %>% 
  group_by(Experiment,CoverCrop) %>% 
  summarise(inter.mpd =  mean(inter.mpd,na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(TreatmentID = paste(Experiment,CoverCrop,sep = "_"),
         .keep = "unused") %>% 
  column_to_rownames(var = "TreatmentID")
```

    ## `summarise()` has grouped output by 'Experiment'. You can override using the
    ## `.groups` argument.

``` r
##  WinterPrep----
WeedSpCleanSimpWint<- WeedSpCleanSimp %>% 
  rownames_to_column() %>% 
  filter(rowname %like% "Winter cover crops") %>% 
  mutate(rowname = str_replace(rowname,".*_", "")) %>% 
  column_to_rownames(var = "rowname")
WeedSpCleanSimpWint<-WeedSpCleanSimpWint[,which(colSums(WeedSpCleanSimpWint)>0)]

AlphaAnalysisDistWint <- AlphaAnalysisDist %>% 
  rownames_to_column() %>% 
  filter(rowname %like% "Winter cover crops") %>% 
  mutate(rowname = str_replace(rowname,".*_", "")) %>% 
  column_to_rownames(var = "rowname")

#Matching order and modifying row names
WeedSpCleanSimpWint<-WeedSpCleanSimpWint[order(match(rownames(WeedSpCleanSimpWint),
                                                     rownames(AlphaAnalysisDistWint))),]
WeedSpCleanSimpWint<-WeedSpCleanSimpWint[,order(match(colnames(WeedSpCleanSimpWint),
                                                      tree_boundDicot.Wint$tip.label))]
tree_boundDicot.Wint$tip.label <- str_replace(tree_boundDicot.Wint$tip.label,"_"," ")
colnames(WeedSpCleanSimpWint) <- str_replace(colnames(WeedSpCleanSimpWint),"_"," ")

DendroClustWint<-as.dendrogram(hclust(dist(AlphaAnalysisDistWint)))
plot(DendroClustWint)
```

![](phylogenetic-competition_files/figure-gfm/heatmaps-1.png)<!-- -->

``` r
DendroClustWint.reorder <- reorder(DendroClustWint, c(1,2,5,3,4))
plot(DendroClustWint.reorder)
```

![](phylogenetic-competition_files/figure-gfm/heatmaps-2.png)<!-- -->

``` r
DendroTreeWint <- as.dendrogram(as.hclust(tree_boundDicot.Wint))
plot(DendroTreeWint,horiz = TRUE,xlim = c(300,-200),ylim = c(0,50)) 
```

![](phylogenetic-competition_files/figure-gfm/heatmaps-3.png)<!-- -->

``` r
WinterAnno <- 
  AlphaAnalysis %>% 
  filter(Experiment == "Winter cover crops") %>%
  group_by(CoverCrop,Block) %>% 
  summarise(inter.mpd = mean(inter.mpd,na.rm = TRUE))
```

    ## `summarise()` has grouped output by 'CoverCrop'. You can override using the
    ## `.groups` argument.

``` r
WinterAnno<-droplevels(WinterAnno)
WinterAnno<-reshape2::acast(WinterAnno, Block~CoverCrop,value.var = "inter.mpd")

Wint_InterMPD.letters = anno_simple(trimws(interMPDLettersWint$.group),
                                    col = circlize::colorRamp2(c(0, 2), c("white", "white")) ,
                                    pch = trimws(interMPDLettersWint$.group),
                                    pt_size = unit(.45, "cm"))
Wint_InterMPD.boxplot = anno_boxplot(WinterAnno, which = "column",
                                     box_width = 0.85,ylim=c(100,275),border = FALSE,
                                     height = unit(4, "cm"),
                                     axis_param = list(gp=gpar(fontsize=10))) 
Winter_TopAnno1 = HeatmapAnnotation("Statistical group" = Wint_InterMPD.letters,
                                   show_annotation_name = FALSE)
Winter_TopAnno2 = HeatmapAnnotation("Interspecific\nMPD" = Wint_InterMPD.boxplot,
                                   annotation_name_side = "left",
                                   annotation_name_rot = 0,
                                   annotation_name_gp = gpar(fontsize=12))
#Top weeds

##  SummerPrep----
WeedSpCleanSimpSum<-WeedSpCleanSimp %>% 
  rownames_to_column() %>% 
  filter(rowname %like% "Summer cover crops") %>% 
  mutate(rowname = str_replace(rowname,".*_", "")) %>% 
  column_to_rownames(var = "rowname")
WeedSpCleanSimpSum<-WeedSpCleanSimpSum[,which(colSums(WeedSpCleanSimpSum)>0)]

AlphaAnalysisDistSum <- AlphaAnalysisDist %>% 
  rownames_to_column() %>% 
  filter(rowname %like% "Summer cover crops") %>% 
  mutate(rowname = str_replace(rowname,".*_", "")) %>% 
  column_to_rownames(var = "rowname")

#Matching order and modifying row names
WeedSpCleanSimpSum<-WeedSpCleanSimpSum[order(match(rownames(WeedSpCleanSimpSum),
                                                   rownames(AlphaAnalysisDistSum))),]
WeedSpCleanSimpSum<-WeedSpCleanSimpSum[,order(match(colnames(WeedSpCleanSimpSum),
                                                      tree_boundDicot.Sum$tip.label))]
tree_boundDicot.Sum$tip.label <- str_replace(tree_boundDicot.Sum$tip.label,"_"," ")
colnames(WeedSpCleanSimpSum) <- str_replace(colnames(WeedSpCleanSimpSum),"_"," ")

DendroClustSum<-as.dendrogram(hclust(dist(AlphaAnalysisDistSum)))
plot(DendroClustSum,ylim = c(-50,100))
```

![](phylogenetic-competition_files/figure-gfm/heatmaps-4.png)<!-- -->

``` r
DendroClustSum.reorder <- reorder(DendroClustSum, c(1,5,4,3,2))
plot(DendroClustSum.reorder,ylim = c(-50,100))
```

![](phylogenetic-competition_files/figure-gfm/heatmaps-5.png)<!-- -->

``` r
DendroTreeSum <- as.dendrogram(as.hclust(tree_boundDicot.Sum))
plot(DendroTreeSum,horiz = TRUE,xlim = c(300,-200),ylim = c(0,60)) 
```

![](phylogenetic-competition_files/figure-gfm/heatmaps-6.png)<!-- -->

``` r
SummerAnno <- 
  AlphaAnalysis %>% 
  filter(Experiment == "Summer cover crops") %>%
  group_by(CoverCrop,Block) %>% 
  summarise(inter.mpd = mean(inter.mpd,na.rm = TRUE))
```

    ## `summarise()` has grouped output by 'CoverCrop'. You can override using the
    ## `.groups` argument.

``` r
SummerAnno<-droplevels(SummerAnno)
SummerAnno<-reshape2::acast(SummerAnno, Block~CoverCrop,value.var = "inter.mpd")

Sum_InterMPD.letters = anno_simple(trimws(interMPDLettersSum$.group),
                                    col = circlize::colorRamp2(c(0, 2), c("white", "white")) ,
                                    pch = trimws(interMPDLettersSum$.group),
                                    pt_size = unit(.45, "cm"))
Sum_InterMPD.boxplot = anno_boxplot(SummerAnno, which = "column",
                                     box_width = 0.85,ylim=c(50,250),border = FALSE,
                                     height = unit(4, "cm"),
                                     axis_param = list(gp=gpar(fontsize=10))) 
Summer_TopAnno1 = HeatmapAnnotation("Statistical group" = Sum_InterMPD.letters,
                                   show_annotation_name = FALSE)
Summer_TopAnno2 = HeatmapAnnotation("Interspecific\nMPD" = Sum_InterMPD.boxplot,
                                   annotation_name_side = "left",
                                   annotation_name_rot = 0,
                                   annotation_name_gp = gpar(fontsize=12))

#Top weeds

##  Viz----
col_funWint = circlize::colorRamp2(c(0,0.0001, 300), c("white", "#D2EEEF","#a80000")) #BuRd
col_funSum = circlize::colorRamp2(c(0,0.0001, 400), c("white", "#D2EEEF","#a80000"))

### Winter----
ht_Wint<-Heatmap(t(WeedSpCleanSimpWint*20),
        cluster_rows = DendroTreeWint,
        show_row_dend = TRUE,
        cluster_columns = DendroClustWint.reorder,
        show_column_dend = TRUE,
      #labels
        row_title = "Weed species phylogeny",
        row_title_gp = gpar(fontsize = 14.5),
        show_row_names = TRUE,
        row_names_gp = gpar(fontsize = 12,fontface = "italic"),
        column_title = "Treatments clustered\nby Interspecific MPD",
        column_title_gp = gpar(fontsize = 14.5),
        show_column_names = TRUE,
        column_names_side = "top",
        column_names_rot = 90,
        column_names_centered = TRUE,
      #over all aesthetics
        rect_gp = gpar(col = "white", lwd = 1.5),
        col = col_funWint,
        heatmap_legend_param = list(title = "Weed biomass (kg/ha)",
                                    title_gp = gpar(fontsize = 12.5),
                                    direction = "horizontal",
                                    title_position = "lefttop",
                                    labels_gp = gpar(fontsize = 12)),
        row_dend_width = unit(2.5, "cm"),
        column_dend_height = unit(.75, "cm"),
        top_annotation = c(Winter_TopAnno1,Winter_TopAnno2),
        column_split = 2,
        column_gap = unit(3, "mm"))
draw(ht_Wint,heatmap_legend_side = "bottom")
```

![](phylogenetic-competition_files/figure-gfm/heatmaps-7.png)<!-- -->

``` r
### Summer----
ht_Sum<-Heatmap(t(WeedSpCleanSimpSum*20),
                 cluster_rows = DendroTreeSum,
                 show_row_dend = TRUE,
                 cluster_columns = DendroClustSum.reorder,
                 show_column_dend = TRUE,
                 #labels
                 row_title = "Weed species phylogeny",
                 row_title_gp = gpar(fontsize = 14.5),
                 show_row_names = TRUE,
                 row_names_gp = gpar(fontsize = 12,fontface = "italic"),
                 column_title = "Treatments clustered\nby Interspecific MPD",
                 column_title_gp = gpar(fontsize = 14.5),
                 show_column_names = TRUE,
                 column_names_side = "top",
                 column_names_rot = 90,
                 column_names_centered = TRUE,
                 #over all aesthetics
                 rect_gp = gpar(col = "white", lwd = 1.5),
                 col = col_funSum,
                 heatmap_legend_param = list(title = "Weed biomass (kg/ha)",
                                             title_gp = gpar(fontsize = 12.5),
                                             direction = "horizontal",
                                             title_position = "lefttop",
                                             labels_gp = gpar(fontsize = 12)),
                 row_dend_width = unit(2.5, "cm"),
                 column_dend_height = unit(.75, "cm"),
                 top_annotation = c(Summer_TopAnno1,Summer_TopAnno2))
draw(ht_Sum,heatmap_legend_side = "bottom")
```

![](phylogenetic-competition_files/figure-gfm/heatmaps-8.png)<!-- -->

``` r
ht_WintFinal <- draw(ht_Wint,heatmap_legend_side = "bottom")
```

![](phylogenetic-competition_files/figure-gfm/heatmaps-9.png)<!-- -->

``` r
ht_SumFinal <- draw(ht_Sum,heatmap_legend_side = "bottom")
```

![](phylogenetic-competition_files/figure-gfm/heatmaps-10.png)<!-- -->

``` r
library(multipanelfigure)
HeatMap <- multi_panel_figure(
   width = 290, height = 330,
   columns = 2, rows = 1)
HeatMap %<>%
  fill_panel(ht_WintFinal, column = 1) %<>%
  fill_panel(ht_SumFinal, column = 2)
```

    ## Setting row to 1
    ## Setting row to 1

``` r
#pdf("Figures (450dpi)/HeatMap/HeatMap2.pdf",width = 11.42, height = 12.99)
HeatMap
```

![](phylogenetic-competition_files/figure-gfm/heatmaps-11.png)<!-- -->

``` r
#dev.off()
```

Stuff I’ve used in presentations

``` r
### Pylo cleaning viz----
#tiff("FullTree.tiff", units="in", width=5.5, height=10, res=300)
plot(Full.tree$scenario.3, cex = 0.65)
```

![](phylogenetic-competition_files/figure-gfm/other-1.png)<!-- -->

``` r
#dev.off()

#tiff("FullTreeBound.tiff", units="in", width=5.5, height=10, res=300)
plot(Full.tree_bound$scenario.3,cex = 0.6)
```

![](phylogenetic-competition_files/figure-gfm/other-2.png)<!-- -->

``` r
#dev.off()

#tiff("FullTreeBoundUltmetric.tiff", units="in", width=5.5, height=10, res=300)
plot(Full.tree_boundDicot,cex = 0.6)
```

![](phylogenetic-competition_files/figure-gfm/other-3.png)<!-- -->

``` r
#dev.off()

## Shrestha example----
plot(phytools::pbtree(n = 10))
```

![](phylogenetic-competition_files/figure-gfm/other-4.png)<!-- -->

``` r
ExampleWeeds <- SpeciesList %>% 
  filter(species %in% c("Cerastium fontanum","Chenopodium album",
              "Stellaria media","Erigeron annuus","Taraxacum officinale"))

#all weeds  
Example.tree_Shrestha<-phylo.maker(sp.list = ExampleWeeds,
                             tree = GBOTB.extended.WP,
                             nodes = nodes.info.1.WP, scenarios = "S3")
```

    ## [1] "All species in sp.list are present on tree."

``` r
plot(Example.tree_Shrestha$scenario.3)
```

![](phylogenetic-competition_files/figure-gfm/other-5.png)<!-- -->

``` r
#Per vs annual biomass
ExampleAbu_Shrestha<-
  data.frame(Treatment = c("A","B","C","D","E"),
             WeedBiomass = c(10,2,1,8,12,
                             5,13,14,7,3),
             GroupEx = c(rep("Perennial",5),rep("Annual",5)))

ggplot(ExampleAbu_Shrestha,aes(Treatment,WeedBiomass,fill=GroupEx))+
  geom_bar(stat = "identity")+
  labs(fill = "Life cycle",y = "Percentage of weed biomass")+
  scale_y_continuous(breaks=c(0,7.5,15),
        labels=c("0 %", "50 %", "100 %"))+
  theme_bw(base_size = 16)
```

![](phylogenetic-competition_files/figure-gfm/other-6.png)<!-- -->

``` r
##  Dispersion example----

### Clustered
ExampleWeeds_Clust <- SpeciesList %>% 
  filter(species %in% c("Cerastium fontanum","Chenopodium album","Lamium purpureum",
              "Stellaria media","Erigeron annuus","Taraxacum officinale"))
Example.tree_Clust<-phylo.maker(sp.list = ExampleWeeds_Clust,
                             tree = GBOTB.extended.WP,
                             nodes = nodes.info.1.WP, scenarios = "S3")
```

    ## [1] "All species in sp.list are present on tree."

``` r
Example.tree_Clust$scenario.3$tip.label <- 
  c(" Weed spp."," Weed spp."," Cover crop",
    " Weed spp."," Weed spp."," Weed spp.")

#pdf("Figures (450dpi)/ClustTree.pdf", width=5.65, height=6.30)
plot(Example.tree_Clust$scenario.3, cex=2,edge.width=2.5)
```

![](phylogenetic-competition_files/figure-gfm/other-7.png)<!-- -->

``` r
#dev.off()

### Spread
ExampleWeeds_Spred <- SpeciesList %>% 
  filter(species %in% c("Cerastium fontanum","Chenopodium album","Digitaria ischaemum",
              "Stellaria media","Erigeron annuus","Taraxacum officinale"))
Example.tree_Spred <- phylo.maker(sp.list = ExampleWeeds_Spred,
                                  tree = GBOTB.extended.WP,
                                  nodes = nodes.info.1.WP, scenarios = "S3")
```

    ## [1] "All species in sp.list are present on tree."

``` r
Example.tree_Spred$scenario.3$tip.label <- 
  c(" Weed spp."," Weed spp."," Weed spp.",
    " Weed spp."," Weed spp."," Cover crop")

#pdf("Figures (450dpi)/SpredTree.pdf", width=5.65, height=6.30)
plot(Example.tree_Spred$scenario.3, cex=2,edge.width=2.5)
```

![](phylogenetic-competition_files/figure-gfm/other-8.png)<!-- -->

``` r
#dev.off()

### Neutral
ExampleWeeds_Netral <- SpeciesList %>% 
  filter(species %in% c("Cerastium fontanum","Chenopodium album",
              "Stellaria media","Erigeron annuus","Taraxacum officinale"))
Example.tree_Netral <- phylo.maker(sp.list = ExampleWeeds_Netral,
                                  tree = GBOTB.extended.WP,
                                  nodes = nodes.info.1.WP, scenarios = "S3")
```

    ## [1] "All species in sp.list are present on tree."

``` r
Example.tree_Netral$scenario.3$tip.label <- rep(" Weed spp.",5)


#pdf("Figures (450dpi)/NetralTree.pdf", width=5.65, height=6.30)
plot(Example.tree_Netral$scenario.3, cex=2,edge.width=2.5)
```

![](phylogenetic-competition_files/figure-gfm/other-9.png)<!-- -->

``` r
#dev.off()
```
