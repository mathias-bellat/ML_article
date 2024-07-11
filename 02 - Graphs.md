Graphic from article
================
Mathias Bellat
July, 11, 2024

**All the path and file names are personal and can no be access**

# 00 Preparation

``` r
# 00 Preparation ---------------------------------------------------------------

# Folder check
getwd()

# Set folder direction
setwd(dir="C:/your_directory")

# Clean up workspace
rm(list = ls(all.names = TRUE))

# Load packages
install.packages("pacman")
library(pacman) #Easier way of loading packages

# Specify required packages and download it if needed
pacman::p_load(dplyr, readr, ggalluvial, stringr, plyr, tibble, googlesheets4, rnaturalearth, RColorBrewer)

#Authentify for access the drive
gs4_auth() 
```

`gs4_auth()` Gave you an acess to your drive folder

``` r
#Show session infos
sessionInfo()
```

    ## R version 4.3.3 (2024-02-29 ucrt)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 10 x64 (build 19045)
    ## 
    ## Matrix products: default
    ## 
    ## 
    ## locale:
    ## [1] LC_COLLATE=French_France.utf8  LC_CTYPE=French_France.utf8   
    ## [3] LC_MONETARY=French_France.utf8 LC_NUMERIC=C                  
    ## [5] LC_TIME=French_France.utf8    
    ## 
    ## time zone: Europe/Berlin
    ## tzcode source: internal
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] knitr_1.45          RColorBrewer_1.1-3  rnaturalearth_1.0.1
    ##  [4] googlesheets4_1.1.1 tibble_3.2.1        plyr_1.8.9         
    ##  [7] stringr_1.5.1       ggalluvial_0.12.5   ggplot2_3.4.4      
    ## [10] readr_2.1.5         dplyr_1.1.4        
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] utf8_1.2.4         generics_0.1.3     class_7.3-22       KernSmooth_2.23-22
    ##  [5] stringi_1.8.3      hms_1.1.3          digest_0.6.34      magrittr_2.0.3    
    ##  [9] evaluate_0.23      grid_4.3.3         fastmap_1.1.1      cellranger_1.1.0  
    ## [13] jsonlite_1.8.8     e1071_1.7-14       DBI_1.2.2          googledrive_2.1.1 
    ## [17] httr_1.4.7         purrr_1.0.2        fansi_1.0.6        scales_1.3.0      
    ## [21] codetools_0.2-19   cli_3.6.2          rlang_1.1.3        units_0.8-5       
    ## [25] munsell_0.5.0      withr_3.0.0        yaml_2.3.8         tools_4.3.3       
    ## [29] tzdb_0.4.0         gargle_1.5.2       colorspace_2.1-0   vctrs_0.6.5       
    ## [33] R6_2.5.1           proxy_0.4-27       lifecycle_1.0.4    classInt_0.4-10   
    ## [37] fs_1.6.3           pkgconfig_2.0.3    terra_1.7-71       pillar_1.9.0      
    ## [41] gtable_0.3.4       glue_1.7.0         Rcpp_1.0.12        sf_1.0-16         
    ## [45] xfun_0.42          tidyselect_1.2.0   rstudioapi_0.15.0  htmltools_0.5.7   
    ## [49] rmarkdown_2.25     compiler_4.3.3

## 01 Import data sets

``` r
# 01 Import data sets ----------------------------------------------------------
# 01.1 Import the general infos ################################################
sheet <- gs4_find("ML in archaeology")

info <- range_read(sheet, sheet = "Metadata")
info <- as.data.frame(subset(info, is.na(info$Name) == FALSE))
```

| ID    | Name                          | Year | Authors                                                                        | Tittle                                                                                                    | Abstract                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         | Country of affiliation | Journal                                        | Open Access |
|:------|:------------------------------|-----:|:-------------------------------------------------------------------------------|:----------------------------------------------------------------------------------------------------------|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:-----------------------|:-----------------------------------------------|:------------|
| ID001 | Nguifo et al. 1997            | 1997 | Nguifo, EM; Lagrange, MS; Renaud, M; Sallantin, J                              | PLATA: An application of LEGAL, a machine learning based system, to a typology of archaeological ceramics | The authors here show that machine learning techniques can be used for designing an archaeological typology, at an early stage when the classes are not yet well defined. The program (LEGAL, LEarning with GAlois Lattice) is a machine learning system which uses a set of examples and counter-examples in order to discriminate between classes. Results show a good compatibility between the classes such as they are defined by the system and the archaeological hypotheses.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             | FRA                    | Computers and the Humanities                   | Yes         |
| ID002 | O’Sullivan and Haklay 2000    | 2000 | O’Sullivan, D; Haklay, M                                                       | Agent-based models and individualism: is the world agent-based?                                           | Agent-based models (ABMs) are an increasingly popular tool in the social sciences. This trend seems likely to continue, so that they will become widely used in geography and in urban and regional planning. We present an overview of examples of these models in the life sciences, economics, planning, sociology, and archaeology. We conclude that ABMs strongly tend towards an individualist view of the social world. This point is reinforced by closer consideration of particular examples. This discussion pays attention to the inadequacy of an individualist model of society with reference to debates in social theory. We argue that because models are closed representations of an open world it is important that institutions and other social structures be explicitly included, or that their omission be explained. A tentative explanation for the bias of ABMs is offered, based on an examination of early research in artificial intelligence and distributed artificial intelligence from which disciplines the approach is derived. Some implications of these findings are discussed. We indicate some useful research directions which are beginning to tackle the individualism issue directly. We further note that the underlying assumptions of ABMs are often hidden in the implementation details. We conclude that such models must be subject to critical examination of their assumptions, and that model builders should engage with social theory if the approach is to realise its full potential. | GBR                    | Environnement and Planning A-Economy and Space | No          |
| ID003 | Amigoni and Schiaffonati 2009 | 2009 | Amigoni, F; Schiaffonati, V                                                    | THE MINERVA SYSTEM: A STEP TOWARD AUTOMATICALLY CREATED VIRTUAL MUSEUMS                                   | The application of artificial intelligence (AI) tools to cultural heritage yields new technological solutions for the fruition of museums and art exhibitions. In this article we present the latest version of the Minerva system, able to support the organization of virtual museums by cooperating with curators to set a collection of works of art in an environment. The new version of Minerva organizes a part of the arche-ological finds belonging to the collection of the Archeological Museum of Milan. The role of Minerva is not to substitute the curators but to assist them in setting up a museum or an art exhibition. In this perspective, Minerva carries out automatically some tasks involved in museum organization and constitutes a unique system among those oriented to cultural heritage. The experimental activities have been conducted in cooperation with archeological experts who validated the results produced by Minerva.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                | ITA                    | Applied Artificial Intelligence,               | Yes         |
| ID004 | Boon et al. 2009-1            | 2009 | Boon, Paul; van Der Maaten, Laurens; Paijmans, Hans; Postma, Eric; Lange, Guus | Digital Support for Archaeology                                                                           | We describe an interdisciplinary approach in which computer scientists develop techniques to support archaeology. In the Reading Images for the Cultural Heritage ( RICH) project, a variety of methods have been developed to support archaeologists in the visualization, categorization, and characterization of archaeological objects, such as medieval glass, coins, ceramics, and seeds. The methods are based on image processing and machine learning algorithms that are tailored to the task at hand. We describe the algorithms and illustrate their application on archaeological datasets. The virtues and pitfalls of the interdisciplinary approach to archaeology are discussed.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                | NLD                    | Interdisciplinary Science Reviews              | No          |
| ID005 | Toler et al. 2010             | 2010 | Toler-Franklin, C; Brown, B; Weyrich, T; Funkhouser, T; Rusinkiewicz, S        | Multi-Feature Matching of Fresco Fragments                                                                | We present a multiple-feature approach for determining matches between small fragments of archaeological artifacts such as Bronze-Age and Roman frescoes. In contrast with traditional 2D and 3D shape matching approaches, we introduce a set of feature descriptors that are based on not only color and shape, but also normal maps. These are easy to acquire and combine high data quality with discriminability and robustness to some types of deterioration. Our feature descriptors range from general-purpose to domain-specific, and are quick to compute and match. We have tested our system on three datasets of fresco fragments, demonstrating that multi-cue matching using different subsets of features leads to different tradeoffs between efficiency and effectiveness. In particular, we show that normal-based features are more effective than color-based ones at similar computational complexity, and that 3D features are more discriminative than ones based on 2D or normals, but at higher computational cost. We also demonstrate how machine learning techniques can be used to effectively combine our new features with traditional ones. Our results show good retrieval performance, significantly improving upon the match prediction rate of state-of-the-art 3D matching algorithms, and are expected to extend to general matching problems in applications such as texture synthesis and forensics.                                                                                                   | USA                    | Acm Transactions on Graphics                   | No          |

Five first row of infos table

``` r
# 01.2 Import the basic observations ###########################################
obs <- range_read(sheet, sheet = "Base Table")
obs <- as.data.frame(subset(obs, is.na(obs$Name) == FALSE))
obs <- obs[c(1:216),]
```

| Name                           | ID    | Date                                 | Algorithms used | Best model | Archaeology Categories | Result                                                  | Input data      | Size of the dataset | Training data set | Test data set | Comment                                                                                             | Goal | Pre-trained model | …15 | …16 | …17                              |
|:-------------------------------|:------|:-------------------------------------|:----------------|:-----------|:-----------------------|:--------------------------------------------------------|:----------------|:--------------------|:------------------|:--------------|:----------------------------------------------------------------------------------------------------|:-----|:------------------|:----|:----|:---------------------------------|
| Nguifo et al. 1997             | ID001 | 1997                                 | LEGAL           | x          | 7                      | Successfulas a proof of concept, but with mixed results | Ceramics images | 332                 | x                 | x             | Classes: Sector 1–5, 7– 9, 14.                                                                      |      |                   |     |     |                                  |
| GL is not realy a ML algorithm | NA    | Not clear LEGAL system Nguifo (1994) | NA              | ID         | Archaeologies          |                                                         |                 |                     |                   |               |                                                                                                     |      |                   |     |     |                                  |
| O’Sullivan and Haklay 2000     | ID002 | 2000                                 | NULL            | NA         | x                      | No                                                      | NA              | NULL                | NULL              | NULL          | NA                                                                                                  | NA   | NA                | NA  | 1   | Surveying (and Site Prospection) |
| Amigoni and Schiaffonati 2009  | ID003 | 2009                                 | NULL            | NA         | x                      | No                                                      | NA              | NULL                | NULL              | NULL          | NA                                                                                                  | NA   | NA                | NA  | 2   | Landscape Archaeology            |
| Boon et al. 2009-1             | ID004 | 2009                                 | x               | x          | 7;16                   | No                                                      | Theory          | 30000               | 20000             | 10000         | p.188 Part of a Cultural Heritage project, with a digitisation, ML and finally dissemination phase. |      |                   |     |     |                                  |

Five first row of observation table

p.194 The author is looking for a model for visualising structures
rather than quantifying them. p.195 A study of coins with images that
take account of the ‘gradient’ with two elements: the magnitude, which
is the strength of the change in value of the pixel, and an orientation,
which is the direction of the change in value. The three phases are -
Calculation of the gradients of each image. - Similarity between the
gradients or not. - Classification based on similarities with a KNN of
n= 1, which is a very weak method. p.196 The data set is made up of
30,000 images of 15,000 parts (recto-verso) with a training set of 2/3
and 2,268 different parts corresponding to 692 different classes. 4% of
the coins were deliberately not classified. p.197 Validation with the 50
coins classified with the greatest certainty by the algo. p.198 Fairly
poor results, with an SD of 8.8 % for magnitude and 10.19 % for
orientation for the archaeological coins. p.199 Another attempt with
ceramics (996 profiles) studied with a t-Distrubutaed Stochastic
Neighbour Embeddingg (t-SNE) supposed to create visualisation clusters.
None were identified with ML. p.201 Open Boek software designed to
identify and standardise certain elements of excavation reports using
Memory-Based Learning (MBL), more specifically TI MBL 5.1 (a
decision-tree-based). p.203 The author does not believe that ML can be
used in archaeology “it is eveident to us that \[…\] in the application
(in this case archaeology) is bound to fail”. \|NA \|Not mentioned \|NA
\|3 \|Archaeological Excavation \| \|Boon et al. 2009-2 \|ID004 \|2009
\|TiMBL \|x \|6 \|Not defined \|Text data \|x \|x \|x \|p.188 Part of a
Cultural Heritage project, with a digitisation, ML and finally
dissemination phase. p.194 The author is looking for a model for
visualising structures rather than quantifying them. p.195 A study of
coins with images that take account of the ‘gradient’ with two elements:
the magnitude, which is the strength of the change in value of the
pixel, and an orientation, which is the direction of the change in
value. The three phases are - Calculation of the gradients of each
image. - Similarity between the gradients or not. - Classification based
on similarities with a KNN of n= 1, which is a very weak method. p.196
The data set is made up of 30,000 images of 15,000 parts (recto-verso)
with a training set of 2/3 and 2,268 different parts corresponding to
692 different classes. 4% of the coins were deliberately not classified.
p.197 Validation with the 50 coins classified with the greatest
certainty by the algo. p.198 Fairly poor results, with an SD of 8.8 %
for magnitude and 10.19 % for orientation for the archaeological coins.
p.199 Another attempt with ceramics (996 profiles) studied with a
t-Distrubutaed Stochastic Neighbour Embeddingg (t-SNE) supposed to
create visualisation clusters. None were identified with ML. p.201 Open
Boek software designed to identify and standardise certain elements of
excavation reports using Memory-Based Learning (MBL), more specifically
TI MBL 5.1 (a decision-tree-based). p.203 The author does not believe
that ML can be used in archaeology “it is eveident to us that \[…\] in
the application (in this case archaeology) is bound to fail”. \|NA \|Not
mentioned \|NA \|4 \|Geoarchaeology \|

``` r
# 01.3 Import the task infos ###################################################
task <- range_read(sheet, sheet = "Tasks")
task <- as.data.frame(subset(task, is.na(task$Name) == FALSE))
```

| Name                     | ID    | Date | Level 1                                | Level 2                      | Level 3                    | Level 4           | Level 5        | General task                 |
|:-------------------------|:------|-----:|:---------------------------------------|:-----------------------------|:---------------------------|:------------------|:---------------|:-----------------------------|
| Nguifo et al. 1997       | ID001 | 1997 | ceramics classification                | NA                           | Artefact Classification    | image recognition | Classification | Artefact Classification      |
| Boon et al. 2009-1       | ID004 | 2009 | x                                      | x                            | x                          | x                 | Theory         | Theory                       |
| Boon et al. 2009-2       | ID004 | 2009 | archaeological reports text extraction | text extraction              | named entities recognition | NA                | Classification | Text Extraction              |
| Toler et al. 2010        | ID005 | 2010 | fresco reconstruction                  | Architectural reconstruction | NA                         | image recognition | Classification | Architectural reconstruction |
| Barcelo and Almeida 2012 | ID006 | 2012 | x                                      | x                            | x                          | x                 | Theory         | Theory                       |

Five first row of task table

``` r
# 01.4 Import the models list ##################################################
modelsList <- range_read(sheet, sheet = "Models statistics")
modelsList <- as.data.frame(subset(modelsList, is.na(modelsList$Algorithm) == FALSE))
```

| Algorithm                    | Acronym | Time Used | Time Best | Paper type                   | …6  |  ID | Model family                 | Frequency | Frequency of models of family |
|:-----------------------------|:--------|----------:|----------:|:-----------------------------|:----|----:|:-----------------------------|----------:|------------------------------:|
| Random Forest                | RF      |        54 |        20 | Ensemble Learning            | NA  |   1 | Artificial Neural Network    |       112 |                            36 |
| Support Vector Machine       | SVM     |        26 |         2 | Linear Classifier            | NA  |   2 | Bayesian Classifier          |        15 |                             3 |
| k-nearest neighbor           | kNN     |        21 |         1 | Nearest Neighbour Classifier | NA  |   3 | Nearest Neighbour Classifier |        24 |                             2 |
| Convolutional Neural Network | CNN     |        14 |         1 | Artificial Neural Network    | NA  |   4 | Linear Classifier            |        27 |                             2 |
| ResNet                       | ResNet  |        12 |         2 | Artificial Neural Network    | NA  |   5 | Polynomial Classifier        |         7 |                             1 |

Five first row of the model table

``` r
# 01.5 Remove non reviewed papers ##############################################

# Reviewed and not reviewed papers, one papers might have several studies
not_review <- subset(obs, obs$`Archaeology Categories` == "x")
review <- subset(obs, obs$`Archaeology Categories` != "x")

# Merge with metadata
merge <- merge(info, not_review, by = "ID")

# Select papers which have several studies to not remove them in case one of their study do fit review protocol
df.1 <- merge[grep("-1", merge$Name.y, ignore.case = TRUE ),]
df.2 <- merge[grep("-2", merge$Name.y, ignore.case = TRUE ),]
df <- rbind(df.1, df.2)

# Select the papers in question and unlist them from the papers to remove
ID <- c(df$ID)
not_review <- merge[!merge$ID %in% ID, ]
head(not_review)
```

    ##      ID                        Name.x Year
    ## 1 ID002    O'Sullivan and Haklay 2000 2000
    ## 2 ID003 Amigoni and Schiaffonati 2009 2009
    ## 3 ID007          Praisler et al. 2013 2013
    ## 4 ID011       Andrew and Shepard 2017 2017
    ## 5 ID012              Drap et al. 2017 2017
    ## 6 ID014              Lowe et al. 2017 2017
    ##                                                                   Authors
    ## 1                                                O'Sullivan, D; Haklay, M
    ## 2                                             Amigoni, F; Schiaffonati, V
    ## 3                Praisler, Mirela; Domnisoru, Danieal; Domnisoru, Leonard
    ## 4                                                Andrew, ME; Shephard, JM
    ## 5                  Drap, P; Papini, O; Pruno, E; Nucciotti, M; Vannini, G
    ## 6 Lowe, DJ; Pearce, NJG; Jorgensen, MA; Kuehn, SC; Tryon, CA; Hayward, CL
    ##                                                                                                                                       Tittle
    ## 1                                                                            Agent-based models and individualism: is the world agent-based?
    ## 2                                                                    THE MINERVA SYSTEM: A STEP TOWARD AUTOMATICALLY CREATED VIRTUAL MUSEUMS
    ## 3                                         Chemometric method for the automated identification of cucuteni ceramics based on ATR-FTIR spectra
    ## 4 Semi-automated detection of eagle nests: an application of very high-resolution image data and advanced image analyses to wildlife surveys
    ## 5                             Ontology-Based Photogrammetry Survey for Medieval Archaeology: Toward a 3D Geographic Information System (GIS)
    ## 6      Correlating tephras and cryptotephras using glass compositional analyses and numerical and statistical methods: Review and evaluation
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    Abstract
    ## 1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          Agent-based models (ABMs) are an increasingly popular tool in the social sciences. This trend seems likely to continue, so that they will become widely used in geography and in urban and regional planning. We present an overview of examples of these models in the life sciences, economics, planning, sociology, and archaeology. We conclude that ABMs strongly tend towards an individualist view of the social world. This point is reinforced by closer consideration of particular examples. This discussion pays attention to the inadequacy of an individualist model of society with reference to debates in social theory. We argue that because models are closed representations of an open world it is important that institutions and other social structures be explicitly included, or that their omission be explained. A tentative explanation for the bias of ABMs is offered, based on an examination of early research in artificial intelligence and distributed artificial intelligence from which disciplines the approach is derived. Some implications of these findings are discussed. We indicate some useful research directions which are beginning to tackle the individualism issue directly. We further note that the underlying assumptions of ABMs are often hidden in the implementation details. We conclude that such models must be subject to critical examination of their assumptions, and that model builders should engage with social theory if the approach is to realise its full potential.
    ## 2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         The application of artificial intelligence (AI) tools to cultural heritage yields new technological solutions for the fruition of museums and art exhibitions. In this article we present the latest version of the Minerva system, able to support the organization of virtual museums by cooperating with curators to set a collection of works of art in an environment. The new version of Minerva organizes a part of the arche-ological finds belonging to the collection of the Archeological Museum of Milan. The role of Minerva is not to substitute the curators but to assist them in setting up a museum or an art exhibition. In this perspective, Minerva carries out automatically some tasks involved in museum organization and constitutes a unique system among those oriented to cultural heritage. The experimental activities have been conducted in cooperation with archeological experts who validated the results produced by Minerva.
    ## 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           A chemometric software application was developed in order to obtain an automatic identification of original Cucuteni ceramic artifacts and distinguish them from fake ceramic samples similar to those found on the black market. The software application is based on Principal Component Analysis (PCA), a method of artificial intelligence that allowed us to build multivariate models that can be used for the efficient spectral discrimination of genuine Cucuteni ceramic samples. The spectra obtained by Fourier Transform Infrared Spectroscopy with ATR-Attenuated Total Reflection (FTIR-ATR) for a set of Cucuteni ceramic samples discovered in archaeological sites from Moldova Romania were used as input database for the detection system.
    ## 4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            Very high-resolution (VHR) image data, including from unmanned aerial vehicle (UAV) platforms, are increasingly acquired for wildlife surveys. Animals or structures they build (e.g. nests) can be photointerpreted from these images, however, automated detection is required for more efficient surveys. We developed semi-automated analyses to map white-bellied sea eagle (Haliaeetus leucogaster) nests in VHR aerial photographs of the Houtman Abrolhos Islands, Western Australia, an important breeding site for many seabird species. Nest detection is complicated by high environmental heterogeneity at the scale of nests (similar to 1-2 m), the presence of many features that resemble nests and the variability of nest size, shape and context. Finally, the rarity of nests limits the availability of training data. These challenges are not unique to wildlife surveys and we show how they can be overcome by an innovative integration of object-based image analyses (OBIA) and the powerful machine learning one-class classifier Maxent. Maxent classifications using features characterizing object texture, geometry and neighborhood, along with limited object color information, successfully identified over 90% of high quality nests (most weathered and unusually shaped nests were also detected, but at a slightly lower rate) and labeled <2% of objects as candidate nests. Although this overestimates the occurrence of nests, the results can be visually screened to rule out all but the most likely nests in a process that is simpler and more efficient than manual photointerpretation of the full image. Our study shows that semi-automated image analyses for wildlife surveys are achievable. Furthermore, the developed strategies have broad relevance to image processing applications that seek to detect rare features differing only subtly from a heterogeneous background, including remote sensing of archeological remains. We also highlight solutions to maximize the use of imperfect or uncalibrated image data, such as some UAV-based imagery and the growing body of VHR imagery available in Google Earth and other virtual globes.
    ## 5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             This paper presents certain reflections concerning an interdisciplinary project between medieval archaeologists from the University of Florence (Italy) and computer science researchers from CNRS, National Center for Scientific Research, (France), aiming towards a connection between 3D spatial representation and archaeological knowledge. We try to develop an integrated system for archaeological 3D survey and all other types of archaeological data and knowledge by incorporating observable (material) and non-graphic (interpretive) data. Survey plays a central role, since it is both a metric representation of the archaeological site and, to a wider extent, an interpretation of it (being also a common basis for communication between the two teams). More specifically, 3D survey is crucial, allowing archaeologists to connect actual spatial assets to the stratigraphic formation processes (i.e., to the archaeological time) and to translate spatial observations into historical interpretation of the site. It is well known that laser scanner, photogrammetry and computer vision are very useful tools for archaeologists, although the integration of the representation of space, as well as archaeological time has not yet found a methodological standard of reference. We propose a common formalism for describing photogrammetric survey and archaeological knowledge stemming from ontologies: indeed, ontologies are fully used to model and store 3D data and archaeological knowledge. We equip this formalism with a qualitative representation of time, starting from archaeological stratigraphy. Stratigraphic analyses (both of excavated deposits and of upstanding structures) are closely related to Edward Cecil Harris's theory of the \\Unit of Stratigraphication\\\\ (referred to as \\\\"US\\\\", while a stratigraphic unit of an upstanding structure Unita Stratigrafica Murale, in Italian, will be referred to as \\\\"USM\\\\"). Every US is connected to the others by geometric, topological and, eventually, temporal links, and these are recorded by the 3D photogrammetric survey. However, the limitations of the Harris matrix approach led us to use another formalism for representing stratigraphic relationships, namely Qualitative Constraints Networks (QCN), which was successfully used in the domain of knowledge representation and reasoning in artificial intelligence for representing temporal relations.\\""
    ## 6 We define tephras and cryptotephras and their components (mainly ash-sized particles of glass crystals in distal deposits) and summarize the basis of tephrochronology as a chronostratigraphic correlational and dating tool for palaeoenvironmental, geological, and archaeological research. We then document and appraise recent advances in analytical methods used to determine the major, minor, and trace elements of individual glass shards from tephra or cryptotephra deposits to aid their correlation and application. Protocols developed recently for the electron probe microanalysis of major elements in individual glass shards help to improve data quality and standardize reporting procedures. A narrow electron beam (diameter similar to 3-5 gm) can now be used to analyze smaller glass shards than previously attainable. Reliable analyses of 'microshards' (defined here as glass shards < 32 gm in diameter) using narrow beams are useful for fine-grained samples from distal or ultra-distal geographic locations, and for vesicular or microlite-rich glass shards or small melt inclusions. Caveats apply, however, in the microprobe analysis of very small microshards (<=similar to 5 gm in diameter), where particle geometry becomes important, and of microlite-rich glass shards where the potential problem of secondary fluorescence across phase boundaries needs to be recognised. Trace element analyses of individual glass shards using laser ablation inductively coupled plasma-mass spectrometry (LA-ICP-MS), with crater diameters of 20 mu m and 10 mu m, are now effectively routine, giving detection limits well below 1 ppm. Smaller ablation craters (< 10 mu m) can be subject to significant element fractionation during analysis, but the systematic relationship of such fractionation with glass composition suggests that analyses for some elements at these resolutions may be quantifiable. In undertaking analyses, either by microprobe or LA-ICP-MS, reference material data acquired using the same procedure, and preferably from the same analytical session, should be presented alongside new analytical data. In part 2 of the review, we describe, critically assess, and recommend ways in which tephras or cryptotephras can be correlated (in conjunction with other information) using numerical or statistical analyses of compositional data. Statistical methods provide a less subjective means of dealing with analytical data pertaining to tephra components (usually glass or crystalsiphenocrysts) than heuristic alternatives. They enable a better understanding of relationships among the data from multiple viewpoints to be developed and help quantify the degree of uncertainty in establishing correlations. In common with other scientific hypothesis testing, it is easier to infer using such analysis that two or more tephras are different rather than the same. Adding stratigraphic, chronological, spatial, or palaeoenvironmental data (i.e. multiple criteria) is usually necessary and allows for more robust correlations to be made. A two-stage approach is useful, the first focussed on differences in the mean composition of samples, or their range, which can be visualised graphically via scatterplot matrices or bivariate plots coupled with the use of statistical tools such as distance measures, similarity coefficients, hierarchical cluster analysis (informed by distance measures or similarity or cophenetic coefficients), and principal components analysis (PCA). Some statistical methods (cluster analysis, discriminant analysis) are referred to as 'machine learning' in the computing literature. The second stage examines sample variance and the degree of compositional similarity so that sample equivalence or otherwise can be established on a statistical basis. This stage may involve discriminant function analysis (DFA), support vector machines (SVMs), canonical variates analysis (CVA), and ANOVA or MANOVA (or its two-sample special case, the Hotelling two-sample T-2 test). Randomization tests can be used where distributional assumptions such as multivariate normality underlying parametric tests are doubtful. Compositional data may be transformed and scaled before being subjected to multivariate statistical procedures including calculation of distance matrices, hierarchical cluster analysis, and PCA. Such transformations may make the assumption of multivariate normality more appropriate. A sequential procedure using Mahalanobis distance and the Hotelling two-sample T-2 test is illustrated using glass major element data from trachytic to phonolitic Kenyan tephras. All these methods require a broad range of high-quality compositional data which can be used to compare 'unknowns' with reference (training) sets that are sufficiently complete to account for all possible correlatives, including tephras with heterogeneous glasses that contain multiple compositional groups. Currently, incomplete databases are tending to limit correlation efficacy. The development of an open, online global database to facilitate progress towards integrated, high-quality tephrostratigraphic frameworks for different regions is encouraged. (C) 2017 Elsevier Ltd. All rights reserved.
    ##   Country of affiliation                                        Journal
    ## 1                    GBR Environnement and Planning A-Economy and Space
    ## 2                    ITA               Applied Artificial Intelligence,
    ## 3                    ROU       European Journal of Science and Theology
    ## 4                    AUS     Remote Sensing in Ecology and Conservation
    ## 5                    FRA                                    Geosciences
    ## 6                    NZL                     Quaternary Science Reviews
    ##   Open Access                        Name.y Date Algorithms used Best model
    ## 1          No    O'Sullivan and Haklay 2000 2000            NULL       <NA>
    ## 2         Yes Amigoni and Schiaffonati 2009 2009            NULL       <NA>
    ## 3         Yes          Praisler et al. 2013 2013            NULL       <NA>
    ## 4         Yes       Andrew and Shepard 2017 2017            NULL       <NA>
    ## 5         Yes              Drap et al. 2017 2017            NULL       <NA>
    ## 6          No              Lowe et al. 2017 2017            NULL       <NA>
    ##   Archaeology Categories Result Input data Size of the dataset
    ## 1                      x     No       <NA>                NULL
    ## 2                      x     No       <NA>                NULL
    ## 3                      x     No       <NA>                NULL
    ## 4                      x     No       <NA>                NULL
    ## 5                      x     No       <NA>                NULL
    ## 6                      x     No       <NA>                NULL
    ##   Training data set Test data set Comment Goal Pre-trained model ...15 ...16
    ## 1              NULL          NULL    <NA> <NA>              <NA>    NA     1
    ## 2              NULL          NULL    <NA> <NA>              <NA>    NA     2
    ## 3              NULL          NULL    <NA> <NA>              <NA>    NA     7
    ## 4              NULL          NULL    <NA> <NA>              <NA>    NA    11
    ## 5              NULL          NULL    <NA> <NA>              <NA>    NA    12
    ## 6              NULL          NULL    <NA> <NA>              <NA>    NA    14
    ##                                       ...17
    ## 1          Surveying (and Site Prospection)
    ## 2                     Landscape Archaeology
    ## 3 Classification (of material) and Typology
    ## 4                            Zooarchaeology
    ## 5                             Archaeobotany
    ## 6                     Cognitive Archaeology

``` r
# Create the reviewed metadata file
ID <- c(not_review$ID)
metadata <- info[!info$ID %in% ID, ]

# Show how many papers were included in the review and how many not

# Reviewed paper
nrow(metadata)
```

    ## [1] 142

``` r
# Number of studies
nrow(review)
```

    ## [1] 158

``` r
# Not reviewed paper
nrow(not_review)
```

    ## [1] 56

## 02 Basics statistics

``` r
# 02 Basics statistics ---------------------------------------------------------
# 02.1 Year of publication graph ###############################################
pub <- table(metadata$Year)
pub <- as.data.frame(pub)
colnames(pub) <- c("year","Freq")
pub$year <- as.numeric(as.character(pub$year))

# Left join df years to all years
df1 <- join(data.frame(year = 1997:2022), pub)   
```

    ## Joining by: year

``` r
df1[is.na(df1$Freq), "Freq"] <- 0

# Create the plot
plot <- ggplot(subset(df1, Freq != 0), aes(x = year, y = Freq))+
  geom_rect( xmin = 2019, xmax = 2022,  ymin = -Inf,
             ymax = Inf,  fill = "lightblue",  alpha = 0.03) +
  coord_cartesian(xlim =c(1997, 2023), ylim = c(0, 60)) +
  geom_line(color = "black", linewidth = 0.75) +  labs(x = "Years", y = paste0("Number of articles (n = ", sum(df1$Freq),")")) +
  geom_text(aes(label = round(Freq, 2)), vjust = -0.95, hjust = 0.95) +
  geom_point(shape=21, color="black", fill="#69b3a2", size=4) +
  scale_x_continuous(breaks = seq(2000, max(df1$year), by = 5)) +
  theme_classic()

# Check the plot
plot 
```

![screenshot](02 - Graphsfigures/models_year_of_publication-1.png)

``` r
# Export the plot  
ggsave("./Export/Graph/Figure.02.png", plot = plot, width = 12, height = 6, units = "in", dpi = 600)
ggsave("./Export/Graph/Figure.02.pdf", plot = plot, width = 12, height = 6, units = "in")
```

``` r
# 02.2 Country list ############################################################
countries <- table(metadata$`Country of affiliation`)
countries <- as.data.frame(countries)
countries$Var1 <- as.character(countries$Var1)

# Get world map data
world <- merge(ne_countries(), countries, by.x = "iso_a3_eh", by.y = "Var1", all.x = TRUE)

# remove antarctica
world <- world[!world$iso_a3 %in% c("ATA"),]

# Manually specify breaks for creating categories
custom_breaks <- c(1, 3, 5, 10, 20, max(na.omit(world$Freq)))  # Adjust the breaks as needed
world$categories <- cut(world$Freq, breaks = custom_breaks, include.lowest = TRUE)
red_palette <- brewer.pal(5, "Reds")

# Create the plot
plot <- ggplot() +
  geom_sf(data = world, aes(fill = categories)) +
  labs(fill =  "Number of publications \n per countires (n = 198)") +
  scale_fill_manual(values = red_palette,na.value = "white",  labels=c("< 3", "3 < 5", "5 < 10", "10 < 20"," > 20", "NAs"))  +  # Adjust the color palette as needed
  theme_void() +
  theme(legend.position = c(0.05, 0.25),  # Place legend at the bottom
    legend.justification = "left",  # Center the legend
    legend.box.just = "left",
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 12, face = "bold"))# Center the legend box

# Check the plot
plot 
```

![](02 - Graphs_figures/world-1.png)<!-- -->

``` r
# Export the plot  
ggsave("./Export/Graph/Figure_03.png", plot = plot, width = 16, height = 10, units = "in", dpi = 600)
ggsave("./Export/Graph/Figure_03.pdf", plot = plot, width = 16, height = 10, units = "in")
```

``` r
# 02.3 Most prolific authors ####################################################
# Prepare the authors dataset
authors <- as.data.frame(metadata$Authors)
authors <- authors %>% add_column(pest_matrix = authors$`metadata$Authors` %>% str_split(';', simplify = T))
authors <- as.data.frame(authors$pest_matrix)


# Create a function to remove all blank space
remove_spaces <- function(x) {
  str_replace_all(x, " ", "")
}
authors <- authors %>% mutate_all(.funs = remove_spaces)

# Split the column of authors to have only one main column
for (i in 1:length(authors)) {
  x <- as.data.frame(authors[i])
  x <- x %>% add_column(pest_matrix = x[[1]] %>% str_split(',', simplify = T))
  x <- as.data.frame(x[,2])
  authors[[i]] <- x[[1]]
}

# Create a data frame with each unique authors participation
authorsFinal <- as.data.frame(authors[[1]])
for (i in 2:length(authors)) {
  x <- as.data.frame(authors[[i]])
  colnames(x) <- colnames(authorsFinal)
  authorsFinal <- rbind(authorsFinal, x)
}

# Create a data frame with the frequencies for each authors
authorsFinal[[1]][authorsFinal[[1]] ==""] <- NA
authorsFinal <- na.omit(authorsFinal)
freq <- table(authorsFinal)
freq_df <- as.data.frame(freq)
```

|     | authors..1..      | Freq |
|:----|:------------------|-----:|
| 115 | Domínguez-Rodrigo |    9 |
| 26  | Baquedano         |    7 |
| 258 | Li                |    6 |
| 350 | Orengo            |    5 |
| 528 | Yravedra          |    5 |
| 30  | Bataille          |    4 |

Six first row of msot prolific authors. Careful Li, refers to theree
different authors

``` r
# 02.4 Number of articles for each categories ##################################
cat <- review[,c(3,6)]
cat <- cat %>% add_column(pest_matrix = cat$`Archaeology Categories` %>% str_split(';', simplify = T))
cat_full <- as.data.frame(cat$pest_matrix)
cat <- cbind(cat[,c(1:2)], cat_full)

# Create separation function
separate <- function(df, A, B) {
  for (i in 1:nrow(df)) {
    for (j in A:B) {
      y <- df[i, ]
      y[ , A:B] <- NA  # Corrected indexing for columns
      x <- df[i, j]
      y[A] <- ifelse(is.na(x), y[j], x)
      df <- rbind(df, y)
    }
  }
  return(df)
} 

# Split the column to have every values as a frequency table
hist <- separate(cat,3,5)
hist <- hist[c(nrow(cat)+1:nrow(hist)),c(1,3)]
hist$V1[hist$V1 =="No"] <- NA
hist$V1 <- str_replace_all(hist$V1, " ", "")
hist$V1[hist$V1 ==""] <- NA
hist$V1[hist$V1 =="x"] <- NA
hist <- na.omit(hist)

# Convert as number date
hist$Date <- as.numeric(hist$Date)

# Create the frenquence table
freq_table <- table(hist$Date, hist$V1)
freq_df <- as.data.frame(freq_table)
colnames(freq_df) <- c("year", "category", "Freq")

# Remove absence of data
freq_df <- freq_df[freq_df$Freq > 0,]

# Convert the date to number
freq_df$year <- as.numeric(as.character(freq_df$year))

# Convert the categories from factors to characters
freq_df$category <- as.character(freq_df$category)

# Create the conversion for the categories names
old <- na.omit(as.vector(as.character(obs$...16)))
new <- na.omit(as.vector(obs$...17))

# Apply conversion
for (i in 1:length(old)) {
  freq_df[2][freq_df[2] == old[i]] <- new[i]
}

# Create the plot
plot <- ggplot(freq_df, aes(x=year, y=Freq, fill = category)) +
  geom_bar(stat = "identity", colour="black") +
  geom_text(aes(label = Freq), vjust = 0, col = "white")+
  labs(x = "Year", y = paste0("Number of studies (n =", sum(freq_df$Freq),")"), fill = "Archaeological categories") +
  coord_cartesian(xlim =c(1997, 2022), ylim = c(0, 90)) +
  theme_bw()+
  theme(legend.position="bottom", legend.box="vertical", legend.margin=margin()) 

# Plot
plot
```

![](02 - Graphs_figures/categories%20per%20years-1.png)<!-- -->

``` r
# Export plot
ggsave("./Export/Graph/Figure_04.png", plot = plot, width = 15, height = 10, units = "in", dpi = 600)
ggsave("./Export/Graph/Figure_04.pdf", plot = plot, width = 15, height = 10, units = "in")
```

## 03 Alluvial diagram

``` r
# 03 Alluvial diagram ----------------------------------------------------------
# 03.1 Models used #############################################################

# Split the model column by semicolon
models <- as.data.frame(review[,c(1,4)])
models <- models %>% add_column(pest_matrix = models$`Algorithms used` %>% str_split(';', simplify = T))
models_full <- as.data.frame(models$pest_matrix)
models_full <- cbind(models$Name, models_full)
colnames(models_full)[colnames(models_full) == "models$Name" ] <- "Name"

# 03.2 Archaeological categories ###############################################

# Split the categories column by semicolon
cat <- review[,c(1:8)]
cat <- cat %>% add_column(pest_matrix = cat$`Archaeology Categories` %>% str_split(';', simplify = T))
cat_full <- as.data.frame(cat$pest_matrix)
cat_full <- cbind(cat[,c(1,7:8)], cat_full)
colnames(cat_full)[colnames(cat_full) == "cat$Name" ] <- "Name"

# 03.3 Tasks ###################################################################
task_full <- task[,c(1:3,8:9)]

# Remove the theory paper
task_full <- subset(task_full, task_full[,4]!= "Theory")

# 03.4 Merge ###################################################################
full <- merge(task_full, models_full, by = "Name")
full <- merge(full, cat_full, by = "Name")


# 03.5 Function to concatenate the columns #####################################
separate <- function(df, A, B) {
  for (i in 1:nrow(df)) {
    for (j in A:B) {
      y <- df[i, ]
      y[ , A:B] <- NA  # Corrected indexing for columns
      x <- df[i, j]
      y[A] <- ifelse(is.na(x), y[j], x)
      df <- rbind(df, y)
    }
  }
  return(df)
} 

# 03.6 Concatenate the archaeological categories columns #######################
full_first <- separate(full, 14, 17)
full_first <- full_first[-c(1:nrow(full)),]
full_first <- full_first[,-c(15:17)]

# Replace white spaces
full_first[,14] <- str_replace_all(full_first[,14], " ", "")
full_first[,14][full_first[,14] == ""] <- NA

# Remove absence of case
full_first <- full_first[complete.cases(full_first[,14]), ]

# Replace the number by categories
old <- na.omit(as.vector(as.character(obs[,16])))
new <- na.omit(as.vector(obs[,17]))

for (i in 1:length(old)) {
  full_first[,14][full_first[,14] == old[i]] <- new[i]
}

# 03.7 Concatenate the models columns ##########################################
full_second <- separate(full_first, 6, 11)
full_second <- full_second[-c(1:nrow(full_first)),]
full_second <- full_second[,-c(7:11)]

# Replace white spaces
full_second[,6] <- str_replace_all(full_second[,6], " ", "")
full_second[,6][full_second[,6] == ""] <- NA 

# Replace the acronyms by the model family
old <- as.vector(modelsList$Acronym)
new <- as.vector(modelsList$`Paper type`)
for (i in 1:length(old)) {
  full_second[,6][full_second[,6] == old[i]] <- new[i]
}

# Remove non ML methods
full_second[,6][full_second[,6] == "N/A"] <- NA

# Remove absence of case
final <- full_second[complete.cases(full_second[,6]), ]

# 03.8 Merge all for a frequency table #########################################
row.names(final) <- 1:nrow(final)
colnames(final) <- c("Author","ID", "Date","Evaluation","Task", "Architecture", "Result", "Input", "Categorie")
write.csv(final, "./Export/final_infos.csv", fileEncoding = "UTF-8")

# 03.9 Remove under represented tasks ##########################################
final <- as.data.frame(final)
frequency_table <- table(final$Task)
frequency_df <- as.data.frame(frequency_table)

# Define the number
x <- 10
list <- subset(frequency_df, frequency_df$Freq < x)
old <- as.vector(list$Var1)
for (i in 1:nrow(final)) {
  for (j in 1:length(old)) {
    ifelse(final[i,"Task"] == old[j], final[i,"Task"] <- "Others", final[i,"Task"] <- final[i,"Task"])
  }
}

# 03.10 Remove under represented archaeological categories ######################
frequency_table <- table(final$Categorie)
frequency_df <- as.data.frame(frequency_table)

x <- 10
list <- subset(frequency_df, frequency_df$Freq < x)
old <- as.vector(list$Var1)
for (i in 1:nrow(final)) {
  for (j in 1:length(old)) {
    ifelse(final[i,"Categorie"] == old[j], final[i,"Categorie"] <- "Others", final[i,"Categorie"] <- final[i,"Categorie"])
  }
}

# 03.11 Remove under represented ML categories #################################
frequency_table <- table(final$Architecture)
frequency_df <- as.data.frame(frequency_table)

x <- 5
list <- subset(frequency_df, frequency_df$Freq < x)
old <- as.vector(list$Var1)
for (i in 1:nrow(final)) {
  for (j in 1:length(old)) {
    ifelse(final[i,"Architecture"] == old[j], final[i,"Architecture"] <- "Others", final[i,"Architecture"] <- final[i,"Architecture"])
  }
}


# 03.12 Save and export the plot ###############################################
frequency_table <- table(final$Evaluation, final$Categorie, final$Task, final$Architecture)

# Convert the frequency table to a data frame
frequency_df <- as.data.frame(frequency_table)

frequency <- frequency_df[frequency_df$Freq > 0,]


colnames(frequency) <- c("Evaluation","Categorie","Task","Architecture","freq")
save(list = c("full","final","frequency"), file = "./Export/AlluvialGraph.RData")
rm(list = ls())

# 03.13 Plot the alluvial diagramm #############################################
load("./Export/AlluvialGraph.RData")

# Plot from Task to Architecture with Eval in background
plot <- ggplot(data = frequency,
               aes(axis1 = Task, axis2 = Architecture,  y = freq)) +
  geom_alluvium(aes(fill = Evaluation),
                curve_type = "sigmoid") +
  geom_stratum(aes(fill = Evaluation), col = "black", fill="lightgrey") +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum)), size = 4.5) +
  
  scale_x_discrete(limits = c("Task","Architecture"),
                   expand = c(0.15, 0.05)) +
  labs( fill = paste0("Counted observations (n = ", sum(frequency$freq), ")")) +
  theme_void()+
  theme(legend.position="bottom", legend.box="vertical", legend.margin=margin(), legend.text = element_text(size = 12)) 

# Plot the graph
plot
```

    ## Warning in to_lodes_form(data = data, axes = axis_ind, discern =
    ## params$discern): Some strata appear at multiple axes.

    ## Warning in to_lodes_form(data = data, axes = axis_ind, discern =
    ## params$discern): Some strata appear at multiple axes.

    ## Warning in to_lodes_form(data = data, axes = axis_ind, discern =
    ## params$discern): Some strata appear at multiple axes.

![](02 - Graphs_figures/alluvial%20diagram-1.png)<!-- -->

``` r
ggsave("./Export/Graph/Figure_05.png", plot = plot, width = 16, height = 10, units = "in", dpi = 600)
ggsave("./Export/Graph/Figure_05.pdf", plot = plot, width = 16, height = 10, units = "in")
```

``` r
# Plot from Task to Archaeological fields with Eval in background
plot <- ggplot(data = frequency,
               aes(axis1 = Task, axis2 = Categorie,  y = freq)) +
  geom_alluvium(aes(fill = Evaluation),
                curve_type = "sigmoid") +
  geom_stratum(aes(fill = Evaluation), col = "black", fill="lightgrey") +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum)), size = 4.5) +
  
  scale_x_discrete(limits = c("Task","Categories"),
                   expand = c(0.15, 0.05)) +
  labs( fill = paste0("Counted observations (n = ", sum(frequency$freq), ")")) +
  theme_void()+
  theme(legend.position="bottom", legend.box="vertical", legend.margin=margin(), legend.text = element_text(size = 12)) 

# Plot the graph
plot
```

    ## Warning in to_lodes_form(data = data, axes = axis_ind, discern =
    ## params$discern): Some strata appear at multiple axes.

    ## Warning in to_lodes_form(data = data, axes = axis_ind, discern =
    ## params$discern): Some strata appear at multiple axes.

    ## Warning in to_lodes_form(data = data, axes = axis_ind, discern =
    ## params$discern): Some strata appear at multiple axes.

![](02 - Graphs_figures/alluvial%20diagram%20bis-1.png)<!-- -->

``` r
ggsave("./Export/Graph/Figure_06.png", plot = plot, width = 16, height = 10, units = "in", dpi = 600)
ggsave("./Export/Graph/Figure_06.pdf", plot = plot, width = 16, height = 10, units = "in")
```

# 03.1 Additionnal alluvial diagramm

``` r
plot <- ggplot(data = frequency,
       aes(axis1 = Task, axis2 = Evaluation, axis3 = Architecture, y = freq)) +
  geom_alluvium(aes(fill = Categorie),
               curve_type = "sigmoid") +
  geom_stratum(aes(fill = Categorie), col = "black", fill="lightgrey") +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum)), size = 4.5) +
  
scale_x_discrete(limits = c("Task","Architecture","Evaluation"),
                 expand = c(0.15, 0.05)) +
  labs( fill = paste0("Archaeological categories (n = ", sum(frequency$freq), ")")) +
  theme_void()+
  theme(legend.position="bottom", legend.box="vertical", legend.margin=margin(), legend.text = element_text(size = 12)) 

plot
```

    ## Warning in to_lodes_form(data = data, axes = axis_ind, discern =
    ## params$discern): Some strata appear at multiple axes.

    ## Warning in to_lodes_form(data = data, axes = axis_ind, discern =
    ## params$discern): Some strata appear at multiple axes.

    ## Warning in to_lodes_form(data = data, axes = axis_ind, discern =
    ## params$discern): Some strata appear at multiple axes.

![](02 - Graphs_figures/alluvial%20diagram%20optional-1.png)<!-- -->
