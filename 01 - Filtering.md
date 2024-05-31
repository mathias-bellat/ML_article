**All the path and file names are personal and can no be access**

# 00 Preparation

    # 00 Preparation ---------------------------------------------------------------

    # Folder check
    getwd()

    # Clean up workspace
    rm(list = ls(all.names = TRUE))

    # Load packages
    install.packages("pacman")
    library(pacman) #Easier way of loading packages

    # Specify required packages and download it if needed
    pacman::p_load(dplyr, tm, tidyr, tibble, stringr) 

    #Show session infos
    sessionInfo()

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
    ## [1] tibble_3.2.1  stringr_1.5.1 tidyr_1.3.0   tm_0.7-11     NLP_0.2-1    
    ## [6] dplyr_1.1.4  
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] vctrs_0.6.5       slam_0.1-50       cli_3.6.2         knitr_1.45       
    ##  [5] rlang_1.1.3       xfun_0.42         stringi_1.8.3     purrr_1.0.2      
    ##  [9] generics_0.1.3    glue_1.7.0        htmltools_0.5.7   fansi_1.0.6      
    ## [13] rmarkdown_2.25    evaluate_0.23     fastmap_1.1.1     yaml_2.3.8       
    ## [17] lifecycle_1.0.4   compiler_4.3.3    Rcpp_1.0.12       pkgconfig_2.0.3  
    ## [21] rstudioapi_0.15.0 digest_0.6.34     R6_2.5.1          tidyselect_1.2.0 
    ## [25] utf8_1.2.4        pillar_1.9.0      parallel_4.3.3    magrittr_2.0.3   
    ## [29] tools_4.3.3       xml2_1.3.6

# 01 Convert PDF to .TXT

You can download the *xpdf* app at the following
[link](http://www.xpdfreader.com/). Version *xpdf.4.04* was used.

    # 01 Convert PDF ---------------------------------------------------------------
    # 01.1 Select the path and the files

    #Select the location of PDFs files
    path  <- "./Data/pdf_full" 

    #Make a vector of PDFs in the folder
    files <- list.files(path = "./Data/pdf_full/", pattern = "pdf")   

    #Path for XPDF tool accessible at (https://www.xpdfreader.com/download.html)
    xpdf <- "C:/Program Files/xpdf-tools-win-4.04/bin64/pdftotext.exe"  

    # 01.2 Transform and export in the same folder as the pdf
    for (i in 1:length(files)){
      pdf <- file.path(path, files[i])
      system(paste("\"",xpdf,"\" \"", pdf, "\"", sep =""), wait = FALSE)
      }

    rm(list=ls())

# 02 Prepare the txt files

    # 02 Prepare the txt files ---------------------------------------------------------------
    # 02.1 Import the data under Corpus file
    corpus <- VCorpus(DirSource(directory = "G:/OneDrive/Ecole/Articles/Article_ML/R_text_analysis/Data/txt_full/",
                                pattern = ".txt"))  #Corpus from the package tm

    # 02.2 Clean the elements
    df <- corpus

    # Function to remove element and replace it by blank space
    toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

    # Remove some characters
    df <- tm_map(df, toSpace, "/")
    df <- tm_map(df, toSpace, "@")
    df <- tm_map(df, toSpace, "\\|")

    # Put all tall letters to smaller
    df <- tm_map(df, content_transformer(tolower)) 

    # Remove numbers
    df <- tm_map(df, removeNumbers)
    df <- tm_map(df, removePunctuation)
    df <- tm_map(df, stripWhitespace)

    # 02.3 Put into data frame and export

    # Create a data frame with text and the ID of text(file name)
    data <- data.frame(id=sapply(df, meta, "id"),
                         text=unlist(lapply(sapply(df, '[', "content"),paste,collapse="\n")),
                         stringsAsFactors=FALSE) 

    # Remove page number
    data$text <- gsub("[\r\n]", "", data$text)

    # Write the table
    write.table(data, "G:/OneDrive/Ecole/Articles/Article_ML/R_text_analysis/Export/pre_process/export_data.txt", sep = ";", row.names = FALSE , col.names = FALSE)

    # 02.4 Remove reference part

    # For separating the different part of the file and removing the "reference part"
    remove_references <- function(x){
      z <- as.data.frame(x[,1]) 
      z$text <-x[,2]
      z$check <- grepl("introduction", z$text)    # Look for introduction word
      
      z$number <- ifelse(z$check == TRUE, z$number <- 3 , z$number <- 0)
      
      y <- z
      y$check <- grepl("abstract", y$text)    # Look for abstract word
      y$number <- ifelse(y$check == TRUE, y$number <- 2, y$number <- 0 )
      z$number <- ifelse(z$number == 3, z$number <- 3, z$number <- y$number)
      
      y$check <- grepl("doi", y$text)     # Look for doi mention
      y$number <- ifelse(y$check == TRUE, y$number <- 1, y$number <- 0 )
      z$number <- ifelse(z$number > 0, z$number <- z$number, z$number <- y$number)
      
      # Separate
      z$intro <- ifelse(z$number == 3, z$intro <- regmatches(z$text, gregexpr("(?<=introduction).*?(?=references)", z$text, perl=TRUE)), z$intro <- NA)
      z$abs <- ifelse(z$number >= 2, z$abs <- regmatches(z$text, gregexpr("(?<=abstract).*?(?=references)", z$text, perl=TRUE)), z$abs <- NA)
      z$doi <- ifelse(z$number >= 1, z$doi <- regmatches(z$text, gregexpr("(?<=doi).*?(?=references)", z$text, perl=TRUE)), z$doi <- NA)
     return(z)
    }

    clean <-  remove_references(data)

    #Remove the ".pdf" old information to remove any issues with further analysis
    clean[,1]<- gsub(".pdf", "", clean[,1]) 
    colnames(clean) <- c("Id", "text", "check", "number", "intro",  "abs", "doi")

    # 02.5 Export the elements
    save(list = c("clean", "corpus"), file = "G:/OneDrive/Ecole/Articles/Article_ML/R_text_analysis/Export/pre_process/Pre-process.RData")
    rm(list=ls())

# 03 Combine the different elements

    # 03 Combine the different elements  ---------------------------------------------------------------
    load("G:/OneDrive/Ecole/Articles/Article_ML/R_text_analysis/Export/pre_process/Pre-process.RData")

    intro <- subset(clean, clean$number == 3)
    abs <- subset(clean, clean$number == 2)
    doi <- subset(clean, clean$number == 1)

    #Text can be seen as full but only filled with "character(0)" string

    # 03.1 Clean the Introduction selection
    intro$char <- intro$intro == "character(0)"  
    intro_final <- subset(intro, char == FALSE)

    intro <- subset(intro, char == TRUE)
    intro$char <- intro$abs == "character(0)"
    abs_final <- subset(intro, char == FALSE)
    abs_final$number <- 2

    intro <- subset(intro, char == TRUE)
    intro$char <- intro$doi == "character(0)"
    doi_final <- subset(intro, char == FALSE)
    doi_final$number <- 1

    no_resume <- subset(intro, char == TRUE)
    no_resume$number <- 0

    # 03.2 Clean the abstract selection
    abs$char <- abs$abs == "character(0)"
    abs_final2 <- subset(abs, char == FALSE)

    abs <- subset(abs, char == TRUE)
    abs$char <- abs$doi == "character(0)"
    doi_final2 <- subset(abs, char == FALSE)
    doi_final2$number <- 1

    no_resume2 <- subset(abs, char == TRUE)
    no_resume2$number <- 0

    # 03.3 Clean the selection without abstract and introduction
    doi$char <- doi$doi == "character(0)"
    doi_final3 <- subset(doi, char == FALSE)

    no_resume3 <- subset(doi, char == TRUE)
    no_resume3$number <- 0

    # 03.4 Merge all the data

    names <- c("Id", "Full_text", "Number","Removed_ref")

    intro <- intro_final[,c(1:2,4:5)] #merge the introduction selections 
    colnames(intro) <- names

    abs <- rbind(abs_final, abs_final2) #merge the abstract selections 
    abs <- abs [,c(1:2,4,6)]
    colnames(abs) <- names

    doi <- rbind(doi_final, doi_final2, doi_final3) #merge the selections without abstract and introduction
    doi <- doi [,c(1:2,4,7)]
    colnames(doi) <- names

You might face some issue with articles were “abstract” “introduction”
or “references” word are absent or embedded with others words, maybe due
to the `grepx` function.

    # Merge the txt files which cannot be treated
    not_treated <- rbind(no_resume, no_resume2 , no_resume3)
    not_treated <- not_treated [,c(1:2,4)]

    # Look if a references part as not been splited from an article
    no_doi <- subset(clean, is.na(clean$doi) == TRUE) 
    no_doi <- no_doi [,c(1:2,4)]

    not_treated <- rbind(not_treated, no_doi)
    colnames(not_treated) <- c ("Id", "Full_text", "Number") 

    final_data <- rbind(intro, abs, doi) #Combine all the data in one data frame


    write.table(not_treated$Id, "G:/OneDrive/Ecole/Articles/Article_ML/R_text_analysis/Export/pre_process/not_treated.txt", sep = ";", row.names = FALSE , col.names = FALSE)

You will need to check files which are export as *not\_treated.txt* and
remove manually the elements that might perturb the automatic selection.
Afterwards you have to repeat steps from 2.1 until 2.4 which the re
uploaded *.txt* files. Here the name of the cleaned file is
`re_uploaded`

    # 03.5 Add the articles with embedded or no references part (after cleaning the .txt file)
    new_ref <- subset(clean, clean$Id %in% re_uploaded$id)
    new_ref <- data.frame(Id = new_ref$Id,
                       Full_text = new_ref$text,
                       Number = 0,
                       Removed_ref = re_uploaded$$text)
    final_data <- rbind(final_data, new_ref)

    # Update the non referenced files
    not_treated <- not_treated[!(not_treated$Id %in% new_ref$Id),]

    # Save the pre-process step
    save(list = c("clean", "corpus", "final_data", "not_treated"), file = "G:/OneDrive/Ecole/Articles/Article_ML/R_text_analysis/Export/pre_process/Filtered_references.RData")
    rm(list=ls())

# 04 First filter

The *Metadata* were collected from the information available from the
DOI and imports from the different web libraries.

    # 04 First filter ---------------------------------------------------------------
    load("G:/OneDrive/Ecole/Articles/Article_ML/R_text_analysis/Export/pre_process/Filtered_references.RData")

    # 04.1 Import and clean the metadata
    Metadata <- read.csv("G:/OneDrive/Ecole/Articles/Article_ML/R_text_analysis/Data/Metadata.csv", sep =",", na.strings = "NA")
    Metadata$File.Attachments[Metadata$File.Attachments == ""] <-NA
    Metadata <- Metadata %>% drop_na(File.Attachments) #Remove the ones without Pdfs
    Test <- Metadata  %>% add_column(pest_matrix = Metadata$File.Attachments %>% str_split("\\\\", simplify = T))

    # 04.2 Split the column to only kept the file name
    x <- as.data.frame(Test[,88])
    x[x == ""] <-NA

    for (i in 1:nrow(x)) {
      for (j in 1:length(x[i,])) {
        y <- x[i,j]
        ifelse(is.na(y) =="TRUE", y <- z, z <- y)
        x[i,1] <- y
      }
    }
    x[,1] <- gsub(";", "", x[,1])
    x[,1] <- gsub(".pdf.pdf", ".txt", x[,1])
    x[,1] <- gsub(".pdf", ".txt", x[,1])
    Metadata$File.Attachments <- x[,1]


    # 04.3 Filter only Articles from scientific journals
    Metadata <- subset(Metadata, Metadata$Item.Type == "journalArticle") #Select only journal articles
    test <- subset(final_data, final_data$Id %in% Metadata$File.Attachments)#Select in the data set journal articles

    write.table(Metadata, "G:/OneDrive/Ecole/Articles/Article_ML/R_text_analysis/Data/Metadata_clean.csv", sep = "\t", row.names = FALSE , col.names = FALSE)


    # 04.4 Function for filtering the data
    filter_func <- function(x, z){
      
      list_archeo <- c("archaeology", "archeology", "archaeological", "archeological") #List of archaeological words related
      list_ML <- c("machine learning", "ml", "deep learning", "artificial intelligence") #List of ML words related
      
      #Filter for the archaeological words
      x$check <- grepl(list_archeo[1], z)
      x$number <- ifelse(x$check == TRUE, x$number <- 1 , x$number <- 0)
      x$archeo_value <- x$number
      
      x$check <- grepl(list_archeo[2], z)
      x$number <- ifelse(x$check == TRUE, x$number <- 1 , x$number <- 0)
      x$archeo_value <- x$archeo_value + x$number
      
      x$check <- grepl(list_archeo[3], z)
      x$number <- ifelse(x$check == TRUE, x$number <- 1 , x$number <- 0)
      x$archeo_value <- x$archeo_value + x$number
      
      x$check <- grepl(list_archeo[4], z)
      x$number <- ifelse(x$check == TRUE, x$number <- 1 , x$number <- 0)
      x$archeo_value <- x$archeo_value + x$number

      
      #Filter for the machine learning words
      x$check <- grepl(list_ML[1], z)
      x$number <- ifelse(x$check == TRUE, x$number <- 1 , x$number <- 0)
      x$ML_value <- x$number
      
      x$check <- grepl(list_ML[2], z)
      x$number <- ifelse(x$check == TRUE, x$number <- 1 , x$number <- 0)
      x$ML_value <- x$ML_value + x$number
      
      x$check <- grepl(list_ML[3], z)
      x$number <- ifelse(x$check == TRUE, x$number <- 1 , x$number <- 0)
      x$ML_value <- x$ML_value + x$number
      
      x$check <- grepl(list_ML[4], z)
      x$number <- ifelse(x$check == TRUE, x$number <- 1 , x$number <- 0)
      x$ML_value <- x$ML_value + x$number

      x$filtered <- ifelse(x$archeo_value > 0 & x$ML_value > 0, x$filtered <- 1, x$filtered <- 0)  

      return(x)
    }

    # 04.5 Apply the filtering to the articles without references part
    first_filtered_data <- filter_func(final_data, final_data$Removed_ref)

    # 04.6 Subset the different results
    #subset the article without archeo keywords
    no_archeo <- subset(first_filtered_data, first_filtered_data$archeo_value == 0)

    #subset the article without ML keywords
    no_ML <- subset(first_filtered_data, first_filtered_data$ML_value == 0)

    #subset the article without either archeo or ML keywords
    no_combined <- subset(first_filtered_data, first_filtered_data$filtered == 0)

    #subset the article with archeo AND ML keywords
    first_filtered_data <- subset(first_filtered_data, first_filtered_data$filtered == 1)

    # 04.7 Export the resuts
    write.table(no_archeo[1], "G:/OneDrive/Ecole/Articles/Article_ML/R_text_analysis/Export/first_filter/first_filter_no_archeo.csv", sep = "\t", row.names = FALSE , col.names = FALSE)
    write.table(no_ML[1], "G:/OneDrive/Ecole/Articles/Article_ML/R_text_analysis/Export/first_filter/first_filter_no_ML.csv", sep = "\t", row.names = FALSE , col.names = FALSE)
    write.table(no_combined[1], "G:/OneDrive/Ecole/Articles/Article_ML/R_text_analysis/Export/first_filter/first_filter_no_combined.csv", sep = "\t", row.names = FALSE , col.names = FALSE)
    write.table(first_filtered_data[1], "G:/OneDrive/Ecole/Articles/Article_ML/R_text_analysis/Export/first_filter/first_filtered.csv", sep = "\t", row.names = FALSE , col.names = FALSE)

    save(list = c("final_data", "filter_func", "first_filtered_data","no_combined"), file = "G:/OneDrive/Ecole/Articles/Article_ML/R_text_analysis/Export/first_filter/First_filter.RData")
    rm(list=c(ls()))

# 05 Second filter

    # 05 Second filter ---------------------------------------------------------------
    load("G:/OneDrive/Ecole/Articles/Article_ML/R_text_analysis/Export/first_filter/First_filter.RData")

    # 05.1 Import the metadata file
    Metadata <- read.csv("G:/OneDrive/Ecole/Articles/Article_ML/R_text_analysis/Data/Metadata_clean.csv", sep="\t", na.strings = "NA", header = FALSE)

    # 05.2 Filter the metadata
    Metadata_first_filter <- subset(Metadata, Metadata$V38 %in% first_filtered_data$Id)  #Export the metadata from the first filter
    write.table(Metadata_first_filter, "G:/OneDrive/Ecole/Articles/Article_ML/R_text_analysis/Export/first_filter/Metadata_first_filter.csv", sep = "\t", row.names = FALSE , col.names = FALSE)


    # 05.3 Merge metadata and first filtered ones
    second_filtered_data <- merge(first_filtered_data, Metadata, by.x = "Id", by.y = "V38")

    #Only select articles with an abstract
    second_filtered_data <- subset(second_filtered_data, is.na(second_filtered_data$V11) == FALSE) 

    # 05.4 Filter the abstract
    x <- second_filtered_data
    z <- x$V11

    #Function filtering the data for the abstract
    abstract_filtered_data <- filter_func(x, z)

    #subset the article without archeo keywords
    no_archeo_abstract <- subset(abstract_filtered_data, abstract_filtered_data$archeo_value == 0)

    #subset the article without ML keywords
    no_ML_abstract <- subset(abstract_filtered_data, abstract_filtered_data$ML_value == 0)

    #subset the article without either archeo or ML keywords
    no_combined_abstract <- subset(abstract_filtered_data, abstract_filtered_data$filtered == 0)

    #subset the article with archeo AND ML keywords
    abstract_filtered_data <- subset(abstract_filtered_data, abstract_filtered_data$filtered == 1)

    # 05.5 Export the results
    write.table(no_archeo_abstract[1], "G:/OneDrive/Ecole/Articles/Article_ML/R_text_analysis/Export/second_filter/Abstract_filtered_no_archeo.csv", sep = "\t", row.names = FALSE , col.names = FALSE)
    write.table(no_ML_abstract[1], "G:/OneDrive/Ecole/Articles/Article_ML/R_text_analysis/Export/second_filter/Abstract_filtered_no_ML.csv", sep = "\t", row.names = FALSE , col.names = FALSE)
    write.table(no_combined_abstract[1], "G:/OneDrive/Ecole/Articles/Article_ML/R_text_analysis/Export/second_filter/Abstract_filtered_no_combined.csv", sep = "\t", row.names = FALSE , col.names = FALSE)
    write.table(abstract_filtered_data[1], "G:/OneDrive/Ecole/Articles/Article_ML/R_text_analysis/Export/second_filter/Abstract_filtered.csv", sep = "\t", row.names = FALSE , col.names = FALSE)

    save(list = c("no_archeo_abstract", "filter_func",  "no_combined_abstract", "no_ML_abstract", "abstract_filtered_data"), file = "G:/OneDrive/Ecole/Articles/Article_ML/R_text_analysis/Export/second_filter/Abstract_filtered.RData")

    rm(list=c("no_archeo_abstract", "no_combined_abstract", "no_ML_abstract"))

    # 05.6 Filter the title
    x <- second_filtered_data
    x$V5 <- na.omit(x$V5)
    z <- x$V5

    #Function filtering the data
    title_filtered_data <- filter_func(x, z)

    #subset the article without archeo keywords
    no_archeo_title <- subset(title_filtered_data, title_filtered_data$archeo_value == 0)

    #subset the article without ML keywords
    no_ML_title <- subset(title_filtered_data, title_filtered_data$ML_value == 0)

    #subset the article without either archeo or ML keywords
    no_combined_title <- subset(title_filtered_data, title_filtered_data$filtered == 0)

    #subset the article with archeo AND ML keywords
    title_filtered_data <- subset(title_filtered_data, title_filtered_data$filtered == 1)

    # 05.7 Export the results
    write.table(no_archeo_title[1], "G:/OneDrive/Ecole/Articles/Article_ML/R_text_analysis/Export/second_filter/Title_filtered_no_archeo.csv", sep = "\t", row.names = FALSE , col.names = FALSE)
    write.table(no_ML_title[1], "G:/OneDrive/Ecole/Articles/Article_ML/R_text_analysis/Export/second_filter/Title_filtered_no_ML.csv", sep = "\t", row.names = FALSE , col.names = FALSE)
    write.table(no_combined_title[1], "G:/OneDrive/Ecole/Articles/Article_ML/R_text_analysis/Export/second_filter/Title_filtered_no_combined.csv", sep = "\t", row.names = FALSE , col.names = FALSE)
    write.table(title_filtered_data[1], "G:/OneDrive/Ecole/Articles/Article_ML/R_text_analysis/Export/second_filter/Title_filtered.csv", sep = "\t", row.names = FALSE , col.names = FALSE)
    save(list = c("no_archeo_title", "no_ML_title", "no_combined_title", "title_filtered_data"), file = "G:/OneDrive/Ecole/Articles/Article_ML/R_text_analysis/Export/second_filter/Title_filtered.RData")

    rm(list=c("no_archeo_title", "no_ML_title", "no_combined_title","x","z"))


    # 05.8 Combine the title and abstract filters
    second_filtered_data <- rbind(title_filtered_data, abstract_filtered_data)
    second_filtered_data <- second_filtered_data[c(1:5)]
    second_filtered_data <- distinct(second_filtered_data) #Remove duplicates

    write.table(second_filtered_data[1], "G:/OneDrive/Ecole/Articles/Article_ML/R_text_analysis/Export/second_filter/second_filtered.txt", sep = ";", row.names = FALSE , col.names = FALSE)

    # 05.9 Import the metadata
    Metadata <- read.csv("G:/OneDrive/Ecole/Articles/Article_ML/R_text_analysis/Data/Metadata_clean.csv", sep="\t", na.strings = "Na", header = FALSE)


    # 05.10 Export the metadata with the second filter
    Metadata_second_filter <- merge(Metadata, second_filtered_data[1:2], by.x = "V38", by.y = "Id", all = FALSE)
    write.table(Metadata_second_filter, "G:/OneDrive/Ecole/Articles/Article_ML/R_text_analysis/Export/second_filter/Metadata_second_filter.txt", sep = ";", row.names = FALSE , col.names = FALSE)
