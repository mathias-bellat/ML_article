####################################################################
# This script is for Filtering the pdf files extracted for         #
# statistical analysis                                             #                                                         
#                                                                  #                                                  
# Author: Mathias Bellat                                           #
# Affiliation : Tuebingen University                               #
# Creation date : 12/02/2024                                       #
# E-mail: mathias.bellat@uni-tubingen.de                           #
####################################################################


# 00 Preparation ---------------------------------------------------------------

# Folder check
getwd()

# Set folder direction
setwd(dir="")

# Clean up workspace
rm(list = ls(all.names = TRUE))

# Load packages
install.packages("pacman")
library(pacman) #Easier way of loading packages
pacman::p_load(dplyr, readr, ggalluvial, stringr, plyr, tibble, googlesheets4, rnaturalearth, RColorBrewer) # Specify required packages and download it if needed

#Authentify for access the drive
gs4_auth() 

#Show session infos
sessionInfo()

# 01 Import data sets ----------------------------------------------------------
# 01.1 Import the general infos ################################################
sheet <- gs4_find("ML in archaeology")

info <- range_read(sheet, sheet = "Metadata")
info <- as.data.frame(subset(info, is.na(info$Name) == FALSE))
head(info)

# 01.2 Import the basic observations ###########################################
obs <- range_read(sheet, sheet = "Base Table")
obs <- as.data.frame(subset(obs, is.na(obs$Name) == FALSE))
obs <- obs[c(1:216),]
head(obs)

# 01.3 Import the task infos ###################################################
task <- range_read(sheet, sheet = "Tasks")
task <- as.data.frame(subset(task, is.na(task$Name) == FALSE))
head(task)

# 01.4 Import the models list ##################################################
modelsList <- range_read(sheet, sheet = "Models statistics")
modelsList <- as.data.frame(subset(modelsList, is.na(modelsList$Algorithm) == FALSE))
head(modelsList)

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

# Create the reviewed metadata file
ID <- c(not_review$ID)
metadata <- info[!info$ID %in% ID, ]

# Show how many papers were included in the review and how many not

# Reviewed paper
nrow(metadata)
# Number of studies
nrow(review)
# Not reviewed paper
nrow(not_review)

# 02 Basics statistics ---------------------------------------------------------
# 02.1 Year of publication graph ###############################################
pub <- table(metadata$Year)
pub <- as.data.frame(pub)
colnames(pub) <- c("year","Freq")
pub$year <- as.numeric(as.character(pub$year))



df1 <- join(data.frame(year = 1997:2022), pub)  #Left join df years to all years 
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

# Export the plot  
ggsave("./Export/Graph/Figure.02.png", plot = plot, width = 12, height = 6, units = "in", dpi = 600)
ggsave("./Export/Graph/Figure.02.pdf", plot = plot, width = 12, height = 6, units = "in")

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

# Export the plot  
ggsave("./Export/Graph/Figure_03.png", plot = plot, width = 16, height = 10, units = "in", dpi = 600)
ggsave("./Export/Graph/Figure_03.pdf", plot = plot, width = 16, height = 10, units = "in")

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

# Show the first 6 more prolific authors, careful "Li" refers to three different authors
head(freq_df[order(-freq_df$Freq), ])

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
hist <- separate(cat,3,6)
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

# Export plot
ggsave("./Export/Graph/Figure_04.png", plot = plot, width = 15, height = 10, units = "in", dpi = 600)
ggsave("./Export/Graph/Figure_04.pdf", plot = plot, width = 15, height = 10, units = "in")

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

ggsave("./Export/Graph/Figure_05.png", plot = plot, width = 16, height = 10, units = "in", dpi = 600)
ggsave("./Export/Graph/Figure_05.pdf", plot = plot, width = 16, height = 10, units = "in")


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

ggsave("./Export/Graph/Figure_06.png", plot = plot, width = 16, height = 10, units = "in", dpi = 600)
ggsave("./Export/Graph/Figure_06.pdf", plot = plot, width = 16, height = 10, units = "in")

# 03.14 Additional for a full alluvial diagramm ###############################

#plot <- ggplot(data = frequency,
#       aes(axis1 = Task, axis2 = Evaluation, axis3 = Architecture, y = freq)) +
#  geom_alluvium(aes(fill = Categorie),
#               curve_type = "sigmoid") +
#  geom_stratum(aes(fill = Categorie), col = "black", fill="lightgrey") +
#  geom_text(stat = "stratum",
#            aes(label = after_stat(stratum)), size = 4.5) +
  
#scale_x_discrete(limits = c("Task","Architecture","Evaluation"),
#                 expand = c(0.15, 0.05)) +
#  labs( fill = paste0("Archaeological categories (n = ", sum(frequency$freq), ")")) +
#  theme_void()+
#  theme(legend.position="bottom", legend.box="vertical", legend.margin=margin(), legend.text = element_text(size = 12)) 

