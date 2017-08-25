#Factor Levels for a group of variables within a dataset and then plot to likert with factor 
#levels set to the same number. 

library(likert)
library(tidyverse)
library(forcats)
library(reshape2)

setwd("/Users/Anaya/Dropbox/**UCDavis/PrePostAngus")

# Qualtrix adds an odd formatting third row which needs to be skipped to maintain proper factor levels. 
# Additionally, the post data questions had a hard break coded, so the first four lines need to be skipped
Post17 <- read.csv("ANGUS_post-assessment2017.csv",
                   na.strings = c( "", " ", "NA"),
                   stringsAsFactors = TRUE, skip = 4, header = FALSE)

Pre17 <- read.csv("ANGUS_pre-assessment2017.csv", 
                  na.strings = c("", " ", "NA"),
                  stringsAsFactors = TRUE, skip = 3, header = FALSE)

# The pre data had empty columns, or columns that are not renamed with the colnames() function. Remove these
Pre17 <- Pre17[ , 49:53]

#Change headernames to match
source("PrePostHeaderSyncR.R")
colnames(Post17) <- c(Post17shortname)
colnames(Pre17) <- c(Pre17shortname)

#Remove columns that are survey previews 
Pre17 <- Pre17[!(Pre17$Status=="Survey Preview"| Pre17$Status=="Survey Test" ), ]
Post17 <- Post17[!(Post17$Status=="Survey Preview"| Post17$Status=="Survey Preview Spam" ), ]

#select columns for use for ability Qs vs. agree Qs. 
Pre_AgreeQ_17 <- select(Pre17, 
                        Qcd, Qbwameans, Qtrinity, Qbinbash, Qscriptpy, Qrstart, Qsalamon, 
                        QDEseq2, Qgrep, QedgeR) 

Pre_AbilityQ_17 <- select(Pre17, 
                          Qpython, Qshell, Qcloud, QBWA, Qgenome, Qquery, Qevalassemb) 

Post_AgreeQ_17 <- select(Post17, 
                         Qcd, Qbwameans, Qtrinity, Qbinbash, Qscriptpy, Qrstart, Qsalamon, 
                         QDEseq2, Qgrep, QedgeR) 

Post_AbilityQ_17 <- select(Post17, 
                           Qpython, Qshell, Qcloud, QBWA, Qgenome, Qquery, Qevalassemb) 

##Factor levels for each question type 
abilitylevel <- c("No Ability", "Low Ability", "Intermediate Ability", "High Ability")
agreelevel <- c("Strongly disagree", "Disagree", "Agree", "Strongly Agree")

# Define colors to be used in process_and_plot_survey()
PurpleGreenFour <- c('#7b3294', '#c2a5cf', '#a6dba0', '#008837')
PurpleGreenFive <- c('#7b3294', '#c2a5cf', '#ffffbf', '#a6dba0', '#008837') 
PurpleGreenSix <- c('#762a83', '#af8dc3', '#e7d4e8', '#ffffbf', '#7fbf7b', '#1b7837')
PurpleGreenSeven <- c('#762a83', '#af8dc3', '#e7d4e8', '#ffffbf', '#d9f0d3', '#7fbf7b', '#1b7837')


#Function to mutate data into a form that can be plotted. 
# Define function to process and plot the data
process_and_plot_survey <- function(data_pre, data_post, title, question_levels, color_palette) {
  # data_pre: a data.frame of pre-survey data (Pre16 in this example)
  # data_post: a data.frame of post-survey data (Post16 in this example)
  
  # add a column identifier for "Post" and "Pre"; add ID identifier to distinguish participant
  data_post <- mutate(data_post, 
                      Group = rep("Post", nrow(data_post)))
  data_pre <- mutate(data_pre, 
                     Group = rep("Pre", nrow(data_pre)))
  
  # Join pre- and post-survey data into one dataframe. This prints a verbose output about joins, 
  # and will generate many warnings about coercing factors to character vectors bc of different levels
  joined_data <- full_join(data_post, data_pre)

  # Format levels for each column in the dataframe
  require(plyr)
  joined_data[, ] <- plyr::colwise(as.factor)(joined_data[, ])
  # define column to remove (Group has two levels that are different from question_levels)
  drops <- c("Group")
  # Unify factor level while removing Group column
  # note "ng" stands for "no group"
  joined_data_ng <- fct_unify(joined_data[ , !(names(joined_data) %in% drops)], levels = question_levels) %>% as.data.frame()
  # re-bind the group variable
  joined_data_bound <- cbind(joined_data_ng, joined_data$Group)
  # rename the group column to "Group" for simpler referencing
  joined_data_bound = rename(joined_data_bound, replace = c("joined_data$Group" = "Group"))
  
  # Define group level
  GrpLevel <- c("Post", "Pre")
  
  # Factor the group column with levels as GrpLevel
  joined_data_bound$Group <- factor(joined_data_bound$Group, levels=GrpLevel, ordered = TRUE)
  
  # perform likert using the ng dataframe with releveled factors and the refactored Group vector
  likert_ng_joined <- likert(joined_data_ng, grouping = joined_data_bound$Group)
  
  likert_plot <- plot(likert_ng_joined, colors = color_palette) + ggtitle(title) 
  return(likert_plot)
  }

# Define a dummy in a title 
Title <- "This is my title"

#Running Process function 
process_and_plot_survey(Pre_AbilityQ_17, Post_AbilityQ_17, Title, abilitylevel)
process_and_plot_survey(Pre_AgreeQ_17, Post_AgreeQ_17, Title, agreelevel)

# break -------------------------------------------------------------------


#create a new level for my new joined dataset to distinguish between Pre and Post groups - 
GrpLevel <- c("Post", "Pre")
Join_PerCompAbilSc$Group <- factor(Join_PerCompAbilSc$Group, levels=GrpLevel, ordered = TRUE)
Join_CompUndSc$Group <- factor(Join_CompUndSc$Group, levels=GrpLevel, ordered = TRUE)
Join_CodeAbilSc1$Group <- factor(Join_CodeAbilSc1$Group, levels=GrpLevel, ordered = TRUE)
Join_CodeAbilSc2$Group <- factor(Join_CodeAbilSc2$Group, levels=GrpLevel, ordered = TRUE)
Join_ComfCompTask$Group <- factor(Join_ComfCompTask$Group, levels=GrpLevel, ordered = TRUE)

#Create df without the group var for plotting. 
WOG_Join_PerCompAb <- select(Join_PerCompAbilSc, everything()) %>% 
  select(-starts_with("Group"))  
WOG_Join_CompUnd <- select(Join_CompUndSc, everything()) %>% 
  select(-starts_with("Group"))  
WOG_Join_CodeAbil1 <- select(Join_CodeAbilSc1, everything()) %>% 
  select(-starts_with("Group"))  
WOG_Join_CodeAbil2 <- select(Join_CodeAbilSc2, everything()) %>% 
  select(-starts_with("Group"))  
WOG_Join_ComfComp <- select(Join_ComfCompTask, everything()) %>% 
  select(-starts_with("Group"))  


#Setup Likert Plots by group with renaming  
PerCompTitle <- "2016 Edamame Perceived Computational Ability Scale"
PerCompAbilScLik <- likert(WOG_Join_PerCompAb, grouping = Join_PerCompAbilSc$Group)

CompUndTitle <- "2016 Edamame Computational Understanding Scale"
CompUndLik <- likert(WOG_Join_CompUnd, grouping = Join_CompUndSc$Group)

CodeAbilTitle1 <- "2016 Edamame Coding Ability Scale, Chart 1"
CodeAbilLik1 <- likert(WOG_Join_CodeAbil1, grouping = Join_CodeAbilSc1$Group)

CodeAbilTitle2 <- "2016 Edamame Coding Ability Scale, Chart 2"
CodeAbilLik2 <- likert(WOG_Join_CodeAbil2, grouping = Join_CodeAbilSc2$Group)

ComfCompTitle <- "2016 Edamame Comfort with Computational Tasks Scale"
ComfCompLik <- likert(WOG_Join_ComfComp, grouping = Join_ComfCompTask$Group)

# plotting ----------------------------------------------------------------

#Plotting 
pdf("PerCompAbilScLikert.pdf", width = 6, height = 9 )
plot(PerCompAbilScLik, colors=PurpleGreenFour) + ggtitle(PerCompTitle)
dev.off()

pdf("CompUndLikert.pdf", width = 6, height = 9 )
plot(CompUndLik, colors=PurpleGreenFour) + ggtitle(CompUndTitle)
dev.off()

pdf("CodeAbilLikert1.pdf", width = 6, height = 9 )
plot(CodeAbilLik1, colors=PurpleGreenFour) + ggtitle(CodeAbilTitle1)
dev.off()

pdf("CodeAbilLikert2.pdf", width = 6, height = 9 )
plot(CodeAbilLik2, colors=PurpleGreenFour) + ggtitle(CodeAbilTitle2)
dev.off()

pdf("ComfComLikert.pdf", width = 6, height = 10)
plot(ComfCompLik, 
     center = 3.5,
     plot.percent.neutral = FALSE,
     colors = PurpleGreenFive) +
  guides(keywidth = 30) +
  ggtitle(ComfCompTitle)
dev.off()
