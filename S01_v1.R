####SCRIPT01 v1######
####DATE: 7 Feb 2025
####FILENAME: S01_v1.R

####Very useful website with lean packages for setting up Github-R Studio:
#https://rfortherestofus.com/2021/02/how-to-use-git-github-with-r

####SCRIPT PURPOSE:  Load in NL050 UC Disaster Resilience Workshop Data and complete basic visualizations for participant feedback.

####Questions we are answering:  How did participant awareness of CA-5th Climate Assessment Data and Planscape software change before/after the workshop?  What were participant perceptions of the utility of different data products?

#####LOAD KEY PACKAGES####
library(tidyverse)


#####1. Set Data Input and Graph Output Directories and Load Raw Dataset####

data_input_dir <- "./data_RREADY/"
file01_name <- "wkshp1_eval_data_prep-for-R.csv"

graph_output_dir <- "./output_graphs/"

#workshop dataset: W1_data"
W1_data <- read.csv(paste0(data_input_dir, file01_name))
head(W1_data)
names(W1_data)

#####2.  Visualize Likert Survey Questions 1-10 Scale: CCA-5 Data#####

#filter out and select questions 1a and 1b pertaining to before/after familiarity with CCA-5 data.
W1_data_Q1ab <- W1_data %>%
  dplyr::filter(question == "1a" | question == "1b")

#try boxplot of Q1 responses (removing R12): 

Q1ab_plot <- ggplot(W1_data_Q1ab %>%
                      filter(respondent != "R12_rich"),
                    aes(x=question, y=rank)) +
  geom_boxplot(fill="darkblue", alpha=0.2)+
  scale_y_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10), limits=c(1,10))+
  scale_x_discrete(labels=c('Before Workshop', 'After Workshop'))+
  labs(x = "Self-Ranked Participant Understanding: \n CCA-5 Wildfire Modeling Data", y="Likert Rank Scale (1=Low, 10=High)")+
  theme(axis.title.x=element_text())+
  labs(title="Before vs. After-Workshop Participant Familiarity: \n California 5th Climate Assessment Data")+
  theme_bw()+
  theme(plot.title=element_text(hjust=0.5))
Q1ab_plot

#write out Q1ab plot to output_graphs folder.
png(file.path(graph_output_dir, "CCA5_Before-After_Participant_Familiarity_boxplot_vertical.png"), width=5, height=6, units="in", pointsize=8, res=400)
Q1ab_plot
dev.off()




