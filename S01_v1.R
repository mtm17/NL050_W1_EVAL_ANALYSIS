####SCRIPT01 v1######
####DATE: 7 Feb 2025
####FILENAME: S01_v1.R

####Very useful website with lean packages for setting up Github-R Studio:
#https://rfortherestofus.com/2021/02/how-to-use-git-github-with-r

####SCRIPT PURPOSE:  Load in NL050 UC Disaster Resilience Workshop Data and complete basic visualizations for participant feedback.

####Questions we are answering:  How did participant awareness of CA-5th Climate Assessment Data and Planscape software change before/after the workshop?  What were participant perceptions of the utility of different data products?

#####LOAD KEY PACKAGES####
library(tidyverse)
library(ggrepel)


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

#FIRST: Make a boxplot of Q1 responses (removing R12): 
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

#write out boxplot to output_graphs folder.
png(file.path(graph_output_dir, "CCA5_Before-After_Participant_Familiarity_boxplot_vertical.png"), width=5, height=6, units="in", pointsize=8, res=400)
Q1ab_plot
dev.off()

#SECOND:  HISTOGRAM OF INDIV. RESPONDENT-LEVEL CHANGE IN UNDERSTANDING.

#starting data subset
W1_data_Q1ab

#pivot data table longer to calculate changes in Q1a vs Q1b (before vs. after) scores
W1_data_Q1ab_diff <- W1_data_Q1ab %>%
  pivot_wider(names_from = question, values_from=rank)
W1_data_Q1ab_diff$delta = W1_data_Q1ab_diff$`1b`-W1_data_Q1ab_diff$`1a`

#plot histogram of respondent understanding rank changes by individual.
Q1ab_changeplot_v1 <- ggplot(W1_data_Q1ab_diff %>%
         filter(respondent != "R12_rich"),
       aes(delta))+
  geom_histogram(color="black", fill="darkblue", alpha=0.2, binwidth=1)+
  scale_x_continuous(limits=c(-10, 10))+
  theme_bw()+
  labs(x = "Likert-Rank Score Change in Indiv. Participant Understanding \n CCA-5 Wildfire Modeling Data", y="Number of Participants")+
  labs(title="Participant Self-Ranked Change in Understanding: \n California 5th Climate Assessment Data")+
  theme(plot.title=element_text(hjust=0.5))

#write out histogram to output_graphs folder.
png(file.path(graph_output_dir, "CCA5_Participant_Understanding_Change_Histogram.png"), width=6, height=4, units="in", pointsize=8, res=400)
Q1ab_changeplot_v1
dev.off()


#THIRD: STACKED BAR GRAPH - INDIV. UNDERSTANDING RANKINGS, BEFORE vs. AFTER.  Are those who are improving most in understanding coming into the workshop with lower, or higher, initial awareness?  is there a bias in who are we reaching?
head(W1_data_Q1ab)
str(W1_data_Q1ab)
Q1ab_stackgraph <- ggplot(W1_data_Q1ab %>%
                      filter(respondent != "R12_rich"),
                    aes(x=respondent, y=rank, fill=question)) +
  geom_bar(color="black", position="dodge", stat="identity", alpha=0.2)+
  scale_fill_manual(values=c("yellow", "blue"),
                    name = "",
                    labels=c("Pre-Wkshp", "Post-Wkshp"))+
  scale_y_continuous(limits=c(0,10), breaks = c(1,2,3,4,5,6,7,8,9,10))+
  theme_bw()+
  labs(y="Likert Rank Scale (1=Low, 10=High)")
Q1ab_stackgraph

png(file.path(graph_output_dir, "CCA5_Participant_Understanding_Change_PrePostBar.png"), width=6, height=4, units="in", pointsize=8, res=400)
Q1ab_stackgraph
dev.off()

#FOURTH: SCATTERPLOT:  CHANGE IN UNDERSTANDING vs. INITIAL LEVEL OF FAMILIARITY
Q1ab_scatterplot_learning <- ggplot(W1_data_Q1ab_diff %>%
         filter(respondent != "R12_rich"),
       aes(x=`1a`, y=delta))+
  geom_point()+
  geom_text_repel(aes(label=respondent))+
  scale_x_continuous(limits=c(0,10), breaks=seq(0,10, by=1))+
  scale_y_continuous(limits=c(0, 10), breaks=seq(-10,10, by=1))+
  labs(x="Initial Participant Likert-Rank Understanding \n CCA-5 Wildfire Data",
       y="Change After Workshop in Likert-Rank Understanding: \n CCA-5 Wildfire Data")+
    theme_bw()
Q1ab_scatterplot_learning

png(file.path(graph_output_dir, "CCA5_Participant_Understanding_Change_Scatterplot.png"), width=5, height=5, units="in", pointsize=8, res=400)
Q1ab_scatterplot_learning
dev.off()


#####3.  Visualize Likert Survey Questions 1-10 Scale: Planscape #####

#filter out and select questions 1a and 1b pertaining to before/after familiarity with CCA-5 data.
W1_data_Q2ab <- W1_data %>%
  dplyr::filter(question == "2a" | question == "2b" | question == "2c")

#FIRST: Make a boxplot of Q1 responses (removing R12): 
Q2ab_plot <- ggplot(W1_data_Q2ab %>%
                      filter(respondent != "R12_rich" & question != "2c"),
                    aes(x=question, y=rank)) +
  geom_boxplot(fill="darkblue", alpha=0.2)+
  scale_y_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10), limits=c(1,10))+
  scale_x_discrete(labels=c('Before Workshop', 'After Workshop'))+
  labs(x = "Self-Ranked Participant Understanding: \n Planscape Software", y="Likert Rank Scale (1=Low, 10=High)")+
  theme(axis.title.x=element_text())+
  labs(title="Before vs. After-Workshop Participant Familiarity: \n Planscape Software")+
  theme_bw()+
  theme(plot.title=element_text(hjust=0.5))
Q2ab_plot

#write out boxplot to output_graphs folder.
png(file.path(graph_output_dir, "Planscape_Before-After_Participant_Familiarity_boxplot_vertical.png"), width=5, height=6, units="in", pointsize=8, res=400)
Q2ab_plot
dev.off()

#SECOND:  HISTOGRAM OF INDIV. RESPONDENT-LEVEL CHANGE IN UNDERSTANDING.

#starting data subset
W1_data_Q1ab

#pivot data table longer to calculate changes in Q1a vs Q1b (before vs. after) scores
W1_data_Q2ab_diff <- W1_data_Q2ab %>%
  pivot_wider(names_from = question, values_from=rank)
W1_data_Q2ab_diff$delta = W1_data_Q2ab_diff$`2b`-W1_data_Q2ab_diff$`2a`

#plot histogram of respondent understanding rank changes by individual.
Q2ab_changeplot_v1 <- ggplot(W1_data_Q2ab_diff %>%
                               filter(respondent != "R12_rich"),
                             aes(delta))+
  geom_histogram(color="black", fill="darkblue", alpha=0.2, binwidth=1)+
  scale_x_continuous(limits=c(-10, 10))+
  theme_bw()+
  labs(x = "Likert-Rank Score Change in Indiv. Participant Understanding \n Planscape Software", y="Number of Participants")+
  labs(title="Participant Self-Ranked Change in Understanding: \n Planscape Software")+
  theme(plot.title=element_text(hjust=0.5))
Q2ab_changeplot_v1
#write out histogram to output_graphs folder.
png(file.path(graph_output_dir, "Planscape_Participant_Understanding_Change_Histogram.png"), width=6, height=4, units="in", pointsize=8, res=400)
Q2ab_changeplot_v1
dev.off()

#THIRD: STACKED BAR GRAPH - INDIV. UNDERSTANDING RANKINGS, BEFORE vs. AFTER.  Are those who are improving most in understanding coming into the workshop with lower, or higher, initial awareness?  is there a bias in who are we reaching?
head(W1_data_Q2ab)
str(W1_data_Q2ab)
Q2ab_stackgraph <- ggplot(W1_data_Q2ab %>%
                            filter(respondent != "R12_rich" & question != "2c"),
                          aes(x=respondent, y=rank, fill=question)) +
  geom_bar(color="black", position="dodge", stat="identity", alpha=0.2)+
  scale_fill_manual(values=c("yellow", "blue"),
                    name = "",
                    labels=c("Pre-Wkshp", "Post-Wkshp"))+
  scale_y_continuous(limits=c(0,10), breaks = c(1,2,3,4,5,6,7,8,9,10))+
  theme_bw()+
  labs(y="Likert Rank Scale (1=Low, 10=High)")
Q2ab_stackgraph

png(file.path(graph_output_dir, "Planscape_Participant_Understanding_Change_PrePostBar.png"), width=6, height=4, units="in", pointsize=8, res=400)
Q2ab_stackgraph
dev.off()

#FOURTH: SCATTERPLOT:  CHANGE IN UNDERSTANDING vs. INITIAL LEVEL OF FAMILIARITY
Q2ab_scatterplot_learning <- ggplot(W1_data_Q2ab_diff %>%
                                      filter(respondent != "R12_rich"),
                                    aes(x=`2a`, y=delta))+
  geom_point(na.rm=T)+
  geom_text_repel(aes(label=respondent))+
  scale_x_continuous(limits=c(0,10), breaks=seq(0,10, by=1))+
  scale_y_continuous(limits=c(0, 10), breaks=seq(-10,10, by=1))+
  labs(x="Initial Participant Likert-Rank Understanding \n Planscape Software",
       y="Change After Workshop in Likert-Rank Understanding: \n Planscape Software")+
  theme_bw()
Q2ab_scatterplot_learning

png(file.path(graph_output_dir, "Planscape_Participant_Understanding_Change_Scatterplot.png"), width=5, height=5, units="in", pointsize=8, res=400)
Q2ab_scatterplot_learning
dev.off()





