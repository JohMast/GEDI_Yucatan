######################################################################################
#         
# Visual Analysis of the distribution of built footprints and their neighbors 
# on the layers:
# - pai (Plant Area Index)
# - cover (Total Canopy Cover)
# - fhd (Foliage height Diversity)
# using a number of different visualisation options:
# - Boxplot
# - Profiles by height  
# - Side-by-Side Profiles by height
# - Pairwise Plots
# Note: This script assumes that data has already been downloaded
# as in the script Download_And_Process_GEDI_L2B.R
# - tagged as pairs (beam_pair field)
# - tagged whether the hit a ruin or not (int_build field)
# 
# Project Contributors:
# Žiga Kokalj, Research Centre of the Slovenian Academy of Sciences and Arts 
# Johannes Mast, University of Würzburg
#
# This research was partly funded by the 
# Slovenian Research Agency core funding No. P2-0406.
######################################################################################


#### 1 : Preamble ####
setwd("D:/SoSe2020/InternshipIAPS/GEDI/Yucatan")
source("GEDI_Utils_l2b.R")
library(gridExtra)
library(tidyverse)
library(ggplot2)
library(GGally)
library(ggrepel)


# Prepare the data
# points within the AOI
AOI_df <- read.csv2("Data/L2B_Statistics/L2B_Table_AOI_Points_v3.csv")
# points selected for analysis (intersecting ruins)
Analysis_df <- read.csv2("Data/L2B_Statistics/L2B_Table_Analysis_Points_v3.csv")

# subset to relevant fields, dropping footprints without values
AOI_df_noNA <- AOI_df %>% drop_na()
Analysis_df_noNA <- Analysis_df %>% drop_na() %>% mutate(int_build=ifelse(int_build==2,"built","non-built")) %>%  group_by(beam_pair) %>% filter(n() == 2) %>% ungroup 


table(AOI_df_noNA$is_bajo)
# Additional prepareations for plotting
#count the observations to use them as x axis labels
int_build_counts <- paste(levels(factor(Analysis_df_noNA$int_build)),"\n(N=",table(Analysis_df_noNA$int_build),")",sep="")
# make some colors
int_build_colors <- c(rgb(37,145,100, max=255),rgb(252,99,42, max=255))

# Helper function for outliers:
# https://stackoverflow.com/a/43659981
is_outlier <- function(x,IQR_factor=1.5) {
  return(x < quantile(x, 0.25,na.rm=T) - IQR_factor * IQR(x) | x > quantile(x, 0.75,na.rm=T) + IQR_factor * IQR(x))
}

#### 2. Boxplot ####
FHD_PAI_Boxplot <- Analysis_df_noNA %>% dplyr::select(beam_pair,int_build,PAI,FHD,cover) %>% 
  # Some gymnastics:
  # We make a  new logical column which indicates if the value is an outlier in any column
  mutate(is_outlier=(is_outlier(PAI)|is_outlier(cover)|is_outlier(FHD))) %>% 
  # change int built to a character factor
  # Then we group by pairse
  group_by(beam_pair) %>% 
  # If one of the pairmembers is outlier in ANY of the three boxplots, we label both pair points (stored in new column)
  mutate(outlier_label=ifelse(any(is_outlier),beam_pair,NA)) %>% 
  ungroup() %>% 
  pivot_longer(cols = c( PAI,FHD,cover),names_to = "Variable",values_to = "Value") %>%
  mutate(Variable=forcats::fct_relevel(Variable,"PAI",after = 0)) %>% #reorder to pai-cover-fhd_normal
  mutate(Variable=forcats::fct_recode(Variable, pai = "PAI", fhd_normal = "FHD")) %>% 
  ggplot(aes(x=int_build, y=Value,fill = int_build)) +
  theme_bw()+ 
  geom_boxplot()+
  geom_text_repel(aes(label=outlier_label),hjust=-0.3,direction = "x")+
  xlab("Presence of Structures")+
  ylab("Value of Index")+
  facet_wrap(~Variable,  ncol=3,scales = "free")+
  scale_fill_manual(values =int_build_colors)+
  scale_x_discrete(labels=int_build_counts)+
  theme(legend.position = "none")

# Save the plot to png
png(filename = "D:/SoSe2020/GEDI_Yucatan/Example_Figures/FHD_PAI_Cover_Boxplot.png",
    width = 2*480, height = 2*480, res=72*2,units = "px")
FHD_PAI_Boxplot
dev.off()
# Save the plot to svg
ggsave(file="Report/Figures/Figure_Components/pai_cover_fhd/FHD_PAI_Boxplot.svg",
       plot=FHD_PAI_Boxplot,
       width=8, height=8)




FHD_PAI_Boxplot_jit_data <- Analysis_df_noNA %>% dplyr::select(beam_pair,int_build,PAI,FHD,cover) %>% group_by(beam_pair) %>% filter(n() == 2) %>% ungroup %>% 
  # Some gymnastics:
  # We make a  new logical column which indicates if the value is an outlier in any column
  mutate(is_outlier=(is_outlier(PAI)|is_outlier(cover)|is_outlier(FHD)|beam_pair==43)) %>% 
  # change int built to a character factor
  
  # Then we group by pairs
  group_by(beam_pair) %>% 
  # If one of the pairmembers is outlier in ANY of the three boxplots, we label both pair points (stored in new column)
  mutate(outlier_label=ifelse(any(is_outlier),beam_pair,NA)) %>% 
  ungroup() %>% 
  pivot_longer(cols = c( PAI,FHD,cover),names_to = "Variable",values_to = "Value") %>%
  #make an additional column which tells us if a point will be an outlier in its specific boxplot
  group_by(Variable , int_build ) %>% mutate(jit_outlier=is_outlier(Value)) %>% ungroup %>% 
  mutate(Variable=forcats::fct_relevel(Variable,"PAI",after = 0)) %>% #reorder to pai-cover-fhd_normal
  mutate(Variable=forcats::fct_recode(Variable, pai = "PAI", fhd_normal = "FHD")) 
FHD_PAI_Boxplot_jit <- FHD_PAI_Boxplot_jit_data %>% ggplot(aes(x=int_build, y=Value,fill = int_build)) +
  theme_bw()+ 
  geom_boxplot(outlier.size = 2, outlier.shape = 23)+
  geom_text_repel(aes(label=outlier_label),hjust=-0.3,direction = "x")+
  xlab("Presence of Structures")+
  ylab("Value of Index")+
  facet_wrap(~Variable,  ncol=3,scales = "free")+
  scale_fill_manual(values =int_build_colors)+
  scale_x_discrete(labels=int_build_counts)+
  theme(legend.position = "none")+
    geom_jitter(data = FHD_PAI_Boxplot_jit_data[!FHD_PAI_Boxplot_jit_data$jit_outlier,],width = 0.15)
FHD_PAI_Boxplot_jit
ggsave(file="Report/Figures/Figure_Components/pai_cover_fhd/FHD_PAI_Boxplot_jit.svg",
       plot=FHD_PAI_Boxplot_jit,
       width=8, height=8)



#### 3. Profiles by height  ####
# Manual sequence of height labels 
heights = seq(from=5, 40,by = 5)

# PAVD Profile
# Calculate the standard deviation per height to use in plot
pavd_z_sd <- Analysis_df_noNA%>% dplyr::select(int_build,contains("pavd_z"))  %>% 
  group_by(int_build)%>% summarise_all(list(sd)) %>% 
  pivot_longer(cols = contains("pavd_z"),values_to = "mean")
pavd_z_means <- Analysis_df_noNA%>% dplyr::select(int_build,contains("pavd_z")) %>% 
  group_by(int_build)%>% summarise_all(list(mean)) %>% 
  pivot_longer(cols = contains("pavd_z"),values_to = "sd") %>% 
  left_join(pavd_z_sd,by=c("int_build","name")) %>% 
  mutate(Built = as.factor(int_build))%>% 
  #start the plotting
  ggplot(aes(x = name, y = mean, group=Built,color=Built,fill=Built)) +
  theme_bw()+ 
  geom_line( size=0.9,)+
  scale_colour_manual(values = int_build_colors,labels=c("Non-Built","Built"))+
  scale_fill_manual(values = int_build_colors,labels=c("Non-Built","Built"))+
  scale_x_discrete(labels= heights)+
  xlab("height [m]")+ 
  ylab("Value of Index")+ 
  theme(legend.position = c(0.8, 0.8))+
  geom_ribbon(aes(ymax = mean + sd, ymin = mean - sd),
              alpha = 0.3,
              colour=NA
  )+
  #add a line for the 0 (technically, values below 0 dont make sense, but here we include them anyway because excluding might not be correct)
  geom_hline(yintercept = 0)+
  #switch the axis so high is on the vertical
  coord_flip()+
  ggtitle("Distribution of pavd_z")
# Save the plot as png
png(filename = "Data/L2B_Statistics/Visual/Plots/pavd_z_profile.png",width = 200,height = 500,units = "px")
pavd_z_means
dev.off()


#PAI Profile
# Calculate the standard deviation per height to use in plot
pai_z_sd <- Analysis_df_noNA%>% dplyr::select(int_build,contains("pai_z"))  %>% 
  group_by(int_build)%>% summarise_all(list(sd)) %>% 
  pivot_longer(cols = contains("pai_z"),values_to = "mean")
pai_z_means <- Analysis_df_noNA%>% dplyr::select(int_build,contains("pai_z")) %>% 
  group_by(int_build)%>% summarise_all(list(mean)) %>% 
  pivot_longer(cols = contains("pai_z"),values_to = "sd") %>% 
  left_join(pai_z_sd,by=c("int_build","name")) %>% 
  mutate(Built = as.factor(int_build))%>% 
  #start the plotting
  ggplot(aes(x = name, y = mean, group=Built,color=Built,fill=Built)) +
  theme_bw()+ 
  geom_line( size=0.9,)+
  scale_colour_manual(values = int_build_colors,labels=c("Non-Built","Built"))+
  scale_fill_manual(values = int_build_colors,labels=c("Non-Built","Built"))+
  scale_x_discrete(labels= heights)+
  xlab("height [m]")+ 
  ylab("Value of Index")+ 
  theme(legend.position = c(0.8, 0.8))+
  geom_ribbon(aes(ymax = mean + sd, ymin = mean - sd),
              alpha = 0.3,
              colour=NA
  )+
  #add a line for the 0 (technically, values below 0 dont make sense, but here we include them anyway because excluding might not be correct)
  geom_hline(yintercept = 0)+
  #switch the axis so high is on the vertical
  coord_flip()+
  ggtitle("Distribution of pai_z")

# Save the plot as png
png(filename = "Data/L2B_Statistics/Visual/Plots/pai_z_profile.png",width = 200,height = 500,units = "px")
pai_z_means
dev.off()


# cover profile
# Calculate the standard deviation per height to use in plot
cover_z_sd <- Analysis_df_noNA%>% dplyr::select(int_build,contains("cover_z"))  %>% 
  group_by(int_build)%>% summarise_all(list(sd)) %>% 
  pivot_longer(cols = contains("cover_z"),values_to = "mean")
cover_z_means <- Analysis_df_noNA%>% dplyr::select(int_build,contains("cover_z")) %>% 
  group_by(int_build)%>% summarise_all(list(mean)) %>% 
  pivot_longer(cols = contains("cover_z"),values_to = "sd") %>% 
  left_join(cover_z_sd,by=c("int_build","name")) %>% 
  mutate(Built = as.factor(int_build))%>% 
  #start the plotting
  ggplot(aes(x = name, y = mean, group=Built,color=Built,fill=Built)) +
  theme_bw()+ 
  geom_line( size=0.9,)+
  scale_colour_manual(values = int_build_colors,labels=c("Non-Built","Built"))+
  scale_fill_manual(values = int_build_colors,labels=c("Non-Built","Built"))+
  scale_x_discrete(labels= heights)+
  xlab("height [m]")+ 
  ylab("Value of Index")+ 
  theme(legend.position = c(0.8, 0.8))+
  geom_ribbon(aes(ymax = mean + sd, ymin = mean - sd),
              alpha = 0.3,
              colour=NA
  )+
  #add a line for the 0 (technically, values below 0 dont make sense, but here we include them anyway because excluding might not be correct)
  geom_hline(yintercept = 0)+
  #switch the axis so high is on the vertical
  coord_flip()+
  ggtitle("Distribution of cover_z")

# Save the plot as png
png(filename = "Data/L2B_Statistics/Visual/Plots/cover_z_profile.png",width = 200,height = 500,units = "px")
cover_z_means
dev.off()



#### 4. Side-by-Side Profiles ####

# prepare data for pavd profile
pavd_z_sd <- Analysis_df_noNA%>% dplyr::select(int_build,contains("pavd_z"))  %>% 
  group_by(int_build)%>% summarise_all(list(sd)) %>% 
  pivot_longer(cols = contains("pavd_z"),values_to = "mean")
pavd_z_means <- Analysis_df_noNA%>% dplyr::select(int_build,contains("pavd_z")) %>% 
  group_by(int_build)%>% summarise_all(list(mean)) %>% 
  pivot_longer(cols = contains("pavd_z"),values_to = "sd") %>% 
  left_join(pavd_z_sd,by=c("int_build","name")) %>% 
  mutate(Built = as.factor(int_build))%>% 
  mutate(name=as.numeric(str_split_fixed(name, "_z", 2)[,2]))%>% 
  mutate(layer="pavd_z")

#prepare data for pai profile
pai_z_sd <- Analysis_df_noNA%>% dplyr::select(int_build,contains("pai_z"))  %>% 
  group_by(int_build)%>% summarise_all(list(sd)) %>% 
  pivot_longer(cols = contains("pai_z"),values_to = "mean")
pai_z_means <- Analysis_df_noNA%>% dplyr::select(int_build,contains("pai_z")) %>% 
  group_by(int_build)%>% summarise_all(list(mean)) %>% 
  pivot_longer(cols = contains("pai_z"),values_to = "sd") %>% 
  left_join(pai_z_sd,by=c("int_build","name")) %>% 
  mutate(Built = as.factor(int_build))%>% 
  mutate(name=as.numeric(str_split_fixed(name, "_z", 2)[,2]))%>% 
  mutate(layer="pai_z")

# prepare data for cover profile
cover_z_sd <- Analysis_df_noNA%>% dplyr::select(int_build,contains("cover_z"))  %>% 
  group_by(int_build)%>% summarise_all(list(sd)) %>% 
  pivot_longer(cols = contains("cover_z"),values_to = "mean")
cover_z_means <- Analysis_df_noNA%>% dplyr::select(int_build,contains("cover_z")) %>% 
  group_by(int_build)%>% summarise_all(list(mean)) %>% 
  pivot_longer(cols = contains("cover_z"),values_to = "sd") %>% 
  left_join(cover_z_sd,by=c("int_build","name")) %>% 
  mutate(Built = as.factor(int_build)) %>% 
  mutate(name=(str_split_fixed(name, "_z", 2)[,2])) %>% 
  mutate(layer="cover_z")

# combine the three datasets
triple_profile_plot <- rbind(pai_z_means,pavd_z_means,cover_z_means)%>% 
  # Create ggplot faceted by layer
  ggplot(aes(x = name, y = mean, group=Built,color=Built,fill=Built)) +
  theme_bw()+ 
  geom_line( size=0.9,)+
  scale_colour_manual(values = int_build_colors,labels=c("non-built","built"))+
  scale_fill_manual(values = int_build_colors,labels=c("non-built","built"))+
  scale_x_discrete(labels= heights)+
  xlab("height [m]")+ 
  ylab("Value of Index")+ 
  theme(legend.position = c(0.55, 0.8))+
  geom_ribbon(aes(ymax = mean + sd, ymin = mean - sd),
              alpha = 0.3,
              colour=NA
  )+
  #add a line for the 0 (technically, values below 0 dont make sense, but here we include them anyway because excluding might not be correct)
  geom_hline(yintercept = 0)+
  #switch the axis so high is on the vertical
  coord_flip()+
  facet_wrap(~layer,  ncol=3,scales = "free")+
  guides(color=guide_legend(title="Presence of \nStructures"))+
  guides(fill = FALSE)

png(filename = "D:/SoSe2020/GEDI_Yucatan/Example_Figures/triple_profile_plot.png",width = 2*750, height = 2*480, res=72*2,units = "px")
triple_profile_plot
dev.off()

ggsave(file="Report/Figures/Figure_Components/triple_profile/triple_profile.svg",
       plot=triple_profile_plot,
       width=9.5, height=8)

  
#### 5. Pairwise Plot ####

# We now want to analyse the differences within each pair. 
# We group by the pairs, 
# then sort by the built intensity (here, we treat 1 and 0 equally, as being un built (vs 2=built))
# and calculate the differences
# For selected statistics
# We can drop the rows where a NA appears for one of the points, no matter which parameter -
# because usually, a NA in one parameter means an NA in all of them 
df <- Analysis_df
pairwise_diff <- df %>% 
  group_by(beam_pair) %>%
  arrange(int_build,.by_group=TRUE) %>%
  summarise(
    pai_diff = diff(PAI),
    fhd_diff = diff(FHD),
    cover_diff = diff(cover),
    pai_z1_diff = diff(pai_z1),
    pai_z2_diff = diff(pai_z2),
    pai_z3_diff = diff(pai_z3),
    pai_z4_diff = diff(pai_z4),
    pai_z5_diff = diff(pai_z5),
  ) %>%
  drop_na %>%
  ungroup()
# Save the pairwise differences
xlsx::write.xlsx2(pairwise_diff,file = "Report/Figures/6_Pairwise_Differences/pairwise_differences.xlsx")

# Sum up over all heights
pairwise_diff %>% summarise_all(mean)


# Making a pair plot for every singular variable!
# We first calculate the mean of both sides
# and then plot the individual pairs, with the average as a thick black bar
# Note! To make plotting things easier here, we treat int_build of 1 as equal to 0,
# unlike in the plots above!

# PAI
# calculate the mean difference to add as a highly visible bar
avg_pai <- df %>% 
  dplyr::select(beam_pair,int_build,PAI) %>% 
  mutate(int_build_binary=(int_build=="2")) %>% #make a new column 
  dplyr::select(-int_build) %>% 
  tidyr::spread(int_build_binary,PAI) %>% 
  drop_na %>%
  summarise_all(mean) %>% 
  gather(key = built, value=beam_pair) 
# Create the plot
PAI_pairplot <- df %>% 
  dplyr::select(beam_pair,int_build,PAI) %>% 
  mutate(int_build_binary=(int_build=="2")) %>% #make a new column 
  dplyr::select(-int_build) %>% 
  tidyr::spread(int_build_binary,PAI)%>% 
  ggparcoord(columns = 2:3,
             groupColumn = 1,
             showPoints = TRUE, 
             scale="globalminmax",
             title = "PAI Values of Pairs",
             alphaLines = 0.6)+ scale_color_gradientn(colours = rainbow(50))+
  geom_line(data =avg_pai,mapping =  aes(x=built,y=beam_pair,group=TRUE,size=1),inherit.aes = FALSE)+
  ylab("PAI Value") + 
  xlab("Built")+
  theme(legend.position = "none")

# FHD
# calculate the mean difference to add as a highly visible bar
avg_fhd <- df %>% 
  dplyr::select(beam_pair,int_build,FHD) %>% 
  mutate(int_build_binary=(int_build=="2")) %>% #make a new column 
  dplyr::select(-int_build) %>% 
  tidyr::spread(int_build_binary,FHD) %>% 
  drop_na %>%
  summarise_all(mean) %>% 
  gather(key = built, value=beam_pair) 
# Create the plot
FHD_pairplot <- df %>% 
  dplyr::select(beam_pair,int_build,FHD) %>% 
  mutate(int_build_binary=(int_build=="2")) %>% #make a new column 
  dplyr::select(-int_build) %>% 
  tidyr::spread(int_build_binary,FHD)%>% 
  ggparcoord(columns = 2:3,
             groupColumn = 1,
             showPoints = TRUE, 
             scale="globalminmax",
             title = "FHD Values of Pairs",
             alphaLines = 0.6)+ scale_color_gradientn(colours = rainbow(50))+
  geom_line(data =avg_fhd,mapping =  aes(x=built,y=beam_pair,group=TRUE,size=1),inherit.aes = FALSE)+
  ylab("FHD Value") + 
  xlab("Built")+
  theme(legend.position = "none")

# Cover
# calculate the mean difference to add as a highly visible bar
avg_cover <- df %>% 
  dplyr::select(beam_pair,int_build,cover) %>% 
  mutate(int_build_binary=(int_build=="2")) %>% #make a new column 
  dplyr::select(-int_build) %>% 
  tidyr::spread(int_build_binary,cover) %>% 
  drop_na %>%
  summarise_all(mean) %>% 
  gather(key = built, value=beam_pair) 
# Create the plot
cover_pairplot <- df %>% 
  dplyr::select(beam_pair,int_build,cover) %>% 
  mutate(int_build_binary=(int_build=="2")) %>% #make a new column 
  dplyr::select(-int_build) %>% 
  tidyr::spread(int_build_binary,cover)%>% 
  ggparcoord(columns = 2:3,
             groupColumn = 1,
             showPoints = TRUE, 
             scale="globalminmax",
             title = "Cover Values of Pairs",
             alphaLines = 0.3)+ scale_color_gradientn(colours = rainbow(50))+
  geom_line(data =avg_cover,mapping =  aes(x=built,y=beam_pair,group=TRUE,size=1),inherit.aes = FALSE)+
  ylab("Cover Value") + 
  xlab("Built")+
  theme(legend.position = "none")
cover_pairplot

# Save the plot
png(filename = "D:/SoSe2020/GEDI_Yucatan/Example_Figures/PAI_FHD_Cover_pairplots.png")
grid.arrange(PAI_pairplot, FHD_pairplot, cover_pairplot,ncol=3)
dev.off()
