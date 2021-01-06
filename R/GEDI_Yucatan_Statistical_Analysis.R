######################################################################################
#         
# Statistical Analysis of the distribution of built footprints and their neighbors 
# Various statistical tests are applied to check if there are significant differences
# on the layers:
# - pai (Plant Area Index)
# - cover (Total Canopy Cover)
# - fhd (Foliage height Diversity)
# between GEDI footprints overlapping ruins and those that do not
# - Permutation test on build-differences
# - Manual permutation 
# - Coin independence test using an asymptotic reference distribution
# - Permutation test on build-differences
# - Paired 2 sample t-test
# - MANOVA
# - Wilcoxon rank sum test
# - Scatterplot
# - Moran's I
# Note: This script assumes that data has already been downloaded
# as in the script Download_And_Process_GEDI_L2B.R
# Not all of these were deemed appropriate to the data and used in the publication.
# and footprints have been labeled:
# - tagged as pairs (beam_pair field)
# - tagged whether the hit a ruin or not (int_build field)
# - tagged whether the footprints intersect a bajo (is_bajo field)
# Project Contributors:
# Žiga Kokalj, Research Centre of the Slovenian Academy of Sciences and Arts 
# Johannes Mast, University of Würzburg
#
# This research was partly funded by the 
# Slovenian Research Agency core funding No. P2-0406.
######################################################################################


#### 1 : Preamble ####
set.seed(1337)
setwd("D:/SoSe2020/InternshipIAPS/GEDI/Yucatan")
library(ggridges)
source("GEDI_Utils_l2b.R")
library(gridExtra)
library(tidyverse)
library(ggplot2)
library(GGally)
library(grid)
library(coin)

# Prepare the data
# points within the A
AOI_df <- read.csv2("Data/L2B_Statistics/L2B_Table_AOI_Points_v3.csv")
# points selected for analysis (intersecting ruins)
Analysis_df <- read.csv2("Data/L2B_Statistics/L2B_Table_Analysis_Points_v3.csv") 

#### 2 : Permutation test ####

#### 2.1 :  On the entire Dataset ####
#### 2.1.1 : Manual permutation ####
# subset to relevant fields, dropping footprints without values
AOI_df_noNA <- AOI_df %>% drop_na() %>% dplyr::select(int_build,PAI,cover,FHD)
Analysis_df_noNA <- Analysis_df %>% drop_na()%>%  dplyr::select(int_build,PAI,cover,FHD)
AOI_df_noNA_nonbuilt <- AOI_df_noNA[AOI_df_noNA$int_build==0,] 
AOI_df_noNA_built <- AOI_df_noNA[AOI_df_noNA$int_build==2,]


# One function for getting us the statistics we need
calculate_stats <- function(data){
data %>%
  dplyr::select(-int_build) %>% 
  summarise_all(list(mean=mean))
}

# helper function for doing the same but randomized
calculate_stats_sampled <- function(data,n=51){
  data %>%
    sample_n(n) %>% 
    calculate_stats
}

# calculate the stats to observe (mean and sd)
# measured_stats <- AOI_df_noNA_built %>% calculate_stats();measured_stats
 measured_built_stats <- AOI_df_noNA_built %>% calculate_stats();measured_built_stats
 measured_nonbuilt_stats <- AOI_df_noNA_nonbuilt %>% calculate_stats();measured_nonbuilt_stats


# calculate the observed difference
observed_diff <- measured_built_stats-measured_nonbuilt_stats;observed_diff
# Create the permutations, recalculating the stats
permuted_random_stats <- replicate((99999+1),calculate_stats_sampled(AOI_df_noNA_nonbuilt),simplify = F) %>% do.call(rbind,.)

# Calculate the p-values: On the differences between observed mean and reference means
# Calculate the difference per permutation from the mean of the AOI
permuted_random_stats_diff <- permuted_random_stats
for(i in 1:nrow(permuted_random_stats)) {
  permuted_random_stats_diff[i,] <- permuted_random_stats[i,]-measured_nonbuilt_stats
}

# One tailed (using differences between observed mean and reference means)
p_values_ot <- list()
for(i in 1:ncol(measured_built_stats)){
  p_values_ot[[i]] <- sum(permuted_random_stats_diff[,i]>observed_diff[,i])/(99999+1)
}
p_values_ot

# Two tailed (using differences of absolute values)
p_values_tt <- list()
for(i in 1:ncol(measured_built_stats)){
  p_values_tt[[i]] <- sum(abs(permuted_random_stats_diff[,i])>=abs(observed_diff[,i]))/(99999+1)
}
p_values_tt

# convert the results to a dataframe
sigs <- data.frame(name=names(measured_built_stats),value=as.numeric(t(as.data.frame(measured_built_stats))))
sigs$p_onetailed <- do.call(rbind,p_values_ot)[,]
sigs$p_twotailed<- do.call(rbind,p_values_tt)[,]

#### 2.1.2 : Coin independence test  ####
# using an asymptotic reference distribution

# onetailed

coin_PAI_data <- AOI_df_noNA %>% dplyr::select(int_build,PAI)%>% 
  pivot_longer(cols = PAI)
coin_PAI_ot <- coin::independence_test(value ~ int_build,
                                    data = coin_PAI_data,alternative="greater",distribution="asymptotic");coin_PAI_ot

coin_cover_data <- AOI_df_noNA %>% dplyr::select(int_build,cover)%>% 
  pivot_longer(cols = cover)
coin_cover_ot <- coin::independence_test(value ~ int_build,
                                      data = coin_cover_data,alternative="greater",distribution="asymptotic");coin_cover_ot

coin_FHD_data <- AOI_df_noNA %>% dplyr::select(int_build,FHD)%>% 
  pivot_longer(cols = FHD)
coin_FHD_ot <- coin::independence_test(value ~ int_build,
                                    data = coin_FHD_data,alternative="greater",distribution="asymptotic");coin_FHD_ot



sigs$p_onetailed_coin <- c(coin_PAI_ot@distribution@pvalue(coin_PAI_ot@statistic@teststatistic),
                           coin_cover_ot@distribution@pvalue(coin_cover_ot@statistic@teststatistic),
                           coin_FHD_ot@distribution@pvalue(coin_FHD_ot@statistic@teststatistic))
# twotailed

coin_PAI_data <- AOI_df_noNA %>% dplyr::select(int_build,PAI)%>% 
  pivot_longer(cols = PAI)
coin_PAI <- coin::independence_test(value ~ int_build,
                                    data = coin_PAI_data,distribution="asymptotic");coin_PAI
coin_PAI@distribution@pvalue(coin_PAI@statistic@teststatistic)
coin_cover_data <- AOI_df_noNA %>% dplyr::select(int_build,cover)%>% 
  pivot_longer(cols = cover)
coin_cover <- coin::independence_test(value ~ int_build,
                                      data = coin_cover_data,distribution="asymptotic");coin_cover
coin_cover@distribution@pvalue(coin_cover@statistic@teststatistic)



coin_FHD_data <- AOI_df_noNA %>% dplyr::select(int_build,FHD)%>% 
  pivot_longer(cols = FHD)
coin_FHD <- coin::independence_test(value ~ int_build,
                                    data = coin_FHD_data,distribution="asymptotic");coin_FHD
coin_FHD@distribution@pvalue(coin_FHD@statistic@teststatistic)


sigs$p_twotailed_coin <- c(coin_PAI@distribution@pvalue(coin_PAI@statistic@teststatistic),
                           coin_cover@distribution@pvalue(coin_cover@statistic@teststatistic),
                           coin_FHD@distribution@pvalue(coin_FHD@statistic@teststatistic))

sigs_bajo <- sigs
write.csv2(sigs_bajo,"sigs_bajo.csv")
#### 2.2 : On the non-bajo dataset ####

#### 2.2.1 : Manual permutation ####
# subset to relevant fields, dropping footprints without values
AOI_df_noNA <- AOI_df %>%  filter(is_bajo==0) %>%  drop_na() %>% dplyr::select(int_build,PAI,cover,FHD)
#Analysis_df_noNA <- Analysis_df %>% drop_na()%>%  dplyr::select(int_build,PAI,cover,FHD)
#AOI_df_noNA_nonbuilt <- AOI_df_noNA[AOI_df_noNA$int_build==0,] 
AOI_df_noNA_built <-  AOI_df %>% drop_na() %>% filter(int_build==2 & is_bajo==0) %>% dplyr::select(int_build,PAI,cover,FHD)
AOI_df_noNA_nonbuilt <- AOI_df %>% drop_na() %>% filter(int_build==0 & is_bajo==0) %>% dplyr::select(int_build,PAI,cover,FHD)

# One function for getting us the statistics we need
calculate_stats <- function(data){
  data %>%
    dplyr::select(-int_build) %>% 
    summarise_all(list(mean=mean))
}

# helper function for doing the same but randomized
calculate_stats_sampled <- function(data,n=51){
  data %>%
    sample_n(n) %>% 
    calculate_stats
}

# calculate the stats to observe (mean and sd)
# measured_stats <- AOI_df_noNA_built %>% calculate_stats();measured_stats
measured_built_stats <- AOI_df_noNA_built %>% calculate_stats();measured_built_stats
measured_nonbuilt_stats <- AOI_df_noNA_nonbuilt %>% calculate_stats();measured_nonbuilt_stats


# calculate the observed difference
observed_diff <- measured_built_stats-measured_nonbuilt_stats;observed_diff
# Create the permutations, recalculating the stats
permuted_random_stats <- replicate(99999+1,calculate_stats_sampled(AOI_df_noNA_nonbuilt),simplify = F) %>% do.call(rbind,.)

# Calculate the p-values: On the differences between observed mean and reference means
# Calculate the difference per permutation from the mean of the AOI
permuted_random_stats_diff <- permuted_random_stats
for(i in 1:nrow(permuted_random_stats)) {
  permuted_random_stats_diff[i,] <- permuted_random_stats[i,]-measured_nonbuilt_stats
}

# One tailed (using differences between observed mean and reference means)
p_values_ot <- list()
for(i in 1:ncol(measured_built_stats)){
  p_values_ot[[i]] <- sum(permuted_random_stats_diff[,i]>observed_diff[,i])/(99999+1)
}

# Two tailed (using differences of absolute values)
p_values_tt <- list()
for(i in 1:ncol(measured_built_stats)){
  p_values_tt[[i]] <- sum(abs(permuted_random_stats_diff[,i])>=abs(observed_diff[,i]))/(99999+1)
}

# convert the results to a dataframe
sigs <- data.frame(name=names(measured_built_stats),value=as.numeric(t(as.data.frame(measured_built_stats))))
sigs$p_onetailed <- do.call(rbind,p_values_ot)[,]
sigs$p_twotailed<- do.call(rbind,p_values_tt)[,]

#### 2.2.2 : Coin independence test  ####
# using an asymptotic reference distribution

# onetailed

coin_PAI_data <- AOI_df_noNA %>% dplyr::select(int_build,PAI)%>% 
  pivot_longer(cols = PAI)
coin_PAI_ot <- coin::independence_test(value ~ int_build,
                                       data = coin_PAI_data,alternative="greater",distribution="asymptotic");coin_PAI_ot

coin_cover_data <- AOI_df_noNA %>% dplyr::select(int_build,cover)%>% 
  pivot_longer(cols = cover)
coin_cover_ot <- coin::independence_test(value ~ int_build,
                                         data = coin_cover_data,alternative="greater",distribution="asymptotic");coin_cover_ot

coin_FHD_data <- AOI_df_noNA %>% dplyr::select(int_build,FHD)%>% 
  pivot_longer(cols = FHD)
coin_FHD_ot <- coin::independence_test(value ~ int_build,
                                       data = coin_FHD_data,alternative="greater",distribution="asymptotic");coin_FHD_ot

sigs$p_onetailed_coin <- c(coin_PAI_ot@distribution@pvalue(coin_PAI_ot@statistic@teststatistic),
                           coin_cover_ot@distribution@pvalue(coin_cover_ot@statistic@teststatistic),
                           coin_FHD_ot@distribution@pvalue(coin_FHD_ot@statistic@teststatistic))
# twotailed

coin_PAI_data <- AOI_df_noNA %>% dplyr::select(int_build,PAI)%>% 
  pivot_longer(cols = PAI)
coin_PAI <- coin::independence_test(value ~ int_build,
                                    data = coin_PAI_data,distribution="asymptotic");coin_PAI
coin_PAI@distribution@pvalue(coin_PAI@statistic@teststatistic)
coin_cover_data <- AOI_df_noNA %>% dplyr::select(int_build,cover)%>% 
  pivot_longer(cols = cover)
coin_cover <- coin::independence_test(value ~ int_build,
                                      data = coin_cover_data,distribution="asymptotic");coin_cover
coin_cover@distribution@pvalue(coin_cover@statistic@teststatistic)

coin_FHD_data <- AOI_df_noNA %>% dplyr::select(int_build,FHD)%>% 
  pivot_longer(cols = FHD)
coin_FHD <- coin::independence_test(value ~ int_build,
                                    data = coin_FHD_data,distribution="asymptotic");coin_FHD
coin_FHD@distribution@pvalue(coin_FHD@statistic@teststatistic)


sigs$p_twotailed_coin <- c(coin_PAI@distribution@pvalue(coin_PAI@statistic@teststatistic),
                           coin_cover@distribution@pvalue(coin_cover@statistic@teststatistic),
                           coin_FHD@distribution@pvalue(coin_FHD@statistic@teststatistic))

sigs_nonbajo <- sigs
write.csv2(sigs_nonbajo,"sigs_nonbajo.csv")

#### 3 : Wilcoxon rank sum test ####
# Note: Here we compare the built footprints not to all AOI footprints, but just the unbuilt footprints
PAI_wilcox <- wilcox.test(AOI_df_noNA_built$PAI, AOI_df_noNA_nonbuilt$PAI, alternative = "two.sided", var.equal = FALSE)
FHD_wilcox <- wilcox.test(AOI_df_noNA_built$FHD, AOI_df_noNA_nonbuilt$FHD, alternative = "two.sided", var.equal = FALSE)
cover_wilcox <- wilcox.test(AOI_df_noNA_built$cover, AOI_df_noNA_nonbuilt$cover, alternative = "two.sided", var.equal = FALSE)

sigs$p_twotailed_wilcox <- c(PAI_wilcox$p.value,cover_wilcox$p.value,FHD_wilcox$p.value)

#write the significances to a csv
write.csv2(sigs,"Data/L2B_Statistics/significances_A.csv")


#### 4 : Permutation test on build-bifferences ####
# Here, we consider no neighbors, so it is okay if the NAs are removed before matching
Analysis_df_noNA <- Analysis_df %>% drop_na()%>%  dplyr::select(int_build,beam_pair,PAI,cover,FHD) %>% group_by(beam_pair) %>% filter(n() == 2) %>% ungroup

# One function for getting us the statistics we need
# differences of means
# calculate_basic_param_statistics_diff_mean <- function(data){
#   data %>% group_by(int_build) %>% 
#     summarise_all(list(mean=mean)) %>% summarise_all(diff) %>% dplyr::select(-int_build) 
# }

# One other function for getting us the statistics we need
# mean of differences
calculate_basic_param_statistics_mean_diff <- function(data){
  data %>% group_by(beam_pair) %>% arrange(int_build) %>% 
    summarise_all(list(diff=diff)) %>% ungroup() %>% summarise(mean(PAI_diff),mean(cover_diff),mean(FHD_diff))
}


# One function for doing the same, but randomized!
# calculate_basic_param_statistics_shuffled_diff_mean <- function(data){
# data %>%
#   mutate(int_build=sample(int_build)) %>%  #randomly reorder the int_build column, thereby randomly assigning all values
#   calculate_basic_param_statistics_diff_mean
# }

# One other function for doing the same, but randomized!
calculate_basic_param_statistics_shuffled_mean_diff <- function(data){
  data %>%
    mutate(int_build=sample(int_build)) %>%  #randomly reorder the int_build column, thereby randomly assigning all values
    calculate_basic_param_statistics_mean_diff
}


measured_stats <- Analysis_df_noNA %>% calculate_basic_param_statistics_mean_diff()

permuted_random_stats <- replicate((99999+1),calculate_basic_param_statistics_shuffled_mean_diff(Analysis_df_noNA),simplify = F) %>% do.call(rbind,.)

permuted_random_stats_long <- permuted_random_stats %>% 
  pivot_longer(cols = everything())

# retransform for the vlines
mea_sta <- data.frame(name=names(measured_stats),value=as.numeric(t(as.data.frame(measured_stats))))

#make the pseudo p values
p_values <- list()
for(i in 1:ncol(measured_stats)){
  p_values[[i]] <- (table(permuted_random_stats[,i]>measured_stats[[i]])[2])/(99999+1)
}
names(p_values) <- names(measured_stats)
p_values_onetail <- do.call(rbind,p_values)
mea_sta$p_onetailed <- p_values_onetail[,]
p_values <- list()
for(i in 1:ncol(measured_stats)){
  p_values[[i]] <- (table(abs(permuted_random_stats[,i])>abs(measured_stats[[i]]))[2])/(99999+1)
}
names(p_values) <- names(measured_stats)
p_values_twotail <- do.call(rbind,p_values) 
mea_sta$p_twotailed <- p_values_twotail[,]#the nonsense-index automatically drop the columns inherent "NA" rowname


# Optionally, examine the histograms
# Option2_Histograms <- ggplot(permuted_random_stats_long, aes(x =value )) + 
#   geom_density_line(fill = "#56B4E9", color = "blue", kernel = "gaussian",bw=0.001) +
#   geom_vline(data = mea_sta, aes(xintercept = value,col="red")) +
#   geom_text(data = mea_sta, aes(x=value,y=0.5,label=round(value,5)))+
#   geom_label(data = mea_sta, aes(x=+Inf,y=+Inf,label=paste("p_onetailed: ",round(p_onetailed,5)," ","\np_twotailed: ",round(p_twotailed,5)," ")),nudge_y = 20,vjust=1,hjust=1)+
#   facet_wrap(~ name,scales = "free",ncol=2)+
#   theme(legend.position = "None")
# 
# 
# png(filename = "Data/L2B_Statistics/Visual/Plots/Option2_Histograms.png")
# Option2_Histograms
# dev.off()
write.csv2(mea_sta,"mea_sta_paired.csv")

##### 5 : paired 2 sample t-test ####
#checking for normal distribution

hist(AOI_df_noNA$PAI,breaks=25)
shapiro.test(AOI_df_noNA$PAI) #significant
shapiro.test(Analysis_df_noNA$PAI)

hist(AOI_df_noNA$cover,breaks=25)
shapiro.test(AOI_df_noNA$cover) 
shapiro.test(Analysis_df_noNA$cover) 

hist(AOI_df_noNA$FHD,breaks=25)
shapiro.test(AOI_df_noNA$FHD) 
shapiro.test(Analysis_df_noNA$FHD) 
#Conclusion: All asymptoticly normally distributed, we can continue


# get a dataframe of only those observations which are paired
Analysis_df_only_paired <- Analysis_df %>% 
  dplyr::select(int_build,beam_pair,PAI,cover,FHD) %>% 
  group_by(beam_pair) %>%
  drop_na() %>% 
  arrange(int_build,.by_group=TRUE) %>%
  mutate(n_part=n()) %>%   #calculate number of pair members (should be 2, but after dropping NAs some could be 1)
  filter(n_part==2) %>%   #remove loners
  ungroup()

# do the t-test
built_pai <- Analysis_df_only_paired$PAI[Analysis_df_only_paired$int_build==0]
nonbuit_pai <- Analysis_df_only_paired$PAI[Analysis_df_only_paired$int_build==2]
t.test(x=built_pai,y = nonbuit_pai,paired = T)

built_fhd <- Analysis_df_only_paired$FHD[Analysis_df_only_paired$int_build==0]
nonbuit_fhd <- Analysis_df_only_paired$FHD[Analysis_df_only_paired$int_build==2]
t.test(x=built_fhd,y = nonbuit_fhd,paired = T)

built_cover <- Analysis_df_only_paired$cover[Analysis_df_only_paired$int_build==0]
nonbuit_cover <- Analysis_df_only_paired$cover[Analysis_df_only_paired$int_build==2]
t.test(x=built_cover,y = nonbuit_cover,paired = T)

####  6 : MANOVA #########
#Guided by: https://www.datanovia.com/en/lessons/one-way-manova-in-r/
library(tidyverse)
library(ggpubr)
library(rstatix)
library(car)
library(broom)

manova(cbind(PAI, FHD, cover)  ~ int_build, data=Analysis_df_noNA) %>% summary()

#### 7 : Scatterplot ####

df <- AOI_df_noNA
df %>% ggplot(aes(x=FHD,y=cover,col=as.factor(int_build)))+
  geom_point()

#### 8 : Spatial Autocorrelation####
library(spdep)
library(sf)
AOI_points_df_noNA <- st_read("Data/L2B_Statistics/points_AOI_spdf_v2.shp")%>% drop_na()%>%  dplyr::select(int_bld,PAI,cover,FHD) %>% st_transform(32616)

# select all points in a 180 meter distance as neighbors
nb <- spdep::dnearneigh(x = AOI_points_df_noNA,d1 = 0,d2 = 180)
nblist <- nb2listw(nb,zero.policy = TRUE) #allow empty neighbors

# test for spatial autocorrelation
moran.test(x = AOI_points_df_noNA$cover,listw = nblist,zero.policy = T)
moran.test(x = AOI_points_df_noNA$FHD,listw = nblist,zero.policy = T)
moran.test(x = AOI_points_df_noNA$PAI,listw = nblist,zero.policy = T)

# morans scatterplot,  example PAI
mp <- moran.plot(x = AOI_points_df_noNA$PAI,listw = nblist,zero.policy = T)

xname <- attr(mp, "xname")
mp$int_build <- AOI_points_df_noNA$int_bld
ggplot(mp, aes(x=x, y=wx,color=int_build)) + geom_point(shape=1) + 
  geom_smooth(formula=y ~ x, method="lm") + 
  geom_hline(yintercept=mean(mp$wx), lty=2) + 
  geom_vline(xintercept=mean(mp$x), lty=2) + theme_bw() + 
  geom_point(data=mp[mp$is_inf,], aes(x=x, y=wx), shape=9) +
  geom_text(data=mp[mp$is_inf,], aes(x=x, y=wx, label=labels, vjust=1.5)) +
  xlab("PAI") + ylab(paste0("Spatially lagged PAI"))+
  ggtitle("Moran's Scatterplot","PAI of points in AOI vs the point's neighbors (180 m distance neighbors)")
