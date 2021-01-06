######################################################################################
# Elevation transects        
# Create transect profile plots comparing the elevation 
# from ALS and GEDI data
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
# Prepare the inputs 
# Get the basic extent
library(raster)
aoi <- rgdal::readOGR("Data/Ancillary_Data/ALS/outline_area.shp")
e_wgs84 <- extent(sp::spTransform(aoi, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")))

min_lon = e_wgs84[1]
max_lon = e_wgs84[2]
min_lat = e_wgs84[3]
max_lat = e_wgs84[4]

# Find and check the L2B data
dl_dir <- "F:/GEDI_Downloads_Yucatan/L2B_ALS/"  #The dir where the downloads can be found
L2B_files <- list.files("F:/GEDI_Downloads_Yucatan/L2B_ALS/",pattern = "02_B",full.names = T)
L2B_structure <- h5ls(L2B_files[[1]], recursive = T)

####2 : Prepare GEDI inputs ####
# Extract the layers from the GEDI L2B hdf files
# Coordinates
lon_lowestmode_aoi <- extract_GEDI_attribute_by_bbox(L2B_files,
                                                     desired_parameter_pattern = "lon_lowestmode",
                                                     min_long = min_lon,
                                                     max_long = max_lon,
                                                     min_lat = min_lat,
                                                     max_lat = max_lat)
lat_lowestmode_aoi <- extract_GEDI_attribute_by_bbox(L2B_files,
                                                     desired_parameter_pattern = "lat_lowestmode",
                                                     min_long = min_lon,
                                                     max_long = max_lon,
                                                     min_lat = min_lat,
                                                     max_lat = max_lat)
# elevation
elev_lowestmode_aoi <- extract_GEDI_attribute_by_bbox(L2B_files,
                                                      desired_parameter_pattern = "elev_lowestmode",
                                                      min_long = min_lon,
                                                      max_long = max_lon,
                                                      min_lat = min_lat,
                                                      max_lat = max_lat)
# alternative elevation
elevation_lastbin_aoi <- extract_GEDI_attribute_by_bbox(L2B_files,
                                                      desired_parameter_pattern = "elevation_lastbin",
                                                      min_long = min_lon,
                                                      max_long = max_lon,
                                                      min_lat = min_lat,
                                                      max_lat = max_lat)

# quality flags
l2a_quality_flag_aoi <- extract_GEDI_attribute_by_bbox(L2B_files,desired_parameter_pattern = "l2a_quality_flag",
                                                       min_long = min_lon,
                                                       max_long = max_lon,
                                                       min_lat = min_lat,
                                                       max_lat = max_lat)

# bind the layers together in a tibble
elev_points <- tibble(lon_lowestmode_aoi,lat_lowestmode_aoi,elev_lowestmode_aoi,elevation_lastbin_aoi,l2a_quality_flag_aoi,.name_repair = "minimal")
# convert it to a point object
coordinates(elev_points)<- ~lon_lowestmode1+lat_lowestmode1
projection(elev_points) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")


#### 3 : Create the Plots ####
 
# Set global colors
transectred <- rgb(255/255, 86/255, 43/255, 0.8)
alsblue <- "dodgerblue2"
ruingreen <- rgb(1/255,255/255,1/255,0.3)

####A####
T2_original <- make_transsect_plot(transects_path = "Data/Ancillary_Data/ALS/Transsects/GEDI_Chactun_transects_points_UTM_1.shp",
                    elev_points = elev_points, 
                    ALS_csv_path = "Data/Ancillary_Data/ALS/Transsects/Chactun_GEDI_ALS_profileA.csv",
                    plot_title = "Transect 2",
                    plot_legpos = c(0.5, 0.15),
                    remove_outliers = F,
                    remove_lowquality = F,
                    plot_rectcol = ruingreen,plot_alscol = "dodgerblue2",plot_gedicol = transectred,
                    linepath = "Data/Ancillary_Data/ALS/Transsects/GEDI_Chactun_transects_lines_A.shp",
                    ruinspath = "Data/Ancillary_Data/My_Ruins/buildings_150m_within_transects_cut_to_pieces.shp")

T2_outl <- make_transsect_plot(transects_path = "Data/Ancillary_Data/ALS/Transsects/GEDI_Chactun_transects_points_UTM_1.shp",
                    elev_points = elev_points, 
                    ALS_csv_path = "Data/Ancillary_Data/ALS/Transsects/Chactun_GEDI_ALS_profileA.csv",
                    plot_title = "Transect 2 without Outliers",
                    plot_legpos = c(0.5, 0.15),
                    remove_outliers = T,
                    remove_lowquality = F,
                    plot_rectcol = ruingreen,plot_alscol = "dodgerblue2",plot_gedicol = transectred,
                    linepath = "Data/Ancillary_Data/ALS/Transsects/GEDI_Chactun_transects_lines_A.shp",
                    ruinspath = "Data/Ancillary_Data/My_Ruins/buildings_150m_within_transects_cut_to_pieces.shp")

T2_lq <- make_transsect_plot(transects_path = "Data/Ancillary_Data/ALS/Transsects/GEDI_Chactun_transects_points_UTM_1.shp",
                             elev_points = elev_points, 
                             ALS_csv_path = "Data/Ancillary_Data/ALS/Transsects/Chactun_GEDI_ALS_profileA.csv",
                             plot_title = "Transect 2 without LQ flagged points",
                             plot_legpos = c(0.5, 0.15),
                             remove_outliers = F,
                             remove_lowquality = T,
                             plot_rectcol = ruingreen,plot_alscol = "dodgerblue2",plot_gedicol = transectred,
                             linepath = "Data/Ancillary_Data/ALS/Transsects/GEDI_Chactun_transects_lines_A.shp",
                             ruinspath = "Data/Ancillary_Data/My_Ruins/buildings_150m_within_transects_cut_to_pieces.shp")

T2_outl_lq <- make_transsect_plot(transects_path = "Data/Ancillary_Data/ALS/Transsects/GEDI_Chactun_transects_points_UTM_1.shp",
                    elev_points = elev_points, 
                    ALS_csv_path = "Data/Ancillary_Data/ALS/Transsects/Chactun_GEDI_ALS_profileA.csv",
                    plot_title = "Transect 2 without Outliers or LQ flagged points",
                    plot_legpos = c(0.5, 0.15),
                    remove_outliers = T,
                    remove_lowquality = T,
                    plot_rectcol = ruingreen,plot_alscol = "dodgerblue2",plot_gedicol = transectred,
                    linepath = "Data/Ancillary_Data/ALS/Transsects/GEDI_Chactun_transects_lines_A.shp",
                    ruinspath = "Data/Ancillary_Data/My_Ruins/buildings_150m_within_transects_cut_to_pieces.shp")

T2 <- grid.arrange(T2_original, T2_outl,T2_lq,T2_outl_lq,ncol=1)


####B#### 
T3_original <- make_transsect_plot(transects_path = "Data/Ancillary_Data/ALS/Transsects/GEDI_Chactun_transects_points_UTM_2.shp",
                                   elev_points = elev_points, 
                                   ALS_csv_path = "Data/Ancillary_Data/ALS/Transsects/Chactun_GEDI_ALS_profileC.csv",
                                   plot_title = "Transect 3",
                                   plot_legpos = c(0.5, 0.15),
                                   remove_outliers = F,
                                   remove_lowquality = F,
                                   plot_rectcol = ruingreen,plot_alscol = "dodgerblue2",plot_gedicol = transectred,
                                   linepath = "Data/Ancillary_Data/ALS/Transsects/GEDI_Chactun_transects_lines_C.shp",
                                   ruinspath = "Data/Ancillary_Data/My_Ruins/buildings_150m_within_transects_cut_to_pieces.shp")

T3_outl <- make_transsect_plot(transects_path = "Data/Ancillary_Data/ALS/Transsects/GEDI_Chactun_transects_points_UTM_2.shp",
                               elev_points = elev_points, 
                               ALS_csv_path = "Data/Ancillary_Data/ALS/Transsects/Chactun_GEDI_ALS_profileC.csv",
                               plot_title = "Transect 3 without Outliers",
                               plot_legpos = c(0.5, 0.15),
                               remove_outliers = T,
                               remove_lowquality = F,
                               plot_rectcol = ruingreen,plot_alscol = "dodgerblue2",plot_gedicol = transectred,
                               linepath = "Data/Ancillary_Data/ALS/Transsects/GEDI_Chactun_transects_lines_C.shp",
                               ruinspath = "Data/Ancillary_Data/My_Ruins/buildings_150m_within_transects_cut_to_pieces.shp")

T3_lq <- make_transsect_plot(transects_path = "Data/Ancillary_Data/ALS/Transsects/GEDI_Chactun_transects_points_UTM_2.shp",
                             elev_points = elev_points, 
                             ALS_csv_path = "Data/Ancillary_Data/ALS/Transsects/Chactun_GEDI_ALS_profileC.csv",
                             plot_title = "Transect 3 without LQ flagged points",
                             plot_legpos = c(0.5, 0.15),
                             remove_outliers = F,
                             remove_lowquality = T,
                             plot_rectcol = ruingreen,plot_alscol = "dodgerblue2",plot_gedicol = transectred,
                             linepath = "Data/Ancillary_Data/ALS/Transsects/GEDI_Chactun_transects_lines_C.shp",
                             ruinspath = "Data/Ancillary_Data/My_Ruins/buildings_150m_within_transects_cut_to_pieces.shp")

T3_outl_lq <- make_transsect_plot(transects_path = "Data/Ancillary_Data/ALS/Transsects/GEDI_Chactun_transects_points_UTM_2.shp",
                                  elev_points = elev_points, 
                                  ALS_csv_path = "Data/Ancillary_Data/ALS/Transsects/Chactun_GEDI_ALS_profileC.csv",
                                  plot_title = "Transect 3 without Outliers or LQ flagged points",
                                  plot_legpos = c(0.5, 0.15),
                                  remove_outliers = T,
                                  remove_lowquality = T,
                                  plot_rectcol = ruingreen,plot_alscol = "dodgerblue2",plot_gedicol = transectred,
                                  linepath = "Data/Ancillary_Data/ALS/Transsects/GEDI_Chactun_transects_lines_C.shp",
                                  ruinspath = "Data/Ancillary_Data/My_Ruins/buildings_150m_within_transects_cut_to_pieces.shp")

T3 <- grid.arrange(T3_original, T3_outl,T3_lq,T3_outl_lq,ncol=1)

####C#### 

T4_original <- make_transsect_plot(transects_path = "Data/Ancillary_Data/ALS/Transsects/GEDI_Chactun_transects_points_UTM_3.shp",
                                   elev_points = elev_points, 
                                   ALS_csv_path = "Data/Ancillary_Data/ALS/Transsects/Chactun_GEDI_ALS_profileD.csv",
                                   plot_title = "Transect 4",
                                   plot_legpos = c(0.5, 0.15),
                                   remove_outliers = F,
                                   remove_lowquality = F,
                                   plot_rectcol = ruingreen,plot_alscol = "dodgerblue2",plot_gedicol = transectred,
                                   linepath = "Data/Ancillary_Data/ALS/Transsects/GEDI_Chactun_transects_lines_D.shp",
                                   ruinspath = "Data/Ancillary_Data/My_Ruins/buildings_150m_within_transects_cut_to_pieces.shp")

T4_outl <- make_transsect_plot(transects_path = "Data/Ancillary_Data/ALS/Transsects/GEDI_Chactun_transects_points_UTM_3.shp",
                               elev_points = elev_points, 
                               ALS_csv_path = "Data/Ancillary_Data/ALS/Transsects/Chactun_GEDI_ALS_profileD.csv",
                               plot_title = "Transect 4 without Outliers",
                               plot_legpos = c(0.5, 0.15),
                               remove_outliers = T,
                               remove_lowquality = F,
                               plot_rectcol = ruingreen,plot_alscol = "dodgerblue2",plot_gedicol = transectred,
                               linepath = "Data/Ancillary_Data/ALS/Transsects/GEDI_Chactun_transects_lines_D.shp",
                               ruinspath = "Data/Ancillary_Data/My_Ruins/buildings_150m_within_transects_cut_to_pieces.shp")

T4_lq <- make_transsect_plot(transects_path = "Data/Ancillary_Data/ALS/Transsects/GEDI_Chactun_transects_points_UTM_3.shp",
                             elev_points = elev_points, 
                             ALS_csv_path = "Data/Ancillary_Data/ALS/Transsects/Chactun_GEDI_ALS_profileD.csv",
                             plot_title = "Transect 4 without LQ flagged points",
                             plot_legpos = c(0.5, 0.15),
                             remove_outliers = F,
                             remove_lowquality = T,
                             plot_rectcol = ruingreen,plot_alscol = "dodgerblue2",plot_gedicol = transectred,
                             linepath = "Data/Ancillary_Data/ALS/Transsects/GEDI_Chactun_transects_lines_D.shp",
                             ruinspath = "Data/Ancillary_Data/My_Ruins/buildings_150m_within_transects_cut_to_pieces.shp")

T4_outl_lq <- make_transsect_plot(transects_path = "Data/Ancillary_Data/ALS/Transsects/GEDI_Chactun_transects_points_UTM_3.shp",
                                  elev_points = elev_points, 
                                  ALS_csv_path = "Data/Ancillary_Data/ALS/Transsects/Chactun_GEDI_ALS_profileD.csv",
                                  plot_title = "Transect 4 without Outliers or LQ flagged points",
                                  plot_legpos = c(0.5, 0.15),
                                  remove_outliers = T,
                                  remove_lowquality = T,
                                  plot_rectcol = ruingreen,plot_alscol = "dodgerblue2",plot_gedicol = transectred,
                                  linepath = "Data/Ancillary_Data/ALS/Transsects/GEDI_Chactun_transects_lines_D.shp",
                                  ruinspath = "Data/Ancillary_Data/My_Ruins/buildings_150m_within_transects_cut_to_pieces.shp")

T4 <- grid.arrange(T4_original, T4_outl,T4_lq,T4_outl_lq,ncol=1)

####D#### 

T1_original <- make_transsect_plot(transects_path = "Data/Ancillary_Data/ALS/Transsects/GEDI_Chactun_transects_points_UTM_4.shp",
                                   elev_points = elev_points, 
                                   ALS_csv_path = "Data/Ancillary_Data/ALS/Transsects/Chactun_GEDI_ALS_profileB.csv",
                                   plot_title = "Transect 1",
                                   plot_legpos = c(0.5, 0.15),
                                   remove_outliers = F,
                                   remove_lowquality = F,
                                   plot_rectcol = ruingreen,plot_alscol = "dodgerblue2",plot_gedicol = transectred,
                                   linepath = "Data/Ancillary_Data/ALS/Transsects/GEDI_Chactun_transects_lines_B.shp",
                                   ruinspath = "Data/Ancillary_Data/My_Ruins/buildings_150m_within_transects_cut_to_pieces.shp")

T1_outl <- make_transsect_plot(transects_path = "Data/Ancillary_Data/ALS/Transsects/GEDI_Chactun_transects_points_UTM_4.shp",
                               elev_points = elev_points, 
                               ALS_csv_path = "Data/Ancillary_Data/ALS/Transsects/Chactun_GEDI_ALS_profileB.csv",
                               plot_title = "Transect 1 without Outliers",
                               plot_legpos = c(0.5, 0.15),
                               remove_outliers = T,
                               remove_lowquality = F,
                               plot_rectcol = ruingreen,plot_alscol = "dodgerblue2",plot_gedicol = transectred,
                               linepath = "Data/Ancillary_Data/ALS/Transsects/GEDI_Chactun_transects_lines_B.shp",
                               ruinspath = "Data/Ancillary_Data/My_Ruins/buildings_150m_within_transects_cut_to_pieces.shp")

T1_lq <- make_transsect_plot(transects_path = "Data/Ancillary_Data/ALS/Transsects/GEDI_Chactun_transects_points_UTM_4.shp",
                             elev_points = elev_points, 
                             ALS_csv_path = "Data/Ancillary_Data/ALS/Transsects/Chactun_GEDI_ALS_profileB.csv",
                             plot_title = "Transect 1 without LQ flagged points",
                             plot_legpos = c(0.5, 0.15),
                             remove_outliers = F,
                             remove_lowquality = T,
                             plot_rectcol = ruingreen,plot_alscol = "dodgerblue2",plot_gedicol = transectred,
                             linepath = "Data/Ancillary_Data/ALS/Transsects/GEDI_Chactun_transects_lines_B.shp",
                             ruinspath = "Data/Ancillary_Data/My_Ruins/buildings_150m_within_transects_cut_to_pieces.shp")

T1_outl_lq <- make_transsect_plot(transects_path = "Data/Ancillary_Data/ALS/Transsects/GEDI_Chactun_transects_points_UTM_4.shp",
                                  elev_points = elev_points, 
                                  ALS_csv_path = "Data/Ancillary_Data/ALS/Transsects/Chactun_GEDI_ALS_profileB.csv",
                                  plot_title = "Transect 1 without Outliers or LQ flagged points",
                                  plot_legpos = c(0.5, 0.15),
                                  remove_outliers = T,
                                  remove_lowquality = T,
                                  plot_rectcol = ruingreen,plot_alscol = "dodgerblue2",plot_gedicol = transectred,
                                  linepath = "Data/Ancillary_Data/ALS/Transsects/GEDI_Chactun_transects_lines_B.shp",
                                  ruinspath = "Data/Ancillary_Data/My_Ruins/buildings_150m_within_transects_cut_to_pieces.shp")



#### 4 : Outputs ####
# combine plots and write to png

png(filename = "Report/Figures/Transects/T1_all.png",width = 950*2,height = 800*2,units = "px",res = 72*2)
grid.arrange(T1_original[[1]]+labs(title = "Comparison of ALS and GEDI elevation profiles",
                                   subtitle = "Transect 1"),
             T1_outl[[1]]+ylim(c(266,282))+labs(title = "Comparison of ALS and GEDI elevation profiles",
                                                subtitle = "Transect 1 without outliers"),
             T1_lq[[1]]+ylim(c(266,282))+labs(title = "Comparison of ALS and GEDI elevation profiles",
                                              subtitle = "Transect 1 without low quality points"),
             ncol=1)
dev.off()
png(filename = "Report/Figures/Transects/T2_all.png",width = 950*2,height = 800*2,units = "px",res = 72*2)
grid.arrange(T2_original[[1]]+labs(title = "Comparison of ALS and GEDI elevation profiles",
                                   subtitle = "Transect 2"),
             T2_outl[[1]]+labs(title = "Comparison of ALS and GEDI elevation profiles",
                               subtitle = "Transect 2 without outliers"),
             T2_lq[[1]]+labs(title = "Comparison of ALS and GEDI elevation profiles",
                             subtitle = "Transect 2 without low quality points"),
             ncol=1)
dev.off()
png(filename = "Report/Figures/Transects/T2_all_w_elev_lastbin.png",width = 950*2,height = 800*2,units = "px",res = 72*2)
grid.arrange(T2_original[[3]]+labs(title = "Comparison of ALS and GEDI elevation profiles",
                                   subtitle = "Transect 2"),
             T2_outl[[3]]+labs(title = "Comparison of ALS and GEDI elevation profiles",
                               subtitle = "Transect 2 without outliers"),
             T2_lq[[3]]+labs(title = "Comparison of ALS and GEDI elevation profiles",
                             subtitle = "Transect 2 without low quality points"),
             ncol=1)
dev.off()
png(filename = "Report/Figures/Transects/T3_all.png",width = 950*2,height = 800*2,units = "px",res = 72*2)
grid.arrange(T3_original[[1]]+labs(title = "Comparison of ALS and GEDI elevation profiles",
                                   subtitle = "Transect 3"),
             T3_outl[[1]]+labs(title = "Comparison of ALS and GEDI elevation profiles",
                               subtitle = "Transect 3 without outliers"),
             T3_lq[[1]]+labs(title = "Comparison of ALS and GEDI elevation profiles",
                             subtitle = "Transect 3 without low quality points"),
             ncol=1)
dev.off()
png(filename = "Report/Figures/Transects/T4_all.png",width = 950*2,height = 800*2,units = "px",res = 72*2)
grid.arrange(T4_original[[1]]+labs(title = "Comparison of ALS and GEDI elevation profiles",
                                   subtitle = "Transect 4"),
             T4_outl[[1]]+labs(title = "Comparison of ALS and GEDI elevation profiles",
                               subtitle = "Transect 4 without outliers"),
             T4_lq[[1]]+labs(title = "Comparison of ALS and GEDI elevation profiles",
                             subtitle = "Transect 4 without low quality points"),
             ncol=1)
dev.off()


# Saving the individual figure components
library(ggplot2)
ggsave(file="Report/Figures/Transects/Figure_Components/T1/T1a.svg",
       plot=T1_original[[1]]+labs(title = "Comparison of ALS and GEDI elevation profiles",
                                  subtitle = "Transect 1"),
       width=9.5, height=3)
ggsave(file="Report/Figures/Transects/Figure_Components/T1/T1b.svg",
       plot=T1_outl[[1]]+labs(title = "Comparison of ALS and GEDI elevation profiles",
                              subtitle = "Transect 1 without outliers"),
       width=9.5, height=3)
ggsave(file="Report/Figures/Transects/Figure_Components/T1/T1c.svg",
       plot=T1_lq[[1]]+labs(title = "Comparison of ALS and GEDI elevation profiles",
                            subtitle = "Transect 1 without low quality points"),
       width=9.5, height=3)


ggsave(file="Report/Figures/Transects/Figure_Components/T2/T2a.svg",
       plot=T2_original[[1]]+labs(title = "Comparison of ALS and GEDI elevation profiles",
                                  subtitle = "Transect 2"),
       width=9.5, height=3)
ggsave(file="Report/Figures/Transects/Figure_Components/T2/T2b.svg",
       plot=T2_outl[[1]]+labs(title = "Comparison of ALS and GEDI elevation profiles",
                              subtitle = "Transect 2 without outliers"),
       width=9.5, height=3)
ggsave(file="Report/Figures/Transects/Figure_Components/T2/T2c.svg",
       plot=T2_lq[[1]]+labs(title = "Comparison of ALS and GEDI elevation profiles",
                            subtitle = "Transect 2 without low quality points"),
       width=9.5, height=3)



ggsave(file="Report/Figures/Transects/Figure_Components/T3/T3a.svg",
       plot=T3_original[[1]]+labs(title = "Comparison of ALS and GEDI elevation profiles",
                                  subtitle = "Transect 3"),
       width=9.5, height=3)
ggsave(file="Report/Figures/Transects/Figure_Components/T3/T3b.svg",
       plot=T3_outl[[1]]+labs(title = "Comparison of ALS and GEDI elevation profiles",
                              subtitle = "Transect 3 without outliers"),
       width=9.5, height=3)
ggsave(file="Report/Figures/Transects/Figure_Components/T3/T3c.svg",
       plot=T3_lq[[1]]+labs(title = "Comparison of ALS and GEDI elevation profiles",
                            subtitle = "Transect 3 without low quality points"),
       width=9.5, height=3)



ggsave(file="Report/Figures/Transects/Figure_Components/T4/T4a.svg",
       plot=T4_original[[1]]+labs(title = "Comparison of ALS and GEDI elevation profiles",
                                  subtitle = "Transect 4"),
       width=9.5, height=3)
ggsave(file="Report/Figures/Transects/Figure_Components/T4/T4b.svg",
       plot=T4_outl[[1]]+labs(title = "Comparison of ALS and GEDI elevation profiles",
                                  subtitle = "Transect 4 without outliers"),
       width=9.5, height=3)
ggsave(file="Report/Figures/Transects/Figure_Components/T4/T4c.svg",
       plot=T4_lq[[1]]+labs(title = "Comparison of ALS and GEDI elevation profiles",
                           subtitle = "Transect 4 without low quality points"),
       width=9.5, height=3)


#### 5 : Addendum ####
# Quality check on outliers: Calculate overlap between outliers and Low Quality points
# in a confusion matrix
all_ts_df <- rbind(T1_original[[2]],T2_original[[2]],T3_original[[2]],T4_original[[2]]) %>% 
  filter(source=="GEDI") %>% 
  mutate(is_low_quality=as.factor(as.numeric(1-(as.numeric(quality)-1)))) %>% 
  mutate(is_outlier=as.factor(as.numeric(outliers)))
caret::confusionMatrix(all_ts_df$is_low_quality,all_ts_df$is_outlier)
