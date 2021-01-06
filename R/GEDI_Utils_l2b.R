######################################################################################
#         
# Utility functions for the processing of GEDI L2B Data.
# Most of these are related to extracting and visualising data:
#
# - make_transsect_plot() : Plots ALS elevation vs GEDI elevation in a profile along a transect.
# - get_clip_distance_along_straight_line() : Utility which calculates
# intersections between a line and object
# - extract_GEDI_attribute_by_bbox() :Extracts parameters(layers) 
# for every point from a L2B hdf file. 
# - extract_GEDI_points() : Extracts parameters(layers) for every point from
# a L2B hdf file. (deprecated, use extract_GEDI_attribute_by_bbox)
# - extract_GEDI_points_wrapper(): Wrapper for the above which additonally 
# subsets by bbox (deprecated, use extract_GEDI_attribute_by_bbox)
# - query_GEDI_download_links() : Query GEDI download links from the LP DAAC Server.
#
# Project Contributors:
# Žiga Kokalj, Research Centre of the Slovenian Academy of Sciences and Arts 
# Johannes Mast, University of Würzburg
#
# This research was partly funded by the 
# Slovenian Research Agency core funding No. P2-0406.
######################################################################################


library(rhdf5)
library(rgdal)
library(tidyverse)
#' make_transsect_plot
#' @description Creates a specific type of transect plot GEDI Points along a ALS-DEM points transect with a different number of points.
#' Only recommended if the two transects are in reality overlapping. Little divergences are inconsequential but larger ones lead to
#' misleading plots. This is very specific to the data formats used within this project and may well not be useful outside of its scope.
#' @param transects_path Path to the shapefile containing the ALS-DEM points 
#' @param elev_points Path to a points shapefile containing the GEDI Points 
#' @param ALS_csv_path Path to a csv file which lists for certain distances from the transect-start the relevant DEM elevation.
#' @param plot_title Optional plot title
#' @param plot_legpos legend position
#' @param plot_rectcol color of the rectangle which indicates presence of ruins.
#' @param remove_outliers remove outliers before plotting?
#' @param remove_lowquality remove points marked as low quality before plotting?
#' @param plot_alscol Color of ALS-DEM points
#' @param plot_gedicol Color of GEDI-Points
#' @param linepath path to a line shapefile which should approximate the transect. Used for calculating intersections with ruins.
#' @param ruinspath Path to polygon shapefile of ruins which may intersect the transect-line.
#' @return A list with [[1]] The transect without elev_lastbin [[2]] The dataframe used for creating the plot [[3]] the transect plot with elev_lastbin
make_transsect_plot <- function(transects_path,
                                elev_points, 
                                ALS_csv_path, 
                                plot_title = "Transect",
                                plot_legpos = c(0.5, 0.15),
                                plot_rectcol="green",
                                remove_outliers = T,
                                remove_lowquality = T,
                                plot_alscol="orange",
                                plot_gedicol="dodgerblue2",
                                linepath=NULL,
                                ruinspath=NULL){
  
  
  
  #First, read in points. Calculate the distances starting from the southernmost
  ts_points_T1 <- readOGR(transects_path) 
  #we first have to order them, starting from the southernmost
  ts_points_T1_reordered <- ts_points_T1[order(ts_points_T1@coords[,1]),]
  
  ts_dists_T1 <- dist(ts_points_T1_reordered@coords) %>% 
    as.matrix()
  #We take the diagonal of the matrix nudged by 1 (so we always get the distance to the next neighbor)
  ts_cumdists_T1 <- c(0,cumsum(diag(ts_dists_T1[,-1])))
  
  ts_footprints_T1 <- readOGR(transects_path) %>% 
    spTransform( CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")) %>% 
    buffer(width=12,dissolve=F)
  ts_footprints_T1_reordered <- ts_footprints_T1[order(ts_points_T1@coords[,1]),]
  ts_footprints_T1_reordered$cumdists <- ts_cumdists_T1
  
  #extract the elevation from the elev_points
  ts_footprints_T1_reordered$elev <- over(ts_footprints_T1_reordered,elev_points)[,1] %>% deframe()
  ts_footprints_T1_reordered$elevlastbin <- over(ts_footprints_T1_reordered,elev_points)[,2] %>% deframe()
  ts_footprints_T1_reordered$l2aqual <- over(ts_footprints_T1_reordered,elev_points)[,3] %>% deframe()

  #Make one DF for GEDi
  GEDI_cumdists <- ts_footprints_T1_reordered$cumdists
  GEDI_elevs <- ts_footprints_T1_reordered$elev
  GEDI_elevslastbin <- ts_footprints_T1_reordered$elevlastbin
  GEDI_quality <- as.character(ts_footprints_T1_reordered$l2aqual)
  GEDI_df <- data.frame(GEDI_cumdists,GEDI_elevs,GEDI_elevslastbin,GEDI_quality)
  GEDI_df$source <- "GEDI"  
  names(GEDI_df) <- c("cumdists","elev","elev_lastbin","quality","source")
  
  #Make one DF for ALS
  ALS_cumdists <- read.csv2(ALS_csv_path)[,1]
  ALS_elevs <- read.csv2(ALS_csv_path)[,2]
  ALS_elevslastbin <- rep(NA,length(ALS_elevs))
  ALS_df <- data.frame(ALS_cumdists,ALS_elevs,ALS_elevslastbin)
  ALS_df$quality <- NA
  ALS_df$source <- "ALS"
  names(ALS_df) <- c("cumdists","elev","elev_lastbin","quality","source")
  combined_df_T1 <- rbind(GEDI_df,ALS_df)

  #Find outliers. Optionally remove outliers and points of low quality in gedi.
  combined_df_T1$outliers <- is_outlier(combined_df_T1$elev)
  combined_df_filtered <- combined_df_T1 %>% 
    {if(remove_outliers) filter(.,!outliers) else .} %>% 
    
    {if(remove_lowquality) filter(.,is.na(as.numeric(as.character(quality))) | as.logical(as.numeric(as.character(quality)))) else .} 
  
  # get distances to plot
  ruindists <- get_clip_distance_along_straight_line(linepath,ruinspath) %>% as.data.frame()
  ruindists$V3 <- min(combined_df_filtered$elev)
  ruindists$V4 <- max(combined_df_filtered$elev)
  # pivot elevation column
  combined_df_filtered %>% pivot_longer(cols=starts_with("elev"))
  
  transect_T1_ol <-combined_df_filtered %>% 
    ggplot(aes(x=cumdists,y = elev,group=source,color=source,))+
    geom_point(aes(shape=quality,size=2),show.legend=!remove_lowquality)+
    geom_line(size=0.3)+ 
    geom_rect(data=ruindists, inherit.aes=FALSE,
              aes(xmin=V1,xmax=V2,ymin=V3,ymax=V4), fill = plot_rectcol,
              alpha=0.23,show.legend = F)+
    theme_bw()+
    xlab("distance along transect [m]")+
    ylab("elevation lowest mode [m]")+
    scale_color_manual(values = c(plot_alscol,plot_gedicol))+
    scale_shape_manual(breaks = c("0","1"),values = c(23,16))+
    ggtitle(plot_title)+ 
    theme(legend.position = plot_legpos)+
    guides(color=guide_legend(title="data \nsource"))+
    guides(shape=guide_legend(title="GEDI L2A \nquality flag"))+
    guides(size=FALSE)+
    guides(fill=FALSE)+
    theme(legend.direction = "horizontal", legend.box = "horizontal")+
    theme(legend.key = element_rect(fill = "white", colour = "black"))
  
  transect_T1_ol_with_lastbin <- transect_T1_ol+geom_line(aes(x=cumdists,y=elev_lastbin),linetype=2)
  return(list(transect_T1_ol,combined_df_filtered,transect_T1_ol_with_lastbin))
}


#' get_clip_distance_along_straight_line
#' @description Calculates intersections between ruins and a straight line.
#' @param linepath path to a line shapefile which should approximate the transect. Used for calculating intersections with ruins.
#' @param ruinspath Path to polygon shapefile of ruins which may intersect the transect-line.
#' @return A two column matrix containing coordinates (along the line) which signify the outer boundaries of the intersected ruins.
get_clip_distance_along_straight_line <- function(linepath,ruinspath){
  line <- readOGR(linepath) 
  ruins <- readOGR(ruinspath) %>% buffer(width=0.0,dissolve=F)
  
  # Intersect the line into a number of segments
  intlines <- rgeos::gIntersection(line,ruins)
  
  # For every line segments, collect the coordiantes
  intcoords <- list()
  for(i in 1: length(intlines@lines[[1]]@Lines)){
    coordints <-coordinates(intlines)[[1]][i][[1]]
    intcoords[[i]] <- rbind(coordints[1,],coordints[nrow(coordints),])
  }
  intcoords <- do.call(rbind.data.frame,intcoords)
  
  # Format the relevant coordinates into a matrix
  line_start_sw <- coordinates(line)[[1]][[1]][1,]
  intdists <- pointDistance(intcoords,line_start_sw,lonlat = F) %>% sort() %>% matrix(ncol=2,byrow=T)
  return(intdists)
  
}




#' extract_GEDI_attribute_by_bbox
#' @description From a number of GEDI L2B .hdf files, extract the desired fields within a certain lat-long bounding box.
#' @param desired_parameter_pattern The parameter to extract
#' @param lat_name The field containing the desired lat to filter by. Default is "lat_lowestmode"
#' @param lon_name The field containing the desired lon to filter by. Default is "lon_lowestmode"
#' @param paths path to the GEDI hdf file.
#' @param min_lon which is applied to lon_lowestmode to subset the points
#' @param max_lon which is applied to lon_lowestmode to subset the points
#' @param min_lat which is applied to lat_lowestmode to subset the points
#' @param max_lat which is applied to lat_lowestmode to subset the points
extract_GEDI_attribute_by_bbox <- function(paths, desired_parameter_pattern,min_long,max_long,min_lat,max_lat,lat_name="lat_lowestmode",lon_name="lon_lowestmode") {

  internal_extract <- function(path){
  #Get the internal structure of the file
  l2b_structure <- h5ls(path, recursive = T)
  #Read the group names
  l2b_groups <- unique(l2b_structure$name)
  #Read the beam designations
  l2b_beams <- l2b_groups[grepl("BEAM", l2b_groups)]
  
  #one field for lat and ond field for lon are temporarily required
  desired_parameter_pattern <- c(lat_name,lon_name,desired_parameter_pattern)
  
  
  #Extract and bind together the desired parameters for the desired beams
  df <- lapply(
    l2b_beams,
    FUN = function(current_beam)
    {
      #subset the structure, selecting only the ones for this beam
      beam_parameters <-
        l2b_structure[grepl(current_beam, l2b_structure$group), ]
      #Get a list of all the matching parameters for this beam
      parameter_matches <-
        lapply(
          desired_parameter_pattern,
          FUN = function(y) {
            #beam_parameters[grepl(y, beam_parameters$name), ]
            beam_parameters[beam_parameters$name %in% y,]
          }
        ) %>% do.call(rbind, .)
      
      #Merge the first two fields to get a list of paths to read in with h5read
      parameter_matches_m <-
        paste0(parameter_matches$group, "/", parameter_matches$name)
      
      #Read out the lats and the lons and use them to create an index vector
      lats <- h5read(path,parameter_matches_m[1])
      lons <- h5read(path,parameter_matches_m[2])
      aoi_subset <- lons > min_lon & lons <= max_lon & lats > min_lat & lats <= max_lat
      #remove the lats and lons from the list of parameters we want, they are no longer necessary
      parameter_matches_m <- parameter_matches_m[-c(1,2)]
      parameter_matches <- parameter_matches[-c(1,2),]
      #Read in all the desired arrays
      attribute_fields <- lapply(
        parameter_matches_m,
        FUN = function(x) {
          t <- h5read(file = path,name = x)
          if(length(dim(t))==1){t <- t[aoi_subset]} #If t is a vector, index by index
          if(length(dim(t))==2){t <- t[,aoi_subset] %>% t()} #if t is a matrix, index by column and transpose it
          return(t)
        }
      )
      #Convert and bind them into a dataframe for this beam
      names(attribute_fields) <- parameter_matches$name
      beam_df = do.call(cbind, attribute_fields) %>% as.data.frame() 
      names(beam_df) <- paste0(desired_parameter_pattern[3], 1:ncol(beam_df))
      return(beam_df)
    }
  ) %>% do.call(rbind, .)
  return(df)
  }
  
  out_df <- lapply(paths,FUN = internal_extract)%>% do.call(rbind, .)
}






#' extract_GEDI_points
#' @description Reads a L2B GEDI hdf file. Extracts only the desired parameters.
#' @param path to a l2b GEDI hdf file
#' @param desired_parameter_pattern a vector of strings which are used as patterns to grab the desired parameters
#' @return a dataframe of the requested fields
extract_GEDI_points <- function(path, desired_parameter_pattern) {
  # Get the internal structure of the file
  l2b_structure <- h5ls(path, recursive = T)
  # Read the group names
  l2b_groups <- unique(l2b_structure$name)
  # Read the beam designations
  l2b_beams <- l2b_groups[grepl("BEAM", l2b_groups)]
  
  #Extract and bind together the desired parameters for the desired beams
  df <- lapply(
    l2b_beams,
    FUN = function(current_beam)
    {
      # Subset the structure, selecting only the ones for this beam
      beam_parameters <-
        l2b_structure[grepl(current_beam, l2b_structure$group), ]
      # Get a list of all the matching parameters for this beam
      parameter_matches <-
        lapply(
          desired_parameter_pattern,
          FUN = function(y) {
            #beam_parameters[grepl(y, beam_parameters$name), ]
            beam_parameters[beam_parameters$name %in% y,]
          }
        ) %>% do.call(rbind, .)
      # Merge the first two fields to get a list of paths
      parameter_matches_m <-
        paste0(parameter_matches$group, "/", parameter_matches$name)
      
      # Read in all the desired arrays
      columns <- lapply(
        parameter_matches_m,
        FUN = function(x) {
          t <- h5read(path, x)
          return(t)
        }
      )
      # Convert and bind them into a dataframe for this beam
      names(columns) <- parameter_matches$name
      beam_df = do.call(cbind, columns) %>% as.data.frame()
      beam_df$Beam <- current_beam
      return(beam_df)
    }
  ) %>% do.call(rbind, .)
  return(df)
}

#Helper function for outliers:
#https://stackoverflow.com/a/43659981
is_outlier <- function(x) {
  return(x < quantile(x, 0.25,na.rm=T) - 1.5 * IQR(x) | x > quantile(x, 0.75,na.rm=T) + 1.5 * IQR(x))
}


  
#' extract_GEDI_points_wrapper
#' @param l2b_path path to a l2b GEDI hdf file
#' @param params a vector of strings which are used as patterns to grab the desired parameters
#' @param min_lon which is applied to lon_lowestmode to subset the points
#' @param max_lon which is applied to lon_lowestmode to subset the points
#' @param min_lat which is applied to lat_lowestmode to subset the points
#' @param max_lat which is applied to lat_lowestmode to subset the points
#' @param set_NA_dem Optionally set DEM nodata value (-999999) to NA?
#' @return a SpatialPointsDataframe of the requested fields, subset by location
#' @export
extract_GEDI_points_wrapper <-
  function(l2b_path,
           params = c("gfit"),
           min_lon,
           max_lon,
           min_lat,
           max_lat,
           set_NA_dem = FALSE) {
    
    #Extract the required params from the L2B file
    df <- extract_GEDI_points(l2b_path, params)
    
    # Optionally set DEM nodata value (-999999) to NA
    if(set_NA_dem){
      df$digital_elevation_model[df$digital_elevation_model==-999999] <- NA
    }
    
    df_processed <- df %>%
      ##Optional: Subset by Longitude and Latitude
      filter(between(lon_lowestmode, min_lon, max_lon)) %>% 
      filter(between(lat_lowestmode, min_lat, max_lat))
    if(nrow(df_processed)==0){return(0)}
    # Create a shapefile
    coordinates(df_processed) <- ~lon_lowestmode+lat_lowestmode
    df_processed@proj4string <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")
    
    #add the name of the source file as a column (called out_name because it could be used as output filename)
    out_name <- strsplit(l2b_path,"/")
    out_name <- out_name[[1]][length(out_name[[1]])] %>% 
      paste0("_extracted_points_",
             min_lon,"_",max_lon,"_",min_lat,"_",max_lat,"3")
    df_processed$src_file <- out_name

    return(df_processed)
  }



library(httr)
library(jsonlite)
library(magrittr)
library(assertthat)
#' query_GEDI_download_links
#' @description Finds available GEDI download links from the LP DAAC.
#' @param max_lat Upper latitude of the query
#' @param min_lon Lower longitude of the query
#' @param min_lat Lower latitude of the query
#' @param max_lon Upper longitude of the query
#' @param product_type One of "GEDI01_B", "GEDI02_A", "GEDI02_B". Desired product type.
#' @return a vector of download links.
query_GEDI_download_links <- function(max_lat, min_lon, min_lat, max_lon, product_type = "GEDI02_A") {
  assert_that(product_type %in% c("GEDI01_B", "GEDI02_A", "GEDI02_B"))
  
  link <- paste0("https://lpdaacsvc.cr.usgs.gov/services/gedifinder?product=GEDI02_B&version=001&bbox=",max_lat,",",min_lon,",",min_lat,",",max_lon,",","&output=json")
  response <- GET(link)
  
  if(response$status_code==200){
    print("Successfully requested files!")
  }else{
    print(paste("Could not request files. Status Code:", response$status_code))
  }
  response_content <- rawToChar(response$content) %>% 
    fromJSON()
  
  download_links <- response_content$data %>% unlist() %>% c()
  return(download_links)
}


