######################################################################################
#         
# Download GEDI L2B Data.
# Process them into a shapefile.
#
# Project Contributors:
# Žiga Kokalj, Research Centre of the Slovenian Academy of Sciences and Arts 
# Johannes Mast, University of Würzburg
#
# This research was partly funded by the 
# Slovenian Research Agency core funding No. P2-0406.
######################################################################################

#### 1 : Preamble ####

# Set the Main Working Directory
setwd("D:/SoSe2020/GEDI_Yucatan/")

# Loading required functions
 source("GEDI_Utils_l2b.R")  # util functions
library(raster)  #for reading extents

# Set Output Directory
out_name <-   paste0("Data/L2B_All_Yucatan/")

# Set input directory
dl_dir <- "F:/GEDI_Downloads_Yucatan/L2B_All_Yucatan/" 


# Set the extent via the extent of an AOI (from shapefile)
aoi <- rgdal::readOGR("Data/Ancillary_Data/ALS/outline_area.shp")
e_wgs84 <- extent(sp::spTransform(aoi, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")))

min_lon = e_wgs84[1]
max_lon = e_wgs84[2]
min_lat = e_wgs84[3]
max_lat = e_wgs84[4]

# Alternatively set the extent by coordinates
min_lon = -92
max_lon = -85.5
min_lat = 15
max_lat = 22.5

#### 2 : Download Files ####

#Credit to the Download script to: Cole Krehbiel
#https://git.earthdata.nasa.gov/projects/LPDUR/repos/daac_data_download_r/browse
# ------------------------------------------------------------------------------------------------ #
#
# Load necessary packages into R
library(sys)
library(getPass)
library(httr)



# ---------------------------------SET UP ENVIRONMENT--------------------------------------------- #
# IMPORTANT: Update the line below if you want to download to a different directory (ex: "c:/data/")
                               # Set dir to download files to                                              # Set the working dir to the dl_dir
usr <- file.path(Sys.getenv("USERPROFILE"))                  # Retrieve home dir (for netrc file)
if (usr == "") {usr = Sys.getenv("HOME")}                    # If no user profile exists, use home
netrc <- file.path(usr,'.netrc', fsep = .Platform$file.sep)  # Path to netrc file

# ------------------------------------CREATE .NETRC FILE------------------------------------------ #
# If you already have a .netrc file with your Earthdata Login credentials stored in your home
# directory, this portion will be skipped. Otherwise you will be prompted for your NASA Earthdata
# Login Username/Password and a netrc file will be created to store your credentials (in home dir)
if (file.exists(netrc) == FALSE || grepl("urs.earthdata.nasa.gov", readLines(netrc)) == FALSE) {
  netrc_conn <- file(netrc)
  
  # User will be prompted for NASA Earthdata Login Username and Password below
  writeLines(c("machine urs.earthdata.nasa.gov",
               sprintf("login %s", getPass(msg = "Enter NASA Earthdata Login Username \n (or create an account at urs.earthdata.nasa.gov) :")),
               sprintf("password %s", getPass(msg = "Enter NASA Earthdata Login Password:"))), netrc_conn)
  close(netrc_conn)
}




# ---------------------------CONNECT TO DATA POOL AND DOWNLOAD FILES------------------------------ #
files <- query_GEDI_download_links(max_lat,min_lon,min_lat,max_lon,product_type = "GEDI02_B")
old_dir <- getwd()
setwd(dl_dir)  
# Loop through all files
for (i in 1:length(files)) {
  filename <-  tail(strsplit(files[i], '/')[[1]], n = 1) # Keep original filename
  
  # Write file to disk (authenticating with netrc) using the current directory/filename
  response <- GET(files[i], write_disk(filename, overwrite = TRUE), progress(),
                  config(netrc = TRUE, netrc_file = netrc), set_cookies("LC" = "cookies"))
  
  # Check to see if file downloaded correctly
  if (response$status_code == 200) {
    print(sprintf("%s downloaded at %s", filename, dl_dir))
  } else {
    print(sprintf("%s not downloaded. Verify that your username and password are correct in %s", filename, netrc))
  }
}
#return to old working directory
setwd(old_dir)

#### 3 : Process to Shapefile ####
L2B_files <- list.files("F:/GEDI_Downloads_Yucatan/L2B_All_Yucatan/",pattern = "02_B",full.names = T)

##Inspect the file structure
L2B_structure <- h5ls(L2B_files[[1]], recursive = T)

points <- list()

for(i in 1:length(L2B_files)){
  testpoints <- extract_GEDI_points_wrapper(
    l2b_path = L2B_files[i],
    params = c("lon_lowestmode",
               "lat_lowestmode"
    ),
    min_lon = min_lon,
    max_lon = max_lon,
    min_lat = min_lat,
    max_lat = max_lat
  )
  if(!is.null(testpoints)){
    points[[i]] <- testpoints
  }
}

#Select only functional spatialpoints
v <- lapply(points, function(x) class(x)=="SpatialPointsDataFrame") %>% unlist()
points_to_merge <- points[v]

#merge the list
merged_points <- do.call(rbind, points_to_merge)

#write to disk
writeOGR(merged_points,dsn ="Data/L2B_All_Yucatan","L2B_Points_Yucatan","ESRI Shapefile",overwrite_layer = T)
merged_points$Beam <- NA
merged_points$Beam <- NULL
merged_points$src_file <- NULL
merged_points$ph <- 0

a <- strsplit(files,"/")
b <- unlist(lapply(a,FUN = function(x){x[7]}))
fileConn<-file("Data/L2B_All_Yucatan/sourcefiles.txt")
writeLines(b, fileConn)
close(fileConn)
