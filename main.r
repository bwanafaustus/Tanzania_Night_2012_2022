#******************************************************************************
#                 TANZANIA NIGHT LIGHT BETWEEN 2012 AND 2022                    
#                         Author: Faustus S. Reginald
#                         Email: bwanafaustus@gmail.com
#                         Tel: +255 75 428 4641
#******************************************************************************
#Set the working directory
setwd("C:/Users/hp/Documents/R")

#Loading the required packages
libs <- c("tidyverse", "terra","sf","rgeoboundaries", "shiny", "gifski") 

invisible(lapply(libs, library, character.only=T)) 

#Download Administrative boundary of Tanzania and set CRS
TZ <-geoboundaries("Tanzania")
st_crs(TZ)=4326

#The nightlight dataset was downloaded from Earth Observation Group
#Link: https://eogdata.mines.edu/products/vnl/#annual_v2

#Listing the downloaded files
files <- list.files(
    path=getwd(),
    pattern='npp',
    full.names=T
)

#Load the data
globe_light <- lapply(paste0("/vsigzip/", files),
    terra::rast
)

#Crop the data
cropped_light <- lapply(globe_light,
    function(x){
        terra::crop(x, vect(TZ), snap ='in', mask = T)
    }
)

#Remove Zeros from the rasters
cropped_light_final <- lapply(cropped_light,
    function(x) {
        terra::ifel(x<=0, NA, x)
    }
)

#Convert raster to dataframe
TZ_night_light_df <-lapply(cropped_light_final,
        function(x) {
            as.data.frame(x, xy=T, na.rm=T)
        }
)

#Renaming columns of the raster datasets
col_names <- c('x', 'y', 'value')
TZ_night_light_df <- lapply(TZ_night_light_df, setNames, col_names)

#Naming the raster in list (2012 and 2022)
years <- c('2012', '2022')
names(TZ_night_light_df) <- years

cols <- c("#1f4762", "#FFD966", "white")
pal <- colorRampPalette(
    cols, bias = 8
)(512)

# mapping the datasets

map <- lapply(
    names(TZ_night_light_df),
    function(df){
        ggplot(
            data = TZ_night_light_df[[df]]
        ) +
        geom_sf(
            data = TZ,
            fill = NA,
            color = cols[[1]],
            size = .05
        ) +
        geom_tile(
            aes(x = x, y = y, fill = value)) +
        scale_fill_gradientn(name = "", colors = pal) +
        theme_void() +
        theme(
            legend.position = "none",
            plot.title = element_text(size = 80, color = "white", hjust = .5, vjust = 0),
            plot.caption=element_text(color = "white", face = "italic", size = 12, hjust = 1, vjust = 1),
            plot.margin = unit(
                c(
                    t = 0, r = 0,
                    l = 0, b = 0
                ), "lines"
            )
        ) +
        labs(title = df, caption="@ 2023 Bwana Muki | Data: Earth Observation Group ")
    }
)

#Saving the maps in the drive
for (i in 1:2){
    file_name = paste0("TZ_Night_",i,".png")
      png(file_name,width = 800, 
                    height = 800, units = "px",
                    bg = "#182833"
    )
    print(map[[i]])
    dev.off()
}

#Create a GIF 
map_files <- list.files(path=getwd(), pattern='TZ_Night', full.names=T)

gifski::gifski(
    map_files,
    loop = T,
    delay = 1,
    width = 800,
    height = 800,
    gif_file = "TZ_Night.gif"
)
