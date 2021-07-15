# CLASSICAL PACKAGE IMPORTER (Stefan Graf & Tobias Frey)
packages_checker <- function(packages_list){
  for (package in packages_list){
    if (!require(package, character.only = T)) {
      install.packages(package)
    }
    library(package, character.only = T)
  }
}

packages_checker(
  c('gganimate',
    'gifski',
    'ggforce',
    'png',
    'gganimate',
    'tmap',
    'tmaptools',# read_gpx function
    'hrbrthemes', # for colors --> viridis
    'rgdal',
    'ctmm',
    'lubridate',
    'plyr',
    'dplyr',
    'tidyr',
    'tidyverse',
    'sf', # Simple feature --> super spatial data handling structure
    'raster', # For raster data
    'rgdal',
    'plotly', # zoomable, pannable ggplot
    'move',# Brownian Bridge Movement Model
    'adehabitatHR',
    'lme4',
    'nlme',
    'ggplot2',
    'ggpubr',
    'GGally',
    'ggfortify', #check linear regression assumptions
    'GGally', # usefull pairs function to create many scatterplots
    'maptools',# used in move package
    'viridis',
    'lmerTest',
    'sp',
    'ks' # used in KDE functions
  ))
#install.packages("tlocoh", dependencies=TRUE, repos=c("http://R-Forge.R-project.org", "http://cran.cnr.berkeley.edu"), type="source")
#require(tlocoh)

rm(list = ls())
getwd()



### CUSTOM FUNCTIONS ###

# Function for displaying scale labels in ggplot not as f.e. 4e+05
fancy_scientific <- function(l) {
  # turn in to character string in scientific notation
  l <- format(l, scientific = TRUE)
  # quote the part before the exponent to keep all the digits
  l <- gsub("^(.*)e", "'\\1'e", l)
  # turn the 'e+' into plotmath format
  l <- gsub("e", "%*%10^", l)
  # return this as an expression
  parse(text=l)
}


clockS = function(t){hour(t)*3600+minute(t)*60+second(t)}
st_kde <- function(points,cellsize, bandwith, extent = NULL){
  require(MASS)
  require(raster)
  require(sf)
  if(is.null(extent)){
    extent_vec <- st_bbox(points)[c(1,3,2,4)]
  } else{
    extent_vec <- st_bbox(extent)[c(1,3,2,4)]
  }
  
  n_y <- ceiling((extent_vec[4]-extent_vec[3])/cellsize)
  n_x <- ceiling((extent_vec[2]-extent_vec[1])/cellsize)
  
  extent_vec[2] <- extent_vec[1]+(n_x*cellsize)-cellsize
  extent_vec[4] <- extent_vec[3]+(n_y*cellsize)-cellsize
  
  coords <- st_coordinates(points)
  matrix <- kde2d(coords[,1],coords[,2],h = bandwith,n = c(n_x,n_y),lims = extent_vec)
  raster(matrix)
}




#### Custom functions for analysis
clockS = function(t){hour(t)*3600+minute(t)*60+second(t)}


euclid <- function(x1,y1,x2,y2){
  return(sqrt((x1-x2)^2+(y1-y2)^2))
}

turning_angle <- function(x,y,lead_lag = 1){ 
  if(length(x) < 3){return(NA)}
  if(length(x) != length(y)){stop("x and y must be of the same length")}
  p1x <- lag(x,lead_lag)
  p1y <- lag(y,lead_lag)
  p2x <- x
  p2y <- y
  p3x <- lead(x,lead_lag)
  p3y <- lead(y,lead_lag)
  p12 <- euclid(p1x,p1y,p2x,p2y)
  p13 <- euclid(p1x,p1y,p3x,p3y)
  p23 <- euclid(p2x,p2y,p3x,p3y)
  rad <- acos((p12^2+p23^2-p13^2)/(2*p12*p23))
  grad <- (rad*180)/pi
  grad[p12 == 0 | p23 == 0] <- NA
  d <-  (p3x-p1x)*(p2y-p1y)-(p3y-p1y)*(p2x-p1x)
  d <- ifelse(d == 0,1,d)
  d[d>0] <- 1
  d[d<0] <- -1
  d[d==0] <- 1
  turning <- grad*d*-1+180
  return(turning)
}

# Source https://exploratory.io/note/kanaugust/1701090969905358
calculate_mode <- function(x) {
  uniqx <- unique(na.omit(x))
  uniqx[which.max(tabulate(match(x, uniqx)))]
}


# Statistics from here: https://github.com/aufrank/R-hacks/blob/master/mer-utils.R
vif.mer <- function (fit) {
  ## adapted from rms::vif
  
  v <- vcov(fit)
  nam <- names(fixef(fit))
  
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)]
  }
  
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v
}

aggregate.sf = function(x, by, FUN, ..., do_union = TRUE, simplify = TRUE,
                        join = st_intersects) {
  
  if (inherits(by, "sf") || inherits(by, "sfc")) {
    if (inherits(by, "sfc"))
      by = st_sf(by)
    i = join(st_geometry(by), st_geometry(x))
    st_geometry(x) = NULL
    # dispatch to stats::aggregate:
    a = aggregate(x[unlist(i), , drop = FALSE],
                  list(rep(seq_len(nrow(by)), lengths(i))), FUN, ...)
    nrow_diff = nrow(by) - nrow(a)
    if(nrow_diff > 0) {
      a_na = a[rep(NA, nrow(by)),] # 'top-up' missing rows
      a_na[a$Group.1,] = a
      a = a_na
    }
    a$Group.1 = NULL # remove
    row.names(a) = row.names(by)
    st_set_geometry(a, st_geometry(by))
  } else {
    crs = st_crs(x)
    lst = lapply(split(st_geometry(x), by), function(y) do.call(c, y))
    geom = do.call(st_sfc, lst[!sapply(lst, is.null)])
    
    if (do_union)
      geom = st_union(st_set_precision(geom, st_precision(x)), by_feature = TRUE)
    
    st_geometry(x) = NULL
    x = aggregate(x, by, FUN, ..., simplify = simplify)
    st_geometry(x) = geom # coerces to sf
    st_crs(x) = crs
    
    # now set agr:
    geoms = which(vapply(x, function(vr) inherits(vr, "sfc"), TRUE))
    agr_names = names(x)[-geoms]
    agr = rep("aggregate", length(agr_names))
    names(agr) = agr_names
    # which ones are identity variables?
    n = if (!is.null(names(by)))
      names(by)
    else
      paste0("Group.", seq_along(by))
    agr[n] = "identity"
    st_agr(x) = agr
    
    x
  }
}


setwd("/Users/stefgr/Nextcloud/UZH/12_Semester/20200313_Masterthesis/R/")


### Load Project variables ### 
##### waypoints ##### "data/Processed_data/SUAQ_waypoints_11all20.csv"
##### ATTENTION: read_csv guesses a columns type as boolean if first 1200 entries are NA
# find most recent file
mostrecent <- list.files(path = "data/Processed_data/", full.names = FALSE, recursive = FALSE)
mostrecent <- as.data.frame(mostrecent)
mostrecent <- mostrecent %>% filter(grepl("SUAQ_waypoints_11all20",mostrecent)) %>% 
  mutate(date=as.numeric(str_extract(mostrecent,"\\d{8}"))) %>% arrange(desc(date)) %>% top_n(1,date)

SUAQ_waypoints_11all20_loc <-
  read_csv(paste0("data/Processed_data/",mostrecent$date[1],"_SUAQ_waypoints_11all20.csv"),guess_max = min(10000, 30000))
SUAQ_waypoints_11all20_loc <- SUAQ_waypoints_11all20_loc %>% mutate(monthYearDate=format(as.Date(manualDate), "%Y-%m"))
SUAQ_waypoints_11all20_loc <- SUAQ_waypoints_11all20_loc %>% mutate(automaticDatetime=with_tz(automaticDatetime,tz = "Asia/Pontianak"))
SUAQ_waypoints_11all20_loc <- SUAQ_waypoints_11all20_loc %>% mutate(manualDatetime=with_tz(manualDatetime,tz = "Asia/Pontianak"))



#### add party individuals as individual gps points.
SUAQ_waypoints_11all20_loc_party <- SUAQ_waypoints_11all20_loc %>% filter(PtType%in%c("party"))

SUAQ_waypoints_11all20_loc_party1 <- SUAQ_waypoints_11all20_loc_party %>% 
  dplyr::select(altitude,name_original,sym,fileN,gpsextraction,gpsextractionquality,filedate,followername,manualDatetime, manualTime, manualDate, automaticDatetime, automaticTime,automaticDate,PtType,E,N,AgeSex,follow,PtType_individual_1) %>% filter(!is.na(PtType_individual_1)) %>%  mutate(focal=PtType_individual_1) %>% dplyr::select(-PtType_individual_1) 

SUAQ_waypoints_11all20_loc_party2 <- SUAQ_waypoints_11all20_loc_party %>% 
  dplyr::select(altitude,name_original,sym,fileN,gpsextraction,gpsextractionquality,filedate,followername,manualDatetime, manualTime, manualDate, automaticDatetime, automaticTime,automaticDate,PtType,E,N,AgeSex,follow,PtType_individual_2) %>% filter(!is.na(PtType_individual_2)) %>%  mutate(focal=PtType_individual_2) %>% dplyr::select(-PtType_individual_2) 

#### Combine datasets if for one follow there is an individual at party1 and at party2 position (f.e. if maybe at the first party point another individual was present but at the second on the same follow not) its still gets one new follownumber.
SUAQ_waypoints_11all20_loc_party <- SUAQ_waypoints_11all20_loc_party1 %>% bind_rows(SUAQ_waypoints_11all20_loc_party2)

SUAQ_waypoints_11all20_loc_party <- SUAQ_waypoints_11all20_loc_party%>% 
  group_by(fileN,focal,manualDate,follow) %>% # if .$follow it can be modified later idk why with only "follow" it doesnt work
  mutate(follow = as.numeric(cur_group_id()+1110000)) ## adding FN above 11

SUAQ_waypoints_11all20_loc <- SUAQ_waypoints_11all20_loc %>% 
  bind_rows(SUAQ_waypoints_11all20_loc_party)

##### Weather and FAI #####
SUAQ_weather <- read_csv("data/Processed_data/SUAQ_weather.csv")
SUAQ_fai <- read_csv("data/Processed_data/SUAQ_fai.csv")

SUAQ_weather_monthly<-SUAQ_weather %>% 
  group_by(SUAQ_weather.year,SUAQ_weather.month) %>% 
  summarise_all(funs(mean))
SUAQ_weather_monthly<- SUAQ_weather_monthly%>% dplyr::select(-SUAQ_weather.weather_id,-SUAQ_weather.day,-SUAQ_weather.daymonthyear)

##### location network ##### 
SUAQ_locationnetwork_sf <- st_read("data/20201201_StudyBalimbingStudyArea_points.GPX")
SUAQ_locationnetwork_sf_loc <- st_transform(SUAQ_locationnetwork_sf,crs = 23847)
##### for locale coordinate system
coordinates_tmp <- st_coordinates(SUAQ_locationnetwork_sf_loc)
colnames(coordinates_tmp) <- c("E","N")
SUAQ_locationnetwork_sf_loc <- cbind(SUAQ_locationnetwork_sf_loc,coordinates_tmp)
SUAQ_locationnetwork_loc <- st_drop_geometry(SUAQ_locationnetwork_sf_loc)

SUAQ_pathnetwork<- as.data.frame(read_GPX(file="data/SUAQ_Peta_WithLines.GPX",layers = c("routes")))
colnames(SUAQ_pathnetwork)[15]<- "geometry"
SUAQ_pathnetwork_sf<- st_as_sf(SUAQ_pathnetwork,crs=4326)
SUAQ_pathnetwork_sf_loc <- st_transform(SUAQ_pathnetwork_sf,crs = 23847)

##### physical features for plots
SUAQ_river_sf_wgs<- st_read(dsn="data/external/GeodataIndonesia/Wasser/fluss_50k_perimetergross.shp")
SUAQ_river_sf_loc <- st_transform(SUAQ_river_sf_wgs,crs = 23847)

SUAQ_smallriver_sf_wgs<- st_read(dsn="data/external/GeodataIndonesia/Wasser/kleinflüsse_50k_perimetergross.shp")
SUAQ_smallriver_sf_loc <- st_transform(SUAQ_smallriver_sf_wgs,crs = 23847)

SUAQ_contour_sf_wgs<- st_read(dsn="data/external/GeodataIndonesia/Hoehenllinien/hohenlinien_50k_perimetergross.shp")
SUAQ_contour_sf_loc <- st_transform(SUAQ_contour_sf_wgs,crs = 23847)

SUAQ_station_sf_wgs<- st_read(dsn="data/external/researchstation.shp")
SUAQ_station_sf_loc <- st_transform(SUAQ_station_sf_wgs,crs = 23847)

##### Selection only pointtypes which are taken on the orangutan way? is this true for all? ##### 
SUAQ_rangepoints_11all20_loc <- SUAQ_waypoints_11all20_loc %>% filter(PtType %in% c("daynest","found","lost","mornnest","nightnest","range","tree")) # get rid of lcg lch points and

##### Recalculate edge lengths etc. based on new point selection for rangepoints dataframe
SUAQ_rangepoints_11all20_loc<- SUAQ_rangepoints_11all20_loc %>% 
  group_by(follow)%>% arrange(manualTime,.by_group = TRUE) %>% 
  mutate(timelag=as.numeric(difftime(as.POSIXlt(manualTime),
                                     as.POSIXlt(lag(manualTime)),units 
                                     ="secs")),
         # calculate distance with approach number 1 --> st_distance but to do that we need to calculate lead first because the lead function doesnt work within st distance (not same datatype (df))
         timelag_lead=as.numeric(difftime(as.POSIXlt(lead(manualTime)),
                                          as.POSIXlt(manualTime),units ="secs")),
         manual_distTolast=sqrt((E-lag(E))^2+(N-lag(N))^2),
         manual_distToNext=sqrt((E-lead(E))^2+(N-lead(N))^2),
         speed_last = manual_distTolast/timelag,
         speed_next = manual_distToNext/timelag_lead,
         turningAngle=turning_angle(E,N),
         altitudediff_next=altitude-lead(altitude),
  )





##### WGS84 file
SUAQ_rangepoints_11all20_loc_sf <-  st_as_sf(SUAQ_rangepoints_11all20_loc, 
                                             coords = c("E","N"), 
                                             crs = 23847) # can also do that in the new sricpts
SUAQ_rangepoints_11all20_loc_wgs <- st_transform(SUAQ_rangepoints_11all20_loc_sf,4326)
coordinates_tmp <- st_coordinates(SUAQ_rangepoints_11all20_loc_wgs)
colnames(coordinates_tmp) <- c("long","lat")
SUAQ_rangepoints_11all20_loc_wgs <- cbind(SUAQ_rangepoints_11all20_loc_wgs,coordinates_tmp)
SUAQ_rangepoints_11all20_loc_wgs<- st_drop_geometry(SUAQ_rangepoints_11all20_loc_wgs)


##### Selection only females / most tracked females ##### 
SUAQ_rangepoints_11all20_loc_females <-
  SUAQ_rangepoints_11all20_loc %>%   filter(!(SUAQ_orangutans.Sex == "male")) %>% ungroup()

SUAQ_rangepoints_11all20_loc_above50GPS <- SUAQ_rangepoints_11all20_loc %>% filter(orangutan_tot_num_of_gps>250) %>% 
  filter(!(ClassFocal%in%c("character(0)","infant","juvenile")))
levels(as.factor(SUAQ_rangepoints_11all20_loc$ClassFocal))
SUAQ_rangepoints_11all20_loc_females_above50GPS <- SUAQ_rangepoints_11all20_loc %>% filter(!(SUAQ_orangutans.Sex == "male")) %>% filter(orangutan_tot_num_of_gps>50)

SUAQ_rangepoints_11all20_loc_males_above50GPS <- SUAQ_rangepoints_11all20_loc %>% filter((SUAQ_orangutans.Sex == "male")) %>% filter(orangutan_tot_num_of_gps>50)

SUAQ_rangepoints_11all20_loc_females_10 <-
  SUAQ_rangepoints_11all20_loc %>%   filter(!(SUAQ_orangutans.Sex == "male")) %>% filter(
    focal %in% c(
      "lisa",
      "friska",
      "ellie",
      "cissy",
      "lilly",
      "yulia",
      "raffi",
      "sarabi",
      "trident",
      "tiara"
    )
  )%>% ungroup()

#### Read in most recent bandwiths for KDE if not newly calculated
mostrecent <- list.files("/Users/stefgr/Nextcloud/UZH/12_Semester/20200313_Masterthesis/R/output/Homeranges/KDE/bandwiths/", full.names = FALSE, recursive = FALSE)
mostrecent <- as.data.frame(mostrecent)
colnames(mostrecent) <- c("date")
mostrecent <- mostrecent %>% filter(date!="_archive") %>%  arrange(desc(date)) %>% top_n(1,date)
imdir <-paste("/Users/stefgr/Nextcloud/UZH/12_Semester/20200313_Masterthesis/R/output/Homeranges/KDE/bandwiths/")
bandwiths <- read_csv(paste0(imdir,paste(mostrecent$date[1], sep="")))

#### Homerange calculations parameter
### Extent
# use always same extent
extentmargin <- 500
axisshifty <- 125 # shift of axis in plots y axis
axisshiftx <- -280 # shift of axis in plots x axis
legendoffsetx <- 0
legendoffsety <- 0
xmin <- min(SUAQ_rangepoints_11all20_loc_females_10$E)-extentmargin 
xmax <- max(SUAQ_rangepoints_11all20_loc_females_10$E)+extentmargin
ymin <- min(SUAQ_rangepoints_11all20_loc_females_10$N)-extentmargin 
ymax <- max(SUAQ_rangepoints_11all20_loc_females_10$N)+extentmargin
step <- 200
xmin_xmax_round_steps<- c(step*(ceiling(xmin/step)):(round(xmax/step)))
xmin_xmax_round_steps <- xmin_xmax_round_steps[2:(length(xmin_xmax_round_steps)-2)] #manual adjustment of tick breaks how many does it need
ymin_ymax_round_steps<- c(step*(ceiling(ymin/step)):(round(ymax/step)))
ymin_ymax_round_steps <- ymin_ymax_round_steps[2:(length(ymin_ymax_round_steps)-1)] #manual adjustment of tick breaks how many does it need

#### research periods data as list
# Split the data into 4 sampling periods. Calculate the respective HR.
SUAQ_rangepoints_11all20_loc_females_10 <- SUAQ_rangepoints_11all20_loc_females_10 %>% mutate(year=year(manualDatetime))

# make 5 lists for all 4 research periods and one for all data
period_list <- split(SUAQ_rangepoints_11all20_loc_females_10, f = SUAQ_rangepoints_11all20_loc_females_10$researchperiod)

# add all data as the 5th list (period)
period_list[["total research period"]] <- SUAQ_rangepoints_11all20_loc_females_10
period_list <- period_list[c("total research period","before 2012","2012 until 2016","2016 until August 2018","Since August 2018")]

# make lists for every year
year_list <- split(SUAQ_rangepoints_11all20_loc_females_10, f = SUAQ_rangepoints_11all20_loc_females_10$year)

#### Selected animals as list/vector
selected_females <- SUAQ_rangepoints_11all20_loc_females_10 %>% group_by(focal) %>% summarise()
selected_females <- as_vector(selected_females)

#### mapping features
SUAQ_river_sf_wgs<- st_read(dsn="data/external/GeodataIndonesia/Wasser/sungai_50k_perimeter.shp")
SUAQ_river_sf_loc <- st_transform(SUAQ_river_sf_wgs,crs = 23847)

SUAQ_smallriver_sf_wgs<- st_read(dsn="data/external/GeodataIndonesia/Wasser/kleinflüsse_50k_perimetergross.shp")
SUAQ_smallriver_sf_loc <- st_transform(SUAQ_smallriver_sf_wgs,crs = 23847)

SUAQ_contour_sf_wgs<- st_read(dsn="data/external/GeodataIndonesia/Hoehenllinien/hohenlinien_50k_perimetergross.shp")
SUAQ_contour_sf_loc <- st_transform(SUAQ_contour_sf_wgs,crs = 23847)

SUAQ_station_sf_wgs<- st_read(dsn="data/external/researchstation.shp")
SUAQ_station_sf_loc <- st_transform(SUAQ_station_sf_wgs,crs = 23847)

SUAQ_dem <- raster(x = "data/external/DEM_50m.tif")

SUAQ_dem_spdf <- as(SUAQ_dem, "SpatialPixelsDataFrame")
SUAQ_dem_df <- as.data.frame(SUAQ_dem_spdf)
colnames(SUAQ_dem_df) <- c("value", "E1", "N1")


first_date<- SUAQ_rangepoints_11all20_loc_females_10[order(SUAQ_rangepoints_11all20_loc_females_10$manualDatetime , decreasing = FALSE ),] %>% dplyr::select(manualDatetime)
first_date<- dplyr::pull(first_date,manualDatetime)
first_date<- rep(first_date, times = 13)
SUAQ_dem_df <- SUAQ_dem_df %>% cbind(first_date[1:nrow(SUAQ_dem_df)])
colnames(SUAQ_dem_df) <- c("value", "E1", "N1","manualDatetime")


for(i in selected_females){
  start <- Sys.time()
  SUAQ_rangepoints_11all20_loc_females_10 <- SUAQ_rangepoints_11all20_loc_females_10[order(SUAQ_rangepoints_11all20_loc_females_10$trackpoint_id , decreasing = FALSE ),] 
  focal_selected<- SUAQ_rangepoints_11all20_loc_females_10 %>%  filter(focal=="cissy")
  focal_selected_trees <- SUAQ_rangepoints_11all20_loc_females_10 %>%  filter(PtType=="tree") %>% filter(focal=="cissy")
  ggplot(focal_selected,aes(x = E,y = N)) +
    geom_tile(data=SUAQ_dem_df, aes(fill=value))+
    geom_path(data=focal_selected,aes(group= follow, colour= as.factor(follow)))+
    geom_point(
      size = 1,
      stroke = 0,
      shape = 16,
      mapping = aes(
        group=identification
      ),
      alpha = 1,
      na.rm = TRUE,
      show.legend = FALSE
    )+
    geom_point(
      shape="*",
      data=focal_selected_trees,
      mapping = aes(
        size = 0.1,
        group=identification
      ),
      colour="springgreen4",
      alpha = 1,
      na.rm = TRUE,
      show.legend = FALSE
    )+
    shadow_wake(wake_length = 0.1,
                alpha = 0.5)+
    theme(legend.position = "none")+
    guides(colour= guide_legend(title = "Orangutan\nwith buffered circles [12m]"))+
    #theme(legend.position = "none")+
    
    # Here comes the gganimate specific bits
    labs(title = 'Time: {hour(hms::as_hms(frame_along))}:{minute(hms::as_hms(frame_along))}', x = 'E', y = 'N')+
    transition_reveal(hms::as_hms(round_date(manualDatetime, unit = "minute")),keep_last = TRUE)
  plot1
  p1<- gganimate::animate(plot1, height = 800, width =800,fps=10)
  gganimate::anim_save(animation = p1,filename = paste0("output/animations/","cissy","_fps_5_onlylines.gif"),renderer = gifski_renderer())
  print(paste0("finished: ",(Sys.time()-start)))
  rm(plot1,p1)
}

p1<- ggplot(focal_selected) +
 
  scale_fill_gradient(low = "white",high = "black")+
  
focal_selected_stack <- focal_selected %>% dplyr::select(identifictfocal,follow)
write_csv(focal_selected_stack,path = "output/animations/stackoverflow/data.csv")
p1

ggplot(focal_selected) +
  #geom_sf(data=SUAQ_river_sf_loc, fill="cadetblue3",lwd = 0,aes(group = "hello"))+
  geom_tile(data=SUAQ_dem_df, aes(x=E1,y=N1,fill=value,group = 1))+
  geom_path(data=focal_selected,aes(x = E,y = N,group= follow, colour= as.factor(follow)))+
  geom_point(
    size = 1,
    stroke = 0,
    shape = 16,
    mapping = aes(
      x = E,y = N,group=identification
    ),
    alpha = 1,
    na.rm = TRUE,
    show.legend = FALSE
  )+
  geom_point(
    shape="*",
    data=focal_selected_trees,
    mapping = aes(
      size = 0.1,
      x = E,y = N,group=identification
    ),
    colour="springgreen4",
    alpha = 1,
    na.rm = TRUE,
    show.legend = FALSE
  )+
  theme(legend.position = "none")+
  guides(colour= guide_legend(title = "Orangutan\nwith buffered circles [12m]"))+
  #theme(legend.position = "none")+
  coord_sf(xlim = c(xmin,xmax), ylim = c(ymin,ymax))+
  # Here comes the gganimate specific bits
  #labs(title = 'Time: {hour(hms::as_hms(frame_along))}:{minute(hms::as_hms(frame_along))}', x = 'E', y = 'N')+
  transition_reveal(hms::as_hms(round_date(manualDatetime, unit = "minute")),keep_last = TRUE)



