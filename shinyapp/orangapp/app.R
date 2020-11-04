#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Shiny has three scoping levels for variables. 
# 1. All sessions of all users --> Big data sets, global variables
# 2. Session variables --> f.e. time when session is called, user specific infos/variables etc
# 3. in Server variables --> recalled all the time

library('tmaptools')# read GPX to dataframe
library('hR')# making realtionships of orangutans
library('dbplyr')
library('ggmap')
library('plyr')
library('tidyr')
library('tidyverse')
library('sf') # Simple feature --> super spatial data handling structure
library('ggspatial')
library('rgdal')
library('ggraph') # visualization
library('tmap')
library('plotly') # zoomable pannable ggplot
library('mapview')
library('ggmap') # easier interactive ggmaps 
library('leaflet') # Javascript library (originally) for plotting on a maptile interactive map
library('remotes') # geom_convexhull function
library('shiny')
library('spdplyr')
library('adehabitatHR')
library('lubridate')
library('ggplot2')

#setwd("/Users/stefgr/Nextcloud/12_Semester/20200313_Masterthesis/R/shinyapp/orangapp")


# All sessions data
SUAQ_waypoints_11_20_backup<- read_csv("data/SUAQ_waypoints_11all20_backup.csv")

SUAQ_waypoints_11_20_backup_sf <-  st_as_sf(SUAQ_waypoints_11_20_backup, 
                                       coords = c("E","N"), 
                                       crs = 32647)

specs_numoffollows<- SUAQ_waypoints_11_20_backup %>% group_by(follow,focal) %>% summarize(count=n()) %>% group_by(focal) %>% summarize(count=n())
specs_orangutan_tot_num_of_gps <-SUAQ_waypoints_11_20_backup %>% 
    group_by(focal) %>%
    summarise(n()) %>% left_join(specs_numoffollows,by=c('focal'='focal'))
colnames(specs_orangutan_tot_num_of_gps) <- c("focal",
                                              "orangutan_tot_num_of_gps","orangutan_num_of_follows")



SUAQ_pathnetwork<- st_read("data/SUAQ_pathnetwork.shp")
SUAQ_pathnetwork <- st_transform(SUAQ_pathnetwork, 4326)

# with mcp
tmp_SUAQ_waypoints_morethan50gps_11all20<- SUAQ_waypoints_11_20_backup %>% 
  left_join(specs_orangutan_tot_num_of_gps[,c(1:3)],by=c("focal"="focal"))%>% 
  filter(orangutan_tot_num_of_gps>=100)

tmp_SUAQ_waypoints_morethan50gps_11all20.sp<- tmp_SUAQ_waypoints_morethan50gps_11all20 %>% dplyr::select(focal,E,N)
coordinates(tmp_SUAQ_waypoints_morethan50gps_11all20.sp) <- c("E","N")
proj4string(tmp_SUAQ_waypoints_morethan50gps_11all20.sp) <- CRS( "+proj=utm +zone=47 +datum=WGS84 +units=m +no_defs" )

tmp_SUAQ_waypoints_morethan50gps_11all20.mcp<- mcp(tmp_SUAQ_waypoints_morethan50gps_11all20.sp,percent = 98)

tmp_SUAQ_waypoints_morethan50gps_11all20.spgeo <- spTransform(tmp_SUAQ_waypoints_morethan50gps_11all20.sp, CRS("+proj=longlat"))
tmp_SUAQ_waypoints_morethan50gps_11all20.mcpgeo <- spTransform(tmp_SUAQ_waypoints_morethan50gps_11all20.mcp, CRS("+proj=longlat"))

tmp_SUAQ_waypoints_morethan50gps_11all20.geo <- data.frame(tmp_SUAQ_waypoints_morethan50gps_11all20.spgeo@coords, 
                                                           id = tmp_SUAQ_waypoints_morethan50gps_11all20.spgeo@data$focal )

mybasemap <- get_stamenmap(bbox = c(left = min(tmp_SUAQ_waypoints_morethan50gps_11all20.spgeo@coords[,1])-0.005, 
                                    bottom = min(tmp_SUAQ_waypoints_morethan50gps_11all20.spgeo@coords[,2])-0.005, 
                                    right = max(tmp_SUAQ_waypoints_morethan50gps_11all20.spgeo@coords[,1])+0.005, 
                                    top = max(tmp_SUAQ_waypoints_morethan50gps_11all20.spgeo@coords[,2])+0.005), 
                           zoom = 12)
# API key: https://maps.googleapis.com/maps/api/directions/json?origin=Toronto&destination=Montreal&key=AIzaSyABLRgWSFHCKuSyFF7QgZpa9ZgIqc9izZg


tmp_SUAQ_waypoints_morethan50gps_11all20_unflmale <- tmp_SUAQ_waypoints_morethan50gps_11all20 %>% filter(SUAQ_followlog.ClassFocal %in% c("unfl.male"))
tmp_SUAQ_waypoints_morethan50gps_11all20_others <- tmp_SUAQ_waypoints_morethan50gps_11all20 %>% filter(SUAQ_followlog.ClassFocal %in% c("infant","juvenile","unk"))
tmp_SUAQ_waypoints_morethan50gps_11all20_flmale <- tmp_SUAQ_waypoints_morethan50gps_11all20 %>% filter(SUAQ_followlog.ClassFocal %in% c("fl.male"))
tmp_SUAQ_waypoints_morethan50gps_11all20_adfemales <- tmp_SUAQ_waypoints_morethan50gps_11all20 %>% filter(SUAQ_followlog.ClassFocal %in% c("ad.female","mother"))

# Define UI for application that draws a histogram
ui <- fluidPage(
  
    # Application title
    titlePanel(h1("Minimum convex polygon for orangutans in SUAQ",
                  style='background-color:cadetblue;
                     padding-left: 5px;padding-bottom:5px;padding-top:5px')),
    
    # Session Data
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(position = "right",
          sidebarPanel(
          tags$head(tags$style("#mapPlot{height:100vh !important;}")),
          style='background-color:#e5feff',
          br(),
          sliderInput("daterange",
                      "Dates: (minimum 14 days)",
                      min = as.Date("2010-01-01","%Y-%m-%d"),
                      max = as.Date("2020-12-01","%Y-%m-%d"),
                      value=c(as.Date("2010-01-01"),as.Date("2020-12-01")),
                      timeFormat="%Y-%m-%d"),
          numericInput("mcppercentage","How many points should be considered for MCP? [0-100%]", value = 98),
          checkboxInput(inputId="pointsactive","Show GPS points", TRUE),
          
          # dateRangeInput("daterange", "Date range:",
          #                start = min(tmp_SUAQ_waypoints_morethan50gps_11all20$manualDate, na.rm = TRUE),
          #                end   = max(tmp_SUAQ_waypoints_morethan50gps_11all20$manualDate, na.rm = TRUE)),
            
            checkboxGroupInput(inputId = "focalids_unflmale",
                               "Unflannged males",levels(as.factor(tmp_SUAQ_waypoints_morethan50gps_11all20_unflmale$focal)),levels(as.factor(tmp_SUAQ_waypoints_morethan50gps_11all20_unflmale$focal))[1],inline = T),
            checkboxGroupInput(inputId = "focalids_flmale",
                             "Flanged males",levels(as.factor(tmp_SUAQ_waypoints_morethan50gps_11all20_flmale$focal)),levels(as.factor(tmp_SUAQ_waypoints_morethan50gps_11all20_flmale$focal))[1],inline = T),
            checkboxGroupInput(inputId = "focalids_adfemales",
                             "Adult females",levels(as.factor(tmp_SUAQ_waypoints_morethan50gps_11all20_adfemales$focal)),levels(as.factor(tmp_SUAQ_waypoints_morethan50gps_11all20_adfemales$focal))[1],inline = T),
            checkboxGroupInput(inputId = "focalids_others",
                             "Others (Juvenile, infant, unknown)",levels(as.factor(tmp_SUAQ_waypoints_morethan50gps_11all20_others$focal)),levels(as.factor(tmp_SUAQ_waypoints_morethan50gps_11all20_others$focal))[1],inline = T),
          width = 3
        ),
        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("mapPlot"),width = 9
        ),
        
    ),

    hr(),
    print("Considered Data: 2010-2020 | Only focals with more than 100 GPS fixes are considered"),br(),
    print("® Data is owned by the University of Zurich and is not allowed to use. Further information @stefan.grafen@gmail.com")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$mapPlot <- renderPlotly({
    selected <- c(input$focalids_unflmale,input$focalids_flmale,input$focalids_adfemales,input$focalids_others) 
    # make sure end date later than start date
    validate(
      need(input$daterange[2] > input$daterange[1], "end date is earlier than start date"
      )
    )
    
    # make sure greater than 2 week difference
    validate(
      need(difftime(input$daterange[2], input$daterange[1], "days") > 14, "date range less the 14 days"
      )
    )
    
    # Prepare the data
    tmp_SUAQ_waypoints_morethan50gps_11all20_server <- tmp_SUAQ_waypoints_morethan50gps_11all20 %>% filter(manualDate>as_date(input$daterange[1])) %>%filter(manualDate<as_date(input$daterange[2])) %>% filter(focal %in% selected)
    tmp_SUAQ_waypoints_morethan50gps_11all20.sp<- tmp_SUAQ_waypoints_morethan50gps_11all20_server %>% dplyr::select(focal,E,N)
    coordinates(tmp_SUAQ_waypoints_morethan50gps_11all20.sp) <- c("E","N")
    proj4string(tmp_SUAQ_waypoints_morethan50gps_11all20.sp) <- CRS( "+proj=utm +zone=47 +datum=WGS84 +units=m +no_defs" )
    
    tmp_SUAQ_waypoints_morethan50gps_11all20.mcp<- mcp(tmp_SUAQ_waypoints_morethan50gps_11all20.sp,percent = input$mcppercentage)
    
    tmp_SUAQ_waypoints_morethan50gps_11all20.spgeo <- spTransform(tmp_SUAQ_waypoints_morethan50gps_11all20.sp, CRS("+proj=longlat"))
    tmp_SUAQ_waypoints_morethan50gps_11all20.mcpgeo <- spTransform(tmp_SUAQ_waypoints_morethan50gps_11all20.mcp, CRS("+proj=longlat"))
    
    tmp_SUAQ_waypoints_morethan50gps_11all20.geo <- data.frame(tmp_SUAQ_waypoints_morethan50gps_11all20.spgeo@coords, 
                                                               id = tmp_SUAQ_waypoints_morethan50gps_11all20.spgeo@data$focal )
    
      # tmp_SUAQ_selected_chulls <- SUAQ_waypoints_11_20_backup_sf %>%
        #     filter(focal %in% input$focalids) %>%
        #     left_join(specs_orangutan_tot_num_of_gps,by = c("focal" = "focal")) %>% 
        #     mutate(focal = paste(focal," [#GPS:", orangutan_tot_num_of_gps,"] [#Follows:",orangutan_num_of_follows,"]")) %>% 
        #     group_by(focal) %>%
        #     summarise(geometry = st_combine(geometry) ) %>%
        #     st_convex_hull()
        # ggplot(tmp_SUAQ_selected_chulls) +
        #     geom_sf(data = tmp_SUAQ_selected_chulls,
        #             aes(colour = focal),alpha=0)+
        #     geom_sf(data = SUAQ_pathnetwork,aes(fill = "path-network"))+
        #     labs(col="Focal")+
        #     theme(legend.title = element_blank(),legend.position = "bottom",plot.caption = element_text(hjust=0.5, size=rel(1.2)))
    mcp_map_wPoints <- ggplot() + 
        geom_polygon(data = fortify(tmp_SUAQ_waypoints_morethan50gps_11all20.mcpgeo),  
                     # Polygon layer needs to be "fortified" to add geometry to the dataframe
                     aes(long, lat, colour = id),
                     alpha = 1,size=0.4,fill=NA) + # alpha sets the transparency
        geom_point(data = tmp_SUAQ_waypoints_morethan50gps_11all20.geo, 
                                                               aes(x = tmp_SUAQ_waypoints_morethan50gps_11all20.geo$E, y = tmp_SUAQ_waypoints_morethan50gps_11all20.geo$N, colour = 
                                                                     tmp_SUAQ_waypoints_morethan50gps_11all20.geo$id),size=0.3,alpha=0.6)  +
         theme(legend.position = c(0.15, 0.80),plot.title =element_blank()) +
        labs(x = "Longitude", y = "Latitude")+
        geom_sf(data = SUAQ_pathnetwork, inherit.aes = FALSE,size=0.1)
    
    mcp_map_oPoints <- ggplot() + 
      geom_polygon(data = fortify(tmp_SUAQ_waypoints_morethan50gps_11all20.mcpgeo),  
                   # Polygon layer needs to be "fortified" to add geometry to the dataframe
                   aes(long, lat, colour = id),
                   alpha = 1,size=0.4,fill=NA) + # alpha sets the transparency
      theme(legend.position = c(0.15, 0.80),plot.title =element_blank()) +
      labs(x = "Longitude", y = "Latitude")+
      geom_sf(data = SUAQ_pathnetwork, inherit.aes = FALSE,size=0.1) 
        
        
    if(input$pointsactive){ggplotly(mcp_map_wPoints)}else{ggplotly(mcp_map_oPoints)}
        
    }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
