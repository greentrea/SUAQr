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

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Minimum convex polygon for orangutans in SUAQ"),
    
    # Session Data
    
    
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(position = "right",
        sidebarPanel(
          tags$head(tags$script('$(document).on("shiny:connected", function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            $(window).resize(function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            ')),
            checkboxGroupInput(inputId = "focalids",
                               "Orangutan name",specs_orangutan_tot_num_of_gps$focal,specs_orangutan_tot_num_of_gps$focal[10],inline = T),
          width = 4
        ),

        # Show a plot of the generated distribution
        fillPage(mainPanel(
           plotOutput("mapPlot"),width = 8
        ))
    ),
    hr(),
    print("(Considered Data: 2010-2020) \n ® Data is owned by the University of Zurich and is not allowed to use. Further information @stefan.grafen@gmail.com ~~~")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$mapPlot <- renderPlot({
        tmp_SUAQ_selected_chulls <- SUAQ_waypoints_11_20_backup_sf %>%
            filter(focal %in% input$focalids) %>%
            left_join(specs_orangutan_tot_num_of_gps,by = c("focal" = "focal")) %>% 
            mutate(focal = paste(focal," [#GPS:", orangutan_tot_num_of_gps,"] [#Follows:",orangutan_num_of_follows,"]")) %>% 
            group_by(focal) %>%
            summarise(geometry = st_combine(geometry) ) %>%
            st_convex_hull()
        ggplot(tmp_SUAQ_selected_chulls) +
            geom_sf(data = tmp_SUAQ_selected_chulls,
                    aes(colour = focal),alpha=0)+
            geom_sf(data = SUAQ_pathnetwork,aes(fill = "path-network"))+
            labs(col="Focal")+
            theme(legend.title = element_blank(),legend.position = "bottom",plot.caption = element_text(hjust=0.5, size=rel(1.2)))
      
    },
    height=reactive(ifelse(!is.null(input$innerWidth),input$innerWidth*2.5/5,0)))
}

# Run the application 
shinyApp(ui = ui, server = server)
