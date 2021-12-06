##built by TurtleWu55
if(!require('tidyverse',character.only = TRUE)) install.packages('tidyverse')
if(!require('DT',character.only = TRUE)) install.packages('DT')
if(!require('cowplot',character.only = TRUE)) install.packages('cowplot')
if(!require('leaflet',character.only = TRUE)) install.packages('leaflet')
library(tidyverse)
library(DT)
library(cowplot)
library(leaflet)
distance_data <- read_csv("new_distance_data.csv")
ori_list <- distance_data %>% select(origin) %>% unlist() %>% unique()
dest_list <- distance_data %>% select(dest) %>% unlist() %>% unique()



ui <- fluidPage(
    tabsetPanel(    
        tabPanel(
            "Calculator",
            # Application title
            titlePanel("Distance for given 2 Airports"),
            sidebarPanel(
                
                selectInput("ori", "Origin:",
                            ori_list,
                            selected = "EWR"
                            ),
                selectInput("dest", label = "Destination:",
                          dest_list,
                          selected = "IAH"
                          )#,
                
                #actionButton("submitbutton", "Submit", class = "button"),
            ),
            mainPanel(
                tableOutput("data_calculator"),
                leafletOutput("map")
            )
        ),
        
        tabPanel(
            "Complete Data",
            mainPanel(
                DT::dataTableOutput("data")
            )
        ),
        
        tabPanel(
            "Distance Differences",
            mainPanel(
                plotOutput("plot_differences"),
                DT::dataTableOutput("data_differences")
            )
        )
        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
 
    #results <- distance_data %>% filter(origin==input$ori) %>% filter(dest==input$dest)
        
    #results <- data.frame(results)
    output$data_calculator <- renderTable({
        distance_data %>% filter(origin==input$ori) %>% filter(dest==input$dest) %>%
            head(1) %>%
            select(origin, dest, theory_dist, round, episoid, flat)
    })
    
    
    # map
    
    toListen <- reactive({
        list(input$ori, input$dest)
    })
    
    # two abbreviation of airport => return lon and lat as vec
    get_lon_lat <- function(ori, des){
        result <- distance_data %>% 
            filter(origin == ori) %>%
            filter(dest == des) %>%
            select(ori_lon, dest_lon, ori_lat, dest_lat) %>%
            unlist() %>%
            as.numeric()
        return(result)
    }
    
    point <- eventReactive( toListen(), {
        matrix(get_lon_lat(input$ori, input$dest), nrow=2, ncol=2)
    })
    
    output$map <- renderLeaflet({
        leaflet()  %>% addProviderTiles(providers$Stamen.TonerLite,
                                        options = providerTileOptions(noWrap = TRUE)
                                        ) %>% 
            addMarkers(data = point())
    })
    
    output$data <- DT::renderDataTable({
        return(distance_data)
    })
    
    # data difference vars mutated
    new_distance_data <- distance_data 
    new_distance_data$num <- seq.int(nrow(new_distance_data))
    
    new_distance_data <- new_distance_data %>%
        mutate(delta_theory_round = (theory_dist-round)/theory_dist,
               delta_theory_episoid = (theory_dist-episoid)/theory_dist,
               delta_theory_flat = (theory_dist-flat)/theory_dist) %>%
        filter(abs(delta_theory_round) < 0.020) %>%
        select(num, origin, dest, theory_dist, round, episoid, flat,
               delta_theory_round, delta_theory_episoid, delta_theory_flat)
    
    output$data_differences <- DT::renderDataTable({
        return(new_distance_data %>% select(-num))
    })
    
    output$plot_differences <- renderPlot({
        p1=new_distance_data%>%ggplot(aes(x=num,y=delta_theory_round))+geom_smooth(color='Red',se=F)+	
            geom_abline(intercept = 0, slope = 0)+geom_point(size=1,color='Grey')+
            labs(title = "Dist~Round_Dist", x = "Index", y = "Relative Difference")+
            theme(plot.title = element_text(hjust = 0.5))
        
        p2=new_distance_data%>%ggplot(aes(x=num,y=delta_theory_episoid))+geom_smooth(color='Red',se=F)+	
            geom_abline(intercept = 0, slope = 0)+geom_point(size=1,color='Grey')+
            labs(title = "Dist~Ellipsoid_Dist", x = "Index", y = "Relative Difference")+
            theme(plot.title = element_text(hjust = 0.5))
        
        p3=new_distance_data%>%ggplot(aes(x=num,y=delta_theory_flat))+geom_smooth(color='Red',se=F)+	
            geom_abline(intercept = 0, slope = 0)+geom_point(size=1,color='Grey')+
            labs(title = "Dist~Flat_Dist", x = "Index", y = "Relative Difference")+
            theme(plot.title = element_text(hjust = 0.5))
        
        p_all <- plot_grid(p1,p2,p3,labels='AUTO')
        
        return(p_all)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
