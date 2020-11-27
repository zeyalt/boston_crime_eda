library(shiny)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(readr)
library(rlang)
library(plotly)
library(leaflet)
library(shinyWidgets)
library(sf)
library(shinyjs)
library(tmap)
library(zoo)

##### DEFINE UI #####

ui <- fluidPage(
    
    tags$head(
        tags$style(HTML('.shiny-split-layout>div {overflow: hidden;}')),
    ),
    
    titlePanel("An Exploratory Analysis of Crimes in Boston"),

    # shinythemes::themeSelector(),
    theme = shinythemes::shinytheme('spacelab'),
    
    fluidRow(
        
        column(3, 
               pickerInput(
                   inputId = "ucr",
                   label = "Select UCR Category",
                   choices = c("Part One", "Part Two", "Part Three", "Other"),
                   options = list(`actions-box` = TRUE),
                   multiple = T,
                   selected = c("Part One")
               )
               ),

        column(3,
               pickerInput(
                   inputId = "neighbourhood",
                   label = "Select Neighbourhood",
                   choices = c("Mission Hill", "Fenway", "Back Bay", "Allston",
                               "Brighton", "Jamaica Plain", "Roslindale", "Hyde Park",
                               "West Roxbury", "Mattapan", "Dorchester", "East Boston",
                               "North End", "West End", "Longwood Medical Area", "Roxbury",
                               "South Boston Waterfront", "Charlestown", "South End",
                               "Bay Village", "Leather District", "South Boston", "Harbor Islands"),
                   options = list(`actions-box` = TRUE),
                   multiple = T,
                   selected = c("Mission Hill", "Fenway", "Back Bay", "Allston",
                                "Brighton", "Jamaica Plain", "Roslindale", "Hyde Park",
                                "West Roxbury", "Mattapan", "Dorchester", "East Boston",
                                "North End", "West End", "Longwood Medical Area", "Roxbury",
                                "South Boston Waterfront", "Charlestown", "South End",
                                "Bay Village", "Leather District", "South Boston", "Harbor Islands")
               )
               ),

        column(3,
               dateRangeInput(
                   inputId = "date_range",
                   label = "Select Date Range",
                   start = "2015-01-01",
                   end = NULL
               )
               )
    ),

    tabsetPanel(
        tabPanel("OVERVIEW", 
                 br(),
                 splitLayout(
                     
                     verticalLayout(
                         radioGroupButtons(
                             inputId = "aggregation_mode",
                             label = "Select Mode of Aggregation",
                             choices = c("Year"="YEAR",
                                         "Month"="MONTH",
                                         "Day"="DAY_OF_WEEK",
                                         "Hour"="TIME_HOUR"),
                             justified = TRUE,
                             selected = "TIME_HOUR"),
                         
                         plotlyOutput("by_aggregation_mode", width = 550, height = 250),
                         sliderInput(
                             inputId = "top_n",
                             label = "Select Number of Top Categories to View",
                             min = 3,
                             max = 15,
                             value = 5),  
                         
                         plotlyOutput("most_common", height = 270)
                         
                         ),

                     verticalLayout(
                         plotlyOutput("by_crime_class", width = 570, height = 360),
                         plotlyOutput("by_shooting", width = 570, height = 360)
                     ),
                     
                     verticalLayout(
                         br(),
                         tmapOutput("geospatial_overview", height = 650)
                     )
                 )
        ),
        
        tabPanel("ANALYSIS BY OFFENSE TYPES",
                 br(),
                 splitLayout(
                     cellWidths = c("34%", "66%"),
                     verticalLayout(
                         pickerInput(
                             inputId = "offense",
                             label = "Select Offense Type(s)",
                             choices = ""
                         ),
                         tmapOutput("geospatial", height = 610)
                     ), 
                     verticalLayout(
                         plotlyOutput("time_series", height = 340),
                         splitLayout(
                             plotOutput("heatmap_1", height = 380),
                             plotOutput("heatmap_2", height = 380)
                         )
                     )
                 )
        ),
        
        tabPanel("DATA",
                 br(),
                 DT::dataTableOutput("data")
        )
    )
)

##### DEFINE SERVER #####

server <- function(input, output, session) {
    
    ##### LOAD CRIME DATASET #####
    
    # Define file path
    filepath_crime_geo <- paste(getwd(), "/Data/crime_main_geo.csv", sep="")
    filepath_social <- paste(getwd(), "/Data/3aeae140-8174-4d77-8c0e-de3ef0ce4b672020330-1-1rr22uq.veze.shp", sep = "")
    
    # Define levels of categorical variable
    day_order <- c("Sunday", "Monday", "Tuesday", "Wednesday", 
                   "Thursday", "Friday", "Saturday")
    
    month_order <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                     "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    
    ucr_order = c("Part One", "Part Two", "Part Three", "Other")
    
    # Main crime dataset with geographical information
    crime_main <- read_csv(filepath_crime_geo) %>%
        st_as_sf(coords = c("X", "Y"), crs = 4326) %>%
        rename(DATE = d1) %>%
        mutate(DAY_OF_WEEK = factor(DAY_OF_WEEK, levels = day_order)) %>%
        mutate(MONTH = factor(MONTH, levels = month_order)) %>%
        mutate(UCR = factor(UCR, levels = ucr_order))
    
    geo <- st_read(filepath_social) %>% 
        select(FID, GEOID10, Name) 
    
    ##### DEFINE REACTIVE EXPRESSIONS ####

    r_crime_main <- reactive({
        
        crime_main %>% 
            filter(UCR %in% input$ucr) %>% 
            filter(Name %in% input$neighbourhood) %>% 
            filter(DATE >= input$date_range[1] & DATE <= input$date_range[2]) 
        
    })
    
    r_offense_list <- reactive({
        
        unique(filter(r_crime_main(), UCR %in% input$ucr)$OFFENSE_CODE_GROUP)
        
    })
    
    observe({

        updatePickerInput(session,
                          "offense",
                          choices = unique(crime_main$OFFENSE_CODE_GROUP))
    })

    observe({

        updatePickerInput(
            session,
            inputId = "offense",
            label = "Select Offense Type(s)",
            choices = r_offense_list(),
            selected = r_offense_list()[1]
        )

    })

    r_crime_main_by_offense <- reactive({
        
        r_crime_main() %>% 
            filter(OFFENSE_CODE_GROUP %in% input$offense)
        
    })
    
    ###### DEFINE TAB #1 OUTPUTS #####
    
    output$by_aggregation_mode <- renderPlotly({

        r_crime_main() %>% 
        # crime_main_geo_sf %>%
            st_drop_geometry() %>%
            count(MODE = get(input$aggregation_mode)) %>%
            plot_ly(x = ~MODE, y = ~n, type = "bar", marker = list(color = "#ADEAC3")) %>% 
            layout(
                title = "AGGREGATION OF CRIME REPORTS", 
                xaxis = list(title = ""), 
                yaxis = list(title = "Number of Crime Reports"))
        
    })
    
    output$most_common <- renderPlotly({
        
        r_crime_main() %>% 
            st_drop_geometry() %>% 
            count(OFFENSE_CODE_GROUP) %>% 
            filter(!is.na(OFFENSE_CODE_GROUP)) %>% 
            top_n(n, n = input$top_n) %>%
            plot_ly(x = ~n, y = ~reorder(OFFENSE_CODE_GROUP, n), type = "bar", 
                    orientation = "h", marker = list(color = "#FFA26D"), 
                    textposition='inside') %>% 
            layout(
                title = "TOP CRIME CATEGORIES", 
                xaxis = list(title = "Number of Crime Reports"), 
                yaxis = list(title = "", zeroline = FALSE, showline = FALSE, 
                             showticklabels = FALSE, showgrid = FALSE)) %>% 
            add_annotations(
                text = ~OFFENSE_CODE_GROUP, showarrow=FALSE, xanchor = 'right')
        
    })
    
    output$by_crime_class <- renderPlotly({

        r_crime_main() %>%
            st_drop_geometry() %>% 
            count(CRIME_CLASS) %>%
            filter(!is.na(CRIME_CLASS)) %>% 
            plot_ly(type = "pie", labels = ~CRIME_CLASS,
                    values = ~n, textinfo='label+percent',
                    insidetextorientation='horizontal', 
                    hole = 0.5,
                    marker = list(colors = c('#E3BDD9', '#CB99C8', '#766EC7', '#5153C7', '#76AFEC'))) %>%
            layout(title = "PROPORTION OF CRIME CLASSES", showlegend = FALSE, 
                   margin = list(l=100, t = 100))
        
    })
    
    output$by_shooting <- renderPlotly({
        
        r_crime_main() %>%
            st_drop_geometry() %>%
            count(SHOOTING) %>%
            filter(!is.na(SHOOTING)) %>%
            plot_ly(type = "pie", labels = ~SHOOTING,
                    values = ~n, textinfo='label+percent',
                    insidetextorientation='horizontal',
                    hole = 0.5,
                    marker = list(colors = c('#9DBBE3', '#EE9D94'))) %>%
            layout(title = "PROPORTION OF SHOOTING INCIDENTS", showlegend = FALSE,
                   margin = list(l=100, t = 100))

    })
    
    output$geospatial_overview <- renderTmap({

        r_crime_main() %>%
            count(GEOID10) %>%
                st_drop_geometry() %>% 
                mutate(GEOID10 = as.character(GEOID10)) %>% 
                left_join(geo, by = "GEOID10") %>%
                st_as_sf() %>%
                tm_shape() +
                tm_polygons("n", palette = "Blues", alpha = 0.6, title = c(""), 
                            popup.vars=c("Geo Code: " = "GEOID10", 
                                         "Neighbourhood" = "Name", 
                                         "Number of Crime Reports" = "n"))

    })
    
    ###### DEFINE TAB #2 OUTPUTS #####
    
    output$geospatial <- renderTmap({
        
        r_crime_main_by_offense() %>%
            tm_shape() +
            tm_dots("OFFENSE_DESCRIPTION", 
                    popup.vars=c("Date" = "DATE",
                                 "Time" = "TIME",
                                 "Neighbourhood" = "Name", 
                                 "Street Name" = "STREET",
                                 "Offense Description" = "OFFENSE_DESCRIPTION")) + 
            tm_view(view.legend.position = c("left", "bottom"))
        
    })
    
    output$time_series <- renderPlotly({
        
        r_crime_main_by_offense() %>% 
            st_drop_geometry() %>% 
            count(DATE) %>%
            mutate(MOV_AVG = rollmean(n, 14, fill = NA)) %>%
            
            plot_ly(x = ~DATE, y = ~n, name = 'Actual', type = 'scatter', mode = 'lines', line = list(width = 0.7)) %>%
            add_trace(y = ~MOV_AVG, name = '14-Days Moving Average', mode = "lines", line = list(width = 2)) %>%
            layout(yaxis = list(title = "Number of Crime Reports"), xaxis = list(title = ""),
                   legend = list(x = 0.1, y = 0.9, orientation = 'h'))
        
        
    })
    
    output$heatmap_1 <- renderPlot({
        
        r_crime_main_by_offense() %>%
            count(DAY_OF_WEEK, MONTH) %>%
            st_drop_geometry() %>%
            spread(key = MONTH, value = n) %>%
            replace(is.na(.), 0) %>%
            gather(seq(2, 1 + length(unique(r_crime_main_by_offense()$MONTH))), key = "MONTH", value = n, -DAY_OF_WEEK) %>%
            mutate(MONTH = factor(MONTH, levels = month_order)) %>%
            ggplot(aes(x = DAY_OF_WEEK, y = MONTH, fill = n)) +
            geom_tile(colour="white") +
            scale_fill_distiller(palette = "YlOrRd", direction = 1, name = "No. of\nCrime Reports") +
            xlab("") +
            ylab("") +
            theme_bw() + theme_minimal() +
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  axis.text.x = element_text(size = 14, angle = 90, vjust = 0.1, hjust = 1),
                  axis.text.y = element_text(size = 14),
                  legend.position = "right")
        
    })
    
    output$heatmap_2 <- renderPlot({

        r_crime_main_by_offense() %>%
            count(DAY_OF_WEEK, TIME_HOUR) %>%
            st_drop_geometry() %>%
            spread(key = TIME_HOUR, value = n) %>%
            replace(is.na(.), 0) %>%
            gather(seq(2, 1 + length(unique(r_crime_main_by_offense()$TIME_HOUR))), key = "TIME_HOUR", value = n, -DAY_OF_WEEK) %>%
            mutate(TIME_HOUR = factor(TIME_HOUR, levels = sort(unique(r_crime_main()$TIME_HOUR)))) %>%
            ggplot(aes(x = DAY_OF_WEEK, y = TIME_HOUR, fill = n)) +
            geom_tile(colour="white") +
            scale_fill_distiller(palette = "YlGnBu", direction = 1, name = "No. of\nCrime Reports") +
            xlab("") +
            ylab("Hour of the Day") +
            theme_bw() + theme_minimal() +
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  axis.text.x = element_text(size = 14, angle = 90, vjust = 0.1, hjust = 1),
                  axis.text.y = element_text(size = 14),
                  legend.position = "right")

    })
    
    ##### DEFINE TAB #3 OUTPUTS #####

    output$data <- DT::renderDataTable({

        r_crime_main() %>%
            as.data.frame() %>%
            select(c("DATE", "DAY"="DAY_OF_WEEK", "TIME", "NEIGHBOURHOOD"="Name",
                     "STREET", "OFFENSE CODE GROUP"="OFFENSE_CODE_GROUP", "OFFENSE DESCRIPTION"="OFFENSE_DESCRIPTION",
                     "UCR", "SHOOTING"))


    })

}

##### RUN APPLICATION ##### 

shinyApp(ui = ui, server = server)
