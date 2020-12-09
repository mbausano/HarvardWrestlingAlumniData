

library(readxl)
library(sf)
library(fs)
library(leaflet)
library(raster)
library(sp)
library(janitor)
library(usmap)
library(maps)
library(purrr)
library(DT)
library(shiny)
library(shinythemes)
library(tidyverse)
library(gt)
library(gtsummary)

alum
states <- c("MA", "NY", "AZ", "FL", "CA", "TX",
            "MI", "PA", "OH", "IL", "IA",
            "HI", "OK", "CT", "MD", "NJ",
            "WV")

ui <- fluidPage(
    navbarPage("Harvard Wrestling Database", theme = shinytheme("superhero"),
               
               #The first tab is the map which was made using leaflet
               tabPanel("Map",
                        fluidPage(
                            titlePanel("Home States of Alumni Based in the United States"),
                            
                            hr(),
                            
                            leafletOutput("mymap", height = "600"),
                            p("This  map  shows the home state of Wrestling Alums, there are no current foreign born wrestlers.")
                        )),
               
               
               #The second tab is the search tab which was made using DT in the server.
               tabPanel("Search",
                        titlePanel("Wrestling Alumns"),
                        mainPanel(
                            DTOutput('search'), width = "100%", height = "auto"
                        )),
              
                tabPanel("Regression",
                        titlePanel("Industry by Region"),
                        sidebarLayout(sidebarPanel(
                            checkboxGroupInput(
                                "State",
                                ("Pick a state:"),
                                choiceNames =
                                    states,
                                choiceValues =
                                    states,
                                selected = c("NY", "MA")
                            )
                        ),
                        mainPanel( plotOutput("mygraph"), 

                                   
                                   gt_output("mytable")
                            
                      ) )),
               #The third tab is the About page.
               tabPanel("About",
                        titlePanel(h3("Consolidation and Analysis of The Harvard Wrestling ALumni Network")),
                        
                        hr(),
                        h4("Background"),
                        p("This project aimed to consolidatevarious sources of data into one working database
                           of current and former Harvard wrestlers. Beyond just the wrestling team, any Excel could
                           now be read into the app, and it would provide the same features and usability, ideally
                           providing other teams, and organizations with a tool that they could use to collate and 
                           analyze their alumni databases"),
                        
                        # The br() function adds white space to the app.
                        # Data Collection Explanation
                        br(),
                        h4("Sources for Data"), 
                        p("Data for this project was maually pulled from various sources including; past team rosters,
                          LinkedIn, the Harvard Varsity Club, and existing contact information. Given the spread of 
                          sources,  the manual input of data was one of the more tedious aspects of the project. Ideally,
                          a google form or survey couldbe produced that inputs data directly into the existing excel for 
                          future iterationns."),
                        br(),
                        
                        # Map Page Explanation
                        h4("The US Map"),
                        p("The map is a blank U.S. state map that uses the Leaflet program to overlay data onto it. By 
                          building polygons that represent the states and assigning each wrestlers home city a  distinct
                          latitude and longitude, the app can map each wrestler directly to their home town, giving some 
                          sense of geographic diversity."),
                        br(),
                        
                        # Search Page Explanation
                        h4("The Directory"),
                        p("This is a searchable database. It places all the important data from all data sources in one place
                          that allows the user to search by a word in ay of the listed columns. Ideally, a name pulled from the
                          visual interface prior could  then be searched here for further information about the individual"),
                        br(),
                        
                        h4("The Regression"),
                        p("The regression provides an estimation, based on the class year, of the predicted size of the team
                          although currentlimitations to the sample size do not make it as reliable, as the dataset grows, 
                          the regression should become an increasingly accurate predictor of team size"),
                        br(),
                        
                        h4("The Graph"),
                        p("This is a graphical analysis of industry by counnt, each point is labeled  by state and the radio 
                          buttons allow the user to sort by only states that are of interested. This was an interesting mini case
                          study in both the prevelance of different careers in different geographic regions and the popularity more
                          broadly of various cities post-grad"),
                        br(),
                        
                        #Bio
                        h4("About"),
                        p("My name is Pierce Bausano and  I am a junior in Lowell House concentrating in Government with a Secondary
                          in economics. Post graduation, I plan on working for a few years before pursuing the possibility of graduate 
                          school"),
                        br(),
                        
                        #Embed the video link into the About Page.
                        h4("Project Video"),
                        p("Blank"),
                        br(),
                        
                        
                        h4("Note"),
                        p("Blank"), 
                        
                        br(),
                        hr(),
                        # Repository Link
                        h5("Github Repository:"))))


#The server portion of the app, which takes the inputs and produces outputs
server <- function(input, output, session) {
    
    output$search <- DT::renderDT({
        table_data <- alum %>% 
            dplyr::select(Name, GradYear, State, City, 
                   Concentration, Industry, Employment, Interests,
                   Email, CellNumber) 
        
        colnames(table_data) <- c("Name", "Social Class", "State", "City", 
                                   "Concentration", "Indsutry", "Employment", 
                                  "Interests", "Email", "CellNumber")
        table_data %>% 
            datatable(extensions = c('Responsive', 'Buttons'), options = list(
                dom = 'Bfrtip',
                buttons = c('copy', 'excel', 'pdf')
            ), rownames = FALSE)
            
        
    }, escape = FALSE)
    
    #Create the map ouput using leaflet
    output$mymap <- renderLeaflet({
        mapStates = maps::map("state", fill = TRUE, plot = FALSE)
        leaflet(data = mapStates) %>% 
            addTiles() %>%
            addPolygons(fillColor = terrain.colors(15, alpha = NULL), stroke = FALSE)
        
        leaflet(data = mapStates) %>%
            addProviderTiles(providers$Thunderforest.Transport,
                             options = providerTileOptions(noWrap = FALSE)
            ) %>%
            addTiles() %>%
            # Making sure the map has the shapes of the states, is colorful, and has markers.
            addPolygons(fillColor = terrain.colors(15, alpha = NULL), stroke = FALSE) %>%
            addMarkers(data = alum, ~lng, ~lat, popup = ~as.character(Name), 
                      icon = greenLeafIcon)
    })
    
    output$mygraph <- renderPlot({
        clean_alum <- alum %>%
            dplyr::select(State, Industry) %>%
            drop_na() %>% 
            group_by(Industry, State) %>%
            summarize(count = n())
        statselected <- input$State
        clean_alum %>%
            filter(State %in% statselected) %>%
            ggplot(mapping = aes( x = Industry, y= count)) +
            ggrepel::geom_label_repel(aes(label = State), label.size = .05, color = "blue") +theme_minimal() +
            labs(title = "Number of Wreslters in Each Profession", 
                 subtitle = "Labeled by State",
                 y = "Number of Wreslters", x = "Industry")
    })
    
    output$mytable <- render_gt({
        alum_by_year <- alum %>%
            dplyr::select(GradYear) %>%
            drop_na() %>%
            group_by(GradYear) %>%
            summarize(count = n()) 
        lmmodel <- lm(count ~ GradYear, data = alum_by_year) %>%
            tbl_regression(intercept = TRUE) %>%
            as_gt()
        
    })
}


# Run the application 
shinyApp(ui = ui, server = server)