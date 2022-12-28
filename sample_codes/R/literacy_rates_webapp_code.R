
library(shiny)
library(tidyverse)
library(leaflet)
library(sf)
library(rsconnect)
library(sys)


# This Web App lets the user see the global spatial and temporal distribution of 
# literacy rates at country level
# The Web App can be seen at https://nicolasbeglinger.shinyapps.io/geo878-group01/

raw_data <- readRDS("data/RDS/joined.rds")

# define the colours that are later used in the map and the ggplot, they're suitable for colourblind people
mapColors <- c('#ffffa9',
               '#edf8b1',
               '#c7e9b4',
               '#7fcdbb',
               '#41b6c4',
               '#1d91c0',
               '#225ea8',
               '#253494',
               '#081d58')

# define small functions to extract minimum and maximum values of vectors to define the y-range in the plot and to scale the colour map
my.max <- function(x) ifelse( !all(is.na(x)), max(x, na.rm=T), NA)
my.min <- function(x) ifelse( !all(is.na(x)), min(x, na.rm=T), NA)


# create the user interface
ui <- fluidPage(
  # set the background colour of the map to white
  tags$head(
    tags$style(HTML(".leaflet-container { background: #FFFFFF; }"))
  ),
  
  # create sidebar layout
  sidebarLayout(
    sidebarPanel(
      # create slider for the user to choose the year displayed
      sliderInput("year",
                  "Year:",
                  min = 1970,
                  max = 2020,
                  value = 2000,
                  sep = ""),
      # create radio buttons to let the user decide what thematic data should be displayed
      radioButtons(inputId = "dataKind",
                   label = "Data Kind",
                   choices = c(
                     "Labour Force Participation" = "labour",
                     "Literacy Rate" = "literacyRate"
                   )),
      # create radio buttons to let the user decide to which population group the data displayed belongs
      radioButtons(inputId = "group",
                   label = "Population Group",
                   choiceNames = list("Female", "Male", "Adult", HTML("Difference between female and male<br/><p style=\"font-size:11px\">Female [%] – Male [%]</p>")),
                   choiceValues = list("Female", "Male", "Adult", "Diff"),
                   selected = "Diff"
      ),
      # plot the ggplot that is created in the server
      plotOutput("linePlot",
                 height = 270),
      # simple text input that is generated in the server and informs the user about the two lines that are displayed in the ggplot
      uiOutput("legend")
      
    ),
    # create the mainpanel with the map in it
    mainPanel(
      leafletOutput("map",
                    width = "100%",
                    height = 600)
    )
  )
)

# create the server
server <- function(input, output) {
  
  # reactive variable that changes depending on what the user chose for the datakind and population group
  # the value corresponds to the respective column name in the dataframe
  dataVariable = reactive(paste(input$dataKind, input$group, sep = ""))
  
  # generate ggplot to be passed to the ui
  output$linePlot = renderPlot({
    # define the title for the y-axis
    columnName = switch(dataVariable(),
                        literacyRateFemale = "Literacy Rate Female",
                        literacyRateAdult = "Literacy Rate Adult",
                        literacyRateMale = "Literacy Rate Male",
                        literacyRateDiff = "Difference of Literacy Rates\nbetween Female and Male",
                        labourFemale = "Labour Force Participation Female",
                        labourMale = "Labour Force Participation Male",
                        labourAdult = "Labour Force Participation Adult",
                        labourDiff = "Difference of Labour Force Participation\nbetween Female and Male"
                        
    )
    

    # when no country is clicked, Bangladesh is displayed first 
    if (is.null(input$map_shape_click$id)) {
      # iso3 value to filter the dataframe for a specific country
      filter <- "BGD"
      # Define name to display in title and in the text-legend
      name <- "Bangladesh"
      # column name of the dataframe
      yValue <- dataVariable()
      # define range of the y-axis
      range = c(my.min(get(yValue, raw_data)),
                my.max(get(yValue, raw_data)))
    } else if (dataVariable() %in% c("literacyRateDiff", "labourDiff")) {
      filter <- input$map_shape_click$id
      name <- select(filter(raw_data, iso3 == input$map_shape_click$id), name)[[1]][1]
      yValue <- dataVariable()
      range = c(my.min(get(yValue, raw_data)),
                my.max(get(yValue, raw_data)))
    } else {
      filter <- input$map_shape_click$id
      name <- select(filter(raw_data, iso3 == input$map_shape_click$id), name)[[1]][1]
      yValue <- dataVariable()
      range = c(0,100)
    } 
    
    # create relevant subset of the data
    filtered <- raw_data %>% filter(iso3 == filter)
    
    # create legend that is passed to the ui
    output$legend = renderUI(HTML(
      paste("<p style=\"font-size:13px\"><strong>Black line</strong>: Aggregated World Data", paste("<strong>Coloured Line</strong>: ", name, "</p>"), sep = "<br/>")
    ))
    
    print(get(yValue, filtered))
    
    # create ggplot
    ggplot() +
      # lineplot for the country that is selected
      geom_line(data = filtered,
                mapping = aes(x = year,
                              y = get(yValue, filtered),
                              colour = get(yValue, filtered)),
                lwd = 2) +
      # specify continuous color gradient that matches the colours of the map
      scale_color_gradientn(colours = mapColors,
                            guide = F,
                            limits = range) +
      # create black lines for the world data
      geom_line(data = raw_data %>% filter(iso3 == "WLD"),
                mapping = aes(x = year,
                              y = get(yValue, raw_data %>% filter(iso3 == "WLD")))) +
      
      labs(title = name,
           y = paste(columnName,"[%]"),
           x = "Year") +
      ylim(range) +
      # set plotbackground to same colour as sidepanel
      theme(panel.background = element_rect(fill = "#F5F5F5", colour = NA),
            plot.background = element_rect(fill = "#F5F5F5", colour = NA),
            # remove minor grid lines
            panel.grid.minor = element_blank())
    
  })
  
  
  # create reactive dataframe subset of one year with one thematic data column
  data <- reactive({
    
    raw_data %>%
      mutate(selected_column = get(dataVariable(), raw_data)) %>%
      filter(year == input$year)
    
  })
  
  # create reactive colourmap that depends on the data variable that should be displayed
  colorpal <- reactive({
    
    if (dataVariable() %in% c("literacyRateDiff", "labourDiff")) {
      colorBin(palette = mapColors,
               domain = c(my.min(get(dataVariable(), raw_data)),
                          my.max(get(dataVariable(), raw_data))),
               bins = 5,
               na.color = "#bdbdbd")
    } else {
      colorBin(mapColors,
               c(0,100),
               5,
               na.color = "#bdbdbd")
    }
    
  })
  
  # create basis for the leaflet map
  output$map <- renderLeaflet({
    
    map <- leaflet(data(),
                   # set the zoom limits
                   options = leafletOptions(minZoom = 1.25,
                                            maxZoom = 5)) %>%
      setMaxBounds(-180, -80, 180, 90) %>%
      # add home button
      addEasyButton(easyButton(
        icon = "fa-globe", 
        title = "See whole world",
        onClick = JS("function(btn, map){ map.setZoom(1); }")))
    
  })
  
  
  # create part of the map that should change depending on the input of the user
  observe({
    pal = colorpal()
    
    # create the legend title
    columnName = switch(dataVariable(),
                        literacyRateFemale = "Literacy Rate Female",
                        literacyRateAdult = "Literacy Rate Adult",
                        literacyRateMale = "Literacy Rate Male",
                        literacyRateDiff = HTML(paste("Difference of Literacy Rates between", "Female and Male", sep = "<br>")),
                        labourFemale = "Labour Force Participation Female",
                        labourMale = "Labour Force Participation Male",
                        labourAdult = "Labour Force Participation Adult",
                        labourDiff = HTML(paste("Difference of Labour Force Participation", "between Female and Male", sep = "<br>"))
    )
    
    # create html for the popups (if literacy rate is selected, only information regarding literacy rate
    # should be displayed, the same for labour force participation)
    if (startsWith(input$dataKind, "literacyRate")) {
      popup = paste0("<strong>",
                     data()$name,
                     "</strong>",
                     "<br>",
                     "Literacy Rate Female: ",
                     round(data()$literacyRateFemale),
                     " ",
                     "<span>&#37;</span>",
                     "<br>",
                     "Literacy Rate Male: ",
                     round(data()$literacyRateMale),
                     " ",
                     "<span>&#37;</span>",
                     "<br>",
                     "Literacy Rate Adult: ",
                     round(data()$literacyRateAdult),
                     " ",
                     "<span>&#37;</span>",
                     "<br>",
                     "Difference of Literacy Rates",
                     "<br>",
                     "between Female and Male: ",
                     round(data()$literacyRateDiff),
                     " ",
                     "<span>&#37;</span>")
    } else if (startsWith(input$dataKind, "labour")) {
      popup = paste0("<strong>",
                     data()$name,
                     "</strong>",
                     "<br>",
                     "Labour Force Participation Female: ",
                     round(data()$labourFemale),
                     " ",
                     "<span>&#37;</span>",
                     "<br>",
                     "Labour Force Participation Male: ",
                     round(data()$labourMale),
                     " ",
                     "<span>&#37;</span>",
                     "<br>",
                     "Labour Force Participation Adult: ",
                     round(data()$labourAdult),
                     " ",
                     "<span>&#37;</span>",
                     "<br>",
                     "Difference of Labour Force Participation",
                     "<br>",
                     "between Female and Male: ",
                     round(data()$labourDiff),
                     " ",
                     "<span>&#37;</span>")
    }
    
    # create label for the information that should be displayed when hovering over a country
    label = paste0("<strong>",
                   data()$name,
                   "</strong>")
    # isolate: the map only refreshes, if a button is clicked
    isolate({
      # get the viewed extent of the map before changing the data and save it to zoom back in
      bounds <- c(input$map_bounds$west,
                  input$map_bounds$north,
                  input$map_bounds$east,
                  input$map_bounds$south)
      
      # create labels for the legend classes
      if (dataVariable() == "literacyRateDiff") {
        labels <- c("-40 – -31%", "-30 – -21%", "-20 – -11%", "-10 – -1%", "0 – 9%", "10 – 20%", "No Data")
        
      } else if (dataVariable() == "labourDiff") {
        labels <- c("-100 – -81%", "-80 – -61%", "-60 – -41%", "-40 – -21%", "-20 – -1%", "0 – 20%", "No Data")
        
      } else {
        labels <- c("0 – 19%", "20 – 39%", "40 – 59%", "60 – 79%", "80 – 100%", "No Data")
      }
      
      # create dynamic part of the leaflet map
      leafletProxy("map", data = data())  %>%
        addPolygons(
          color = "black",
          weight = 1,
          fillOpacity = 1,
          fillColor = ~ pal(selected_column),
          # highlight the boundaries of the countries when hovering over it
          highlightOptions = highlightOptions(color = "white", 
                                              weight = 2, 
                                              bringToFront = TRUE),
          popup = popup,
          label = lapply(label, HTML),
          # set the id of countries to retrieve them in the ggplot
          layerId = ~iso3) %>%
        addLegend(
          "bottomright",
          title = columnName,
          pal = pal,
          values = ~ selected_column,
          labFormat = function(type, cuts, p) {paste0(labels)},
          opacity = 0.8
        ) %>%
        # zoom the map to the extent that was viewed before changing the data
        fitBounds(
          bounds[1],
          bounds[2],
          bounds[3],
          bounds[4]
        )
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

