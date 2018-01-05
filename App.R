library(shiny)
library(leaflet)
library(tigris)
library(acs)
library(maptools)
library(base)

Dat.ACS <-
  read.csv(
    "https://raw.githubusercontent.com/subartle/Understanding-Lancaster/master/Employment_ACS16_Cleaned.csv"
  )

LancasterCounty.Tracts <- tracts(state = 42, county = 071, cb = TRUE)
Lancaster.Tracts <- LancasterCounty.Tracts[LancasterCounty.Tracts$NAME == "1" |
                                             LancasterCounty.Tracts$NAME == "2" |
                                             LancasterCounty.Tracts$NAME == "3" |
                                             LancasterCounty.Tracts$NAME == "4" |
                                             LancasterCounty.Tracts$NAME == "5" |
                                             LancasterCounty.Tracts$NAME == "6" |
                                             LancasterCounty.Tracts$NAME == "7" |
                                             LancasterCounty.Tracts$NAME == "8" |
                                             LancasterCounty.Tracts$NAME == "9" |
                                             LancasterCounty.Tracts$NAME == "10" |
                                             LancasterCounty.Tracts$NAME == "11" |
                                             LancasterCounty.Tracts$NAME == "12" |
                                             LancasterCounty.Tracts$NAME == "14" |
                                            #LancasterCounty.Tracts$NAME == "118.02" |
                                             LancasterCounty.Tracts$NAME == "118.05" |
                                            #LancasterCounty.Tracts$NAME == "132.02" |
                                            #LancasterCounty.Tracts$NAME == "133.01" |
                                            #LancasterCounty.Tracts$NAME == "134" |
                                            #LancasterCounty.Tracts$NAME == "135.03" |
                                             LancasterCounty.Tracts$NAME == "147" ,]

Lancaster <- merge(Lancaster.Tracts, Dat.ACS, by.x = "NAME", by.y = "NAME", all.x = TRUE)

bins <- c(0, 3, 6, 9, 12, 15, 18, 21)
pal <- colorBin("YlOrRd", domain = Lancaster$X16plus_U, bins = bins)
pal2 <- colorBin("YlOrRd", domain = Lancaster$X25to64_U, bins = bins)
labels <- paste("Rate Unemployed: ", Lancaster$X16plus_U, ".  Total Unemployed: ", Lancaster$X16plus_T*(.01*Lancaster$X16plus_U))
labels2 <- paste("Rate Unemployed: ", Lancaster$X25to64_U, ".  Total Unemployed: ", Lancaster$X25to64_T*(.01*Lancaster$X25to64_U))

ui <- fluidPage(
  column(6, leafletOutput("mymap", height = "800px")),
  column(6, leafletOutput("mymap2", height = "800px")))

server <- function(input, output, session) {
  output$mymap <- renderLeaflet({
    leaflet(data = Lancaster) %>% addTiles() %>%
      addPolygons(fillColor = ~pal(X16plus_U),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.7,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "white",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(
                    stylestyle = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>% 
      addLegend(pal = pal, values = ~X16plus_U, opacity = 0.7, title = NULL,
                                                       position = "bottomright")
  })
    
    output$mymap2 <- renderLeaflet({
      leaflet(data = Lancaster) %>% addTiles() %>%
        addPolygons(fillColor = ~pal2(X25to64_U),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                      weight = 5,
                      color = "white",
                      dashArray = "",
                      fillOpacity = 0.7,
                      bringToFront = TRUE),
                    label = labels2,
                    labelOptions = labelOptions(
                      stylestyle = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")) %>% 
        addLegend(pal = pal2, values = ~X25to64_U, opacity = 0.7, title = NULL,
                  position = "bottomright")  
  })
}
shinyApp(ui, server)
