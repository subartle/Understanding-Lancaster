library(shiny)
library(leaflet)
library(tigris)
library(acs)
library(maptools)

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


leaflet(data = Lancaster.Tracts) %>% addTiles() %>%
  addPolygons(fillColor = topo.colors(19, alpha = NULL), stroke = FALSE)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- fluidPage(
  leafletOutput("mymap"),
  p()
)

server <- function(input, output, session) {
  output$mymap <- renderLeaflet({
    leaflet(data = Lancaster.Tracts) %>%
      addTiles() %>%
      addPolygons(fillColor = topo.colors(19, alpha = NULL), stroke = FALSE)
  })
}

shinyApp(ui, server)
