library(shiny)
library(shinythemes)
library(leaflet)
library(tigris)
library(acs)
library(maptools)
library(base)
library(dygraphs)
library(png)
library(plotly)
library(rgeos)
library(tidyverse)
library(rgdal)
library(ggthemes)


Dat.ACS <-
  read.csv(
    "https://raw.githubusercontent.com/subartle/Understanding-Lancaster/master/Employment_ACS16_Cleaned.csv"
  )

Dat.LAUS <-
  read.csv(
    "https://raw.githubusercontent.com/subartle/Understanding-Lancaster/master/LAUS.csv"
  )

Dat.Graph1 <-
  read.csv("https://raw.githubusercontent.com/subartle/Understanding-Lancaster/master/Inclusion%20ACS%20Graph%201.csv")

Dat.Graph2 <-
  read.csv("https://raw.githubusercontent.com/subartle/Understanding-Lancaster/master/Inclusion%20ACS%20Graph%203.csv")

Dat.Map <-
  read.csv("https://raw.githubusercontent.com/subartle/Understanding-Lancaster/master/Inclusion%20ACS%20Graph%204.csv")

Dat.ABGroups <-
  read.csv("https://raw.githubusercontent.com/subartle/Understanding-Lancaster/master/CityParcels_Blocks2.csv")

colnames(Dat.Graph2) <- c("Description", "Total Population (16+)", "Labor Force Participation Rate", "Employment Ratio",
                          "Unemployment Rate", "FilterOne", "FilterTwo", "Color", "Color2", "Color3")

Dat.Graph1$Lable <- as.factor(Dat.Graph1$Lable)
Dat.Graph1$Percent_Change2 <- as.numeric(Dat.Graph1$Percent_Change*100)

Dat.Graph1$Color <- "Peer PA City"
Dat.Graph1$Color <- ifelse(Dat.Graph1$Geography == "Pennsylvania", "Pennsylvania", Dat.Graph1$Color)
Dat.Graph1$Color <- ifelse(Dat.Graph1$Geography == "Lancaster city, Pennsylvania", "Lancaster", Dat.Graph1$Color)
Dat.Graph1$Color <- ifelse(Dat.Graph1$Geography == "United States", "United States", Dat.Graph1$Color)

Dat.Graph2$Color <- " "
Dat.Graph2$Color <- ifelse(Dat.Graph2$Color2 == "Black", "  ", Dat.Graph2$Color)

ax <- list(
  title = "")
ay <- list(
  title = "% Change")
ay2 <- list(
  title = "")

InclusionPal1 <- c("gray", "red", "blue", "gold")
InclusionPal1 <- setNames(InclusionPal1, c("Peer PA City", "Lancaster", "United States", "Pennsylvania"))
InclusionPal2 <- c("black", "blue", "blue", "blue", "green", "green", "red", "gold", "red", "green",
                   "orange", "red", "gold", "gold", "grey", "blue", "blue", "green", "green")
InclusionPal2 <- setNames(InclusionPal2, unique(Dat.Graph2$Description))

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
Dat.Map$CT2 <- as.character(Dat.Map$Census.Tract)
Dat.Map2 <- merge(Dat.Map, Lancaster.Tracts, by.x = "CT2", by.y = "NAME", all.x = TRUE)

#download block groups into R
LancasterCounty.BlockGroups <- block_groups(state = 42, county = 071, cb = TRUE)
Lancaster.BlockGroups <- LancasterCounty.BlockGroups[LancasterCounty.BlockGroups$GEOID == "420710147003" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710135031" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710147001" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710135011" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710134002" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710133013" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710133012" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710133011" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710118052" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710014004" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710014003" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710014002" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710014001" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710012004" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710012003" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710012002" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710012001" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710001001" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710001002" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710001003" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710002001" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710002002" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710003001" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710003002" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710003003" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710003004" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710004001" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710004002" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710004003" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710004004" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710005001" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710005002" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710005003" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710005004" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710006001" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710006002" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710006003" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710006004" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710007001" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710007002" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710007003" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710008001" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710008002" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710008003" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710008004" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710009001" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710009002" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710009003" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710009004" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710010002" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710010001" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710010003" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710010004" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710011001" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710011002" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710132021" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710147002" |
                                                       LancasterCounty.BlockGroups$GEOID == "420710011003" ,]

Dat.ABGroups$GEOID <- as.character(Dat.ABGroups$GEOID1)

LancasterBGs <- merge(Lancaster.BlockGroups, Dat.ABGroups, by.x = "GEOID", by.y = "GEOID", all.x = TRUE)

d <- as.ts(Dat.LAUS$LAUS_UnemploymentRate)
#dygraph(d) %>% dyRangeSelector()

bins <- c(0, 3, 6, 9, 12, 15, 18, 21)
pal <- colorBin("YlOrRd", domain = Lancaster$X16plus_U, bins = bins)
pal2 <- colorBin("YlOrRd", domain = Lancaster$X25to64_U, bins = bins)
labels <- paste("Rate Unemployed: ", Lancaster$X16plus_U, ".  Total Unemployed: ", Lancaster$X16plus_T*(.01*Lancaster$X16plus_U))
labels2 <- paste("Rate Unemployed: ", Lancaster$X25to64_U, ".  Total Unemployed: ", Lancaster$X25to64_T*(.01*Lancaster$X25to64_U))

ui <- fluidPage(theme = shinytheme("flatly"),
                mainPanel(
                  h2("Lancaster & Inclusion"),
                  tabsetPanel(
                    tabPanel(
                      h4("Compared to Other Cities"),
                      h5("In 2017, The Brookings Institution published a set of metrics designed to 
                         'provides leaders across metropolitan America with a set of objective metrics to guide their
                         efforts in shaping advanced regional economies that work for all.' Using the similar data and 
                         methodology, this dashboard replicates Brookings objective metrics and analysis around inclusion 
                         for Lancaster City and its peer Pennsylvanian Cities."),
                      h5("Brookings defines inclusion metrics as 'measures of how the benefits of growth and prosperity 
                         in a metropolitan economy (specifically, changes in employment and income) are distributed among individuals. 
                         Inclusive growth enables more people to invest in their skills and to purchase more goods and services. 
                         Thus, inclusive growth can increase human capital and raise aggregate demand, boosting prosperity and growth. 
                         Ensuring that all people can contribute to and benefit from growth and prosperity also helps sustain widespread 
                         support for the policies on which growth and prosperity depend.'"),
                      tags$ol(
                        tags$li("% Employed (of CLF): # Employed/Civilian Labor Force (see Data & Methodology for more info)"),
                        tags$li("% Unemployed (of CLF): # Unemployed/Civilian Labor Force (see Data & Methodology for more info)"),
                        tags$li("Median Household Income: The estimated income of the household in the middle of a city's income
                                distribution (among people at least 16 years old)."),
                        tags$li("Relative Poverty: The share of people earning less than half of the local median wage (among
                                people at lease 16 years old).")),
                      h6("Note: As Lancaster and many of its peer cities are too small for accurate yearly ACS estimates,
                         the 2007 - 2011 and 2012 - 2016 5-Year Estimates were used to evaluate change. See the Data & 
                         Methodolgy tab for more information."),
                      tags$hr(),
                      tags$hr(),
                      h4(align = 'center', "% Change in Employment & Income across Pennsylvanian Cities"),
                      h5(align = 'center', "Data: ACS 5-Year Estiamtes from 2011 & 2016"),
                      plotlyOutput("InclusionPlot1"),
                      tags$hr(),
                      tags$hr(),
                      fixedRow(align = 'center',
                        column(6,
                               h4("% Change in Employment & Income in Pennsyvanian Cities"),
                               h5("Data: ACS 5-Year Estiamtes from 2011 & 2016")),
                        column(6,
                               selectInput(
                                 inputId = "Input1",
                                 label = "Percent Change in... (use the dropdown to choose your metric)",
                                 choices = unique(Dat.Graph1$Lable),
                                 selected = "% Employmed (of CLF)"
                                 ))),
                      fixedRow(
                        column(12,
                               leafletOutput("InclusionPMap1", height = '450px', width = '900px'))),
                      tags$hr(),
                      tags$hr()),
                    tabPanel(h4("Subpopulations & Neighborhoods"),
                             fixedRow(align = 'center', h3("Unemployment Rate")),
                             fixedRow(align = 'center',
                                 column(6, 
                                        selectInput(
                                          inputId = "Input2",
                                          label = "Select a Category",
                                          choices = unique(Dat.Graph2$FilterTwo),
                                          selected = "Race"
                                        )),
                                 column(6,
                                        selectInput(
                                          inputId = "Input3",
                                          label = "Select a Year",
                                          choices = unique(Dat.Graph2$FilterOne),
                                          selected = "2016"
                                        ))),
                             fixedRow(plotlyOutput("InclusionPlot5")),
                             fixedRow(align = 'center',
                                      column(6, selectInput(
                                        inputId = "Input4",
                                        label = "Select a Subpopulation",
                                        choices = list(
                                          Race = c(`American Indian and Alaska Native` = 'American Indian and Alaska Native', 
                                                   `Native Hawaiian and Other Pacific Islander` = 'Native Hawaiian and Other Pacific Islander',
                                                   `Black or African American` = 'Black or African American',
                                                   `Asian` = 'Asian',
                                                   `White` = 'White',
                                                   `Some other race` = 'Some other race'),
                                          Ethnicity = c(`Hispanic or Latino origin (of any race)` = 'Hispanic or Latino origin (of any race)',
                                                        `White alone, not Hispanic or Latino` = 'White alone, not Hispanic or Latino'),
                                          Age = c(`16 to 19 years` = '16 to 19 years',
                                                  `20 to 24 years` = '20 to 24 years',
                                                  `20 to 64 years` = '20 to 64 years',
                                                  `25 to 64 years` = '25 to 64 years',
                                                  `65 to 74 years` = '75 years and over'),
                                          Education = c(`Less than high school graduate` = 'Less than high school graduate', 
                                                        `High school graduate (includes equivalency)` = 'High school graduate (includes equivalency)',
                                                        `Some college or associates degree` = 'Some college or associates degree',
                                                        `Bachelors degree or higher` = 'Bachelors degree or higher'),
                                          Sex = c(`Female` = 'Female',
                                                     `Male` = 'Male'),
                                          Other = c(`With any disability` = 'With any disability',
                                                    `Civilian Workforce (16 years and over)` = 'Civilian Workforce (16 years and over)',
                                                    `Below poverty level` = 'Below poverty level')), selectize = FALSE)),
                                      column(6, selectInput(
                                        inputId = "Input5",
                                        label = "Select a Year",
                                        choices = unique(Dat.Map2$FilterOne),
                                        selected = "2016"))),
                             fixedRow(leafletOutput("mymap3", height = "500")),
                             fixedRow(align = 'center', h3("Population Count")),
                             fixedRow(align = 'center',
                                      column(6, 
                                             selectInput(
                                               inputId = "Input7",
                                               label = "Select a Category",
                                               choices = unique(Dat.Graph2$FilterTwo),
                                               selected = "Race"
                                             )),
                                      column(6,
                                             selectInput(
                                               inputId = "Input8",
                                               label = "Select a Year",
                                               choices = unique(Dat.Graph2$FilterOne),
                                               selected = "2016"
                                             ))),
                               fixedRow(plotlyOutput("InclusionPlot2")),
                             fixedRow(align = 'center',
                                      column(6, selectInput(
                                        inputId = "Input9",
                                        label = "Select a Subpopulation",
                                        choices = list(
                                            Race = c(`American Indian and Alaska Native` = 'American Indian and Alaska Native', 
                                                     `Native Hawaiian and Other Pacific Islander` = 'Native Hawaiian and Other Pacific Islander',
                                                     `Black or African American` = 'Black or African American',
                                                     `Asian` = 'Asian',
                                                     `White` = 'White',
                                                     `Some other race` = 'Some other race'),
                                            Ethnicity = c(`Hispanic or Latino origin (of any race)` = 'Hispanic or Latino origin (of any race)',
                                                          `White alone, not Hispanic or Latino` = 'White alone, not Hispanic or Latino'),
                                            Age = c(`16 to 19 years` = '16 to 19 years',
                                                    `20 to 24 years` = '20 to 24 years',
                                                    `20 to 64 years` = '20 to 64 years',
                                                    `25 to 64 years` = '25 to 64 years',
                                                    `65 to 74 years` = '75 years and over'),
                                            Education = c(`Less than high school graduate` = 'Less than high school graduate', 
                                                          `High school graduate (includes equivalency)` = 'High school graduate (includes equivalency)',
                                                          `Some college or associates degree` = 'Some college or associates degree',
                                                          `Bachelors degree or higher` = 'Bachelors degree or higher'),
                                            Sex = c(`Female` = 'Female',
                                                    `Male` = 'Male'),
                                            Other = c(`With any disability` = 'With any disability',
                                                      `Civilian Workforce (16 years and over)` = 'Civilian Workforce (16 years and over)',
                                                      `Below poverty level` = 'Below poverty level')), selectize = FALSE)),
                                      column(6, selectInput(
                                        inputId = "Input10",
                                        label = "Select a Year",
                                        choices = unique(Dat.Map2$FilterOne),
                                        selected = "2016"))),
                             fixedRow(leafletOutput("mymap4", height = "500")),
                             fixedRow(h6("Data: ACS 5-Year Estiamtes 2012-2016"))),
                    tabPanel(h4("Block to Block Comparison"),
                             fixedRow(leafletOutput("mymap5", height = "500"))),
                    tabPanel(h4("Data & Methodology"),
                             h4("US Census Bureau's Data Definitions"),
                             tags$li("Civilian labor force: Included are all persons in the civilian noninstitutional 
                                     population ages 16 and older classified as either employed or unemployed. (See the definitions below.)"),
                             tags$li("Employed persons: These are all persons who, during the reference week (the week including the 12th day of the month), 
                                     (a) did any work as paid employees, worked in their own business or profession or on their own farm, or worked 15 hours 
                                     or more as unpaid workers in an enterprise operated by a member of their family, or (b) were not working but who had jobs 
                                     from which they were temporarily absent because of vacation, illness, bad weather, childcare problems, maternity or 
                                     paternity leave, labor-management dispute, job training, or other family or personal reasons, whether or not they were 
                                     paid for the time off or were seeking other jobs. Each employed person is counted only once, even if he or she holds more 
                                     than one job."),
                             tags$li("Unemployed persons: Included are all persons who had no employment during the reference week, were available for work, 
                                     except for temporary illness, and had made specific efforts to find employment some time during the 4 week-period ending 
                                     with the reference week. Persons who were waiting to be recalled to a job from which they had been laid off need not have 
                                     been looking for work to be classified as unemployed."),
                             tags$hr(),
                             tags$hr(),
                             h4("Comparing the 2007 - 11 & 2012 - 16 ACS 5- Year Estimates"),
                             h5("When comparing the 2007-2011 ACS 5-year estimates with the 2012-2016 ACS 5-year estimates of employment and income data, 
                                The US Census Bureau advises municipalities to consider the following:"),
                             tags$li("Employment: The Census Bureau introduced an improved sequence of labor force questions in the 2008 ACS questionnaire. 
                                     Accordingly, we recommend using caution when making labor force data comparisons between the 2007-2011 ACS 5-year and 2012-2016 ACS 5-year."),
                             tags$li("Income estimates: Income and earnings estimates in the 2007-2011 ACS 5-year data set are inflation-adjusted to 2011 dollars.
                                     Income and earnings estimates in the 2012-2016 ACS 5-year data set are inflation-adjusted to 2016 dollars. To compare income 
                                     estimates between the two, multiply the 2007-2011 dollar estimates by 1.06686838 (CPI-U-RS) in order to inflation-adjust 2011 
                                     dollars to 2016 dollars. The exception is the Comparative Economic Characteristics Profile (CP03) where all income and earnings 
                                     dollar values are presented in 2016 inflation-adjusted dollars. Note that the Census Bureau inflation-adjusts microdata to arrive 
                                     at the adjusted dollar values in CP03. Data users will not be able to exactly replicate the CP03 2007-2011 inflation-adjusted values."),
                             tags$hr(),
                             tags$hr(),
                             h4("Equations"),
                             tags$li("% Change = (Current Date Stat - Past Date Stat)/Absolute value of Past Date Stat"),
                             tags$li("% Unemployed = 100 * (unemployed/civilian labor force)"),
                             tags$hr(),
                             tags$hr(),
                             h4("LAUS vs ACS Unemployment Data"),
                  h5("The City of Lancaster is committed to communicating an accurate and consistent message on Lancaster's state 
                     of unemployment to the community. There are several major, federally produced reports that estimate unemployment 
                     numbers. Determining the best data source for our community is essential to Lancaster understanding its unemployment 
                     status and working to improve it."),
                  h5("For a city of our size (59,341 individuals per the ACS 2012-2016 5-Year Estimate), the US Census Bureau 
                     recommends two data sources:"),
                  tags$ol(
                    tags$li("Local Area Unemployment Statistics (LAUS)"),
                    tags$li("American Community Survey's (ACS)")),
                  h5("Both are highly reputable data sources and both produce unemployment rates for Lancaster city. However, due to 
                     differences in these way the data is gathered and analyzed, numbers can conflict with eachother."),
                  h5("Overview of the major differences between ACS and LAUS:"), 
                  tags$ol(
                    tags$li("Timeframe: 60 months factored together vs the most current month"),
                    tags$li("Data source: An annual survey vs Lancaster and the State's UI counts divided and then multiplied against the State's unemployment rate."),
                    tags$li("Slight differences in the definition of an 'unemployed person', survey questions and modes of collection")),
                  h5("More details on the intake and analysis process for LAUS vs ACS data:"), 
                  tags$ol(
                    tags$li("LAUS: On a statewide basis, LAUS produces a monthly unemployment rate by combining data from the Current Population (CPS) survey 
                            (i.e. a nationwide survey of the labor force), the Current Employment Statistics (CES) program (i.e.  a nationwide survey of hiring 
                            establishments and their payroll reports), and the State Unemployment Insurance (UI) Systems (i.e. records on individuals who file 
                            claims for unemployment insurance benefits). Per LAUS, Pennsylvania's unemployment rate is estimated at 4.6  as of November 
                            2017. CES and CPS are nationwide surveys that only question a fraction of the population and too few 'Lancastrians' are surveyed to
                            draw any conclusions. Instead, LAUS takes the number of UI claims issued within Lancaster city, finds the ratio of Lancaster UI claims to
                            total number of Pennsylvania UI claims and multiplies that to Pennsylvania's unemployment rate."),
                    tags$li("ACS: Data comes from the largest household survey in the United States (3.5 million addresses per year). Annual data is then built into
                            data from previous years to increase accuracy (i.e. the 3- and 5-Year Estimates). Lancaster city receives 5-Year Estimates down to the
                            census tract level. (Please note that due to the recentness of great recession, the 2016 5-Year Estimates may show a higher unemployment
                            than what is accurate)")),
                  h5("The US Census Bureau recommends that city's of our size use LAUS data for basic, monthly estimates and ACS data for citywide and census tract 
                     socioeconomic analysis. Examples of this analysis below:"),
                  tags$hr(),
                  tags$hr(),
                  h4("Lancaster's Montly Unemployment Rate (LAUS)"),
                  dygraphOutput("LAUSTimeLine", height = "600px"),
                  tags$hr(),
                  tags$hr(),
                  h4("How Socioeconomic Characteristics Relate to the Unemployment Rate (ACS 2012-2016 5-Year Estimate)"),
                  fixedRow(
                    column(6,
                           h5("Unemployment Rate of Civilian Population"),
                           leafletOutput("mymap", height = "750px")),
                    column(6, 
                           h5("Unemployment Rate (Individuals 25 - 64 of Age)"),
                           leafletOutput("mymap2", height = "750px"))),
                  tags$hr(),
                  tags$hr()))))

server <- function(input, output, session) {
  
  output$InclusionPlot1 <- renderPlotly({
    plot_ly(data = Dat.Graph1, x = ~Lable, y = ~Percent_Change2, width = 1050, type = 'scatter',
            hoverinfo = 'text',
            text = ~paste(Dat.Graph1$Geography, 
                          '<br>', Dat.Graph1$Lable, '(2011): ', Dat.Graph1$Stat3,
                          '<br>', Dat.Graph1$Lable, '(2016): ', '<b>',Dat.Graph1$Stat,'</b>',
                          '<br>% Change (2011 - 2016): ', Dat.Graph1$Stat2),
            mode = 'markers', symbol = ~Color, symbols = c('circle','o','circle', 'circle'),
            color = ~Color, colors = InclusionPal1, marker = list(size = 10)) %>%
      layout(xaxis = ax, yaxis = ay)
  })
      
  output$InclusionPMap1 <- renderLeaflet({
    MapPAData <-
      Dat.Graph1[Dat.Graph1$Lable == input$Input1, ]
    MapPAData <-
      MapPAData[MapPAData$Color != "Pennsylvania", ]
    MapPAData <-
      MapPAData[MapPAData$Color != "United States", ]
    
    MapPAData$Lat <- as.numeric(as.character(MapPAData$Lat))
    MapPAData$Lon <- as.numeric(as.character(MapPAData$Lon))
    
    leaflet(MapPAData) %>%
      setView(lng = -77.0000,
              lat = 40.5000,
              zoom = 7) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addCircleMarkers(
        lng = MapPAData$Lon,
        lat = MapPAData$Lat,
        radius = MapPAData$Percent_Change2*2,
        color = MapPAData$NegPos,
        stroke = FALSE, fillOpacity = 0.5,
        label= paste('% Change (2011 - 2016): ', MapPAData$Stat2)) %>%
          addLegend(
        "bottomright",
        colors = c("blue", "red"),
        labels = c("Positive (good) Change", "Negative (troubling) Change"),
        title = paste("% Change in ", unique(MapPAData$Lable)))
  })
  
  output$InclusionPlot2 <- renderPlotly({
    Dat.Graph2.Filtered <-
      Dat.Graph2[Dat.Graph2$FilterTwo == input$Input7, ]
    Dat.Graph2.Filtered <-
      Dat.Graph2.Filtered[Dat.Graph2.Filtered$FilterOne == input$Input8, ]
    Dat.Graph2.Filtered$Color3 <- as.character(Dat.Graph2.Filtered$Color3)
    Dat.Graph2.Filtered$Description <- as.character(Dat.Graph2.Filtered$Description)
    Dat.Graph2.Filtered$YColumn <- " "
    
    plot_ly(data = Dat.Graph2.Filtered, x = Dat.Graph2.Filtered$`Total Population (16+)`, y = ~YColumn, height = 350, type = 'scatter',
            hoverinfo = 'text',
            text = ~paste(Dat.Graph2.Filtered$Description, ': ', Dat.Graph2.Filtered$`Total Population (16+)`),
            mode = 'markers',
            color = Dat.Graph2.Filtered$Description, colors = InclusionPal2 , marker = list(size = 10)) %>%
      layout(title = ~paste('Population Count by ', unique(Dat.Graph2.Filtered$FilterTwo), ' in ', unique(Dat.Graph2.Filtered$FilterOne)),yaxis = ay2, legend = list(orientation = 'h'))
    
  })

   output$InclusionPlot5 <- renderPlotly({
    Dat.Graph5.Filtered <-
      Dat.Graph2[Dat.Graph2$FilterTwo == input$Input2, ]
    Dat.Graph5.Filtered <-
      Dat.Graph5.Filtered[Dat.Graph5.Filtered$FilterOne == input$Input3, ]
    Dat.Graph5.Filtered$Color3 <- as.character(Dat.Graph5.Filtered$Color3)
    Dat.Graph5.Filtered$Description <- as.character(Dat.Graph5.Filtered$Description)
    Dat.Graph5.Filtered$YColumn <- " "
    
    plot_ly(data = Dat.Graph5.Filtered, x = Dat.Graph5.Filtered$`Unemployment Rate`, y = ~YColumn, height = 350, type = 'scatter',
            hoverinfo = 'text',
            text = ~paste(Dat.Graph5.Filtered$Description, ': ', Dat.Graph5.Filtered$`Unemployment Rate`),
            mode = 'markers',
            color = Dat.Graph5.Filtered$Description, colors = InclusionPal2 , marker = list(size = 10)) %>%
      layout(title = ~paste('Unemployment Rate by ', unique(Dat.Graph5.Filtered$FilterTwo), ' in ', unique(Dat.Graph5.Filtered$FilterOne)),yaxis = ay2, legend = list(orientation = 'h'))
  })
  
  output$LAUSTimeLine <- renderDygraph({
    d <- as.ts(Dat.LAUS$LAUS_UnemploymentRate)
    dygraph(d) %>% dyRangeSelector()
  })
  
  output$mymap3 <- renderLeaflet({
     Dat.Map2.Filtered <- Dat.Map2[Dat.Map2$FilterOne == input$Input5, ]
     Dat.Map2.Filtered <- Dat.Map2.Filtered[Dat.Map2.Filtered$Description == input$Input4, ]
     Dat.Map2.Filtered$Description <- as.character(Dat.Map2.Filtered$Description)
     Dat.Map2.Filtered$CT2 <- as.character(Dat.Map2.Filtered$CT2)
     Dat.Map2.Filtered$Unemployment.Rate <- as.numeric(as.character(Dat.Map2.Filtered$Unemployment.Rate))
     Dat.Map2.Filtered$YColumn <- " "
     Dat.Map2.Filtered2 <- merge(Lancaster.Tracts, Dat.Map2.Filtered, by.x = "NAME", by.y = "CT2", all.x = TRUE)
   
     pal3 <- colorBin("YlOrRd", domain = Dat.Map2.Filtered2$Unemployment.Rate, bins = 5)
   
     leaflet(data = Dat.Map2.Filtered2) %>% 
       addProviderTiles(providers$Stamen.Toner) %>%
       addPolygons(fillColor = ~pal3(Unemployment.Rate),
                   weight = 2,
                   opacity = 1,
                   color = "white",
                   dashArray = "3",
                   fillOpacity = 0.6,
                   highlight = highlightOptions(
                     weight = 5,
                     color = "white",
                     dashArray = "",
                     fillOpacity = 0.7,
                     bringToFront = TRUE)) %>% 
       addLegend(pal = pal3, values = ~Unemployment.Rate, opacity = 0.7, title = "Unemployment Rate",
                 position = "bottomright")
    })
  
  output$mymap4 <- renderLeaflet({
    Dat.Map3.Filtered <- Dat.Map2[Dat.Map2$FilterOne == input$Input10, ]
    Dat.Map3.Filtered <- Dat.Map3.Filtered[Dat.Map3.Filtered$Description == input$Input9, ]
    Dat.Map3.Filtered$Description <- as.character(Dat.Map3.Filtered$Description)
    Dat.Map3.Filtered$CT2 <- as.character(Dat.Map3.Filtered$CT2)
    Dat.Map3.Filtered$Total.Count <- as.numeric(as.character(Dat.Map3.Filtered$Total.Count))
    Dat.Map3.Filtered$YColumn <- " "
    Dat.Map3.Filtered2 <- merge(Lancaster.Tracts, Dat.Map3.Filtered, by.x = "NAME", by.y = "CT2", all.x = TRUE)
    
    pal4 <- colorBin("YlOrRd", domain = Dat.Map3.Filtered2$Total.Count, bins = 5)
    
    leaflet(data = Dat.Map3.Filtered2) %>% 
      addProviderTiles(providers$Stamen.Toner) %>%
      addPolygons(fillColor = ~pal4(Total.Count),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.6,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "white",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE)) %>% 
      addLegend(pal = pal4, values = ~Total.Count, opacity = 0.7, title = "Population Count",
                position = "bottomright")
  })
  
  output$mymap5 <- renderLeaflet({
    LancasterBGs$Buckets <- as.numeric(as.character(LancasterBGs$BUCKET))
    factpal <- colorFactor(rainbow(8), LancasterBGs$Buckets)
    leaflet(LancasterBGs) %>%
      addProviderTiles(providers$Stamen.Toner) %>%
      addPolygons(fillColor = ~factpal(Buckets),
                  weight = 1.5,
                  opacity = 1,
                  color = "white",
                  dashArray = "2",
                  fillOpacity = 0.6) %>% 
      addLegend(pal = factpal, values = ~Buckets, opacity = 0.7, title = "Subgroup",
                position = "bottomright")
  })
  
  
  
  output$mymap <- renderLeaflet({
    leaflet(data = Lancaster) %>% 
      addProviderTiles(providers$Stamen.Toner) %>%
      addPolygons(fillColor = ~pal(X16plus_U),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.6,
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
      addLegend(pal = pal, values = ~X16plus_U, opacity = 0.7, title = "Unemployment Rate (Ages: 16+)",
                                                       position = "bottomright")
  })
    
    output$mymap2 <- renderLeaflet({
      leaflet(data = Lancaster) %>% 
        addProviderTiles(providers$Stamen.Toner) %>%
        addPolygons(fillColor = ~pal2(X25to64_U),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.6,
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
        addLegend(pal = pal2, values = ~X25to64_U, opacity = 0.7, title = "Unemployment Rate (Ages: 25 - 64)",
                  position = "bottomright")
    
    
  })
    
}

shinyApp(ui, server)
