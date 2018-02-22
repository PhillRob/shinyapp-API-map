source("helper.R")
displaydata <- loadData()

navbarPage("Riyadh Urban Forest", id="nav",
           # Map display
           tabPanel("Map",
                    div(class="outer",
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ),
                        leafletOutput("map", width="100%", height="100%"),
                        
                        # Shiny versions prior to 0.11 should use class="modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                      width = 330, height = "auto",
                                      h2("Tree Explorer"),
                                      p("Data can be trimmed using the Data Explorer"),
                                      p("Spread and Height are shown as point sizes (not to scale). All other variables are shown as colours with spread (canopy) as the point size."),
                                      selectInput("variable", "Variable", choices = c("Species","Height","Spread","Health","Project"),selected = "Species"),
                                      plotOutput("dist", height = 200),
                                      hr(),
                                      textOutput("cite")#,
                                      #textOutput("s")
                        )#,
                        #tags$div(id="cite", tags$p(textOutput("cite", inline=F)))
                    )
           ),
           #data panel display
           tabPanel("Data Explorer",
                    fluidPage(
                      titlePanel("Tree Data"),
                      # Create a new Row in the UI for selectInputs
                      fluidRow(
                        column(3,
                               selectInput("species",multiple = T,selected = "All",
                                           "Species",selectize = TRUE,
                                           c("All",
                                             unique(as.character(displaydata$Species))))
                               ),
                        column(3,
                               selectInput("project",multiple = T,selected = "All",
                                           "Project",selectize = TRUE,
                                           c("All",
                                             unique(as.character(displaydata$Project))))
                        ),
                        column(3,
                               selectInput("health",multiple = T,selected = "All",
                                           "Health",selectize = TRUE,
                                           c("All",
                                             unique(as.character(displaydata$Health))))
                        ),
                        
                               column(3,
                               sliderInput("height", "height range:",round=T, step=1,
                                           min = min(displaydata$Height,na.rm = T), max = max(displaydata$Height,na.rm = T),
                                           value = c(1,30))
                               ),
                               column(3,
                                     sliderInput("spread", "spread range:",round=T,step=1,
                                                 min = min(displaydata$Spread,na.rm = T), max = max(displaydata$Spread,na.rm = T),
                                                 value = c(1,30))
                               ),
                        column(3,
                               textOutput("timenow"),textOutput("count")
                        )
                      ),
                      # Create a new row for the table.
                      hr(),
                      fluidRow(
                        DT::dataTableOutput("table")
                      )
                    )
           ),
           tabPanel("Info",
                    fluidPage(
                      titlePanel("Vegetation Data"),
                      tags$div(class="header", checked=NA,
                               tags$p("17.2.2018: Setup"),
                               tags$p("Planned: Add Al Hair data"),
                               tags$p("Planned: Add NDVI areas of vegetation"),
                               tags$p("Planned: Add AMANAH projects"),
                               tags$p("Planned: Publish tree data collection form"))
                    )),
                        
           conditionalPanel("false", icon("crosshair"))
)
