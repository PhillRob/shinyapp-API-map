source("helper.R")
function(input, output, session) {
  # Make the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = mean(datain()$longitude), lat = mean(datain()$latitude), zoom = 10)
  })
  ## Get Data
  updatefreqencyinsec = 10000000000
  apidata <- reactivePoll(updatefreqencyinsec, session,
                          checkFunc = function() {max(loadData()$updated_at)},
                          valueFunc = function() {loadData()}
  )
  time.now <- reactivePoll(updatefreqencyinsec, session,
                           checkFunc = function() {max(loadData()$updated_at)},
                           valueFunc = function() {return(format(Sys.time(), '%d %B %Y at %H:%M:%S'))}
  )
  output$timenow <- renderText({
    timenow <- time.now()
    noquote(paste(sprintf("Last updated: %s", timenow)))})
  output$cite <- renderText({
    timenow <- time.now()
    noquote(paste(sprintf("Data compiled for ArRiyadh Development Authority and the City of Riyadh: %s", timenow)))})
  output$s <- renderText({session$token})
  # Table and report
  ## Filter data for table based on selections
  
  datain <- reactive({
    data <- apidata()
    req(input$species)
    req(input$health)
    req(input$project)
    req(input$spread)
    req(input$height)
    if (!(input$species %in% "All"))
    {
      data <- data[data$Species %in% input$species,]
    }
    if (!(input$project %in% "All"))
    {
      data <- data[data$Project %in% input$project,]
    }
    if (!(input$health %in% "All"))
    {
      data <- data[data$Health %in% input$health,]
    }
    data <- data[(data$Spread >= input$spread[1]) & (data$Spread <= input$spread[2]),]
    data <- data[(data$Height >= input$height[1]) & (data$Height <= input$height[2]),]
    return(data)
  })
  
  
  ## create output table for UI
  output$table <- DT::renderDataTable(DT::datatable({
    data <- datain()[,c("Species","Health","Height","Spread","Project")]
    #colnames(data)<-c("No.","KM","Status","Species","Action","Created", "Updated")
    # if (!(input$actionrequired %in% "All")) {
    #   data <- na.omit(data[data$Action %in% input$actionrequired,])
    # }
    # if (!(input$status %in% "All")) {
    #   data <- data[data$Status == input$status,]
    # }
    # if (!(input$species %in% "All")) {
    #   data <- data[data$Species == input$species,]
    # }
    # if (!is.na(input$dateRange[1])){
    #   data <- data[(data$Created >= input$dateRange[1]) & (data$Created <= input$dateRange[2]),]
    # }
    data
  }, rownames = FALSE
  ))
  
  ##report generation tool
  # output$downloadReport <- downloadHandler(
  #   
  #   filename = function() {
  #     paste0(format(Sys.time(), '%Y%M%d'),'-WH-work-order-list-',docounts()$count+1, '.pdf')
  #   },
  #   
  #   content = function(file) {
  #     src <- normalizePath('report.Rmd')
  #     src1 <- normalizePath('WH-logo.png')
  #     src2 <- normalizePath('bpla-logo-220.png')
  #     owd <- setwd(tempdir())
  #     on.exit(setwd(owd))
  #     file.copy(src, 'report.Rmd', overwrite = TRUE)
  #     file.copy(src1,'WH-logo.png')
  #     file.copy(src2,'bpla-logo-220.png')
  #     library(rmarkdown)
  #     out <- render('report.Rmd',output_format = pdf_document())
  #     writetolog(1, session$token)
  #     file.rename(out, file)
  #   }
  # )
  docounts <- reactivePoll(10000000, session,
                           checkFunc = function() {(getcounts()$count)},
                           valueFunc = function() {return(getcounts())}
  )
  output$count <- renderText({
    rv <- docounts()
    noquote(paste(sprintf("Report Downloads: %d Logins: %d",rv$count,rv$sessioncount)))
  })
  
  # map
  # A reactive expression that returns the set of zips that are
  # in bounds right now
  # datainbounds <- reactive({
  #     if (is.null(input$map_bounds))
  #         return(datain()[FALSE,])
  #     bounds <- input$map_bounds
  #     latRng <- range(bounds$north, bounds$south)
  #     lngRng <- range(bounds$east, bounds$west)
  # 
  #     subset(datain(),
  #            latitude >= latRng[1] & latitude <= latRng[2] &
  #                longitude >= lngRng[1] & longitude <= lngRng[2])
  # })
  #plot goez ito observe lacer
  
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    data<-datain()
    var = input$variable
    colorData <- data[[var]]
    if (is.numeric(data[[var]]))
    {
      if (var=="DBH")
      {
        pal <- colorBin("viridis", colorData)#, 7, pretty = FALSE)
        radius <- data[[var]]/10# / max(datain()[[var]],na.rm = T) * 30000
      }else{
        pal <- colorBin("viridis", colorData)#, 7, pretty = FALSE)
        radius <- data[[var]]# / max(datain()[[var]],na.rm = T) * 30000}
      }
    }
    if (!is.numeric(data[[var]]))
    {
      pal <- colorFactor("viridis", colorData)#, 7, pretty = FALSE)
      radius <- data[["Spread"]]# / max(datain()[["Spread"]],na.rm = T) * 30000
    }
    
    leafletProxy("map", data = data) %>%
      clearShapes() %>%
      addCircles(~longitude, ~latitude, radius=radius, layerId=~id,
                 stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
      addLegend("bottomleft", pal=pal, values=colorData, title=var,
                layerId="colorLegend")
    return(unique(pal(colorData)))
  })
  
  output$dist <- renderPlot({
    data<-datain()
    var = input$variable
    
    if (is.numeric(data[[var]]))
    {
      colorData <- data[[var]]
      pal <- colorBin("viridis", colorData)
      hist(x=data[[var]],
           breaks = length(unique(pal(colorData))),
           #range(datain()[[var]],na.rm = T)[2]-range(datain()[[var]],na.rm = T)[1],
           main = "Frequency",
           xlab = var,
           #xlim = range(datain()[[var]],na.rm = T),
           col = unique(pal(colorData)),
           border = 'white')
    }
    if (!is.numeric(data[[var]]))
    {
      colorData <- data[[var]]
      pal <- colorFactor("viridis", colorData)
      plot(
        x = as.factor(data[[var]]),
        #breaks = length(unique(pal(colorData))),
        main = "Frequency",
        xlab = var,
        col = unique(pal(colorData)),
        border = 'white'
      )
    }
    # If no zipcodes are in view, don't plot
    # if (nrow(datainbounds()) == 0)
    #     return(NULL)
  })
  
  # output$maptable <- DT::renderDataTable(DT::datatable({
  #     var = input$variable
  #     maptable<-table(datain()[[var]])
  # }, rownames = FALSE
  # ))
  
  
  # Show a popup at the given location
  showtreePopup <- function(id, lat, lng) {
    selectedtree <- datain()[datain()$id == id,]
    content <- as.character(tagList(
      #tags$h4(selectedtree$form_values.4665),
      tags$h4(selectedtree$Species),
      sprintf("Health: %s", selectedtree$Health), tags$br(),
      sprintf("Project: %s", selectedtree$Project), tags$br(),
      # sprintf("Groundwater: %s", selectedtree$Groundwater), tags$br(),
      sprintf("Height (m): %s", selectedtree$Height,"m"), tags$br(),
      sprintf("Spread (m): %s", selectedtree$Spread,"m"), tags$br()
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = id)
  }
  
  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
      showtreePopup(event$id, event$lat, event$lng)
    })
  })
  
  
}
