

suppressPackageStartupMessages(library(shinydashboard))
suppressPackageStartupMessages(library(shinyWidgets))
suppressPackageStartupMessages(library(shinycssloaders))
suppressPackageStartupMessages(library(shinyBS))
suppressPackageStartupMessages(library(shinydashboardPlus))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(highcharter))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(jsonlite))

# <- readRDS("data/banana.rds")

observe({
  updateSelectInput(session, "site", "Select Site:", choices = c(levels(banana$Location)))
})

crossesBox <- reactive({
  result <- banana %>%
    dplyr::select(Location, Crossnumber,FemaleGenotype, MaleGenotype, CrossType,`First Pollination Date`) %>%
    .[complete.cases(.),]
  
  if(!is.null(input$site)){
    result <- result %>%
      dplyr::filter(Location %in% input$site)
  }
  result %>%
    dplyr::filter(between(`First Pollination Date`, input$dateRange[1], input$dateRange[2]))
})

output$n_crosses <- renderValueBox({
  result <- crossesBox() %>%
    dplyr::group_by(FemaleGenotype, MaleGenotype) %>%
    dplyr::tally()
  
  box1<-valueBox(value=nrow(crossesBox()),
                 color = "teal",
                 href="#",
                 subtitle=HTML("<b>Total Crosses</b><br>", nrow(result)," Unique combinations")
  )
  box1$children[[1]]$attribs$class<-"action-button"
  box1$children[[1]]$attribs$id<-"button_n_crosses"
  return(box1)
})

observeEvent(input$button_n_crosses, {
  toggleModal(session,"mod_crosses","open")
  
  output$list_crosses <- DT::renderDT({
    result <- crossesBox()
    DT::datatable(result, filter = 'top', rownames = FALSE, escape = FALSE, 
                  options = list(pageLength = 10, lengthMenu = c(5, 10, 50, 100, 500,1000),
                                 searchHighlight=T, stateSave = TRUE))
  })
})
# download all or selected rows

downloadCrosses <- reactive({ 
  result <- crossesBox()
  result = result[input[["list_crosses_rows_all"]],]
  if(!is.null(input$list_crosses_rows_selected)){
    result <- result[input$list_crosses_rows_selected,]
  }
  result = result[complete.cases(result$Crossnumber),]
  result = janitor::remove_empty(result, "cols")
  return(result)
})


output$download_crosses <- downloadHandler(
  filename = function(){
    paste0(input$site,"-","Crosses",Sys.time(),".csv")
  },
  content = function(file) {
    readr::write_csv(downloadCrosses(), file)
  }
)

# back crosses
source("tabs/server/back_crosses.R", local = T)

# HARVESTED BUNCHES
bunchesBox <- reactive({
  result <- dplyr::select(banana, Location, Crossnumber, FemaleGenotype, MaleGenotype,
                          CrossType, `First Pollination Date`, `Bunch Harvest Date`) %>%
    .[complete.cases(.),]
  if(!is.null(input$site)){ 
    result <- result %>% 
      dplyr::filter(Location %in% input$site)
  }
  if(input$follow_crosses==TRUE){
    result = result %>%
      dplyr::filter(between(`First Pollination Date`, input$dateRange[1], input$dateRange[2]))
  } else {
    result = result %>%
      dplyr::filter(between(`Bunch Harvest Date`, input$dateRange[1], input$dateRange[2]))
  }
  result
})
output$n_bunches <- renderValueBox({
  result <- bunchesBox() %>%
    dplyr::group_by(FemaleGenotype, MaleGenotype) %>%
    dplyr::tally()
  
  box1<-valueBox(value=nrow(bunchesBox()),
                 width=1,
                 color = "teal",
                 href="#",
                 subtitle=HTML("<b>Bunch Harvested</b><br>", nrow(result), " Unique crosses")
  )
  box1$children[[1]]$attribs$class<-"action-button"
  box1$children[[1]]$attribs$id<-"button_n_bunches"
  return(box1)
})

observeEvent(input$button_n_bunches, {
  toggleModal(session, "modal_bunches", "open")
})
output$list_bunches <- DT::renderDataTable({
  result = bunchesBox()%>%
    dplyr::select(-c(`First Pollination Date`))
  DT::datatable(result, filter = 'top', rownames = FALSE, escape = FALSE, 
                options = list(pageLength = 10, lengthMenu = c(5, 10, 50, 100, 500,1000),
                               searchHighlight=T, stateSave = TRUE))
})

downloadBunches <- reactive({ 
  result <- bunchesBox()%>%
    dplyr::select(-c(`First Pollination Date`))
  result = result[input[["list_bunches_rows_all"]],]
  if(!is.null(input$list_bunches_rows_selected)){
    result <- result[input$list_bunches_rows_selected,]
  }
  result = result[complete.cases(result$Crossnumber),]
  result = janitor::remove_empty(result, "cols")
  return(result)
})


output$download_bunches <- downloadHandler(
  filename = function(){paste0(input$site,"-","Bunch Harvested",Sys.time(),".csv")},
  content = function(file) {
    readr::write_csv(downloadBunches(), file)
  }
)
# SEED EXTRACTION
######################################################################################### 

seedsBox <- reactive({
  result <- banana %>%
    dplyr::select(Location, Crossnumber,FemaleGenotype, MaleGenotype,CrossType,
                  `First Pollination Date`, `Seed Extraction Date`,`Total Seeds`) %>%
    .[complete.cases(.),]
  
  if(is.null(input$site)){ 
    df <- result 
  }else {
    df <- result %>%
      dplyr::filter(Location %in% input$site)
  }
  if(input$follow_crosses==TRUE){
    df <- df %>%
      dplyr::filter(between(`First Pollination Date`, input$dateRange[1], input$dateRange[2]))
  } else {
    df <- df %>%
      dplyr::filter(between(`Seed Extraction Date`, input$dateRange[1], input$dateRange[2]))
  }
  df$`Total Seeds` = as.integer(df$`Total Seeds`)
  df 
})

output$n_totalseeds <- renderValueBox({
  result <- seedsBox() %>%
    dplyr::group_by(FemaleGenotype, MaleGenotype) %>%
    dplyr::summarise(n=sum(na.omit(`Total Seeds`)), .groups = 'drop')
  box1<-valueBox(value=sum(result$n),
                 width=1,
                 color = "teal",
                 href="#",
                 subtitle=HTML("<b>Total Seeds Extracted</b><br>", nrow(result)," Unique crosses")
  )
  box1$children[[1]]$attribs$class<-"action-button"
  box1$children[[1]]$attribs$id<-"button_n_seeds"
  return(box1)
})

observeEvent(input$button_n_seeds, {
  toggleModal(session, "modal_totalseeds", "open")
})
output$list_totalseeds <- DT::renderDataTable({
  result <- seedsBox()%>%
    dplyr::select(-c(`First Pollination Date`))
  DT::datatable(result, filter = 'top', rownames = FALSE, escape = FALSE, 
                options = list(pageLength = 10, lengthMenu = c(5, 10, 50, 100, 500,1000),
                               searchHighlight=T, stateSave = TRUE))
})

downloadTotalSeeds <- reactive({ 
  result <- seedsBox()%>%
    dplyr::select(-c(`First Pollination Date`))
  #result <- dplyr::select(result,"Location", "Crossnumber","Seed Extraction Date","Total Seeds")
  
  result = result[input[["list_totalseeds_rows_all"]],]
  if(!is.null(input$list_totalseeds_rows_selected)){
    result <- result[input$list_totalseeds_rows_selected,]
  }
  result = result[complete.cases(result$Crossnumber),]
  result = janitor::remove_empty(result, "cols")
  return(result)
})


output$download_totalseeds <- downloadHandler(
  filename = function(){paste0(input$site,"-","Seed Extraction",Sys.time(),".csv")},
  content = function(file) {
    readr::write_csv(downloadTotalSeeds(), file)
  }
)

######################################################################################## 
# EMBRYO RESCUE
######################################################################################### 

embryoBox <- reactive({
  result <- banana %>%
    dplyr::select(`Location`, `Crossnumber`, FemaleGenotype,MaleGenotype, CrossType,
                  `First Pollination Date`, `Embryo Rescue Date`,`Number of Embryo Rescued`) %>%
    .[complete.cases(.),]
  if(!is.null(input$site)){ 
    result <- result %>%
      dplyr::filter(Location %in% input$site)
  }
  if(input$follow_crosses==TRUE){
    result = result %>%
      dplyr::filter(between(`First Pollination Date`,input$dateRange[1], input$dateRange[2]))
  } else {
    result = result %>%
      dplyr::filter(between(`Embryo Rescue Date`, input$dateRange[1],input$dateRange[2]))
  }
  result$`Number of Embryo Rescued` = as.integer(result$`Number of Embryo Rescued`)
  result
})

output$n_rescued <- renderValueBox({
  result <- embryoBox() %>%
    dplyr::group_by(FemaleGenotype, MaleGenotype) %>%
    dplyr::summarise(n=sum(na.omit(`Number of Embryo Rescued`)), .groups = 'drop')
  
  box1<-valueBox(value=sum(result$n),
                 width=1,
                 color = "teal",
                 href="#",
                 subtitle=HTML("<b>Embryo rescue</b><br>",nrow(result)," Unique crosses")
  )
  box1$children[[1]]$attribs$class<-"action-button"
  box1$children[[1]]$attribs$id<-"button_n_rescued"
  return(box1)
})

observeEvent(input$button_n_rescued, {
  toggleModal(session, "modal_rescued", "open")
})
output$list_rescued <- DT::renderDataTable({
  result <- embryoBox()%>%
    dplyr::select(-c(`First Pollination Date`))
  DT::datatable(result, filter = 'top', rownames = FALSE, escape = FALSE, 
                options = list(pageLength = 10, lengthMenu = c(5, 10, 50, 100, 500,1000),
                               searchHighlight=T, stateSave = TRUE))
})

downloadRescued <- reactive({ 
  result <- embryoBox()%>%
    dplyr::select(-c(`First Pollination Date`))
  result = result[input[["list_rescued_rows_all"]],]
  
  if(!is.null(input$list_rescued_rows_selected)){
    result <- result[input$list_rescued_rows_selected,]
  }
  
  result = result[complete.cases(result$Crossnumber),]
  result = janitor::remove_empty(result, "cols")
  
  return(result)
})

output$download_rescued <- downloadHandler(
  filename = function(){paste0(input$site,"-","Embryo rescued",Sys.time(),".csv")},
  content = function(file) {
    readr::write_csv(downloadRescued(), file)
  }
)

######################################################################################## 
# GERMINATION
######################################################################################### 

germinationBox <- reactive({
  result <- banana %>%
    dplyr::select(Location, Crossnumber,FemaleGenotype,MaleGenotype,CrossType,
                  `First Pollination Date`,`Embryo Rescue Date`,`Germination Date`,`Number of Embryo Germinating`) %>%
    .[complete.cases(.),]
  if(!is.null(input$site)){ 
    result <- result %>%
      dplyr::filter(Location %in% input$site)
  }
  if(input$follow_crosses==TRUE){
    result = result %>%
      dplyr::filter(between(`First Pollination Date`,input$dateRange[1], input$dateRange[2]))
  } else {
    result = result %>%
      dplyr::filter(between(`Germination Date`,input$dateRange[1],input$dateRange[2]))
  }
  result$`Number of Embryo Germinating` = as.integer(result$`Number of Embryo Germinating`)
  result
})

output$n_germination <- renderValueBox({
  result <- germinationBox() %>%
    dplyr::group_by(FemaleGenotype, MaleGenotype) %>%
    dplyr::summarise(n=sum(na.omit(`Number of Embryo Germinating`)), .groups = 'drop')
  
  box1<-valueBox(value=sum(result$n),
                 width=1,
                 color = "teal",
                 href="#",
                 subtitle=HTML("<b>Embryo Germinating</b><br>",nrow(result)," Unique crosses" )
  )
  box1$children[[1]]$attribs$class<-"action-button"
  box1$children[[1]]$attribs$id<-"button_n_germination"
  return(box1)
})

observeEvent(input$button_n_germination, {
  toggleModal(session, "modal_germination", "open")
})
output$list_germination <- DT::renderDataTable({
  result <- germinationBox() %>%
    dplyr::select(-c(`First Pollination Date`,`Embryo Rescue Date`))
  DT::datatable(result, filter = 'top', rownames = FALSE, escape = FALSE, 
                options = list(pageLength = 10, lengthMenu = c(5, 10, 50, 100, 500,1000),
                               searchHighlight=T, stateSave = TRUE))
})
downloadGermination <- reactive({ 
  result <- germinationBox()%>%
    dplyr::select(-c(`First Pollination Date`,`Embryo Rescue Date`))
  
  result = result[input[["list_germination_rows_all"]],]
  if(!is.null(input$list_germination_rows_selected)){
    result <- result[input$list_germination_rows_selected,]
  }
  result = result[complete.cases(result$Crossnumber),]
  result = janitor::remove_empty(result, "cols")
  return(result)
})
output$download_germination <- downloadHandler(
  filename = function(){paste0(input$site,"-","Germination",Sys.time(),".csv")},
  content = function(file) {
    readr::write_csv(germinationBox(), file)
  }
)

######################################################################################## 
# OPEN FIELD
######################################################################################### 

openfieldBox <- reactive({
  result <- readRDS("data/openfield.rds") %>%
    dplyr::left_join(readRDS("data/plantlets.rds") %>% dplyr::select(Crossnumber, PlantletID)) %>%
    dplyr::left_join(
      banana %>% dplyr::select(Crossnumber, FemaleGenotype, MaleGenotype, CrossType, 'First Pollination Date')
    ) %>%
    dplyr::select(Location, Crossnumber, PlantletID, FemaleGenotype, MaleGenotype, 'First Pollination Date',
                  Openfield_Transfer_Date, Number) %>%
    dplyr::filter(!is.na(Openfield_Transfer_Date)) %>%
    .[complete.cases(.),]
  if(!is.null(input$site)){ 
    result <- result %>%
      dplyr::filter(Location %in% input$site)
  }
  if(input$follow_crosses==TRUE){
    result <- result %>%
      dplyr::filter(between(`First Pollination Date`, input$dateRange[1], input$dateRange[2]))
  } else {
    result <- result %>%
      dplyr::filter(between(Openfield_Transfer_Date , input$dateRange[1],input$dateRange[2]))
  }
  result$Number <- as.integer(result$Number)
  result
})
output$n_openfield_plantlets <- renderValueBox({
  result <- openfieldBox() %>%
    dplyr::group_by(FemaleGenotype, MaleGenotype) %>%
    dplyr::summarise(n=sum(as.integer(na.omit(Number))), .groups = 'drop') %>%
    .[complete.cases(.),]
  
  box1<-valueBox(value=sum(result$n),
                 width=1,
                 color = "teal",
                 href="#",
                 subtitle=HTML("<b>Plants in openfield</b><br>",
                               nrow(result), " Unique plantlets")
  )
  box1$children[[1]]$attribs$class<-"action-button"
  box1$children[[1]]$attribs$id<-"button_n_openfield"
  return(box1)
})

observeEvent(input$button_n_openfield, {
  toggleModal(session, "modal_openfield", "open")
})
output$list_openfield <- DT::renderDataTable({
  result <- openfieldBox() %>%
    dplyr::select(-c(Crossnumber,"First Pollination Date"))
  
  DT::datatable(result, filter = 'top', rownames = FALSE, escape = FALSE, 
                options = list(pageLength = 10, lengthMenu = c(5, 10, 50, 100, 500,1000),
                               searchHighlight=T, stateSave = TRUE))
})

downloadOpenfield <- reactive({ 
  result <- openfieldBox() %>%
    dplyr::select(-c(Crossnumber,`First Pollination Date`))
  if(!is.null(input$site)){ 
    result <- dplyr::filter(plantlets, Location %in% input$site, 
                            between(Openfield_Transfer_Date, input$dateRange[1], input$dateRange[2]))
  }
  
  result = result[input[["list_openfield_rows_all"]],]
  
  if(!is.null(input$list_openfield_rows_selected)){
    result <- result[input$list_openfield_rows_selected,]
  } 
  result = result[complete.cases(result$PlantletID),]
  result = janitor::remove_empty(result, "cols")
  
  return(result)
})

output$download_openfield <- downloadHandler(
  filename = function(){paste0(input$site,"-","Plantlets_in_openfield",Sys.time(),".csv")},
  content = function(file) {
    readr::write_csv(downloadOpenfield(), file)
  }
)


######################################################################################## 
# GRAPHS
#########################################################################################

js_bar_clicked <- JS("function(event) {Shiny.onInputChange('bar_clicked', [event.point.category]);}")

output$totals_site <- renderHighchart({
  result <- banana %>%
    dplyr::select("Location","Crossnumber","FemaleGenotype", "MaleGenotype","First Pollination Date") %>%
    dplyr::mutate(
      Yearly = lubridate::year(`First Pollination Date`),
      Monthly = lubridate::month(`First Pollination Date`),
      Daily = lubridate::day(`First Pollination Date`)
    ) %>%
    dplyr::filter(between(`First Pollination Date`, input$dateRange[1], input$dateRange[2]))
  
  if(!is.null(input$site)){
    result <- result %>%
      dplyr::filter(Location %in% input$site)
  }
  if(nrow(result)==0)return(NULL)
  
  if((input$dateRange[2] - input$dateRange[1]) < 31){
    result <- result %>% #[,.N,by=.(Location, Daily)][order(Daily)]
      dplyr::group_by(Location, Daily) %>%
      dplyr::tally()
    result$Time <- result$Daily
    group_name <- "Daily"
  } else if((lubridate::year(input$dateRange[2]) == lubridate::year(input$dateRange[1]))){
    result <- result %>% #[,.N,by=.(Location, Daily)][order(Daily)]
      dplyr::group_by(Location, Monthly) %>%
      dplyr::tally()
    
    #result = result[,.N, by = .(Location, Monthly)][order(Monthly)]
    result$Time = month.abb[result$Monthly]
    #result$Time = month.abb[result$month]
    group_name = "Monthly"
  }  else if((lubridate::year(input$dateRange[2]) != lubridate::year(input$dateRange[1]))){ 
    result <- result %>% #[,.N,by=.(Location, Daily)][order(Daily)]
      dplyr::group_by(Location, Yearly) %>%
      dplyr::tally()
    
    #result = result[,.N, by=.(Location, Yearly)][order(Yearly)]
    result$Time = result$Yearly
    group_name = "Yearly"
  }
  
  colnames(result)[3]="Number of crosses"
  
  hc = highcharter::hchart(result, "column", hcaes(x = `Time`, y = `Number of crosses`, group = Location), events = list(click = js_bar_clicked))
  
  hc %>%
    hc_exporting(enabled = TRUE) %>% 
    hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5", valueDecimals=0,
               shared = TRUE, borderWidth = 2) %>%
    hc_title(text="Total number of crosses") %>%
    hc_add_theme(hc_theme_elementary())
})

output$totalcrosses <- renderUI({
  result <- banana %>%
    dplyr::select(Location,Crossnumber,FemaleGenotype, MaleGenotype,`First Pollination Date`) %>% 
    .[complete.cases(.),]%>% 
    dplyr::filter(between(`First Pollination Date`, input$dateRange[1], input$dateRange[2]))
  
  if(!is.null(input$site)){
    result <- result %>%
      dplyr::filter(Location %in% input$site)
  }
  
  result <- result %>% 
    dplyr::summarize(crosses = n())
  paste0("N = ", result)
  
})


# parents
parentsInput <- reactive({
  result <- banana %>%
    dplyr::select(Location,Crossnumber,FemaleGenotype, MaleGenotype,`First Pollination Date`) %>%
    dplyr::filter(`First Pollination Date` >= input$dateRange[1] & `First Pollination Date` <=  input$dateRange[2])
  
  if(!is.null(input$site)){
    result = result %>%
      dplyr::filter(Location %in% input$site)
  }
  result
})

js_female_bar_clicked <- JS("function(event) {Shiny.onInputChange('female_bar_clicked', [event.point.category]);}")

output$mother <- renderHighchart({
  
  if(is.null(parentsInput())){return(NULL)}
  else {
    result <- parentsInput() %>%
      dplyr::group_by(FemaleGenotype) %>%
      dplyr::tally() %>%
      dplyr::arrange(desc(n)) %>%
      dplyr::collect() %>%
      head(20)
     
    
    # paste0("<a href='",result$FemaleGermplasmDbId,"'>",'photo',"</a>")
    highchart() %>%
      hc_add_series(data = result$n,type = "bar", name = paste("Female plants"), events = list(click = js_female_bar_clicked)) %>%
      hc_xAxis(categories = result$FemaleGenotype) %>%
      hc_exporting(enabled = TRUE) %>% 
      hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5", valueDecimals=0,
                 shared = TRUE, borderWidth = 2) %>%
      hc_title(text="Female genotypes (top 20)") %>%
      hc_add_theme(hc_theme_elementary())
  }
})


output$totalFemales <- renderUI({
  if(is.null(parentsInput())){return(NULL)}
  else {
    result <- parentsInput() %>% 
      dplyr::summarize(dplyr::n_distinct(FemaleGenotype))
    paste0("N = ", result)
  }
})


# on female click, go to 'Data' tab
observeEvent(input$female_bar_clicked, {
  updateTabsetPanel(session, "inTabset",
                    selected = "Data")
})


#.....................................................................................MALES 

js_male_bar_clicked <- JS("function(event) {Shiny.onInputChange('male_bar_clicked', [event.point.category]);}")

output$father <- renderHighchart({
  
  if(is.null(parentsInput())){return(NULL)}
  else {
    result <- parentsInput()%>%
      group_by(MaleGenotype) %>%
      dplyr::tally() %>%
      arrange(desc(n)) %>%
      dplyr::collect()  %>%
      head(20)
    
    highchart() %>%
      hc_add_series(data = result$n, type = "bar",name = paste("Male plants"),  events = list(click = js_male_bar_clicked)) %>%
      hc_xAxis(categories = result$MaleGenotype,tickmarkPlacement="on") %>%
      hc_exporting(enabled = TRUE) %>% 
      hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                 shared = TRUE, borderWidth = 2) %>%
      hc_title(text="Male genotypes (top 20)") %>%
      hc_add_theme(hc_theme_elementary())
  }
})


output$totalMales <- renderUI({
  if(is.null(parentsInput())){return(NULL)}
  else {
    result <- parentsInput() %>% 
      dplyr::summarize(dplyr::n_distinct(MaleGenotype))
    paste0("N = ", result)
  }
})

# on male click, go to 'Data' tab
observeEvent(input$male_bar_clicked, {
  updateTabsetPanel(session, "inTabset",
                    selected = "Data")
})
  
  
# })
# )
