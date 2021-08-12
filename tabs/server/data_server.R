

suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(shinydashboardPlus))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(DT))

# render <- c(
#   "function(data, type, row){",
#   "  if(type === 'display'){",
#   "    var a = '<a href=\"' + row[7] + '\">' + data + '</a>';",
#   "    return a;",
#   "  } else {",
#   "    return data;",
#   "  }",
#   "}"
# )

css <- "
#large .selectize-input { line-height: 40px; }
#large .selectize-dropdown { line-height: 30px; }"

# sidepanel
observeEvent(input$datatabs,{
  if(input$datatabs == "Data Table"){
    shinyjs::show("dataset")
    shinyjs::hide("sum_dateRange")
    shinyjs::hide("groupby")
    shinyjs::hide("gen_activity")
    shinyjs::hide("gen_dateRange")
    shinyjs::hide("gen_groupby")
  }else if (input$datatabs == "Summary Table"){
    shinyjs::hide("dataset")
    shinyjs::show("sum_dateRange")
    shinyjs::show("groupby")
    shinyjs::hide("gen_activity")
    shinyjs::hide("gen_dateRange")
    shinyjs::hide("gen_groupby")
  } else {
    shinyjs::hide("dataset")
    shinyjs::hide("sum_dateRange")
    shinyjs::hide("groupby")
    shinyjs::show("gen_activity")
    shinyjs::show("gen_dateRange")
    shinyjs::show("gen_groupby")
  }
  
})

# -----------------------------------DATA TABLE --------------------------------------------------------------------------

observe({
  dt <- banana
  updateSelectInput(session, "dt_site",label = "Site:",choices = c(unique(as.character(dt$Location))))
})


datasetInput <- reactive({
  req(input$dataset)
  
  switch(input$dataset, 
         "Flowering" = readRDS("data/flowering.rds"),
         "Crosses" = banana,# %>% select(-c(`Days in ripening shed`)),
         "Plantlets" = readRDS("data/plantlets.rds"),
         "Status" = readRDS("data/status.rds"),
         "Contamination" = readRDS("data/contamination.rds"),
         "Subcultures" = readRDS("data/subcultures.rds") %>%
           dplyr::rename("Number of copies" = "Number"),
         "Rooting" = readRDS("data/rooting.rds") %>%
           dplyr::rename("Number in rooting" = "Number"),
         "Weaning 1" = readRDS("data/weaning1.rds") %>%
           dplyr::rename("Number in weaning 1" = "Number"),
         "Weaning 2" = readRDS("data/weaning2.rds") %>%
           dplyr::rename("Number in weaning 2" = "Number"),
         "Screenhouse" = readRDS("data/screenhouse.rds") %>%
           dplyr::rename("Number in screenhouse" = "Number"),
         "Hardening" = readRDS("data/hardening.rds") %>%
           dplyr::rename("Number in hardening" = "Number"),
         "Open-field" = readRDS("data/openfield.rds") %>%
           dplyr::rename("Number in openfield" = "Number")
         )
})

# Data Table TAB
##################################################
viewInput <- reactive({
  result = data.frame(datasetInput())
  columns = colnames(result)
  columns = input$showVars
  if(input$dataset=='Crosses'){
    if(!is.null(input$female_bar_clicked)){
      result <- result %>% 
        dplyr::filter(FemaleGenotype == input$female_bar_clicked[1])
    } 
    if(!is.null(input$male_bar_clicked)){
      result = result %>% 
        dplyr::filter(MaleGenotype == input$male_bar_clicked[1])
    }
  }
  
  result <-  result %>%
    #janitor::remove_empty("cols") %>%
    dplyr::filter(!is.na(Location))
  colnames(result) = gsub("[.]"," ", names(result))
  colnames(result) = gsub("_"," ", names(result))
  return(result)
})

output$viewdt <- renderDT({
  DT::datatable(viewInput(), 
                filter = 'top', 
                rownames = FALSE, 
                escape = FALSE, 
                options = list(pageLength = 10,
                               lengthMenu = c(10, 50, 100, 500,1000),
                               searchHighlight=T, 
                               stateSave = TRUE)
  )
})

downloadView <- reactive({
  result <- data.frame(datasetInput())
  columns <- colnames(result)
  columns <- input$showVars
  if(input$dataset=='Crosses'){
    if(!is.null(input$female_bar_clicked)){
      result <- result %>%
        dplyr::filter(FemaleGenotype %in% 
                        input$female_bar_clicked[1])
    } 
    if(!is.null(input$male_bar_clicked)){
      result <- result %>% 
        dplyr::filter(MaleGenotype %in% 
                        input$male_bar_clicked[1])
    }
    result <- result %>%
      dplyr::select(-ends_with('Link'))
  }
  result <- result[input[["viewdt_rows_all"]],]
  
  if(!is.null(input$viewdt_rows_selected)){
    result <- result[input$viewdt_rows_selected,]
  }
  
  colnames(result) <- gsub("[.]"," ", names(result))
  result <- result %>%
    janitor::remove_empty("cols")
  result
})
output$downloadTbl <- downloadHandler(
  filename = function(){paste(input$dataset,'-', Sys.time(), '.csv')},
  content = function(file) {
    readr::write_csv(downloadView(), file) #datasetInput
  }
)

# -----------------------------------SUMMARY TABLE------------------------------------------------------------------------
observeEvent(input$datatabs,{
  if(input$datatabs == 'Summary Table'){
    fpdate <- banana$`First Pollination Date`
    updateDateRangeInput(session, "sum_dateRange", "First Pollination Date",
                         start = min(fpdate), end = max(fpdate) , min = min(fpdate) , max = max(fpdate) )
  } 
})

summaryIn <- reactive({
  result <- banana %>%
    dplyr::select(-contains("Plot"), -Cycle) %>%
    dplyr::left_join(
      readRDS("data/plantlets.rds") %>%
        dplyr::group_by(Crossnumber) %>%
        dplyr::summarise(
          Copies = sum(Copies, na.rm=T),
          Number_Rooting = sum(Number_Rooting, na.rm=T),
          Number_Sent_Out = sum(Number_Sent_Out, na.rm=T),
          Weaning_2_Plantlets = sum(Weaning_2_Plantlets, na.rm=T),
          Number_in_Screenhouse = sum(Number_in_Screenhouse, na.rm=T),
          Number_in_hardening = sum(Number_in_hardening, na.rm=T),
          Number_in_Openfield = sum(Number_in_Openfield, na.rm=T)
        )
    )
  colnames(result) <- gsub("_"," ", names(result))
  
  result <- result %>%
    dplyr::filter(FemaleGenotype !='' & MaleGenotype !='', Location !='',
                  between(`First Pollination Date`, input$sum_dateRange[1], input$sum_dateRange[2])
    ) 
  
  result$Bunches = ifelse(!is.na(result$`Bunch Harvest Date`), 1,0)
  result[,c( "Bunch Harvest Date","Seed Extraction Date", "Embryo Rescue Date","Germination Date")] = NULL
  result$`Year_of_Pollination` = as.integer(lubridate::year(result$`First Pollination Date`))
  result$Month_of_Pollination <- lubridate::month(result$`First Pollination Date`)
  
  result <- data.frame(month = 1:12, x = LETTERS[1:12], Month_of_Pollination = 1:12) %>% 
    mutate(month = factor(month.name[month], levels = month.name)) %>% 
    arrange(month) %>%
    dplyr::left_join(result)%>%
    dplyr::select(-c(x, Month_of_Pollination)) %>%
    dplyr::rename(
      Month_of_Pollination = month
    )  %>%
    dplyr::select(
      Location, Crossnumber, TrialName, FemaleGenotype, FemalePloidy, MaleGenotype, MalePloidy, CrossType, `First Pollination Date`, Year_of_Pollination, Month_of_Pollination, 
      `Number of Repeats`, Bunches, `Total Seeds`,`Good Seeds`, `Number of Embryo Rescued`, `Number of Embryo Germinating`, Copies,              
      `Number Rooting`,`Number Sent Out`, `Weaning 2 Plantlets`, `Number in Screenhouse`, `Number in hardening`, `Number in Openfield`
    )
  result
  
})

summaryData <- reactive({
  
  result <- summaryIn() 
  
  if(length(input$groupby)>0){
    result %>%
      dplyr::group_by(.dots = input$groupby)%>% 
      dplyr::summarise(`Number of Crosses`=n(),
                       Bunches = sum(Bunches), 
                       `Total Seeds`=sum(na.omit(as.integer(`Total Seeds`))),
                       `Good Seeds` = sum(na.omit(as.integer(`Good Seeds`))),
                       `Number of Embryo Rescued`= sum(na.omit(as.integer(`Number of Embryo Rescued`))),
                       `Number of Embryo Germinating`= sum(na.omit(as.integer(`Number of Embryo Germinating`))),
                       `Subcultures`= sum(na.omit(as.integer(`Copies`))),
                       `Number Rooting`= sum(na.omit(as.integer(`Number Rooting`))),
                       `Number Sent Out`= sum(na.omit(as.integer(`Number Sent Out`))),
                       `Weaning 2 Plantlets`= sum(na.omit(as.integer(`Weaning 2 Plantlets`))),
                       `Number in Screenhouse`= sum(na.omit(as.integer(`Number in Screenhouse`))),
                       `Number in hardening`= sum(na.omit(as.integer(`Number in hardening`))),
                       `Number in Openfield`= sum(na.omit(as.integer(`Number in Openfield`)))
                       , .groups = 'drop') %>%
      ungroup()
  }else {
    result
  }
  
})

# summary table
output$summaryDT <- renderDT({
  
  DT::datatable(summaryData(), 
                filter = 'top', 
                rownames = FALSE, 
                escape = FALSE, 
                options = list(pageLength = 10,
                               lengthMenu = c(10, 50, 100, 500,1000),
                               searchHighlight=T, 
                               stateSave = TRUE
                               )
  )
})


# summary details

summaryDrill <- reactive({    
  loc <- summaryData()[as.integer(input$summaryDT_rows_selected), ]$Location
  female <- summaryData()[as.integer(input$summaryDT_rows_selected), ]$FemaleGenotype
  male <- summaryData()[as.integer(input$summaryDT_rows_selected), ]$MaleGenotype
  crosstype <- summaryData()[as.integer(input$summaryDT_rows_selected), ]$CrossType
  fploidy <- summaryData()[as.integer(input$summaryDT_rows_selected), ]$FemalePloidy
  mploidy <- summaryData()[as.integer(input$summaryDT_rows_selected), ]$MalePloidy
  
  result <- summaryIn() %>%
    dplyr::select(-c(Year_of_Pollination, Month_of_Pollination))
  
  if(!is.null(loc)){
    result <- result %>%
      dplyr::filter(Location %in% loc)
  }
  if(!is.null(female)){
    result <- result %>%
      dplyr::filter(FemaleGenotype %in% female)
  }
  if(!is.null(male)){
    result <- result %>%
      dplyr::filter(MaleGenotype %in% male)
  }
  
  if(!is.null(crosstype)){
    result <- result %>%
      dplyr::filter(CrossType %in% crosstype)
  }
  if(!is.null(fploidy)){
    result <- result %>%
      dplyr::filter(FemalePloidy %in% fploidy)
  }
  if(!is.null(mploidy)){
    result <- result %>%
      dplyr::filter(MalePloidy %in% mploidy)
  }
  result
})


observeEvent(input$summaryDT_rows_selected,{
  shinyjs::show("sDetails_id")
})


output$sum_details <- renderDT({
  
  DT::datatable(summaryDrill(), 
                filter = 'top', 
                rownames = FALSE, 
                escape = FALSE, 
                options = list(pageLength = 10,
                               lengthMenu = c(10, 50, 100, 500,1000),
                               searchHighlight=T, 
                               stateSave = TRUE)
  )
  
})

summaryDownload <- reactive({
  result <- summaryData()
  
  if(!is.null(input$summaryDT_rows_selected)){
    result <- result[input$summaryDT_rows_selected,]
  }
  result <- janitor::remove_empty(result, "cols")
  result[complete.cases(result[,1]),]
})

output$downloadSummary <- downloadHandler(
  filename <- function(){paste0('SummaryTable-',input$summaryDateRange[1], '-',input$summaryDateRange[2],'.csv')},
  content <- function(file) {
    readr::write_csv(summaryDownload(), file) #datasetInput
  }
)

summaryDetailsDownload <- reactive({
  result <- summaryDrill()
  
  if(!is.null(input$sum_details_rows_selected)){
    result <- result[input$sum_details_rows_selected,]
  }
  result <- janitor::remove_empty(result, "cols")
  result[complete.cases(result[,1]),]
})

output$save_sum_details <- downloadHandler(
  filename <- function(){paste0('SummaryDetails-',input$summaryDateRange[1], '-',input$summaryDateRange[2],'.csv')},
  content <- function(file) {
    readr::write_csv(summaryDetailsDownload(), file) #datasetInput
  }
)


# --------------------------ACTIVITY SPECIFIC SUMMARIES-------------------------------------------------------------------  

observeEvent(input$datatabs,{
  if(input$datatabs == "Generate Specific Summaries"){
    updateSelectInput(session, "gen_activity","Select activity", 
                      choices = unique(cleantable$Activity),
                      selected = "First pollination")
  }
  
})


activityInput <- reactive({
  req(input$dataset)
  banana <- banana
  
  switch(input$gen_activity, 
         "First pollination" = banana %>% 
           dplyr::select(Location, Crossnumber, FemaleGenotype, MaleGenotype, CrossType, `First Pollination Date`),
         
         "Bunches" = banana %>% 
           dplyr::select(Location, Crossnumber, FemaleGenotype, MaleGenotype, CrossType, `Bunch Harvest Date`) %>%
           dplyr::filter(!is.na(Crossnumber), !is.na(`Bunch Harvest Date`)),
         
         "Seed extraction" = banana %>% 
           dplyr::select(Location, Crossnumber, FemaleGenotype, MaleGenotype, CrossType, `Seed Extraction Date`, `Total Seeds`, `Good Seeds`) %>%
           dplyr::filter(!is.na(Crossnumber), !is.na(`Seed Extraction Date`)),
         
         "Embryo rescue" = banana %>% 
           dplyr::select(Location, Crossnumber, FemaleGenotype, MaleGenotype, CrossType, `Embryo Rescue Date`, `Number of Embryo Rescued`) %>%
           dplyr::filter(!is.na(Crossnumber), !is.na(`Embryo Rescue Date`)),
         
         "Embryo germination" = banana %>% 
           dplyr::select(Location, Crossnumber, FemaleGenotype, MaleGenotype, CrossType, `Germination Date`, `Number of Embryo Germinating`) %>%
           dplyr::filter(!is.na(Crossnumber), !is.na(`Germination Date`)),
         
         "Subcultures" = readRDS("data/subcultures.rds")%>%
           mutate(Crossnumber = paste0(stringr::str_split_fixed(PlantletID, "_",3)[,1],"_", stringr::str_split_fixed(PlantletID, "_",3)[,2])) %>%
           left_join(
             banana %>% 
               dplyr::select(Crossnumber, FemaleGenotype, MaleGenotype, CrossType)
           ) %>%
           dplyr::select(-Crossnumber, Location, PlantletID, FemaleGenotype, MaleGenotype, CrossType, everything()),
         
         "Rooting" = readRDS("data/rooting.rds") %>%
           dplyr::mutate(Crossnumber = paste0(stringr::str_split_fixed(PlantletID, "_",3)[,1],"_", 
                                              stringr::str_split_fixed(PlantletID, "_",3)[,2])) %>%
           dplyr::left_join(banana %>%
                              dplyr::select(Crossnumber, FemaleGenotype, MaleGenotype, CrossType)) %>% 
           dplyr::filter(!is.na(Date_of_Rooting)),
         
         "Weaning 1"  = readRDS("data/weaning1.rds")%>%
           dplyr::mutate(Crossnumber = paste0(stringr::str_split_fixed(PlantletID, "_",3)[,1],"_", 
                                              stringr::str_split_fixed(PlantletID, "_",3)[,2])) %>%
           dplyr::left_join(banana %>%
                              dplyr::select(Crossnumber, FemaleGenotype, MaleGenotype, CrossType)) %>% 
           dplyr::filter(!is.na(Sending_Out_Date)), 
         
         "Weaning 2" = readRDS("data/weaning2.rds")%>%
           dplyr::mutate(Crossnumber = paste0(stringr::str_split_fixed(PlantletID, "_",3)[,1],"_", 
                                              stringr::str_split_fixed(PlantletID, "_",3)[,2])) %>%
           dplyr::left_join(banana %>%
                              dplyr::select(Crossnumber, FemaleGenotype, MaleGenotype, CrossType)) %>% 
           dplyr::filter(!is.na(Weaning_2_Date)), 
         
         "Screenhouse" = readRDS("data/screenhouse.rds")%>%
           dplyr::mutate(Crossnumber = paste0(stringr::str_split_fixed(PlantletID, "_",3)[,1],"_", 
                                              stringr::str_split_fixed(PlantletID, "_",3)[,2])) %>%
           dplyr::left_join(banana %>%
                              dplyr::select(Crossnumber, FemaleGenotype, MaleGenotype, CrossType)) %>% 
           dplyr::filter(!is.na(Screenhouse_Transfer_Date)), 
         
         "Hardening" = readRDS("data/hardening.rds") %>%
           dplyr::mutate(Crossnumber = paste0(stringr::str_split_fixed(PlantletID, "_",3)[,1],"_", 
                                              stringr::str_split_fixed(PlantletID, "_",3)[,2])) %>%
           dplyr::left_join(banana %>%
                              dplyr::select(Crossnumber, FemaleGenotype, MaleGenotype, CrossType)) %>% 
           dplyr::filter(!is.na(Hardening_Date)),
         
         "Open-field" = readRDS("data/openfield.rds")%>%
           dplyr::mutate(Crossnumber = paste0(stringr::str_split_fixed(PlantletID, "_",3)[,1],"_", 
                                              stringr::str_split_fixed(PlantletID, "_",3)[,2])) %>%
           dplyr::left_join(banana %>%
                              dplyr::select(Crossnumber, FemaleGenotype, MaleGenotype, CrossType)) %>% 
           dplyr::filter(!is.na(Openfield_Transfer_Date))
  )
})

observeEvent(input$gen_activity,{
  result <- activityInput()%>%
    dplyr::select(contains("Date"))
  
  updateDateRangeInput(session, "gen_dateRange", "Date Range",
                       start=min(result[,1]) , end=max(result[,1]), 
                       min=min(result[,1]), max=max(result[,1]))
})

activityData <- reactive({
  result <- activityInput()
  
  if(input$gen_activity == "First pollination"){
    result <- result  %>%
      dplyr::filter(between(`First Pollination Date`, input$gen_dateRange[1], input$gen_dateRange[2]))
    
  }else if(input$gen_activity == "Bunches"){
    result <- result  %>%
      dplyr::filter(between(`Bunch Harvest Date`, input$gen_dateRange[1], input$gen_dateRange[2]))
    
  }else if(input$gen_activity == "Seed extraction"){
    result <- result  %>%
      dplyr::filter(between(`Seed Extraction Date`, input$gen_dateRange[1], input$gen_dateRange[2]))
    
  }else if(input$gen_activity == "Embryo rescue"){
    result <- result  %>%
      dplyr::filter(between(`Embryo Rescue Date`, input$gen_dateRange[1], input$gen_dateRange[2]))
    
  }else if(input$gen_activity == "Embryo germination"){
    result <- result  %>%
      dplyr::filter(between(`Germination Date`, input$gen_dateRange[1], input$gen_dateRange[2]))
    
  }else if(input$gen_activity == "Subcultures"){
    result <- result  %>%
      dplyr::filter(between(Subculture_Date, input$gen_dateRange[1], input$gen_dateRange[2])) %>%
      dplyr::rename(Copies = Number)
    
  }else if(input$gen_activity == "Rooting"){
    result <- result  %>%
      dplyr::filter(between(Date_of_Rooting, input$gen_dateRange[1], input$gen_dateRange[2]))%>%
      dplyr::rename("Number in Rooting" = Number)
    
  }else if(input$gen_activity == "Weaning 1"){
    result <- result  %>%
      dplyr::filter(between(Sending_Out_Date, input$gen_dateRange[1], input$gen_dateRange[2]))%>%
      dplyr::rename("Number Sent Out" = Number)
    
  }else if(input$gen_activity == "Weaning 2"){
    result <- result  %>%
      dplyr::filter(between(Weaning_2_Date, input$gen_dateRange[1], input$gen_dateRange[2]))%>%
      dplyr::rename("Number in Weaning 2" = Number)
    
  }else if(input$gen_activity == "Screenhouse"){
    result <- result  %>%
      dplyr::filter(between(Screenhouse_Transfer_Date, input$gen_dateRange[1], input$gen_dateRange[2]))%>%
      dplyr::rename("Number in Screenhouse" = Number) 
    
  }else  if(input$gen_activity == "Hardening"){
    result <- result  %>%
      dplyr::filter(between(Hardening_Date, input$gen_dateRange[1], input$gen_dateRange[2]))%>%
      dplyr::rename("Number in Hardening" = Number) 
    
  }else if(input$gen_activity == "Open-field"){
    result <- result  %>%
      dplyr::filter(between(Openfield_Transfer_Date, input$gen_dateRange[1], input$gen_dateRange[2]))%>%
      dplyr::rename("Number in Openfield" = Number)
  }
  result <- result %>%
    dplyr::select(Location, Crossnumber, FemaleGenotype, MaleGenotype, CrossType, everything()) %>%
    .[complete.cases(.),]
  colnames(result) <- gsub("_"," ", names(result))
  result
})

actvAggr <- reactive({
  result <- activityData() %>%
    dplyr::group_by(.dots = input$gen_groupby)
  
  if(input$gen_activity == "First pollination"){
    result <- result  %>%
      dplyr::filter(between(`First Pollination Date`, input$gen_dateRange[1], input$gen_dateRange[2])) %>%
      dplyr::summarise("Number of crosses" = n())
    
  }else if(input$gen_activity == "Bunches"){
    result <- result  %>%
      dplyr::filter(between(`Bunch Harvest Date`, input$gen_dateRange[1], input$gen_dateRange[2]))%>%
      dplyr::summarise("Number of bunches" = n())
    
  }else if(input$gen_activity == "Seed extraction"){
    result <- result  %>%
      dplyr::filter(between(`Seed Extraction Date`, input$gen_dateRange[1], input$gen_dateRange[2]))%>%
      dplyr::summarise("Total Seeds" = sum(`Total Seeds`, na.rm = T),
                       "Good Seeds" = sum(`Good Seeds`, na.rm=T))
    
  }else if(input$gen_activity == "Embryo rescue"){
    result <- result  %>%
      dplyr::filter(between(`Embryo Rescue Date`, input$gen_dateRange[1], input$gen_dateRange[2]))%>%
      dplyr::summarise("Number of embryo" = sum(`Number of Embryo Rescued`, na.rm=T))
    
  }else if(input$gen_activity == "Embryo germination"){
    result <- result  %>%
      dplyr::filter(between(`Germination Date`, input$gen_dateRange[1], input$gen_dateRange[2]))%>%
      dplyr::summarise("Number of Germinated Embryo" = sum(`Number of Embryo Germinating`, na.rm=T))
    
  }else if(input$gen_activity == "Subcultures"){
    result <- result  %>%
      dplyr::filter(between(`Subculture Date`, input$gen_dateRange[1], input$gen_dateRange[2]))%>%
      dplyr::summarise("Number of Subcultures" = sum(Copies, na.rm=T))
    
  }else if(input$gen_activity == "Rooting"){
    result <- result  %>%
      dplyr::filter(between(`Date of Rooting`, input$gen_dateRange[1], input$gen_dateRange[2]))%>%
      dplyr::summarise("Number rooted" = sum(`Number in Rooting`, na.rm=T))
    
  }else if(input$gen_activity == "Weaning 1"){
    result <- result  %>%
      dplyr::filter(between(`Sending Out Date`, input$gen_dateRange[1], input$gen_dateRange[2]))%>%
      dplyr::summarise("Number of Sent Out" = sum(`Number Sent Out`, na.rm=T))
    
  }else if(input$gen_activity == "Weaning 2"){
    result <- result  %>%
      dplyr::filter(between(`Weaning 2 Date`, input$gen_dateRange[1], input$gen_dateRange[2]))%>%
      dplyr::summarise("Number in Weaning 2" = sum(`Number in Weaning 2`, na.rm=T))
    
  }else if(input$gen_activity == "Screenhouse"){
    result <- result  %>%
      dplyr::filter(between(`Screenhouse Transfer Date`, input$gen_dateRange[1], input$gen_dateRange[2]))%>%
      dplyr::summarise("Number in Screenhouse" = sum(`Number in Screenhouse`, na.rm=T))
    
  }else  if(input$gen_activity == "Hardening"){
    result <- result  %>%
      dplyr::filter(between(`Hardening Date`, input$gen_dateRange[1], input$gen_dateRange[2]))%>%
      dplyr::summarise("Number in Hardening" = sum(`Number in Hardening`, na.rm=T))
    
  }else if(input$gen_activity == "Open-field"){
    result <- result  %>%
      dplyr::filter(between(`Openfield Transfer Date`, input$gen_dateRange[1], input$gen_dateRange[2]))%>%
      dplyr::summarise("Number in Openfield" = sum(`Number in Openfield`, na.rm=T))
  }
  result %>%
    .[complete.cases(.),]
})


output$activityDT <- renderDT({
  
  DT::datatable(actvAggr(), 
                filter = 'top', 
                rownames = FALSE, 
                escape = FALSE, 
                options = list(pageLength = 10,
                               lengthMenu = c(10, 50, 100, 500,1000),
                               searchHighlight=T, 
                               stateSave = TRUE)
  )
})



# download
actvDownload <- reactive({
  result <- actvAggr()
  
  if(!is.null(input$activityDT_rows_selected)){
    result <- result[input$activityDT_rows_selected,]
  }
  result <- janitor::remove_empty(result, "cols")
  result[complete.cases(result[,1]),]
})

output$save_activityDT <- downloadHandler(
  filename <- function(){paste0('ActivitySpecific-',input$gen_dateRange[1], '-',input$gen_dateRange[2],'.csv')},
  content <- function(file) {
    readr::write_csv(actvDownload(), file) 
  }
)

# selected row details

activityDrill <- reactive({    
  loc <- actvAggr()[as.integer(input$activityDT_rows_selected), ]$Location
  female <- actvAggr()[as.integer(input$activityDT_rows_selected), ]$FemaleGenotype
  male <- actvAggr()[as.integer(input$activityDT_rows_selected), ]$MaleGenotype
  fploidy <- actvAggr()[as.integer(input$activityDT_rows_selected), ]$FemalePloidy
  mploidy <- actvAggr()[as.integer(input$activityDT_rows_selected), ]$MalePloidy
  crosstype <- actvAggr()[as.integer(input$activityDT_rows_selected), ]$CrossType
  
  result <- activityData() 
  
  if(!is.null(loc)){
    result <- result %>%
      dplyr::filter(Location %in% loc)
  }
  if(!is.null(female)){
    result <- result %>%
      dplyr::filter(FemaleGenotype %in% female)
  }
  if(!is.null(male)){
    result <- result %>%
      dplyr::filter(MaleGenotype %in% male)
  }
  result
})

observeEvent(input$gdetail_id,{
  shinyjs::show("gdetail_id")
})

output$activityDT <- renderDT({
  
  DT::datatable(activityDrill(), 
                filter = 'top', 
                rownames = FALSE, 
                escape = FALSE, 
                options = list(pageLength = 10,
                               lengthMenu = c(10, 50, 100, 500,1000),
                               searchHighlight=T, 
                               stateSave = TRUE)
  )
})


# download 

actvDetailsDownload <- reactive({
  result <- activityDrill()
  
  if(!is.null(input$activity_details_rows_selected)){
    result <- result[input$activity_details_rows_selected,]
  }
  result <- janitor::remove_empty(result, "cols")
  result[complete.cases(result[,1]),]
})

output$save_gen_details <- downloadHandler(
  filename <- function(){paste0('ActivitySpecificDetails-',input$gen_dateRange[1], '-',input$gen_dateRange[2],'.csv')},
  content <- function(file) {
    readr::write_csv(actvDetailsDownload(), file) #datasetInput
  }
)



