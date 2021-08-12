
bcBox <- reactive({
  result <- banana %>%
    dplyr::select(Location, Crossnumber,FemaleGenotype, MaleGenotype, CrossType,`First Pollination Date`) %>%
    dplyr::filter(CrossType == "Back-cross")
  
  if(!is.null(input$site)){
    result <- result %>%
      dplyr::filter(Location %in% input$site)
  }
  result %>%
    dplyr::filter(between(`First Pollination Date`, input$dateRange[1], input$dateRange[2]))
})

output$n_bc <- renderValueBox({
  result <- bcBox() %>%
    dplyr::group_by(FemaleGenotype, MaleGenotype) %>%
    dplyr::tally()
  
  box1<-valueBox(value=nrow(bcBox()),
                 color = "teal",
                 href="#",
                 subtitle=HTML("<b>Back Crosses</b><br>", nrow(result)," Unique combinations")
  )
  box1$children[[1]]$attribs$class<-"action-button"
  box1$children[[1]]$attribs$id<-"button_n_bc"
  return(box1)
})

observeEvent(input$button_n_bc, {
  toggleModal(session,"mod_bc","open")
  
  output$list_bc <- DT::renderDT({
    result <- bcBox()
    DT::datatable(result, filter = 'top', rownames = FALSE, escape = FALSE, 
                  options = list(pageLength = 10, lengthMenu = c(5, 10, 50, 100, 500,1000),
                                 searchHighlight=T, stateSave = TRUE))
  })
})
# download all or selected rows

downloadbc <- reactive({ 
  result <- bcBox()
  result = result[input[["list_bc_rows_all"]],]
  if(!is.null(input$list_bc_rows_selected)){
    result <- result[input$list_bc_rows_selected,]
  }
  result = result[complete.cases(result$Crossnumber),]
  result = janitor::remove_empty(result, "cols")
  return(result)
})


output$download_bc <- downloadHandler(
  filename = function(){
    paste0(input$site,"-","backcrosses",Sys.time(),".csv")
  },
  content = function(file) {
    readr::write_csv(downloadbc(), file)
  }
)