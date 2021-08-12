

suppressPackageStartupMessages(library(qrencoder))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(DT))

banana <- readRDS("data/banana.rds")

output$downloadlabelsDT <- renderDT({
dt <- banana %>%
  dplyr::select(Location,Crossnumber,FemalePlotName,FemaleGenotype,MalePlotName,MaleGenotype,`First Pollination Date`)# %>%

DT::datatable(dt, filter = 'top', rownames = FALSE, escape = FALSE, 
              options = list(pageLength = 5, lengthMenu = c(5, 10, 50, 100, 500,1000),
                              searchHighlight=T, stateSave = TRUE))
})


downloadlabelsIn <- reactive({
  result = banana %>%
    dplyr::select(Location,Crossnumber,FemalePlotName,Mother,MalePlotName,Father,`First Pollination Date`)
  
  if(!is.null(input$downloadlabelsDT_rows_selected)){
    result = result[input$downloadlabelsDT_rows_selected,]
  }
  result
})

 output$downloadlabels <- downloadHandler(
   filename = paste("Barcode labels -",Sys.Date(),".pdf"),
   
   content = function(file) {
     pdf(file, paper = "letter", width = 2, height = 11, pagecentre = F) # right align width=6.0 # left width=2.0,
      par(mfrow=c(10, 1),mar=c(1,0,1,1), oma=c(1,1,1,1)) # right align mar=c(0,30,3,0)
     
     for(i in 1:(nrow(downloadlabelsIn()))){
       txtLabel <- noquote(grep(input$barcode_text, names(banana)))
       image(
           qrencode_raster(as.character(downloadlabelsIn()[i,"Crossnumber"])), # QRcode
              cex.main = 1.5, cex.sub = .8, asp=1, col=c("white", "black"), axes=F, 
              xlab="", ylab="", subtitle = mtext(paste(as.character(downloadlabelsIn()[i,"Crossnumber"]),"\n", 
                                                       downloadlabelsIn()[i,..txtLabel]), side = 4, line = 0,
                                                 outer = F, at = NA, adj = 0, padj = 0.5, cex = 1, col = 1, las=1, font = 10)
       )
       
      }
     dev.off()
   }
 )

