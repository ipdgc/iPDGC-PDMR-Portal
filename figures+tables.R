#generate the forest plot
#read in data
forest_reactive <- reactive({
  req(input$execute)
  dataForPlottingtemp <- isolate(fread(paste0("www/dataForestPlot_PD_", sub(".*(20\\d\\d).*", "\\1", input$outcome), ".csv")))
  dataForPlotting <- isolate(dataForPlottingtemp[dataForPlottingtemp$Exposure==input$trait])# & dataForPlottingtemp$Outcome==input$outcome)
  
  #select data for the selected trait
  #dataForPlotting2 <- subset(dataForPlotting1,  Exposure ==input$trait)
  
  #sort the data so that SNPs are in order and summary statistics are at the bottom
  #dataForPlotting$index <- substr(dataForPlotting$SNP,1,2)
  # dataForPlotting <- dataForPlotting[order(-as.numeric(as.factor(dataForPlotting$index)), dataForPlotting$b),]
  dataForPlotting <- dataForPlotting[order(-as.numeric(as.factor(dataForPlotting$SNP))),]
  #summaryLength <- length(dataForPlotting$index[dataForPlotting$index == "Al"])
  
  #stops plotting if no data
  req(nrow(dataForPlotting) >0 )
  return(dataForPlotting)
  
  #create the vectors for plotting
  #return(forestplot)
})
#funnel plot
# funnel_reactive <- reactive({
#   req(input$execute)
#   dataForPlottingtemp <- isolate(fread(paste0("www/dataForestPlot_PD_", sub(".*(20\\d\\d).*", "\\1",  input$outcome), ".csv")))#read_xlsx("www/dataForestPlot_PD.xlsx", sheet = input$outcome))#"res0001")
#   dataForPlotting <- isolate(dataForPlottingtemp[dataForPlottingtemp$Exposure==input$trait])# & dataForPlottingtemp$Outcome==input$outcome)
#   
#   #select data for the selected trait
#   #dataForPlotting2 <- subset(dataForPlotting1,  Exposure ==input$trait)
#   
#   #sort the data so that SNPs are in order and summary statistics are at the bottom
#   #dataForPlotting$index <- substr(dataForPlotting$SNP,1,2)
#   # dataForPlotting <- dataForPlotting[order(-as.numeric(as.factor(dataForPlotting$index)), dataForPlotting$b),]
#   dataForPlotting <- dataForPlotting[order(-as.numeric(as.factor(dataForPlotting$SNP))),]
#   #summaryLength <- length(dataForPlotting$index[dataForPlotting$index == "Al"])
#   dataForPlotting$`1/SE` <- 1/as.numeric(dataForPlotting$se)
#   funnelplotlines <- dataForPlotting[grepl("All", dataForPlotting$SNP)] # startsWith(SNP, "All")) 
#   dataForPlotting <- dataForPlotting[!grepl("^All - ", dataForPlotting$SNP)]
#   
#   #stops plotting if no data
#   req(nrow(dataForPlotting) >0 )
#   
#   #create the vectors for plotting
#   plot(dataForPlotting$b, dataForPlotting$`1/SE`, xlab = expression('β'["IV"]), ylab = expression("1/SE"["IV"]))
#   abline(v = funnelplotlines[grepl("All - Inverse variance weighted", funnelplotlines$SNP)][1,c("b")], col="blue", lwd = 2)
#   abline(v = funnelplotlines[grepl("All - MR Egger", funnelplotlines$SNP)][1,c("b")], col="#ff5e5e", lwd = 2)
#   abline(v = funnelplotlines[grepl("All - Weighted median", funnelplotlines$SNP)][1,c("b")], col="#f2c71d", lwd = 2)
#   #abline(v = 0, col = "red") #test line
# })

# output$boxheight <- reactive({function() {
#   dataForPlottingtemp <- read_xlsx("www/dataForestPlot_PD.xlsx", sheet = "res0001")
#   dataForPlotting <- subset(dataForPlottingtemp, dataForPlottingtemp$Exposure==input$trait & dataForPlottingtemp$Outcome==input$outcome)
#   return(paste0(as.character(nrow(dataForPlotting)*75+100)), "px")
# }})

# render the plot
output$forestplot <- renderPlot({
  dataForPlotting <- forest_reactive()
  summaryLength <- length(dataForPlotting$SNP[grepl("^All", dataForPlotting$SNP)])
  len <- nrow(dataForPlotting[,3])
  labelSize <- ifelse(len<30,1,0.35)
  summary <- c(rep.int(FALSE,len-3),TRUE, TRUE, TRUE)
  if(summaryLength == 3){summary <- c(rep.int(FALSE,len-3),TRUE, TRUE, TRUE)}
  if(summaryLength == 2){summary <- c(rep.int(FALSE,len-2),TRUE, TRUE)}
  if(summaryLength == 1){summary <- c(rep.int(FALSE,len-1),TRUE)}
  IVW <- dataForPlotting[dataForPlotting$SNP == "All - Inverse variance weighted"]
  if(IVW$b > 0){clrs <- fpColors(box="royalblue",line="darkblue",summary="red")}else{clrs <- fpColors(box="royalblue",line="darkblue",summary="darkgreen")}
  #construct the plot
  forestplot <- forestplot(labeltext=dataForPlotting$SNP, 
                           txt_gp = fpTxtGp(label=gpar(cex=labelSize), ticks=gpar(cex=labelSize)),
                           mean=dataForPlotting$b, 
                           lower=dataForPlotting$Lower95, 
                           upper=dataForPlotting$Upper95,
                           is.summary=summary,
                           col=clrs)
  forestplot
},
width = function()
{return(as.numeric(input$dimension)*.9)},
height = function() {
  req(input$execute)
  #dataForPlottingtemp <- isolate(fread(paste0("www/dataForestPlot_PD_", sub(".*(20\\d\\d).*", "\\1",  input$outcome), ".csv")))#"res0001")
  #dataForPlotting <- forest_reactive()# & dataForPlottingtemp$Outcome==input$outcome)
  heightout <- ifelse(nrow(forest_reactive())*50 > 1000, 1000, nrow(forest_reactive())*50)
  return(heightout)
}) #"700")

output$funnelplot <- renderPlot({
  dataForPlotting <- forest_reactive()
  dataForPlotting$`1/SE` <- 1/as.numeric(dataForPlotting$se)
  funnelplotlines <- dataForPlotting[grepl("All", dataForPlotting$SNP)]
  dataForPlotting <- dataForPlotting[!grepl("^All - ", dataForPlotting$SNP)]
  plot(dataForPlotting$b, dataForPlotting$`1/SE`, xlab = expression('β'["IV"]), ylab = expression("1/SE"["IV"]))
  abline(v = funnelplotlines[grepl("All - Inverse variance weighted", funnelplotlines$SNP)][1,c("b")], col="blue", lwd = 2)
  abline(v = funnelplotlines[grepl("All - MR Egger", funnelplotlines$SNP)][1,c("b")], col="#ff5e5e", lwd = 2)
  abline(v = funnelplotlines[grepl("All - Weighted median", funnelplotlines$SNP)][1,c("b")], col="#f2c71d", lwd = 2)
},
width = function()
{return(as.numeric(input$dimension)*.9)},
height = 400) 

#generate the tables
ResTbl_reactive <- reactive({
  req(input$execute)
  tbl1 <- fread("www/dataResultsTable-res.csv")
  tbl2 <- isolate(tbl1[tbl1$Exposure==input$trait & tbl1$Outcome==input$outcome])
  tbl2$`no. of SNPs` <- as.character(tbl2$`no. of SNPs`)
  
  #stops call if no data
  req(nrow(tbl2) >0 )
  
  return(tbl2)
})

# MiniResTbl_reactive <- reactive({
#   req(input$execute)
#   tbl1 <- fread("www/dataResultsTable-res.csv")
#   tbl2 <- isolate(subset(tbl1, tbl1$Exposure==input$trait & tbl1$Outcome==input$outcome))
#   tbl2$`no. of SNPs` <- as.character(tbl2$`no. of SNPs`)
#   tbl2$Exposure <- NULL
#   tbl2$Outcome <- NULL
#   pvalBold <- function (x) {
#     ifelse(x < 0.05, paste0("<strong>", x , "</strong>"), x) 
#   }
#   tbl2$`p-value` <- lapply(tbl2$`p-value`, pvalBold)
#   
#   #stops call if no data
#   req(nrow(tbl2) >0 )
#   
#   return(tbl2)
# })

output$table1.output <- renderTable({ ResTbl_reactive() }, digits = -2)
output$minitable1.output <- renderTable({
  pvalBold <- function (x) {
    ifelse(x < 0.05, paste0("<strong>", x , "</strong>"), x) 
  }
  tbl2 <- ResTbl_reactive()
  tbl2$`p-value` <- lapply(tbl2$`p-value`, pvalBold)
  tbl2 }, digits = NULL, sanitize.text.function = function(x) x)


HetTbl_reactive <- reactive({
  req(input$execute)
  tbl1_2 <- fread("www/dataResultsTable-het.csv") 
  tbl2_2 <- isolate(subset(tbl1_2, tbl1_2$Exposure==input$trait & tbl1_2$Outcome==input$outcome))
  # tbl2$`no. of SNPs` <- as.character(tbl2$`no. of SNPs`)
  
  #stops call if no data
  req(nrow(tbl2_2) >0 )
  
  return(tbl2_2)
})

output$table2.output <- renderTable({ HetTbl_reactive() }, digits = -2)

PleTbl_reactive <- reactive({
  req(input$execute)
  tbl1_3 <- fread("www/dataResultsTable-ple.csv")  
  tbl2_3 <- isolate(subset(tbl1_3, tbl1_3$Exposure==input$trait & tbl1_3$Outcome==input$outcome))
  # tbl2$`no. of SNPs` <- as.character(tbl2$`no. of SNPs`)
  
  #stops call if no data
  req(nrow(tbl2_3) >0 )
  
  return(tbl2_3)
})

output$table3.output <- renderTable({ PleTbl_reactive() }, digits = -2)

PubTbl_reactive <- reactive({
  req(input$execute)
  tbl1_4 <- fread("www/dataResultsTable-link.csv")
  tbl2_4 <- isolate(subset(tbl1_4, tbl1_4$Exposure==input$trait))
  skip <- ifelse(isolate(tbl2_4$PubmedID == 0), T, F)
  req(skip == F)
  tbl2_4$PubmedID <- paste0("<a href='https://www.ncbi.nlm.nih.gov/pubmed/",tbl2_4$PubmedID,"'>",tbl2_4$PubmedID,"</a>")
  # tbl2$`no. of SNPs` <- as.character(tbl2$`no. of SNPs`)
  
  #stops call if no data
  req(nrow(tbl2_4) >0 )
  
  return(tbl2_4)
})

output$table4.output <- renderTable({ PubTbl_reactive() }, digits = 0, sanitize.text.function = function(x) x)

RevTbl_reactive <- reactive({
  req(input$execute)
  tbl1_5 <- fread("www/dataResultsTable-RC.csv") 
  tbl2_5 <- isolate(subset(tbl1_5, Outcome==input$trait & Exposure==input$outcome))
  # tbl2$`no. of SNPs` <- as.character(tbl2$`no. of SNPs`)
  
  #stops call if no data
  req(nrow(tbl2_5) >0 )
  
  return(tbl2_5)
})

output$table5.output <- renderTable({ RevTbl_reactive() }, digits = -2)