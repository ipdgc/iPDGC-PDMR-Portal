ForestDL_function <- function() {
  #sheet <- "res0001"
  dataForPlotting1temp <- fread(paste0("www/dataForestPlot_PD_", sub(".*(20\\d\\d).*", "\\1", input$outcome), ".csv"))#sheet)
  dataForPlotting1 <- subset(dataForPlotting1temp, dataForPlotting1temp$Exposure==input$trait)# & dataForPlotting1temp$Outcome==input$outcome)
  
  #select data for the selected trait
  dataForPlotting2 <- subset(dataForPlotting1,  Exposure ==input$trait)
  
  #sort the data so that SNPs are in order and summary statistics are at the bottom
  dataForPlotting2$index <- substr(dataForPlotting2$SNP,1,2)
  # dataForPlotting2 <- dataForPlotting2[order(-as.numeric(as.factor(dataForPlotting2$index)), dataForPlotting2$b),]
  dataForPlotting2 <- dataForPlotting2[order(-as.numeric(as.factor(dataForPlotting2$SNP))),]
  summaryLength <- length(dataForPlotting2$index[dataForPlotting2$index == "Al"])
  
  #stops plotting if no data
  req(nrow(dataForPlotting2) >0 )
  
  #create the vectors for plotting
  mean <- dataForPlotting2$b
  lower <- dataForPlotting2$Lower95
  upper <- dataForPlotting2$Upper95
  labeltext <- dataForPlotting2$SNP
  len <- nrow(dataForPlotting2[,3])
  labelSize <- ifelse(len<30,1,0.35)
  summary <- c(rep.int(FALSE,len-3),TRUE, TRUE, TRUE)
  if(summaryLength == 3){summary <- c(rep.int(FALSE,len-3),TRUE, TRUE, TRUE)}
  if(summaryLength == 2){summary <- c(rep.int(FALSE,len-2),TRUE, TRUE)}
  if(summaryLength == 1){summary <- c(rep.int(FALSE,len-1),TRUE)}
  IVW <- subset(dataForPlotting2, SNP == "All - Inverse variance weighted")
  if(IVW$b > 0){clrs <- fpColors(box="royalblue",line="darkblue",summary="red")}else{clrs <- fpColors(box="royalblue",line="darkblue",summary="darkgreen")}
  #construct the plot
  forestplot <- forestplot(labeltext=labeltext, 
                           txt_gp = fpTxtGp(label=gpar(cex=labelSize), ticks=gpar(cex=labelSize)),
                           mean=mean, 
                           lower=lower, 
                           upper=upper,
                           is.summary=summary,
                           col=clrs,
                           title=ifelse(input$forestTitle, paste0(input$trait, " vs ", input$outcome), ""))
  return(forestplot)
}

FunnelDL_function <- function(){
  dataForPlottingtemp <- fread(paste0("www/dataForestPlot_PD_", sub(".*(20\\d\\d).*", "\\1", input$outcome), ".csv"))
  dataForPlotting <- subset(dataForPlottingtemp, dataForPlottingtemp$Exposure==input$trait)# & dataForPlottingtemp$Outcome==input$outcome)
  
  #select data for the selected trait
  #dataForPlotting2 <- subset(dataForPlotting1,  Exposure ==input$trait)
  
  #sort the data so that SNPs are in order and summary statistics are at the bottom
  dataForPlotting$index <- substr(dataForPlotting$SNP,1,2)
  # dataForPlotting <- dataForPlotting[order(-as.numeric(as.factor(dataForPlotting$index)), dataForPlotting$b),]
  dataForPlotting <- dataForPlotting[order(-as.numeric(as.factor(dataForPlotting$SNP))),]
  summaryLength <- length(dataForPlotting$index[dataForPlotting$index == "Al"])
  dataForPlotting$`1/SE` <- 1/as.numeric(dataForPlotting$se)
  funnelplotlines <- subset(dataForPlotting, grepl("All", dataForPlotting$SNP)) # startsWith(SNP, "All")) 
  dataForPlotting <- subset(dataForPlotting, !grepl("^All - ", dataForPlotting$SNP))
  
  #stops plotting if no data
  req(nrow(dataForPlotting) >0 )
  
  #create the vectors for plotting
  plot(dataForPlotting$b, dataForPlotting$`1/SE`, xlab = expression('β'["IV"]), ylab = expression("1/SE"["IV"]), main = ifelse(input$funnelTitle, paste0(input$trait, " vs ", input$outcome), ""))
  abline(v = subset(funnelplotlines, grepl("All - Inverse variance weighted", funnelplotlines$SNP))[1,c("b")], col="blue", lwd = 2)
  abline(v = subset(funnelplotlines, grepl("All - MR Egger", funnelplotlines$SNP))[1,c("b")], col="#ff5e5e", lwd = 2)
  abline(v = subset(funnelplotlines, grepl("All - Weighted median", funnelplotlines$SNP))[1,c("b")], col="#f2c71d", lwd = 2)
}

# DownloadHandlers for the forest plot
output$FoPlotDLTIFF <- downloadHandler(
  filename = function() {
    paste('PD_MR_ForestPlot-', Sys.Date(), '.tiff', sep='')
  },
  content = function(file) {
    tiff(file, bg = input$fobg, width = input$FoDLplotw, height = input$FoDLploth, units = "in", compression = "none", res = 600)
    ForestDL_function()
    dev.off()
  }
)

output$FoPlotDLPDF <- downloadHandler(
  filename = function() {
    paste('PD_MR_ForestPlot-', Sys.Date(), '.pdf', sep='')
  },
  content = function(file) {
    cairo_pdf(file, bg = input$fobg, width = input$FoDLplotw, height = input$FoDLploth)
    ForestDL_function()
    dev.off()
  }
)

output$FoPlotDLSVG <- downloadHandler(
  filename = function() {
    paste('PD_MR_ForestPlot-', Sys.Date(), '.svg', sep='')
  },
  content = function(file) {
    svg(file, bg = input$fobg, width = input$FoDLplotw, height = input$FoDLploth)
    ForestDL_function()
    dev.off()
  }
)


output$FuPlotDLTIFF <- downloadHandler(
  filename = function() {
    paste('PD_MR_FunnelPlot-', Sys.Date(), '.tiff', sep='')
  },
  content = function(file) {
    tiff(file, bg = input$fubg, width = input$FuDLplotw, height = input$FuDLploth, units = "in", compression = "none", res = 600)
    FunnelDL_function()
    dev.off()
  }
)

output$FuPlotDLPDF <- downloadHandler(
  filename = function() {
    paste('PD_MR_FunnelPlot-', Sys.Date(), '.pdf', sep='')
  },
  content = function(file) {
    cairo_pdf(file, bg = input$fubg, width = input$FuDLplotw, height = input$FuDLploth)
    FunnelDL_function()
    dev.off()
  }
)

output$FuPlotDLSVG <- downloadHandler(
  filename = function() {
    paste('PD_MR_FunnelPlot-', Sys.Date(), '.svg', sep='')
  },
  content = function(file) {
    svg(file, bg = input$fubg, width = input$FuDLplotw, height = input$FuDLploth)
    FunnelDL_function()
    dev.off()
  }
)

# downloadHandlers for the tables
output$ResultDLCSV <- downloadHandler(
  filename = function() {
    paste('PD_MR_tables-', Sys.Date(), '.csv', sep='')
  },
  content = function(file) {
    if ('resDLcheck' %ni% input$ResultDLselect &&
        'hetDLcheck' %ni% input$ResultDLselect &&
        'pleDLcheck' %ni% input$ResultDLselect &&
        'revDLcheck' %ni% input$ResultDLselect &&
        'pubDLcheck' %ni% input$ResultDLselect) {
      stop('ERROR-Please select at least one table')}
    tbl2 <- NULL
    tbl2_2 <- NULL
    tbl2_3 <- NULL
    tbl2_5 <- NULL
    tbl2_4 <- NULL
    tbl1 <- fread("www/dataResultsTable-res.csv")
    if ('resDLcheck' %in% input$ResultDLselect) {
      tbl2 <- subset(tbl1, tbl1$Exposure==input$trait & tbl1$Outcome==input$outcome)}
    tbl2$`no. of SNPs` <- ifelse('resDLcheck' %in% input$ResultDLselect, as.character(tbl2$`no. of SNPs`), NULL)
    #tbl2 <- subset(tbl1, tbl1$Exposure==input$trait & tbl1$Outcome==input$outcome)
    #tbl2$`no. of SNPs` <- as.character(tbl2$`no. of SNPs`)
    tbl1_2 <- fread("www/dataResultsTable-het.csv")
    if ('hetDLcheck' %in% input$ResultDLselect) {
      tbl2_2 <- subset(tbl1_2, tbl1_2$Exposure==input$trait & tbl1_2$Outcome==input$outcome)}
    #tbl2_2 <- subset(tbl1_2, tbl1_2$Exposure==input$trait & tbl1_2$Outcome==input$outcome)
    tbl1_3 <- fread("www/dataResultsTable-ple.csv")
    if ('pleDLcheck' %in% input$ResultDLselect) {
      tbl2_3 <- subset(tbl1_3, tbl1_3$Exposure==input$trait & tbl1_3$Outcome==input$outcome)}
    #tbl2_3 <- subset(tbl1_3, tbl1_3$Exposure==input$trait & tbl1_3$Outcome==input$outcome)
    tbl1_4 <- fread("www/dataResultsTable-link.csv")
    if ('pubDLcheck' %in% input$ResultDLselect) {
      tbl2_4 <- subset(tbl1_4, tbl1_4$Exposure==input$trait)}
    #tbl2_4 <- subset(tbl1_4, tbl1_4$Exposure==input$trait)
    tbl1_5 <- fread("www/dataResultsTable-RC.csv")
    if ('revDLcheck' %in% input$ResultDLselect){
      tbl2_5 <- subset(tbl1_5, Outcome==input$trait & Exposure==input$outcome)}
    resDL <- list(tbl2, tbl2_2, tbl2_3, tbl2_5, tbl2_4)
    #print(resDL)
    #lapply(resDL, write, file, append=TRUE, ncolumns=1000)
    resDL <- resDL[!sapply(resDL, is.null)]
    lapply(resDL, function(x) write.table( data.frame(x), file, append= T, row.names = F, quote = T, sep = ","))
    #capture.output(summary(resDL), file = file)
  }
)

# output$ResDLtab <- downloadHandler(
#   filename = function() {
#     paste('PD_MR_tables-', Sys.Date(), '.txt', sep='')
#   },
#   content = function(file) {
#     write.table(ResTbl_reactive(), file, quote = F, sep = "\t")
#     }
# )
output$ResultDLTab <- downloadHandler(
  filename = function() {
    paste('PD_MR_tables-', Sys.Date(), '.txt', sep='')
  },
  content = function(file) {
    if ('resDLcheck' %ni% input$ResultDLselect &&
        'hetDLcheck' %ni% input$ResultDLselect &&
        'pleDLcheck' %ni% input$ResultDLselect &&
        'revDLcheck' %ni% input$ResultDLselect &&
        'pubDLcheck' %ni% input$ResultDLselect) {
      stop('ERROR-Please select at least one table')}
    tbl2 <- NULL
    tbl2_2 <- NULL
    tbl2_3 <- NULL
    tbl2_5 <- NULL
    tbl2_4 <- NULL
    #resDL <- list()
    tbl1 <- fread("www/dataResultsTable-res.csv")
    if ('resDLcheck' %in% input$ResultDLselect) {
      tbl2 <- tbl1[tbl1$Exposure==input$trait & tbl1$Outcome==input$outcome]}
    tbl2$`no. of SNPs` <- ifelse('resDLcheck' %in% input$ResultDLselect, as.character(tbl2$`no. of SNPs`), NULL)
    #tbl2 <- subset(tbl1, tbl1$Exposure==input$trait & tbl1$Outcome==input$outcome)
    #tbl2$`no. of SNPs` <- as.character(tbl2$`no. of SNPs`)
    tbl1_2 <- fread("www/dataResultsTable-het.csv") 
    if ('hetDLcheck' %in% input$ResultDLselect) {
      tbl2_2 <- tbl1_2[tbl1_2$Exposure==input$trait & tbl1_2$Outcome==input$outcome]}
    #tbl2_2 <- subset(tbl1_2, tbl1_2$Exposure==input$trait & tbl1_2$Outcome==input$outcome)
    tbl1_3 <- fread("www/dataResultsTable-ple.csv") 
    if ('pleDLcheck' %in% input$ResultDLselect) {
      tbl2_3 <- tbl1_3[tbl1_3$Exposure==input$trait & tbl1_3$Outcome==input$outcome]}
    #tbl2_3 <- subset(tbl1_3, tbl1_3$Exposure==input$trait & tbl1_3$Outcome==input$outcome)
    tbl1_4 <- fread("www/dataResultsTable-link.csv")
    if ('pubDLcheck' %in% input$ResultDLselect) {
      tbl2_4 <- tbl1_4[tbl1_4$Exposure==input$trait]}
    #tbl2_4 <- subset(tbl1_4, tbl1_4$Exposure==input$trait)
    tbl1_5 <- fread("www/dataResultsTable-RC.csv")
    if ('revDLcheck' %in% input$ResultDLselect){
      tbl2_5 <- tbl1_5[tbl1_5$Outcome==input$trait & Exposure==input$outcome]}
    resDL <- list(tbl2, tbl2_2, tbl2_3, tbl2_5, tbl2_4)
    #print(resDL)
    #lapply(resDL, write, file, append=TRUE, ncolumns=1000)
    resDL <- resDL[!sapply(resDL, is.null)]
    lapply(resDL, function(x) write.table( data.frame(x), file, append= T, row.names = F, quote = T, sep = ","))
    #capture.output(summary(resDL), file = file)
  }
)

output$ResultDLPDF <- downloadHandler(
  filename = function() {
    paste('PD_MR_tables-', Sys.Date(), '.tex', sep='')
  },
  content = function(file) {
    if ('resDLcheck' %ni% input$ResultDLselect &&
        'hetDLcheck' %ni% input$ResultDLselect &&
        'pleDLcheck' %ni% input$ResultDLselect &&
        'revDLcheck' %ni% input$ResultDLselect &&
        'pubDLcheck' %ni% input$ResultDLselect) {
      stop('ERROR-Please select at least one table')}
    tbl2 <- NULL
    tbl2_2 <- NULL
    tbl2_3 <- NULL
    tbl2_5 <- NULL
    tbl2_4 <- NULL
    tbl1 <- fread("www/dataResultsTable-res.csv")
    #valOutcome <- #sub("(.*20\\d\\d).*", "\\1", input$outcome)
    if ('resDLcheck' %in% input$ResultDLselect) {
      tbl2 <- subset(tbl1, tbl1$Exposure==input$trait & tbl1$Outcome==input$outcome)}
    tbl2$`no. of SNPs` <- ifelse('resDLcheck' %in% input$ResultDLselect, as.character(tbl2$`no. of SNPs`), NULL)
    #tbl2 <- subset(tbl1, tbl1$Exposure==input$trait & tbl1$Outcome==input$outcome)
    #tbl2$`no. of SNPs` <- as.character(tbl2$`no. of SNPs`)
    tbl1_2 <- fread("www/dataResultsTable-het.csv")
    if ('hetDLcheck' %in% input$ResultDLselect) {
      tbl2_2 <- subset(tbl1_2, tbl1_2$Exposure==input$trait & tbl1_2$Outcome==input$outcome)}
    #tbl2_2 <- subset(tbl1_2, tbl1_2$Exposure==input$trait & tbl1_2$Outcome==input$outcome)
    tbl1_3 <- fread("www/dataResultsTable-ple.csv")
    if ('pleDLcheck' %in% input$ResultDLselect) {
      tbl2_3 <- subset(tbl1_3, tbl1_3$Exposure==input$trait & tbl1_3$Outcome==input$outcome)}
    #tbl2_3 <- subset(tbl1_3, tbl1_3$Exposure==input$trait & tbl1_3$Outcome==input$outcome)
    tbl1_4 <- fread("www/dataResultsTable-link.csv")
    if ('pubDLcheck' %in% input$ResultDLselect) {
      tbl2_4 <- subset(tbl1_4, tbl1_4$Exposure==input$trait)}
    #tbl2_4 <- subset(tbl1_4, tbl1_4$Exposure==input$trait)
    tbl1_5 <- fread("www/dataResultsTable-RC.csv")
    if ('revDLcheck' %in% input$ResultDLselect){
      tbl2_5 <- subset(tbl1_5, Outcome==input$trait & Exposure==input$outcome)}
    resDL <- list(tbl2, tbl2_2, tbl2_3, tbl2_5, tbl2_4)
    #print(resDL)
    #lapply(resDL, write, file, append=TRUE, ncolumns=1000)
    resDL <- resDL[!sapply(resDL, is.null)]
    lapply(resDL, function(x) print.xtable( xtable(data.frame(x)), file = file, append= T, include.rownames = F))
    #capture.output(summary(resDL), file = file)
  }
)

output$qkDLforestplot <- downloadHandler(
  filename = function() {
    paste('PD_MR_ForestPlot-', Sys.Date(), '.pdf', sep='')
  },
  content = function(file) {
    cairo_pdf(file, width = 8.5, height = 11)
    ForestDL_function()
    dev.off()
  }
)

output$qkDLfunnelplot <- downloadHandler(
  filename = function() {
    paste('PD_MR_FunnelPlot-', Sys.Date(), '.pdf', sep='')
  },
  content = function(file) {
    cairo_pdf(file, width = 8.5, height = 11)
    FunnelDL_function()
    dev.off()
  }
)

output$qkDLtable <- downloadHandler(
  filename = function() {
    paste('PD_MR_tables-', Sys.Date(), '.txt', sep='')
  },
  content = function(file) {
    tbl1 <- fread("www/dataResultsTable-res.csv")
    tbl2 <- subset(tbl1, tbl1$Exposure==input$trait & tbl1$Outcome==input$outcome)
    tbl2$`no. of SNPs` <- as.character(tbl2$`no. of SNPs`)
    tbl1_2 <- fread("www/dataResultsTable-het.csv")
    tbl2_2 <- subset(tbl1_2, tbl1_2$Exposure==input$trait & tbl1_2$Outcome==input$outcome)
    tbl1_3 <- fread("www/dataResultsTable-ple.csv")
    tbl2_3 <- subset(tbl1_3, tbl1_3$Exposure==input$trait & tbl1_3$Outcome==input$outcome)
    tbl1_4 <- fread("www/dataResultsTable-link.csv")
    tbl2_4 <- subset(tbl1_4, tbl1_4$Exposure==input$trait)
    tbl1_5 <- fread("www/dataResultsTable-RC.csv")
    tbl2_5 <- subset(tbl1_5, Outcome==input$trait & Exposure==input$outcome)
    resDL <- list(tbl2, tbl2_2, tbl2_3, tbl2_5, tbl2_4)
    lapply(resDL, function(x) write.table( data.frame(x), file, append= T, row.names = F, quote = T, sep = ","))
  }
)