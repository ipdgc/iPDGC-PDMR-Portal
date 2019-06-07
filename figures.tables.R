#generate the forest plot


# render the plot
output$forestplot <- renderPlot({
  req(window.size.slow())
  req(forest.reactive())
  dat <- forest.reactive()
  summaryLength <- length(dat$SNP[grepl("^All", dat$SNP)])
  len <- nrow(dat[,3])
  labelSize <- ifelse(len<30,1,0.35)
  summary <- c(rep.int(FALSE,len-3),TRUE, TRUE, TRUE)
  if(summaryLength == 3){summary <- c(rep.int(FALSE,len-3),TRUE, TRUE, TRUE)}
  if(summaryLength == 2){summary <- c(rep.int(FALSE,len-2),TRUE, TRUE)}
  if(summaryLength == 1){summary <- c(rep.int(FALSE,len-1),TRUE)}
  IVW <- dat[dat$SNP == "All - Inverse variance weighted"]
  if(IVW$b > 0){clrs <- fpColors(box="royalblue",line="darkblue",summary="red")}else{clrs <- fpColors(box="royalblue",line="darkblue",summary="darkgreen")}
  #construct the plot
  forestplot(labeltext=dat$SNP, 
             txt_gp = fpTxtGp(label=gpar(cex=labelSize), ticks=gpar(cex=labelSize)),
             mean=dat$b, 
             lower=dat$Lower95, 
             upper=dat$Upper95,
             is.summary=summary,
             col=clrs)
},
width = function()
  {
  req(input$execute)
  req(window.size.slow())
  return(as.numeric(window.size.slow())*.9)
  },
height = function() {
  req(input$execute)
  heightout <- ifelse(nrow(forest.reactive())*50 > 1000, 1000, nrow(forest.reactive())*50)
  return(heightout)
}) #"700")

output$funnelplot <- renderPlot({
  req(window.size.slow())
  req(forest.reactive())
  dat <- forest.reactive()
  dat$`1/SE` <- 1/as.numeric(dat$se)
  funnelplotlines <- dat[grepl("All", dat$SNP)]
  dat <- dat[!grepl("^All - ", dat$SNP)]
  plot(dat$b, dat$`1/SE`, xlab = expression('β'["IV"]), ylab = expression("1/SE"["IV"]))
  abline(v = funnelplotlines[grepl("All - Inverse variance weighted", funnelplotlines$SNP)][1,c("b")], col="blue", lwd = 2)
  abline(v = funnelplotlines[grepl("All - MR Egger", funnelplotlines$SNP)][1,c("b")], col="#ff5e5e", lwd = 2)
  abline(v = funnelplotlines[grepl("All - Weighted median", funnelplotlines$SNP)][1,c("b")], col="#f2c71d", lwd = 2)
},
width = function()
{
  req(input$execute)
  req(window.size.slow())
  return(as.numeric(window.size.slow())*.9)},
height = 400)


output$table1.output <- renderTable({ ResTbl.reactive() }, digits = -2)
output$minitable1.output <- renderTable({
  pvalBold <- function (x) {
    ifelse(x < 0.05, paste0("<strong>", x , "</strong>"), x) 
  }
  tbl2 <- ResTbl.reactive()[,c(2:6)]
  tbl2$`p-value` <- lapply(tbl2$`p-value`, pvalBold)
  tbl2 }, digits = NULL, sanitize.text.function = function(x) x)

output$table2.output <- renderTable({ HetTbl.reactive() }, digits = -2)

output$table3.output <- renderTable({ PleTbl.reactive() }, digits = -2)

output$table4.output <- renderTable({ PubTbl.reactive() }, digits = 0, sanitize.text.function = function(x) x)

output$table5.output <- renderTable({ RevTbl.reactive() }, digits = -2)