#generate the forest plot


# render the plot
output$forestplot <- renderPlot({
  req(window.size.slow())
  req(forest.reactive())
  dat.temp <- forest.reactive()
  #sort the data so that SNPs are in order and summary statistics are at the bottom
  #dat <- dat[order(-as.numeric(as.factor(dat$b)))]
  res <- as.character(dat.temp$SNP) %in% c("All - Inverse variance weighted", "All - MR Egger", "All - Weighted median")
  dat.temp.res <- dat.temp[res,]
  dat.temp <- dat.temp[!res,]
  dat.temp <- transform(dat.temp, SNP = reorder(SNP, b) )
  dat <- rbind(dat.temp.res, dat.temp)
  dat$highlight <- ifelse(
    grepl("^All.+", dat$SNP),
    ifelse(
      dat$b > 0,
      "protective",
      "risk"
      ),
    "normal")
  mycolours <- c("risk" = "red", "protective" = "seagreen4", "normal" = "black")
  
  # dat <- rbind(dat[!res,], dat[res,])
  # dat <- dat[order(-as.numeric(as.factor(dat$SNP))),]
  # summaryLength <- length(dat$SNP[grepl("^All", dat$SNP)])
  # len <- nrow(dat[,3])
  # labelSize <- ifelse(len<30,1,0.35)
  # summary <- c(rep.int(FALSE,len-3),TRUE, TRUE, TRUE)
  # if(summaryLength == 3){summary <- c(rep.int(FALSE,len-3),TRUE, TRUE, TRUE)}
  # if(summaryLength == 2){summary <- c(rep.int(FALSE,len-2),TRUE, TRUE)}
  # if(summaryLength == 1){summary <- c(rep.int(FALSE,len-1),TRUE)}
  # IVW <- dat[dat$SNP == "All - Inverse variance weighted"]
  # if(IVW$b > 0){clrs <- fpColors(box="royalblue",line="darkblue",summary="red")}else{clrs <- fpColors(box="royalblue",line="darkblue",summary="darkgreen")}
  # numCol <- round(isolate(window.size.slow()/200))
  #construct the plot
  ggplot(data=dat,
         aes(x = SNP,
             y = b,
             ymin = Lower95,
             ymax = Upper95)
  ) +
    scale_color_manual("Status", values = mycolours) +
    geom_pointrange(
      aes(col = highlight)
    ) +
    geom_hline(
               yintercept = 0,
               linetype=2) +
    geom_errorbar(
      width = 0,
      aes(
        ymin = Lower95,
        ymax = Upper95,
        col = highlight
          ),
      cex = 1) +
    theme(plot.title = element_text(size = 20,
                                    face = "bold"),
          axis.text.y = element_text(size = 8,
                                     face = 'bold'),
          axis.ticks.y = element_blank(),
          axis.text.x = element_text(face="bold"),
          axis.title = element_text(size = 16,
                                    face="bold"),
          # strip.text.y = element_text(hjust = 0,
          #                             vjust = 1,
          #                             angle = 180,
          #                             face = "bold"),
          #legend.title = element_blank(),
          #legend.text = element_text(size = 14),
          legend.position = "none"
    ) +
    #guides(col = guide_legend(ncol = numCol)) +
    xlab('SNP') +
    ylab("Beta Coefficient (95% Confidence Interval)") +
    #ggtitle(paste(input$plotNumber, 'Most Statistically Significant Gene Sets (FDR-Corrected)')) +
    coord_flip()
  # forestplot(labeltext=dat$SNP, 
  #            txt_gp = fpTxtGp(label=gpar(cex=labelSize), ticks=gpar(cex=labelSize)),
  #            mean=dat$b, 
  #            lower=dat$Lower95, 
  #            upper=dat$Upper95,
  #            is.summary=summary,
  #            col=clrs)
},
width = function()
  {
  req(input$execute)
  req(window.size.slow())
  return(as.numeric(window.size.slow())*.9)
  },
height = function() {
  req(input$execute)
  heightout <- ifelse(nrow(forest.reactive())*50 > 1000, 1500, nrow(forest.reactive())*50)
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