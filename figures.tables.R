#generate the forest plot


# render the plot
output$forestplot <- renderPlot({
  req(window.size.slow())
  req(forest.reactive())
  dat.temp <- forest.reactive()
  # sort the data so that SNPs are in order of beta and summary statistics are at the bottom
  res <- as.character(dat.temp$SNP) %in% c("All - Inverse variance weighted", "All - MR Egger", "All - Weighted median")
  dat.temp.res <- dat.temp[res,]
  dat.temp <- dat.temp[!res,]
  dat.temp <- transform(dat.temp, SNP = reorder(SNP, b) )
  dat <- rbind(dat.temp.res, dat.temp)
  dat$highlight <- ifelse(
    grepl("^All.+", dat$SNP),
    ifelse(
      dat$b > 0,
      "risk",
      "protective"
      ),
    "normal")
  mycolours <- c("risk" = "red", "protective" = "seagreen4", "normal" = "black")
  
  #construct the plot
  ggplot(data=dat,
         aes(x = SNP,
             y = b,
             ymin = Lower95,
             ymax = Upper95)
  ) +
    scale_color_manual("Status", values = mycolours) +
    geom_pointrange(
      aes(col = highlight,
          ymin = Lower95,
          ymax = Upper95),
      cex = 0.7
    ) +
    geom_hline(
               yintercept = 0,
               linetype = 2) +
    theme(plot.title = element_text(size = 20,
                                    face = "bold"),
          axis.text.y = element_text(size = 8,
                                     face = 'bold'),
          axis.ticks.y = element_blank(),
          axis.text.x = element_text(face="bold"),
          axis.title = element_text(size = 16,
                                    face="bold"),
          legend.position = "none"
    ) +
    xlab('SNP') +
    ylab("Beta Coefficient (95% Confidence Interval)") +
    coord_flip()
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
})

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