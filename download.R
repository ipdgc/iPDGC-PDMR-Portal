# download functionss
ForestDL.Function <- function() {
  dat.temp <- fread(paste0("www/dataForestPlot_PD_", sub(".*(20\\d\\d).*", "\\1", input$outcome), ".csv"))
  dat.temp <- dat.temp[dat.temp$Exposure==input$trait]# & dat.Temptemp$Outcome==input$outcome)
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
  
  # numCol <- round(isolate(window.size.slow()/200))
  # construct the plot
  ggplot(data = dat,
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
    # geom_errorbar(
    #   width = 0,
    #   aes(
    #     ymin = Lower95,
    #     ymax = Upper95,
    #     col = highlight
    #   ),
    #   cex = 1) +
    ggtitle(paste(input$trait, 'vs', input$outcome)) +
    theme(plot.title = forest.Plot.Title.DL.Key(),
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
}

FunnelDL.Function <- function(){
  dat.Temp <- fread(paste0("www/dataForestPlot_PD_", sub(".*(20\\d\\d).*", "\\1", input$outcome), ".csv"))
  dat <- dat.Temp[dat.Temp$Exposure==input$trait]# & dat.Temptemp$Outcome==input$outcome)
  
  dat$`1/SE` <- 1/as.numeric(dat$se)
  funnelplotlines <- dat[grepl("All", dat$SNP)]
  dat <- dat[!grepl("^All - ", dat$SNP)]
  plot(dat$b, dat$`1/SE`, xlab = expression('β'["IV"]), ylab = expression("1/SE"["IV"]), main = funnel.Plot.Title.DL.Key())
  abline(v = funnelplotlines[grepl("All - Inverse variance weighted", funnelplotlines$SNP)][1,c("b")], col="blue", lwd = 2)
  abline(v = funnelplotlines[grepl("All - MR Egger", funnelplotlines$SNP)][1,c("b")], col="#ff5e5e", lwd = 2)
  abline(v = funnelplotlines[grepl("All - Weighted median", funnelplotlines$SNP)][1,c("b")], col="#f2c71d", lwd = 2)
}

# DownloadHandlers for the forest plot
output$FoPlotDLTIFF <- downloadHandler(
  filename = function() {
    paste('PD_MR_ForestPlot-', Sys.Date(), '.tiff', sep='')
  },
  content = function(file) {
    ggsave(file, plot = ForestDL.Function(), device = tiff(), bg = input$fobg, width = input$FoDLplotw, height = input$FoDLploth, units = "in", compression = "none", dpi = 600, limitsize = F)
    # tiff(file, bg = input$fobg, width = input$FoDLplotw, height = input$FoDLploth, units = "in", compression = "none", res = 600)
    # ForestDL.Function()
    # dev.off()
  }
)

output$FoPlotDLPNG <- downloadHandler(
  filename = function() {
    paste('PD_MR_ForestPlot-', Sys.Date(), '.png', sep='')
  },
  content = function(file) {
    ggsave(file, plot = ForestDL.Function(), device = "png", bg = input$fobg, width = input$FoDLplotw, height = input$FoDLploth, units = "in", dpi = 600, limitsize = F)
    # tiff(file, bg = input$fobg, width = input$FoDLplotw, height = input$FoDLploth, units = "in", compression = "none", res = 600)
    # ForestDL.Function()
    # dev.off()
  }
)

output$FoPlotDLPDF <- downloadHandler(
  filename = function() {
    paste('PD_MR_ForestPlot-', Sys.Date(), '.pdf', sep='')
  },
  content = function(file) {
    ggsave(file, plot = ForestDL.Function(), bg = input$fobg, width = input$FoDLplotw, height = input$FoDLploth, device = cairo_pdf)
    # cairo_pdf(file, bg = input$fobg, width = input$FoDLplotw, height = input$FoDLploth)
    # ForestDL.Function()
    # dev.off()
  }
)

output$FoPlotDLSVG <- downloadHandler(
  filename = function() {
    paste('PD_MR_ForestPlot-', Sys.Date(), '.svg', sep='')
  },
  content = function(file) {
    ggsave(file, plot = ForestDL.Function(), bg = input$fobg, width = input$FoDLplotw, height = input$FoDLploth, device = svg)
    # svg(file, bg = input$fobg, width = input$FoDLplotw, height = input$FoDLploth)
    # ForestDL.Function()
    # dev.off()
  }
)

# DownloadHandlers for the funnel plot
output$FuPlotDLTIFF <- downloadHandler(
  filename = function() {
    paste('PD_MR_FunnelPlot-', Sys.Date(), '.tiff', sep='')
  },
  content = function(file) {
    tiff(file, bg = input$fubg, width = input$FuDLplotw, height = input$FuDLploth, units = "in", compression = "none", res = 600)
    FunnelDL.Function()
    dev.off()
  }
)

output$FuPlotDLPNG <- downloadHandler(
  filename = function() {
    paste('PD_MR_FunnelPlot-', Sys.Date(), '.png', sep='')
  },
  content = function(file) {
    png(file, bg = input$fubg, width = input$FuDLplotw, height = input$FuDLploth, units = "in", res = 600)
    FunnelDL.Function()
    dev.off()
  }
)

output$FuPlotDLPDF <- downloadHandler(
  filename = function() {
    paste('PD_MR_FunnelPlot-', Sys.Date(), '.pdf', sep='')
  },
  content = function(file) {
    cairo_pdf(file, bg = input$fubg, width = input$FuDLplotw, height = input$FuDLploth)
    FunnelDL.Function()
    dev.off()
  }
)

output$FuPlotDLSVG <- downloadHandler(
  filename = function() {
    paste('PD_MR_FunnelPlot-', Sys.Date(), '.svg', sep='')
  },
  content = function(file) {
    svg(file, bg = input$fubg, width = input$FuDLplotw, height = input$FuDLploth)
    FunnelDL.Function()
    dev.off()
  }
)

# DownloadHandlers for the tables
# Result table
output$ResultDLCSV <- downloadHandler(
  filename = function() {
    paste('PD_MR_tables-', Sys.Date(), '.csv', sep='')
  },
  content = function(file) {
    # choices
    if ('resDLcheck' %ni% input$ResultDLselect &&
        'hetDLcheck' %ni% input$ResultDLselect &&
        'pleDLcheck' %ni% input$ResultDLselect &&
        'revDLcheck' %ni% input$ResultDLselect &&
        'pubDLcheck' %ni% input$ResultDLselect) {
      stop('Please select at least one table')}
    resDL <- list()
    if ('resDLcheck' %in% input$ResultDLselect) {
      res <- fread("www/dataResultsTable-res.csv")
      res <- subset(res, res$Exposure==input$trait & res$Outcome==input$outcome)
      res$`no. of SNPs` <- as.character(res$`no. of SNPs`)
      resDL[["res"]] <- res
      }
    if ('hetDLcheck' %in% input$ResultDLselect) {
      het <- fread("www/dataResultsTable-het.csv")
      het <- subset(het, het$Exposure==input$trait & het$Outcome==input$outcome)
      resDL[["het"]] <- het
      }
    if ('pleDLcheck' %in% input$ResultDLselect) {
      ple <- fread("www/dataResultsTable-ple.csv")
      ple <- subset(ple, ple$Exposure==input$trait & ple$Outcome==input$outcome)
      resDL[["ple"]] <- ple
      }
    if ('pubDLcheck' %in% input$ResultDLselect) {
      pub <- fread("www/dataResultsTable-link.csv")
      pub <- subset(pub, pub$Exposure==input$trait)
      resDL[["pub"]] <- pub
      }
    if ('revDLcheck' %in% input$ResultDLselect){
      rev <- fread("www/dataResultsTable-RC.csv")
      rev <- subset(rev, Outcome==input$trait & Exposure==input$outcome)
      resDL[["rev"]] <- rev
      }
    #resDL <- list(tbl2, tbl2_2, tbl2_3, tbl2_5, tbl2_4)
    #resDL <- resDL[!sapply(resDL, is.null)]
    lapply(resDL, function(x) write.table( data.frame(x), file, append= T, row.names = F, quote = T, sep = ","))
  }
)


output$ResultDLTab <- downloadHandler(
  filename = function() {
    paste('PD_MR_tables-', Sys.Date(), '.txt', sep='')
  },
  content = function(file) {
    # choices
    if ('resDLcheck' %ni% input$ResultDLselect &&
        'hetDLcheck' %ni% input$ResultDLselect &&
        'pleDLcheck' %ni% input$ResultDLselect &&
        'revDLcheck' %ni% input$ResultDLselect &&
        'pubDLcheck' %ni% input$ResultDLselect) {
      stop('Please select at least one table')}
    resDL <- list()
    if ('resDLcheck' %in% input$ResultDLselect) {
      res <- fread("www/dataResultsTable-res.csv")
      res <- subset(res, res$Exposure==input$trait & res$Outcome==input$outcome)
      res$`no. of SNPs` <- as.character(res$`no. of SNPs`)
      resDL[["res"]] <- res
    }
    if ('hetDLcheck' %in% input$ResultDLselect) {
      het <- fread("www/dataResultsTable-het.csv")
      het <- subset(het, het$Exposure==input$trait & het$Outcome==input$outcome)
      resDL[["het"]] <- het
    }
    if ('pleDLcheck' %in% input$ResultDLselect) {
      ple <- fread("www/dataResultsTable-ple.csv")
      ple <- subset(ple, ple$Exposure==input$trait & ple$Outcome==input$outcome)
      resDL[["ple"]] <- ple
    }
    if ('pubDLcheck' %in% input$ResultDLselect) {
      pub <- fread("www/dataResultsTable-link.csv")
      pub <- subset(pub, pub$Exposure==input$trait)
      resDL[["pub"]] <- pub
    }
    if ('revDLcheck' %in% input$ResultDLselect){
      rev <- fread("www/dataResultsTable-RC.csv")
      rev <- subset(rev, Outcome==input$trait & Exposure==input$outcome)
      resDL[["rev"]] <- rev
    }
    lapply(resDL, function(x) write.table( data.frame(x), file, append= T, row.names = F, quote = T, sep = "\t"))
  }
)

output$ResultDLPDF <- downloadHandler(
  filename = function() {
    paste('PD_MR_tables-', Sys.Date(), '.tex', sep='')
  },
  content = function(file) {
    # choices
    if ('resDLcheck' %ni% input$ResultDLselect &&
        'hetDLcheck' %ni% input$ResultDLselect &&
        'pleDLcheck' %ni% input$ResultDLselect &&
        'revDLcheck' %ni% input$ResultDLselect &&
        'pubDLcheck' %ni% input$ResultDLselect) {
      stop('Please select at least one table')}
    resDL <- list()
    if ('resDLcheck' %in% input$ResultDLselect) {
      res <- fread("www/dataResultsTable-res.csv")
      res <- subset(res, res$Exposure==input$trait & res$Outcome==input$outcome)
      res$`no. of SNPs` <- as.character(res$`no. of SNPs`)
      resDL[["res"]] <- res
    }
    if ('hetDLcheck' %in% input$ResultDLselect) {
      het <- fread("www/dataResultsTable-het.csv")
      het <- subset(het, het$Exposure==input$trait & het$Outcome==input$outcome)
      resDL[["het"]] <- het
    }
    if ('pleDLcheck' %in% input$ResultDLselect) {
      ple <- fread("www/dataResultsTable-ple.csv")
      ple <- subset(ple, ple$Exposure==input$trait & ple$Outcome==input$outcome)
      resDL[["ple"]] <- ple
    }
    if ('pubDLcheck' %in% input$ResultDLselect) {
      pub <- fread("www/dataResultsTable-link.csv")
      pub <- subset(pub, pub$Exposure==input$trait)
      resDL[["pub"]] <- pub
    }
    if ('revDLcheck' %in% input$ResultDLselect){
      rev <- fread("www/dataResultsTable-RC.csv")
      rev <- subset(rev, Outcome==input$trait & Exposure==input$outcome)
      resDL[["rev"]] <- rev
    }
    lapply(resDL, function(x) print.xtable(xtable(data.frame(x)), file = file, append= T, include.rownames = F))
  }
)
# downloadHandlers for quick downloads
output$qkDLforestplot <- downloadHandler(
  filename = function() {
    paste('PD_MR_ForestPlot-', Sys.Date(), '.pdf', sep='')
  },
  content = function(file) {
    ggsave(file, plot = ForestDL.Function(), width = 8.5, height = 11, device = cairo_pdf)
  }
)

output$qkDLfunnelplot <- downloadHandler(
  filename = function() {
    paste('PD_MR_FunnelPlot-', Sys.Date(), '.pdf', sep='')
  },
  content = function(file) {
    cairo_pdf(file, width = 8.5, height = 11)
    FunnelDL.Function()
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