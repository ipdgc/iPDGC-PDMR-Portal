'%ni%' <- Negate('%in%')

#======-menuItemCustom allows you to set an id for the generated tab link (for use in javascript, etc.).
#======Do not use unless absolutely necessary---it lacks some input verification functions

menuItemCustom <- function (text, ..., icon = NULL, badgeLabel = NULL, badgeColor = "green",
                            tabName = NULL, href = NULL, newtab = TRUE, selected = NULL,
                            expandedName = as.character(gsub("[[:space:]]", "", text)),
                            startExpanded = FALSE, idItem = NULL)
{
  subItems <- list(...)
  if (!is.null(href) + !is.null(tabName) + (length(subItems) >
                                            0) != 1) {
    stop("Must have either href, tabName, or sub-items (contained in ...).")
  }
  if (!is.null(badgeLabel) && length(subItems) != 0) {
    stop("Can't have both badge and subItems")
  }
  isTabItem <- FALSE
  target <- NULL
  if (!is.null(tabName)) {
    isTabItem <- TRUE
    href <- paste0("#shiny-tab-", tabName)
  }
  else if (is.null(href)) {
    href <- "#"
  }
  else {
    if (newtab)
      target <- "_blank"
  }
  if (!is.null(badgeLabel)) {
    badgeTag <- tags$small(class = paste0("badge pull-right bg-",
                                          badgeColor), badgeLabel)
  }
  else {
    badgeTag <- NULL
  }
  if (length(subItems) == 0) {
    return(tags$li(a(href = href, `data-toggle` = if (isTabItem) "tab",
                     `data-value` = if (!is.null(tabName)) tabName, `data-start-selected` = if (isTRUE(selected)) 1 else NULL,
                     target = target, icon, span(text), badgeTag), id = idItem))
  }
  default <- if (startExpanded)
    expandedName
  else ""
  dataExpanded <- shiny::restoreInput(id = "sidebarItemExpanded",
                                      default) %OR% ""
  isExpanded <- nzchar(dataExpanded) && (dataExpanded == expandedName)
  tags$li(class = "treeview", id = idItem, a(href = href, icon, span(text),
                                             shiny::icon("angle-left", class = "pull-right")), do.call(tags$ul,
                                                                                                       c(class = paste0("treeview-menu", if (isExpanded) " menu-open" else ""),
                                                                                                         style = paste0("display: ", if (isExpanded) "block;" else "none;"),
                                                                                                         `data-expanded` = expandedName, subItems)))
}


# notification item for top right of the dashboard
notificationItemCustom <- function (text, icon = shiny::icon("warning"), status = "success") 
{
  icon <- tagAppendAttributes(icon, class = paste0("text-", 
                                                   status))
  tags$li(a(icon, text))
}

# download functionss
ForestDL.Function <- function() {
  dat.Temp <- fread(paste0("www/dataForestPlot_PD_", sub(".*(20\\d\\d).*", "\\1", input$outcome), ".csv"))
  dat <- dat.Temp[dat.Temp$Exposure==input$trait]# & dat.Temptemp$Outcome==input$outcome)
  
  #sort the data so that SNPs are in order and summary statistics are at the bottom
  # dat$index <- substr(dat$SNP,1,2)
  # dat <- dat[order(-as.numeric(as.factor(dat$index)), dat$b),]
  dat <- dat[order(-as.numeric(as.factor(dat$SNP))),]
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
}

FunnelDL.Function <- function(){
  dat.Temp <- fread(paste0("www/dataForestPlot_PD_", sub(".*(20\\d\\d).*", "\\1", input$outcome), ".csv"))
  dat <- dat.Temp[dat.Temp$Exposure==input$trait]# & dat.Temptemp$Outcome==input$outcome)
  
  dat$`1/SE` <- 1/as.numeric(dat$se)
  funnelplotlines <- dat[grepl("All", dat$SNP)]
  dat <- dat[!grepl("^All - ", dat$SNP)]
  plot(dat$b, dat$`1/SE`, xlab = expression('β'["IV"]), ylab = expression("1/SE"["IV"]))
  abline(v = funnelplotlines[grepl("All - Inverse variance weighted", funnelplotlines$SNP)][1,c("b")], col="blue", lwd = 2)
  abline(v = funnelplotlines[grepl("All - MR Egger", funnelplotlines$SNP)][1,c("b")], col="#ff5e5e", lwd = 2)
  abline(v = funnelplotlines[grepl("All - Weighted median", funnelplotlines$SNP)][1,c("b")], col="#f2c71d", lwd = 2)
}