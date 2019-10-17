window.size <- reactive(input$dimension)
window.size.slow <- debounce(window.size, 2000)
#forest.reactive <- reactiveValues() 

#===Advanced filtering option===
# This is for the advanced filtering options
observe({
  prevSelected <- input$trait
  updateSelectizeInput(session, inputId = "trait", label = "Exposures", choices = gwasList(), selected = ifelse(prevSelected %in% gwasList()$Exposure, prevSelected, ""))
})

observeEvent(input$tabBut,
             hide(id = "jumbo_custom")
)

gwasList <- reactive({
  midgwasList <- fread("exposureList.txt")
  midgwasList <- midgwasList[midgwasList$Outcome == input$outcome]
  if ('nSNP' %in% input$advfilter){
    midgwasList <- midgwasList[midgwasList$`no. of SNPs` >= 10]}
  if ('nCase' %in% input$advfilter){
    midgwasList <- midgwasList[midgwasList$ncase >= 250]}
  if ('UKBB' %in% input$advfilter){
    midgwasList <- midgwasList[grepl("UKB", midgwasList$id)]}
  if ('noUKBB' %in% input$advfilter){
    midgwasList <- midgwasList[grepl("^[^U]", midgwasList$id)]
  }
  midgwasList[,"Exposure"]
})

#read in data
forest.reactive <- eventReactive(input$execute,
                                 {
                                   req(window.size.slow())
                                   dat.Temp <- isolate(fread(paste0("www/dataForestPlot_PD_", sub(".*(20\\d\\d).*", "\\1", input$outcome), ".csv")))
                                   #select data for the selected trait
                                   dat <- isolate(dat.Temp[dat.Temp$Exposure==input$trait])
                                   
                                   #stops plotting if no data
                                   req(nrow(dat) > 0)
                                   
                                   # #sort the data so that SNPs are in order and summary statistics are at the bottom
                                   # dat <- dat[order(-as.numeric(dat$b))]
                                   # res <- as.character(dat$SNP) %in% c("All - Inverse variance weighted", "All - MR Egger", "All - Weighted median")
                                   # dat <- rbind(dat[!res,], dat[res,])
                                   # #dat <- dat[order(-as.numeric(as.factor(dat$SNP))),]
                                   
                                   return(dat)
                                 }
)

#generate the tables
ResTbl.reactive <- eventReactive(input$execute,
                                 {
                                   tbl1 <- fread("www/dataResultsTable-res.csv")
                                   tbl2 <- isolate(tbl1[tbl1$Exposure==input$trait & tbl1$Outcome==input$outcome])
                                   tbl2$`no. of SNPs` <- as.character(tbl2$`no. of SNPs`)
                                   
                                   #stops call if no data
                                   req(nrow(tbl2) >0 )
                                   
                                   return(tbl2)
                                 })

HetTbl.reactive <- eventReactive(input$execute, {
  tbl1_2 <- fread("www/dataResultsTable-het.csv") 
  tbl2_2 <- isolate(subset(tbl1_2, tbl1_2$Exposure==input$trait & tbl1_2$Outcome==input$outcome))
  
  #stops call if no data
  req(nrow(tbl2_2) >0 )
  
  return(tbl2_2)
})

PleTbl.reactive <- eventReactive(input$execute, {
  tbl1_3 <- fread("www/dataResultsTable-ple.csv")  
  tbl2_3 <- isolate(subset(tbl1_3, tbl1_3$Exposure==input$trait & tbl1_3$Outcome==input$outcome))
  
  #stops call if no data
  req(nrow(tbl2_3) >0 )
  return(tbl2_3)
})

PubTbl.reactive <- eventReactive(input$execute, {
  tbl1_4 <- fread("www/dataResultsTable-link.csv")
  tbl2_4 <- isolate(subset(tbl1_4, tbl1_4$Exposure==input$trait))
  skip <- ifelse(isolate(tbl2_4$PubmedID == 0), T, F)
  req(skip == F)
  tbl2_4$PubmedID <- paste0("<a href='https://www.ncbi.nlm.nih.gov/pubmed/",tbl2_4$PubmedID,"'>",tbl2_4$PubmedID,"</a>")
  
  #stops call if no data
  req(nrow(tbl2_4) >0 )
  
  return(tbl2_4)
})

RevTbl.reactive <- eventReactive(input$execute, {
  tbl1_5 <- fread("www/dataResultsTable-RC.csv") 
  tbl2_5 <- isolate(subset(tbl1_5, Outcome==input$trait & Exposure==input$outcome))
  
  #stops call if no data
  req(nrow(tbl2_5) >0 )
  
  return(tbl2_5)
})

preerrorNotification <- eventReactive(input$execute,
                                      {
                                        `if`(input$trait %in% warning.List$Exposure && input$outcome %in% warning.List$Outcome,
                                             {
                                               text <- switch(warning.List[warning.List$Exposure == input$trait & warning.List$Outcome == input$outcome]$warning,
                                                              "snp" = "Less than 10 instrumental variables detected.",
                                                              "snp+na" = c("Less than 10 instrumental variables detected.", "Number of case-participants in the exposure study is missing. If the exposure is a continuous trait, please ignore."),
                                                              "snp+lowcase" = c("Less than 10 instrumental variables detected.", "Less than 250 participants in the exposure study."),
                                                              "na" = "Number of case-participants in the exposure study is missing. If the selected exposure is a continuous trait, please ignore this warning.",
                                                              "lowcase" = "Less than 250 case-participants in the exposure study."
                                               )
                                               
                                               msgs <- lapply(text, function(x) {
                                                 notificationItemCustom(div(x, id = "warningNotification"),
                                                                        icon = icon("exclamation-triangle"),
                                                                        status = "warning")}
                                               )
                                               
                                               dropdownMenu(headerText = "We advise caution when interpreting the results due to the following:",
                                                            .list = msgs,
                                                            type = "notifications"
                                               ) 
                                             },
                                             {
                                               notificationItem("", icon = icon("check"))
                                             })
                                      }
                                      )

forest.Plot.Title.DL.Key <- reactive({             
  if (input$forestTitle) {
    element_text(size = 16,
                 face = "bold")
  } else {
    element_blank()
  }
}
)

funnel.Plot.Title.DL.Key <- reactive({
  if (input$funnelTitle) {
    paste(input$trait, 'vs', input$outcome)
  } else {
    NULL
  }
})