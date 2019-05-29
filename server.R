server <- function(input, output, session) {
  '%ni%' <- Negate('%in%')
  #observe({updateTextInput(session, "dimension", value = 960)})
  observeEvent(input$tabBut,
               {
                 hide(id = "jumbo_custom")
               })
  #===Opening Notification===
  # observe({
  #   showNotification("PLEASE NOTE: Caution in the interpretation of these results should be taken when GWAS including < 250 individuals and < 10 instrumental variables are considered as exposures of interest",
  #                    duration = NULL,
  #                    type = "error")
  # })
  #===Advanced filtering option===
  #This is for the advanced filtering options
  observe({
    prevSelected <- input$trait
    updateSelectizeInput(session, inputId = "trait", label = "Exposures", choices = gwasList(), selected = ifelse(prevSelected %in% gwasList()$Exposure, prevSelected, ""))})
  gwasList <- reactive({
    #presnplist <- fread("www/dataResultsTable-res.csv")
    #presnplist2 <- unique(presnplist[,c(1,3,7)])
    #presnplist3 <- fread("www/exposures_MRbase.txt", sep = "\t")
    #pregwasList <- merge(presnplist2, presnplist3, by = "Exposure", all=T)
    #pregwasList <- pregwasList[complete.cases(pregwasList[,3]),]
    #midgwasList <- pregwasList[,c("Exposure", "no. of SNPs", "Outcome", "id", "ncase")]
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
    #midgwasList <- midgwasList[midgwasList$Outcome == input$outcome]
    midgwasList <- midgwasList[,"Exposure"]
    return(midgwasList)})
  source("figures+tables.R", local = T)
  source("download_functions.R", local = T)
  
  
  notificationItemCustom <- function (text, icon = shiny::icon("warning"), status = "success") 
  {
    #tagAssert(icon, type = "i")
    #validateStatus(status)
    icon <- tagAppendAttributes(icon, class = paste0("text-", 
                                                     status))
    tags$li(a(icon, text))
  }
  
  
  
  output$errorNotification <- renderMenu({preerrorNotification()})
  preerrorNotification <- reactive({
    req(input$execute)
    #snpBoolean <- isolate(subset(presnplist, Exposure == input$trait & Outcome == input$outcome, `no. of SNPs`) < 10)
    #sampleBoolean <- isolate(subset(presnplist3, Exposure == input$trait, `sample_size`) < 250)
    source("warning_message-ifelse.R", local = T)
    
  })
}