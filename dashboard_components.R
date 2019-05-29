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



#=================Notifications========================
newsNotification <- dropdownMenu(type = "notifications")
#======
#=================Header========================
header <- dashboardHeader(title = div(HTML('<span style="color:orange">i</span>PDGC'), "PD MR Research Portal", style = "color:white; /font-weight:bold"),
                          dropdownMenuOutput("errorNotification"),
                          titleWidth = 300)#, #see in server
                          #newsNotification)
#======
#=================About Tab========================
abouttab <- span(style = "background-color:white;",
  div(h3("International Parkinson's disease Genomics Consortium (IPDGC)", style = "color:orange")),
  div(h4("The Parkinson's Disease Mendelian Randomization Research Portal")
      #,img(src = "iPDGC-transparent-logo.png", height = 50, width = 50)
  ),
  div(h4("Version 2.01 beta", style = "color:black")),
  div(h5("Background", style = "color:orange")),
  div(p("Despite tremendous advances in understanding the genetic architecture underlying Parkinson disease (PD), there remains an as yet under-investigated component of risk, namely phenotypic traits that predispose or protect individuals to disease. The availability of large amounts of genome wide association data across varied traits affords the opportunity to investigate the relationship between myriad traits and PD. Mendelian randomization (MR), the gold standard for causality in genetic studies, is a statistical approach that uses genetic data in the form of SNPs to study if an exposure exerts a casual effect in an outcome. By using Two-sample MR randomization, we have created a new platform for the PD research community that can be used to assess evidence of causality where observational associations exist.", style = "font-size:100%; color:black")), 
  div(p("We explore causal relationships between 5,854 GWASes across multiple phenotypes versus the last three PD meta-analysis to date (Nalls et al., 2014, Chang et al., 2017, Nalls et al., 2019). This unbiased approach greatly simplifies the implementation of MR by a simple lookup. For each phenotype, the association with PD was assessed using the inverse variance weighted (IVW), MR Egger and Weighted Median methods, as well as a range of sensitivity analyses.Here, we have applied the following inclusion criteria; genome-wide associated SNPs with a minimum p-value less than 5.0x10-8; SNPs or their proxies (minimum R2 value = 0.8) present in both the exposure and outcome (PD) datasets; R2 clumping threshold = 0.001. See links below for detailed information on the principles and limitations of Mendelian randomization.", style = "font-size:100%; color:black")),  
  div(h5("Links of interest", style = "color:orange")),
  div(p(a(href="http://pdgenetics.org", "International Parkinson Disease Genomics Consortium -> IPDGC"), style = "font-size:100%")),
  div(p(a(href="http://app.mrbase.org/", "MR-base webapp -> Summarizes the GWAS data used as exposures and provides links for more detailed information on a per publication level."), style = "font-size:100%")),
  div(p(a(href="https://www.biorxiv.org/content/early/2016/12/16/078972", "Learn more about Mendelian randomization -> Hemani et al. 2016"), style = "font-size:100%")),
  div(p(a(href="https://www.biorxiv.org/content/10.1101/388165v3", "Largest PD meta-analysis to date -> Nalls et al. 2019, under review"), style = "font-size:100%")),
  div(p(a(href="https://www.ncbi.nlm.nih.gov/pubmed/28892059", "Chang et al. 2017"), style = "font-size:100%")),
  div(p(a(href="https://www.ncbi.nlm.nih.gov/pubmed/25064009", "Nalls et al. 2014"), style = "font-size:100%")),
  div(h5("Contact information", style = "color:orange")),
  div(
    div(span("Alastair Noyce", style = "font-weight:bold; font-size:110%"),
        div(a(href="a.noyce@qmul.ac.uk", "a.noyce@qmul.ac.uk")),
        div("Preventive Neurology Unit"),
        div("Wolfson Institute of Preventive Medicine"),
        div("Queen Mary University of London"),
        div("London"),
        div("EC1M 6BQ"),
        div("United Kingdom", style = "margin-bottom:10px"),
        div("Department of Molecular Neuroscience"),
        div("Institute of Neurology"),
        div("University College London"),
        div("London"),
        div("WC1N 1PJ"),
        div("United Kingdom"),
        class = "column",
        style = "margin-left:16px"),
    div(span("Sara Bandres-Ciga", style = "font-weight:bold; font-size:110%"),
        div(a(href="sara.bandresciga@nih.gov", "sara.bandresciga@nih.gov")),
        div("Molecular Genetics Section"),
        div("Laboratory of Neurogenetics"),
        div("National Institute on Aging"),
        div("National Institutes of Health"),
        div("Bethesda, Maryland 20892"),
        div("United States"),
        class = "column"),
    div(span("Jonggeol Jeff Kim", style = "font-weight:bold; font-size:110%"),
        div(a(href="jonggeol.kim@nih.gov", "jonggeol.kim@nih.gov")),
        div("Molecular Genetics Section"),
        div("Laboratory of Neurogenetics"),
        div("National Institute on Aging"),
        div("National Institutes of Health"),
        div("Bethesda, Maryland 20892"),
        div("United States"),
        class = "column"),
    class = "row")
)
#======
#============SIDEBAR============
#     Contains the portal tab, download tab, and the About tab
sidebar <- dashboardSidebar(
  sidebarMenu(
    selectInput(inputId = "outcome",
                label = HTML("Outcomes"),
                choices = c("Nalls et al. 2019 (clinically diagnosed cases)", "Chang et al. 2017", "Nalls et al. 2014")),
    selectizeInput("trait", "Exposures",
                   choices = gwasList, multiple = F
    ),
    # Advanced filter-icon
    div(id = "filter",
        actionLink('expfilter', '', icon = icon("filter"))
    ),
    # Conditional Panel for the icon
    conditionalPanel(
      condition = "input.expfilter%2 == 1",
      span(
        checkboxGroupInput("advfilter",
                           label = "",
                           inline = F,
                           choices = c("nSNP ≥ 10" = "nSNP",
                                       "nCase ≥ 250" = "nCase",
                                       "only UKBiobank" = "UKBB",
                                       "no UKBiobank" = "noUKBB")
        ),
        style = "text-indent: 10px")#,
      # multiple checkboxes should use merge() function
      #checkboxInput("nsample", "nSample ≥ 250")
    ),
    span(actionButton(inputId = "execute", "Analyze"), style = "position:relative; left:200px"),
    menuItemCustom("Plots", tabName = "portal", icon = icon("chart-bar"), idItem = "plotButton"),
    menuItem("MR Results/Tables", tabName = "table", icon = icon("table")),
    menuItem("Download Plot/Tables",
             icon = icon("download"),
             tabName = "dltab"
             #, menuSubItem("Plot", tabName = "plotDL", icon = icon("chart-bar")),
             #menuSubItem("Tables", tabName = "tableDL", icon = icon("table"))
    ),
    menuItem("About", tabName = "about", icon = icon("question")),
    # Style for the sidebarPanel 
    tags$style(type='text/css', 
               ".selectize-dropdown-content {
               max-height: 600px; 
               }") #initial deployment
  ),
  width = 300
)
#======
#============BODY============
body <- dashboardBody(
  useShinyjs(),
  tabItems(
    tabItem(tabName = "portal", introbox,
            fluidRow(
              column(width = 6,
                     forestplotbox),
              column(width = 6,
                     MiniResTableBox, funnelplotbox) )),
    tabItem(tabName = "table", KeyBox, ResTableBox, HetTableBox, PleTableBox, RevTableBox, PubTableBox, qkDLtableBox),
    tabItem(tabName = "dltab", FoPlotDLbox, FuPlotDLbox, tableDLbox),
    tabItem(tabName = "about", abouttab)
  ),
  div(class = "tab-content",
      id = "body_logo",
      a(img(src = "nia-logo-vector.png", style = "height:30px"), href = "https://www.nia.nih.gov/", id = "body_logo_image"),
      a(img(src = "qmul_logo.png", style = "height:50px"), href = "https://www.qmul.ac.uk/", id = "body_logo_image"),
      a(img(src = "IDPGC-logo-01.png", style = "height:50px"), href = "https://pdgenetics.org/", id = "body_logo_image")),
  # a(img(src = "iPDGC-transparent-logo.png", id = "body_logo_ipdgc"), href = "https://pdgenetics.org/"),
  # a(img(src = "nia-logo-vector.png", id = "body_logo_nia")),
  # a(img(src = "qmul_logo.png", id = "body_logo_qmul")),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "theme.css")
  )#,
  #introsplash
)#end of body
#======