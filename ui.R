
ui = fluidPage(
  titlePanel("TestforDEP"),
  
  sidebarLayout(
    sidebarPanel(
      ##################################
      #control panel for upload data
      ##################################
      fileInput('file1', 'Choose CSV File',
                accept=c('text/csv', 
								 'text/comma-separated-values,text/plain', 
								 '.csv')),

      checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   ',', inline = TRUE),
      radioButtons('quote', 'Quote',
                   c(None='',
                     Double='"',
                     Single="'"),
                   '"', inline = TRUE),
      

      tags$hr(),
      ##################################
      #control panel for tests
      ##################################
      
      fluidRow(
        column(9, selectInput('test', 'Dependence test', width="100%",
                  choices = c(Pearson = "PEARSON", Kendall = "KENDALL", Spearman = "SPEARMAN", 
                   'DBEL (Vexler)' = "VEXLER", V = "V", TS2 = "TS2", MIC = "MIC", 
                    Hoeffding = "HOEFFD", Tel = "EL"), selected = "PEARSON")
        ),

       column(3, actionButton("info", "info"))
      ),
      tags$style(type='text/css', "#info { width:100%; margin-top: 25px;}"),
      
      singleton(
        tags$head(tags$script(src = "msg.js"))
      ),

      
      uiOutput("infoText"),
      
      uiOutput("p.opt"),
      
      uiOutput("ifMC"),
           
      uiOutput("BS.CI"),
      
      checkboxInput('rm.na', 'Remove missing data', TRUE),
      
      actionButton("compute", "Compute"), downloadButton('downloadData', 'Download'),
      
      helpText("Technical report: https://sphhp.buffalo.edu/content/dam/
sphhp/biostatistics/Documents/techreports/
UB-Biostatistics-TR1701.pdf")
      
  ),
    mainPanel(
      fluidRow( 
        column(6,plotOutput("scatter")),
        column(6,plotOutput("kplot"))
      ),

       tags$hr(),
       tableOutput("result")
    )
  )
)

