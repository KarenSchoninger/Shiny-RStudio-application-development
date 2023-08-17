UI. R:
##Global options
options(spinner.color="#60b1d1")
options(shiny.maxRequestSize = 100*1024^2)
options(expressions = 5000)

#packages
library(shiny)
library(shinydashboard)
library(shinymanager)

#Header
header <- dashboardHeader(title = "GeneTranslateR") 

#Sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(id="tabs",
              menuItem("AboutGeneTranslateR",tabName = "AboutGeneTranslateR",icon=icon("server"),badgeLabel = "Info!",badgeColor = "light-blue"),
              menuItem("Use",tabName = "Use",icon = icon("poll-h
"), badgeLabel="How?",badgeColor="light-blue"),
              menuItem("Results",tabName = "Results",icon= icon("spell-check"),badgeLabel = "Table",badgeColor = "light-blue"),
              menuItem("multiMiR",tabName = "multiMiR"),
              tags$img(src="./logo.png", height=100, width=150,aling="center"),
              tags$style(HTML("@import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700"))))

#Body
body <- dashboardBody(
  tabItems(     
    tabItem(tabName = "AboutGeneTranslateR",
            h1("About GeneTranslateR",align = "center",style="font-family: Calibri Light"),
            fluidRow(
              h4("The existence of various methods and rules for assigning identifiers to genes creates problems when analyzing genetic data because not all databases or online platforms recognize the same identifiers. There are committees, institutions and rules that determine the different IDs for genes, such as the",strong("HUGO Genetic Nomenclature Committee"),", the", strong("Consensus CDS Project (CCDS)"),"and",strong(" SwissProt / UniProt"),". A common case occurs with",strong("Affymetrix arrays"),", because their identifiers are not usually easy to recognize by external programs."),
              h4("For example, the Ensembl platform generates IDs made up of five parts: ENS, species, type of object, identifier and version. A common case is the human BRCA2 gene, a gene associated with DNA repair and which can be found with the following IDs."),
              tags$img(src="./tabla.PNG", height=300, width=600,aling="center"))),
    tabItem(tabName="Use",
            h1("GeneTranslateR",style="font-family: Calibri Light",align = "center"),
            fluidRow(
              h4("Enter the identifier of the gene or genes you want to convert and select from the different available options:"),
              box(title = "Input Data",solidHeader = T,status = "primary", width = 12,
                  fluidRow( box(title = "ID File",
                          solidHeader=T, status="info",width=6,
                        ### selección del archivo con los nombres de affy
                        fileInput("file","Select file", multiple = TRUE,
          placeholder = "Select file"),
         box(textOutput("genes_file"),background = "green")),
                    box(title = "Affymetrix ID Text",
                        solidHeader = T, status = "info",width = 6,
                        textInput("genes_text", "Genes",""),
                        box(textOutput("genes_text"),background = "green"))),
                  fluidRow(
                    box(title = "Select type of Affymetrix File",
                        solidHeader = T, status = "info",width = 6,
                        selectInput(inputId = "dataset",selected = "U133",label = "Choose a dataset:",choices = c("Human Genome U133 Plus 2.0 Array"="U133","Human Clariom D assays"="clariD","miRNA 3.0"="miRNA30")),
                    box(textOutput("db_selected"),background = "green")),
                    box(title = "Select database",
                        solidHeader = T, status="info",width = 6,
                        checkboxGroupInput("Options","Databases:",selected = c("symbol","entrez"),choices = list("SYMBOL"="symbol","UNIPROT"="uniprot","ENSEMBL","ENTREZID"="entrez","ALIAS","GO","UNIGENE"))),
 ),actionButton("update", "Go!", icon =icon("play"))))),
    tabItem(tabName = "Results",
            h1("Results",style="font-family: Calibri Light",align = "center"),
            fluidRow(h4("As a result, a table is obtained containing the different identities of the gene. It is a dynamic table in which there is a search button to find the specific gene that interests us. This content is downloadable."),
              box(title = "Results",solidHeader = T, status = "primary", width = 12,
                   h4("Results table"),
                  dataTableOutput("info"), 
                  downloadButton("downloadData", "Download",style = "background-color:#EDB600; color:#000000; border-color:#BEBEBE;
 border-style:dashed; border-width:1px;border-radius:70%;font-size:18px;")))),
    tabItem(tabName = "multiMiR",
            h1("multiMiR",style="font-family: Calibri Light",align = "center"),
            fluidRow(
              h4("Enter your miRNA of interest to obtain information about target genes, drugs, and related diseases."),
              box(title="Input data and select options",solidHeader = T, status = "primary", width = 12, textInput("miRNA_text", "Inster miRNA list", "hsa-miR-18a-3p") p("or"),
                 fileInput("miRNA_file", "Inster file with miRNAs (.csv)", accept = ".csv"),
             box(radioButtons("options", "Options of miRNA targets:",
                               c("Validated targets" = "validated","Predicted targets" = "predicted","Disease or drug" = "disease.drug","All information" ="all")),
                )), ),
            fluidRow(
              actionButton("sumbit", "Get results",class = "btn-success", style = "background-color:#EDB600; color:#000000; border-color:#BEBEBE;border-style:dashed; border-width:1px;  border-radius:70%;font-size:18px;") ),
            fluidRow(h4("Results"), box(width=14, status = "warning", DT::dataTableOutput("results"),style = "height:500px; overflow-y: scroll;overflow-x: scroll;")) )))

#######End UI################################  
ui <- dashboardPage(header,sidebar, body, skin = "green")
ui <- secure_app(ui)






