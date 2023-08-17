options(shiny.maxRequestSize=100*1024^2) 
# library(org.Hs.eg.db)
library(shiny)
library(clusterProfiler)
library(shinymanager)
library(annotationTools)
library(dplyr)
library("shinybusy")
library(DT)
options(repos = BiocManager::repositories())
credentials <- data.frame(user = c("karen", "1a"),password = c("karen123", "1a"), # password will automatically be hashed stringsAsFactors = FALSE)
server <- function(input, output,session) {
  res_auth <- secure_server(check_credentials = check_credentials(credentials))
  observe({
    x <- input$dataset
    b<-NA
    if(!is.null(x)){
      if(x=="U133"){    db<-head(read.csv('./HG-U133_Plus_2.na36.annot.csv',colClasses='character',comment.char='#'))
        b<-colnames(db)[c(1,5,11,14,15,18,19)]}
      else if(x=="clariD"){  db<-head(read.csv('./Clariom_D_Human.r1.na36.hg38.a1.transcript.csv',colClasses='character',comment.char='#'))
        b<-colnames(db)[c(1,2,8,9,11)]
        ## La asignación de genes tiene 5 genes ID
      }
      else if(x=="miRNA30"){
db<-head(read.csv('./miRNA-3_0-st-v1.annotations.20140513.csv',colClasses='character',comment.char='#'))
        b<-colnames(db)[c(1,2,3,4)]}}
    updateCheckboxGroupInput(session, "Options",label = paste("Checkboxgroup label", length(b)),choices = b,selected = b)})  annotation_db<-renderText({head(read.csv(input$file,colClasses='character',comment.char='#'))})
    output$db_selected <- renderText({ 
    if(input$dataset=="U133")
db<-read.csv('./HG-U133_Plus_2.na36.annot.csv',colClasses='character',comment.char='#')
    if(input$dataset=="miRNA30")
db<-read.csv('./miRNA-3_0-st-v1.annotations.20140513.csv',colClasses='character',comment.char='#')
    if(input$dataset=="clariD")
db<-read.csv('./Clariom_D_Human.r1.na36.hg38.a1.transcript.csv',colClasses='character',comment.char='#')
        print(paste0("Number of probes in db: ",dim(db)[1]));}) 
  output$genes_text <- renderText({ 
    if(input$genes_text=="introduce probeset names")
      return("Number introduced: 0")
    b<-gsub(" ","",input$genes_text)
    b<-strsplit(b,",")[[1]]
    print(paste0("Number introduced: ",length(b)));})
   output$genes_file <- renderText({ 
    inFile <- input$file
    if (is.null(inFile))
      return(print(paste0("Number introduced 0: ")))
    file_in_num_genes<-read.csv(inFile$datapath,header = F)
    print(paste0("Number introduced: ",dim(file_in_num_genes)[1]));})
  output$info<- renderDataTable({
    # annotation_db<-db<-read.csv('./HG-U133_Plus_2.na36.annot.csv',colClasses='character',comment.char='#')
    if(is.null(input$file)&&input$genes_text=="")
    {return("No file")}
    if(!is.null(input$file)){
      inFile <- input$file
      inFile<-read.csv(inFile$datapath,header=F)
      b<-as.list(inFile)[[1]]}
    else if(input$genes_text!=""){
      b<-gsub(" ","",input$genes_text)
      b<-strsplit(b,",")[[1]]}
    if(input$dataset=="U133")
      db<-read.csv('./HG-U133_Plus_2.na36.annot.csv',colClasses='character',comment.char='#')
    db<-db[,input$Options]
    if(input$dataset=="miRNA30") db<-read.csv('./miRNA-3_0-st-v1.annotations.20140513.csv',colClasses='character',comment.char='#')
    db<-db[,input$Options]
    if(input$dataset=="clariD")
db<-read.csv('./Clariom_D_Human.r1.na36.hg38.a1.transcript.csv',colClasses='character',comment.char='#')
    db<-db[,input$Options]
    db<- db%>% filter(Probe.Set.ID%in%b)
    data.frame(db)}) 
  observeEvent(input$update, {
    newtab <- switch(input$tabs, "Use" = "Results")
    updateTabItems(session, "tabs", newtab)})
  output$downloadData <- downloadHandler(
    filename = function() {paste(input$dataset,".csv", sep = "") },
    content = function(file) {write.csv(db, file, row.names = FALSE)},
    contentType = ".csv")
  output$results <-DT::renderDataTable({
    if(input$sumbit!=0){    
      show_modal_spinner()
      example1<-get_multimir(mirna = input$miRNA_text, summary = TRUE,table = input$options)
      remove_modal_spinner()
      datatable(example1@data)}})
}
server
