library(httr)
library(jsonlite)
library(shiny)
library(data.table)
library(dplyr)
library(DT)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(aws.s3)
options(stringsAsFactors = FALSE)

baseurl  <- "https://api.fulcrumapp.com"
path <- "/api/v2/records.json?form_id="
WHTMformid <- "ffa6c729-9e45-4e2d-b69e-64118549554c"
WSTAformid <- "3ca7df0d-f808-40dc-991a-3d396eedaacb"
DQTMformid <- "0046e79b-ef91-4b37-99b9-d6340e28094c"
BPLAXApiToken <- ""
AMNXApiToken <- ""
logfname <- "RUH-UF-download-log.csv"

# amazon
s3BucketName <- "logfiles-shinyapp"
Sys.setenv("AWS_ACCESS_KEY_ID" = "",
           "AWS_SECRET_ACCESS_KEY" = "","AWS_DEFAULT_REGION" = "us-east-1")

# report counter
writetolog <- function(newcount, newsessionid)
{
  s3load(object = logfname, bucket = s3BucketName)
  df <- data.frame(count = newcount, sessionid = newsessionid)
  RUHOFdownloadlog <- rbind(RUHOFdownloadlog,df)
  s3save(RUHOFdownloadlog, bucket = s3BucketName, object = logfname, accelerate=T)
}

getcounts <- function()
{
  if (exists("RUHOFdownloadlog"))
  {
    rlst <-
      list(
        count = sum(RUHOFdownloadlog$count),
        sessioncount = length(unique(RUHOFdownloadlog$sessionid))
      )
    return(rlst)
  }
  else{
    s3load(object = logfname, bucket = s3BucketName)
    rlst <-
      list(
        count = sum(RUHOFdownloadlog$count),
        sessioncount = length(unique(RUHOFdownloadlog$sessionid))
      )
    return(rlst)
  }
}
#TODO:Write a partail get funcrion for the lookup
checkData<-{
  a<-curlGetHeaders(paste0(baseurl, path, WHTMformid))[4]
  b<-curlGetHeaders(paste0(baseurl, path, DQTMformid))[4]
  c<-curlGetHeaders(paste0(baseurl, path, WSTAformid))[4]
}
# function to load data
## WH TM
loadData <- function(){
  
  
  #WH
  req <-
    GET(
      url = paste0(baseurl, path, WHTMformid),
      add_headers(`X-ApiToken` = BPLAXApiToken),
      accept_json()
    )
  req <- fromJSON(content(req, type = "text", encoding = "utf-8"),flatten = T,simplifyVector=T)
  WH <- as.data.frame(req$records)
  WH$Project <- "Wadi Hanifah"
  #WS
  req <-
    GET(
      url = paste0(baseurl, path, WSTAformid),
      add_headers(`X-ApiToken` = AMNXApiToken),
      accept_json()
    )
  req <- fromJSON(content(req, type = "text", encoding = "utf-8"),flatten = T,simplifyVector=T)
  WS <- as.data.frame(req$records)
  WS$Project <- "Wadi As Sulai"
  #DQ
  req <-
    GET(
      url = paste0(baseurl, path, DQTMformid),
      add_headers(`X-ApiToken` = BPLAXApiToken),
      accept_json()
    )
  req <- fromJSON(content(req, type = "text", encoding = "utf-8"),flatten = T,simplifyVector=T)
  DQ <- as.data.frame(req$records)
  DQ$Project <- "DQ"
  
  WH <- WH[,names(WH) %in% names(DQ)]
  WS <- WS[,names(WS) %in% names(WH)]
  DQ <- DQ[,names(DQ) %in% names(WH)]
  
  a<-rbind(WH, WS)
  a<- rbind(a,DQ)

  # Species
  a$Species <-
    ifelse(
      test = (a$form_values.3fee.choice_values=="list()"),
      a$form_values.3fee.other_values,
      a$form_values.3fee.choice_values
    )
  a$Species<-paste(a$Species)
  a$Species<-gsub("NULL", NA, a$Species)
  a$Species<-iconv(a$Species, "latin1", "ASCII", sub="")
  a$Species<-trimws(gsub(" - ", "", a$Species, perl=TRUE)) 
  
  #spread
  names(a)[names(a) == 'form_values.b96d'] <- 'Spread'
  a$Spread<-as.numeric(a$Spread)
  #height
  names(a)[names(a) == 'form_values.009b'] <- 'Height'
  a$Height<-as.numeric(a$Height)
  #Health
  names(a)[names(a) == 'form_values.f6ef.choice_values'] <- 'Health'
  a$Health<-paste(a$Health)
  a$Health<-iconv(a$Health, "latin1", "ASCII", sub="")
  a$Health<-trimws(gsub(" - ", "", a$Health, perl=TRUE)) 
  a<-a[!is.na(a$Spread), ]
  # a$lng<-a$longitude
  # a$lat<-a$latitude
  a <- a[,c("id","latitude","longitude","Species","Height","Spread","Health","Project","updated_at")]
  return(a)
}

