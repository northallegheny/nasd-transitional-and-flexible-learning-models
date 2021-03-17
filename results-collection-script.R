

## Libraries ##

x<-c("httr", "jsonlite", "tidyverse", "lubridate", "ggplot2","RODBC", "DBI", "odbc")
lapply(x, require, character.only=T)

## Typeform API Auth ##
baseURL<-"https://api.typeform.com/"

creds<-read.csv("creds.csv", stringsAsFactors = F) # API Key/Personal Access Token stored in separate file
PAT<-creds$Key[creds$Application=="Typeform"]

## GET Responses ##
responsesREQ<-GET(paste0(baseURL, "/forms/FORM_ID_HERE/responses?page_size=1000"),
                  add_headers("Authorization" = 
                                paste0("Bearer ", PAT)))
responses<-fromJSON(rawToChar(responsesREQ$content))

## Plunk essential response details into a data frame
responsesDF<-matrix(nrow = length(responses$items$answers), ncol = 3)
colnames(responsesDF)<-c("studentID", "enrollmentChoice", "submitTime")
responsesDF<-as.data.frame(responsesDF)
responsesDF$submitTime<-as_datetime(responsesDF$submitTime)
for(i in 1:length(responses$items$answers)){                       # These lines will need updates based on form structure
  responsesDF$studentID[i]<-responses$items$hidden$studentid[i]
  responsesDF$enrollmentChoice[i]<-responses$items$answers[i][[1]]$choice$label[5]
  responsesDF$submitTime[i]<-as_datetime(responses$items$submitted_at[i])-14400
}
#View(responsesDF)

allResponsesDF<-responsesDF


#################################
### Write Results to Database ###
#################################

## Get existing results from DB ##
ch<-odbcConnect(
  # ODBC Connection (usually a System DSN) to district form results database
)

storedResults<-sqlQuery(ch, "SELECT * FROM RegistrationFormResults 
                             WHERE formName = '2021-Q4-Reregistration'")

## Re-shape survey data
allResponsesDF<-responsesDF%>%
  pivot_longer(c(enrollmentChoice), 
               names_to = "item", 
               values_to = "response")

allResponsesDF$formName<-"2021-Q4-Reregistration"
allResponsesDF$formID<-"FORM_ID_HERE"
allResponsesDF$respondentType<-"student"
allResponsesDF$respondentID<-allResponsesDF$studentID
allResponsesDF$submitDateTime<-allResponsesDF$submitTime
allResponsesDF$createdOn<-Sys.time()
allResponsesDF$createdBy<-"scheduledUploadJob"

## Create Key Columns for anti-join. This is necessary to avoid inserting duplicate responses ##
allResponsesDF$key<-paste0(allResponsesDF$respondentID, 
                           allResponsesDF$item,
                           allResponsesDF$response,
                           allResponsesDF$submitDateTime)
storedResults$key<-paste0(storedResults$respondentID,
                          storedResults$item,
                          storedResults$response,
                          storedResults$submitDateTime)

newResults<-anti_join(allResponsesDF, storedResults, by = "key")

## Add to DB ##
maxID<-sqlQuery(ch, "SELECT max(resultID) FROM RegistrationFormResults")%>%as.integer()
baseID<-ifelse(maxID>0,maxID,0)

for(i in 1:nrow(newResults)){
  tempquery<-sqlQuery(ch, paste0("INSERT INTO RegistrationFormResults(resultID, 
                      formName, 
                      formID,
                      respondentType,
                      respondentID,
                      item,
                      response,
                      submitDateTime, 
                      createdOn,
                      createdBy)
           VALUES(",
                                 baseID+i, ", '",
                                 newResults$formName[i], "', '",
                                 newResults$formID[i], "', '",
                                 newResults$respondentType[i], "', '",
                                 newResults$respondentID[i], "', '",
                                 newResults$item[i], "', '",
                                 newResults$response[i], "', '",
                                 newResults$submitDateTime[i], "', '",
                                 newResults$createdOn[i], "', '",
                                 newResults$createdBy[i], "')"))
  print(paste0("Attempted insert for row ", i, " of ", nrow(newResults), " - response: ", tempquery))
}


RODBC::odbcClose(ch)
