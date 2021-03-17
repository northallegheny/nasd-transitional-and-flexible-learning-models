
## Libraries ##

x<-c("httr", "jsonlite", "tidyverse", "lubridate", "ggplot2", "DBI", "odbc", "RODBC", "openssl")
lapply(x, require, character.only=T)

## Load Extract ##

df<-read.csv(#//NETWORK/PATH/TO/STUDENT/EXTRACT/FROM/SIS DATABASE 
             stringsAsFactors = F)

## Select Columns ##
df<-df%>%
  mutate(Student_Full_Name = paste(Student_First_Name, Student_Last_Name), # Create Full Name field for form URL parameter
         Current_Enrollment = ifelse(Student_Building=="40-NACA", 
                                     "NA Cyber Academy",                   # Create field to show current learning model in form
                                     paste(
                                       substr(Student_Building,4,nchar(Student_Building)),
                                       "in-person")),
         Home_Building = paste(substr(Home_School,4,nchar(Home_School)),
                               "in-person"))%>%                            # Field to indicate home building for cyber students
  select(Student_Number, # Pull out only student basic info and parent contact info
         Student_Last_Name,
         Student_First_Name,
         Student_Full_Name,
         Student_Email,
         Student_Building,
         Student_Grade,
         LV1_Type,
         LV1_First_Name,
         LV1_Last_Name,
         LV1_HomePhone,
         LV1_CellPhone,
         LV1_WorkPhone,
         LV1_EmergPhone,
         LV1_OtherPhone,
         LV1_Email,
         LV2_Type,
         LV2_First_Name,
         LV2_Last_Name,
         LV2_HomePhone,
         LV2_CellPhone,
         LV2_WorkPhone,
         LV2_EmergPhone,
         LV2_OtherPhone,
         LV2_Email,
         Non_Custodial_Type,
         Non_Custodial_First_Name,
         Non_Custodial_Last_Name,
         Non_Custodial_HomePhone,
         Non_Custodial_CellPhone,
         Non_Custodial_WorkPhone,
         Non_Custodial_EmergPhone,
         Non_Custodial_OtherPhone,
         Non_Custodial_Email,
         Current_Enrollment, 
         Home_Building)

## Paste Fields to Create Form Link with Selected URL Parameters ##
df$formLink<-NA
for(i in 1:nrow(df)){
  df$formLink[i] <- paste0("https://nasd.typeform.com/to/###FORM ID HERE#fname=",
                           URLencode(df$Student_First_Name[i], reserved = T),
                           "&lname=",
                           URLencode(df$Student_Last_Name[i], reserved = T),
                           "&building=",
                           URLencode(df$Student_Building[i], reserved = T),
                           "&grade=",
                           URLencode(df$Student_Grade[i], reserved = T),
                           "&studentid=",
                           URLencode(as.character(df$Student_Number[i]), reserved = T),
                           "&sentmail=",
                           URLencode(df$LV1_Email[i], reserved = T),
                           "&currenroll=",
                           URLencode(df$Current_Enrollment[i], reserved = T),
                           "&homebuilding=",
                           URLencode(df$Home_Building[i], reserved = T)
  )  
  
}

## Create GUID for each student ##
df$GUID<-sha1(as.character(df$Student_Number))

## Create Chosen ReBrandly URL and Tracking Column ##
df$shortLink<-NA
df$shortLinkActive<-0

##############################
### Create ReBrandly Links ###
##############################

# Connect to API and Get Existing Links #

creds<-read.csv("creds.csv", stringsAsFactors = F) # Read in API credentials from separate file
RBKey<-creds$Key[creds$Application=="Rebrandly"]
baseURL<-"https://api.rebrandly.com/v1/"
workspace<-## Workspace Unique ID

## Get existing Links for validation ##
currentLinksReq<-GET(paste0(baseURL, "links"),                  # API Request
                     add_headers("apikey"=RBKey,
                                 "workspace" = workspace))
currentLinks<-fromJSON(rawToChar(currentLinksReq$content))      # Parse Response
allLinks<-currentLinks%>%select(id,                             # Keep only necessary link properties
                                title, 
                                slashtag, 
                                destination, 
                                createdAt, 
                                shortUrl)

if(nrow(allLinks)==25){                                         # Repeat until all existing links in workspace are
  continue<-1                                                   # collected. Ignore if running for the first time.
  while(continue==1){
    nextLinksReq<-GET(paste0(baseURL, "links?last=",allLinks$id[nrow(allLinks)]),
                      add_headers("apikey"=RBKey,
                                  "workspace" = workspace))
    nextLinks<-fromJSON(rawToChar(nextLinksReq$content))%>%
      select(id, title, slashtag, destination, createdAt, shortUrl)
    allLinks<-rbind(allLinks, nextLinks)
    continue<-ifelse(nrow(nextLinks)==25,1,0)
    print(paste0(nrow(allLinks), " Rebrandly links collected"))
  }
}


## Pick up shortlinks for records that already had them made (should be most) ##  (ignore if running for the first time)
numWithLinks<-length(df$Student_Number %in% substr(allLinks$title, nchar(allLinks$title)-6, nchar(allLinks$title)-1))
for(i in which(is.na(df$shortLink))){
  if(df$Student_Number[i] %in% substr(allLinks$title, nchar(allLinks$title)-6, nchar(allLinks$title)-1)){
    df$shortLink[i]<-allLinks$shortUrl[substr(allLinks$title, 
                                              nchar(allLinks$title)-6, 
                                              nchar(allLinks$title)-1)==df$Student_Number[i]]
    print(paste0(i, " of ", numWithLinks))
  }
}


## Create Short Links for all Students who don't have them ##
counter<-1
numNeedingLinks<-length(which(is.na(df$shortLink) & !df$formLink %in% allLinks$destination))
for(i in 1:nrow(df)){                                                     # For each student...
  
  if(is.na(df$shortLink[i]) & !df$formLink[i] %in% allLinks$destination){ # If no link has been generated yet...
    linkPOST<-POST(paste0(baseURL, "links"),                              # Create a unique Rebrandly link
                   add_headers("apikey"= RBKey,
                               "content-type" = "application/json",
                               "workspace" = workspace),
                   body = toJSON(
                     list(
                       "destination" = df$formLink[i],                    # to the form URL created above
                       "slashtag" = paste0("ReRegQ4/", 
                                           df$GUID[i]),                   # and using the student GUID created above
                       "domain" = list(
                         "id" = ## Unique domain ID,
                         "fullName" = "nasd.school"
                       ),
                       "title" = paste("March 2021 Re-registration Link -",# Name the link after the student/form 
                                       df$Student_Full_Name[i],           # in Rebrandly workspace, using student ID
                                       paste0("(",                        # which will be helpful for auditing later.
                                              df$Student_Number[i],")"))
                     ), auto_unbox = T, pretty = T))
    link<-fromJSON(rawToChar(linkPOST$content))                           # Save the new link details, and...
    if(linkPOST$status_code=="200"){                                      # if the link was successfully made...
      df$shortLink[i]<-link$shortUrl                                      # store it in the student's row in our dataset.
      df$shortLinkActive[i]<-1
      print(paste(counter, " of ",                                        # Log our progress.
                  numNeedingLinks, 
                  df$Student_Full_Name[i], link$shortUrl, linkPOST$status_code))
    }else{
      print(paste(counter, " of ", 
                  numNeedingLinks, 
                  df$Student_Full_Name[i], link$shortUrl, linkPOST$status_code))
      break                                                               # Print details and break the loop if a link 
    }                                                                     # fails to be created.
    counter<-counter+1
    
  }
}

# Prepend HTTPS to rebranded links (especially necessary for access from mobile devices...)
df$shortLink<-paste0("https://", df$shortLink)


## Check for stored shortlinks file. Augment with new links if it exists; create it if not ##
# Note: this will not replace existing student links with new if, e.g., student info (like current school) has changed
if(!file.exists("2021Q4/allShortLinksQ4.csv")){
  write.csv(df%>%select(Student_Number, formLink, shortLink), "2021Q4/allShortLinksQ4.csv", row.names = F)
}else{
  asl<-read.csv("2021Q4/allShortLinksQ4.csv", stringsAsFactors = F)
  aslAdd<-dplyr::anti_join(df%>%select(Student_Number, formLink, shortLink), asl, by = c("Student_Number"))
  aslOut<-rbind(asl,aslAdd)
  write.csv(aslOut, "2021Q4/allShortLinksQ4.csv", row.names = F)
}



## Lines to delete links from selected workspace, if necessary. Requires "allLinks" object (generated above) as iterator ##
# for(i in 1:nrow(allLinks)){
#   linkDELETE<-DELETE(paste0(baseURL, "links/", allLinks$id[i]),
#                      add_headers("apikey"= RBKey,
#                                  "workspace" = "512d0d1b87144bc4acc231f09a4e5811"))#"512d0d1b87144bc4acc231f09a4e5811"))
#   print(paste0(i, " of ", nrow(allLinks), " - status:", linkDELETE$status_code))
# 
# }


## Write student list to CSV, optionally ##
#write.csv(df, "studentExtract.csv", row.names = F)

#############################################
### Re-Shape Data for Parent-wise sending ###
#############################################

# This section is helpful for creating a mailer file that will allow, e.g. Constant Contact or other mail-merge
# software to send each parent a single email including form links for all of their students in the district.

# Create Empty Output DF for Mailer List
sendDF<-data.frame(
  "RecipientEmail" = NA,
  "Student1_Name" = NA,
  "Student2_Name" = NA,
  "Student3_Name" = NA,
  "Student4_Name" = NA,
  "Student5_Name" = NA,
  "Student6_Name" = NA,
  "Student7_Name" = NA,
  "Student8_Name" = NA,
  "Student1_Link" = NA,
  "Student2_Link" = NA,
  "Student3_Link" = NA,
  "Student4_Link" = NA,
  "Student5_Link" = NA,
  "Student6_Link" = NA,
  "Student7_Link" = NA,
  "Student8_Link" = NA,
  "Student1_ID" = NA,
  "Student2_ID" = NA,
  "Student3_ID" = NA,
  "Student4_ID" = NA,
  "Student5_ID" = NA,
  "Student6_ID" = NA,
  "Student7_ID" = NA,
  "Student8_ID" = NA
)

# Grab existing responses from database (optional, and will need updates based on your response collection setup,
# but helpful for removing existing respondents from reminder emails)
ch<-odbcConnect(# Response Database as System DSN ODBC connection)
                                
                                  # Adjust results query based on District data collection setup 
allResponsesDF<-sqlQuery(ch, "
                          select 
                          respondentID AS studentID
                          from registrationformresults fr
                          where fr.formname = '2021-Q4-Reregistration' 
                                and fr.item = 'enrollmentChoice'")
odbcClose(ch)

noResponseDF<-df%>%filter(!Student_Number %in% allResponsesDF$studentID)

# IF ABOVE SECTION NOT RUN...#
#
# (e.g. when running this the first time), use this line to save existing
# dataset as 'noResponsesDF' object: noResponsesDF<-df

# Get unique list of parents, accounting for variation in capitalization and trailing whitespace
allParents<-unique(c(noResponseDF$LV1_Email%>%tolower()%>%trimws(), 
                     noResponseDF$LV2_Email%>%tolower()%>%trimws(), 
                     noResponseDF$Non_Custodial_Email%>%tolower()%>%trimws()))

# Drop Blank Values
allParents<-allParents[-which(allParents=="")]

# For each unique parent...
for(i in seq_along(allParents)){
  
  # Set Recipient Email
  sendDF[i,1]<-allParents[i]
  
  # Subset students associated with that parent
  tempDF<-noResponseDF%>%filter(LV1_Email%>%tolower()%>%trimws()==allParents[i] | 
                                  LV2_Email%>%tolower()%>%trimws()==allParents[i] | 
                                  Non_Custodial_Email%>%tolower()%>%trimws()==allParents[i])
  
  # For each student associated with that parent...
  for(j in seq_len(nrow(tempDF))){
    
    # Populate student name, link, and ID in output DF
    sendDF[[paste0("Student",j,"_Name")]][i]<-tempDF$Student_Full_Name[j]
    sendDF[[paste0("Student",j,"_Link")]][i]<-tempDF$shortLink[j]
    sendDF[[paste0("Student",j,"_ID")]][i]<-tempDF$Student_Number[j]
  }
  
  # Log progress
  print(paste0("Parent ", i, " of ", length(allParents)))
  
}

# Replace output DF NA values with blank
sendDF[is.na(sendDF)]<-""

# Save to disk
write.csv(sendDF, paste0("2021Q4/sendList_ReRegistrationQ4_", Sys.time()%>%str_replace_all(":", "_"), ".csv"), row.names = F)
