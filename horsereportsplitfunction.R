#Converts pdf into table of shows
horseRecord = function(horsereport){

  #read pdf
  pony<-pdf_text(horsereport)
  pony<-strsplit(pony,"\n")
  pony<-unlist(pony,recursive=TRUE)
  
  #pull horse name
  horseRow<-grep("Horse Name",pony)
  horse<-gsub("\\s\\(.*","\\1",pony[horseRow])
  horse<-gsub(".*\\:\\s","\\1",horse)

  #find division rows
  divRow<-grep("Division",pony)
  
  #find date rows
  dateRow<-grep("State[:punct:]",pony)
  
  #totals row
  totalRow<-grep("TOTALS[:punct:]",pony)
  
  #start dataframe
  df<-data.frame(Division = pony[divRow],Totals = pony[totalRow],CompDate="",stringsAsFactors = FALSE)
  
  #add date info
  i<-1
  j<-1
  while(i<=nrow(df) && j<=length(dateRow)){
    if((divRow[i]>dateRow[j]) && ((divRow[i] < dateRow[j+1])| j==length(dateRow) )){
      df[i,3]<-pony[dateRow[j]]
      i<-i+1
    } else {j<-j+1}
  }
  
  dt<-as.data.table(df)
  df<-data.frame(cbind(sapply(dt,trimws,which="both"),dt))
  #remove pony list
  rm(pony)
  horserecordoutput<-c(df,horse)
  return(horserecordoutput)
}

#Filters by division and date range
divisionReport<-function(horserecordoutput,startdate,enddate,division) {
    
  df<-as.data.frame(horserecordoutput[1:3],stringsAsFactors = FALSE)
  horse<-as.character(horserecordoutput[length(horserecordoutput)])
  div<-division
  
  #pull division
  division<-strsplit(df$Division,"\\:\\s")
  division<-data.frame(matrix(unlist(division),nrow=length(division),byrow=TRUE),stringsAsFactors = FALSE)
  division<-division[,2]
  
  #pull Totals
  totals<-substr(df$Totals,8,200)
  totals<-gsub("\\s","",totals,perl=TRUE)
  totals<-strsplit(totals,"\\/")
  totals<-data.frame(matrix(unlist(totals),nrow=length(totals),byrow=TRUE),stringsAsFactors = FALSE)
  totals<-totals[,1]
  
  #pull date
  compdate<-gsub("(\\d{1,2}\\/\\d{1,2}\\/\\d{4}).*","\\1",df$CompDate)
  compdate<-gsub(".*Comp","\\1",compdate)
  compdate<-gsub("\\:\\s","\\1",compdate)
  
  #combine
  df<-data.frame(Division=division,Totals=totals,CompDate=compdate,stringsAsFactors = FALSE)
  df<-df %>% mutate(Totals=as.numeric(Totals),CompDate=as.Date(CompDate,format="%m/%d/%Y"))
  
  print(div)
  
  #select division
  df<-df %>% filter(str_detect(Division,paste0("^",div))) %>% arrange(desc(Totals))
  
  #filter by start and end dates
  df<-df %>% filter(CompDate>startdate) %>% filter(CompDate<enddate)
  
  #sum top 15 shows
  if(nrow(df)>15){points<-sum(df$Totals[1:15])
  } else{points<-sum(df$Totals)}
  printoutput<-str_c("Horse: ",horse,"   Points: ",points)
  print(printoutput)
  
  # construct return
  outputlist<-c(df,horse,points)
  return(outputlist)
  }

#add show data to excel file
AddPonytoRank<-function(outputlist,excelfile,division){
  
  #convert variables
  div<-division
  df<-outputlist[1]
  horse<-outputlist[2]
  points<-outputlist[3]
  
  
  #Set up sheet and excel file
  wsheetrank<-paste(div,"RANK")
  wsheetpoints<-paste(div,"POINTS")
  
  
  #load workbook
  wb<-loadWorkbook(file=excelfile)
  
  #add pony to ranked list
  rank<-read.xlsx(excelfile,wsheetrank)
  rank<-rank[,2:3]
  rank<-rank %>% mutate(Points=as.numeric(Points))
  dat<-rank
  dat<-dat%>% add_row(Pony=horse,Points=points) %>% arrange(desc(Points))
  
  
    
  #remove duplicates
  dat<-dat[!duplicated(dat$Pony),]
  rownames(dat)<-seq(from=1,to=nrow(dat),by=1)
  
  #find rank
  rank<-grep(horse,dat$Pony)
  
  
  #Write to excel file
  writeData(wb,sheet=wsheetrank,x=dat,colNames=TRUE,rowNames = TRUE,borders="all")
  saveWorkbook(wb,excelfile,overwrite=TRUE)
  
  return(dat)
}

addPonytoShows<-function(outputlist,excelfile,division){
  
  #add shows to points sheet
  #add Year-Week and horse column, rearrange columns, arrange by year, week
  
  df<-df %>% mutate(Year=year(CompDate),Week=week(CompDate))
  df<-df %>% select(Year,Week,CompDate,Totals) %>% arrange(Year,Week)
  df<-df %>% setnames(old="Totals",new=gsub("\\s+","\\.",horse,perl=TRUE))
  
  ponypoints<-read.xlsx(excelfile,wsheetpoints,detectDates = TRUE)
  
  #Determine if already added, add to that column
   ponypoints<-full_join(ponypoints,df,by=c("Year","Week","CompDate"))
   j<-4
   while(j<=ncol(ponypoints)){
     if(grepl("\\.x",colnames(ponypoints[j]))){
       ponypoints<-subset(ponypoints,select=-j)
       colnames(ponypoints)<-sub("\\.y","",colnames(ponypoints))
       break
     } 
     j<-j+1
     
   }
   
   ponypoints<-ponypoints %>% arrange(Year,Week,CompDate)  
  
  #order columns alphabetically
  pp1<-ponypoints[,c(1,2,3)]
  pp2<-ponypoints[,c(4:ncol(ponypoints))]
  pp2<-pp2[,order(colnames(pp2))]
  ppalph<-cbind(pp1,pp2)
  
  
  #Write data to excel file
  writeData(wb,sheet=wsheetpoints,x=ppalph,colNames=TRUE,borders="all")
  saveWorkbook(wb,excelfile,overwrite=TRUE)
  
  mylist<-c("horse"=horse,"points"=points,"rank"=rank,"ppalph"=ppalph)
  return(mylist)
}