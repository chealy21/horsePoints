calcpoints<-function(rating,diventries,classicentries,model,confirmation,hunter1,hunter2,handy,us,classic,championship){
  
  #Create Placing dataframe
  placings<-data.frame("Class"=c("Model","Confirmation","Hunter1","Hunter2","Handy","US","Classic","Championship"),
                       "Place"=c(model,confirmation,hunter1,hunter2,handy,us,classic,championship))
  
  
  
  #Point calculation tables
  regpoints<-data.frame("Place"=c(1,2,3,4,5,6,7,8),"AA"=c(25,20,16,12,11,10,9,8),"A"=c(20,15,12,9,8,7,6,5),"B"=c(15,10,8,6,5,4,3,2),"C"=c(10,6,4,3,2,1.5,1,0.5))
  multiplier<-data.frame("Place"=c("Champion","Reserve","Model","Classic"),"Multiplier"=c(2,1.2,0.5,1.75))
  
  
  #Create dataframe
  ponypoints<-data.frame("Class"=c("Model","Confirmation","Hunter1","Hunter2","Handy","US","Classic","Championship"),"Points"=as.numeric(0))
  
  ponypoints$Entries<-diventries
  ponypoints$Entries[which(ponypoints$Class=="Classic")]<-classicentries
  
  
  
  #Pull appropriate points
  ratingpoints<-cbind(regpoints$Placing,regpoints[,colnames(regpoints)==rating])
  
  
  #combine placings and points
  dt<-as.data.table(ponypoints)
  
  dtplace<-as.data.table(placings)
  
  
  df<-full_join(dt,dtplace,by="Class")
  
  
  
  df<-as.data.frame(df)
  
  
  
  df<-df %>% mutate(Class=as.factor(Class),Points=as.numeric(Points),Entries=as.numeric(Entries),Place=as.numeric(as.character(Place)))
  
  
  row.names(df)<-c("Model","Confirmation","Hunter1","Hunter2","Handy","US","Classic","Championship")
  
  
  
  i<-1
  while(i<8){
    if(df$Place[i]==0){
      df$Points[i]<-0
    }else{
    
    if(df[i,1]=="Model"){
      df["Model","Points"]<-ratingpoints[df["Model","Place"]]*multiplier[which(multiplier$Place=="Model"),2]+df["Model","Entries"]
    } else if(df[i,1]=="Classic"){
      df["Classic","Points"]<-(ratingpoints[df["Classic","Place"]]+df["Classic","Entries"])*multiplier[which(multiplier$Place=="Classic"),2]
    } else {
    
      df$Points[i]<-ratingpoints[df$Place[i]]+df$Entries[i]
    
    }}
    i<-i+1
  }
  
  #Championship/Reserve
  if(df["Championship","Place"]==1){
    df["Championship","Points"]<-(ratingpoints[1]+df["Championship","Entries"])*multiplier[which(multiplier$Place=="Champion"),2]
  } else if (df["Championship","Place"]==2) {
    df["Championship","Points"]<-(ratingpoints[1]+df["Championship","Entries"])*multiplier[which(multiplier$Place=="Reserve"),2]
  } 
  
  totalpoints<-sum(df$Points)
  showpoints<-df %>% select(Class,Place,Points) %>% add_row(Class="Total",Place=NA,Points=totalpoints)
 
  return(showpoints) 
}

addpointstofile<-function(showpoints,division,showname,ponyname,showdate,excelfile){
  
  ponyname<-toupper(ponyname)
  
  #load workbook
  wb<-loadWorkbook(file=excelfile)
  wsheet<-ponyname
  
  #mutate division to match excel
  division<-gsub("PONY HUNTER-","\\1",division)
  
  df<-data.frame("Show"=showname,"CompDate"=as.Date(showdate),"Pony"=ponyname,"Division" = division,"Total"=showpoints)
  addponyshow<-df %>% mutate(Date=CompDate,Points=Total)
  addponyshow<-addponyshow %>% select(Show,Date,Division,Points)
  
  #add show list to workbook
  if(!(wsheet %in% sheets(wb))){
    addWorksheet(wb,wsheet)
    nationalpoints<-addponyshow
    
  } else {
    ponyshows<-read.xlsx(wb,wsheet,detectDates = TRUE)
    nationalpoints<-rbind(ponyshows,addponyshow)
    nationalpoints<-nationalpoints %>% distinct()
  }
  
  #save show list to workbook
  writeData(wb,wsheet,nationalpoints)
  
  #add shows to all shows
  showlist<-read.xlsx(wb,"Shows",detectDates = TRUE)
  
  #add horse
  names(df)<-c("Show","Date","Pony","Division","Points")
  
  #add shows to list
  df2<-rbind(showlist,df)
  
  df2<-df2 %>% distinct() 
  df2<-df2 %>% arrange(Pony,Date)

  #write to excel
  writeData(wb,"Shows",df2)
  
  #save workbook
  saveWorkbook(wb,excelfile,overwrite = TRUE)
}
