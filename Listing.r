 
 #adding comments
 
> library(deplyr)
 
> new_tbm <- filter(TBM_CM, CMCAT =='MEDICATION HISTORY',CMCAT=='GENERAL CONMED') 
 

> TBM_VS <-read.csv(file="TBM_VS.csv") 
 

 

#####  READING RAW DATASETS # 

 

> new_vs <- TBM_VS %>% select(Study,Site,Subject,VisitName,VSDAT,VSDTC,VSDSTDTC,VSDENDTC,VisitGroupIndex,FormIndex,ItemGroupIndex,VisitName) 
 

> TBM_CM <-read.csv(file="TBM_CM.csv") 
 

> new_cm <- TBM_CM %>% select(Study,Site,Subject,CMTRT,CMCAT,CMSTDTC,CMENDTC,CMSTDAT,CMENDAT,VisitGroupIndex,FormIndex,ItemGroupIndex,VisitName) 

# PRACTICING JOIN
 

> new_vscm<- full_join(new_cm,new_vs,by=c('Subject','Site','VisitName','VisitGroupIndex','FormIndex','ItemGroupIndex')) 
 

> new_vscm<- right_join(new_cm,new_vs,by=c('Subject','Site','VisitName','VisitGroupIndex','FormIndex','ItemGroupIndex')) 
 

#LEFT-JOIN TO TAKE DATA ONLY FROM CM 

> new_vscm<- left_join(new_cm,new_vs,by=c('Subject','Site','VisitName','VisitGroupIndex','FormIndex','ItemGroupIndex')) 

 
> new_vscm2<-filter(new_vscm,VisitName=='SCRN') 
 

 
#REMOVING DUPLICATES BY DISTINCT FUNX 

> new_vscm2<-new_vscm%>%distinct(.keep_all = TRUE) 
 

> new_vscm2 <- new_vscm2%>% mutate(Conflag = ifelse(VisitName == 'SCRN', 'Y','N')) 
 

#DATE FORMATTING FROM CHAR TO DATE  

> new_vscm2 <-  new_vscm2%>% mutate(Startdate = as.Date(CMSTDTC)) 
> new_vscm2 <-  new_vscm2%>% mutate(Enddate = as.Date(CMENDTC)) 
> new_vscm2 <-  new_vscm2%>% mutate(VSdate = as.Date(VSDTC)) 
 

 #ADDING FLAG BASED ON CONDITIONS

> new_vscm2 <- new_vscm2%>% mutate(Conflag = ifelse(Conflag=='Y' & VSdate>=Startdate & VSdate<=Enddate, 'Y','N')) 
 

> datalisting<-new_vscm2 %>% select(Study.x,Site,Subject,CMTRT,CMCAT,CMSTDAT,CMENDAT,Conflag) 

 
> View(datalisting) 

#Generating PDF and CSV for the Listing 

 

> View(datalisting) 

 

> pdf("Listing1.pdf",width = 10,height = 12) 
> title<-textGrob("Listing-1.1 - Review of concomitant medications for required washout prior to ABPM screening assessment",gp = gpar(fontface="bold")) 
> table<-tableGrob(datalisting) 
> grid.arrange(title,table,nrow=2,heights=c(0.2,1)) 
> dev.off() 

 

########### CSV 

 

> write.csv(datalisting,"Listing-1.1.1.csv", row.names = FALSE) 