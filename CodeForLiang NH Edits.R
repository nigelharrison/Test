library(RODBC)
library(data.table)
library(ggplot2)
library(gridExtra)
library(gtable)
library(MASS)


## Messing with Github


## and messing around some more

# For accessing the data on Redshift
library(RPostgreSQL)

#### Look at DataDictionary ----
# Ryan created a Data Dictionary of what they are referring to
#date()
#connect = odbcConnect('Vertica_Adobe')
#tab1=sqlQuery(connect,'select * from Backcountry.BackcountryDataDictionary')
#date()
#View(tab1)
#rm(tab1)


##
t0 <- proc.time()

setwd("\\\\MINFILE05/MYDOCUMENTSREDIRECT$/nharrison/Documents/R/Backcountry")


## ggplot2 themes
ss_theme <- theme_bw() + theme(panel.border=element_blank(), axis.line=element_line()) + theme(panel.grid.major.y = element_line("dashed", size = .5, colour="blue"), plot.margin = unit(c(1, 2, 0.5, 0.5), "lines"))
ss_themeGr <- theme_bw() + theme(panel.border=element_blank(), axis.line=element_line()) + theme(panel.grid.major.y = element_line("dashed", size = .5, colour="grey"), plot.margin = unit(c(1, 2, 0.5, 0.5), "lines"))



## connect <- odbcConnect('Vertica_TRU')
connect <- dbConnect(PostgreSQL(), user= "nharrison", password="Sherpa123", dbname="truprod",host="54.166.125.238",port="5439")

## NH list of table names on server
dbGetQuery(connect,"select * from information_schema.tables")
#dat=dbGetQuery(connect,paste('SELECT ',dat.nam,' FROM backcountry.fact_click '))


#### Read in the Data ----
# Ram created the table pdb_Backcountry.Fact_Click_Sample_V7_Final as the newest version. Talk to him about any changes you would like in the future or getting access to it.
# The table has 2,206,490 rows from 100,000 unique VisitorID's. I was going to increase the number of visitors once we figured out some odd things with the data.
# dat is the name of the original data
date()
#connect = odbcConnect('Vertica_Adobe')
# dbGetQuery(connect,paste('SELECT count(*) FROM pdb_Backcountry.Fact_Click_Sample_V7_Final '))  #Visitor_Sample_100k     Fact_Click_Sample_V7_Final

# dat.nam is the list of variables from the table that I am interested in using for this analysis. 
# dat.nam=paste("VisitorID,VisitNbr,VisitClick,HitTime,ClickAction,ClickValue,TimeOnPageInSeconds,PageType,PreviousClickValue,PreviousClickType,CartAction,ProductID",
#               ",Tier1,Tier2,Tier3,Tier4,Tier5,Tier6,NbrNonNullTiers,DistanceHierarchy,JJaccard,JInclusionIndex,JCosine,JAssociation",
#               ",VisitReferrer,ProductViewNbrFromBeginning,TimeOnPageInSecondsFromBegining,ProductViewNbrFromPreviousPurchase,TimeOnPageInSecondsFromPreviousPurchase",sep='')
# dat=dbGetQuery(connect,paste('SELECT ',dat.nam,' FROM pdb_Backcountry.Fact_Click_Sample_V7_Final '))  #Visitor_Sample_100k     Fact_Click_Sample_V7_Final

## NH List of fields to select

dat.nam=paste("VisitorID,VisitNbr,VisitClick,HitTime,ClickAction,ClickValue,TimeOnPageInSeconds,PageType,CartAction,ProductID",
              ",Tier1,Tier2,Tier3,Tier4,Tier5,Tier6,Tier7,Tier8,NbrNonNullTiers,DistanceHierarchy,DistanceHierarchyFromNew,JJaccard,JInclusionIndex,JCosine,JAssociation,VisitReferrer",sep='')

## Pulls back the Fact Click Sample v7 table. Takes ~ 1hr and 20 mins

dat=dbGetQuery(connect,paste('SELECT * FROM pdb_Backcountry.Fact_Click_Sample_V7_Final '))  #Visitor_Sample_100k     Fact_Click_Sample_V7_Final
colnames(dat) <- strsplit(dat.nam,",")[[1]]

date()

#### Read in the Purchase Data ----
# This table is all purchases by visitors in dat
date()
#connect = odbcConnect('Vertica_Adobe')
Purchase.dat=dbGetQuery(connect,paste("SELECT * FROM Backcountry.Fact_Cart where CartAction='Purchase'"))
#Purchase.dat2=dbGetQuery(connect,paste("SELECT * FROM pdb_Backcountry.Fact_Cart_For_DateRange_Sample where CartAction='Purchase'"))
colnames(Purchase.dat) <- c("VisitorID","VisitDate","VisitTime","VisitNbr","VisitClick","LogDataID","ProductNumber","ProductID","Quantity","Price","Tax","Shipping","Cogs","CartAction")
# Change the Purchase.dat from a data.frame to a data.table. This is all the Cart events for each VisitorID
Purchase.dat=data.table(Purchase.dat)
# # Sort Purchase.dat and dat2 the same way
# setkeyv(Purchase.dat,c('VisitorID','VisitNbr','VisitClick'))
# setkeyv(dat2,c('VisitorID','VisitNbr','VisitClick'))
# # Merge dat2 and Purchase.dat where they have the same VisitorID,VisitNbr,and VisitClick
# Purchase.dat=Purchase.dat[dat2,nomatch=0]
# remove some vectors that aren't used anymore.

date()
rm(connect)
rm(dat.nam)

#### Create Variables ----
date()
# Change dat from a data.frame to a data.table. Is much faster for large datasets from my experience. 
dat=data.table(dat)
#The code below essentaially orders dat by these columns from left to right.
setkeyv(dat,c('VisitorID','VisitNbr','VisitClick'))
# Change the PageType variable to a factor instead of a character
dat$PageType=as.factor(dat$PageType)
# Change the variable VisitClick to numeric
dat$VisitClick=as.numeric(dat$VisitClick)
# Change the variable VisitNbr to numeric
dat$VisitNbr=as.numeric(dat$VisitNbr)
# Change the variable NbrNonNullTiers to numeric
dat$NbrNonNullTiers=as.numeric(dat$NbrNonNullTiers)
# Create a function that takes the PageType, CartAction, and Tier1 and classifies them into 11 different classes
# These are my own classifications, open to change if you would like.
pageclass.func=function(x,y,z){
  #x=PageType ; y=CartAction ; z=Tier1
  tmp=numeric(length(x))
  # The following PageTypes make up the PageClass=='Home'
  ind=which(x%in%c('Home','Site Map'))
  tmp[ind]='Home'
  # The following PageTypes make up the PageClass=='Brand
  ind=which(x%in%c('PLP Brand','Brand Home','All Brands'))
  tmp[ind]='Brand'
  # If it has the following PageTypes and not missing Tier1 and not a 'Product View', then it is a Category Page
  ind=which(!is.na(z) & is.na(y) & !x%in%c('PLP Brand','Brand Home','All Brands'))
  tmp[ind]='Category'
  # if it isn't missing Tier1 and the CartAction is a 'Product View', then it is a Product Page
  ind=which(!is.na(z) & (y=='Product View' | x%in%c('Product Detail - Permanently Out of Stock','Product Detail - Temporarily Out of Stock')))
  tmp[ind]='Product'
  # The following PageTypes make up the PageClass=='Checkout'
  ind=which(x%in%c('Checkout','Speedy Checkout'))
  tmp[ind]='Checkout'
  # The following PageTypes make up the PageClass=='Receipt', It has to be labelled as a receipt page in order for a purchase to go through
  ind=which(x%in%c('Receipt'))
  tmp[ind]='Receipt'
  # The following PageTypes make up the PageClass=='Account'
  ind=which(x%in%c('Community Login','Customer Login','Account','User Profile Page','Leaderboard'))
  tmp[ind]='Account'
  # The following PageTypes make up the PageClass=='Search'
  ind=which(x%in%c('Search Results'))
  tmp[ind]='Search'
  # The following PageTypes make up the PageClass=='Information'
  ind=which(x%in%c('gearhead-landing','Help Center','Newsletter','articles page','Explore Landing','videos page'))
  tmp[ind]='Information'
  # The following PageTypes make up the PageClass=='Cart'
  ind=which(x%in%c('Cart'))
  tmp[ind]='Cart'
  # Any page that doesn't fit in is labeled "Other". These typically are old product pages we don't have information on, or special seasonal sales and such. They do have other ones as well though.
  ind=which(tmp==0)
  tmp[ind]='Other'
  return(tmp)
}
# I change ClickAction that are labelled 'First Page' or 'Last Page' that don't have a URL as 'On Page' 
dat[ClickAction%in%c('First Page','Last Page') & !substr(ClickValue,1,4)%in%'http']$ClickAction='On Page'
# I add a 12th PageClass for all 'On Page' actions.
dat[ClickAction=='On Page',PageClass:='On Page Action']
date()
# Run my pageclass.func function from above
dat[ClickAction!='On Page',PageClass:=pageclass.func(PageType,CartAction,Tier1)]
date()
# rm the pageclass.func
rm(pageclass.func)
# Make PageClass a factor variable instead of a character
dat$PageClass=as.factor(dat$PageClass)

# Below I list all ClickValue of interest where ClickAction=='On Page'
# I am only interested in User defined behavior. Some of the rows that show up in the data not listed below are rows created by the website. 
# For example if the site shows an ad, they add a row where ClickAction=='On Page' and ClickValue=='Recommendation Displayed'. But since 
# this has nothing to do with the users behavior I will be removing them
cv=c('Sizing Chart','Remove Item (Cart)','Promo Content Click','Filter Type','Customer Login','sign-in','User PDP Image Zoom','UpdateCart',
     'Move to Cart','Remove Item (Wishlist)','Move to Wishlist','Remove','Live Help','Read Reviews','User shared on Facebook','Autocomplete','Truefit clicks','Customer Login',
     'Product Review Navigation review links','Product Review Navigation review dropdown','sign-in','User Clicked "Sign In" on header','Step 1: Select Gender','receipt_email_signup',
     'Step 4: View Results','Step 2: Select Bike Type','Step 3: Measure Inseam','Shop Bikes','Product Review Submission Thank you','profile created','sign in clicks')

# Remove any ClickAction=='On Page' where ClickValue is not in the list cv above.
dat2=dat[PageClass!='On Page Action' | (PageClass=='On Page Action' & ClickValue%in%cv)]
rm(cv)
# Sort the data
setkeyv(dat2,c('VisitorID','VisitNbr','VisitClick'))
# Calculate the number or rows for each VisitorID
tmp=dat2[,.N,by=VisitorID]
# Remove any VisitorID's that don't have at least 2 clicks in our data set.
# This brings the number of VisitorID's from 100,000 to 55,532
dat2=dat2[VisitorID%in%tmp[N>1]$VisitorID]
rm(tmp)
# Change the HitTime column to class POSIXct, to be able to take differences of time
dat2$HitTime=as.POSIXct(dat2$HitTime,tz='GMT')
# function that changes a visitlength from 30 minutes to 1.25 days or 30 hours
new.visit=function(x,y){
  #x=HitTime
  #y=VisitNbr[1]
  visit=numeric(length(x))
  visit[1]=y
  if(length(x)==1){
    return(visit)
  }
  for(i in 2:length(x)){
    visit[i]=ifelse(difftime(x[i],x[i-1],units="days")<1.25,visit[i-1],visit[i-1]+1)  # if you want to look at different visit lengths change 1.25 
  }
  return(visit)
}
# Change the Visit length and thus changing the VisitNbr to now CycleNbr
dat2[,CycleNbr:=new.visit(HitTime,VisitNbr[1]),by=VisitorID]
rm(new.visit)
# With the new visit length change the clicks in that visit to CycleClick instead of VisitClick
dat2[,CycleClick:=1:length(VisitClick),by=c('VisitorID','CycleNbr')]
setkeyv(dat2,c('VisitorID','CycleNbr','CycleClick'))
# Calcualte the First Page and the Last Page of a  visit
dat2[,CycleFirstPage:=ifelse(CycleClick==min(CycleClick),'Yes','No'),by=c('VisitorID','CycleNbr')]
dat2[,CycleLastPage:=ifelse(CycleClick==max(CycleClick),'Yes','No'),by=c('VisitorID','CycleNbr')]
dat2[,VisitFirstPage:=ifelse(VisitClick==min(VisitClick),'Yes','No'),by=c('VisitorID','VisitNbr')]
dat2[,VisitLastPage:=ifelse(VisitClick==max(VisitClick),'Yes','No'),by=c('VisitorID','VisitNbr')]
# Calculate the TimeOnPage (used for 30 hour visit) instead of using TimeOnPageInSeconds (TimeOnPageInSeconds is the time for a 30 min visit) 
# The last pages of a visit is given a NA for TimeOnPageInSeconds because we can't tell when he leaves. So times are always calculated once the Visitor makes another click 
pagetime.func=function(x,y,z){
  #x=TimeOnPageInSeconds
  #y=HitTime
  #z=PageClass
  nona=which(is.na(x) & !z=='On Page Action')
  nona=nona[-length(nona)]
  if(length(nona)==0){
    return(x)
  }
  for(i in nona){
    x[i]=as.integer(difftime(y[i+1],y[i],units='secs'))
  }
  return(x)
}
# Run the pagetime.func function
dat2[,TimeOnPage:=pagetime.func(TimeOnPageInSeconds,HitTime,PageClass),by=c('VisitorID','CycleNbr')]
# Remove any visitors that have less than 0.
dat2=dat2[!VisitorID%in%dat2[TimeOnPage<0]$VisitorID]
rm(pagetime.func)
setkeyv(dat2,c('VisitorID','CycleNbr','CycleClick'))
# Calculate the number of clicks each user has ever made
dat2[,click:=(1:length(CycleClick)),by=c('VisitorID')]
# This function is used to calculate whenever a time difference of 10 minutes has past for each visitors activity
# I am only using this variable for plotting purposes. See the function Adobe_Figure_Lines to see how this is implemented
time10=function(x){
  x=x$HitTime
  n=length(x)
  tmp=numeric(n)
  tmp[1]=1
  if(n==1){
    return(tmp[1])
  }
  time=x[1]
  for(i in 2:n){
    val=as.numeric(difftime(x[i],time,units='mins'))
    tmp[i]=ifelse(val >=10 ,1,0)
    if(tmp[i]==1){
      time=x[i]
    }
  }
  return(tmp)
}
dat2[,Time10:=time10(.SD),by=c('VisitorID','CycleNbr'),.SDcols='HitTime']
rm(time10)
# Change the NbrNonNullTiers to -1 if it is missing instead of having NA
dat2[is.na(NbrNonNullTiers)]$NbrNonNullTiers=-1
# Change the TimeOnPage NA values to 0. This should only change the last page of a visit and ClickAction=='On Page' values to 0.
dat2[is.na(TimeOnPage)]$TimeOnPage=0
# Fucntion that calculates the length of Time that has past from the first click of a visit until the current click
visit.time=function(x){
  #x=TimeOnPage
  tmp=numeric(length(x))
  tmp[1]=x[1]
  if(length(x)==1){
    return(tmp)
  }
  for(i in 2:length(x)){
    tmp[i]=x[i]+tmp[i-1]
  }
  return(tmp)
}
# Run the visit.time function
dat2[,CycleTime:=visit.time(TimeOnPage),by=c('VisitorID','CycleNbr')]
rm(visit.time)
# Total time spent for each visit
time.density=as.numeric(sort(dat2[,sum(TimeOnPage,na.rm=TRUE),by=c('VisitorID','CycleNbr')]$V1))
# Calculate the percentile of the density for each click, currently not being used
dat2[,Time.Weight:=ecdf(time.density)(CycleTime)]
# Get the maximum number of click for each visit
click.density=as.numeric(sort(dat2[,max(click)-min(click),by=c('VisitorID','CycleNbr')]$V1))
# Calculate the percentile of the density for each click currently not being used
dat2[,Click.Weight:=ecdf(click.density)(CycleClick)]
# Get all TimeOnPage of the dataset
pagetime.density=as.numeric(sort(dat2$TimeOnPage))
# Calculate the percentile of the density for each click. Currently being used for focus
dat2[,PageTime.Weight:=ecdf(pagetime.density)(TimeOnPage)]
setkeyv(dat2,c('VisitorID','CycleNbr','CycleClick'))
# Give all visits where a purchase was made a value of 1, else 0
dat2[,VisitPurchase:=ifelse('Receipt'%in%PageClass,rep(1,length(PageClass)),rep(0,length(PageClass))),by=c('VisitorID','VisitNbr')]
dat2[,CyclePurchase:=ifelse('Receipt'%in%PageClass,rep(1,length(PageClass)),rep(0,length(PageClass))),by=c('VisitorID','CycleNbr')]
rm(time.density)
rm(click.density)
rm(pagetime.density)
# Calculate the time that has past from the last click of one visit by a VisitorID to the first click of the next visit else is 0
v2vtime=function(x,y,w){
  #x=CycleFirstPage ; y=CycleLastPage ; w=HitTime
  tmp=numeric(length(x))
  for(i in 2:length(x)){
    if(x[i]=='Yes' & y[i-1]=='Yes'){
      tmp[i]=as.numeric(difftime(w[i],w[i-1],units='days'))
    }
  }
  return(tmp)
}
# Run the v2vtime (stands for visit to visit time)
dat2[,V2VTime:=v2vtime(CycleFirstPage,CycleLastPage,HitTime),by=c('VisitorID')]
rm(v2vtime)
# Get the values of visit to visit time differences
V2VTime.density=sort(dat2[V2VTime>0]$V2VTime)
# A Visit to Visit weight will be used to calculate the Visit to Visit focus
dat2[,V2V.Weight:=1-ecdf(V2VTime.density)(V2VTime)]
# Used to calculate the number of threads (thoughts or categories) the VisitorID is interested in so far in the visit
thread.class=function(x,y){
  #x=Tier1; y=PageClass
  tmp=rep(NA,length(x))
  ind=which(x%in%c('Shoes','Snowshoe','Kids','Women','Men','Snowboard','Ski','Climb','Fly Fishing','Hike & Camp',
                   'Bike','Accessories','Paddle','Travel','Run'))
  tmp[ind]=as.character(x[ind])
  ind=which(!x%in%c('Shoes','Snowshoe','Kids','Women','Men','Snowboard','Ski','Climb','Fly Fishing','Hike & Camp',
                    'Bike','Accessories','Paddle','Travel','Run') & y%in%c('Brand','Product','Category','Other') & !is.na(x))
  tmp[ind]='Brands'
  tmp[y=='Search']='Search'
  return(as.factor(tmp))
}
# Run the thread.class function
dat2[,Thread:=thread.class(Tier1,PageClass)]
rm(thread.class)

#### Calculate the number of page views of a product for each visitor ----
# I created this to possibly use in the Consideration Level variable
# Ram added something similar to the sql table. But I haven't seen how they compare or used either of them yet really
# It is calculating the number of product views by a VisitorID
prod.view.nbr=function(x){
  tmp=unique(x)
  n=length(x)
  tmp1=numeric(n)
  for(i in 1:n){
    tmp1[i]=sum(x[1:i]==x[i])
  }
  return(tmp1)
}
setkeyv(dat2,c('VisitorID','CycleNbr','click'))
date()
dat2[PageClass=='Product',ProductViewNbr:=prod.view.nbr(ProductID),by=c('VisitorID')]
date()
rm(prod.view.nbr)

#### Calculate the total time spent viewing a product for each visitor ----
# I created this to possibly use in the Consideration Level variable
# Ram added something similar to the sql table. But I haven't seen how they compare or used either of them yet really
# It is calculating the total time spent viewing a product on page by a VisitorID
prod.view.time=function(x,y){
  tmp=unique(x)
  n=length(x)
  tmp1=numeric(n)
  for(i in 1:n){
    ind=which(x[1:i]==x[i])
    tmp1[i]=sum(y[ind],na.rm=TRUE)
  }
  return(tmp1)
}
setkeyv(dat2,c('VisitorID','CycleNbr','click'))
date()
dat2[PageClass=='Product',ProductViewTime:=prod.view.time(ProductID,TimeOnPage),by=c('VisitorID')]
date()
rm(prod.view.time)

#### Calculate the total time from first view to last view ----
# I created this to possibly use in the Consideration Level variable
# Ram added something similar to the sql table. But I haven't seen how they compare or used either of them yet really
# It is calculating the total time since first view of a product by a VisitorID
setkeyv(dat2,c('ProductID','VisitorID','CycleNbr','click'))
date()
dat2[PageClass=='Product',ProductTotalTime:=as.numeric(difftime(HitTime,HitTime[1],units="days")),by=c('ProductID','VisitorID')]
date()

#### Temporary fix of DistanceHierarchy ----
# This is used to lessen the distance between products that weren't exactly the same. Right now it is by 0.9. 
# I think the JJaccard or JCosine will be better distance measures. But if you interested you could look at different ways to weight them.
distance.fix=function(x,y){
  #x=DistanceHierarchy
  #y=ProductID
  if(length(x)==1){
    return(x)
  }
  tmp=numeric(length(x))
  tmp[1]=x[1]
  for(i in 2:length(x)){
    if(!is.na(x[i]) & !is.na(y[i]) & !is.na(y[i-1])){
      if(y[i]==y[i-1]){
        tmp[i]=x[i]
      } else{
        tmp[i]=.9*x[i]
      }
    } else if(!is.na(x[i])){
      tmp[i]=.9*x[i]
    } else {
      tmp[i]=x[i]
    }
  }
  return(tmp)
}
dat2[PageClass%in%c('Category','Product'),Dist2:=distance.fix(DistanceHierarchy,ProductID),by=c('VisitorID','CycleNbr')]
rm(distance.fix)

#### Add Consideration Weights ----
# My only attempt at adding a Consideration Level variable. I did it for graphical purposes. But I feel it is an interesting idea.
# I would like to look more into how many alternatives have they viewd also. But haven't gotten to it.
setkeyv(dat2,c('VisitorID','CycleNbr','CycleClick'))
# gets the maximum number of views by each product for each VisitorID
view.density=as.numeric(sort(dat2[PageClass=='Product',max(ProductViewNbr),by=c('VisitorID','ProductID')]$V1))
# assigns a weight of the quantile of views for this product
dat2[PageClass=='Product',View.Weight:=ecdf(view.density)(ProductViewNbr)]
# gets the total page view time for each product and for each VisitorID
view.time.density=as.numeric(sort(dat2[PageClass=='Product' & TimeOnPage>0,max(ProductViewTime),by=c('VisitorID','ProductID')]$V1))
# assigns a weight of the quantile page view time for this product
dat2[PageClass=='Product',ViewTime.Weight:=ecdf(view.time.density)(ProductViewTime)]
# gets the total time from first view for each product and for each VisitorID
view.total.time.density=as.numeric(sort(dat2[PageClass=='Product',difftime(HitTime[length(HitTime)],HitTime[1],units='days'),by=c('VisitorID','ProductID')]$V1))
# assigns a weight of the quantile page view time for this product
dat2[PageClass=='Product',ViewTotalTime.Weight:=ecdf(view.total.time.density)(ProductTotalTime)]
rm(view.density)
rm(view.time.density)
rm(view.total.time.density)

#### Calculation of Consideration ----
# All I did was quickly say Consideration Level is a percent of the weights calcualted above.
setkeyv(dat2,c('VisitorID','CycleNbr','CycleClick'))
consideration=function(x,y,z){
  #x=View.Weight
  #y=ViewTime.Weight
  #z=ViewTotalTime.Weight
  consideration=numeric(length(x))
  if(length(x)==1){
    if(!(is.na(x) & is.na(y) & is.na(z))){
      return(y)
    } else {
      return(0)
    }
  }
  for(i in 2:length(x)){
    if(!(is.na(x[i]) & is.na(y[i]) & is.na(z[i]))){
      consideration[i]=(.7*y[i]+.3*z[i])
    } else {
      consideration[i]=consideration[i-1]
    }
  }
  return(consideration) 
}
dat2[,Consideration:=consideration(View.Weight,ViewTime.Weight,ViewTotalTime.Weight),by=c('VisitorID','CycleNbr')]

#### Plot Function ----
# This is a function that will plot in one figure: 
# 1. The log10 of time viewing a page.
# 2. the PageClass of the page they are on
# 3. the Cluster the click is in
# 4. The Focus,Concreteness(Also known as Abstraction), and Consideration Level Lines
# The time labels correspond to clicks where 10 minutes has past since the previous time label
# You pass it the data(dat2),visitor=VisitorID,min_e=the first click you want,max_e=last click you want to see
Adobe_Figure_Lines=function(data,visitor,min_e,max_e){
  tmp.dat=data[VisitorID==visitor & click>=min_e & click<=max_e]
  #### Following Time bar chart
  setkeyv(tmp.dat,c('VisitorID','CycleNbr','CycleClick'))
  mn=min_e #1400
  mx=max_e #1500
  
  gg1=ggplot(tmp.dat,aes(x=as.factor(click),y=log10(TimeOnPage)))+
    geom_bar(stat='identity',color='white',position='identity',size=.85)+
    geom_bar(data=tmp.dat[CycleFirstPage=='Yes'],color='black',stat='identity',size=.85,position='identity')+
    xlab("")+ylab(expression(atop('Time Per Event\n (Log_10 Sec)')))+ggtitle(paste('VisitorID=',visitor,', from Event=',mn,' - ',max(tmp.dat$click)))+
    scale_x_discrete(breaks=seq(mn,mx,5))+scale_y_continuous(breaks=c(seq(0,3,1),log10(3500)),labels=c(1,10,100,1000,3500))+
    theme(legend.position="none",axis.text.x=element_blank(),axis.ticks.x=element_blank(),plot.margin=unit(c(1,1,-.85,1), "cm"))
  
  #### Following Page Behavior
  gg2=ggplot(tmp.dat,aes(x=as.factor(click),y=as.factor(VisitorID),fill=as.factor(PageClass)))+
    geom_tile()+
    geom_tile(color='white',show_guide=FALSE,size=.85)+
    geom_tile(data=tmp.dat[CycleFirstPage=='Yes'],color='black',size=.85,show_guide=FALSE)+
    xlab("")+ylab('')+scale_x_discrete(breaks=seq(mn,mx,5))+
    scale_fill_manual(values=c('Product'='tan1','Cart'='steelblue2','Brand'='blue','Category'='yellow2','Receipt'='maroon',
                               'Home'='salmon4','Account'='green','Search'='darkolivegreen','On Page Action'='grey',
                               'Checkout'='violet','Information'='red','Other'='black'),
                      labels=c('Product','Cart','Brand','Category','Receipt',
                               'Home','Account','Search','On Page Action',
                               'Checkout','Information','Other'),
                      breaks=c('Product','Cart','Brand','Category','Receipt',
                               'Home','Account','Search','On Page Action',
                               'Checkout','Information','Other'),
                      name='Page Class',drop=FALSE)+
    theme(legend.position="bottom",axis.text.x=element_blank(),plot.margin=unit(c(0,1,-0.5,1), "cm"),
          axis.text.y=element_blank(),axis.ticks.y=element_blank())+guides(fill = guide_legend(ncol=4))
  leg1 = gtable_filter(ggplot_gtable(ggplot_build(gg2+theme(legend.position="bottom"))), "guide-box")
  
  #### Following Cluster of the Visitor
  gg4=ggplot(tmp.dat,aes(x=as.factor(click),y=as.factor(VisitorID),fill=as.factor(Cluster)))+
    geom_tile()+
    geom_tile(color='white',show_guide=FALSE,size=.85)+
    geom_tile(data=tmp.dat[CycleFirstPage=='Yes'],color='black',size=.85,show_guide=FALSE)+
    xlab("")+ylab('')+scale_x_discrete(breaks=seq(mn,mx,5))+
    scale_fill_manual(values=c('Group 1'='grey18','Group 2'='red1','Group 3'='blue1','Group 4'='goldenrod2'),
                      labels=c('Low Focus; Low Concreteness','High Focus; Low Concreteness','Low Focus; High Concreteness','High Focus; High Concreteness'),
                      breaks=c('Group 1','Group 2','Group 3','Group 4'),
                      name='Group Cluster',drop=FALSE)+
    theme(legend.position="bottom",axis.text.x=element_blank(),plot.margin=unit(c(0,1,-0.5,1), "cm"),
          axis.text.y=element_blank(),axis.ticks.y=element_blank())+guides(fill = guide_legend(ncol=1))
  leg3 = gtable_filter(ggplot_gtable(ggplot_build(gg4+theme(legend.position="bottom"))), "guide-box")
  
  #### Following Focus Score
  gg3=ggplot(tmp.dat,aes(x=1:length(click),y=Focus))+
    geom_line(aes(color='Focus'),alpha=.5)+
    geom_point(aes(color='Focus'),size=3,alpha=.5)+
    geom_line(aes(y=Abstraction,color='Concreteness'),alpha=.5)+
    geom_point(aes(y=Abstraction,color='Concreteness'),size=3,alpha=.5)+
    geom_line(aes(y=Consideration,color='Consideration'),alpha=.5)+
    geom_point(aes(y=Consideration,color='Consideration'),size=3,alpha=.5)+
    scale_color_manual(values=c('Concreteness'='blue1','Focus'='red1','Consideration'='green1'),
                       labels=c('Concreteness','Focus','Consideration'),
                       breaks=c('Concreteness','Focus','Consideration'),name='',drop=FALSE)+
    scale_y_continuous(breaks=seq(0,1,.25),limits=c(0,1.001))+
    xlab("")+ylab('Focus/Concreteness')+scale_x_discrete(breaks=c(ifelse(tmp.dat$Time10==1,tmp.dat$click,''),seq(mn,mx,5)),
                                                         labels=c(ifelse(tmp.dat$Time10==1,as.character(tmp.dat$HitTime),''),rep('',length(seq(mn,mx,5)))))+
    theme(legend.position='bottom',plot.margin=unit(c(0,1,-.5,1), "cm"),#c(-1,1,-.25,1)
          axis.text.x=element_text(size=9,vjust=0.4,angle=-90))+guides(color = guide_legend(ncol=1))
  leg2=gtable_filter(ggplot_gtable(ggplot_build(gg3+theme(legend.position="bottom"))), "guide-box")
  
  #### Make Figure
  gA <- ggplotGrob(gg1+theme(legend.position="none"))
  gB <- ggplotGrob(gg2+theme(legend.position="none"))
  gC <- ggplotGrob(gg3+theme(legend.position='none'))
  gD <- ggplotGrob(gg4+theme(legend.position='none'))
  maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5],gD$widths[2:5],gC$widths[2:5])
  gA$widths[2:5] <- as.list(maxWidth)
  gB$widths[2:5] <- as.list(maxWidth)
  gC$widths[2:5] <- as.list(maxWidth)
  gD$widths[2:5] <- as.list(maxWidth)
  leg=arrangeGrob(leg1,leg2,leg3,ncol=3,widths=c(.75,.5,.75))
  return(grid.arrange(gA,gB,gD,gC,leg,ncol=1,heights=c(1.5,.25,.25,1.5,.5)))
}

#### Experimental Exit Probability ----
# This is something that I thought might be cool to see is a probability of exiting on this click. Not sure if
# it is something of interest for the project or not. But I thought it might be valuable in the future. I haven't done anything with though.
# I commented out my code until I could spend more time on it.

##tmp=dat2[,list(CycleLastPage,PageClass,Focus,Abstraction)]
##glm1=glm(as.factor(CycleLastPage)~PageClass+Focus+Abstraction,data=tmp,family=binomial)
##summary(glm1)
##tmp$pred.glm1=predict(glm1,type='response')
##hist(tmp$pred.glm1/max(tmp$pred.glm1))

#### Concreteness (Abstraction) Calculation ----
date()
# This is an the focus code chunks are where most of my work has been in.
# Concreteness is used to measure kind of how concrete is the VisitorIDs state of mind as far as purchasing. 
# So home page they could go anywhere on the site so they aren't very concrete. They get more concrete as they
# go to product pages or on page actions of interest.
# The a1 function gives a weight to PageClasses of interest.
a1=function(x){
  y=x[[2]] #NbrNonNullTiers
  m=x[[3]] #TimeOnPage
  x=x[[1]] #PageClass
  #
  tmp=rep(NA,length(x))
  ind=which(x%in%c('Home'))
  tmp[ind]=0
  # I don't know if Account should effect Concreteness or not.
  # each click is multiplied by the quantile of how long they spend, compared to all page views of that PageClass
  # All of the weights are subjective. I would like to get a more objective way of figureing out these weights.
  ##ind=which(x=='Account')
  ##ecdf.tmp=as.numeric(sort(m[ind]))
  ##ecdf.density=ecdf(ecdf.tmp)(m[ind])
  ##tmp[ind]=.25*ecdf.density
  ind=which(x=="Information")
  ecdf.tmp=as.numeric(sort(m[ind]))
  ecdf.density=ecdf(ecdf.tmp)(m[ind])
  tmp[ind]=.1*ecdf.density
  ind=which(x%in%c("Category",'Brand','Other'))
  ecdf.tmp=as.numeric(sort(m[ind]))
  ecdf.density=ecdf(ecdf.tmp)(m[ind])
  tmp[ind]=(.7 + y[ind]/30)*ecdf.density
  ind=which(x=="Search")
  ecdf.tmp=as.numeric(sort(m[ind]))
  ecdf.density=ecdf(ecdf.tmp)(m[ind])
  tmp[ind]=.85*ecdf.density
  ind=which(x=="Product")
  ecdf.tmp=as.numeric(sort(m[ind]))
  ecdf.density=ecdf(ecdf.tmp)(m[ind])
  tmp[ind]=(.9+y[ind]/60)*ecdf.density
  return(tmp)
}
dat2[,A1:=a1(list(PageClass,NbrNonNullTiers,TimeOnPage))]
date()
# the abs.onpage function give a boost if they add the product to their cart, look at sizing charts, etc.
# and it gives a negative boost if you remove an item from the cart.
abs.onpage=function(x){
  y=x[[4]] #Abstraction
  w=x[[3]] #CartAction
  z=x[[2]] #ClickValue
  x=x[[1]] #PageClass
  ind=c(which(x=='On Page Action' & z%in%c('Sizing Chart','User PDP Image Zoom','Read Reviews','Remove Item (Cart)','Remove',
                                           'Product Reveiw Navigation review links','Product Review Navigation review dropdown')),
        which(x=='Cart' & w%in%c('Shopping Cart Add','Shopping Cart Remove')))
  if(length(ind)==0){
    return(y)
  }
  for(i in ind){
    last=tail(y[which(!is.na(y[1:i]))],1)
    if(length(last)==0){
      last=0
    }
    if(x[i]=="On Page Action" & z[i]%in%c('Sizing Chart','User PDP Image Zoom','Read Reviews',
                                          'Product Reveiw Navigation review links','Product Review Navigation review dropdown')){
      y[i]=min(1,last*1.05)
    }
    if(x[i]=='Cart' & w[i]=='Shopping Cart Add'){
      y[i]=min(1,last*1.1)
    }
    if((x[i]=='On Page Action' & z[i]%in%c('Remove Item (Cart)','Remove')) | (x[i]=='Cart' & w[i]=='Shopping Cart Remove')){
      y[i]=last*.75
    }
  }
  return(y)
}
dat2[,A2:=abs.onpage(list(PageClass,ClickValue,CartAction,A1)),by=c('VisitorID')]
date()
# weights is how much the past pages of interest that effect your Concreteness score. 
weights=c(.2,.2,.2,.2,.2)
# this function returns the previous score if the PageClass doesn't effect Concreteness. Else its final score is the weights%*%the scores that effect the value
abs.score=function(x,w){
  #x=abstraction;  k=window size; w=weights =.175,...,.3
  k=length(w)
  no.na=!is.na(x)
  tmp=numeric(length(x))
  if(is.na(x[1])){
    tmp[1]=0
  }else{
    tmp[1]=x[1]
  }
  if(length(x)==1){
    return(tmp)
  }
  for(i in 2:length(x)){
    if(no.na[i]==1){
      y=tail(x[which(no.na[1:i]==1)],k)
      ww=w[((k+1)-length(y)):k]
      tmp[i]=ww%*%y
    }else{
      tmp[i]=tmp[i-1]
    }
  }
  return(tmp)
}
dat2[,Abstraction:=abs.score(A2,weights),by=c('VisitorID','CycleNbr')]
date()

#View(dat2[Focus>0 & Abstraction==0,list(VisitorID,CycleNbr,CycleClick,NbrNonNullTiers,A1,A2,Abstraction,Focus,PageClass,CartAction,ClickValue)])

#### Look at rolling focus from visit to visit ----
# can replace JJaccard, JCosine, JAssociation, and JInclusionIndex interchangeably
# if you change one of the above, you need to change also the corresponding DistJ,DistC,DistA,DistI
dist.density=as.numeric(sort(dat2$JJaccard))
dat2[,DistJ:=ecdf(dist.density)(JJaccard)]
# Below I have tried using this to decide the Thread.Weight that going into the focus score.
# Definitely isn't optimal. More of a starting point. 
thread.density=numeric(18)
k=5
for(i in 1:18){
  thread.density[i]=(ppois(i,k)-ppois(i-1,k))/(ppois(k,k)-ppois(k-1,k))#-ppois(i-1,6))/(ppois(6,6)-ppois(5,6))
}
threadweight=function(x,y,z){
  #x=Thread ; y=thread.density ; z=JJaccard
  ind=which(!is.na(z) | !is.na(x))
  tmp=numeric(length(x))
  for(i in ind){
    num=sum(levels(x)%in%x[1:i])+1
    tmp[i]=y[num]
  }
  return(tmp)
}
dat2[,Thread.Weight:=threadweight(Thread,thread.density,JJaccard),by=c('VisitorID','CycleNbr')]

# This is used the same as the weights for Concreteness. It is only giving weight to the past 4 values of clicks that effect focus.
weights=c(.1,.2,.2,.5)  #.1,.1,.2,.2,.4
rolling.focus=function(w,x,y,z,r,b){
  #w=Dist ; x=Thread.weight ; y=PageTime.Weight ; z=weights ; r=V2V.Weight ; b=CycleNbr
  f1=w*(.7*y+.3*x)  # the .7 and .3 are how much weight I give to the PageTime.Weight and Thread.Weight. Open to change
  no.na=!is.na(w)   # this is used to identify clicks that do and don't effect focus score
  first=1           # which click is the first of this CycleNbr. Only looking at focus inside a visit, then I roll that over.
  k=length(z)
  f2=numeric(length(f1))
  for(i in 2:length(x)){
    if(b[i]==b[i-1]){    # identifies those in the same CycleNbr
      if(no.na[i]==1){   # identifies those that effect focus
        tmp=f1[first:i]  # looks at those from first click of visit to ith click of the visit
        tmp=tmp[!is.na(tmp)]
        if(length(tmp)<k){
          tmp=c(rep(0,k-length(tmp)),tmp)
        }
        tmp1=tail(tmp,k)  
        f2[i]=z%*%tmp1   # is the score given for focus
      } else{
        f2[i]=f2[i-1]    # if the click doesn't effect focus I return the previous value. But doesn't get used in focus score of future clicks
      }
    }else{
      first=i            # if the previous click has a different CycleNbr change first to i, the first click of the visit.
      if(is.na(f1[i])){
        f2[i]=r[i]*f2[i-1]  # if missing dist the return a portion of the last visits last focus score
      }else{
        f2[i]=r[i]*f1[i] # if not missing the dist, return a portion 
      }
    }
  }
  return(f2)
}
date()
dat2[,Focus:=rolling.focus(DistJ,Thread.Weight,PageTime.Weight,weights,V2V.Weight,CycleNbr),by=VisitorID]
date()

# This is one metric I looked at to see if the Concreteness scores was useful. Need to calculate Focus before it will return anything.
cor(dat2[PageClass=='Receipt' & Focus>0]$Focus,dat2[PageClass=='Receipt' & Focus>0]$Abstraction)

# Another way of seeing if it seemed like a good measure.
ggplot(dat2[Focus>0 & Abstraction>0],aes(x=Abstraction,group=CyclePurchase,fill=as.factor(CyclePurchase)))+
  geom_density(alpha=.35)+xlab('Click Abstraction')+ggtitle('Visits w/o Purchase vs Visits w/ Purchase')+
  scale_fill_manual(values=c('0'='black','1'='red'),labels=c('No','Yes'),name='Purchase')+
  scale_x_continuous(limits=c(0,1),breaks=seq(0,1,.25)) + ss_theme

# another visual to see how it was doing.
den3d <- kde2d(dat2[Focus>0 & Abstraction>0]$Focus,dat2[Focus>0 & Abstraction>0]$Abstraction)
persp(den3d, box=FALSE,theta=90)

# way to measure how it is doing
cor(dat2[PageClass=='Receipt' & Focus>0 & Abstraction>0]$Focus,dat2[PageClass=='Receipt' & Focus>0 & Abstraction>0]$Abstraction)

# visual of how it is doing
ggplot(dat2[Focus>0 & Abstraction>0],aes(x=Focus,group=CyclePurchase,fill=as.factor(CyclePurchase)))+
  geom_density(alpha=.35)+xlab('Click Focus')+ggtitle('Visits w/o Purchase vs Visits w/ Purchase')+
  scale_fill_manual(values=c('0'='black','1'='red'),labels=c('No','Yes'),name='Purchase')+
  scale_x_continuous(limits=c(0,1),breaks=seq(0,1,.25))

#### Simple Click Cluster ----
# cluster function that is clustering each click into one of 4 groups. 
# I eventually wanted to try something other than kmeans, like Hidden Markov Model to cluster instead. But haven't got around to it yet.
# Group1 = low focus, low concreteness. Group2 = high focus, low concreteness. Group3 = low focus, high concreteness. Group4 = high focus, high concreteness
clust.func=function(seed,x,y){
  # seed=seed ; x=focus ; y=abstraction
  set.seed(seed)  #95789757 #95757
  clust=kmeans(cbind(x,y),4)
  clust2=rep('Group 3',length(x))
  tmp=clust$centers[,1]*clust$centers[,2]
  clust2[clust$cluster==which(tmp==max(tmp))]='Group 4'
  clust2[clust$cluster==which(tmp==min(tmp))]='Group 1'
  ind=as.numeric(names(tmp[which(!tmp%in%c(max(tmp),min(tmp)))]))
  tmp=clust$centers[ind,]
  ind=as.numeric(names(which(c(tmp[,1]==max(tmp[,1]))==TRUE)))
  clust2[clust$cluster==ind]='Group 2'
  return(clust2)
}
#95789757 #95757
date()
dat2[,Cluster:=as.factor(clust.func(95757,Focus,Abstraction))]
date()


t1 <- proc.time()
t1 - t0
dim(dat2)


#### Formatting Purchase.dat table
Purchase.dat$VisitDate <- as.Date(Purchase.dat$VisitDate)
#Purchase.dat$VisitTime <- as.POSIXlt(paste(Purchase.dat$VisitDate,Purchase.dat$VisitTime))
Purchase.dat$Quantity  <- as.numeric(Purchase.dat$Quantity)
Purchase.dat$Price     <- as.numeric(Purchase.dat$Price)
Purchase.dat$Tax       <- as.numeric(Purchase.dat$Tax)
Purchase.dat$Shipping  <- as.numeric(Purchase.dat$Shipping)

# Sort Purchase.dat and dat2 the same way
setkeyv(Purchase.dat,c('VisitorID','VisitNbr','VisitClick'))
setkeyv(dat2,c('VisitorID','VisitNbr','VisitClick'))
# Merge dat2 and Purchase.dat where they have the same VisitorID,VisitNbr,and VisitClick
Purchase.dat2=Purchase.dat[dat2,nomatch=0]


#### Additional fields
setkeyv(dat2,c('VisitorID','VisitNbr','VisitClick'))
dat2[,Depth:=Abstraction]
dat2[,PurchaseFlag:=ifelse(PageClass=="Receipt",1,0)]
#dat3[Purchase.dat]$NumPurchases <- Purchase.dat[dat2,.(.N),by=.(VisitorID,VisitNbr,VisitClick),allow.cartesian=T]$N
dat2 <- merge(dat2,Purchase.dat[,.(NumPurchases=.N,TotalPrice=sum(Price),TotalTax=sum(Tax),TotalShipping=sum(Shipping),TotalQuantity=sum(Quantity)),by=.(VisitorID,VisitNbr,VisitClick)],all.x=T)
dat2[,NumPurchases:=ifelse(is.na(NumPurchases),0,NumPurchases)]
dat2[,TotalPrice:=ifelse(is.na(TotalPrice),0,TotalPrice)]
dat2[,TotalTax:=ifelse(is.na(TotalTax),0,TotalTax)]
dat2[,TotalShipping:=ifelse(is.na(TotalShipping),0,TotalShipping)]
dat2[,TotalQuantity:=ifelse(is.na(TotalQuantity),0,TotalQuantity)]

# Calculate TimeToNextPurchase for each Click
dat2[,PurchaseNum:=cumsum(PurchaseFlag) + 1 - PurchaseFlag,by="VisitorID"]
dat2[,TimeToNextPurchase:=(max(HitTime)-HitTime)/max(PurchaseFlag),by=c("VisitorID","PurchaseNum")]
#dat2[,TimeToNextPurchase:=ifelse(PurchaseNum==max(PurchaseNum),Inf,TimeToNextPurchase),by="VisitorID"]
dat2$TimeToNextPurchase[is.na(dat2$TimeToNextPurchase)] <- Inf
dat2[PurchaseFlag==1,TimeToNextPurchase:=c(diff(as.numeric(HitTime)),Inf),by="VisitorID"]

# Calculate TimeToNextVisit
dat2[VisitFirstPage=="Yes",TimeToNextVisit:=c(diff(as.numeric(HitTime)),Inf),by="VisitorID"]
dat2[,TimeToNextVisit:=TimeToNextVisit[1] + as.numeric(HitTime)-min(as.numeric(HitTime)),by=c("VisitorID","VisitNbr")]
# Calculate TimeToNextCycle
dat2[CycleFirstPage=="Yes",TimeToNextCycle:=c(diff(as.numeric(HitTime)),Inf),by="VisitorID"]
dat2[,TimeToNextCycle:=TimeToNextCycle[1] + as.numeric(HitTime)-min(as.numeric(HitTime)),by=c("VisitorID","CycleNbr")]

#dat2[VisitorID==44]


#  save(dat2,file="Data/dat.rda")
#  save(Purchase.dat,Purchase.dat2,dat2,file="Data/dat2.rda")

##### May be able to end loop here.


# # Summary statistics that I'm using to evaluate how the focus/concreteness changes have done.
# # What were the receipt pages clustered into
# summary(as.factor(dat2[PageClass=='Receipt' & Focus>=0]$Cluster))
# # What were the receipt pages clustered into where the click focus>0
# summary(as.factor(dat2[PageClass=='Receipt' & Focus>0]$Cluster))
# # What were the Shopping cart add clicks clustered into
# summary(as.factor(dat2[CartAction=='Shopping Cart Add' & Focus>=0]$Cluster))
# # What percentage of clicks are in each cluster
# dat2[,.N/nrow(dat2),by=Cluster]
# # What percent of visits that contained a Group4 click made a purchase had at least one click in Group4
# tmp=dat2[,list(ifelse('Group 4'%in%Cluster,1,0),ifelse('Receipt'%in%PageClass,1,0)),by=c('VisitorID','CycleNbr')]
# nrow(tmp[V1==1 & V2==1])/nrow(tmp[V1==1])
# # What percent of visits that contained a Group3 click made a purchase had at least one click in Group3
# tmp=dat2[,list(ifelse('Group 3'%in%Cluster,1,0),ifelse('Receipt'%in%PageClass,1,0)),by=c('VisitorID','CycleNbr')]
# nrow(tmp[V1==1 & V2==1])/nrow(tmp[V1==1])
# # What percent of visits that contained a Group2 click made a purchase had at least one click in Group2
# tmp=dat2[,list(ifelse('Group 2'%in%Cluster,1,0),ifelse('Receipt'%in%PageClass,1,0)),by=c('VisitorID','CycleNbr')]
# nrow(tmp[V1==1 & V2==1])/nrow(tmp[V1==1])
# # What percent of visits that contained a Group1 click made a purchase had at least one click in Group1
# tmp=dat2[,list(ifelse('Group 1'%in%Cluster,1,0),ifelse('Receipt'%in%PageClass,1,0)),by=c('VisitorID','CycleNbr')]
# nrow(tmp[V1==1 & V2==1])/nrow(tmp[V1==1])
# 
# # Some figures that were helpful to look at
# png('Figures/Adobe_Click3Clustering%d.png',width=18,height=10,res=100,units='in')
# ggplot(dat2[CyclePurchase!=1],aes(x=Focus,y=Abstraction,color=as.factor(Cluster)))+
#   geom_point(size=1.25,alpha=.5)+xlab('Click Focus ')+ylab('Click Concreteness')+ggtitle('All Clicks')+
#   scale_color_manual(values=c('Group 1'='grey18','Group 2'='red1','Group 3'='blue1','Group 4'='goldenrod2'),
#                      labels=c('Low Focus; Low Concreteness','High Focus; Low Concreteness','Low Focus; High Concreteness','High Focus; High Concreteness'),name='')+
#   scale_x_continuous(limits=c(0,1),breaks=seq(0,1,.25))+scale_y_continuous(limits=c(0,1),breaks=seq(0,1,.25))+
#   theme(legend.position='bottom')+guides(colour = guide_legend(override.aes = list(size=4)))
# 
# ggplot(dat2[PageClass=='Receipt'],aes(x=Focus,y=Abstraction,color=as.factor(Cluster)))+
#   geom_point(size=2,alpha=.5)+xlab('Click Focus ')+ylab('Click Concreteness')+ggtitle('Receipt Clicks')+
#   scale_color_manual(values=c('Group 1'='grey18','Group 2'='red1','Group 3'='blue1','Group 4'='goldenrod2'),
#                      labels=c('Low Focus; Low Concreteness','High Focus; Low Concreteness','Low Focus; High Concreteness','High Focus; High Concreteness'),name='')+
#   scale_x_continuous(limits=c(0,1),breaks=seq(0,1,.25))+scale_y_continuous(limits=c(0,1),breaks=seq(0,1,.25))+
#   theme(legend.position='bottom')+guides(colour = guide_legend(override.aes = list(size=4)))
# 
# ggplot(dat2[CartAction=='Shopping Cart Add'],aes(x=Focus,y=Abstraction,color=as.factor(Cluster)))+
#   geom_point(size=2,alpha=.5)+xlab('Click Focus ')+ylab('Click Concreteness')+ggtitle('Cart Add Clicks')+
#   scale_color_manual(values=c('Group 1'='grey18','Group 2'='red1','Group 3'='blue1','Group 4'='goldenrod2'),
#                      labels=c('Low Focus; Low Concreteness','High Focus; Low Concreteness','Low Focus; High Concreteness','High Focus; High Concreteness'),name='')+
#   scale_x_continuous(limits=c(0,1),breaks=seq(0,1,.25))+scale_y_continuous(limits=c(0,1),breaks=seq(0,1,.25))+
#   theme(legend.position='bottom')+guides(colour = guide_legend(override.aes = list(size=4)))
# 
# ggplot(dat2[Focus>0 & Abstraction>0 ],aes(x=Cluster,y=CycleTime))+
#   geom_boxplot()
# 
# ggplot(dat2,aes(x=Focus,group=CyclePurchase,fill=as.factor(CyclePurchase)))+
#   geom_density(alpha=.35)+xlab('Click Focus')+ggtitle('Visits w/o Purchase vs Visits w/ Purchase')+
#   scale_fill_manual(values=c('0'='black','1'='red'),labels=c('No','Yes'),name='Purchase')+
#   scale_x_continuous(limits=c(0,1),breaks=seq(0,1,.25))
# 
# ggplot(dat2,aes(x=Abstraction,group=CyclePurchase,fill=as.factor(CyclePurchase)))+
#   geom_density(alpha=.35)+xlab('Click Abstraction')+ggtitle('Visits w/o Purchase vs Visits w/ Purchase')+
#   scale_fill_manual(values=c('0'='black','1'='red'),labels=c('No','Yes'),name='Purchase')+
#   scale_x_continuous(limits=c(0,1),breaks=seq(0,1,.25))
# 
# Adobe_Figure_Lines(dat2,49577,1,200)   #498 in mine   3165254,49577 with Ram's
# dev.off()
# 
# #### Summary of click to click Patterns ----
# # This function I was hoping to  find something interesting. It is able to go through and see the path of the clusters
# cluster.summary=function(x,data){
#   tmp=list()
#   tmp[[1]]=data[(CycleClick==1 & Cluster==x[1]),TimeOnPage,by=c('VisitorID','CycleNbr')]
#   setkeyv(tmp[[1]],c('VisitorID','CycleNbr'))
#   setkeyv(data,c('VisitorID','CycleNbr'))
#   tmp1=numeric(length(x))
#   tmp2=numeric(length(x))
#   tmp2[1]=nrow(data[CycleClick==1])
#   exit=numeric(length(x))
#   ave.page.time=numeric(length(x))
#   ave.page.time[1]=mean(tmp[[1]][TimeOnPage!=0]$TimeOnPage)
#   if(length(x)==1){
#     tmp1[1]=nrow(tmp[[1]])
#     tmp3=table(data[tmp[[1]]][CycleClick==2]$CycleClick,data[tmp[[1]]][CycleClick==2]$Cluster)
#     names(tmp3)=sort(levels(data$Cluster))
#     Total=sum(tmp3)
#     exit[1]=tmp1[1]-Total
#     tmp4=matrix(c(tmp1,tmp2,tmp1/tmp2,ave.page.time,exit),nrow=length(x),ncol=5,dimnames=list(x,c('Clicked','Total','Click/Total','AvePageTime','Exited')))
#     return(list(tmp4,c(tmp3,Total=Total)))
#   }
#   for(i in 2:length(x)){
#     tmp1[i-1]=nrow(tmp[[i-1]])
#     tmp[[i]]=data[tmp[[i-1]]][CycleClick==i & Cluster==x[i]]
#     ave.page.time[i]=mean(tmp[[i]][TimeOnPage!=0]$TimeOnPage)
#     tmp2[i]=sum(table(data[tmp[[i-1]]][CycleClick==i]$CycleClick,data[tmp[[i-1]]][CycleClick==i]$Cluster))
#     exit[i-1]=tmp1[i-1]-tmp2[i]
#   }
#   tmp1[i]=nrow(tmp[[i]])
#   tmp3=table(data[tmp[[i]]][CycleClick==i+1]$CycleClick,data[tmp[[i]]][CycleClick==i+1]$Cluster)
#   names(tmp3)=sort(levels(data$Cluster))
#   Total=sum(tmp3)
#   exit[i]=tmp1[i]-Total
#   tmp4=matrix(c(tmp1,tmp2,tmp1/tmp2,ave.page.time,exit),nrow=length(x),ncol=5,dimnames=list(x,c('Clicked','Total','Click/Total','AvePageTime','Exited')))
#   return(list(tmp4,c(tmp3,Total=Total)))
# }
# summary(as.factor(dat2[CycleClick==1]$Cluster))
# visitpath=c('Group 3','Group 3','Group 3')  # give different lengths and cominations of 'Group', 1-4 to see how it changes.
# cluster.summary(visitpath,dat2)
# 
# #########################################  Look at patterns of all data
# # Another function I was hoping to find something interesting. I wanted to see the most reoccurring patterns of clusters. Wasn't very cool in the end.
# # But if you look into it an have a idea of how it could be used, here it is.
# pattern.clust.func=function(x,combins){
#   #x=Cluster
#   k=nrow(combins)
#   n=length(x)
#   x=as.character(x)
#   if(n<k){
#     return(0)
#   }
#   ind=numeric(n)
#   for(i in 1:(n-k+1)){
#     ind[i]=which(colSums(x[i:(i+k-1)]==combins)==k)
#   }
#   return(ind)
# }
# all.combn=t(expand.grid(First=sort(levels(dat2$Cluster)),Second=levels(dat2$Cluster),Third=levels(dat2$Cluster)))
# date()
# tmp=dat2[,pattern.clust.func(Cluster,all.combn),by=c('VisitorID','CycleNbr')]
# date()
# setnames(tmp,'V1','Pattern')
# all.combn=data.table(t(all.combn))
# all.combn[,Pattern:=1:nrow(all.combn)]
# setkeyv(all.combn,'Pattern')
# setkeyv(tmp,'Pattern')
# tmp=all.combn[tmp[,.N,by=Pattern]]
# tmp[N>10000]
# 
# #### look at time diff of clicks ----
# # This is how I origianally decided on 30 hours for a visit length
# # But I'm sure there are other ways to evaluate visit length (you could even try something non-time based, like once they start viewing a new category or something)
# setkeyv(dat2,c('VisitorID','CycleNbr','CycleClick'))
# time.click=function(x){
#   tmp=numeric(length(x))
#   for(i in 2:length(x)){
#     tmp[i]=as.numeric(difftime(x[i],x[i-1],units='hours'))
#   }
#   return(tmp)
# }
# tmp=dat2[,time.click(HitTime),by='VisitorID']
# png('Figures/Adobe_TimeBetweenClicks%d.png',width=18,height=10,res=100,units='in')
# ggplot(tmp[V1>.1 & V1<100],aes(x=V1))+
#   geom_density(fill='black',alpha=.35)+
#   geom_vline(xintercept=30,color='red')+
#   xlab("HitTime Difference From Click to Click (Hours)")+
#   ylab('Density')
# dev.off()
# 
# #### Plot Purchase Cycle ----
# # This is another figure that Nigel is interested in. It is because you can see the time it took to purchase and what happened inbetween.
# # This code will go through and identify Visitors that bought a product, identify the first and last clicks involved with them and then plot them
# # Similar to the Adobe_Figure_Lines function except the time stamps correspond to activity of the product of interest. 
# # order the Purchase.dat data.table
# setkeyv(Purchase.dat,c('ProductID','VisitorID'))
# # Find products that have a lot of activity by VisitorIDs
# tmp=Purchase.dat[,.N,by=c('ProductID','VisitorID')]
# # Find Products that many people view.
# tmp1=tmp[,.N,by='ProductID']
# # Find products that have the most activity
# sort(tmp1[N>1]$N,decreasing=TRUE)
# # selects the maximum activity for ProductID[1] that has 30 VisitorID that have activity on this product 
# max(dat2[ProductID%in%tmp1[N==30]$ProductID[1]]$ProductViewNbr,na.rm=TRUE)
# # View the VisitorIDs activity on ProductID[1]
# View(dat2[ProductID%in%tmp1[N==30]$ProductID[1] & VisitorID==unique(tmp[ProductID%in%tmp1[N==30]$ProductID[1]]$VisitorID)[24]])
# setkeyv(dat2,c('VisitorID','VisitNbr','VisitClick'))
# setkeyv(Purchase.dat,c('VisitorID','VisitNbr','VisitClick'))
# tmp=c(min(dat2[VisitorID==6262622 & ProductID==119004]$click),dat2[Purchase.dat[VisitorID==6262622 & ProductID==119004,c('VisitorID','VisitNbr','VisitClick','ProductID','Price'),with=FALSE]]$click)
# 
# setkeyv(dat2,c('VisitorID','VisitNbr','VisitClick'))
# Adobe_Purchase_Cycle=function(data,visitor,min_e,max_e,p.id,price){
#   tmp.dat=data[VisitorID==visitor & click>=min_e & click<=max(max_e)]
#   
#   #### Following Time bar chart
#   setkeyv(tmp.dat,c('VisitorID','VisitNbr','VisitClick'))
#   mn=min_e #1400
#   mx=max(max_e)
#   
#   gg1=ggplot(tmp.dat,aes(x=as.factor(click),y=log10(TimeOnPageInSeconds)))+
#     geom_bar(stat='identity',color='white',position='identity',size=.85)+
#     geom_bar(data=tmp.dat[CycleFirstPage=='Yes'],color='black',stat='identity',size=.85,position='identity')+
#     xlab("")+ylab(expression(atop('Time Per Event\n (Log_10 Sec)')))+ggtitle(paste('Purchase Cycle for ProductID=',p.id,' VisitorID=',visitor,' and Price=',price))+
#     #scale_x_discrete(breaks=seq(mn,mx,5))+
#     scale_y_continuous(breaks=c(seq(0,3,1),log10(3500)),labels=c(1,10,100,1000,3500))+
#     theme(legend.position="none",axis.text.x=element_blank(),axis.ticks.x=element_blank(),plot.margin=unit(c(1,1,-.85,1), "cm"))
#   
#   #### Following Page Behavior
#   gg2=ggplot(tmp.dat,aes(x=as.factor(click),y=as.factor(VisitorID),fill=as.factor(PageClass)))+
#     geom_tile()+
#     geom_tile(color='white',show_guide=FALSE,size=.85)+
#     geom_tile(data=tmp.dat[CycleFirstPage=='Yes'],color='black',size=.85,show_guide=FALSE)+
#     xlab("")+ylab('')+#scale_x_discrete(breaks=seq(mn,mx,5))+
#     scale_fill_manual(values=c('Product'='tan1','Cart'='steelblue2','Brand'='blue','Category'='yellow2','Receipt'='maroon',
#                                'Home'='salmon4','Account'='green','Search'='darkolivegreen','On Page Action'='grey',
#                                'Checkout'='violet','Information'='red','Other'='black'),
#                       labels=c('Product','Cart','Brand','Category','Receipt',
#                                'Home','Account','Search','On Page Action',
#                                'Checkout','Information','Other'),
#                       breaks=c('Product','Cart','Brand','Category','Receipt',
#                                'Home','Account','Search','On Page Action',
#                                'Checkout','Information','Other'),
#                       name='Page Class',drop=FALSE)+
#     theme(legend.position="bottom",axis.text.x=element_blank(),plot.margin=unit(c(0,1,-0.5,1), "cm"),
#           axis.text.y=element_blank(),axis.ticks.y=element_blank())+guides(fill = guide_legend(ncol=4))
#   leg1 = gtable_filter(ggplot_gtable(ggplot_build(gg2+theme(legend.position="bottom"))), "guide-box")
#   
#   #### Follow Focus and Abstraction scores
#   gg3=ggplot(tmp.dat,aes(x=1:length(click),y=Focus))+
#     geom_line(aes(color='Focus'),alpha=.5)+
#     geom_point(aes(color='Focus'),size=3,alpha=.5)+
#     geom_line(aes(y=Abstraction,color='Concreteness'),alpha=.5)+
#     geom_point(aes(y=Abstraction,color='Concreteness'),size=3,alpha=.5)+
#     scale_color_manual(values=c('Concreteness'='dodgerblue3','Focus'='firebrick2'),
#                        labels=c('Concreteness','Focus'),
#                        breaks=c('Concreteness','Focus'),name='',drop=FALSE)+
#     scale_y_continuous(breaks=seq(0,1,.25),limits=c(0,1.001))+
#     xlab("")+ylab('Focus/Concreteness')+scale_x_discrete(breaks=c(1:nrow(tmp.dat))[which(tmp.dat$ProductID==p.id | tmp.dat$click%in%max_e)],
#                                                          labels=tmp.dat[ProductID==p.id | click%in%max_e]$HitTime)+
#     theme(legend.position='bottom',plot.margin=unit(c(-.25,1,-.5,1), "cm"),#c(-1,1,-.25,1)
#           axis.text.x=element_text(size=9,vjust=0.4,angle=-90))+guides(color = guide_legend(ncol=1))
#   leg2=gtable_filter(ggplot_gtable(ggplot_build(gg3+theme(legend.position="bottom"))), "guide-box")
#   
#   #### Make Figure
#   gA <- ggplotGrob(gg1+theme(legend.position="none"))
#   gB <- ggplotGrob(gg2+theme(legend.position="none"))
#   gC <- ggplotGrob(gg3+theme(legend.position='none'))
#   maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5],gC$widths[2:5])
#   gA$widths[2:5] <- as.list(maxWidth)
#   gB$widths[2:5] <- as.list(maxWidth)
#   gC$widths[2:5] <- as.list(maxWidth)
#   leg=arrangeGrob(leg1,leg2,ncol=2,widths=c(1,.5))
#   return(grid.arrange(gA,gB,gC,leg,ncol=1,heights=c(1.5,.25,1.5,.5)))
# }
# 
# #expensive=sort(dat2$Price,decreasing=TRUE)[1:100]
# #dat2[ProductID==dat2[Price==expensive[100]]$ProductID[1]]
# x=30    #11,11,4,4,3
# y=1   #1,1,9,9,1
# z=1   #7,10,16,17,19,20,23,25,27
# p.id=119004  #tmp1[N==x]$ProductID[y] #119004
# v.id=unique(Purchase.dat[ProductID==p.id]$VisitorID)[z]
# min_event=dat2[VisitorID==v.id & ProductID==p.id & ProductViewNbr==1]$click
# max_event=dat2[Purchase.dat[VisitorID==v.id & ProductID==p.id,c('VisitorID','VisitNbr','VisitClick'),with=FALSE]]$click
# price=Purchase.dat[VisitorID==v.id & ProductID==p.id,c('VisitorID','VisitNbr','VisitClick','ProductID','Price'),with=FALSE]$Price
# Adobe_Purchase_Cycle(dat2,v.id,min_event,max_event,p.id,price)

## End
#########



