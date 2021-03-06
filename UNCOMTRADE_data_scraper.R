
'
script to scrape commodity trade data from UNCOMTRADE API and combine with other datasets related to control variables and economic data.
'
cwd<-"C:/data"
setwd(cwd)
getwd()
library("rjson")
library("reshape2")
memory.limit(15000)

#below function from http://comtrade.un.org/data/Doc/api/ex/r
get.Comtrade <- function(url="http://comtrade.un.org/api/get?"
                         ,maxrec=500
                         ,type="C"
                         ,freq="A"
                         ,px="HS"
                         ,ps="now"
                         ,r
                         ,p
                         ,rg="all"
                         ,cc="TOTAL"
                         ,fmt="json"
)
{
  string<- paste(url
                 ,"max=",maxrec,"&" #maximum no. of records returned
                 ,"type=",type,"&" #type of trade (c=commodities)
                 ,"freq=",freq,"&" #frequency
                 ,"px=",px,"&" #classification
                 ,"ps=",ps,"&" #time period
                 ,"r=",r,"&" #reporting area
                 ,"p=",p,"&" #partner country
                 ,"rg=",rg,"&" #trade flow
                 ,"cc=",cc,"&" #classification code
                 ,"fmt=",fmt        #Format
                 ,sep = ""
  )
  
  if(fmt == "csv") {
    raw<- read.csv(string,header=TRUE)
    return (raw)
  } else {
    if(fmt == "json" ) {
      raw.data<- fromJSON(file=string)
      data<- raw.data$dataset
      validation<- unlist(raw.data$validation, recursive=TRUE)
      ndata<- NULL
      if(length(data)> 0) {
        var.names<- names(data[[1]])
        data<- as.data.frame(t( sapply(data,rbind)))
        ndata<- NULL
        for(i in 1:ncol(data)){
          data[sapply(data[,i],is.null),i]<- NA
          ndata<- cbind(ndata, unlist(data[,i]))
        }
        ndata<- as.data.frame(ndata)
        colnames(ndata)<- var.names
      }
      return(list(validation=validation,data =ndata))
    }
  }
}
#the API can only be pinged once every second. pausefor(x) pauses pinging
#from http://stackoverflow.com/questions/1174799/how-to-make-execution-pause-sleep-wait-for-x-seconds-in-r
pausefor <- function(x)
{
  p1 <- proc.time()
  Sys.sleep(x)
  proc.time() - p1 # The cpu usage should be negligible
}

######code to ping the API
ping<-function(y_,comms_)
{
  temp<-data.frame()  
  for (comm in comms_){
    news<-try(get.Comtrade(r="all",p="0",ps=y_,rg="1%2C2",cc=comm,max="50000",fmt="csv"), silent=T)
    temp<-rbind(temp, news)
    print (comm)
    pausefor(36.1) #wait 36.1 secs so that it doesn't exceed 100 pings per hour
  }
  return (temp)
}

#list of commodities. only 20 can be called at a time #comms<-c("01","02","03","04","05","06","07","08","09",seq("10","100"))
comms<-list("01%2C02%2C03%2C04%2C05%2C06%2C07%2C08%2C09%2C10%2C11%2C12%2C13%2C14%2C15%2C16%2C17%2C18%2C19%2C20",
            "21%2C22%2C23%2C24%2C25%2C26%2C27%2C28%2C29%2C30%2C31%2C32%2C33%2C34%2C35%2C36%2C37%2C38%2C39%2C40",
            "41%2C42%2C43%2C44%2C45%2C46%2C47%2C48%2C49%2C50%2C51%2C52%2C53%2C54%2C55%2C56%2C57%2C58%2C59%2C60",
            "61%2C62%2C63%2C64%2C65%2C66%2C67%2C68%2C69%2C70%2C71%2C72%2C73%2C74%2C75%2C76%2C77%2C78%2C79%2C80",
            "81%2C82%2C83%2C84%2C85%2C86%2C87%2C88%2C89%2C90%2C91%2C92%2C93%2C94%2C95%2C96%2C97%2C98%2C99%2C100",
            "2710%2C2709%2C2711") #these 27XX codes are oil and natural gas exports

#can call maximum of 5 years at once. data starts 1989
#copy these into the ps= VALUE to ping these five years
#it runs much faster with ony five years at a time with no looping over yrs<-c(seq("1962","2015"))
yrs<-list("1989","1990%2C1991%2C1992%2C1993%2C1994",
          "1995%2C1996%2C1997%2C1998%2C1999",
          "2000%2C2001%2C2002%2C2003%2C2004",
          "2005","2006","2007","2008","2009",#these are problematic
          "2010%2C2011%2C2012%2C2013%2C2014")

#####api call code loops over years, pinging each loop
data<-data.frame()
for (y in yrs){
  new_data<-ping(toString(y),comms)
  data<-rbind(data,new_data)
  print (y)
}

#delete duplicate entries
data<-unique(data, incomparables = FALSE)
write.csv(data,file="UNCOMTRADE_tradevalues_fullset.csv")

#then filter and throw out entries for uninteresting countries 
#to narrow down to countries actually in the analysis
#these are interesting countries:
countries<-list("4","8","12","24","32","51","36","40","31","50","112","56","204","64","68",
                "70","72","76","96","100","854","108","116","120","124","140","148",
                "152","156","170","180","178","188","384","191","196","203","200",
                "208","262","626","218","818","222","226","232","233","231","246",
                "251","266","270","268","276","278","280","288","300","320","324",
                "340","348","352","699","360","364","368","372","376","381","392",
                "400","398","404","410","414","417","418","428","422","426","430",
                "434","440","442","450","454","458","466","470","478","484","498",
                "496","504","508","104","516","524","528","554","558","562","566",
                "886","579","512","586","591","598","600","604","608","616","620",
                "634","642","643","646","682","686","694","703","705","706","710",
                "720","724","144","729","748","752","757","760","762","834","764",
                "768","788","792","795","800","826","804","784","858","842","860",
                "862","866","704","887","890","894","716","868","841","736","810",
                "230","58")
data2<-data[data$Reporter.Code %in% countries, ]

#save dataset filtered by country
write.csv(data2,file="UNCOMTRADE_subset.csv")

#subset to just the key variables
# using subset function 
data3 <- subset(data2, select=c(Year,Reporter.Code,Reporter.ISO,Reporter,
                                Trade.Flow.Code,Trade.Flow,
                                Commodity.Code,Commodity,
                                Trade.Value..US..))
#data3<-read.csv("UNCOMTRADE_subset_keyvars.csv")
write.csv(data3,file="UNCOMTRADE_subset_keyvars.csv")

#transform to wide form with years wideners
#tradedata from long-form now or turn into format that I can manipulate in excel
data3$value<-data3$Trade.Value..US..
data3$variable<-data3$Year
data4<-dcast(data3, Reporter.Code+Reporter.ISO+Reporter+Trade.Flow.Code+Trade.Flow+Commodity.Code+Commodity ~ variable, id=1:6)
write.csv(data4,file="UNCOMTRADE_forzstats_transformed.csv")
remove(data,data2)

#NOTE these are in current USD for the year reported, so they are converted to 2005 USD in excel
#source is http://unstats.un.org/unsd/tradekb/Knowledgebase/Calculation-of-dollar-value-in-trade-statistics-Current-value-or-constant-dollar-value
#uses the CPI data from the Federal Reserve of st. louis online portal

#transform excel z-stats data into long form
#for each of these, do following
#1 melt into long form by country years
#2 cast into wide form like data 4 with commodity codes as columns
#3 rename commodity codes at top to indicate the z-stats (THIS IS KEY so not to wrongly merge)
#4 merge to data4, is wide form by commodities codes which has commodity trade values

#this function melts from coutries by wide-form on years to country years by wide form on commodities

melt_it <-function(z,label){
  z<-subset(z,select=c(Reporter.ISO, Reporter.Code, Trade.Flow, Commodity.Code, 8:(ncol(z))))
  for (y in 1985:2014){
    names(z)[names(z) == paste("X",toString(y),sep="")] <- toString(y)
  }
  #the index might be wrong
  z_melt<-melt(z,id=1:4, measure=5:ncol(z))
  names(z_melt)[names(z_melt) == "variable"] <- "Year"
  #need to combine import code and comm code for unique variable
  #change so there is leading 0 for single-digit commodity codes
  z_melt$Commodity.Code <- sprintf("%02d", z_melt$Commodity.Code)
  z_melt$variable<-paste(label, z_melt$Trade.Flow, z_melt$Commodity.Code, sep = "_")
  names(z_melt)[names(z_melt) == "Commodity.Code"] <- "variable"
  #rearrange columns for dcast
  z_melt<-z_melt[,c(5,1,2,7,6)]
  #for some reason there are many duplicates
  z_melt<-unique(z_melt, incomparables = FALSE)
  z_melt<-na.omit(z_melt)
  z_melt<-dcast(z_melt, Year+Reporter.ISO+Reporter.Code ~ variable)
  return(z_melt)
}

#this lappy call gets the zstats from csvs and casts into wide from with commodities columns
zstats<<-lapply(list("03","05","07","10"),function(i){
  #assign(paste("z",i,sep=""),read.csv(paste("zstats_",i,".csv",sep="")) )
  #maybe just define this locally
  z_<-read.csv(paste("zstats_",i,".csv",sep=""),na.strings=c("NA", ""))
  #call melt_it function on z_
  z_melt<-melt_it(z_,paste("zstat_", i,sep=""))
  #use this code to spot check
  #z_melt[z_melt[,"variable"]=="1_Import" & z_melt[,"Reporter.ISO"]=="LKA" & z_melt[,"Year"]=="1994",]
  return(z_melt)
})
#for some reason 2709 is missing from zstats[[4]], so the next few lines add "NA"s here
zstats[[4]]$"zstat_10_Export_2709"<-NA
x<-grep("zstat_10_Export_2710", colnames(zstats[[4]]))
zstats[[4]]<-cbind(zstats[[4]][1:(x-1)],
           zstats[[4]][ncol(zstats[[4]])],
           zstats[[4]][(x:(ncol(zstats[[4]])-1))])

#this next Reduce would merge them all together without duplicates
#zstats <- Reduce(function(...) merge(..., all=T), zstats)
write.csv(Reduce(function(...) merge(..., all=T), zstats),"UNCOMTRADE_justzstats.csv")
tradevalues<-melt_it(data4,"tradevalueUSD")
#remove(data3,data4)




##Start adding covariates
data6<-tradevalues[,1:3]
#add PRIO codes
prio<-read.csv("PRIO_to_Comtrade_codes.csv")
data6<-merge(data6,prio,by="Reporter.Code")
names(data5)[names(data5) == 'Reporter.x'] <- 'Reporter'

#add GDP from world bank data
gdp<-read.csv("wb_GDP_2005USD.csv")
pop<-read.csv("wb_pop.csv")

#add population data from World bank
for (y in 1985:2014){
  names(gdp)[names(gdp) == paste("X",toString(y),sep="")] <- toString(y)
  names(pop)[names(pop) == paste("X",toString(y),sep="")] <- toString(y)
}

gdp<-melt(gdp,id=1)
names(gdp)[names(gdp) == "variable"] <- "Year"
names(gdp)[names(gdp) == "value"] <- "gdp_2005usd"

pop<-melt(pop,id=1)
names(pop)[names(pop) == "variable"] <- "Year"
names(pop)[names(pop) == "value"] <- "pop"
pop$lpop<-eval(log(pop$pop))

wb<-merge(gdp,pop)

#NOTE this the World Bank Data, so we have to append the codebook to convert WB iso (WBA3) to
#the UN ISO codes. Alternative data is the gleditch and ward.
wb_to_iso<-read.csv("wb_to_iso.csv")
wb<-merge(wb,wb_to_iso)

###merge wb data to whole dataset
data6<-merge(data6,wb)

#add conflict data
#conflict data is INTRAstate from UCDP/PRIO Monadic Conflict Onset and Incidence Dataset
conflicts<-read.csv("124922_1onset-conf2014.csv")
names(conflicts)[names(conflicts) == 'year'] <- 'Year'
names(conflicts)[names(conflicts) == 'gwno'] <- 'PRIO.Code'
data6<-merge(data6,conflicts,id=c("Year","PRIO.Code"),all.x = TRUE)

#proj_330 is maier's old compilation. other variables may be wrong
#but the ethnic/ling variable is time invariant and therefore useful
proj<-read.csv("proj_330.csv") 
names(proj)[names(proj) == 'gwno'] <- 'PRIO.Code'
names(proj)[names(proj) == 'Abbrev'] <- 'PRIO.ISO'
data6<-merge(data6,proj,id=c("Year","PRIO.Code"),all.x = TRUE)

#get polity4 data
#we only need the first 12 variables, so drop others
polity<-read.csv("p4v2014.csv")
polity<-polity[,1:12]
names(polity)[names(polity) == 'ccode'] <- 'PRIO.Code'
names(polity)[names(polity) == 'scode'] <- 'PRIO.ISO'
names(polity)[names(polity) == 'year'] <- 'Year'
data6<-merge(data6,polity,id=c("Year","PRIO.Code"),all.x = TRUE)

#add terrorist incident data
terror<-read.csv("HCBTE_sums.csv")
for (y in 1990:2014){
  names(terror)[names(terror) == paste("X",toString(y),sep="")] <- toString(y)
}
terror<-melt(terror,id="PRIO.ISO")
names(terror)[names(terror) == "variable"] <- "Year"
names(terror)[names(terror) == "value"] <- "terrorbomb_deaths"
data6<-merge(data6,terror,id=c("Year","PRIO.Code"),all.x = TRUE)
data6$"terrorbomb_deaths"[is.na(data6$"terrorbomb_deaths")] <- 0

write.csv(data6,file="UNCOMTRADE_controls.csv")
remove(polity,proj,terror,conflicts,pop,prio)

####

#now calculate percent GDP for all of the tradevalue flows
data5<<-tradevalues[,1:3]
data5<-merge(data5,wb_to_iso,all.x = TRUE)
data5<-merge(data5,gdp,all.x = TRUE)
data5<-merge(data5,tradevalues,all.x = TRUE)
cols<-colnames(data5[,6:length(colnames(data5))])

for (i in cols){
  data5[i]  <- data5[i] / data5["gdp_2005usd"]
  #replaces with new column title
  names(data5)[names(data5)==i] <- paste("pctGDP_",substr(i,15,nchar(i)),sep="")
}

#print the percent GDP into its own file for creating pie charts after stata analysis
write.csv(data5,file="UNCOMTRADE_pct_gdp.csv")

cols<<-colnames(data5[,6:length(colnames(data5))])
#now use data5 to weight the zstats
#loop over the pctGDP then grab the zstat column based on the import/export
x<<-list("03",'05','07','10')
ls<-lapply(list(1,2,3,4),function(z){
  data7<-merge(data5,zstats[[z]],all.x = TRUE)
  for (i in cols){
    #this uses colnam to determine trade flow
    flow<-substr(i,8,13)
    comcode<-substr(i,15,nchar(i))
    data7[paste("zstat",x[z],flow,comcode,sep="_")]<-data7[i] * data7[paste("zstat",x[z],flow,comcode,sep="_")]
    names(data7)[names(data7)==paste("zstat",x[z],flow,comcode,sep="_")] <- paste("w_zstat",x[z],flow,comcode,sep="_")
    #check to make sure there are some negative numbers in zstats that they imported correctly  
  }
  return(data7)
})


final_data<-data6
for (flow in list("Export","Import")){
  for (z in list(1,2,3,4)){
    #find starting and stopping indexes, in between which the rows are summed
    st<-grep(paste("w_zstat",x[z],flow,"01",sep="_"),colnames(ls[[z]]))
    sp<-grep(paste("w_zstat",x[z],flow,"99",sep="_"),colnames(ls[[z]]))
    ls[[z]]$z_index<-rowSums(ls[[z]][,st:sp],na.rm=TRUE)
    #this adds NAs if the row has no data
    ls[[z]][,"z_index"][ls[[z]][,"z_index"] == 0] <- NA
    names(ls[[z]])[names(ls[[z]])=="z_index"] <- paste("z_index",x[z],flow,sep="_")
    #might need to normalize z_index because commodity prices are always decreasing
    #but the probability of conflict is always decreasing too, so maybe a positive bias for the Hypothesis
    #clean up dataframes by removing pct_gdp and zstats
    #View(ls[[z]][paste("z_index",x[z],sep="_")])
    tomerge<-cbind(ls[[z]][1:5],ls[[z]][paste("z_index",x[z],flow,sep="_")])
    final_data<-merge(final_data,tomerge,all.x = TRUE)
  }
}
#now data7 will have all pct CDP and weighted zstats
data7<-Reduce(function(...) merge(..., all=T), data7)


#add the final dependent variables from governance indicators
polstab<-read.csv("wb_polstab.csv")
ruleoflaw<-read.csv("wb_ruleoflaw.csv")
for (y in 1996:2014){
  names(polstab)[names(polstab) == paste("X",toString(y),sep="")] <- toString(y)
  names(ruleoflaw)[names(ruleoflaw) == paste("X",toString(y),sep="")] <- toString(y)
}

polstab<-melt(polstab,id=1)
names(polstab)[names(polstab) == "variable"] <- "Year"
names(polstab)[names(polstab) == "value"] <- "polstab"

ruleoflaw<-melt(ruleoflaw,id=1)
names(ruleoflaw)[names(ruleoflaw) == "variable"] <- "Year"
names(ruleoflaw)[names(ruleoflaw) == "value"] <- "ruleoflaw"

###merge wb data to whole dataset
final_data<-merge(final_data,polstab,all.x = TRUE)
final_data<-merge(final_data,ruleoflaw,all.x = TRUE)

remove(polstab,ruleoflaw,gdp,tomerge,tradevalues,wb,wb_to_iso,cols,comcode,flow,i,ls,sp,st,x,z)

write.csv(final_data,"maier_thesis_UNCOMTRADE_full.csv")

write.csv(merge(final_data,zstats[[1]],all.x=TRUE),"maier_thesis_UNCOMTRADE_zstats.csv")
write.csv(merge(final_data,Reduce(function(...) merge(..., all=T), zstats),all.x=TRUE), "maier_thesis_UNCOMTRADE_allzstats.csv")





