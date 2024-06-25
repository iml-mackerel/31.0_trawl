#######
##
##  Read trawl data:
##
##  Note: excluded length frequencies or other bio info here
##           
#######

####### READ #################################################################################################################

nam <- 'survey_trawl'
wddat <- paste0('data/',nam,'/')
wdrdat <- paste0('Rdata/',nam,'/')

##### .Read sGSL  ---------------------------------------------------------------------------------------------------------------------------------------

reg <- 'sGSL'
wddat.reg <- paste0(wddat,reg,'/')

f <- list.files(wddat.reg,full.names = T,pattern = '.csv')
sgsl.raw <- lapply(f,read.csv2,sep=',')
names(sgsl.raw) <- tolower(gsub(".*\\_(.+)\\.*....", "\\1", f))
lapply(sgsl.raw,dim)

# select only mackerel
sgsl.raw$detail <- sgsl.raw$detail[sgsl.raw$detail$species==70,]
sgsl.raw$catch <- sgsl.raw$catch[sgsl.raw$catch$species==70,]
lapply(sgsl.raw,dim)

# merge so that stations with no mackerel are kept
sgsl <- merge(sgsl.raw$sets,sgsl.raw$catch,all.x=T)
  #sgsl <- merge(sgsl,sgsl.raw$detail,all.x=T)
dim(sgsl)

##### .Read nGSL  ---------------------------------------------------------------------------------------------------------------------------------------

reg <- 'nGSL'
wddat.reg <- paste0(wddat,reg,'/')

### ..1) Needler/Teleost ----------------------------------------------------------------------------
f <- list.files(wddat.reg,full.names = T,pattern = 'nGSL')
ngsl.raw <- lapply(f,read.csv2)
names(ngsl.raw) <- tolower(gsub(".*\\_(.+)\\.*....", "\\1", f))
ngsl.raw <- lapply(ngsl.raw, function(x){colnames(x) <- tolower(colnames(x));x})   # unequal colnames
lapply(ngsl.raw,dim)

# select only mackerel
ngsl.raw$detail <- ngsl.raw$detail[ngsl.raw$detail$espece==572,]
ngsl.raw$catch <- ngsl.raw$catch[ngsl.raw$catch$espece==572,]
lapply(ngsl.raw,dim)

# merge so that stations with no mackerel are kept
ngsl.raw$sets$set <- 1:nrow(ngsl.raw$sets)
ngsl <- merge(ngsl.raw$sets,ngsl.raw$catch,all.x=T)
  #ngsl <- merge(ngsl,ngsl.raw$detail,all.x=T)
dim(ngsl)

### ..2) lady hammond  ----------------------------------------------------------------------------
f <- list.files(wddat.reg,full.names = T,pattern = 'LH')
laha.raw <- lapply(f,read.csv2)
names(laha.raw) <- tolower(gsub(".*\\_(.+)\\.*....", "\\1", f))
laha.raw <- lapply(laha.raw, function(x){colnames(x) <- tolower(colnames(x));x})   # unequal colnames
lapply(laha.raw,dim)

# select only mackerel
laha.raw$detail <- laha.raw$detail[laha.raw$detail$espece==572,]
laha.raw$catch <- laha.raw$catch[laha.raw$catch$espece==572,]
lapply(laha.raw,dim)

# merge so that stations with no mackerel are kept
laha.raw$sets$set <- 1:nrow(laha.raw$sets)
laha <- merge(laha.raw$sets,laha.raw$catch,all.x=T)
  #laha <- merge(laha,laha.raw$detail,all.x=T)
dim(laha)

### ..3) Gadus  ----------------------------------------------------------------------------
f <- list.files(wddat.reg,full.names = T,pattern = 'GA')
gadu.raw <- lapply(f,read.csv2)
names(gadu.raw) <- tolower(gsub(".*\\_(.+)\\.*....", "\\1", f))
gadu.raw <- lapply(gadu.raw, function(x){colnames(x) <- tolower(colnames(x));x})   # unequal colnames
lapply(gadu.raw,dim)

# select only mackerel
gadu.raw$detail <- gadu.raw$detail[gadu.raw$detail$espece==572,]
gadu.raw$catch <- gadu.raw$catch[gadu.raw$catch$espece==572,]
lapply(gadu.raw,dim)

# merge so that stations with no mackerel are kept
gadu.raw$sets$set <- 1:nrow(gadu.raw$sets)
gadu <- merge(gadu.raw$sets,gadu.raw$catch,all.x=T)
 # gadu <- merge(gadu,gadu.raw$detail,all.x=T)
dim(gadu)

### ..bind   ----------------------------------------------------------------------------
ngsl$origin <- 'needler.teleost'
laha$origin <- 'laddy.hammond'
gadu$origin <- 'gadus'
this.col <- Reduce(intersect,list(names(ngsl),names(laha),names(gadu)))
ngsl <- rbind(ngsl[,this.col],laha[,this.col],gadu[,this.col])

##### .Read NL  ---------------------------------------------------------------------------------------------------------------------------------------

reg <- 'NL'
wddat.reg <- paste0(wddat,reg,'/')

# read
get(load(paste0(wddat.reg,"set_details_2018-05-23.Rdata"))) 
ms<-setrec[which(setrec$which.survey=="multispecies"),]
nl <-merge(ms,setdet,all.x=TRUE)

# select only mackerel
nl <- nl[nl$spec==572 | is.na(nl$spec),]    # na are rows with rec=5 (details for set, without species info)
nl <- nl[!is.na(nl$rec),]                   # records with no additional data and usually a different gear type. 

##### .Read NS  ---------------------------------------------------------------------------------------------------------------------------------------

reg <- 'NS'
wddat.reg <- paste0(wddat,reg,'/')

            # for extraction from database Maritimes
            # pac <- 'Mar.datawrangling'
            # if(!pac %in% utils::installed.packages()[,"Package"]){
            #     library(devtools)
            #     install_github('Maritimes/Mar.datawrangling')
            # }
            #
            # library(Mar.datawrangling)
            # library(RODBC)
            # get_data('rv') # needs password. Anyway, this should result in the Rdata currently present.

f <- list.files(wddat.reg,full.names = T,pattern='RData')
ns.raw <- lapply(f,function(x)get(load(x)))
names(ns.raw) <- tolower(gsub(".*/(.+)\\.*......", "\\1", f))
lapply(ns.raw,dim)

# select only mackerel
ns.raw$rv.gscat <- ns.raw$rv.gscat[ns.raw$rv.gscat$SPEC==70,]
ns.raw$rv.gsdet <- ns.raw$rv.gsdet[ns.raw$rv.gsdet$SPEC==70,]
lapply(ns.raw,dim)

# merge so that stations with no mackerel are kept
ns <- merge(ns.raw$rv.gsinf,ns.raw$rv.gscat,all.x=T)
 #ns <- merge(ns,ns.raw$rv.gsdet,all.x=T)
ns <- merge(ns,ns.raw$rv.gsmissions,all.x=T)
dim(ns)

####### MERGE #################################################################################################################
### .Reformat -------------

# prep sgsl
my.sgsl <- sgsl[,c('vessel.str','unique.id','year','month','day','latitude','longitude','set.number','number.caught','weight.caught')]

# prep ngsl
ngsl$date_deb <- as.Date(ngsl$date_deb)
ngsl$year <- lubridate::year(ngsl$date_deb)
ngsl$month <- lubridate::month(ngsl$date_deb)
ngsl$day <- lubridate::day(ngsl$date_deb)
ngsl$unique.id <- paste0(ngsl$year,".",ngsl$no_rel)
ngsl$lat_deb2 <-  (as.numeric(substr(ngsl$lat_deb , 1,2)) + (as.numeric(ngsl$lat_deb) - as.numeric(substr(ngsl$lat_deb,1, 2)) * 100)/60)               # as in mar.datawrangling::data_tweaks
ngsl$lon_deb <- gsub("[[:space:]]", "", ngsl$lon_deb)
ngsl$lon_deb2 <- (as.numeric(substr(ngsl$lon_deb,1, 2)) + (as.numeric(ngsl$lon_deb) - as.numeric(substr(ngsl$lon_deb,1, 2)) * 100)/60) * -1       # as in mar.datawrangling::data_tweaks
my.ngsl <- ngsl[,c('origin','unique.id','year','month','day','lat_deb2','lon_deb2','set','nb_capt','pds_ech')]

# prep nl
my.nl <- nl[,c('vessel','trip','survey.year','month','day','lat.start','long.start','set','number','weight','rec')]
my.nl <- merge(my.nl[my.nl$rec==5,c(1:8)],my.nl[my.nl$rec==6,-ncol(my.nl)],all.x=T)                                          # fuse rec 5  and rec 6 (not separate lines)

# prep ns
ns$date_deb <- as.Date(ns$SDATE)
ns$year <- lubridate::year(ns$date_deb)
ns$month <- lubridate::month(ns$date_deb)
ns$day <- lubridate::day(ns$date_deb)
ns <- as.data.frame(apply(ns, 2, gsub, patt=",", replace="."))
ns <- as.data.frame(apply(ns, 2, trimws, which='both'))
ns$lat <-  (as.numeric(substr(ns$SLAT, 1,2)) + (as.numeric(ns$SLAT) - as.numeric(substr(ns$SLAT,1, 2)) * 100)/60)               # as in mar.datawrangling::data_tweaks
ns$long <- (as.numeric(substr(ns$SLONG,1, 2)) + (as.numeric(ns$SLONG) - as.numeric(substr(ns$SLONG,1, 2)) * 100)/60) * -1       # as in mar.datawrangling::data_tweaks
my.ns <- ns[,c('VESEL','MISSION','year','month','day','lat','long','SETNO','TOTNO','TOTWGT')]
my.ns$TOTWGT <- as.numeric(sub(',', '.', my.ns$TOTWGT, fixed = TRUE))

### .bind -------------
trawl <- list(my.ngsl,my.sgsl,my.nl,my.ns)

regs <- c('ngsl','sgsl','nl','ns')
trawl <- lapply(1:length(regs), function(x) {
    colnames(trawl[[x]]) <- c('vessel','mission','year','month','day','lat','long','set','number','weight') # provide same colnames
    x <- cbind(reg=regs[x],trawl[[x]])                                                                                # add region col
    return(x)
})
trawl <- do.call('rbind',trawl)
trawl <- type.convert(trawl)
trawl$id <- with(trawl,paste(mission,set,vessel,sep='_'))
length(unique(trawl$id[duplicated(trawl$id )]))
trawl[is.na(trawl$number),c('number','weight')] <- 0                # if number is NA than weight also is
trawl$pa <- ifelse(trawl$number==0,0,1)
trawl$lat <- ifelse(trawl$lat<0,trawl$lat*-1,trawl$lat)
trawl$long <- ifelse(trawl$long>0,trawl$long*-1,trawl$long)         # a hacky way to correct them all at once
trawl <- trawl[!is.na(trawl$year),]                                 # 2 entries from NS
trawl <- trawl[!is.na(trawl$lat),]                                  # 3 entries (1 ngsl and 2 ns): seem to be tests of some sorts?
trawl <- trawl[!is.na(trawl$long),]                                 # 1 entry for ngsl

### add nafo + grouping
thisproj <- "+proj=longlat +datum=NAD83" # NAD83 (Canadian default)
nafo <- shapefile("data/shapefiles/nafo/Divisions.shp")
nafo <- spTransform(nafo, CRS(thisproj))

points <- SpatialPoints(trawl[,c('long','lat')], proj4string=CRS(thisproj))
pointsnafo <- over(points, nafo)
trawl$nafo <- pointsnafo$ZONE    # two points that could not be appointed a zone. leave them

keyzone <- readexcel(file='data/bio/biocodes.xlsx',2)
keyzone <- unique(rbind(keyzone[,c('nafo','zone_unofficial')],data.frame(nafo='3Ps',zone_unofficial='sNL'),data.frame(nafo='3Pn',zone_unofficial='sNL')))
trawl <- merge(trawl,keyzone,all.x=T)
trawl$zone_unofficial <- factor(trawl$zone_unofficial,levels=rev(unique(trawl$zone_unofficial)))

### save
save(trawl,file=paste0(wdrdat,'trawl.Rdata'))

    