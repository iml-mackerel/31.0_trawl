#######
##
##  Plot trawl data:
##
##  Note: excluded length frequencies or other bio info 
##             -> only interested in presence/absence of mackerel
##             -> info not available for NL
#######

####### SET-UP #################################################################################################################

nam <- 'survey_trawl'
wdrdat <- paste0('Rdata/',nam,'/')
wdimg <- paste0('img/',nam)

load(file=paste0(wdrdat,'trawl.Rdata'))
trawl$month.abb <- factor(month.abb[trawl$month],levels=month.abb)

##### overall exploratory map  ---------------------------------------------------------------------------------------------------------------------------------------
xlim <- maplim(trawl$long)
ylim <- maplim(trawl$lat)
pbase <- mapbase(xlim,ylim)

p <- pbase+ 
    geom_point(data=trawl[trawl$pa==0,], aes(x = long, y = lat),size=0.4)+
    geom_point(data=trawl[trawl$pa==1,], aes(x = long, y = lat),size=0.4,col='red')+
    facet_wrap(~year)
saveplot(p,'maps_all',wdimg,c(30,30))

##### idem by year  ---------------------------------------------------------------------------------------------------------------------------------------
dummy <- lapply(unique(trawl$year),function(x){
    p <- pbase+ 
        geom_point(data=trawl[trawl$pa==0 & trawl$year==x,], aes(x = long, y = lat,col=month),size=0.8)+
        geom_point(data=trawl[trawl$pa==1 & trawl$year==x,], aes(x = long, y = lat,col=month),size=1.4,shape=2)+
        scale_color_viridis_c()
    saveplot(p,x,paste(wdimg,'annual',sep='/'),c(25,25))
    
})

##### grid with percentage  ---------------------------------------------------------------------------------------------------------------------------------------
perc <- ddply(trawl[!is.na(trawl$zone_unofficial),],c('zone_unofficial','year','month.abb'),summarise,
              perc=length(pa[pa==1])*100/length(pa),
              ave=sum(number)/length(pa),
              n=length(pa),
              np=length(pa[pa==1]))
perc[perc$np==0,'np'] <- NA

# classes for N
inwhichrange <- function(y,r)which(apply(as.matrix(r),1,function(x) y < x[2] & y >= x[1]))
cl <- matrix(c(0,10,
               10,20,
               20,50,
               50,100,
               100,200,
               200,500),ncol=2,byrow = T)
rn <- c(paste0('[',cl[-nrow(cl),1],',',cl[-nrow(cl),2],'['),paste0('[',cl[nrow(cl),1],',',cl[nrow(cl),2],']'))

perc$nc <- sapply(perc$n,inwhichrange,cl)
perc$nc <- factor(rn[perc$nc],levels=rn[1:length(rn)])

regs <- c('SS','sGSL','nGSL','wNL','sNL','GrandBanks','eNL')
p <- ggplot(perc[perc$zone_unofficial %in% regs,],aes(x=year,y=month.abb))+
    geom_tile(aes(fill=nc))+
    geom_text(aes(label=np))+
    facet_wrap(~zone_unofficial,ncol=2)+
    scale_fill_viridis_d()+
    labs(fill='Number of sets',title = 'Number of sets with mackerel')
saveplot(p,'grid_month',wdimg,c(60,30))

##### percentage fin-scale  ---------------------------------------------------------------------------------------------------------------------------------------
this <- rbind(trawl[trawl$zone_unofficial=='SS' & trawl$month.abb %in% c('Jun','Jul','Aug'),],
              trawl[trawl$zone_unofficial=='sGSL' & trawl$month.abb %in% c('Aug','Sep'),],
              trawl[trawl$zone_unofficial=='nGSL' & trawl$month.abb %in% c('Jul','Aug','Sep'),],
              trawl[trawl$zone_unofficial=='wNL' & trawl$month.abb %in% c('Jul','Aug'),],
              trawl[trawl$zone_unofficial=='sNL' & trawl$month.abb %in% c('Apr','May','Jun'),],
              trawl[trawl$zone_unofficial=='eNL',])

perc <- ddply(this[!is.na(this$zone_unofficial),],c('zone_unofficial','year'),summarise,
              perc=length(pa[pa==1])*100/length(pa),
              ave=sum(number)/length(pa),
              n=length(pa),
              np=length(pa[pa==1]))
perc[perc$np==0,'np'] <- NA

lab <- unique(this[!is.na(this$zone_unofficial),c('zone_unofficial','month.abb')])
lab <- ddply(lab,c('zone_unofficial'),summarise,month=paste(unique(month.abb),collapse='/'))


p <- ggplot(perc,aes(x=year,y=ave))+
    geom_line(size=1)+
    geom_text(data=lab,aes(x=Inf,y=Inf,label=month),hjust=2,vjust=2)+
    facet_wrap(~zone_unofficial,scale='free_y')+
    labs(y='Average number of mackerel in a set')
saveplot(p,'set_average',wdimg,c(30,10))

p <- ggplot(perc,aes(x=year,y=perc))+
    geom_line(size=1)+
    geom_text(data=lab,aes(x=Inf,y=Inf,label=month),hjust=2,vjust=2)+
    facet_wrap(~zone_unofficial,scale='free_y')+
    labs(y='Percentage of sets with mackerel')
saveplot(p,'set_percentage',wdimg,c(30,10))
