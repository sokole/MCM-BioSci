# ---------------------------------------
# load packages
# ----------------------
require(SiZer)
require(reshape2)
require(ggplot2)
require(segmented)
# ---------------------------------------
# read data
# ----------------------
dat.raw<-read.csv('DATA_RAW_gooseff_biosci.csv',
                  stringsAsFactors = FALSE)
dat.raw$id_data<-with(dat.raw, paste(data_type, var, group,sep='__'))

dat.raw<-na.omit(dat.raw)
# ---------------------------------------
# working
# ----------------------

y.list<-unique(dat.raw$id_data)

# -- look at distributions
for(i.y in y.list){
  graphics.off()
  par(mfrow=c(1,2), mar=c(3,3,1,1))
  dat <- subset(dat.raw, id_data==i.y)
  try({
    hist(dat$value, main=i.y)
    hist(log(dat$value+1), main=paste('log',i.y))
  })
  
  scan()
}

# -- transform vars where appropriate
var.trans.list<-c('total_live','PPR','uw_par','chla','disch','AFDM')

dat.trans<-dat.raw
dat.trans$var <- ifelse(dat.raw$var%in%var.trans.list, paste(dat.trans$var,'log',sep='_'),dat.trans$var)
dat.trans$value <- ifelse(dat.raw$var%in%var.trans.list, log(dat.trans$value + 1), dat.trans$value)

# log transform
# nematode dens, ppr, UW PAR, chla, disch, AFDM

ddply(
  dat.raw,
  .(id_data),
  summarise,
  neg.val.count = sum(value<0, na.rm=TRUE)
)


i.y<-y.list[3]

for (i.y in y.list){
  try({
    graphics.off()
    plot.name <- paste('Fig_SiZer_plot_',i.y,'.pdf',sep='')
    pdf(file = plot.name,
        width=6,
        height=6)
    
    dat <- subset(dat.trans, id_data==i.y)
    
    fit.lm <- lm(value ~ year,
                 data = dat)
    
    # SiZer analysis (for heat map), calculates first derivatives for moving window
    mod.siz <- SiZer(
      x = dat$year,
      y = dat$value
    )
    
    mod.lwpr<-locally.weighted.polynomial(
      x = dat$year,
      y = dat$value)
    
    
    # plot heat map
    par(mfrow=c(2,1), mar=c(2,4.5,2,2))
    p<-plot(mod.siz, main=i.y)
    abline(h=log10(mod.lwpr$h), lty=2)
    
    par(mar=c(3,4.5,0,2))
    plot(mod.lwpr, use.ess = FALSE)
    dev.off()

  })
}

