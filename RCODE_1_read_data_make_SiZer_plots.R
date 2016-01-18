# ---------------------------------------
# load packages
# ----------------------
require(SiZer)
require(plyr)
# ---------------------------------------
# read data and data prep
# ----------------------
dat.raw<-read.csv('DATA_RAW_gooseff_biosci.csv',
                  stringsAsFactors = FALSE)

# -- make a label variable to distinguish data subsets for SiZer models
dat.raw$id_data<-with(dat.raw, paste(data_type, var, group,sep='__'))

# -- get rid of NAs
dat.raw<-na.omit(dat.raw)

# ---------------------------------------
# Explore data
# ----------------------

# -- list for looping
y.list<-unique(dat.raw$id_data)
# 
# # -- look at distributions -- decide what should be log transformed
# for(i.y in y.list){
#   graphics.off()
#   par(mfrow=c(1,2), mar=c(3,3,1,1))
#   dat <- subset(dat.raw, id_data==i.y)
#   try({
#     hist(dat$value, main=i.y)
#     hist(log(dat$value+1), main=paste('log',i.y))
#   })
#   
#   scan() #pause loop
# }

# -- transform vars where appropriate, make new data frame
var.trans.list<-c('total_live','PPR','uw_par','chla','disch','AFDM')

dat.trans<-dat.raw
dat.trans$var <- ifelse(dat.raw$var%in%var.trans.list, paste(dat.trans$var,'log',sep='_'),dat.trans$var)
dat.trans$value <- ifelse(dat.raw$var%in%var.trans.list, log(dat.trans$value + 1), dat.trans$value)

# log transform
# nematode dens, ppr, UW PAR, chla, disch, AFDM

# -- check for negative falues -- only temp should have negatives
ddply(
  dat.raw,
  .(id_data),
  summarise,
  neg.val.count = sum(value<0, na.rm=TRUE)
)


# -- SiZer analysis loop
for (i.y in y.list){
  try({
    graphics.off()
    
    # -- plot name
    plot.name <- paste('Fig_SiZer_plot_',i.y,'.pdf',sep='')
    
    # -- set plot dimensions in inches
    pdf(file = plot.name,
        width=6,
        height=6)
    
    # -- subset data -- using transformed data
    dat <- subset(dat.trans, id_data==i.y)
    
    # SiZer analysis (for heat map), calculates first derivatives for moving window
    mod.siz <- SiZer(
      x = dat$year,
      y = dat$value
    )
    
    # locally weighted polynomial regression
    mod.lwpr<-locally.weighted.polynomial(
      x = dat$year,
      y = dat$value)
    
    
    # -- plot heat map
    par(mfrow=c(2,1), mar=c(2,4.5,2,2))
    p<-plot(mod.siz, main=i.y)
    abline(h=log10(mod.lwpr$h), lty=2)
    
    # -- scatter plot with locally-weighted polynomial
    par(mar=c(3,4.5,0,2))
    plot(mod.lwpr, use.ess = FALSE)
    dev.off()

  })
}

