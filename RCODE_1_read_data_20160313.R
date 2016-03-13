# ---------------------------------------
# load packages
# ----------------------
require(SiZer)
require(plyr)
# ---------------------------------------
# read data and data prep
# ----------------------
dat.raw<-read.csv('DATA_RAW_gooseff_biosci3.csv',
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

save(dat.raw, dat.trans, var.trans.list, file = 'DATA_for_plots.rda')
