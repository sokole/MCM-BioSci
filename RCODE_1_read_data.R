# ---------------------------------------
# load packages
# ----------------------
require(SiZer)
require(reshape2)
require(ggplot2)

# ---------------------------------------
# read data
# ----------------------
dat.raw<-read.csv('DATA_RAW_gooseff_biosci.csv',
                  stringsAsFactors = FALSE)

# ---------------------------------------
# working
# ----------------------

# -- choose response var
y.list<-unique(dat.raw$old_var)
i.y <- 'air_temp'

i.y<-y.list[3]

for (i.y in y.list){
  dat <- subset(dat.raw, old_var==i.y)
  
  # SiZer analysis (for heat map), calculates first derivatives for moving window
  mod.siz <- SiZer(
    x = dat$year,
    y = dat$value
  )
  
  # plot heat map
  p<-plot(mod.siz)
  
  scan()
}

