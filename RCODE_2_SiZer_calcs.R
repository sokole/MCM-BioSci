# -- load packages
library(SiZer)
library(ggplot2)
library(scales)
library(dplyr)

# -- load data
load('DATA_for_plots.rda')

# ------------------------------------------------------
# -- get SiZer grids for each data set
# -------------------------------
y.list<-unique(dat.trans$id_data)
dat.siz.plot.long<-data.frame()
for(i.y in y.list){
  dat.temp <- subset(dat.trans, id_data==i.y)
  
  # SiZer analysis (for heat map), calculates first derivatives for moving window
  mod.siz <- SiZer(
    x = dat.temp$year,
    y = dat.temp$value
  )
  
  siz.long<-data.frame(
    expand.grid(year=mod.siz$x.grid,
                log10_h=log10(mod.siz$h.grid)),
    slope = array(expand.grid(t(mod.siz$slopes))))
  siz.long$slope[siz.long$slope==2]<-NA
  siz.long<-na.omit(siz.long)
  
  dat.siz.plot.long<-rbind(
    dat.siz.plot.long,
    data.frame(
      id_data = i.y,
      ecosystem_type = dat.temp[1,]$ecosystem_type,
      data_type = dat.temp[1,]$data_type,
      var = dat.temp[1,]$var,
      group = dat.temp[1,]$group,
      units = dat.temp[1,]$units,
      panel_label = dat.temp[1,]$panel_label,
      siz.long,
      stringsAsFactors = FALSE
    )
  )
}

write.csv(dat.siz.plot.long,'dat_2_sizer_data_long.csv')

