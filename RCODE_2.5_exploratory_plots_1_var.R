
# i.y <- 'freeze_thaw_cycles__FTC__SOIL'
i.y <- 'climate_data__O3__OZONE'

y.list<-unique(dat.raw$id_data)

dat <- subset(dat.raw, id_data==i.y)

graphics.off()

# -- plot name
plot.name <- paste('Fig_SiZer_plot_',i.y,'.pdf',sep='')
# 
# # -- set plot dimensions in inches
# pdf(file = plot.name,
#     width=6,
#     height=6)
# #     windows(width = 6, height = 6)

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