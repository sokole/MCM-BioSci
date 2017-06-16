#####################
# -- load packages
library(ggplot2)
library(scales)
library(grid)
library(stringr)
library(cowplot)

############################################
# -- read data ####
############################################
if(! exists('dat.siz.plot.long')){
  dat.siz.plot.long<-read.csv('dat_2_sizer_data_long.csv')
  
  # -- subset out unecessary flows
  dat.siz.plot.long<-subset(dat.siz.plot.long, 
                            !(var=='disch_log' & !group=='Total_FRX_basin'))
}

data.types<-as.character(unique(dat.siz.plot.long$data_type))
############################################

#######################
# panel A - summer air temp and solar flux - 2 panels ####
i.data.type <- 'air_temp_hoare_met'

# -- subset data
d.plot<-subset(dat.siz.plot.long,
               data_type==i.data.type)

d.plot$panel_label <- gsub('\\n','\n',d.plot$panel_label, fixed = TRUE)

plot_A <- ggplot(d.plot, aes(year, log10_h, fill=slope)) + 
  geom_raster() + 
  scale_fill_gradientn(colours = c('red2','gray','green3')) +
  facet_wrap( ~ panel_label) +
  geom_vline(xintercept = 2002, color = 'black', lty=5, lwd=1) +
  theme_bw() + 
  xlim(c(1990,2013.1)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        # axis.text.x = element_text(angle = 90, vjust = .5)) +
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 10),
        plot.margin = unit(c(1,.2,.2,.2), "lines")) +
  # labs(title = capitalize(gsub('_',' ',i.data.type))) +
  theme(strip.text.y = element_text(size = 8),
        strip.text.x = element_text(size = 10)) +
  #     ylab(expression(paste('Bandwidth',~~(log[10]*h)))) +
  # ylab('Bandwidth (log10 h)\n') +
  # xlab('Year') +
  guides(fill = FALSE)

#######################
# panel B - streamflow - 1 panel
i.data.type <- 'frx_basin_dsch'

# -- subset data
d.plot<-subset(dat.siz.plot.long,
               data_type==i.data.type)

d.plot$panel_label <- gsub('\\n','\n',d.plot$panel_label, fixed = TRUE)

plot_B <- ggplot(d.plot, aes(year, log10_h, fill=slope)) + 
  geom_raster() + 
  scale_fill_gradientn(colours = c('red2','gray','green3')) +
  facet_wrap( ~ panel_label) +
  geom_vline(xintercept = 2002, color = 'black', lty=5, lwd=1) +
  theme_bw() + 
  xlim(c(1990,2013.1)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        # axis.text.x = element_text(angle = 90, vjust = .5)) +
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 10),
        plot.margin = unit(c(1,.2,.2,.2), "lines")) +
  # labs(title = capitalize(gsub('_',' ',i.data.type))) +
  theme(strip.text.y = element_text(size = 8),
        strip.text.x = element_text(size = 10)) +
  #     ylab(expression(paste('Bandwidth',~~(log[10]*h)))) +
  # ylab('Bandwidth (log10 h)\n') +
  # xlab('Year') +
  guides(fill = FALSE)

#######################
# panel C - cyano mats - 2 panels
i.data.type <- 'stream_mat_biomass'

# -- subset data
d.plot<-subset(dat.siz.plot.long,
               data_type==i.data.type)

d.plot$panel_label <- gsub('\\n','\n',d.plot$panel_label, fixed = TRUE)

plot_C <- ggplot(d.plot, aes(year, log10_h, fill=slope)) + 
  geom_raster() + 
  scale_fill_gradientn(colours = c('red2','gray','green3')) +
  facet_wrap( ~ panel_label) +
  geom_vline(xintercept = 2002, color = 'black', lty=5, lwd=1) +
  theme_bw() + 
  xlim(c(1990,2013.1)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        # axis.text.x = element_text(angle = 90, vjust = .5)) +
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 10),
        plot.margin = unit(c(1,.2,.2,.2), "lines")) +
  # labs(title = capitalize(gsub('_',' ',i.data.type))) +
  theme(strip.text.y = element_text(size = 8),
        strip.text.x = element_text(size = 10)) +
  #     ylab(expression(paste('Bandwidth',~~(log[10]*h)))) +
  # ylab('Bandwidth (log10 h)\n') +
  # xlab('Year') +
  guides(fill = FALSE)

#######################
# panel D - ice thickness - 2 panels
i.data.type <- 'lake_ice_thickness'

# -- subset data
d.plot<-subset(dat.siz.plot.long,
               data_type==i.data.type)

d.plot$panel_label <- gsub('\\n','\n',d.plot$panel_label, fixed = TRUE)

plot_D <- ggplot(d.plot, aes(year, log10_h, fill=slope)) + 
  geom_raster() + 
  scale_fill_gradientn(colours = c('red2','gray','green3')) +
  facet_wrap( ~ panel_label) +
  geom_vline(xintercept = 2002, color = 'black', lty=5, lwd=1) +
  theme_bw() + 
  xlim(c(1990,2013.1)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        # axis.text.x = element_text(angle = 90, vjust = .5)) +
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 10),
        plot.margin = unit(c(1,.2,.2,.2), "lines")) +
  # labs(title = capitalize(gsub('_',' ',i.data.type))) +
  theme(strip.text.y = element_text(size = 8),
        strip.text.x = element_text(size = 10)) +
  #     ylab(expression(paste('Bandwidth',~~(log[10]*h)))) +
  # ylab('Bandwidth (log10 h)\n') +
  # xlab('Year') +
  guides(fill = FALSE)

#######################
# panel E - lake level - 1 panel
i.data.type <- 'lake_level_change'

# -- subset data
d.plot<-subset(dat.siz.plot.long,
               data_type==i.data.type)

d.plot$panel_label <- gsub('\\n','\n',d.plot$panel_label, fixed = TRUE)

plot_E <- ggplot(d.plot, aes(year, log10_h, fill=slope)) + 
  geom_raster() + 
  scale_fill_gradientn(colours = c('red2','gray','green3')) +
  facet_wrap( ~ panel_label) +
  geom_vline(xintercept = 2002, color = 'black', lty=5, lwd=1) +
  theme_bw() + 
  xlim(c(1990,2013.1)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        # axis.text.x = element_text(angle = 90, vjust = .5)) +
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 10),
        plot.margin = unit(c(1,.2,.2,.2), "lines")) +
  # labs(title = capitalize(gsub('_',' ',i.data.type))) +
  theme(strip.text.y = element_text(size = 8),
        strip.text.x = element_text(size = 10)) +
  #     ylab(expression(paste('Bandwidth',~~(log[10]*h)))) +
  # ylab('Bandwidth (log10 h)\n') +
  # xlab('Year') +
  guides(fill = FALSE)

#######################
# panel F - PAR - 2 panels
i.data.type <- 'underwater_par'

# -- subset data
d.plot<-subset(dat.siz.plot.long,
               data_type==i.data.type)

d.plot$panel_label <- gsub('\\n','\n',d.plot$panel_label, fixed = TRUE)

plot_F <- ggplot(d.plot, aes(year, log10_h, fill=slope)) + 
  geom_raster() + 
  scale_fill_gradientn(colours = c('red2','gray','green3')) +
  facet_wrap( ~ panel_label) +
  geom_vline(xintercept = 2002, color = 'black', lty=5, lwd=1) +
  theme_bw() + 
  xlim(c(1990,2013.1)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        # axis.text.x = element_text(angle = 90, vjust = .5)) +
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 10),
        plot.margin = unit(c(1,.2,.2,.2), "lines")) +
  # labs(title = capitalize(gsub('_',' ',i.data.type))) +
  theme(strip.text.y = element_text(size = 8),
        strip.text.x = element_text(size = 10)) +
  #     ylab(expression(paste('Bandwidth',~~(log[10]*h)))) +
  # ylab('Bandwidth (log10 h)\n') +
  # xlab('Year') +
  guides(fill = FALSE)

#######################
# panel G - PPR - 2 panels
i.data.type <- 'lake_ppr'

# -- subset data
d.plot<-subset(dat.siz.plot.long,
               data_type==i.data.type)

d.plot$panel_label <- gsub('\\n','\n',d.plot$panel_label, fixed = TRUE)

plot_G <- ggplot(d.plot, aes(year, log10_h, fill=slope)) + 
  geom_raster() + 
  scale_fill_gradientn(colours = c('red2','gray','green3')) +
  facet_wrap( ~ panel_label) +
  geom_vline(xintercept = 2002, color = 'black', lty=5, lwd=1) +
  theme_bw() + 
  xlim(c(1990,2013.1)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        # axis.text.x = element_text(angle = 90, vjust = .5)) +
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 10),
        plot.margin = unit(c(1,.2,.2,.2), "lines")) +
  # labs(title = capitalize(gsub('_',' ',i.data.type))) +
  theme(strip.text.y = element_text(size = 8),
        strip.text.x = element_text(size = 10)) +
  #     ylab(expression(paste('Bandwidth',~~(log[10]*h)))) +
  # ylab('Bandwidth (log10 h)\n') +
  # xlab('Year') +
  guides(fill = FALSE)

#######################
# panel H - nematodes - 2 panels
i.data.type <- 'nematodes'

# -- subset data
d.plot<-subset(dat.siz.plot.long,
               data_type==i.data.type)

d.plot$panel_label <- gsub('\\n','\n',d.plot$panel_label, fixed = TRUE)

if(i.data.type == 'nematodes'){
  d.plot$panel_label <- factor(d.plot$panel_label,
                               levels = c('Scottnema sp. (#/kg soil)',
                                          'Eudorylaimus sp. (#/kg soil)'),
                               ordered = TRUE)
}

plot_H <- ggplot(d.plot, aes(year, log10_h, fill=slope)) + 
  geom_raster() + 
  scale_fill_gradientn(colours = c('red2','gray','green3')) +
  facet_wrap( ~ panel_label) +
  geom_vline(xintercept = 2002, color = 'black', lty=5, lwd=1) +
  theme_bw() + 
  xlim(c(1990,2013.1)) +
  theme(
    axis.title.x = element_text(size = 14),
    # axis.text.x = element_blank(),
    # axis.text.x = element_text(angle = 90, vjust = .5),
    # axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 10),
    plot.margin = unit(c(1,.2,.2,.2), "lines")) +
  # labs(title = capitalize(gsub('_',' ',i.data.type))) +
  theme(strip.text.y = element_text(size = 8),
        strip.text.x = element_text(size = 10)) +
  #     ylab(expression(paste('Bandwidth',~~(log[10]*h)))) +
  # ylab('Bandwidth (log10 h)\n') +
  xlab('Year') +
  guides(fill = FALSE)

##############################################################################
# megaplot 
graphics.off()
windows(6, 11, pointsize = 8, rescale = 'fixed')
left_marg <- .1
bottom_marg <- .025
width_panel_1 <- .47
width_panel_2 <- .9
fig_height <- .125
fig_spacing <- .115
bottom_fig_height <- .155
bottom_fig_spacing <- .145
y_label <- expression(paste('Bandwidth',~~(log[10]*h)))

ggdraw() + 
  draw_plot(plot_H, left_marg, bottom_marg, 
            width_panel_2, bottom_fig_height) +
  draw_plot(plot_G, left_marg, bottom_marg + bottom_fig_spacing, 
            width_panel_2, fig_height) +
  draw_plot(plot_F, left_marg, bottom_marg + bottom_fig_spacing + 1*fig_spacing, 
            width_panel_2, fig_height) +
  draw_plot(plot_E, left_marg, bottom_marg + bottom_fig_spacing + 2*fig_spacing, 
            width_panel_1, fig_height) +
  draw_plot(plot_D, left_marg - .0125, bottom_marg + bottom_fig_spacing + 3*fig_spacing, 
            width_panel_2 + .0125, fig_height) +
  draw_plot(plot_C, left_marg, bottom_marg + bottom_fig_spacing + 4*fig_spacing, 
            width_panel_2, fig_height) +
  draw_plot(plot_B, left_marg, bottom_marg + bottom_fig_spacing + 5*fig_spacing, 
            width_panel_1, fig_height) +
  draw_plot(plot_A, left_marg, bottom_marg + bottom_fig_spacing + 6*fig_spacing, 
            width_panel_2, fig_height) +
  draw_plot_label(letters[1:8], 
                  rep(0.05,8),
                  (fig_spacing * c(8:1)) + .05,
                  size = 16) +
  draw_label(label = y_label,
            x = .02,
            y = .515,
            angle = 90,
            vjust = .5,
            size = 14)

savePlot(filename = 'Fig_sizer_heatmap',type='pdf')
savePlot(filename = 'Fig_sizer_heatmap',type='eps')
