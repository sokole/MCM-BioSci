##### action items
# -- only include ELB and WLB in lakes plots
# -- remove stream algal mat chl-a, but keep AFDM
# -- change soil inverts plots so scottnema is on top, eudory on bottom


#####################
# -- load packages
library(ggplot2)
library(scales)
library(grid)
# library(Hmisc)
library(stringr)
# ------------------------------------------------------
# -- read data
# -------------------------------
if(! exists('dat.siz.plot.long')){
  dat.siz.plot.long<-read.csv('dat_2_sizer_data_long.csv')
  
  # -- subset out unecessary flows
  dat.siz.plot.long<-subset(dat.siz.plot.long, 
                            !(var=='disch_log' & !group=='Total_FRX_basin'))
}

data.types<-as.character(unique(dat.siz.plot.long$data_type))

# ------------------------------------------------------
# -- graphics window
# -------------------------------
  # i.data.type <- data.types[i]
  # i.data.type <- data.types[1]
  # i.data.type <- data.types[2]
  # i.data.type <- data.types[3]
  # i.data.type <- data.types[4]
  # i.data.type <- data.types[5]
  # i.data.type <- data.types[6]
  # i.data.type <- data.types[7]
  # i.data.type <- data.types[8]
  # i.data.type <- data.types[9]
  # i.data.type <- data.types[10]

  # -- what data to plot
  # i.data.type <- 'nematodes'

  # ----
  graphics.off()
  panel.width <- 2.25
  panel.height <- 1.75
  # -- plot name
  plot.name <- paste('Fig_SiZer_plot_',i.data.type,'.pdf',sep='')
  
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
  # -- set plot size
  n.panels<-length(unique(as.character(d.plot$panel_label)))

  # windows(
  #   width = .8 + panel.width * n.panels, 
  #   height = panel.height)
  pdf(file=plot.name,
      width = .8 + panel.width * n.panels,
      height = panel.height)
  
  # -- make plot
  ggplot(d.plot, 
         aes(year, log10_h, fill=slope)) + 
    geom_raster() + 
    scale_fill_gradientn(colours = c('red2','gray','green3')) +
    facet_wrap( ~ panel_label) +
    geom_vline(xintercept = 2002, color = 'black', lty=5, lwd=1) +
    theme_bw() + 
    xlim(c(1990,2013.1)) +
    theme(axis.text.x = element_text(angle = 90, vjust = .5)) +
    theme(axis.text.y = element_text(size = 10)) +
    theme(plot.margin = unit(c(1,.2,.2,.2), "lines")) +
    # labs(title = capitalize(gsub('_',' ',i.data.type))) +
    theme(strip.text.y = element_text(size = 8),
          strip.text.x = element_text(size = 10)) +
#     ylab(expression(paste('Bandwidth',~~(log[10]*h)))) +
    ylab('Bandwidth (log10 h)\n') +
    xlab('Year') +
    guides(fill = FALSE)
  
  ggsave(filename = plot.name)
  dev.off()

  cat(i.data.type)
