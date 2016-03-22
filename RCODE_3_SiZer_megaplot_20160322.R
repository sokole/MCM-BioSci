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
  i.data.type <- data.types[8]
  # # i.data.type <- data.types[9] # -- no longer included

  # -- what data to plot
  # i.data.type <- 'nematodes'
  
  # ----
  graphics.off()
  panel.width <- 2
  panel.height <- 1
  # -- plot name
  plot.name <- paste('Fig_SiZer_plot_',i.data.type,'.pdf',sep='')
  
  # -- subset data
  d.plot<-subset(dat.siz.plot.long,
                 data_type==i.data.type)
  
  # -- format group labels
  d.plot$var.label<-gsub('_log','',d.plot$var)
  d.plot$var.label<-gsub('_',' ',d.plot$var.label)
  d.plot$var.label<-str_to_title(d.plot$var.label)
  d.plot$var.label<-gsub('Uw par','Underwater PAR', ignore.case = TRUE, d.plot$var.label)
  d.plot$var.label<-gsub('Disch','Total Basin Q',d.plot$var.label)
  d.plot$var.label<-gsub('Afdm','AFDM',d.plot$var.label)
  d.plot$var.label<-gsub('Ppr','PPR',d.plot$var.label)
  d.plot$var.label<-gsub('Chla','Chlorophyll a',d.plot$var.label)
  
  d.plot$group.label<-d.plot$group
  d.plot$group.label<-gsub('Total_FRX_basin','FRX',d.plot$group)
  d.plot$group.label<-toupper(d.plot$group.label)
  
  if (i.data.type=='nematodes'){
    d.plot$group.label <- factor(d.plot$group.label, 
                                 ordered = TRUE,
                                 levels = c('SCOTTNEMA','EUDORYLAIMUS'))
  }
  
  # -- set plot size
  n.vars<-length(unique(as.character(d.plot$var)))
  n.groups<-length(unique(as.character(d.plot$group)))
  
  # windows(width = 1.5 + panel.width * n.vars, 
  #         height = 1.25 + panel.height * n.groups)
  pdf(file=plot.name,
      width = 1.5 + panel.width * n.vars, 
      height = 1.25 + panel.height * n.groups)
  
  # -- make plot
  ggplot(d.plot, 
         aes(year, log10_h, fill=slope)) + 
    geom_raster() + 
    scale_fill_gradientn(colours = c('red2','gray','green3')) +
    facet_grid(group.label ~ var.label) +
    geom_vline(xintercept = 2002, color = 'black', lty=5, lwd=1) +
    theme_bw() + 
    xlim(c(1990,2013.1)) +
    theme(axis.text.x = element_text(angle = 90, vjust = .5)) +
    theme(plot.margin = unit(c(1,.2,.2,.2), "lines")) +
    # labs(title = capitalize(gsub('_',' ',i.data.type))) +
    theme(strip.text.y = element_text(size = 8),
          strip.text.x = element_text(size = 10)) +
    ylab(expression(paste('Bandwidth',~~(log[10]*h)))) +
    xlab('Year') +
    guides(fill = FALSE)
  
  ggsave(filename = plot.name)
  dev.off()

