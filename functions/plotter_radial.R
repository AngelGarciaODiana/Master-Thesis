plotter_radial <- function(gathered, metrics, sampleSize, transformation, data, plot = c("mean","median","Q05","Q25","Q75","Q95"), ylim = c(0,1),
                           arrangement = "data type - sample size - transformation",
                           palette = "paired"){
  # handle arrangement:
  arrangement <- strsplit(arrangement, spl = " - ")[[1]]
  arrangement <- sapply(arrangement,switch,
                        "data type" = "data",
                        "sample size" = "sampleSizeFactor",
                        "transformation" = "transformation")
  
  formula <- as.formula(paste0(arrangement[3], "~", arrangement[1]," + ",arrangement[2]))
  facet <- facet_nested(formula)
  
  plot <- match.arg(plot)
  
  # Only relevant data type and sample size:
  gathered <- gathered[gathered$data %in% data & gathered$sampleSize %in% sampleSize & gathered$transformation %in% transformation,]
  
  # Only relevant metrics:
  gathered <- gathered %>% filter(metric %in% metrics)
  
  summarized <- gathered %>% dplyr::group_by(sampleSize,method,metric,sampleSizeFactor,transformation,data) %>%
    dplyr::summarize(
      mean = mean(value, na.rm=TRUE),
      median = median(value, na.rm=TRUE),
      Q05 = quantile(value, 0.05, na.rm = TRUE),
      Q25 = quantile(value, 0.25, na.rm = TRUE),
      Q75 = quantile(value, 0.75, na.rm = TRUE),
      Q95 = quantile(value, 0.95, na.rm = TRUE)
    )
  
  # Make a dummy x variable:
  summarized$xDummy <- as.numeric(factor(summarized$metric,levels=metrics))
  
  # Arrange:
  summarized <- summarized %>% arrange(xDummy)
  
  # Make one more row for first:
  # summarized <- bind_rows(summarized,summarized %>% filter(xDummy == 1))
  
  # Metric data set:
  metricsDF <- summarized %>% group_by(xDummy) %>% 
    summarize(x = unique(xDummy), label = unique(metric), 
              angle = 90 - unique(rad2deg(2*pi*(xDummy/max(summarized$xDummy))))) %>%
    mutate(
      hjust = ifelse(angle < -90 & angle > -270, 1, 0)
    ) %>% mutate(
      angle = ifelse(angle < -90 & angle > -270,angle + 180,angle)
    )
  
  # Append the entire data frame with a second first metric (a bit silly but works):
  # summarized <- bind_rows(summarized,summarized%>%filter(xDummy==1))
  #https://stackoverflow.com/questions/42562128/ggplot2-connecting-points-in-polar-coordinates-with-a-straight-line-2
  coord_radar <- function (theta = "x", start = 0, direction = 1) {
    theta <- match.arg(theta, c("x", "y"))
    r <- if (theta == "x") "y" else "x"
    ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start,
            direction = sign(direction),
            is_linear = function(coord) TRUE)
  }
  
  # Silly border line:
  borderLineDF <- data.frame(x=seq(0,max(summarized$xDummy), length = 1000), y = 1)
  borderLineDF_25 <- data.frame(x=seq(0,max(summarized$xDummy), length = 1000), y = 0.25)
  borderLineDF_50 <- data.frame(x=seq(0,max(summarized$xDummy), length = 1000), y = 0.5)
  borderLineDF_75 <- data.frame(x=seq(0,max(summarized$xDummy), length = 1000), y = 0.75)
  
  # Vertical Lines:
  vertLines <- bind_rows(lapply(sort(unique(summarized$xDummy)), function(x){
    data.frame(x=x,y=c(0,1))
  }))
  
  
  g <- ggplot(na.omit(summarized), aes_string(x = "xDummy", y = plot, colour = "method", label = "metric"))  + 
    geom_line(data=borderLineDF,aes(x=x,y=y,colour = "black", label = ""), colour = "black") + 
    geom_line(data=borderLineDF_50,aes(x=x,y=y,colour = "black", label = ""), colour = "black", alpha = 0.5) + 
    geom_line(data=borderLineDF_25,aes(x=x,y=y,colour = "black", label = ""), colour = "black", alpha = 0.25) + 
    geom_line(data=borderLineDF_75,aes(x=x,y=y,colour = "black", label = ""), colour = "black", alpha = 0.25)  +
    geom_point(data=data.frame(x=0,y=0),aes(x=x,y=y,colour = "black", label = ""), colour = "black", cex = 2 ) + 
    facet +
    geom_line(data=vertLines,aes(x=x,y=y,group=x, label = NULL),color = "black", alpha = 0.25) +
    #  geom_line(alpha = 0.75, lwd = 1.25) +
    geom_polygon(fill = NA, lwd = 1.25,show.legend=FALSE, alpha = 0.75) +
    geom_point(cex = 3) +
    # facet_nested(missingFactor ~ data + sampleSizeFactor) +
    theme_bw(14) +
    xlab("")+
    ylab("") +
    scale_x_continuous(limits = c(0,max(summarized$xDummy)), labels = c(metrics,metrics[1]),
                       breaks = 0:max(summarized$xDummy)) +
    scale_y_continuous(limits=c(0,1.25), breaks = c(0,0.25,0.5,0.75,1), minor_breaks=NULL) +
    scale_fill_discrete("") + scale_colour_discrete("") +
    theme(legend.position = "top") +
    coord_radar()  + 
    geom_text(data=metricsDF,aes(x=x, label = label, angle=angle,
                                 y=1.05, hjust = hjust), colour = "black", cex = 4) +
    theme(axis.text.x = element_blank(), panel.border = element_blank(),
          panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank(),
          legend.title = element_text( size = 10),
          legend.text = element_text( size = 9)) + 
    # scale_colour_brewer("",palette = "Paired") +
    guides(colour=guide_legend(nrow=4)) 
  
  
  
  
  if (palette == "paired"){
    g <- g + scale_colour_brewer("",palette = "Paired")
  } else if (palette == "pastel"){
    g <- g + scale_colour_brewer("",palette = "Set3")
  }
  
  return(g)
}
