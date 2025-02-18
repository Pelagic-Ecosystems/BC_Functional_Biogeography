# Modified from p_biplot.pcoa by Patrick Pata
# October 29, 2024
# 
# Requires the rda object and a vector to add colors to the scatter plot 
# (gradient variables or clusters). Can additionally plot the segments of the 
# predictor matrix in xmat. 
#
# Default colors only go to 9 groups and an alternative color vector should be 
# provided when exceeding 9 groups.
#
# This version outputs the xmat variables as points rather than arrows.

'p_biplot_pca_points' <- 
  function(rdaoutput, xmat = NULL, cluster, axestoplot=c(1,2), 
           legendtitle = 'Groups', plottitle = NULL, 
           standardizeXmat = TRUE, alpha = 0.8, ellipse.conf = 0.8,
           clrs = brewer.pal(9, "Set1"), 
           rescaleSites = 1, scaleCorrelations = FALSE) {
    require(RColorBrewer)
    require(ggrepel)

    # axismat <- rdaoutput$CA$u
    axismat <- vegan::scores(rdaoutput, choices = axestoplot, display = "sites")
    varexp <- round( rdaoutput$CA$eig/rdaoutput$tot.chi*100, digits = 1)
    
    # Change axis names to contain % variance explained
    axisname <-  colnames(axismat) 
    a1 <- axisname[1]
    a2 <- axisname[2]
    
    
    # Rescale the site scores to highlight the arrows in the biplot
    axismat.rescaled <- axismat * rescaleSites
    
    
    g <- ggplot(as.data.frame(axismat.rescaled), 
                aes_string(axisname[1],axisname[2], colour = cluster)) + 
      labs(x = paste0(a1," (",varexp[axestoplot[1]],"%)"), 
           y = paste0(a2, " (",varexp[axestoplot[2]], "%)" )) +
      geom_point(size = 3, alpha = alpha, shape = 16, aes(colour=cluster))  + 
      stat_ellipse(size = 1.5,show.legend = FALSE,level = ellipse.conf) +
      geom_vline(xintercept = 0, colour='darkgray') + geom_hline(yintercept = 0, colour='darkgray') +
      theme_bw() +
      theme(legend.position="none", 
            # legend.background = element_rect(colour="gray", size=0.5, linetype="solid"),
            legend.title = element_text(color = 'black', size = 12, face = 'bold'),
            legend.text = element_text(size = 12),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 12),
            aspect.ratio = 1) 
      # ggtitle(plottitle)
    
    if (is.factor(cluster) == TRUE)  { 
      g <- g + scale_color_manual(values = clrs, name = legendtitle)#+scale_color_brewer(palette="Set1")
    } else { 
      g <- g + scale_colour_continuous(name = legendtitle)
    }
    
    # Project standardized explanatory variables to PCoA axes
    if (is.null(xmat) == FALSE) {
      if (standardizeXmat == TRUE) {
        xmat = scale(xmat , center=TRUE, scale=TRUE)
      } 
      n <- nrow(xmat)
      points.stand <- scale(axismat)
      
      # Predictors appear as correlations
      U <- cor(xmat, points.stand, use="pairwise.complete.obs")  # pairwise to disregard NAs for variables with blanks
      
      if (scaleCorrelations == TRUE) {
        maxpos <-  min(max(abs(axismat[,1])),max(abs(axismat[,2])))# previously = 1
      } else {
        maxpos <- c(1,1)
      }
      
      Udf <- data.frame(x1 = rep(0,nrow(U)),y1 =rep(0,nrow(U)), xend = U[,1]*maxpos, yend = U[,2]*maxpos)
      g <- g + 
        geom_point(data = Udf, aes(x = xend, y = yend),
                   color = "black", size = 2) +
        # geom_segment(data = Udf, aes(x = x1, y = y1, xend = xend, yend = yend),
        #              lineend = "round", size = 0.5, colour = "grey40") +
        ggrepel::geom_text_repel(data = Udf, aes(x = xend, y = yend, label=rownames(U)), 
                        color="black", size = 4) 
    }
    
    plot(g)
  }
