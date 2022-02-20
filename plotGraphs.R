#### Load package
require(ggplot2)
require(colorspace)
require(grid)
require(gridExtra)


#### Source Functions ####
## Theme
theme_303 <- function(size_base = 9, size_title = 13, 
                      position_legend = "right", border = c(0.25,0.25,0.25,0.25)) {
  theme(
    text =              element_text(family = "Verdana", size = size_base, colour = "black"),
    
    axis.title =        element_text(size = size_base + 2),
    
    axis.text.x =       element_text(angle = -45, size = size_base, vjust = 1, hjust = 0, colour = "gray40"),
    axis.text.y =       element_text(size = size_base, colour = "gray40"),
    
    axis.line.x =       element_line(colour = "black"),
    axis.line.y =       element_line(colour = "black"),
    axis.ticks =        element_line(colour = "black"),
    
    legend.key =        element_blank(),
    legend.position =   position_legend,
    
    panel.background =  element_blank(),
    panel.border =      element_blank(),
    panel.grid.major.x= element_blank(),
    panel.grid.major.y= element_line(colour = "grey"),
    panel.grid.minor =  element_blank(),
    
    plot.title =        element_text(face = "bold", size = size_title, colour = "black", hjust = 0.5),
    plot.margin =       unit(border, "cm")
    # margin unit is (top, right, bottom, left)
  )
}


## Function: timeplot ####
## Inputs: Data frame 
# Column 1: days
# Column 2: factor
# Column 3: y-var
# Filename to save plot
# Opt: title
## Outputs: Time plot to file
timeplot <- function(d, filename, titl=""){
  names(d) <- c("day", "grp", "y")
  t <- ggplot(d, aes(x= day, y= y)) # Create base layer, store in object 'p'
  t <- t + geom_line(aes(colour= grp, linetype= grp))
  t <- t + scale_x_date(limits = c(startDate, startDate + numDays - 1), date_breaks = "1 weeks", labels = date_format("%d-%b"))
  t <- t + geom_smooth(aes(colour= grp, linetype= grp), method="lm", size = 1, se = F)
  t <- t + scale_colour_manual(values=c("Steelblue", "red", "darkgreen")) + scale_linetype_manual(values=c(2, 3, 4)) 
  t <- t + labs(x="", y="", title=titl) 
  windowsFonts(Verdana=windowsFont("TT Verdana")) # Load verdana font (May not work on non-Windows machines)
  t <- t + theme(text = element_text(family="Verdana", size=12), axis.text.x = element_text(angle= -45, size=9, vjust=1, hjust=0))
  t <- t + theme(axis.line = element_line(colour="black"), axis.ticks = element_line(colour="black"))
  t <- t + theme(panel.background = element_blank(), panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(colour="grey"),
                 plot.title = element_text(size = 10, colour = "black"))
  
  ggsave(paste(filename, ".png", sep = ""), width=6, height=2.7, units="in") # Write image to file (change sizes if you want)
}


## Function: paretoplot ####
## Inputs: Data frame 
# Column 1: factor
# Column 2: counts
# Filename to save plot
# Opt: number of bars to show, title
## Outputs: Bar graph to file
paretoplot <- function(d, filename, numBars = 6, titl=""){
  names(d) = c("grp", "count")
  d <- d[order(d$count, decreasing=F), ]
  d$grp <- factor(d$grp, levels=d$grp)
  d$percent <- round(d$count/sum(d$count), 2)
  if(nrow(d)>numBars) d <- d[(nrow(d)-numBars+1):nrow(d),] # Takes only top 'numBars' groups
  y.upper <- ceiling(max(d$percent)*10 + 1)/10
  
  ## Create Pareto plot
  p <- ggplot(d, aes(x=grp))
  p <- p + geom_bar(aes(y=percent), fill="steelblue", stat="identity", width=0.5) # Add bars
  p <- p + coord_flip() # Flip axes and add percentages
  p <- p + labs(x="", y="", title=titl)
  windowsFonts(Verdana=windowsFont("TT Verdana")) # Load verdana font (May not work on non-Windows machines)
  p <- p + theme(text = element_text(family="Verdana", size=12, colour="black"), axis.text.x = element_text(size=9, hjust=0.5, colour="black"), axis.text.y = element_text(size=9, colour="black"))
  p <- p + theme(axis.line = element_line(colour="black"), axis.ticks = element_line(colour="black"))
  p <- p + theme(panel.background = element_blank(), panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour="grey92"))
  p <- p + geom_text(aes(y = percent+0.015, label = paste(sprintf("%.0f", percent*100), "%", sep="")), size = 3, hjust=0)
  p <- p + expand_limits(y = 0) + scale_y_continuous(expand = c(0,0), labels=percent_format(), limits=c(0, y.upper), breaks = seq(0, y.upper, y.upper/2))  
  p <- p + theme(plot.title = element_text(hjust = 0.5, size=10), plot.margin = unit(c(.5,.4,-.3,-.3), "cm")) 
  # unit is (top, right, bottom, left) margins
  
  # Save to file
  ggsave(paste(filename, ".png", sep = ""), width=4, height=2.7, units="in") # Write image to file (change sizes if you want)
}


## Function: Panels on pairs() function ####
# Example: pairs(myData[numerical variables], lower.panel = panel.smooth, upper.panel = panel.cor, diag.panel = panel.hist)

panel.cor <- function(x, y, digits=2, cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  test <- cor.test(x,y)
  Signif <- ifelse(round(test$p.value, 3) < 0.001, "p < 0.001", paste("p =", round(test$p.value,3)))  
  text(0.5, 0.25, paste("r =", txt))
  text(0.5, 0.75, Signif)
}

panel.smooth<-function (x, y, col = "blue", bg = NA, pch = 18, 
                        cex = 0.8, col.smooth = "red", span = 2/3, iter = 3, ...) 
{
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok)) 
    lines(stats::lowess(x[ok], y[ok], f = span, iter = iter), 
          col = col.smooth, ...)
}

panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
}


## Function: Simple Frequency Histogram (1 Numerical) ####
## Inputs: One Column from a Data frame 
# Column 1: numerical values for x-axis
# Filename to save plot
# Opt: title
## Outputs: Histogram to file 

Hist <- function(data, showDensity = TRUE,
                         Binwidth = 0.5, seq_break_start = min(data[,1]), seq_break_end = max(data[,1]), seq_break_by, 
                         PlotTitle = '', tiff = FALSE, width_in = 10, height_in = 7, filename) {
  x_name <- names(data)[1]
  names(data) <- c("x_value")
  
  myPlot <- ggplot(data = data, aes(x = x_value)) 
  
  fill_colour <- with(data, heat_hcl(n = 1, h = c(0,-100), c = c(40,80), l = c(75,40), power = 1))
  line_colour <- with(data, diverge_hcl(n = 1, h = c(246,40), c = 96))
  
  ifelse(isTRUE(showDensity),
         
         myPlot <- myPlot +
           geom_histogram(aes(y = ..density..), binwidth = Binwidth, colour = line_colour, fill = "white") +
           geom_density(alpha = 0.5, fill = fill_colour, size = 0.77) +
           labs(title = PlotTitle, x = x_name, y = "Density"),
         
         myPlot <- myPlot +
           geom_histogram(binwidth = Binwidth, alpha = 0.5, colour = line_colour, fill = fill_colour) +
           labs(title = PlotTitle, x = x_name, y = "Frequency")
  )
  
  myPlot <- myPlot +
    scale_x_continuous(breaks = round(c(seq(from = seq_break_start, to = seq_break_end, by = seq_break_by), min(data[,1]), max(data[,1])), digits = 2))

  windowsFonts(Verdana=windowsFont("TT Verdana"))
  
  myPlot <- myPlot + theme_303()
  
  # Save to file
  if (isTRUE(tiff)) {
    ggsave(paste(filename, ".tiff", sep = ""), width = width_in, height = height_in, units = "in")
  } else {
    ggsave(paste(filename, ".png", sep = ""), width = width_in, height = height_in, units = "in")
  }
  # Call it
  myPlot
}


## Function: Overlaid Frequency Histogram (1 Numerical) ####
## Inputs: A subset (2 Columns) from a Data frame 
# Column 1: numerical values for x-axis
# Column 2: factors for different layers
# Filename to save plot
# Opt: title
## Outputs: Histogram to file 

OverlaidHist <- function(data, showDensity = TRUE,
                         Binwidth, seq_break_start = min(data[,1]), seq_break_end = max(data[,1]), seq_break_by, 
                         PlotTitle = '', tiff = FALSE, width_in = 10, height_in = 7, filename) {
  x_name <- names(data)[1]
  grp_name <- names(data)[2]
  names(data) <- c("x_value", "grp")
  
  myPlot <- ggplot(data = data, aes(x = x_value)) 
  
  ifelse(isTRUE(showDensity),
         
         myPlot <- myPlot +
           geom_histogram(aes(y = ..density.., fill = grp, colour = grp),
                          binwidth = Binwidth, alpha = 0.5, position="identity") +
           geom_density(aes(colour = grp), size = 0.77) +
           labs(title = PlotTitle, x = x_name, y = "Density"),
         
         myPlot <- myPlot +
           geom_histogram(aes(fill = grp, colour = grp),
                          binwidth = Binwidth, alpha = 0.5, position="identity") +
           labs(title = PlotTitle, x = x_name, y = "Frequency")
  )
  
  fill_colour <- with(data, heat_hcl(n = length(unique(grp)), h = c(0,-100), c = c(40,80), l = c(75,40), power = 1))
  line_colour <- with(data, diverge_hcl(n = length(unique(grp)), h = c(246,40), c = 96))
  
  myPlot <- myPlot +
    scale_x_continuous(breaks = round(c(seq(from = seq_break_start, to = seq_break_end, by = seq_break_by), min(data[,1]), max(data[,1])), digits = 2)) +
    scale_fill_manual(values = fill_colour, name = grp_name) +
    scale_colour_manual(values = line_colour, name = grp_name)
  
  windowsFonts(Verdana=windowsFont("TT Verdana"))
  
  myPlot <- myPlot + theme_303()
  
  # Save to file
  if (isTRUE(tiff)) {
    ggsave(paste(filename, ".tiff", sep = ""), width = width_in, height = height_in, units = "in")
  } else {
    ggsave(paste(filename, ".png", sep = ""), width = width_in, height = height_in, units = "in")
  }
         
  # Call it
  myPlot
}


## Function: Boxplot (Num x Cat) ####
## Inputs: A subset (2 Columns) from a Data frame 
# Column 1: factor for x-axis
# Column 2: values for y-axis
# Filename to save plot
# Opt: title
## Outputs: Boxplot to file

Boxplot <- function(data, PlotTitle = '', tiff = FALSE, filename) {
  x_name <- names(data)[1]
  y_name <- names(data)[2]
  names(data) <- c("x_value", "y_value")
  
  myPlot <- ggplot(data = data, aes(x = x_value, y = y_value)) 
  
  myPlot <- myPlot +
    geom_boxplot() +
    stat_summary(fun.y = mean, geom = "point", shape = 18, size = 2) +
    labs(title = PlotTitle,
         x = x_name,
         y = y_name)
  
  windowsFonts(Verdana=windowsFont("TT Verdana"))
  
  myPlot <- myPlot + theme_303()
  
  # Save to file
  if (isTRUE(tiff)) {
    ggsave(paste(filename, ".tiff", sep = ""), width = 10, height = 7, units = "in")
  } else {
    ggsave(paste(filename, ".png", sep = ""), width = 10, height = 7, units = "in")
  }
  # Call it
  myPlot
}


## Function: Scatterplot (Num x Num) ####
## Inputs: A subset (2 Columns) from a Data frame 
# Column 1: values for x-axis
# Column 2: values for y-axis
# Filename to save plot
# Opt: title
## Outputs: Scatterplot to file

Scatterplot <- function(data, FitMethod = c(NULL, "loess", "gam", "glm", "lm", "rlm"), PlotTitle = '',
                        tiff = FALSE, width_in = 10, height_in = 7, filename) {
  x_name <- names(data)[1]
  y_name <- names(data)[2]
  names(data) <- c("x_value", "y_value")
  
  myPlot <- ggplot(data = data, aes(x = x_value, y = y_value)) 
  
  myPlot <- myPlot +
    geom_point() +
    labs(title = PlotTitle,
         x = x_name,
         y = y_name)
  
  ifelse(is.null(FitMethod),
         myPlot <- myPlot,
         myPlot <- myPlot + geom_smooth(method = FitMethod, se = FALSE, color = "darkgreen"))
  
  windowsFonts(Verdana=windowsFont("TT Verdana"))
  
  myPlot <- myPlot + theme_303()
  
  # Save to file
  if (isTRUE(tiff)) {
    ggsave(paste(filename, ".tiff", sep = ""), width = width_in, height = height_in, units = "in")
  } else {
    ggsave(paste(filename, ".png", sep = ""), width = width_in, height = height_in, units = "in")
  }
  # Call it
  myPlot
}

## Function: Hexagonal heatmap (Num x Num) LARGE #observations ####
## Inputs: A subset (2 Columns) from a Data frame 
# Column 1: values for x-axis
# Column 2: values for y-axis
# Filename to save plot
# Opt: title
## Outputs: Hexagonal heatmap to file

HexHeatmap <- function(data, contrastColours = c("white", "darkblue"), PlotTitle = '',
                       tiff = FALSE, width_in = 10, height_in = 7, filename) {
  x_name <- names(data)[1]
  y_name <- names(data)[2]
  names(data) <- c("x_value", "y_value")
  
  myPlot <- ggplot(data = data, aes(x = x_value, y = y_value)) 
  
  myPlot <- myPlot +
    stat_binhex(colour = "white", na.rm = TRUE) +
    scale_fill_gradientn(colours = contrastColours, name = "Frequency", na.value = NA)
  
  myPlot <- myPlot +
    labs(title = PlotTitle,
         x = x_name,
         y = y_name)
  
  windowsFonts(Verdana=windowsFont("TT Verdana"))
  
  myPlot <- myPlot + theme_303()
  
  # Save to file
  if (isTRUE(tiff)) {
    ggsave(paste(filename, ".tiff", sep = ""), width = width_in, height = height_in, units = "in")
  } else {
    ggsave(paste(filename, ".png", sep = ""), width = width_in, height = height_in, units = "in")
  }
  # Call it
  myPlot
}


## Function: Trivariate plot: Boxplot for different Groups (Num x Cat x Cat) ####
## Inputs: A subset (3 Columns) from a Data frame 
# Column 1: factor for x-axis
# Column 2: values for y-axis
# Column 3: factor for colour
# Filename to save plot
# Opt: title
## Outputs: Boxplot to file

Boxplot2 <- function(data, Colour = TRUE, PlotTitle = '', tiff = FALSE, filename) {
  x_name <- names(data)[1]
  y_name <- names(data)[2]
  group_name <- names(data)[3]
  
  names(data) <- c("x_value", "y_value", "group_value")
  
  ifelse(isTRUE(Colour),
         myPlot <- ggplot(data = data, aes(x = x_value, y = y_value, colour = group_value)),
         myPlot <- ggplot(data = data, aes(x = x_value, y = y_value, linetype = group_value, shape = group_value)))
         
  
  myPlot <- myPlot +
    geom_boxplot() +
    stat_summary(fun.y = mean, geom = "point", shape = 18, size = 2) +
    labs(title = PlotTitle,
         x = x_name,
         y = y_name)
  
  ifelse(isTRUE(Colour),
         myPlot <- myPlot + scale_colour_discrete(name = group_name),
         myPlot <- myPlot + scale_linetype_discrete(name = group_name) + scale_shape_discrete(name = group_name))
         
  
  windowsFonts(Verdana=windowsFont("TT Verdana"))
  
  myPlot <- myPlot + theme_303()
  
  # Save to file
  if (isTRUE(tiff)) {
    ggsave(paste(filename, ".tiff", sep = ""), width = 10, height = 7, units = "in")
  } else {
    ggsave(paste(filename, ".png", sep = ""), width = 10, height = 7, units = "in")
  }
  # Call it
  myPlot
}


## Function: Interaction plot (Num x Cat x Cat) ----
## Inputs: A subset (3 Columns) from a Data frame 
# Column 1: factor for x-axis
# Column 2: factor for colour
# Column 3: values for y-axis
# Filename to save plot
# Opt: title
## Outputs: Interaction (Cat x Cat) to file

Interaction_Cat_Cat <- function(data, Colour = TRUE, PlotTitle = "", tiff = FALSE, width_in = 7, height_in = 7, filename) {
  x_name <- names(data)[1]
  group_name <- names(data)[2]
  y_name <- names(data)[3]
  
  names(data) <- c("x_value", "group_value", "y_value")
  
  ifelse(isTRUE(Colour),
         myPlot <- ggplot(data = data, aes(x = x_value, y = y_value, colour = group_value, group = group_value)),
         myPlot <- ggplot(data = data, aes(x = x_value, y = y_value, linetype = group_value, group = group_value)))
  
  
  myPlot <- myPlot +
    stat_summary(fun.y = mean, geom = "point", shape = 18, size = 2) +
    stat_summary(fun.y = mean, geom = "line") +
    labs(title = PlotTitle,
         x = x_name,
         y = y_name)
  
  ifelse(isTRUE(Colour),
         myPlot <- myPlot + scale_colour_discrete(name = group_name),
         myPlot <- myPlot + scale_linetype_discrete(name = group_name) + scale_shape_discrete(name = group_name))
  
  
  windowsFonts(Verdana=windowsFont("TT Verdana"))
  
  myPlot <- myPlot + theme_303()
  
  # Save to file
  if (isTRUE(tiff)) {
    ggsave(paste(filename, ".tiff", sep = ""), width = 10, height = 7, units = "in")
  } else {
    ggsave(paste(filename, ".png", sep = ""), width = width_in, height = height_in, units = "in")
  }
  # Call it
  myPlot
}

## Function: Trivariate plot (Num x Num x Cat) ####
## Inputs: A subset (3 Columns) from a Data frame 
# Column 1: Num values for x-axis
# Column 2: Num values for y-axis
# Column 3: Factor for colour
# Filename to save plot
# Opt: title
## Outputs: Plot to file

Scatterplot2 <- function(data, Colour = TRUE, PlotTitle = '', tiff = FALSE, filename) {
  x_name <-       names(data)[1]
  y_name <-       names(data)[2]
  colour_name <-  names(data)[3]
  
  names(data) <- c("x_value", "y_value", "colour_value")
  
  ifelse(isTRUE(Colour),
         myPlot <- ggplot(data = data, aes(x = x_value, y = y_value, colour = colour_value)),
         myPlot <- ggplot(data = data, aes(x = x_value, y = y_value, linetype = colour_value, shape = colour_value)))
  
  myPlot <- myPlot +
    geom_point() +
    geom_smooth(method= "lm", se=F) +
    
    labs(title = PlotTitle,
         x = x_name,
         y = y_name)
  
  ifelse(isTRUE(Colour),
         myPlot <- myPlot + scale_colour_discrete(name = colour_name),
         myPlot <- myPlot + scale_linetype_discrete(name = colour_name) + scale_shape_discrete(name = colour_name))
  
  windowsFonts(Verdana=windowsFont("TT Verdana"))
  
  myPlot <- myPlot + theme_303()
  
  # Save to file
  if (isTRUE(tiff)) {
    ggsave(paste(filename, ".tiff", sep = ""), width = 10, height = 7, units = "in")
  } else {
    ggsave(paste(filename, ".png", sep = ""), width = 10, height = 7, units = "in")
  }
  # Call it
  myPlot
}


## Function: Trivariate plot (Num x Num x Cat) facet style ####
## Inputs: A subset (3 Columns) from a Data frame 
# Column 1: Num values for x-axis
# Column 2: Num values for y-axis
# Column 3: Factor for group
# Filename to save plot
# Opt: title
## Outputs: Plot to file

Scatterplot2_facet <- function(data, nrows, ncols, PlotTitle = '', tiff = FALSE, filename) {
  x_name <-       names(data)[1]
  y_name <-       names(data)[2]
  group_name <-   names(data)[3]
  
  names(data) <- c("x_value", "y_value", "group_value")
  
  myPlot <- ggplot(data = data, aes(x = x_value, y = y_value))
         
  myPlot <- myPlot +
    geom_point() +
    geom_smooth(method= "lm", se=F) +
    facet_wrap(~ group_value, nrow = nrows, ncol = ncols)+
    
    labs(title = PlotTitle,
         x = x_name,
         y = y_name)
                                
  windowsFonts(Verdana=windowsFont("TT Verdana"))
  
  myPlot <- myPlot + theme_303()
  
  # Save to file
  if (isTRUE(tiff)) {
    ggsave(paste(filename, ".tiff", sep = ""), width = 10, height = 7, units = "in")
  } else {
    ggsave(paste(filename, ".png", sep = ""), width = 10, height = 7, units = "in")
  }
  # Call it
  myPlot
}


## Function: Trivariate plot (Cat x Cat x Cat) ####
## Inputs: A subset (3 Columns) from a Data frame 
# Column 1: Factor for x-axis
# Column 2: Factor for group
# Column 3: Factor for colour
# Filename to save plot
# Opt: title
## Outputs: Plot to file

Cat3 <- function(data, PlotTitle = '', tiff = FALSE, filename) {
  x_name <-       names(data)[1]
  group_name <-   names(data)[2]
  colour_name <-  names(data)[3]
  
  names(data) <- c("x_value", "group_value", "colour_value")
  
  myPlot <- ggplot(data = data, aes(x = x_value, fill = group_value))
  
  myPlot <- myPlot +
    geom_bar(position = 'dodge') +
    facet_wrap(~ colour_value) +
    
    labs(title = PlotTitle,
         x = x_name)
  
  myPlot <- myPlot + scale_fill_discrete(name = group_name)

  windowsFonts(Verdana=windowsFont("TT Verdana"))
  
  myPlot <- myPlot + theme_303()
  
  # Save to file
  if (isTRUE(tiff)) {
    ggsave(paste(filename, ".tiff", sep = ""), width = 10, height = 7, units = "in")
  } else {
    ggsave(paste(filename, ".png", sep = ""), width = 10, height = 7, units = "in")
  }
  # Call it
  myPlot
}


## Function: Tetravariate plot (Num x Num x Cat x Cat) facet style ####
## Inputs: A subset (4 Columns) from a Data frame 
# Column 1: Num values for x-axis
# Column 2: Num values for y-axis
# Column 3: factor for colour
# Column 4: factor for group
# Filename to save plot
# Opt: title
## Outputs: Plot to file

Num2_Cat2 <- function(data, PlotTitle = '', tiff = FALSE, filename) {
  x_name <-       names(data)[1]
  y_name <-       names(data)[2]
  colour_name <-  names(data)[3]
  group_name <-   names(data)[4]
  
  names(data) <- c("x_value", "y_value", "colour_value", "group_value")
  
  myPlot <- ggplot(data = data, aes(x = x_value, y = y_value, colour = colour_value))
  
  myPlot <- myPlot +
    geom_point() +
    geom_smooth(method= "lm", se=F) +
    facet_wrap(~ group_value) +
    
    labs(title = PlotTitle,
         x = x_name,
         y = y_name)
  
  myPlot <- myPlot + 
    scale_colour_discrete(name = colour_name)
  
  windowsFonts(Verdana=windowsFont("TT Verdana"))
  
  myPlot <- myPlot + theme_303()
  
  # Save to file
  if (isTRUE(tiff)) {
    ggsave(paste(filename, ".tiff", sep = ""), width = 10, height = 7, units = "in")
  } else {
    ggsave(paste(filename, ".png", sep = ""), width = 10, height = 7, units = "in")
  }
  # Call it
  myPlot
}


## Function: Tetravariate plot (Num x Cat x Cat x Cat) ####
## Inputs: A subset (4 Columns) from a Data frame 
# Column 1: Factor for x-axis
# Column 2: Num values for y-axis
# Column 3: Factor for colour
# Column 4: Factor for group
# Filename to save plot
# Opt: title
## Outputs: Plot to file

Num1_Cat3 <- function(data, PlotTitle = "", tiff = FALSE, width_in = 10, height_in = 7, filename) {
  x_name <-       names(data)[1]
  y_name <-       names(data)[2]
  colour_name <-  names(data)[3]
  group_name <-   names(data)[4]
  
  names(data) <- c("x_value", "y_value", "colour_value", "group_value")
  
  myPlot <- ggplot(data = data, aes(x = x_value, y = y_value, colour = colour_value))
  
  myPlot <- myPlot +
    geom_boxplot() +
    facet_wrap(~ group_value, nrow = 1) +
    
    labs(title = PlotTitle,
         x = x_name,
         y = y_name)
  
  myPlot <- myPlot + 
    scale_colour_discrete(name = colour_name)
  
  windowsFonts(Verdana=windowsFont("TT Verdana"))
  
  myPlot <- myPlot + theme_303()
  
  # Save to file
  if (isTRUE(tiff)) {
    ggsave(paste(filename, ".tiff", sep = ""), width = width_in, height = height_in, units = "in")
  } else {
    ggsave(paste(filename, ".png", sep = ""), width = width_in, height = height_in, units = "in")
  }
  # Call it
  myPlot
}


## Function: Volcano plot ####
## Inputs: A subset (3 Columns) from a Data frame 
# Column 1: Log2(Fold Change) for x-axis
# Column 2: q-value for y-axis
# Column 3: Factor: Gene names
# Filename to save plot
# Opt: title
## Outputs: Plot to file

Volcano <- function(data, LFC_threshold = 1, Pvalue_threshold = 0.05, y_offset = 5, PlotTitle = "", tiff = FALSE, filename) {
  x_name <- names(data)[1]
  y_name <- names(data)[2]
  gene <-   names(data)[3]

  names(data) <- c("x_value", "y_value", "gene_value")
  
  setDT(data, key = "gene_value")
  min_positive_LFC <- min(data[(x_value > 0) & (y_value < 0.05), x_value])
  min_negative_LFC <- max(data[(x_value < 0) & (y_value < 0.05), x_value])
  
  data <- within(data, {
    UD_DEG <- factor(ifelse(x_value > 0 & y_value < 0.05, "Up-regulated DE genes",
                            ifelse(x_value < 0 & y_value < 0.05, "Down-regulated DE genes", "Non-DE genes")))
    y_value <- -log10(y_value)
  })
  
  

  myPlot <- ggplot(data = data, aes(x = x_value, y = y_value, colour = UD_DEG, label = gene_value))
  
  myPlot <- myPlot + 
    geom_point() +
    geom_text(aes(label = ifelse((x_value > +LFC_threshold) & (y_value > -log10(Pvalue_threshold)), 
                                 as.character(gene_value), '')), 
              size = 2, angle = -45, vjust = -0.4, hjust = 0.5) +
    geom_text(aes(label = ifelse((x_value < -LFC_threshold) & (y_value > -log10(Pvalue_threshold)), 
                                 as.character(gene_value), '')), 
              size = 2, angle = +45, vjust = -0.4, hjust = 0.5) +
    labs(title = PlotTitle, 
         x = expression(paste(log[2], "(Fold Change)", sep = "")),
         y = expression(paste("Negative ", log[10], "(q-value)")))
  
  myPlot <- myPlot +
    scale_colour_manual(values = c("blue", "black", "red"), name = "Legend")
  
  myPlot <- myPlot +
    geom_hline(aes(yintercept = -log10(Pvalue_threshold)), colour = "darkgreen", linetype = 2) +
    geom_vline(aes(xintercept = -LFC_threshold), colour = "purple", linetype = 2) +
    geom_vline(aes(xintercept = +LFC_threshold), colour = "purple", linetype = 2) +
    geom_vline(aes(xintercept = min_negative_LFC), colour = "darkcyan", linetype = 4) +
    geom_vline(aes(xintercept = min_positive_LFC), colour = "darkcyan", linetype = 4)
  
  myPlot <- myPlot +
    geom_text(aes(x = max(x_value-0.02), y = -log10(Pvalue_threshold), label = "FDR = 0.05"), size = 3, vjust = 1.2, hjust = 1) +
    geom_text(aes(x = -LFC_threshold, y = max(y_value-y_offset), label = paste(round(2^LFC_threshold, 2), "-fold change", sep = "")),
              size = 3, angle = 90, vjust = -0.4, hjust = 0) +
    geom_text(aes(x = +LFC_threshold, y = max(y_value-y_offset), label = paste(round(2^LFC_threshold, 2), "-fold change", sep = "")),
              size = 3, angle = 90, vjust = -0.4, hjust = 0) +
    geom_text(aes(x = min_negative_LFC, y = max(y_value-y_offset), label = paste(round(2^abs(min_negative_LFC), 2), "-fold change", sep = "")),
              size = 3, angle = 90, vjust = -0.4, hjust = 0) +
    geom_text(aes(x = min_positive_LFC, y = max(y_value-y_offset), label = paste(round(2^min_positive_LFC, 2), "-fold change", sep = "")),
              size = 3, angle = 90, vjust = -0.4, hjust = 0)
  
  windowsFonts(Verdana=windowsFont("TT Verdana"))
  
  myPlot <- myPlot + theme_303()
  
  # Save to file
  if (isTRUE(tiff)) {
    ggsave(paste(filename, ".tiff", sep = ""), width = 10, height = 7, units = "in")
  } else {
    ggsave(paste(filename, ".png", sep = ""), width = 10, height = 7, units = "in")
  }
  # Call it
  myPlot
}


## Function: Venn Diagram (up to 5 groups) ####
## Inputs: A LIST of up to 5 sets  
# Opt: Name for each set

Venn <- function(myList, setNames = LETTERS[1:length(myList)], namePosition = 0, mainTitle = "", subTitle = "",
                 imageType = "png", filename = NULL, width_in = 10, height_in = 10, res = 300){
  require(VennDiagram)
  
  # Suppress the log .txt file generated 
  futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger")
  
  windowsFonts(Verdana=windowsFont("TT Verdana"))
  venn.diagram(myList,
               
               lty = "solid",
               col = diverge_hcl(n = length(myList), h = c(246,40), c = 96),
               fill = heat_hcl(n = length(myList), h = c(20,-60), c = c(40,80), l = c(75,40), power = 1),
               alpha = 0.3,
               cex = 2,
               
               cat.pos = namePosition,
               cat.cex = 1.5,
               cat.col = heat_hcl(n = length(myList), h = c(20,-60), c = c(40,80), l = c(75,40), power = 1),
               cat.fontfamily = "Verdana", cat.fontface = "bold",
               
               # Upto 3 sets
               euler.d = TRUE,
               scaled = TRUE,
               
               # Upto 2 sets
               ext.text = TRUE,
               
               main = mainTitle, main.cex = 3, main.just = c(0.5, -0.4), main.fontfamily = "Verdana", main.fontface = "bold",
               sub = subTitle, sub.cex = 2, sub.just = c(0.5, -0.4), sub.fontfamily = "Verdana", sub.fontface = "bold",
               
               category.names = setNames,
               
               #if (isTRUE(showPercent) == TRUE) {print.mode = c("raw", "percent")} else {print.mode = "raw"}, 
               #if (isTRUE(showPercent) == TRUE) {sigdigs = 2},
               
               imagetype = imageType,
               
               if (is.null(filename) == TRUE) {filename = NULL} else {filename = paste(filename, ".", imageType, sep = "")},
               width = width_in, height = height_in, units = "in", resolution = res
               )
}

## Function: Multidimensional Scaling (MDS) plot ####
## Inputs: A data frame (or matrix) of counts (cols = Samples, rows = Genes)
# Filename to save plot
# Opt: title
## Outputs: Plot to file

plotMDS <- function(data, distMetric = c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"),
                    sepCols = c("Line", "Treatment"), sep = "_",
                    PlotTitle = "", tiff = FALSE, width_in = 10, height_in = 7, filename) {
  
  require(data.table)
  require(tidyr)
  
  distMatrix <- dist(t(data), method = distMetric) 
  fit <- cmdscale(distMatrix, eig=TRUE, k=2) # k is the number of dim
  
  MDS <- as.data.table(fit[["points"]], keep.rownames = TRUE)
  MDS <- separate(data = MDS, col = "rn", into = sepCols, sep = sep, extra = "merge")
  
  colour_name <- names(MDS)[grep(sepCols[1], names(MDS))]
  shape_name  <- names(MDS)[grep(sepCols[2], names(MDS))]
  
  setnames(MDS,
           old = c(grep("V1", names(MDS)), grep("V2", names(MDS)), grep(sepCols[1], names(MDS)), grep(sepCols[2], names(MDS))),
           new = c("x_value", "y_value", "colour_value", "shape_value"))

  myPlot <- ggplot(data = MDS, aes(x = x_value, y = y_value, colour = colour_value, shape = shape_value))
  
  myPlot <- myPlot + 
    geom_point() +

    labs(title = PlotTitle, 
         x = "Dimension 1",
         y = "Dimension 2")
  
  fill_colour <- with(MDS, heat_hcl(n = length(unique(colour_value)), h = c(0,-100), c = c(40,80), l = c(75,40), power = 1))
  
  myPlot <- myPlot +
    scale_colour_manual(values = fill_colour, name = colour_name) +
    scale_shape_discrete(name = shape_name)
  
  windowsFonts(Verdana=windowsFont("TT Verdana"))
  
  myPlot <- myPlot + theme_303()
  
  # Save to file
  if (isTRUE(tiff)) {
    ggsave(paste(filename, ".tiff", sep = ""), width = width_in, height = height_in, units = "in")
  } else {
    ggsave(paste(filename, ".png", sep = ""), width = width_in, height = height_in, units = "in")
  }
  # Call it
  myPlot
}


## Function: PCA ####
## Inputs: 
# A transformed count data from dds object in library(DESeq2)
# Groups: "condition", "cell type"
# Filename to save plot
# Opt: title
## Outputs: Plot to file

plotPCA_custom <- function(data, groups = c("condition", "type"), PlotTitle = "", tiff = FALSE, filename) {
  pcaData <- plotPCA(data, intgroup = groups, returnData = TRUE)
  percentVar <- round(100 * attr(pcaData, "percentVar"), digits = 2)
  pcaData <- pcaData[,c(1,2,4,5)]
  
  colour_name <- names(pcaData)[3]
  shape_name <-  names(pcaData)[4]
  
  names(pcaData) <- c("x_value", "y_value", "colour_value", "shape_value")
  
  myPlot <- ggplot(data = pcaData, aes(x = x_value, y = y_value, colour = colour_value, shape = shape_value))
  
  myPlot <- myPlot +
    geom_point(size = 3) +
    labs(title = PlotTitle, 
         x = paste0("PC1: ",percentVar[1],"% variance"), 
         y = paste0("PC2: ",percentVar[2],"% variance")) +
    coord_fixed()
  
  myPlot <- myPlot +
    scale_colour_discrete(name = colour_name) +
    scale_shape_discrete(name = shape_name)
  
  windowsFonts(Verdana=windowsFont("TT Verdana"))
  
  myPlot <- myPlot + theme_303()
  
  # Save to file
  if (isTRUE(tiff)) {
    ggsave(paste(filename, ".tiff", sep = ""), width = 10, height = 7, units = "in")
  } else {
    ggsave(paste(filename, ".png", sep = ""), width = 10, height = 7, units = "in")
  }
  # Call it
  myPlot
}


## Function: Likelihood Ratio Test ####
## Inputs: A 2-D table with 2 factors A and B
# Rows: levels for A
# Cols: levels for B
## Outputs: G2, df, p-value
LR.test <- function(dd) {
  G2        <- 2*sum(dd*log(dd/chisq.test(dd)$expected))
  G2.pvalue <- 1 - pchisq(G2, df = (dim(dd)[1] - 1)*(dim(dd)[2] - 1))
  list(G2 = G2, df = (dim(dd)[1] - 1)*(dim(dd)[2] - 1), p_value = G2.pvalue)
}


## Function: Summarise a list of many data frames ####
my_summary <- function(x){
  m <- mean(x)
  s <- sd(x)
  c(mean = m, 
    UpperCI = m + 1.96*s, 
    LowerCI = m - 1.96*s)
}

#as.data.frame(lapply(data.list, sapply, my_summary))


## Function: Imputation by Means
impute_mean <- function( DT, myCols = c(1:ncol(DT)), number_of_skipped_columns = 1 ) {
  means <- c(rep(0, number_of_skipped_columns), colMeans(DT[, .SD, .SDcols = myCols], na.rm = T))
  for (j in myCols)
    set(DT, i = which(is.na(DT[[j]])), j = j, value = means[j])
}
## Function: Multiple plots layout ----
# Example of usage:

# a <- qplot(1:10, rnorm(10), main = "a")
# b <- qplot(1:10, rnorm(10), main = "b")
# c <- qplot(1:10, rnorm(10), main = "c")
# grid.newpage()
# pushViewport(viewport(layout = grid.layout(2, 2)))
# vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
# print(plot.inter.MA, vp = vplayout(1, 1:2))  # key is to define vplayout
# print(plot.inter.WM, vp = vplayout(2, 1))
# print(plot.inter.WA, vp = vplayout(2, 2))

vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

## Function: leave one out mean ----
# Calculate unbiased mean by "big" group, excluding "small" group
leaveOneOutMean <- function(dt, ind, bybig, bysmall) {
  dtmp <- copy(dt) # copy so as not to alter original dt object w intermediate assignments
  dtmp <- dtmp[is.na(get(ind))==F,]
  dtmp[,`:=`(avg_ind_big=mean(get(ind)), Nbig=.N), by=.(get(bybig))]
  dtmp[,`:=`(Nbigsmall=.N, avg_ind_big_small=mean(get(ind))), by=.(get(bybig), get(bysmall))]
  dtmp[,unbmean:=(avg_ind_big*Nbig-(Nbigsmall*avg_ind_big_small))/(Nbig-Nbigsmall)]
  return(dtmp[,unbmean])
}

# Example
# dt <- data.table(mtcars)[,.(response, group_1, group_2)]
# dt[,unbiased_mean:=leaveOneOutMean(.SD, ind='response', bybig='group_1', bysmall='group_2')]
# dt[,biased_mean:=mean(response), by=group_1]
# head(dt)

## Function: Calculate optimal lambda in GLM-net ----
getOptimalLambda_glmnet <- function (myData = myData, response = response, optimalLambda_cycle = optimalLambda_cycle,
                                     lambda = glmnet.lambda, alpha = 0, nfolds = k_fold, family = glmnet.family) {
  require(glmnet)
  optimalLambda <- 0
  for(h in 1:optimalLambda_cycle) {
    tmp <- cv.glmnet(x = as.matrix(myData[ , .SD, .SDcol = grep(x = names(myData), pattern = paste("[^(", response, ")]", sep = ""), perl = T)]),
                     y = as.vector(myData[[response]]),
                     lambda = lambda,
                     nfolds = nfolds, alpha = alpha, family = family)
    optimalLambda <- optimalLambda + tmp$lambda.min # tmp$lambda.1se
  }
  optimalLambda <- optimalLambda / optimalLambda_cycle
}
## Function: ROC (part of QC for binary classification) ####
## Inputs: 
# tag: binary class vector
# score: predicted probability for a given class
## Output: ROC
mplot_roc <- function(tag, score, model_name = NA, subtitle = NA, interval = 0.2, plotly = FALSE,
                      save = FALSE, file_name = "viz_roc.png") {
  require(pROC)
  require(ggplot2)
  
  if (length(tag) != length(score)) {
    message("The tag and score vectors should be the same length.")
    stop(message(paste("Currently, tag has",length(tag),"rows and score has",length(score))))
  }
  
  roc <- pROC::roc(tag, score, ci=T)
  coords <- data.frame(
    x = rev(roc$specificities),
    y = rev(roc$sensitivities))
  ci <- data.frame(roc$ci, row.names = c("min","AUC","max"))
  
  p <- ggplot(coords, aes(x = x, y = y)) +
    geom_line(colour = "deepskyblue", size = 1) +
    geom_point(colour = "blue3", size = 0.9, alpha = 0.4) +
    geom_segment(aes(x = 0, y = 1, xend = 1, yend = 0), alpha = 0.2, linetype = "dotted") + 
    scale_x_reverse(name = "% Specificity [False Positive Rate]", limits = c(1,0), 
                    breaks = seq(0, 1, interval), expand = c(0.001,0.001)) + 
    scale_y_continuous(name = "% Sensitivity [True Positive Rate]", limits = c(0,1), 
                       breaks = seq(0, 1, interval), expand = c(0.001, 0.001)) +
    theme_minimal() + 
    theme(axis.ticks = element_line(color = "grey80")) +
    coord_equal() + 
    ggtitle("ROC Curve: AUC") +
    annotate("text", x = 0.25, y = 0.10, vjust = 0, size = 4.2, 
             label = paste("AUC =", round(100*ci[c("AUC"),],2))) +
    annotate("text", x = 0.25, y = 0.05, vjust = 0, size = 2.8, 
             label = paste0("95% CI: ", 
                            round(100*ci[c("min"),],2),"-", 
                            round(100*ci[c("max"),],2)))
  if(!is.na(subtitle)) {
    p <- p + labs(subtitle = subtitle)
  }  
  
  if(!is.na(model_name)) {
    p <- p + labs(caption = model_name)
  }
  
  if (plotly == TRUE) {
    require(plotly)
    p <- ggplotly(p)
  }
  
  if (save == TRUE) {
    p <- p + ggsave(file_name, width = 6, height = 6)
  }
  return(p)
}


## Function: Density plot (part of QC for binary classification) ####
## Inputs: 
# tag: binary class vector
# score: predicted probability for a given class
## Output: Density plot
mplot_density <- function(tag, score, model_name = NA, subtitle = NA, 
                          save = FALSE, file_name = "viz_distribution.png") {
  require(ggplot2)
  require(gridExtra)
  
  if (length(tag) != length(score)) {
    message("The tag and score vectors should be the same length.")
    stop(message(paste("Currently, tag has",length(tag),"rows and score has",length(score))))
  }
  
  if (length(unique(tag)) != 2) {
    stop("This function is for binary models. You should only have 2 unique values for the tag value!")
  }
  
  out <- data.frame(tag = as.character(tag),
                    score = as.numeric(score),
                    norm_score = lares::normalize(as.numeric(score)))
  
  p1 <- ggplot(out) + theme_minimal() +
    geom_density(aes(x = 100 * score, group = tag, fill = as.character(tag)), 
                 alpha = 0.6, adjust = 0.25) + 
    guides(fill = guide_legend(title="Tag")) + 
    xlim(0, 100) + 
    labs(title = "Score distribution for binary model",
         y = "Density by tag", x = "Score")
  
  p2 <- ggplot(out) + theme_minimal() + 
    geom_density(aes(x = 100 * score), 
                 alpha = 0.9, adjust = 0.25, fill = "deepskyblue") + 
    labs(x = "", y = "Density")
  
  p3 <- ggplot(out) + theme_minimal() + 
    geom_line(aes(x = score * 100, y = 100 * (1 - ..y..), color = as.character(tag)), 
              stat = 'ecdf', size = 1) +
    geom_line(aes(x = score * 100, y = 100 * (1 - ..y..)), 
              stat = 'ecdf', size = 0.5, colour = "black", linetype="dotted") +
    ylab('Cumulative') + xlab('') + guides(color=FALSE)
  
  if(!is.na(subtitle)) {
    p1 <- p1 + labs(subtitle = subtitle)
  }
  
  if(!is.na(model_name)) {
    p1 <- p1 + labs(caption = model_name)
  }
  
  if(save == TRUE) {
    png(file_name, height = 1800, width = 2100, res = 300)
    grid.arrange(
      p1, p2, p3, 
      ncol = 2, nrow = 2, heights = 2:1,
      layout_matrix = rbind(c(1,1), c(2,3)))
    dev.off()
  }
  
  return(
    grid.arrange(
      p1, p2, p3, 
      ncol = 2, nrow = 2, heights = 2:1,
      layout_matrix = rbind(c(1,1), c(2,3))))
  
}


## Function: Cuts by quantile plot (part of QC for binary classification) ####
mplot_cuts <- function(score, splits = 10, subtitle = NA, model_name = NA, 
                       save = FALSE, file_name = "viz_ncuts.png") {
  
  require(ggplot2)
  
  if (splits > 25) {
    stop("You should try with less splits!")
  }
  
  deciles <- quantile(score, 
                      probs = seq((1/splits), 1, length = splits), 
                      names = TRUE)
  deciles <- data.frame(cbind(Deciles=row.names(as.data.frame(deciles)),
                              Threshold=as.data.frame(deciles)))
  
  p <- ggplot(deciles, 
              aes(x = reorder(Deciles, deciles), y = deciles * 100, 
                  label = round(100 * deciles, 1))) + 
    geom_col(fill="deepskyblue") + 
    xlab('') + theme_minimal() + ylab('Score') + 
    geom_text(vjust = 1.5, size = 3, inherit.aes = TRUE, colour = "white", check_overlap = TRUE) +
    labs(title = paste("Cuts by score: using", splits, "equal-sized buckets"))
  
  if(!is.na(subtitle)) {
    p <- p + labs(subtitle = subtitle)
  } 
  if(!is.na(model_name)) {
    p <- p + labs(caption = model_name)
  }
  if (save == TRUE) {
    p <- p + ggsave(file_name, width = 6, height = 6)
  }
  return(p)
}

## Function: Split and Compare Quantile (part of QC for binary classification) ####
mplot_splits <- function(tag, score, splits = 5, subtitle = NA, model_name = NA, facet = NA, 
                         save = FALSE, subdir = NA, file_name = "viz_splits.png") {
  
  require(ggplot2)
  require(dplyr)
  require(RColorBrewer)
  
  if (length(tag) != length(score)) {
    message("The tag and score vectors should be the same length.")
    stop(message(paste("Currently, tag has",length(tag),"rows and score has",length(score))))
  }
  
  if (splits > 10) {
    stop("You should try with less splits!")
  }
  
  df <- data.frame(tag, score, facet)
  npersplit <- round(nrow(df)/splits)
  names <- df %>% 
    mutate(quantile = ntile(score, splits)) %>% group_by(quantile) %>%
    summarise(n = n(), 
              max_score = round(100 * max(score), 1), 
              min_score = round(100 * min(score), 1)) %>%
    mutate(quantile_tag = paste0(quantile," (",min_score,"-",max_score,")"))
  
  p <- df %>% 
    mutate(quantile = ntile(score, splits)) %>% 
    group_by(quantile, facet, tag) %>% tally() %>%
    ungroup() %>% group_by(facet, tag) %>% 
    arrange(desc(quantile)) %>%
    mutate(p = round(100*n/sum(n),2),
           cum = cumsum(100*n/sum(n))) %>%
    left_join(names, by = c("quantile")) %>%
    ggplot(aes(x = as.character(tag), y = p, label = as.character(p),
               fill = as.character(quantile_tag))) + theme_minimal() +
    geom_col(position = "stack") +
    geom_text(size = 3, position = position_stack(vjust = 0.5), check_overlap = TRUE) +
    xlab("Tag") + ylab("Total Percentage by Tag") +
    guides(fill = guide_legend(title=paste0("~",npersplit," p/split"))) +
    labs(title = "Tag vs Score Splits Comparison") +
    scale_fill_brewer(palette = "Spectral")
  
  if(!is.na(subtitle)) {
    p <- p + labs(subtitle = subtitle)
  }  
  
  if(!is.na(model_name)) {
    p <- p + labs(caption = model_name)
  }
  
  if(!is.na(facet)) {
    p <- p + facet_grid(. ~ facet, scales = "free")
  }  
  
  if (!is.na(subdir)) {
    dir.create(file.path(getwd(), subdir))
    file_name <- paste(subdir, file_name, sep="/")
  }
  
  if (save == TRUE) {
    p <- p + ggsave(file_name, width = 6, height = 6)
  }
  
  return(p)
  
}



## Function: Full QC for binary classification ####
mplot_full <- function(tag, score, splits = 8, subtitle = NA, model_name = NA, 
                       save = FALSE, file_name = "viz_full.png", subdir = NA) {
  require(ggplot2)
  require(gridExtra)
  options(warn=-1)
  
  if (length(tag) != length(score)) {
    message("The tag and score vectors should be the same length.")
    stop(message(paste("Currently, tag has",length(tag),"rows and score has",length(score))))
  }
  
  p1 <- mplot_density(tag = tag, score = score, subtitle = subtitle, model_name = model_name)
  p2 <- mplot_splits(tag = tag, score = score, splits = splits)
  p3 <- mplot_roc(tag = tag, score = score)
  p4 <- mplot_cuts(score = score)
  
  if(save == TRUE) {
    if (!is.na(subdir)) {
      dir.create(file.path(getwd(), subdir))
      file_name <- paste(subdir, file_name, sep="/")
    }
    png(file_name, height = 2000, width = 3200, res = 300)
    grid.arrange(
      p1, p2, p3, p4,
      widths = c(1.3,1),
      layout_matrix = rbind(c(1,2), c(1,2), c(1,3), c(4,3)))
    dev.off()
  }
  return(
    grid.arrange(
      p1, p2, p3, p4,
      widths = c(1.3,1),
      layout_matrix = rbind(c(1,2), c(1,2), c(1,3), c(4,3)))
  ) 
}


## Function: Importance Variables plot ####
mplot_importance <- function(var, imp, colours = NA, limit = 15, model_name = NA, subtitle = NA,
                             save = FALSE, file_name = "viz_importance.png", subdir = NA) {
  
  require(ggplot2)
  require(gridExtra)
  options(warn=-1)
  
  if (length(var) != length(imp)) {
    message("The variables and importance values vectors should be the same length.")
    stop(message(paste("Currently, there are",length(var),"variables and",length(imp),"importance values!")))
  }
  if (is.na(colours)) {
    colours <- "deepskyblue" 
  }
  out <- data.frame(var = var, imp = imp, Type = colours)
  if (length(var) < limit) {
    limit <- length(var)
  }
  
  output <- out[1:limit,]
  
  p <- ggplot(output, 
              aes(x = reorder(var, imp), y = imp * 100, 
                  label = round(100 * imp, 1))) + 
    geom_col(aes(fill = Type), width = 0.1) +
    geom_point(aes(colour = Type), size = 6) + 
    coord_flip() + xlab('') + theme_minimal() +
    ylab('Importance') + 
    geom_text(hjust = 0.5, size = 2, inherit.aes = TRUE, colour = "white") +
    labs(title = paste0("Variables Importances. (", limit, " / ", length(var), " plotted)"))
  
  if (length(unique(output$Type)) == 1) {
    p <- p + geom_col(fill = colours, width = 0.2) +
      geom_point(colour = colours, size = 6) + 
      guides(fill = FALSE, colour = FALSE) + 
      geom_text(hjust = 0.5, size = 2, inherit.aes = TRUE, colour = "white")
  }
  if(!is.na(model_name)) {
    p <- p + labs(caption = model_name)
  }
  if(!is.na(subtitle)) {
    p <- p + labs(subtitle = subtitle)
  }  
  if(save == TRUE) {
    if (!is.na(subdir)) {
      dir.create(file.path(getwd(), subdir))
      file_name <- paste(subdir, file_name, sep="/")
    }
    p <- p + ggsave(file_name, width=7, height=6)
  }
  
  return(p)
  
}