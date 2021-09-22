createAdjacencyList = function(dataForAdjacencyList){
  fromColumn = rep(unique(dataForAdjacencyList[,1]), each=length(unique(dataForAdjacencyList[,2])))
  toColumn = rep(unique(dataForAdjacencyList[,2]), times=length(unique(dataForAdjacencyList[,1])))
  adjacencyList = data.frame(from = fromColumn, 
                             to = toColumn,
                             value = matrix(0, length(fromColumn), 1))
  
  for (j in 1:dim(adjacencyList)[1]){
    for (i in 1:length(dataForAdjacencyList[,1])){
      if(dataForAdjacencyList[i,1] == adjacencyList[j,1]){
        if(dataForAdjacencyList[i,2] == adjacencyList[j,2]){
          adjacencyList[j,3] = adjacencyList[j,3] + 1
        }
      }
    }
  }
  return(adjacencyList)
}

convertNanToZero = function(dataToConvert){
  # if data contains NaN, convert to 0
  if (class(dataToConvert) == "numeric"){
    for (i in 1:length(dataToConvert)){
      if (is.na(dataToConvert[i])){
        dataToConvert[i] = 0
      }
    }
  } else {
    for (i in 1:dim(dataToConvert)[1]){
      for (j in 1:dim(dataToConvert)[2]){
        if (is.na(dataToConvert[i,j])){
          dataToConvert[i,j] = 0
        }  
      }
    }
  }
  return(dataToConvert)
}

convertEmptyToNan = function(dataToConvert){
  library(sjmisc)
  # if data element is empty, convert to NaN
  for (i in 1:dim(dataToConvert)[1]){
    for (j in 1:dim(dataToConvert)[2]){
      if (is_empty(as.character(dataToConvert[i,j]))){
        dataToConvert[i,j] = NaN
      }  
    }
  }
  return(dataToConvert)
}

convertNumberToNan = function(dataToConvert, numbersToConvert){
  for (i in 1:length(dataToConvert)){
    for (j in 1:length(numbersToConvert)){
      if (grepl(numbersToConvert[j], dataToConvert[i])){
        dataToConvert[i] = NaN
      }
    }  
  }
  return(dataToConvert)
}

convertCharacterToNumber = function(dataToConvert, characterToConvert, numberToConvertTo){
  # if data element has specified character, convert to specified number
  for (i in 1:length(dataToConvert)){
    if (grepl(characterToConvert, dataToConvert[i])){
      dataToConvert[i] = numberToConvertTo
    }  
  }
  return(dataToConvert)
}

removeTstageLetters = function(tstageData){
  tstageData = as.character(tstageData)
  numericTstageData = tstageData
  for (i in 1:4){
    for (j in 1:length(tstageData)){
      if (grepl(i, tstageData[j])){
        numericTstageData[j] = i  
      }
    }
  }
  return(numericTstageData)
}

createScatterplot = function(dataset, xvariable, yvariable, labelsForColor, title){
  ggplot(as.data.frame(dataset), aes(x=xvariable, y=yvariable, color = labelsForColor)) +
    geom_point(shape=16, size = 3) +     # Use hollow circles
    ggtitle(title) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          plot.title = element_text(hjust = 0.5))
}

createLoadingsPlot = function(loadings, loadingsLabels, title){
  ggplot(loadings, aes(x=loadings[,1], y=loadings[,2], label = loadingsLabels)) +
    geom_point(shape=16, size = 3) +  geom_text(hjust = 0, nudge_x = 0.005) +
    ggtitle(title) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          plot.title = element_text(hjust = 0.5))
}

createHistogram = function(dataset, title){
  h = ggplot(data=as.data.frame(dataset), aes(dataset)) + geom_histogram(binwidth = 5) + 
    ggtitle(title) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5))
  return(h)
}

createScatterBoxPlot = function(data, xaxis, yaxis, title = ""){
  library(ggplot2)
  plotObject = ggplot(as.data.frame(data), aes(x=factor(xaxis), y=yaxis)) + 
    geom_jitter(colour="#808080", alpha=0.5) +
    geom_boxplot(outlier.shape=NA, alpha = 0) + #avoid plotting outliers twice
    ggtitle(title) + 
    #visuals to make it look nicer:
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          plot.title = element_text(hjust = 0.5))
  return(plotObject)
}

createBarPlotForPercentage = function(numericList, title){
  p = ggplot(data=as.data.frame(numericList), aes(x = 1:length(numericList), y=numericList)) +
    geom_bar(stat="identity") + ylim(0,100) + ggtitle(title) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5))
  return(p)
}

createBarPlot = function(numericList, xLabels, title = ""){
  p = ggplot(data=as.data.frame(numericList), aes(x = xLabels, y=numericList)) +
    geom_bar(stat="identity") + ggtitle(title) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1))
  return(p)
}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  # Multiple plot function
  #
  # ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
  # - cols:   Number of columns in layout
  # - layout: A matrix specifying the layout. If present, 'cols' is ignored.
  #
  # If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
  # then plot 1 will go in the upper left, 2 will go in the upper right, and
  # 3 will go all the way across the bottom.
  # copied from: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_%28ggplot2%29/
  #
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



createSankeyDiagram = function(adjacencyList, nodeNames){
  maxNodeValue = max(adjacencyList[,1])
  adjacencyList[,2] = adjacencyList[,2] + maxNodeValue
  if (min(adjacencyList[,1:2]) > 0){
    adjacencyList[,1:2] = adjacencyList[,1:2] - 1 # start at 0 as required by javascript  
  }
  # #create sankey diagram
  library(networkD3)
  nodes = data.frame("name" = nodeNames)
  sankeyNetwork(Links = adjacencyList, Nodes = nodes,
                Source = "from", Target = "to",
                Value = "value", NodeID = "name",
                fontSize= 12, nodeWidth = 30)
}

createSankeyDiagram3Timepoints = function(adjacencyList1to2, adjacencyList2to3, nodeNames){
  maxNodeValue = max(adjacencyList1to2[,1])
  adjacencyList1to2[,2] = adjacencyList1to2[,2] + maxNodeValue + 1
  adjacencyList2to3[,1] = adjacencyList2to3[,1] + maxNodeValue + 1
  
  maxNodeValue2to3 = max(adjacencyList1to2[,2]) + 1
  adjacencyList2to3[,2] = adjacencyList2to3[,2] + maxNodeValue2to3
  
  adjacencyList = rbind(adjacencyList1to2, adjacencyList2to3)
  
  #create sankey diagram
  nodes = data.frame("name" = nodeNames)
  links = rbind(c(0,0,0), adjacencyList)
  sankeyNetwork(Links = links, Nodes = nodes,
                Source = "from", Target = "to",
                Value = "value", NodeID = "name",
                fontSize= 12, nodeWidth = 30)
}

createPieChart = function(data, title = ""){
  library(plyr)
  library(ggplot2)
  
  countData = count(data)
  countData$percentages = round(countData$freq/sum(countData$freq)*100, digits=1)
  label_pos = cumsum(countData$percentages) - countData$percentages / 2
  
  
  p = ggplot(as.data.frame(countData$percentages), aes(x="", y=countData$percentages, fill=factor(countData$x))) +
    geom_bar(stat="identity", width=1, color="white") +
    coord_polar("y", start=0) +
    ggtitle(title) + 
    theme_void() +
    scale_fill_brewer(palette="Greys") + 
    geom_text(aes(x = 1.82, y = 100 - label_pos, label = paste0(round(countData$percentages), "%")), size = 4)
  return(p)
}
  