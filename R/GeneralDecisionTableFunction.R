# This script contains generalized decision table functions




########################################## may need to add back in

#' Produce generalized decision tables.
#' 
#' This function produces a decision table with between 1 and 20 rows 
#' and columns based on the dimensions of the provided data matrix.
#' Decision table labels, colors, performance ranking, and decision 
#' table dimensions can be specified. Up to three icons may be printed 
#' next to the title, and a summary row can also be automatically calculated. 
#' 
#' @param Data A matrix with row and column names (must me matrix even if only 1 row or column), no default
#' @param BestPerformanceVector A vector equal in length to the number of rows in Data containing "High" or "Low", no default
#'      "High" means highest value in row will be colored to represent best performance
#'      "Low" means lowest value in row will be colored to represent best performance
#' @param SummaryBestPerformance A string containing "High" or "Low" to indicate best performance for summary row, no default
#'      Only required if "MeanValue", "SumValue", or "MedianValue" options chosen. For other options higher summarized rank correspond with better performance
#' @param OutputDirectory A string containing the full path name of folder where resulting graphic should be stored, no default
#' @param OutputFileName A string containing the file name for the resulting graphic, no default
#' @param GraphicTitle A string containing the title for the decision table graphic, no default
#' @param RowHeader A string containing the descriptive title for row names, no default
#' @param ColumnHeader A string containing the descriptive title for column names, no default
#' @param IconList A vector of icon names to be printed right of the GraphicTitle, length may not exceed 3, default = no icons
#' @param SummaryRowOption A string indicating summary row option, default = no summary row
#'      "Off"          No summary row
#'      "MeanRank"     Summary row added, values represent mean rank across all rows in each column, highest rank correspond with best performance
#'      "SumRank"      Summary row added, values represent summed rank across all rows in each column, highest rank correspond with best performance
#'      "MedianRank"   Summary row added, values represent median rank across all rows in each column, highest rank correspond with best performance 
#'      "MeanValue"    Summary row added, values represent mean value of Data across all rows in each column, high/low value corresponding with best performance must be specified
#'      "SumValue"     Summary row added, values represent sum of Data values across all rows in each column, high/low value corresponding with best performance must be specified
#'      "MedianValue"  Summary row added, values represent median value of Data across all rows in each column, high/low value corresponding with best performance must be specified
#' @param ImageWidth A number specifying total graphic image width in number of pixels, default = 500
#' @param ImageHeight A number specifying total graphic image height in number of pixels, default = 800
#' @param Resolution A number between 1 and 3, default = 1.
#'      This parameter increases the image resolution by scaling the number of pixels in the length and width by an equal amount. 
#'      Increasing the resolution (higher numbers) will result in a larger image which can be adjusted in any image processing software to the desired dimension.
#' @param GraphicCellHeight A number specifying height of graphic rows containing data
#' @param GraphicCellWidths A vector of length 2 specifying first the width of row label column second data column width
#' @param BarWidth A number specifying the width of the plotted bar
#' @param BarColors A string specifying single color or a vector of colors equal in length to the number of columns in Data. Colors used to visually show rank, defaults specified by dimensions of Data.
#' @param BarFigureMargins A vector of length 4 specifying margins of barplots in each graphic cell, default = c(0,0,0,0).
#'      If figure margin error generated when producing decision table, try setting this parameter to the default option, reproducing the graphic, and then specifying parameter as desired
#' 
#' @return Customized decision table image (.png)







############### Define General Decision Table Function ###############

MakeDecisionTable <- function(Data = NULL,
                              BestPerformanceVector = NULL,
                              SummaryBestPerformance = NULL,
                              OutputDirectory = NULL, 
                              OutputFileName = NULL,
                              GraphicTitle = NULL,
                              RowHeader = NULL,
                              ColumnHeader = NULL,
                              IconList = NULL,
                              SummaryRowOption = "Off",
                              ImageWidth = 500,
                              ImageHeight = 800,
                              Resolution = 1,
                              GraphicCellHeight = NULL, 
                              GraphicCellWidths = NULL, 
                              BarWidth = 1,
                              BarColors = NULL,
                              BarFigureMargins = c(0,0,0,0)){
  
  # This function plots a decision table with no summary and up to 3 icons
     # May have up to 20 rows & 20 columns
     # May have up to 3 icons printed to right of the GraphicTitle
     # Row & column names set based on row & column names from Data
     # Default: 1 color (no performance ranking across rows), no row summary
     # Requires arguments:   Data 
                           # BestPerformanceVector
                           # OutputDirectory
                           # OutputFileName
                           # GraphicTitle
                           # RowHeader
                           # ColumnHeader
  
  # Data, BestPerformanceVector, OutputDirectory, OutputFileName, GraphicTitle, RowHeader, and ColumnHeader are all required arguments
  

  
  # Args:
#NONONO#       # DecisionTablePath: file path for General_Decision_Table folder containing scripts to produce decision tables
       # Data: A matrix of data with row and colum names (must be matrix even if only 1 row/column), default = NULL
       # BestPerformanceVector: A vector equal in length to the number of rows in Data containing "High" or "Low"
            # Use to rank performance when rows represent different performance metrics 
                 # (where high values represent best performance & low values are best performance for other metrics)
            # "High" = Highest rank colored to be the best
            # "Low" = Lowest rank colored to be the best
       # OutputDirectory: String containing the full path name of folder to store output
       # OutputFileName: A string contining the output file name
       # GraphicTitle: A string containing the title for the decision table graphic
       # RowHeader: A string containing the descriptive title for row names
       # ColumnHeader: A string containing the descriptive title for column names
       # IconList: A vector of icons names to be printed right of the GraphicTitle, number can not exceed 3, default = NULL
       # SummaryRowOption: String indicating options (see below)
            # "Off": No summary row (default)
            # "MeanRank": Summary row added, values for bars = mean rank across rows for each column
            # "SumRank": Summary row added, values for bars = summed rank across rows for each column
       # ImageWidth: A number specifying total graphic image width in number of pixels, default = 500
       # ImageHeight: A number specifying total graphic image height in number of pixels, default = 800
# NONONO       # GraphicCellHeights: A vector containing row heights, must be same length as GraphicNRow
# NONONO       # GraphicCellWidths: A vector containing column widths, must be same length as GraphicNCol
       # BarWidth: A number specifying the width of the plotted bar
       # BarColors: Single color (for all bars the same color) or a vector equal in length to the number of columns in Data, used to color ranks, default = NULL
       # BarFigureMargins: Figure margins defined, default = c(0,0,0,0)
  
    # Returns:
       # A ploted decision table with customized graphics
  
  
  # Create storage objects
  RankOrder <- matrix(NA, nrow=nrow(Data), ncol=ncol(Data)) # used for ranking performance (last row will remain empty if no summary row added)
  
  ######################################################################
  # Plot graphics
  ######################################################################
  # Create png: filename, width, height can all be adjusted
  png(filename = paste(OutputDirectory, paste(OutputFileName, ".png", sep=""), sep="/"), width = ImageWidth*Resolution, height = ImageHeight*Resolution)
  
  # Pick correct graphic format details (number of rows/columns, layout matrix) for given data set Data
     # Includes sourcing files with dependent functions
  if(ncol(Data) == 1){
    #source(paste(DecisionTablePath, "1Col_Graphics.R", sep="/" ))
    GraphicFormatDetails <- GraphicFormat1Col(Data, SummaryRowOption = SummaryRowOption)
  } else if(ncol(Data) == 2){
    GraphicFormatDetails <- GraphicFormat2Col(Data, SummaryRowOption = SummaryRowOption)
  } else if(ncol(Data) == 3){
    GraphicFormatDetails <- GraphicFormat3Col(Data, SummaryRowOption = SummaryRowOption)
  } else if(ncol(Data) == 4){
    GraphicFormatDetails <- GraphicFormat4Col(Data, SummaryRowOption = SummaryRowOption)
  } else if(ncol(Data) == 5){
    GraphicFormatDetails <- GraphicFormat5Col(Data, SummaryRowOption = SummaryRowOption)
  } else if(ncol(Data) == 6){
    GraphicFormatDetails <- GraphicFormat6Col(Data, SummaryRowOption = SummaryRowOption)
  } else if(ncol(Data) == 7){
    GraphicFormatDetails <- GraphicFormat7Col(Data, SummaryRowOption = SummaryRowOption)
  } else if(ncol(Data) == 8){
    GraphicFormatDetails <- GraphicFormat8Col(Data, SummaryRowOption = SummaryRowOption)
  } else if(ncol(Data) == 9){
    GraphicFormatDetails <- GraphicFormat9Col(Data, SummaryRowOption = SummaryRowOption)
  } else if(ncol(Data) == 10){
    GraphicFormatDetails <- GraphicFormat10Col(Data, SummaryRowOption = SummaryRowOption)
  } else if(ncol(Data) == 11){
    GraphicFormatDetails <- GraphicFormat11Col(Data, SummaryRowOption = SummaryRowOption)
  } else if(ncol(Data) == 12){
    GraphicFormatDetails <- GraphicFormat12Col(Data, SummaryRowOption = SummaryRowOption)
  } else if(ncol(Data) == 13){
    GraphicFormatDetails <- GraphicFormat13Col(Data, SummaryRowOption = SummaryRowOption)
  } else if(ncol(Data) == 14){
    GraphicFormatDetails <- GraphicFormat14Col(Data, SummaryRowOption = SummaryRowOption)
  } else if(ncol(Data) == 15){
    GraphicFormatDetails <- GraphicFormat15Col(Data, SummaryRowOption = SummaryRowOption)
  } else if(ncol(Data) == 16){
    GraphicFormatDetails <- GraphicFormat16Col(Data, SummaryRowOption = SummaryRowOption)
  } else if(ncol(Data) == 17){
    GraphicFormatDetails <- GraphicFormat17Col(Data, SummaryRowOption = SummaryRowOption)
  } else if(ncol(Data) == 18){
    GraphicFormatDetails <- GraphicFormat18Col(Data, SummaryRowOption = SummaryRowOption)
  } else if(ncol(Data) == 19){
    GraphicFormatDetails <- GraphicFormat19Col(Data, SummaryRowOption = SummaryRowOption)
  } else if(ncol(Data) == 20){
    GraphicFormatDetails <- GraphicFormat20Col(Data, SummaryRowOption = SummaryRowOption)
  }
  
  # Set widths for graphic columns
  if(is.null(GraphicCellWidths) == TRUE){
    GraphicWidths <-  c(2,rep(1, GraphicFormatDetails$GraphicNCol-1),0.25) # c(rep(1, GraphicFormatDetails$GraphicNCol-1), 0.5)
  } else{
    GraphicWidths <- c(GraphicCellWidths[1],rep(GraphicCellWidths[2], GraphicFormatDetails$GraphicNCol-1),0.25) 
  }
  
  # Set heights for graphic rows
  if(is.null(GraphicCellHeight) == TRUE){
    GraphicHeights <- c(rep(c(1,0.25),3),rep(c(2,0.25), GraphicFormatDetails$GraphicNRow/2-3)) 
  } else{
    GraphicHeights <- c(rep(c(1,0.25),3),rep(c(GraphicCellHeight,0.25), GraphicFormatDetails$GraphicNRow/2-3)) 
  }
  
  # Produce final GraphicFormat
  GraphicFormat <- layout(matrix(GraphicFormatDetails$GraphicLayout, nrow = GraphicFormatDetails$GraphicNRow, ncol = GraphicFormatDetails$GraphicNCol, byrow = TRUE), widths = GraphicWidths, heights = GraphicHeights)
  #layout.show(GraphicFormat)
  #lcm(10)
  
  # Set bar colors
  if(is.null(BarColors)){
    BarColors <- GraphicFormatDetails$GraphicBarColors
  } else {
    BarColors <- BarColors
  }
  
  
  ########## Set up title and headers for table ##########
  # GraphicTitle
  par(mar=c(0,0,0,0))
  plot(1,1,type="n", axes=FALSE, ann=FALSE)
  text(1,1,labels=c(GraphicTitle), cex=2.5*Resolution)
  
  # Optional printed icons to right of title, may not exceed 3 icons
  library(raster)
  library(png)
  if(length(IconList) == 3){
    for(icon in 1:length(IconList)){
      #IconImage <- readPNG(paste(IconList[icon], ".png", sep=""))
      IconImage <- readPNG(IconList[icon])
      plot(1,1, axes=FALSE, ann=FALSE) # Sets up empty plot
      lim <- par() # Gets boundaries of plot space
      rasterImage(IconImage, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4]) # Plots raster image in plot space bounded by lim
    }
  } else if(is.null(IconList)==TRUE){
    for(empty in 1:3){
      par(mar=c(0,0,0,0))
      plot(1,1,type="n", axes=FALSE, ann=FALSE)
      print("Empty Plot Icon" )
    }
  } else if(length(IconList) < 3) { # This fills remaining slots in first row with empty plots if fewer icons than slots (including no icon list)
    for(icon in 1:length(IconList)){
      #IconImage <- readPNG(paste(IconList[icon], ".png", sep=""))
      IconImage <- readPNG(IconList[icon])
      plot(1,1, axes=FALSE, ann=FALSE) # Sets up empty plot
      lim <- par() # Gets boundaries of plot space
      rasterImage(IconImage, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4]) # Plots raster image in plot space bounded by lim
    }
    for(empty in 1:(3 - length(IconList))){
      par(mar=c(0,0,0,0))
      plot(1,1,type="n", axes=FALSE, ann=FALSE)
      print("Empty Plot Icon" )
    }
  } 
  
  # Plot empty space on right side of graph
  plot(1,1,type="n", axes=FALSE, ann=FALSE)
  
  # Plot first horizontal division
  par(mar=c(0,0.5,0,0.5))
  plot(1,1,type="n", axes=FALSE, ann=FALSE)
  abline(h=1, col="black", lwd=2)
  
  # RowHeader
  par(mar=c(0,0,0,0))
  plot(1,1,type="n", axes=FALSE, ann=FALSE)
  text(1,1,labels=c(RowHeader), cex=2*Resolution) 
  
  # ColumnHeader
  par(mar=c(0,0,0,0))
  plot(1,1,type="n", axes=FALSE, ann=FALSE)
  text(1,1,labels=c(ColumnHeader), cex=2*Resolution) 
  
  # Plot horizontal division line
  par(mar=c(0,0.5,0,0.5))
  plot(1,1,type="n", axes=FALSE, ann=FALSE)
  abline(h=1, col="black", lwd=1)
  
  # Plot column names
  for(Name in 1:ncol(Data)){
    #par(mar=c(0,0,0,0))
    plot(1,1,type = "n", axes = FALSE, ann = FALSE)
    text(1,1, labels = colnames(Data)[Name], cex=2*Resolution)
  }
  
  ########## Repeating information in the table (plot Data matrix) ##########
  for(row in 1:nrow(Data)){
    # Plot horizontal division line
    par(mar=c(0,0.5,0,0.5))
    plot(1,1,type="n", axes=FALSE, ann=FALSE)
    abline(h=1, col="black", lwd=1)
    
    # Plot RowName
    plot(1,1,type="n", axes=FALSE, ann=FALSE)
    text(1,1,labels=rownames(Data)[row], cex=2*Resolution)
    
    # Determine rank of data and associated coloring by looping over columns in each row
    if(BestPerformanceVector[row] == "High"){
      # Rank from high to low
      RankOrder[row, ] <- rank(Data[row, ]) # will not be produced if a rowname does not match something in the if statement (returns Rank not found error)
    } else if(BestPerformanceVector[row] == "Low"){
      # Rank from low to high
      RankOrder[row, ] <- rank(-Data[row,])
    }
    
    # Plot bars
    for(i in 1:ncol(Data)){
      par(mar=BarFigureMargins, xpd=TRUE)
      barplot(height=Data[row,i], 
              width=BarWidth, 
              space=0, 
              horiz=FALSE, 
              col=BarColors[RankOrder[row, i]],
              ylim=c(0,1.1*max(Data)), # Max across whole data table
              cex.axis=1*Resolution, 
              cex.names=1*Resolution, 
              offset=0)
      Labels <- round(Data[row,i], digits=2) # Print value rounded to 2 digits below bar
      mtext(Labels, side=1, cex=1.2*Resolution, line=1)
    }
  }
  
  
  ########## Plot Summary Row as specified ##########
  if(SummaryRowOption == "Off"){ ########## SummaryRowOption = "Off" #################################################################
    print("You are done =)")
  } else if(SummaryRowOption == "MeanRank"){ ########## SummaryRowOption = "MeanRank" ##################################################
    # Calculate summary row data as specified
    SummaryRowData <- colMeans(RankOrder) 

    # Rank summary from high to low for coloring. Highest rank always correspond with best performance (may represent high or low values) so "High" (best) coloring given to highest rank
    SummaryRankOrder <- rank(SummaryRowData)
    
    # Plot horizontal division line
    par(mar=c(0,0.5,0,0.5))
    plot(1,1,type="n", axes=FALSE, ann=FALSE)
    abline(h=1, col="black", lwd=1)
    
    # Plot RowName
    plot(1,1,type="n", axes=FALSE, ann=FALSE)
    text(1,1,labels="Summary", cex=2*Resolution)
    
    # Plot bars
    for(i in 1:length(SummaryRowData)){
      par(mar=BarFigureMargins, xpd=TRUE)
      barplot(height=SummaryRowData[i],
              width=BarWidth,
              space=0,
              horiz=FALSE,
              col=BarColors[SummaryRankOrder[i]],
              ylim=c(0,1.15*max(SummaryRowData)), # Max across whole data table
              cex.axis=1*Resolution,
              cex.names=1*Resolution,
              offset=0,
              xaxt='n')
      Labels <- round(SummaryRowData[i], digits=2) # Print value rounded to 2 digits below bar
      mtext(Labels, side=1, cex=1.2*Resolution, line=1)
    }
  } else if(SummaryRowOption == "SumRank"){ ########## SummaryRowOption = "SumRank" ##################################################
    # Calculate summary row data as specified
    SummaryRowData <- colSums(RankOrder)
    
    # Rank summary from high to low for coloring. Highest rank always correspond with best performance (may represent high or low values) so "High" (best) coloring given to highest rank
    SummaryRankOrder <- rank(SummaryRowData)
    
    # Plot horizontal division line
    par(mar=c(0,0.5,0,0.5))
    plot(1,1,type="n", axes=FALSE, ann=FALSE)
    abline(h=1, col="black", lwd=1)
    
    # Plot RowName
    plot(1,1,type="n", axes=FALSE, ann=FALSE)
    text(1,1,labels="Summary", cex=2*Resolution)
    
    # Plot bars
    for(i in 1:length(SummaryRowData)){
      par(mar=BarFigureMargins, xpd=TRUE)
      barplot(height=SummaryRowData[i],
              width=BarWidth,
              space=0,
              horiz=FALSE,
              col=BarColors[SummaryRankOrder[i]],
              ylim=c(0,1.15*max(SummaryRowData)), # Max across whole data table
              cex.axis=1*Resolution,
              cex.names=1*Resolution,
              offset=0,
              xaxt='n')
      Labels <- round(SummaryRowData[i], digits=2) # Print value rounded to 2 digits below bar
      mtext(Labels, side=1, cex=1.2*Resolution, line=1)
    }
  } else if(SummaryRowOption == "MedianRank"){ ########## SummaryRowOption = "MedianRank" ##################################################
    # Calculate summary row data as specified
    SummaryRowData <- apply(RankOrder, 2, median) 
    
    # Rank summary from high to low for coloring. Highest rank always correspond with best performance (may represent high or low values) so "High" (best) coloring given to highest rank
    SummaryRankOrder <- rank(SummaryRowData)
    
    # Plot horizontal division line
    par(mar=c(0,0.5,0,0.5))
    plot(1,1,type="n", axes=FALSE, ann=FALSE)
    abline(h=1, col="black", lwd=1)
    
    # Plot RowName
    plot(1,1,type="n", axes=FALSE, ann=FALSE)
    text(1,1,labels="Summary", cex=2*Resolution)
    
    # Plot bars
    for(i in 1:length(SummaryRowData)){
      par(mar=BarFigureMargins, xpd=TRUE)
      barplot(height=SummaryRowData[i],
              width=BarWidth,
              space=0,
              horiz=FALSE,
              col=BarColors[SummaryRankOrder[i]],
              ylim=c(0,1.15*max(SummaryRowData)), # Max across whole data table
              cex.axis=1*Resolution,
              cex.names=1*Resolution,
              offset=0,
              xaxt='n')
      Labels <- round(SummaryRowData[i], digits=2) # Print value rounded to 2 digits below bar
      mtext(Labels, side=1, cex=1.2*Resolution, line=1)
    }   
  } else if(SummaryRowOption == "MeanValue"){ ########## SummaryRowOption = "MeanValue" ##################################################
    # Calculate summary row data as specified
    SummaryRowData <-  colMeans(Data) 

    # Rank summary from high to low for coloring. Default = Highest rank correspond with best performance 
    if(SummaryBestPerformance == "High"){
      # Rank from high to low
      SummaryRankOrder <- rank(SummaryRowData) # will not be produced if a rowname does not match something in the if statement (returns Rank not found error)
    } else if(SummaryBestPerformance == "Low"){
      # Rank from low to high
      SummaryRankOrder <- rank(-SummaryRowData)
    }

    # Plot horizontal division line
    par(mar=c(0,0.5,0,0.5))
    plot(1,1,type="n", axes=FALSE, ann=FALSE)
    abline(h=1, col="black", lwd=1)
    
    # Plot RowName
    plot(1,1,type="n", axes=FALSE, ann=FALSE)
    text(1,1,labels="Summary", cex=2*Resolution)
    
    # Plot bars
    for(i in 1:length(SummaryRowData)){
      par(mar=BarFigureMargins, xpd=TRUE)
      barplot(height=SummaryRowData[i],
              width=BarWidth,
              space=0,
              horiz=FALSE,
              col=BarColors[SummaryRankOrder[i]],
              ylim=c(0,1.15*max(SummaryRowData)), # Max across whole data table
              cex.axis=1*Resolution,
              cex.names=1*Resolution,
              offset=0,
              xaxt='n')
      Labels <- round(SummaryRowData[i], digits=2) # Print value rounded to 2 digits below bar
      mtext(Labels, side=1, cex=1.2*Resolution, line=1)
    }    
  } else if(SummaryRowOption == "SumValue"){ ########## SummaryRowOption = "SumValue" ##################################################
    # Calculate summary row data as specified
    SummaryRowData <- colSums(Data)
    
    # Rank summary from high to low for coloring. Default = Highest rank correspond with best performance 
    if(SummaryBestPerformance == "High"){
      # Rank from high to low
      SummaryRankOrder <- rank(SummaryRowData) # will not be produced if a rowname does not match something in the if statement (returns Rank not found error)
    } else if(SummaryBestPerformance == "Low"){
      # Rank from low to high
      SummaryRankOrder <- rank(-SummaryRowData)
    }
    
    # Plot horizontal division line
    par(mar=c(0,0.5,0,0.5))
    plot(1,1,type="n", axes=FALSE, ann=FALSE)
    abline(h=1, col="black", lwd=1)
    
    # Plot RowName
    plot(1,1,type="n", axes=FALSE, ann=FALSE)
    text(1,1,labels="Summary", cex=2*Resolution)
    
    # Plot bars
    for(i in 1:length(SummaryRowData)){
      par(mar=BarFigureMargins, xpd=TRUE)
      barplot(height=SummaryRowData[i],
              width=BarWidth,
              space=0,
              horiz=FALSE,
              col=BarColors[SummaryRankOrder[i]],
              ylim=c(0,1.15*max(SummaryRowData)), # Max across whole data table
              cex.axis=1*Resolution,
              cex.names=1*Resolution,
              offset=0,
              xaxt='n')
      Labels <- round(SummaryRowData[i], digits=2) # Print value rounded to 2 digits below bar
      mtext(Labels, side=1, cex=1.2*Resolution, line=1)
    }  
  } else if(SummaryRowOption == "MedianValue"){ ########## SummaryRowOption = "MedianValue" ##################################################
    # Calculate summary row data as specified
    SummaryRowData <- apply(Data, 2, median) 
    
    # Rank summary from high to low for coloring. Default = Highest rank correspond with best performance 
    if(SummaryBestPerformance == "High"){
      # Rank from high to low
      SummaryRankOrder <- rank(SummaryRowData) # will not be produced if a rowname does not match something in the if statement (returns Rank not found error)
    } else if(SummaryBestPerformance == "Low"){
      # Rank from low to high
      SummaryRankOrder <- rank(-SummaryRowData)
    }
    
    # Plot horizontal division line
    par(mar=c(0,0.5,0,0.5))
    plot(1,1,type="n", axes=FALSE, ann=FALSE)
    abline(h=1, col="black", lwd=1)
    
    # Plot RowName
    plot(1,1,type="n", axes=FALSE, ann=FALSE)
    text(1,1,labels="Summary", cex=2*Resolution)

    # Plot bars
    for(i in 1:length(SummaryRowData)){
      par(mar=BarFigureMargins, xpd=TRUE)
      barplot(height=SummaryRowData[i],
              width=BarWidth,
              space=0,
              horiz=FALSE,
              col=BarColors[SummaryRankOrder[i]],
              ylim=c(0,1.15*max(SummaryRowData)), # Max across whole data table
              cex.axis=1*Resolution,
              cex.names=1*Resolution,
              offset=0,
              xaxt='n')
      Labels <- round(SummaryRowData[i], digits=2) # Print value rounded to 2 digits below bar
      mtext(Labels, side=1, cex=1.2*Resolution, line=1)
    } 
  }
  
  # Plot last horizontal division
  par(mar=c(0,0.5,0,0.5))
  plot(1,1,type="n", axes=FALSE, ann=FALSE)
  abline(h=1, col="black", lwd=2)

  dev.off() 
}





