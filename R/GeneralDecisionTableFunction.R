# Reformated functions to produce barplots with error bars (relies on ggplot2 rather than base R for plots)


#' @title Produce generalized decision tables with provided confidence intervals (optional) on bars.
#' 
#' @description 
#' This function produces a decision table with between 1 and 20 rows 
#' and columns based on the dimensions of the provided data matrix.
#' Decision table labels, colors, performance ranking, and decision 
#' table dimensions can be specified. Confidence intervals may be specified using 
#' the upper bounds provided in Data_UpperCI and lower bounds provided in
#' Data_LowerCI matrices (confidence intervals are not automatically generated).
#' Up to three icons may be printed next to the title, and a summary row can 
#' also be automatically calculated. 
#' 
#' @param Data A matrix with row and column names (must me matrix even if only 1 row or column), no default
#' @param BestPerformanceVector A vector equal in length to the number of rows in Data containing "High" or "Low", no default
#'      "High" means highest value in row will be colored to represent best performance
#'      "Low" means lowest value in row will be colored to represent best performance
#' @param SummaryBestPerformance A string containing "High" or "Low" to indicate best performance for summary row, no default.
#'      Only required if "MeanValue", "SumValue", or "MedianValue" options chosen. For other options higher summarized rank correspond with better performance
#' @param OutputDirectory A string containing the full path name of folder where resulting graphic should be stored, default = current working directory
#' @param OutputFileName A string containing the file name for the resulting graphic, default = "DecisionTable"
#' @param GraphicTitle A string containing the title for the decision table graphic, default = "Title"
#' @param RowHeader A string containing the descriptive title for row names, default = "RowHeader"
#' @param ColumnHeader A string containing the descriptive title for column names, default = "ColumnHeader"
#' @param IconList A vector of icon names to be printed right of the GraphicTitle, length may not exceed 3, default = no icons.
#'        A custom black-and-white icon may be printed by providing the filename with .png extension, or an icon supported by the package may be chosen by providing one of the following icon names:
#'        "Commercial_Fisheries_Herring_Mackerel_Lobster",  "Environmental_Considerations",  "Groundfish_Fishery",  "Groundfish_Species",  "Herring_Fishery_Option1",  "Herring_Fishery_Option2",
#'        "Lobster",  "Lobster_Fishery",  "Predator_Fisheries_Tuna_Haddock_Flatfish",  "Predator_Species_Tuna_Haddock_Flatfish",  "Primary_Production",  "Protected_Species",  "Protected_Species_and_Tourism",
#'        "Tourism",  "Tuna",  "Tuna_Fishery"
#' @param IconColor A string specifying the color of icons to be printed, only necessary if IconList provide. default = "black"
#' @param SummaryRowOption A string indicating summary row option, default = "Off" (no summary row)
#' 
#'      "Off"          = No summary row
#'      
#'      "MeanRank"     = Summary row added, values represent mean rank across all rows in each column, highest rank correspond with best performance
#'      
#'      "SumRank"      = Summary row added, values represent summed rank across all rows in each column, highest rank correspond with best performance
#'      
#'      "MedianRank"   = Summary row added, values represent median rank across all rows in each column, highest rank correspond with best performance 
#'      
#'      "MeanValue"    = Summary row added, values represent mean value of Data across all rows in each column, high/low value corresponding with best performance must be specified
#'      
#'      "SumValue"     = Summary row added, values represent sum of Data values across all rows in each column, high/low value corresponding with best performance must be specified
#'      
#'      "MedianValue"  = Summary row added, values represent median value of Data across all rows in each column, high/low value corresponding with best performance must be specified
#'      
#' @param ImageWidth A number specifying total graphic image width in number of pixels, default = 500
#' @param ImageHeight A number specifying total graphic image height in number of pixels, default = 800
#' @param Resolution A number between 1 and 3, default = 1.
#'      This parameter increases the image resolution by scaling the number of pixels in the length and width by an equal amount. 
#'      Increasing the resolution (higher numbers) will result in a larger image which can be adjusted in any image processing software to the desired dimension.
#' @param GraphicCellHeight A number specifying height of graphic rows containing data
#' @param GraphicCellWidths A vector of length 2 specifying first the width of row label column second data column width
#' @param BarWidth A number specifying the width of the plotted bar
#' @param BarColors A string specifying single color or a vector of colors equal in length to the number of columns in Data. Default uses colors specified by data dimenstions to visually show ranked performance.
#' @param IncludeCI A string specifying the inclusion of confidence levels in plot, if = "TRUE" then Data_UpperCI and Data_LowerCI must also be provided. default = "FALSE"
#' @param Data_UpperCI A matrix containing upper confidence levels with row and column names that match Data (must me matrix even if only 1 row or column), no default.
#' @param Data_LowerCI A matrix containing lower confidence levels with row and column names that match Data (must me matrix even if only 1 row or column), no default.
#' 
#' @return Customized decision table image (.png) with optional confidence intervals on bars.
#' @export
#' 
#' @examples
#' Produce example decision table and save as DecisionTable.png in current working directory.
#' MakeDecisionTable(Data = TestMatrix_Ncol_4_Nrow_3, BestPerformanceVector = c("High","High","High","High"))
#' MakeDecisionTable(Data = TestMatrix_Ncol_4_Nrow_3, BestPerformanceVector = c("High","High","High","High"),
#'                   IconList = c("Tuna", "Groundfish_Fishery", "Environmental_Considerations"))
#' MakeDecisionTable(Data = TestMatrix_Ncol_4_Nrow_3, BestPerformanceVector = c("High","High","High","High"),
#'                   IconList = c("Protected_Species", "Commercial_Fisheries_Herring_Mackerel_Lobster", "Predator_Species_Tuna_Haddock_Flatfish"), 
#'                   IconColor = "blue")
#' MakeDecisionTable(Data = TestMatrix_Ncol_4_Nrow_3, BestPerformanceVector = c("High","High","High","High"), 
#'                   SummaryBestPerformance = c("High","High","High","High"),
#'                   SummaryRowOption = "SumRank")
#'





############### Define General Decision Table Function ###############

MakeDecisionTable <- function(Data,
                              BestPerformanceVector,
                              SummaryBestPerformance = NULL,
                              OutputDirectory = getwd(),
                              OutputFileName = "DecisionTable",
                              GraphicTitle = "Title",
                              RowHeader = "RowHeader",
                              ColumnHeader = "ColumnHeader",
                              IconList = NULL,
                              IconColor = "black",
                              SummaryRowOption = "Off",
                              ImageWidth = 500,
                              ImageHeight = 800,
                              Resolution = 1,
                              GraphicCellHeight = NULL, 
                              GraphicCellWidths = NULL, 
                              BarWidth = 1,
                              BarColors = NULL,
                              IncludeCI = "FALSE",
                              Data_UpperCI = NULL,
                              Data_LowerCI = NULL){
  
  # Library dependent packages  
  # library(ggplot2)
  # library(ggplotify)
  # library(grid)
  # library(gridExtra)
  # library(png)
  # library(raster)
  # library(rasterVis)
  # library(rgdal)
  
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
    GraphicWidths <-  c(2,rep(1, (GraphicFormatDetails$GraphicNCol-2)),0.25) # c(rep(1, GraphicFormatDetails$GraphicNCol-1), 0.5)
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
  GraphicFormat <- matrix(GraphicFormatDetails$GraphicLayout, nrow = GraphicFormatDetails$GraphicNRow, ncol = GraphicFormatDetails$GraphicNCol, byrow = TRUE)
  GridList <- NULL
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
  GraphicTitleGrob <- textGrob(GraphicTitle, gp=gpar(cex=1.5*Resolution))
  GridList <- gList(GridList, GraphicTitleGrob)
  
  # Optional printed icons to right of title, may not exceed 3 icons
  if(length(IconList) == 3){
    for(icon in 1:length(IconList)){ # If there are 3 icons
      if(IconList[icon] == "Commercial_Fisheries_Herring_Mackerel_Lobster" | 
         IconList[icon] == "Environmental_Considerations" |
         IconList[icon] == "Groundfish_Fishery" |
         IconList[icon] == "Groundfish_Species" |
         IconList[icon] == "Herring_Fishery_Option1" |
         IconList[icon] == "Herring_Fishery_Option2" |
         IconList[icon] == "Lobster" |
         IconList[icon] == "Lobster_Fishery" |
         IconList[icon] == "Predator_Fisheries_Tuna_Haddock_Flatfish" |
         IconList[icon] == "Predator_Species_Tuna_Haddock_Flatfish" |
         IconList[icon] == "Primary_Production" |
         IconList[icon] == "Protected_Species" |
         IconList[icon] == "Protected_Species_and_Tourism" |
         IconList[icon] == "Tourism" |
         IconList[icon] == "Tuna" |
         IconList[icon] == "Tuna_Fishery"){
         # raster IconImage is from .RData file
          print(paste("Icon", IconList[icon], sep="_"))
          IconImage <- eval(parse(text = paste("Icon", IconList[icon], sep="_")))
          IconImage <- raster(IconImage, layer=1, values=TRUE) # Change from "SpatialPixelsDataFrame" to raster format
        } else {
          IconImage <- raster(IconList[icon]) 
        }
      PlotIcon <- gplot(IconImage) + geom_tile(aes(colour = cut(value, c(0,180,Inf)), fill=cut(value, c(0,180,Inf)))) + # Alter middle number to change pixels coloring
        scale_color_manual(name = "UniqueScale",
                           values = c("(0,180]" = IconColor,
                                      "(180,Inf]" = "white"),breaks=NULL, na.value=IconColor) +
        scale_fill_manual(name = "UniqueScale",
                          values = c("(0,180]" = IconColor,
                                     "(180,Inf]" = "white"),breaks=NULL, na.value=IconColor) +
        coord_equal() + 
        theme(text = element_blank(),
              rect = element_blank(),
              line = element_blank())
      
      GridList <- gList(GridList, as.grob(assign(paste("Icon_", icon, sep=""), PlotIcon)))
    }
  } else if(is.null(IconList)==TRUE){ # If there are no icons
    for(empty in 1:3){ # Fill all columns except title in first row with empty plot (Title, empty the rest of row)
      EmptySpace <- textGrob(" ")
      GridList <- gList(GridList, EmptySpace)
      print("Empty Plot Icon" )
    }
  } else if(length(IconList) < 3) { # This fills remaining slots in first row with empty plots if fewer icons than slots (including no icon list)
    for(icon in 1:length(IconList)){
      if(IconList[icon] == "Commercial_Fisheries_Herring_Mackerel_Lobster" | 
         IconList[icon] == "Environmental_Considerations" |
         IconList[icon] == "Groundfish_Fishery" |
         IconList[icon] == "Groundfish_Species" |
         IconList[icon] == "Herring_Fishery_Option1" |
         IconList[icon] == "Herring_Fishery_Option2" |
         IconList[icon] == "Lobster" |
         IconList[icon] == "Lobster_Fishery" |
         IconList[icon] == "Predator_Fisheries_Tuna_Haddock_Flatfish" |
         IconList[icon] == "Predator_Species_Tuna_Haddock_Flatfish" |
         IconList[icon] == "Primary_Production" |
         IconList[icon] == "Protected_Species" |
         IconList[icon] == "Protected_Species_and_Tourism" |
         IconList[icon] == "Tourism" |
         IconList[icon] == "Tuna" |
         IconList[icon] == "Tuna_Fishery"){
        # raster IconImage is from .RData file
        print(paste("Icon", IconList[icon], sep="_"))
        IconImage <- eval(parse(text = paste("Icon", IconList[icon], sep="_")))
        IconImage <- raster(IconImage, layer=1, values=TRUE) # Change from "SpatialPixelsDataFrame" to raster format
      } else {
        IconImage <- raster(IconList[icon]) 
      }
      PlotIcon <- gplot(IconImage) + geom_tile(aes(colour = cut(value, c(0,180,Inf)), fill=cut(value, c(0,180,Inf)))) + 
        scale_color_manual(name = "UniqueScale",
                           values = c("(0,180]" = IconColor,
                                      "(180,Inf]" = "white"),breaks=NULL, na.value=IconColor) +
        scale_fill_manual(name = "UniqueScale",
                          values = c("(0,180]" = "black",
                                     "(180,Inf]" = "white"),breaks=NULL, na.value=IconColor) +
        coord_equal() + 
        theme(text = element_blank(),
              rect = element_blank(),
              line = element_blank())
      
      GridList <- gList(GridList, as.grob(assign(paste("Icon_", icon, sep=""), PlotIcon)))
    }
    for(empty in 1:(3 - length(IconList))){
      EmptySpace <- textGrob(" ")
      GridList <- gList(GridList, EmptySpace)
      print("Empty Plot Icon" )
    }
  } 
  
  # Plot empty space on right side of graph
  EmptySpace <- textGrob(" ")
  GridList <- gList(GridList, EmptySpace)
  
  # Plot first horizontal division
  EmptyDataFrame <- data.frame()
  HorizontalLine <- ggplot(EmptyDataFrame) + 
    geom_hline(yintercept = 0) + 
    theme(axis.text.x=element_blank(), 
          axis.text.y=element_blank(),
          axis.ticks.x=element_blank(),
          axis.ticks.y=element_blank(),
          axis.title.y=element_blank(), 
          axis.title.x=element_blank(),
          panel.background = element_blank())
  HorizontalLine <- as.grob(HorizontalLine) # Make ggplot a grob, could alternatively do this in the following line, but doing it here means I only do it once (not for every horizontal line)
  for(i in GraphicFormatDetails$GraphicNCol){
    GridList <- gList(GridList, HorizontalLine)
  }
  
  # RowHeader
  RowHeaderLabel <- textGrob(RowHeader, gp=gpar(cex=1.2*Resolution))
  GridList <- gList(GridList, RowHeaderLabel)
  
  # ColumnHeader
  ColumnHeaderLabel <- textGrob(ColumnHeader, gp=gpar(cex=1.2*Resolution))
  GridList <- gList(GridList, ColumnHeaderLabel)
  
  # Plot horizontal division line
  GridList <- gList(GridList, HorizontalLine)
  
  # Plot column names
  for(Name in 1:ncol(Data)){
    ColNameGrob <- textGrob(colnames(Data)[Name], gp=gpar(cex=1*Resolution))
    GridList <- gList(GridList, assign(paste("Column_", Name, "_Title", sep=""), ColNameGrob))
  }
  
  
  ########## Repeating information in the table (plot Data matrix) ##########
  for(row in 1:nrow(Data)){
    # Plot horizontal division line
    GridList <- gList(GridList, HorizontalLine)
    
    # Plot RowName
    RowNameGrob <- textGrob(rownames(Data)[row], gp=gpar(cex=1*Resolution))
    GridList <- gList(GridList, assign(paste("Row_", row, "_Title", sep=""), RowNameGrob))
    
    # Determine rank of data and associated coloring by looping over columns in each row
    if(BestPerformanceVector[row] == "High"){
      # Rank from high to low
      RankOrder[row, ] <- rank(Data[row, ]) # will not be produced if a rowname does not match something in the if statement (returns Rank not found error)
    } else if(BestPerformanceVector[row] == "Low"){
      # Rank from low to high
      RankOrder[row, ] <- rank(-Data[row,])
    }
    
    # Format barplots
    if(IncludeCI == "TRUE"){
      for(col in 1:ncol(Data)){
        PlotData <- data.frame(Data[row, col],Data[row, col])
        colnames(PlotData) <- c("XX", "YY")
        PlotColor <- BarColors[RankOrder[row,col]]
        
        Error_UpperCI <- Data_UpperCI[row, col]
        Error_LowerCI <- Data_LowerCI[row, col]
        limits <- aes(ymax = Error_UpperCI, ymin = Error_LowerCI)
        
        barplot <- ggplot(PlotData, aes(x=XX, y=YY))
        PrettyBarPlot <- barplot + geom_col(fill = PlotColor) +
          geom_errorbar(limits, width = 0.25, size=0.5*Resolution) + 
          theme(axis.text.x=element_blank(),
                axis.text.y=element_text(size=10*Resolution),
                axis.ticks.x=element_blank(),
                axis.title.y=element_blank(), 
                #axis.title.x=element_blank(),
                panel.background = element_blank(),
                axis.line.x = element_line(color="black", size=0.5*Resolution), 
                axis.line.y = element_line(color="black", size=0.5*Resolution),
                text = element_text(size=12*Resolution)) +
          labs(x = paste(round(Data[row,col], digits=2))) + # Print value rounded to 2 digits below bar
          scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0), limits=c(0,max(Data_UpperCI)+2)) # y-axis must encompass uppermost CI provided
        
        # Save Barplot for formatting later
        GridList <- gList(GridList, as.grob(assign(paste("CIBarplot_row_", row, "_column_", col, sep=""), PrettyBarPlot)))
      }
    } else {
      for(col in 1:ncol(Data)){
        PlotData <- data.frame(Data[row, col],Data[row, col])
        colnames(PlotData) <- c("XX", "YY")
        PlotColor <- BarColors[RankOrder[row,col]]
        
        Error_UpperCI <- Data_UpperCI[row, col]
        Error_LowerCI <- Data_LowerCI[row, col]
        limits <- aes(ymax = Error_UpperCI, ymin = Error_LowerCI)
        
        barplot <- ggplot(PlotData, aes(x=XX, y=YY))
        PrettyBarPlot <- barplot + geom_col(fill = PlotColor) +
          theme(axis.text.x=element_blank(),
                axis.text.y=element_text(size=10*Resolution),
                axis.ticks.x=element_blank(),
                axis.title.y=element_blank(), 
                #axis.title.x=element_blank(),
                panel.background = element_blank(),
                axis.line.x = element_line(color="black", size=0.5*Resolution), 
                axis.line.y = element_line(color="black", size=0.5*Resolution),
                text = element_text(size=12*Resolution)) +
          labs(x = paste(round(Data[row,col], digits=2))) + # Print value rounded to 2 digits below bar
          scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0), limits=c(0,max(Data)+2)) # y-axis must encompass uppermost CI provided
        
        # Save Barplot for formatting later
        GridList <- gList(GridList, as.grob(assign(paste("Barplot_row_", row, "_column_", col, sep=""), PrettyBarPlot)))
      }
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
    GridList <- gList(GridList, HorizontalLine)
    
    # Plot RowName
    RowNameGrob <- textGrob("Summary", gp=gpar(cex=1.2*Resolution))
    GridList <- gList(GridList, assign("SummaryRowTitle", RowNameGrob))
    
    # Plot bars
    for(i in 1:length(SummaryRowData)){
      PlotData <- data.frame(SummaryRowData[i], SummaryRowData[i])
      colnames(PlotData) <- c("XX", "YY")
      PlotColor <- BarColors[SummaryRankOrder[i]]
      
      barplot <- ggplot(PlotData, aes(x=XX, y=YY))
      PrettyBarPlot <- barplot + geom_col(fill = PlotColor) +
        theme(axis.text.x=element_blank(),
              axis.text.y=element_text(size=10*Resolution),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(), 
              #axis.title.x=element_blank(),
              panel.background = element_blank(),
              axis.line.x = element_line(color="black", size=0.5*Resolution), 
              axis.line.y = element_line(color="black", size=0.5*Resolution),
              text=element_text(size=12*Resolution)) +
        labs(x = paste(round(SummaryRowData[i], digits=2))) + # Print value rounded to 2 digits below bar
        scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0), limits=c(0,max(SummaryRowData)+1))
      
      # Save Barplot for formatting later
      GridList <- gList(GridList, as.grob(assign(paste("SummaryBarplot_row_", row, "_column_", col, sep=""), PrettyBarPlot)))
    }
  } else if(SummaryRowOption == "SumRank"){ ########## SummaryRowOption = "SumRank" ##################################################
    # Calculate summary row data as specified
    SummaryRowData <- colSums(RankOrder)
    
    # Rank summary from high to low for coloring. Highest rank always correspond with best performance (may represent high or low values) so "High" (best) coloring given to highest rank
    SummaryRankOrder <- rank(SummaryRowData)
    
    # Plot horizontal division line
    GridList <- gList(GridList, HorizontalLine)
    
    # Plot RowName
    RowNameGrob <- textGrob("Summary", gp=gpar(cex=1.2*Resolution))
    GridList <- gList(GridList, assign("SummaryRowTitle", RowNameGrob))
    
    # Plot bars
    for(i in 1:length(SummaryRowData)){
      PlotData <- data.frame(SummaryRowData[i], SummaryRowData[i])
      colnames(PlotData) <- c("XX", "YY")
      PlotColor <- BarColors[SummaryRankOrder[i]]
      
      barplot <- ggplot(PlotData, aes(x=XX, y=YY))
      PrettyBarPlot <- barplot + geom_col(fill = PlotColor) +
        theme(axis.text.x=element_blank(), 
              axis.text.y=element_text(size=10*Resolution),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(), 
              #axis.title.x=element_blank(),
              panel.background = element_blank(),
              axis.line.x = element_line(color="black", size=0.5*Resolution), 
              axis.line.y = element_line(color="black", size=0.5*Resolution),
              text = element_text(size=12*Resolution)) +
        labs(x = paste(round(SummaryRowData[i], digits=2))) + # Print value rounded to 2 digits below bar
        scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0),limits=c(0,((GraphicFormatDetails$GraphicNCol-2)*((GraphicFormatDetails$GraphicNRow-8)/2)+2))) # Scales against highest possible rank (sum best rank across all rows)
      
      # Save Barplot for formatting later
      GridList <- gList(GridList, as.grob(assign(paste("SummaryBarplot_row_", row, "_column_", col, sep=""), PrettyBarPlot)))
    }
  } else if(SummaryRowOption == "MedianRank"){ ########## SummaryRowOption = "MedianRank" ##################################################
    # Calculate summary row data as specified
    SummaryRowData <- apply(RankOrder, 2, median) 
    
    # Rank summary from high to low for coloring. Highest rank always correspond with best performance (may represent high or low values) so "High" (best) coloring given to highest rank
    SummaryRankOrder <- rank(SummaryRowData)
    
    # Plot horizontal division line
    GridList <- gList(GridList, HorizontalLine)
    
    # Plot RowName
    RowNameGrob <- textGrob("Summary", gp=gpar(cex=1.2*Resolution))
    GridList <- gList(GridList, assign("SummaryRowTitle", RowNameGrob))
    
    # Plot bars
    for(i in 1:length(SummaryRowData)){
      PlotData <- data.frame(SummaryRowData[i], SummaryRowData[i])
      colnames(PlotData) <- c("XX", "YY")
      PlotColor <- BarColors[SummaryRankOrder[i]]
      
      barplot <- ggplot(PlotData, aes(x=XX, y=YY))
      PrettyBarPlot <- barplot + geom_col(fill = PlotColor) +
        theme(axis.text.x=element_blank(),
              axis.text.y=element_text(size=10*Resolution),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(), 
              #axis.title.x=element_blank(),
              panel.background = element_blank(),
              axis.line.x = element_line(color="black", size=0.5*Resolution), 
              axis.line.y = element_line(color="black", size=0.5*Resolution),
              text = element_text(size=12*Resolution)) +
        labs(x = paste(round(SummaryRowData[i], digits=2))) + # Print value rounded to 2 digits below bar
        scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0), limits=c(0,max(SummaryRowData)+1))
      
      # Save Barplot for formatting later
      GridList <- gList(GridList, as.grob(assign(paste("SummaryBarplot_row_", row, "_column_", col, sep=""), PrettyBarPlot)))
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
    GridList <- gList(GridList, HorizontalLine)
    
    # Plot RowName
    RowNameGrob <- textGrob("Summary", gp=gpar(cex=1.2*Resolution))
    GridList <- gList(GridList, assign("SummaryRowTitle", RowNameGrob))
    
    # Plot bars
    for(i in 1:length(SummaryRowData)){
      PlotData <- data.frame(SummaryRowData[i], SummaryRowData[i])
      colnames(PlotData) <- c("XX", "YY")
      PlotColor <- BarColors[SummaryRankOrder[i]]
      
      barplot <- ggplot(PlotData, aes(x=XX, y=YY))
      PrettyBarPlot <- barplot + geom_col(fill = PlotColor) +
        theme(axis.text.x=element_blank(), 
              axis.text.y=element_text(size=10*Resolution),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(), 
              #axis.title.x=element_blank(),
              panel.background = element_blank(),
              axis.line.x = element_line(color="black", size=0.5*Resolution), 
              axis.line.y = element_line(color="black", size=0.5*Resolution),
              text = element_text(size=12*Resolution)) +
        labs(x = paste(round(SummaryRowData[i], digits=2))) + # Print value rounded to 2 digits below bar
        scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0), limits=c(0,max(SummaryRowData)+1))
      
      # Save Barplot for formatting later
      GridList <- gList(GridList, as.grob(assign(paste("SummaryBarplot_row_", row, "_column_", col, sep=""), PrettyBarPlot)))
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
    GridList <- gList(GridList, HorizontalLine)
    
    # Plot RowName
    RowNameGrob <- textGrob("Summary", gp=gpar(cex=1.2*Resolution))
    GridList <- gList(GridList, assign("SummaryRowTitle", RowNameGrob))
    
    # Plot bars
    for(i in 1:length(SummaryRowData)){
      PlotData <- data.frame(SummaryRowData[i], SummaryRowData[i])
      colnames(PlotData) <- c("XX", "YY")
      PlotColor <- BarColors[SummaryRankOrder[i]]
      
      barplot <- ggplot(PlotData, aes(x=XX, y=YY))
      PrettyBarPlot <- barplot + geom_col(fill = PlotColor) +
        theme(axis.text.x=element_blank(), 
              axis.text.y=element_text(size=10*Resolution),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(), 
              #axis.title.x=element_blank(),
              panel.background = element_blank(),
              axis.line.x = element_line(color="black", size=0.5*Resolution), 
              axis.line.y = element_line(color="black", size=0.5*Resolution),
              text = element_text(size=12*Resolution)) +
        labs(x = paste(round(SummaryRowData[i], digits=2))) + # Print value rounded to 2 digits below bar
        scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0), limits=c(0,max(SummaryRowData)+2))
      
      # Save Barplot for formatting later
      GridList <- gList(GridList, as.grob(assign(paste("SummaryBarplot_row_", row, "_column_", col, sep=""), PrettyBarPlot)))
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
    GridList <- gList(GridList, HorizontalLine)
    
    # Plot RowName
    RowNameGrob <- textGrob("Summary", gp=gpar(cex=1.2*Resolution))
    GridList <- gList(GridList, assign("SummaryRowTitle", RowNameGrob))
    
    # Plot bars
    for(i in 1:length(SummaryRowData)){
      PlotData <- data.frame(SummaryRowData[i], SummaryRowData[i])
      colnames(PlotData) <- c("XX", "YY")
      PlotColor <- BarColors[SummaryRankOrder[i]]
      
      barplot <- ggplot(PlotData, aes(x=XX, y=YY))
      PrettyBarPlot <- barplot + geom_col(fill = PlotColor) +
        theme(axis.text.x=element_blank(), 
              axis.text.y=element_text(size=10*Resolution),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(), 
              #axis.title.x=element_blank(),
              panel.background = element_blank(),
              axis.line.x = element_line(color="black", size=0.5*Resolution), 
              axis.line.y = element_line(color="black", size=0.5*Resolution),
              text = element_text(size=12*Resolution)) +
        labs(x = paste(round(SummaryRowData[i], digits=2))) + # Print value rounded to 2 digits below bar
        scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0), limits=c(0,max(SummaryRowData)+1)) 
      
      
      # Save Barplot for formatting later
      GridList <- gList(GridList, as.grob(assign(paste("SummaryBarplot_row_", row, "_column_", col, sep=""), PrettyBarPlot)))
    } 
  }
  
  # Plot last horizontal division
  GridList <- gList(GridList, HorizontalLine)
  
  ########## Format #########
  #GridList <- gList(GridList) # Formally make everything in the GridList part of a grob list (if not done already)
  grid.arrange(grobs = GridList, layout_matrix = matrix(GraphicFormatDetails$GraphicLayout, nrow = GraphicFormatDetails$GraphicNRow, ncol = GraphicFormatDetails$GraphicNCol, byrow = TRUE), widths = GraphicWidths, heights = GraphicHeights, nrow = GraphicFormatDetails$GraphicNRow, ncol = GraphicFormatDetails$GraphicNCol, byrow = TRUE)
  #grid.arrange(grobs = GridList, layout_matrix = GraphicFormatDetails$GraphicLayout, widths = GraphicWidths, heights = GraphicHeights,  byrow = TRUE)
  
  dev.off() 
}



