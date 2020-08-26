# Reformated functions to produce barplots with error bars (relies on ggplot2 rather than base R for plots)


#' @title Produce generalized decision tables with provided confidence intervals (optional) on bars.
#'
#' @description
#' This function produces a decision table with dimensions based on those
#' of the provided data matrix or the specified dimensions if data is provided as vector.
#' Labels, figure layout and dimensions, and coloring scheme,
#' can be customized. Confidence intervals may be specified using
#' the upper bounds provided in Data_UpperCI and lower bounds provided in
#' Data_LowerCI data inputs (confidence intervals are not automatically generated).
#' Up to three icons may be printed next to the title, and a summary information can
#' also be automatically calculated and appended as the last row of the decision table.
#'
#' @param data A data matrix with dimensions matching the desired decision table or a vector equal in length to the number of cells in the desired decision table. By default vectors will fill the table by column according to the dimensions specified by `nrow` and `ncol` arguments, no default.
#' @param rownames A vector of strings equal in length to the number of rows to be used as row names for the decision table when data is provided as a vector, no default.
#' @param colnames A vector of strings equal in length to the number of columns to be used as column names for the decision table when data is provided as a vector, no default.
#' @param nrow A number indicating the number of rows to be included when data is provided as a vector, no default.
#' @param ncol A number indicating the number of columns to be included when data is provided as a vector, no default.
#' @param byrow A boolean string indicating whether data provided as a vector fills the table by row or not, default = TRUE.
#' @param OutputDirectory A string containing the full path name of folder where resulting graphic should be stored, default = current working directory
#' @param OutputFileName A string containing the file name for the resulting graphic, default = "DecisionTable"
#' @param GraphicTitle A string containing the title for the decision table graphic, default = "Title"
#' @param RowHeader A string containing the descriptive title for row names, default = "RowHeader"
#' @param ColumnHeader A string containing the descriptive title for column names, default = "ColumnHeader"
#' @param figureWidth A number specifying total graphic image width in number of pixels, default = 500
#' @param figureHeight A number specifying total graphic image height in number of pixels, default = 800
#' @param resolution A number between 1 and 3, default = 1.
#'      This parameter increases the image resolution by scaling the number of pixels in the length and width by an equal amount.
#'      Increasing the resolution (higher numbers) will result in a larger image which can be adjusted in any image processing software to the desired dimension.
#' @param graphicCellWidths A vector of length 2 specifying first the width of row label column second data column width
#' @param graphicCellHeight A number specifying height of graphic rows containing data
#' @param barWidth A number specifying the width of the plotted bar
#' @param yscalerow A string specifying the type of scaling for the y-axis of bar charts, if = "TRUE" the y-axis scale is chosen independently for each row, else the scale is chosen for the entire decision table, default = "FALSE"
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

#'      "WhiskerPlot"  = Summary row added, box and whisker plot summarizes data contained in each column, high/low value corresponding with best performance must be specified
#' @param visualRank A string specifying the use of color to visually show ranked relative performance, if = "TRUE" then `BestPerformanceVector` and `SummaryBestPerformance` (if summary row included) must be provided and the `barColors` argument should be set to "defaultRankColor" or provided a vector of unique colors equal in lenght to the number of columns. Default = "FALSE"
#' @param BestPerformanceVector A vector equal in length to the number of rows in Data containing "High" or "Low". Default = NULL.
#'      "High" = highest value in row will be colored to represent best performance
#'      "Low" = lowest value in row will be colored to represent best performance
#' @param SummaryBestPerformance A string containing "High" or "Low" to indicate best performance for summary row, no default.
#'      Only required if "MeanValue", "SumValue", "MedianValue", or "WhiskerPlot" options chosen. For other options higher summarized rank correspond with better performance
#' @param barColors A string specifying single color OR a vector of colors equal in length to the number of data columns, OR "defaultRankColor" which uses default colors to denote ranked performance. Default = grey columns.
#' @param IncludeCI A string specifying the inclusion of confidence levels in plot, if = "TRUE" then Data_UpperCI and Data_LowerCI must also be provided. Default = "FALSE"
#' @param Data_UpperCI A matrix containing upper confidence levels with row and column names that match Data (must me matrix even if only 1 row or column), no default.
#' @param Data_LowerCI A matrix containing lower confidence levels with row and column names that match Data (must me matrix even if only 1 row or column), no default.
#' @param IconList A vector of icons (identified by string names) to be printed right of the GraphicTitle, length may not exceed 3, default = no icons.
#'        A custom black-and-white icon may be printed by providing the filename with .png extension, or an icon supported by the package may be chosen by providing one of the following icon names:
#'        "Commercial_Fisheries_Herring_Mackerel_Lobster",  "Environmental_Considerations",  "Groundfish_Fishery",  "Groundfish_Species",  "Herring_Fishery_Option1",  "Herring_Fishery_Option2",
#'        "Lobster",  "Lobster_Fishery",  "Predator_Fisheries_Tuna_Haddock_Flatfish",  "Predator_Species_Tuna_Haddock_Flatfish",  "Primary_Production",  "Protected_Species",  "Protected_Species_and_Tourism",
#'        "Tourism",  "Tuna",  "Tuna_Fishery"
#' @param IconColor A string specifying the color of icons to be printed, only necessary if IconList provide. default = "black"
#'
#' @return Customized decision table image (.png) with optional confidence intervals on bars.
#' @export
#'
#' @examples
#' # Produce example decision table and save as DecisionTable.png in current working directory.
#'
#' # Example dataframe
#' set.seed(1)
#' data_df <- matrix(c(abs(rnorm(30,20,sd=5))), ncol = 5, nrow = 6)
#' colnames(data_df) <- c("Column 1", "Column 2", "Column 3", "Column 4", "Column 5")
#' rownames(data_df) <- c("Row 1", "Row 2", "Row 3", "Row 4", "Row 5", "Row 6")
#'
#' # Decision table with efault graphic settings, custom title and headers
#' makeDecisionTable(data = data_df, OutputFileName = "Example1", GraphicTitle = "Example decision table with default formatting", RowHeader = "Row header \n describes type \n of data in rows", ColumnHeader = "Column header describes type of data in columns")
#' # Decision table with ranked coloring scheme, darker colors correspond with better (larger) values in each row
#' makeDecisionTable(data = data_df, OutputFileName = "Example2", GraphicTitle = "Example decision table with ranked performance", RowHeader = "Row header \n describes type \n of data in rows", ColumnHeader = "Column header describes type of data in columns", BestPerformanceVector = rep("High", nrow(data_df)), barColors = "defaultRankColor", visualRank = "TRUE")
#' # Decision table with summary row showing sum values for each column, single custom color.
#' makeDecisionTable(data = data_df, OutputFileName = "Example3", GraphicTitle = "Example decision table with summary row", RowHeader = "Row header \n describes type \n of data in rows", ColumnHeader = "Column header describes type of data in columns", barColors = "cadetblue3", SummaryRowOption = "SumValue", SummaryBestPerformance = "High")
#'





############### Define makeDecisionTable Function ###############

makeDecisionTable <- function(data,
                              rownames = NULL,
                              colnames = NULL,
                              nrow = NULL,
                              ncol = NULL,
                              byrow = TRUE,
                              OutputDirectory = getwd(),
                              OutputFileName = "DecisionTable",
                              GraphicTitle = "Title",
                              RowHeader = "Row_Header",
                              ColumnHeader = "Column_Header",
                              figureWidth = 500,
                              figureHeight = 800,
                              resolution = 1,
                              graphicCellWidths = NULL,
                              graphicCellHeight = NULL,
                              barWidth = 1,
                              yscalerow = "FALSE",
                              SummaryRowOption = "Off",
                              visualRank = "FALSE",
                              BestPerformanceVector = NULL,
                              SummaryBestPerformance = NULL,
                              barColors = NULL,
                              IncludeCI = "FALSE",
                              Data_UpperCI = NULL,
                              Data_LowerCI = NULL,
                              IconList = NULL,
                              IconColor = "black"){

  # Process input data if a vector is provided
  if(is.matrix(data)==FALSE & is.data.frame(data)==FALSE & tibble::is_tibble(data)==FALSE){
    data <- matrix(data, nrow = nrow, ncol = ncol, byrow = byrow)
    # row & column names
    rownames(data) <- rownames
    colnames(data) <- colnames
  }

  # Create storage objects
  RankOrder <- matrix(NA, nrow=nrow(data), ncol=ncol(data)) # used for ranking performance (last row will remain empty if no summary row added)

  ######################################################################
  # Plot graphics
  ######################################################################
  # Create png: filename, width, height, resolution can all be adjusted
  png(filename = paste(OutputDirectory, paste(OutputFileName, ".png", sep=""), sep="/"), width = figureWidth*resolution, height = figureHeight*resolution)

  # Set up correct graphic format (number of rows/columns, layout matrix) for given data set
  graphicFormat <- NULL # Start with empty format object
  if(ncol(data) == 1){

    if(SummaryRowOption == "Off"){
      graphicFormat$graphicNrow <- 6+nrow(data)*2
      loopnums <- seq(from=11, to=11+nrow(data)*3, by=3)
    } else{
      graphicFormat$graphicNrow <- 6+nrow(data)*2 + 2 # Add 2 rows for summary row formatting
      loopnums <- seq(from=11, to=11+(nrow(data)+1)*3, by=3) # Add 2 rows for summary row formatting
    }
    graphicFormat$graphicNcol <- 5
    graphicLayout <- c(1, 2, 3, 4, 5, rep(6, ncol(data)+3), 5, 7, rep(8, ncol(data)+2), 5, 7, rep(9, ncol(data)+2), 5, 7, rep(10, ncol(data)+2), 5, rep(11, ncol(data)+3), 5)
    for(inum in loopnums[-length(loopnums)]){
      graphicLayout <- c(graphicLayout, inum+1, rep(inum+2, ncol(data)+2), 5)
      graphicLayout <- c(graphicLayout, rep(inum+3, ncol(data)+3), 5)
    }
    graphicFormat$graphicLayout <- graphicLayout

  } else if(ncol(data) == 2){

    if(SummaryRowOption == "Off"){
      graphicFormat$graphicNrow <- 6+nrow(data)*2
      loopnums <- seq(from=12, to=12+nrow(data)*4, by=4)
    } else{
      graphicFormat$graphicNrow <- 6+nrow(data)*2+2 # Add 2 rows for summary row formatting
      loopnums <- seq(from=12, to=12+(nrow(data)+1)*4, by=4) # Add 2 rows for summary row formatting
    }
    graphicFormat$graphicNcol <- 6
    graphicLayout <- c(rep(1, ncol(data)), 2, 3, 4, 5, rep(6, ncol(data)+3), 5, 7, rep(8, ncol(data)+2), 5, 7, rep(9, ncol(data)+2), 5, 7, rep(10, ncol(data)), rep(11, ncol(data)), 5, rep(12, ncol(data)+3), 5)
    for(inum in loopnums[-length(loopnums)]){
      graphicLayout <- c(graphicLayout, inum+1, rep(inum+2, ncol(data)), rep(inum+3, ncol(data)), 5)
      graphicLayout <- c(graphicLayout, rep(inum+4, ncol(data)+3), 5)
    }
    graphicFormat$graphicLayout <- graphicLayout

  } else if(ncol(data) == 3){

    if(SummaryRowOption == "Off"){
      graphicFormat$graphicNrow <- 6+nrow(data)*2
      loopnums <- seq(from=13, to=13+nrow(data)*5, by=5)
    } else{
      graphicFormat$graphicNrow <- 6+nrow(data)*2 + 2 # Add 2 rows for summary row formatting
      loopnums <- seq(from=13, to=13+(nrow(data)+1)*5, by=5) # Add 2 rows for summary row formatting
    }
    graphicFormat$graphicNcol <- 5
    graphicLayout <- c(1, 2, 3, 4, 5, rep(6, ncol(data)+1), 5, 7, rep(8, ncol(data)), 5, 7, rep(9, ncol(data)), 5, 7, seq(from=10, to=12), 5, rep(13, ncol(data)+1), 5)
    for(inum in loopnums[-length(loopnums)]){
      graphicLayout <- c(graphicLayout, seq(from=inum+1, to=inum+4), 5)
      graphicLayout <- c(graphicLayout, rep(inum+5, ncol(data)+1), 5)
    }
    graphicFormat$graphicLayout <- graphicLayout

  } else if(ncol(data) >= 4){

    if(SummaryRowOption == "Off"){
      graphicFormat$graphicNrow <- 6+nrow(data)*2
      loopnums <- seq(from=(ncol(data)+10), to=ncol(data)+10+nrow(data)*(ncol(data)+2), by=(ncol(data)+2))
    } else{
      graphicFormat$graphicNrow <- 6+nrow(data)*2+2 # Adds 2 rows for summary row formatting
      loopnums <- seq(from=(ncol(data)+10), to=ncol(data)+10+(nrow(data)+1)*(ncol(data)+2), by=(ncol(data)+2)) # Adds 2 rows for summary row formatting
    }
    graphicFormat$graphicNcol <- ncol(data)+2
    graphicLayout <- c(rep(1, (ncol(data)-2)), 2, 3, 4, 5, rep(6, ncol(data)+1), 5, 7, rep(8, ncol(data)), 5, 7, rep(9, ncol(data)), 5, 7, seq(from=10, to=ncol(data)+9), 5, rep(ncol(data)+10, ncol(data)+1), 5)
    for(inum in loopnums[-length(loopnums)]){
      graphicLayout <- c(graphicLayout, seq(from=inum+1, to=inum+ncol(data)+1), 5)
      graphicLayout <- c(graphicLayout, rep(inum+ncol(data)+2, ncol(data)+1), 5)
    }
    graphicFormat$graphicLayout <- graphicLayout

  }


  # Set widths for graphic columns
  if(is.null(graphicCellWidths) == TRUE){
    GraphicWidths <-  c(2,rep(1, (graphicFormat$graphicNcol-2)),0.25)
  } else{
    GraphicWidths <- c(graphicCellWidths[1],rep(graphicCellWidths[2], graphicFormat$graphicNcol-2),0.25)
  }

  # Set heights for graphic rows
  if(is.null(graphicCellHeight) == TRUE){
    GraphicHeights <- c(rep(c(1,0.25),3),rep(c(2,0.25), graphicFormat$graphicNrow/2-3))
  } else{
    GraphicHeights <- c(rep(c(1,0.25),3),rep(c(graphicCellHeight,0.25), graphicFormat$graphicNrow/2-3))
  }

  # Produce final GraphicFormat
  GraphicFormat <- matrix(graphicFormat$graphicLayout, nrow = graphicFormat$graphicNrow, ncol = graphicFormat$graphicNcol, byrow = TRUE)
  GridList <- NULL

  # Set bar colors
  if(is.null(barColors)==TRUE){
    barColors <- c(rep("grey47", ncol(data)))
  } else if(barColors=="defaultRankColor") {
    if(ncol(data)==1){
      barColors <- c("#238443")
    } else if(ncol(data)==2){
      barColors <- c("#addd8e", "#238443")
    } else if(ncol(data)==3){
      barColors <- c("#addd8e", "#238443", "#004529")
    } else if(ncol(data)==4){
      barColors <- c("#f7fcb9", "#addd8e", "#238443", "#004529")
    } else if(ncol(data)==5){
      barColors <- c("#f7fcb9", "#addd8e", "#41ab5d", "#238443", "#004529")
    } else if(ncol(data)==6){
      barColors <- c("#f7fcb9", "#addd8e", "#78c679", "#41ab5d", "#238443", "#004529")
    } else if(ncol(data)==7){
      barColors <- c("#f7fcb9", "#d9f0a3", "#addd8e", "#78c679", "#41ab5d", "#238443", "#004529")
    } else if(ncol(data)==8){
      barColors <- c("#f7fcb9", "#d9f0a3", "#addd8e", "#78c679", "#41ab5d", "#238443", "#006837", "#004529")
    } else if(ncol(data)==9){
      barColors <- c("#ffffe5", "#f7fcb9", "#d9f0a3", "#addd8e", "#78c679", "#41ab5d", "#238443", "#006837", "#004529")
    } else if(ncol(data)==10){
      barColors <- c("#fe9929", "#fed976", "#f7fcb9", "#d9f0a3", "#addd8e", "#78c679", "#41ab5d", "#238443", "#006837", "#004529")
    } else if(ncol(data)==11){
      barColors <- c("#ec7014", "#fe9929", "#fed976", "#f7fcb9", "#d9f0a3", "#addd8e", "#78c679", "#41ab5d", "#238443", "#006837", "#004529")
    } else if(ncol(data)==12){
      barColors <- c("#253494", "#1d91c0", "#7fcdbb", "#ffffe5", "#f7fcb9","#d9f0a3", "#addd8e", "#78c679", "#41ab5d", "#238443", "#006837", "#004529")
    } else if(ncol(data)==13){
      barColors <- c("#081d58", "#253494", "#1d91c0", "#7fcdbb", "#ffffe5", "#f7fcb9","#d9f0a3", "#addd8e", "#78c679", "#41ab5d", "#238443", "#006837", "#004529")
    } else if(ncol(data)==14){
      barColors <- c("#081d58", "#253494", "#1d91c0", "#41b6c4", "#7fcdbb", "#ffffe5", "#f7fcb9","#d9f0a3", "#addd8e", "#78c679", "#41ab5d", "#238443", "#006837", "#004529")
    } else if(ncol(data)==15){
      barColors <- c("#081d58", "#253494", "#225ea8", "#1d91c0", "#41b6c4", "#7fcdbb", "#ffffe5", "#f7fcb9", "#d9f0a3", "#addd8e", "#78c679", "#41ab5d", "#238443", "#006837", "#004529")
    } else if(ncol(data)==16){
      barColors <- c("#ffffe5", "#f7fcb9", "#d9f0a3", "#addd8e", "#78c679", "#41ab5d", "#238443", "#006837", "#004529", "#02818a", "#7fcdbb", "#41b6c4", "#1d91c0", "#225ea8", "#253494", "#081d58")
    } else if(ncol(data)==17){
      barColors <- c("#fe9929", "#fed976", "#f7fcb9", "#d9f0a3", "#addd8e", "#78c679", "#41ab5d", "#238443", "#006837", "#004529", "#02818a", "#7fcdbb", "#41b6c4", "#1d91c0", "#225ea8", "#253494", "#081d58")
    } else if(ncol(data)==18){
      barColors <- c("#ec7014", "#fe9929", "#fed976", "#f7fcb9", "#d9f0a3", "#addd8e", "#78c679", "#41ab5d", "#238443", "#006837", "#004529", "#02818a", "#7fcdbb", "#41b6c4", "#1d91c0", "#225ea8", "#253494", "#081d58")
    } else if(ncol(data)==19){
      barColors <- c("#cc4c02", "#ec7014", "#fe9929", "#fed976", "#f7fcb9", "#d9f0a3", "#addd8e", "#78c679", "#41ab5d", "#238443", "#006837", "#004529", "#02818a", "#7fcdbb", "#41b6c4", "#1d91c0", "#225ea8", "#253494", "#081d58")
    } else if(ncol(data)==20){
      barColors <- c("#993404", "#cc4c02", "#ec7014", "#fe9929", "#fed976", "#f7fcb9", "#d9f0a3", "#addd8e", "#78c679", "#41ab5d", "#238443", "#006837", "#004529", "#02818a", "#7fcdbb", "#41b6c4", "#1d91c0", "#225ea8",  "#253494", "#081d58")
    } else {
      barColors <- c(rep("grey47", ncol(data)))
    } # End of defaultRankColor
  } else if(length(barColors)==1){ # If a single color is used
    barColors <- c(rep(barColors, ncol(data)))
  } else {
    barColors <- barColors
  }


  ########## Set up title and headers for table ##########
  # GraphicTitle
  GraphicTitleGrob <- grid::textGrob(GraphicTitle, gp=grid::gpar(cex=1.5*resolution))
  GridList <- grid::gList(GridList, GraphicTitleGrob)

  # Optional printed icons to right of title, may not exceed 3 icons
  if(length(IconList) == 3){ # If there are 3 icons
    for(icon in 1:length(IconList)){
      # Pick from default icon list included in package
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
          IconImage <- raster::raster(IconImage, layer=1, values=TRUE) # Change from "SpatialPixelsDataFrame" to raster format

          # Plot icon
          PlotIcon <- rasterVis::gplot(IconImage) + geom_tile(ggplot2::aes(colour = cut(value, breaks = c(-100,0,180,Inf)), fill=cut(value, breaks = c(-100,0,180,Inf)))) + # Alter middle number to change pixels coloring
            ggplot2::scale_color_manual(name = "UniqueScale",
                               values = c("(0,180]" = IconColor,
                                          "(180,Inf]" = "white",
                                          "(-100,0]" = IconColor),breaks=NULL, na.value=IconColor) +
            ggplot2::scale_fill_manual(name = "UniqueScale",
                              values = c("(0,180]" = IconColor,
                                         "(180,Inf]" = "white",
                                         "(-100,0]" = IconColor),breaks=NULL, na.value=IconColor) +
            coord_equal() +
            theme(text = element_blank(),
                  rect = element_blank(),
                  line = element_blank())

          GridList <- grid::gList(GridList, ggplotify::as.grob(assign(paste("Icon_", icon, sep=""), PlotIcon)))

        } else { # Use custom icon
          IconGrob <- ggplotify::as.grob(ggdraw() + draw_image(IconList[icon]))
          # Plot icon
          print("Custom Icon")
          GridList <- grid::gList(GridList,IconGrob)
        }
    }
  } else if(is.null(IconList)==TRUE){ # If there are no icons provided to argument
    for(empty in 1:3){ # Fill all columns except title in first row with empty plot (Title, empty the rest of row)
      EmptySpace <- grid::textGrob(" ")
      GridList <- grid::gList(GridList, EmptySpace)
      print("Empty Plot Icon" )
    }
  } else if(length(IconList) > 3){ # If there are more than 3 icons don't print any and return a warning
    for(empty in 1:3){ # Fill all columns except title in first row with empty plot (Title, empty the rest of row)
      EmptySpace <- grid::textGrob(" ")
      GridList <- grid::gList(GridList, EmptySpace)
      print("Empty Plot Icon" )
    }
    warning("A maximum of 3 icons can be printed to the right of the title, did you provide more than 3 icons?")
  } else if(length(IconList) < 3) { # If there are less than 3 icons, this fills remaining slots in first row with empty plots if fewer icons than slots (including no icon list)
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
        IconImage <- raster::raster(IconImage, layer=1, values=TRUE) # Change from "SpatialPixelsDataFrame" to raster format

        # Plot icon
        PlotIcon <- rasterVis::gplot(IconImage) + geom_tile(ggplot2::aes(colour = cut(value, c(0,180,Inf)), fill=cut(value, c(0,180,Inf)))) +
          ggplot2::scale_color_manual(name = "UniqueScale",
                             values = c("(0,180]" = IconColor,
                                        "(180,Inf]" = "white"),breaks=NULL, na.value=IconColor) +
          ggplot2::scale_fill_manual(name = "UniqueScale",
                            values = c("(0,180]" = "black",
                                       "(180,Inf]" = "white"),breaks=NULL, na.value=IconColor) +
          coord_equal() +
          theme(text = element_blank(),
                rect = element_blank(),
                line = element_blank())

        GridList <- grid::gList(GridList, ggplotify::as.grob(assign(paste("Icon_", icon, sep=""), PlotIcon)))

      } else { # Use custom icon
        IconGrob <- ggplotify::as.grob(ggdraw() + draw_image(IconList[icon]))
        # Plot icon
        print("Custom Icon")
        GridList <- grid::gList(GridList,IconGrob)
      }
    }
    for(empty in 1:(3 - length(IconList))){
      EmptySpace <- grid::textGrob(" ")
      GridList <- grid::gList(GridList, EmptySpace)
      print("Empty Plot Icon" )
    }
  } # End of < 3 icons section

  # Plot empty space on right side of graph
  EmptySpace <- grid::textGrob(" ")
  GridList <- grid::gList(GridList, EmptySpace)

  # Plot first horizontal division
  EmptyDataFrame <- data.frame()
  HorizontalLine <- ggplot2::ggplot(EmptyDataFrame) +
    geom_hline(yintercept = 0) +
    theme(axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.x=element_blank(),
          axis.ticks.y=element_blank(),
          axis.title.y=element_blank(),
          axis.title.x=element_blank(),
          panel.background = element_blank())
  HorizontalLine <- ggplotify::as.grob(HorizontalLine) # Make ggplot a grob, could alternatively do this in the following line, but doing it here means I only do it once (not for every horizontal line)
  for(i in graphicFormat$graphicNcol){
    GridList <- grid::gList(GridList, HorizontalLine)
  }

  # RowHeader
  RowHeaderLabel <- grid::textGrob(RowHeader, gp=grid::gpar(cex=1.2*resolution))
  GridList <- grid::gList(GridList, RowHeaderLabel)

  # ColumnHeader
  ColumnHeaderLabel <- grid::textGrob(ColumnHeader, gp=grid::gpar(cex=1.2*resolution))
  GridList <- grid::gList(GridList, ColumnHeaderLabel)

  # Plot horizontal division line
  GridList <- grid::gList(GridList, HorizontalLine)

  # Plot column names
  for(Name in 1:ncol(data)){
    ColNameGrob <- grid::textGrob(colnames(data)[Name], gp=grid::gpar(cex=1*resolution))
    GridList <- grid::gList(GridList, assign(paste("Column_", Name, "_Title", sep=""), ColNameGrob))
  }


  ########## Repeating information in the table (plot data matrix) ##########
  for(irow in 1:nrow(data)){
    # Plot horizontal division line
    GridList <- grid::gList(GridList, HorizontalLine)

    # Plot RowName
    RowNameGrob <- grid::textGrob(rownames(data)[irow], gp=grid::gpar(cex=1*resolution))
    GridList <- grid::gList(GridList, assign(paste("Row_", irow, "_Title", sep=""), RowNameGrob))

    # Handle ranked performance as applicable
    if(visualRank == "TRUE"){ # Determine relative ranked performance
      # Determine rank of data and associated coloring by looping over columns in each row
      if(BestPerformanceVector[irow] == "High"){
        # Rank from high to low
        RankOrder[irow, ] <- rank(data[irow, ]) # will not be produced if a rowname does not match something in the if statement (returns Rank not found error)
      } else if(BestPerformanceVector[irow] == "Low"){
        # Rank from low to high
        RankOrder[irow, ] <- rank(-data[irow,])
      }
    } else{ # If visualRank == FALSE (default) then set ranking to match the order of columns so coloring follows column order
      RankOrder[irow,] <- seq(1:ncol(data))
    }

    # Handle confidence bounds if applicable
    if(IncludeCI == "TRUE"){
      # Make sure CI data in correct format (matrix)
      if(is.matrix(Data_LowerCI)==FALSE & is.data.frame(Data_LowerCI)==FALSE & tibble::is_tibble(Data_LowerCI)==FALSE){
        Data_LowerCI <- matrix(Data_LowerCI, ...)
      }
      if(is.matrix(Data_UpperCI)==FALSE & is.data.frame(Data_UpperCI)==FALSE & tibble::is_tibble(Data_UpperCI)==FALSE){
        Data_UpperCI <- matrix(Data_UpperCI, ...)
      }

      # Format & produce barplots
      for(icol in 1:ncol(data)){
        PlotData <- data.frame(data[irow, icol],data[irow, icol])
        colnames(PlotData) <- c("XX", "YY")
        PlotColor <- barColors[RankOrder[irow,icol]]

        Error_UpperCI <- Data_UpperCI[irow, icol]
        Error_LowerCI <- Data_LowerCI[irow, icol]
        limits <- ggplot2::aes(ymax = Error_UpperCI, ymin = Error_LowerCI)

        if(yscalerow == TRUE){ # Pick a common y-axis scale for each row independently
          Max_Y <- max(Data_UpperCI[irow,], data[irow,])
        } else { # Pick common y-axis table for the full data table
          Max_Y <- max(Data_UpperCI, data) # Pick max y-axis value either from the Data_UpperCI table or the data itself
        }

        barplot <- ggplot2::ggplot(PlotData, ggplot2::aes(x=XX, y=YY))
        PrettyBarPlot <- barplot + geom_col(fill = PlotColor) +
          geom_errorbar(limits, width = 0.25, size=0.5*resolution) +
          theme(axis.text.x=element_blank(),
                axis.text.y=element_text(size=10*resolution),
                axis.ticks.x=element_blank(),
                axis.title.y=element_blank(),
                #axis.title.x=element_blank(),
                panel.background = element_blank(),
                axis.line.x = element_line(color="black", size=0.5*resolution),
                axis.line.y = element_line(color="black", size=0.5*resolution),
                text = element_text(size=12*resolution)) +
          labs(x = paste(round(data[irow,icol], digits=2)))  + # Print value rounded to 2 digits below bar
          ylim(0, Max_Y+2)
          # scale_x_continuous(expand = c(0,0)) +
          # scale_y_continuous(expand = c(0,0), limits=c(0,Max_Y)+2) # y-axis must encompass uppermost CI provided

        # Save Barplot for formatting later
        GridList <- grid::gList(GridList, ggplotify::as.grob(assign(paste("CIBarplot_row_", irow, "_column_", icol, sep=""), PrettyBarPlot)))
      }
    } else { # Don't include CI
      for(icol in 1:ncol(data)){
        PlotData <- data.frame(data[irow, icol],data[irow, icol])
        colnames(PlotData) <- c("XX", "YY")
        PlotColor <- barColors[RankOrder[irow,icol]]

        if(yscalerow == TRUE){ # Pick a common y-axis scale for each row independently
          Max_Y <- max(data[irow,])
        } else { # Pick common y-axis table for the full data table
          Max_Y <- max(data) # Pick max y-axis value either from the Data_UpperCI table or the data itself
        }

        barplot <- ggplot2::ggplot(PlotData, ggplot2::aes(x=XX, y=YY))
        PrettyBarPlot <- barplot + geom_col(fill = PlotColor) +
          theme(axis.text.x=element_blank(),
                axis.text.y=element_text(size=10*resolution),
                axis.ticks.x=element_blank(),
                axis.title.y=element_blank(),
                #axis.title.x=element_blank(),
                panel.background = element_blank(),
                axis.line.x = element_line(color="black", size=0.5*resolution),
                axis.line.y = element_line(color="black", size=0.5*resolution),
                text = element_text(size=12*resolution)) +
          labs(x = paste(round(data[irow,icol], digits=2))) + # Print value rounded to 2 digits below bar
          scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0), limits=c(0,Max_Y+2)) # y-axis must encompass uppermost data provided

        # Save Barplot for formatting later
        GridList <- grid::gList(GridList, ggplotify::as.grob(assign(paste("Barplot_row_", irow, "_column_", icol, sep=""), PrettyBarPlot)))
      }
    } # End of No CI section
  } # End of repeating plot section


  ########## Plot Summary Row as specified ##########
  if(SummaryRowOption == "Off"){ ########## SummaryRowOption = "Off" #################################################################
    print("You are done =)")
  } else if(SummaryRowOption == "MeanRank"){ ########## SummaryRowOption = "MeanRank" ##################################################
    # Calculate summary row data as specified
    SummaryRowData <- colMeans(RankOrder)

    # Rank summary from high to low for coloring. Highest rank always correspond with best performance (may represent high or low values) so "High" (best) coloring given to highest rank
    SummaryRankOrder <- rank(SummaryRowData)

    # Plot horizontal division line
    GridList <- grid::gList(GridList, HorizontalLine)

    # Plot RowName
    RowNameGrob <- grid::textGrob("Summary", gp=grid::gpar(cex=1.2*resolution))
    GridList <- grid::gList(GridList, assign("SummaryRowTitle", RowNameGrob))

    # Plot bars
    for(icol in 1:length(SummaryRowData)){
      PlotData <- data.frame(SummaryRowData[icol], SummaryRowData[icol])
      colnames(PlotData) <- c("XX", "YY")
      PlotColor <- barColors[SummaryRankOrder[icol]]

      barplot <- ggplot2::ggplot(data=PlotData, ggplot2::aes(x=XX, y=YY))
      PrettyBarPlot <- barplot + geom_col(fill = PlotColor) +
        theme(axis.text.x=element_blank(),
              axis.text.y=element_text(size=10*resolution),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              #axis.title.x=element_blank(),
              panel.background = element_blank(),
              axis.line.x = element_line(color="black", size=0.5*resolution),
              axis.line.y = element_line(color="black", size=0.5*resolution),
              text=element_text(size=12*resolution)) +
        labs(x = paste(round(SummaryRowData[icol], digits=2))) + # Print value rounded to 2 digits below bar
        scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0), limits=c(0,max(SummaryRowData)+1))

      # Save Barplot for formatting later
      GridList <- grid::gList(GridList, ggplotify::as.grob(assign(paste("SummaryBarplot_row_", nrow(data)+1, "_column_", icol, sep=""), PrettyBarPlot)))
    }
  } else if(SummaryRowOption == "SumRank"){ ########## SummaryRowOption = "SumRank" ##################################################
    # Calculate summary row data as specified
    SummaryRowData <- colSums(RankOrder)

    # Rank summary from high to low for coloring. Highest rank always correspond with best performance (may represent high or low values) so "High" (best) coloring given to highest rank
    SummaryRankOrder <- rank(SummaryRowData)

    # Plot horizontal division line
    GridList <- grid::gList(GridList, HorizontalLine)

    # Plot RowName
    RowNameGrob <- grid::textGrob("Summary", gp=grid::gpar(cex=1.2*resolution))
    GridList <- grid::gList(GridList, assign("SummaryRowTitle", RowNameGrob))

    # Plot bars
    for(icol in 1:length(SummaryRowData)){
      PlotData <- data.frame(SummaryRowData[icol], SummaryRowData[icol])
      colnames(PlotData) <- c("XX", "YY")
      PlotColor <- barColors[SummaryRankOrder[icol]]

      barplot <- ggplot2::ggplot(PlotData, ggplot2::aes(x=XX, y=YY))
      PrettyBarPlot <- barplot + geom_col(fill = PlotColor) +
        theme(axis.text.x=element_blank(),
              axis.text.y=element_text(size=10*resolution),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              #axis.title.x=element_blank(),
              panel.background = element_blank(),
              axis.line.x = element_line(color="black", size=0.5*resolution),
              axis.line.y = element_line(color="black", size=0.5*resolution),
              text = element_text(size=12*resolution)) +
        labs(x = paste(round(SummaryRowData[icol], digits=2))) + # Print value rounded to 2 digits below bar
        scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0),limits=c(0,((graphicFormat$graphicNcol-2)*((graphicFormat$graphicNrow-8)/2)+2))) # Scales against highest possible rank (sum best rank across all rows)

      # Save Barplot for formatting later
      GridList <- grid::gList(GridList, ggplotify::as.grob(assign(paste("SummaryBarplot_row_", nrow(data)+1, "_column_", icol, sep=""), PrettyBarPlot)))
    }
  } else if(SummaryRowOption == "MedianRank"){ ########## SummaryRowOption = "MedianRank" ##################################################
    # Calculate summary row data as specified
    SummaryRowData <- apply(RankOrder, 2, median)

    # Rank summary from high to low for coloring. Highest rank always correspond with best performance (may represent high or low values) so "High" (best) coloring given to highest rank
    SummaryRankOrder <- rank(SummaryRowData)

    # Plot horizontal division line
    GridList <- grid::gList(GridList, HorizontalLine)

    # Plot RowName
    RowNameGrob <- grid::textGrob("Summary", gp=grid::gpar(cex=1.2*resolution))
    GridList <- grid::gList(GridList, assign("SummaryRowTitle", RowNameGrob))

    # Plot bars
    for(icol in 1:length(SummaryRowData)){
      PlotData <- data.frame(SummaryRowData[icol], SummaryRowData[icol])
      colnames(PlotData) <- c("XX", "YY")
      PlotColor <- barColors[SummaryRankOrder[icol]]

      barplot <- ggplot2::ggplot(PlotData, ggplot2::aes(x=XX, y=YY))
      PrettyBarPlot <- barplot + geom_col(fill = PlotColor) +
        theme(axis.text.x=element_blank(),
              axis.text.y=element_text(size=10*resolution),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              #axis.title.x=element_blank(),
              panel.background = element_blank(),
              axis.line.x = element_line(color="black", size=0.5*resolution),
              axis.line.y = element_line(color="black", size=0.5*resolution),
              text = element_text(size=12*resolution)) +
        labs(x = paste(round(SummaryRowData[icol], digits=2))) + # Print value rounded to 2 digits below bar
        scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0), limits=c(0,max(SummaryRowData)+1))

      # Save Barplot for formatting later
      GridList <- grid::gList(GridList, ggplotify::as.grob(assign(paste("SummaryBarplot_row_", nrow(data)+1, "_column_", icol, sep=""), PrettyBarPlot)))
    }
  } else if(SummaryRowOption == "MeanValue"){ ########## SummaryRowOption = "MeanValue" ##################################################
    # Calculate summary row data as specified
    SummaryRowData <-  colMeans(data)

    # Rank summary from high to low for coloring. Default = Highest rank correspond with best performance
    if(SummaryBestPerformance == "High"){
      # Rank from high to low
      SummaryRankOrder <- rank(SummaryRowData) # will not be produced if a rowname does not match something in the if statement (returns Rank not found error)
    } else if(SummaryBestPerformance == "Low"){
      # Rank from low to high
      SummaryRankOrder <- rank(-SummaryRowData)
    }

    # Plot horizontal division line
    GridList <- grid::gList(GridList, HorizontalLine)

    # Plot RowName
    RowNameGrob <- grid::textGrob("Summary", gp=grid::gpar(cex=1.2*resolution))
    GridList <- grid::gList(GridList, assign("SummaryRowTitle", RowNameGrob))

    # Plot bars
    for(icol in 1:length(SummaryRowData)){
      PlotData <- data.frame(SummaryRowData[icol], SummaryRowData[icol])
      colnames(PlotData) <- c("XX", "YY")
      PlotColor <- barColors[SummaryRankOrder[icol]]

      barplot <- ggplot2::ggplot(PlotData, ggplot2::aes(x=XX, y=YY))
      PrettyBarPlot <- barplot + geom_col(fill = PlotColor) +
        theme(axis.text.x=element_blank(),
              axis.text.y=element_text(size=10*resolution),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              #axis.title.x=element_blank(),
              panel.background = element_blank(),
              axis.line.x = element_line(color="black", size=0.5*resolution),
              axis.line.y = element_line(color="black", size=0.5*resolution),
              text = element_text(size=12*resolution)) +
        labs(x = paste(round(SummaryRowData[icol], digits=2))) + # Print value rounded to 2 digits below bar
        scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0), limits=c(0,max(SummaryRowData)+1))

      # Save Barplot for formatting later
      GridList <- grid::gList(GridList, ggplotify::as.grob(assign(paste("SummaryBarplot_row_", nrow(data)+1, "_column_", icol, sep=""), PrettyBarPlot)))
    }
  } else if(SummaryRowOption == "SumValue"){ ########## SummaryRowOption = "SumValue" ##################################################
    # Calculate summary row data as specified
    SummaryRowData <- colSums(data)

    # Rank summary from high to low for coloring. Default = Highest rank correspond with best performance
    if(SummaryBestPerformance == "High"){
      # Rank from high to low
      SummaryRankOrder <- rank(SummaryRowData) # will not be produced if a rowname does not match something in the if statement (returns Rank not found error)
    } else if(SummaryBestPerformance == "Low"){
      # Rank from low to high
      SummaryRankOrder <- rank(-SummaryRowData)
    }

    # Plot horizontal division line
    GridList <- grid::gList(GridList, HorizontalLine)

    # Plot RowName
    RowNameGrob <- grid::textGrob("Summary", gp=grid::gpar(cex=1.2*resolution))
    GridList <- grid::gList(GridList, assign("SummaryRowTitle", RowNameGrob))

    # Plot bars
    for(icol in 1:length(SummaryRowData)){
      PlotData <- data.frame(SummaryRowData[icol], SummaryRowData[icol])
      colnames(PlotData) <- c("XX", "YY")
      PlotColor <- barColors[SummaryRankOrder[icol]]

      barplot <- ggplot2::ggplot(PlotData, ggplot2::aes(x=XX, y=YY))
      PrettyBarPlot <- barplot + geom_col(fill = PlotColor) +
        theme(axis.text.x=element_blank(),
              axis.text.y=element_text(size=10*resolution),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              #axis.title.x=element_blank(),
              panel.background = element_blank(),
              axis.line.x = element_line(color="black", size=0.5*resolution),
              axis.line.y = element_line(color="black", size=0.5*resolution),
              text = element_text(size=12*resolution)) +
        labs(x = paste(round(SummaryRowData[icol], digits=2))) + # Print value rounded to 2 digits below bar
        scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0), limits=c(0,max(SummaryRowData)+2))

      # Save Barplot for formatting later
      GridList <- grid::gList(GridList, ggplotify::as.grob(assign(paste("SummaryBarplot_row_", nrow(data)+1, "_column_", icol, sep=""), PrettyBarPlot)))
    }
  } else if(SummaryRowOption == "MedianValue"){ ########## SummaryRowOption = "MedianValue" ##################################################
    # Calculate summary row data as specified
    SummaryRowData <- apply(data, 2, median)

    # Rank summary from high to low for coloring. Default = Highest rank correspond with best performance
    if(SummaryBestPerformance == "High"){
      # Rank from high to low
      SummaryRankOrder <- rank(SummaryRowData) # will not be produced if a rowname does not match something in the if statement (returns Rank not found error)
    } else if(SummaryBestPerformance == "Low"){
      # Rank from low to high
      SummaryRankOrder <- rank(-SummaryRowData)
    }

    # Plot horizontal division line
    GridList <- grid::gList(GridList, HorizontalLine)

    # Plot RowName
    RowNameGrob <- grid::textGrob("Summary", gp=grid::gpar(cex=1.2*resolution))
    GridList <- grid::gList(GridList, assign("SummaryRowTitle", RowNameGrob))

    # Plot bars
    for(icol in 1:length(SummaryRowData)){
      PlotData <- data.frame(SummaryRowData[icol], SummaryRowData[icol])
      colnames(PlotData) <- c("XX", "YY")
      PlotColor <- barColors[SummaryRankOrder[icol]]

      barplot <- ggplot2::ggplot(PlotData, ggplot2::aes(x=XX, y=YY))
      PrettyBarPlot <- barplot + geom_col(fill = PlotColor) +
        theme(axis.text.x=element_blank(),
              axis.text.y=element_text(size=10*resolution),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              #axis.title.x=element_blank(),
              panel.background = element_blank(),
              axis.line.x = element_line(color="black", size=0.5*resolution),
              axis.line.y = element_line(color="black", size=0.5*resolution),
              text = element_text(size=12*resolution)) +
        labs(x = paste(round(SummaryRowData[icol], digits=2))) + # Print value rounded to 2 digits below bar
        scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0), limits=c(0,max(SummaryRowData)+1))


      # Save Barplot for formatting later
      GridList <- grid::gList(GridList, ggplotify::as.grob(assign(paste("SummaryBarplot_row_", nrow(data)+1, "_column_", icol, sep=""), PrettyBarPlot)))
    }
  } else if(SummaryRowOption == "WhiskerPlot"){ ########## SummaryRowOption = "WhiskerPlot" ##################################################
    # Calculate summary row data to be used only for coloring this option
    SummaryRowColorData <- apply(data, 2, median)

    # Rank summary from high to low for coloring. Default = Highest rank correspond with best performance
    if(SummaryBestPerformance == "High"){
      # Rank from high to low
      SummaryRankOrder <- rank(SummaryRowColorData) # will not be produced if a rowname does not match something in the if statement (returns Rank not found error)
    } else if(SummaryBestPerformance == "Low"){
      # Rank from low to high
      SummaryRankOrder <- rank(-SummaryRowColorData)
    }

    # Plot horizontal division line
    GridList <- grid::gList(GridList, HorizontalLine)

    # Plot RowName
    RowNameGrob <- grid::textGrob("Summary", gp=grid::gpar(cex=1.2*resolution))
    GridList <- grid::gList(GridList, assign("SummaryRowTitle", RowNameGrob))

    # Plot whisker plots with ranked coloring based on median
    for(icol in 1:ncol(data)){
      # PlotData <- data.frame(SummaryRowData[icol], SummaryRowData[icol])
      # colnames(PlotData) <- c("XX", "YY")

      PlotColor <- barColors[SummaryRankOrder[icol]]

      PrettyWhiskerPlot <- ggplot2::ggplot(as.data.frame(data), ggplot2::aes(y=data[,icol])) +
        geom_boxplot(color="black", fill=PlotColor) +
        theme(axis.text.x=element_blank(),
              axis.text.y=element_text(size=10*resolution),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              #axis.title.x=element_blank(),
              panel.background = element_blank(),
              axis.line.x = element_line(color="black", size=0.5*resolution),
              axis.line.y = element_line(color="black", size=0.5*resolution),
              text = element_text(size=12*resolution),
              legend.position = "none") +
        labs(x = paste(round(SummaryRowColorData[icol], digits=2))) + # Print median value rounded to 2 digits below bar
        scale_x_continuous(expand = c(0,0)) +
        scale_y_continuous(expand = c(0,0), limits=c(min(data)-5,max(data)+5))


      # Save Barplot for formatting later
      GridList <- grid::gList(GridList, ggplotify::as.grob(assign(paste("SummaryWhiskerPlot_row_", nrow(data)+1, "_column_", icol, sep=""), PrettyWhiskerPlot)))
    }
  }

  # Plot last horizontal division
  GridList <- grid::gList(GridList, HorizontalLine)

  ########## Format #########
  #GridList <- grid::gList(GridList) # Formally make everything in the GridList part of a grob list (if not done already)
  gridExtra::grid.arrange(grobs = GridList, layout_matrix = matrix(graphicFormat$graphicLayout, nrow = graphicFormat$graphicNrow, ncol = graphicFormat$graphicNcol, byrow = TRUE), widths = GraphicWidths, heights = GraphicHeights, nrow = graphicFormat$graphicNrow, ncol = graphicFormat$graphicNcol, byrow = TRUE)
  #gridExtra::grid.arrange(grobs = GridList, layout_matrix = graphicFormat$graphicLayout, widths = GraphicWidths, heights = GraphicHeights,  byrow = TRUE)

  dev.off()
}



