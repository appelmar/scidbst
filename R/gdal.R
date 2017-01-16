


.find_gdal <- function(require.python=FALSE) {
  stopifnot(require(gdalUtils))

  n = length(getOption("gdalUtils_gdalPath"))
  if (n == 0) {
    gdal_chooseInstallation()
    getOption("gdalUtils_gdalPath")
    n = length(getOption("gdalUtils_gdalPath"))
    if (n == 0) {
      stop("could not find GDAL")
    }
  }
  
  supports_scidb = unlist(lapply(getOption("gdalUtils_gdalPath"), function(x) {
    any(c("SciDB-raster-", "SciDB") %in% x$drivers$format_code)
  }))
  
  has_python = unlist(lapply(getOption("gdalUtils_gdalPath"), function(x) {
    length(x$python_utilities) > 0
  }))
  

  valid = which(supports_scidb & (!require.python | has_python))
  if (length(valid) == 0) {
    stop("could not find a suitable GDAL installation")
  }
  
  # return the most recent version
  v = unlist(lapply(getOption("gdalUtils_gdalPath"), function(x) {
     x$version
  }))
  
  mostrecent = which.max(sapply(1:length(v), function(i) {
    x = as.numeric(unlist(strsplit(v[i],"\\.")))
    x[1] + x[2] / 100 + x[3]/10000
  }))
  
  return(getOption("gdalUtils_gdalPath")[[mostrecent]]$path)
}





#' Create a PNG image and TMS from a spatial SciDB array
#' 
#' This function downloads a spatial SciDB array as a PNG file, selects one or three bands, and stretches
#' attribute values to the range from 0 to 255. Optionally, a TMS is created for fast visualization in web maps.
#' If array is a query, it is executed and stored as a temporary array and deleted after successful download.
#' @param array a spatial scidbst object
#' @param TMS logical, defining whether a tile map service (TMS) will be generated. The TMS will be stored in the subfolder TMS of the working directory
#' @param bands integer band indexes to be visualized. Can be either NULL, a single integer or three integer values. If NULL, only the first band will be 
#' downloaded and the PNG image will be a grayscale image. A single integer defines one specific band to download as a grayscale image whereas three indexes represent channels of RGB images. Notice that this argument can also reorder bands mapping to RGB.
#' @param min attribute value(s) that are mapped to 0, either NULL, a single number or individual numbers for all bands. If NULL, minimums value of the source bands are used.
#' @param max attribute value(s) that are mapped to 255, either NULL, a single number or individual numbers for all bands. If NULL, maximum values of the source bands are used.
#' @param layername name of the created layer, by default the original array name. 
#' @param grayscale.LUT lookup table defining color interpretation of one-bands images, must be provided as a data.frame with columns value, R, G, B, and optional A. 
#' @param rm.scidb logical, whether or not the SciDB array shall be removed

#' @return a list with the output file and directory of the TMS if requested
#' 
#' @examples
#' \dontrun{
#' LUT = list()
#' LUT$value = seq(0,4000, length.out=100)
#' cols = col2rgb(rainbow(100))
#' LUT$R = cols["red",]
#' LUT$G = cols["green",]
#' LUT$B = cols["blue",]
#' as_PNG_layer(scidbst("SRTM"), grayscale.LUT=as.data.frame(LUT))
#' }
#' 
#' @export

as_PNG_layer <- function (array, TMS = TRUE, bands = NULL, min = NULL, max = NULL, layername = NULL, grayscale.LUT = NULL, rm.scidb=FALSE) 
{

  if (! "scidbst" %in% class(array)) {
    stop("array is not a scidbst object")
  }
  
  isNamedArray <- function(a) {
    return(length(grep(pattern="[\\(\\)]",x=a@proxy@name)) == 0)
  }
  
  if(!array@isSpatial) {
    stop("array has no spatial reference")
  }
  if(array@isTemporal) {
    stop("visualization of multitemporal array is currently not supported")
  }
  
  if (xor(is.null(min), is.null(max))) {
    stop("either both or none of min and max must be specified")
  }
  
  if (is.null(bands)) {
    bands = 1 # by default, visualize first band
  }
  
  if (!length(bands) %in% c(1,3)) {
    stop("either one or three bands must be specified")
  }
  
  if (!all( bands %in% 1:length(scidb_attributes(array@proxy)))) {
    stop("invalid band indexes")
  }
  
  if (!is.null(min)) {
    if (length(min) == 1) {
      min = rep(min[1], length(bands))
    }
    else {
      if (length(min) != length(bands)) {
        stop("invalid number of elements in min, provide either one or three")
      }
    }
  }
  if (!is.null(max)) {
    if (length(max) == 1) {
      max = rep(max[1], length(bands))
    }
    else {
      if (length(max) !=  length(bands))  {
        stop("invalid number of elements in max, provide either one or three")
      }
    }
  }
  
  
  
  if(is.null(layername)) {
    if (!isNamedArray(array)) {
      layername = basename(tempfile(pattern = "scidbst_temp_"))
    }
    else {
      layername = array@title
    }
  }
  
  # if array reference is a query, run this query and store result as temporary array
  if (!isNamedArray(array)) {
    warning("since the provided array is a (nested) query, argument rm.scidb will be set to TRUE")
    rm.scidb = TRUE
    array = scidbsteval(array,name=layername,temp=TRUE)
  }
  
  
  out = list()
  
  # 1. download array as PNG and scale attribute values to 0 255
  OUTFILE = paste(layername,".png",sep="")
  gdalpath = .find_gdal(require.python=TRUE) 
  downloaded = FALSE
  
  
  if (!is.null(grayscale.LUT))
  {
    if (length(bands) != 1) {
      warning("more than one band selected, ignoring grayscale.LUT argument.")
    }
    else if (!is.data.frame(grayscale.LUT)) {
      warning("grayscale.LUT must be a data.frame with 4 columns, ignoring grayscale.LUT argument.")
    }
    else if (!ncol(grayscale.LUT) %in% c(4,5)) {
      warning("grayscale.LUT must be a data.frame with 4 or 5 columns, ignoring grayscale.LUT argument.")
    }
    else if (!all(c("value", "R", "G", "B") %in% colnames(grayscale.LUT))) {
      warning("grayscale.LUT have columns value, R,G,B, ignoring grayscale.LUT argument.")
    }
    else {
      # generate LUT file
      LUT.file = tempfile()
      alpha = ("A" %in% colnames(grayscale.LUT))
      cat(paste(sapply(1:nrow(grayscale.LUT), function(i) {
        paste(grayscale.LUT$value[i], round(grayscale.LUT$R[i]), round(grayscale.LUT$G[i]), round(grayscale.LUT$B[i]),  ifelse(alpha, round(grayscale.LUT$A[i]), ""))
      }),collapse="\n"), file=LUT.file)
      
      cmd_args = "color-relief"
      cmd_args = c(cmd_args, paste("SCIDB:array=", array@title,  sep = ""))
      cmd_args = c(cmd_args, LUT.file)
      cmd_args = c(cmd_args, OUTFILE)
      cmd_args = c(cmd_args,paste("-b", bands))
      if(alpha) cmd_args = c(cmd_args, "-alpha")
      cmd_args = c(cmd_args, "-of PNG")
      system2(command = file.path(gdalpath, "gdaldem"), args = as.character(cmd_args))
      downloaded = TRUE
    }
  }
  
  
  if (!downloaded) {
    
    cmd_args = c(
      str_ot = "-ot Byte",
      str_of = "-of PNG",
      str_bands = paste("-b", bands))
    
    
    if(is.null(min)) {
      cmd_args = c(cmd_args, str_scale = paste("-scale_", 1:length(bands), sep="")) 
    }
    if (!is.null(min)) {
      cmd_args = c(cmd_args, str_scale = paste(paste("-scale_", 1:length(bands), sep=""), paste(min, " ", max, " ", 0, " ", 255, sep="")))
    }
    
    
    cmd_args = c(cmd_args, paste("SCIDB:array=", array@title, sep=""))
    cmd_args = c(cmd_args, OUTFILE) 
    
    # run gdal_translate
    system2(command = file.path(gdalpath, "gdal_translate"), args = as.character(cmd_args))
    downloaded = TRUE
  }
  
  
  
  out$image = NULL
  if (file.exists(file.path(getwd(), OUTFILE))) 
    out$image = file.path(getwd(), OUTFILE)
  

  
  
  # 2. create a tile map service
  if (TMS) {
    OUTDIR = file.path("TMS", layername)
    if (!dir.exists(OUTDIR)) dir.create(OUTDIR, recursive = TRUE)
    system2(command = file.path(gdalpath,"gdal2tiles.py"), args =  c(OUTFILE, OUTDIR))
    
    out$TMS = NULL
    if(dir.exists(file.path(getwd(),OUTDIR)))  out$TMS = file.path(getwd(),OUTDIR)
  }
  
  
  
  # 3. remove temporary array
  if (rm.scidb) {
    scidbremove(layername, force = TRUE)
  }
  
  
  
  return(out)
}
