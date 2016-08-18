# function to calculate the dimension index by a given time string
# x: scidbst object
# time: POSIXlt or string in POSIXlt format
# returns temporal index
.calcTDimIndex = function (x, time) {
  if (is.character(time)) {
    time = as.POSIXlt(.getDateTime(time,x@tUnit))
  } else {
    #is this called?
    time = as.POSIXlt(time) #assuming valid POSIX string
  }

  if (time >= x@tExtent$min && time <= x@tExtent$max) {
    t0 = x@startTime
    dt = x@tResolution
    unit = x@tUnit
    index  = floor(as.numeric(difftime(time,t0,unit))/dt)

    return(index)
  } else {
    stop("time statement is out of bounds")
  }
}

# Tries to match the temporal resolution string against some regexpressions in order to find
# the correct POSIX time resolution for functions like 'difftime'
#
# res: temporal resolution string like P16D
.findTUnit = function(res) {
  days = "P(\\d)+D"
  months = "P(\\d)+M"
  years = "P(\\d)+Y"
  weeks = "P(\\d)+W"
  hours = "P(\\d)+h"
  minutes = "P(\\d)+m"
  seconds = "P(\\d)+s"

  if (grepl(days,res)) {
    return("days")
  }
  if (grepl(months,res)) {
    return("months")
  }
  if (grepl(years,res)) {
    return("years")
  }
  if (grepl(weeks,res)) {
    return("weeks")
  }
  if (grepl(hours,res)) {
    return("hours")
  }
  if (grepl(minutes,res)) {
    return("mins")
  }
  if (grepl(seconds,res)) {
    return("secs")
  }
}


# transforms a string by a given temporal unit into a valid POSIX string
# currently limited to days... TODO extend
.getDateTime = function (str, unit) {
  if (unit == "days") {
    tmp = strptime(str, "%Y-%m-%d") #day in month of year
    if (is.na(tmp)) {
      tmp = strptime(str, "%Y-%j") #day of year
    }
    return(tmp)
  }
  stop("Cannot extract start time of the time series")
}

#checks if all entries in a matrix are NA
.isMatrixEmpty = function (m) {
  return(all(is.na(m)) == 1 )
}

# creates a affine transformation matrix from a string that is received for example as part of the response of 'eo_getsrs'
.createAffineTransformation = function(srs) {
  .res_matrix = matrix(ncol=3,nrow=2)

  .vec = as.numeric(as.matrix(as.data.frame(strsplit(strsplit(srs$A,"\\s")[[1]],"="))[2,]))
  .res_matrix[,1] = .vec[1:2]
  .res_matrix[,2] = .vec[c(3,5)]
  .res_matrix[,3] = .vec[c(6,4)]

  return(.res_matrix)
}

# Transforms coordinates given in dimension indices to worldly coordinates
# trans: affine transformation matrix
# x: x coordinate (West-East)
# y: y coordinate (North-South)
.transformToWorld = function(trans,x,y) {
  return(trans %*% c(1,x,y))
}


# Copies metadata from a scidbst class to a scidbst class
.cpMetadata = function(from,to) {
  if (class(from) == "scidbst" && class(to) == "scidbst") {
    to@extent = from@extent
    crs(to) = crs(from)

    to@affine = from@affine

    # to@data@names = scidb_attributes(to)
    # to@data@nlayers = length(to@data@names)
    # to@data@fromdisk = TRUE

    to@title = from@title
    to@spatial_dims = from@spatial_dims
    to@temporal_dim = from@temporal_dim
    to@startTime = from@startTime
    to@tExtent = from@tExtent
    to@tResolution = from@tResolution
    to@tUnit = from@tUnit
    to@isSpatial = from@isSpatial
    to@isTemporal = from@isTemporal
    to@sref = from@sref
    to@tref = from@tref

    # if (inMemory(from)) {
    #   to@data@inmemory = FALSE
    # }

    return(to)
  }
}

# Calculates dimension indices from spatial real world coordinates given by an extent
# object: scidbst object
# extent: extent object
#
# returns extent object
.calculateDimIndices = function(object, extent) {
  ll = c(xmin(extent),ymin(extent))
  ur = c(xmax(extent),ymax(extent))

  origin = object@affine[,1]
  sub = object@affine[,2:3]

  img1 = (solve(sub) %*% (ll - origin))
  img2 = (solve(sub) %*% (ur - origin))

  indices = extent(c(range(img1[1],img2[1]),range(img1[2],img2[2])))
  xmin(indices) = floor(xmin(indices))
  ymin(indices) = floor(ymin(indices))
  xmax(indices) = ceiling(xmax(indices))
  ymax(indices) = ceiling(ymax(indices))
  return(indices)
}

# Returns the lengths of each individual dimension
.getLengths = function(obj) {
  dimnames = dimensions(obj)
  dimbounds = scidb_coordinate_bounds(obj)
  v = as.numeric(dimbounds$length)
  names(v) = dimnames

  return(v)
}

# function to create a scidb array from a scidbst array, this approach copies the environment content (object promises)
# rather than querying the scidb instance anew for the same information
# x: scidbst object
#
# returns scidb object
.toScidb = function(x) {
  res = scidb("")
  res@name = x@name
  res@gc = x@gc
  res@meta = suppressWarnings(x@meta)
  return(res)
}

# Returns the reference period of a scidbst object, e.g. P1D, P16D or P1M
# x: scidbst object
.getRefPeriod = function(x) {
  m = matrix(cbind(c("P(\\d)+D","P(\\d)+M","P(\\d)+Y","P(\\d)+W","P(\\d)+h","P(\\d)+m","P(\\d)+s"),
                   c("days","months","years","weeks","hours","mins","secs"),
                   c("D","M","Y","W","h","m","s")),ncol=3)
  colnames(m)=c("regexp","tunit","abbrev")

  out = paste("P",x@tResolution,m[m[,"tunit"]==x@tUnit,"abbrev"],sep="")
  return(out)
}

# Calculates POSIX time from an temporal index
# x: scidbst object
# n: (numeric) the temporal index
.calculatePOSIXfromIndex = function(x,n) {
  baseTime = 0

  if (x@tUnit == "weeks") {
    baseTime = 7*24*60*60
  } else if (x@tUnit == "days") {
    baseTime = 24*60*60
  } else if (x@tUnit == "hours") {
    baseTime = 60 * 60
  } else if (x@tUnit == "mins") {
    baseTime = 60
  } else if (x@tUnit == "secs") {
    baseTime = 1
  } else {
    stop("currently no other temporal unit supported")
  }

  val = as.POSIXlt(as.character(x@startTime + n * x@tResolution * baseTime))
  return(val)
}

#' Lists all scidb arrays with a dimension reference
#'
#' This function will list all the SciDB arrays that have a special reference on one or more dimensions. It will also list the type of
#' array: 's' for spatial, 't' for temporal, st for the combination of both
#'
#' @return data.frame with columns "name" and "setting"
#'
#' @seealso \code{\link{scidb::scidbls}}
#' @export
scidbst.ls = function() {
  result=iquery("eo_arrays()",return=TRUE)
  result = result[,-which("i"==names(result))]
  names(result) = c("name","type")
  return(result)
}