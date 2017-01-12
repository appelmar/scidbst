

#' Connect to a SciDB database from R and GDAL
#' 
#' Calls \code{\link{scidbconnect}} and sets aditional environment variables to connect to the database from GDAL.
#'
#' @rdname scidbstconnect
#' @aliases scidbstconnect
#' @param host see \code{\link{scidbconnect}}
#' @param port see \code{\link{scidbconnect}}
#' @param username see \code{\link{scidbconnect}}
#' @param password A see \code{\link{scidbconnect}}
#' @param auth_type see \code{\link{scidbconnect}}
#' @param protocol see \code{\link{scidbconnect}}
#'
#' @seealso \code{\link{scidbconnect}}
#' @export
scidbstconnect <- function (host = options("scidb.default_shim_host")[[1]], port = options("scidb.default_shim_port")[[1]],  username, password, auth_type = c("scidb", "digest"), protocol = c("http", "https")) 
{
  scidbconnect(host, port, username, password, auth_type, protocol)
  suppressMessages(require(gdalUtils))
  Sys.setenv(SCIDB4GDAL_HOST=paste(protocol, "://",SCIDB_HOST, sep=""), 
             SCIDB4GDAL_PORT=port, 
             SCIDB4GDAL_USER=username,
             SCIDB4GDAL_PASSWD=password)
  gdal_chooseInstallation()
  invisible()
}



