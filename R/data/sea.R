#' Sea Surface elevation dataset
#'
#' @docType data
#'
#' @usage data(sea)
#'
#' @format A time signal in a \code{data.frame} with two columns, c1: time [s], c2: surface elevation [m].
#' The length of signal is 9524 and the sampling rate is 4.0 Hz.
#'
#' @keywords datasets
#'
#' @examples
#' ts <- data(sea)
#' tp <- ts2tp(ts)   # Extract turning points of time signal.
#' plot(ts[,1],ts[,2], type="l", col="blue", main="Turning points",xlab="Time [s]",ylab="Sea surface elevation [m]")
#' lines(tp[,1],tp[,2], type="b", col="black")
"sea"
