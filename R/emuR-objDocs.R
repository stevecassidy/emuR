##' Segment list
##' 
##' A segment list is the result of emu.query() or read.emusegs().
##' 
##' 
##' @aliases segmentlist emusegs
##' @format multi-columned matrix one row per segment \itemize{ \itemfirst
##' columnlabel \itemsecond columnsegment onset time \itemthird columnsegment
##' offset time \itemfourth columnutterance name }
##' @seealso \code{\link{emu.query}}, \code{\link{demo.vowels}}
##' @keywords classes
##' @name segmentlist
##' @examples
##' 
##'    data(demo.vowels)
##'    
##'    #demo.vowels is a segment list
##'    demo.vowels
##' 
NULL





##' Start and end times for EMU segment lists and trackdata objects
##' 
##' Obtain start and end times for EMU segment lists and trackdata objects
##' 
##' The function returns the start and/or end times of either a segment list or
##' a trackdata object. The former refers to the boundary times of segments,
##' the latter the start and end times at which the tracks from segments occur.
##' start.emusegs and end.emusegs give exactly the same output as start and end
##' respectively.
##' 
##' @aliases start.emusegs end.emusegs start.trackdata end.trackdata
##' @param x a segment list or a trackdata object
##' @param ...  due to the generic only
##' @return A vector of times.
##' @author Jonathan Harrington
##' @seealso \code{\link{tracktimes}}
##' @keywords utilities
##' @name start.emusegs
##' @examples
##' 
##' # start time of a segment list
##' start(polhom)
##' # duration of a segment list
##' end(polhom) - start(polhom)
##' # duration from start time of segment list
##' # and start time of parallel EPG trackdata
##' start(polhom) - start(polhom.epg)
##' 
##' 
NULL