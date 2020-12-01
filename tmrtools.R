# tmrtools.R

# functions from tmrtools, until we figure out how to make it work with 
# shinyapps.io


# Define RSG color palette
rsg_colors =
  rbindlist(
    list(
      data.table(section = 'primary', name = 'darkgrey',  r = 72,  g = 72,  b = 74),
      data.table('primary', 'orange', 246, 139,  31),
      data.table('secondary', 'marine',   0, 111, 161),
      data.table('secondary', 'sky', 117, 190, 233),
      data.table('secondary', 'leaf',  99, 175,  94),
      data.table('secondary', 'sunshine', 255, 194,  14),
      data.table('secondary', 'cherry', 186,  18,  34),
      data.table('secondary', 'violet',  82,  77, 133),
      data.table('greys', 'storm', 119, 120, 123),
      data.table('greys', 'fog', 177, 179, 182),
      data.table('greys', 'mist', 220, 221, 222)),
    use.names=FALSE)

rgb2hex = function(r,g,b) rgb(r, g, b, maxColorValue = 255)

rsg_colors[, hex := rgb2hex(r, g, b)]


#' Get hexcodes for RSG colors
#'
#' Gets the color values for the current RSG color palette
#'
#' @param colors An optional vector of color names desired.  Possible values
#' are darkgrey, orange, marine, sky, leaf, sunshine, cherry, violet, storm,
#' fog, mist.  If no colors are specified all are returned.
#' @param dataframe Should function return a data frame of colors with names,
#'  rgb values and hexcodes?  If FALSE (default), a named vector is returned.
#' @return a named vector of hexcodes for the requested colors or a dataframe of colors.
#' @seealso [get_rsg_palette()]
#' @author matt.landis@@rsginc.com
#' @references TODO: Add location of current RSG palette
#' @examples
#' \dontrun{
#' get_rsg_colors()
#' get_rsg_colors(c('orange', 'marine'))}
#'
get_rsg_colors = function(colors = NULL, dataframe=FALSE){
  
  if (is.null(colors)) {
    colors = rsg_colors[, name]
  }
  
  if ( dataframe ){
    output = rsg_colors[name %in% colors]
  } else {
    output = rsg_colors[name %in% colors, hex]
    names(output) = rsg_colors[name %in% colors, name]
  }
  
  return(output)
}

#' Get RSG color palette
#'
#' Gets a named RSG color palette as a vector of named hexcodes
#'
#' @param palette The name of an RSG color palette.  Possible values are
#' qualitative, primary (aka main), secondary, greys (aka grays), cool, hot,
#' spectrum (aka mixed).
#' @param reverse Logical.  Should the order of colors be reversed?
#' @return a named vector of hexcodes for the requested palette
#' @seealso [get_rsg_colors()]
#' @author matt.landis@@rsginc.com
#' @examples
#' get_rsg_palette('qualitative', reverse=TRUE)
#'
get_rsg_palette = function(palette, reverse=FALSE){
  
  color_names = switch(
    palette,
    qualitative = c('orange', 'sky', 'cherry', 'mist', 'darkgrey', 'leaf', 'violet', 'sunshine', 'fog'),
    primary = rsg_colors[section %in% 'primary', name],
    main = rsg_colors[section %in% 'primary', name],
    secondary = rsg_colors[section %in% 'secondary', name],
    greys = rsg_colors[section %in% 'greys', name],
    grays = rsg_colors[section %in% 'greys', name],
    cool = c('leaf', 'sky', 'violet'),
    hot = c('sunshine', 'cherry', 'violet'),
    mixed = c('sky', 'leaf', 'sunshine', 'orange', 'cherry'),
    spectrum = c('sky', 'leaf', 'sunshine', 'orange', 'cherry')
  )
  
  # Make sure the order of the colors is correct
  colors = get_rsg_colors(color_names)
  pal = colors[color_names]
  
  if (reverse) pal <- rev(pal)
  
  return(pal)
}



# For use with ggplot2 ---------------------------------------------------------------

#' RSG colors with ggplot2
#'
#' Functions to use RSG colors with ggplot2.  These functions were written based on
#' the examples shown \href{https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2}{here}
#' @param palette the name of the palette to use.  See \code{\link{get_rsg_palette}}
#' for allowed names.
#' @param discrete Logical.  Should the scale be considered discrete (TRUE) or continuous (FALSE)?
#' @param ... Additional arguments passed to \code{\link[ggplot2]{scale_fill_gradientn}} or
#' \code{\link[ggplot2]{discrete_scale}} when \code{interpolate} is \code{TRUE} or \code{FALSE}
#' respectively.
#' @return Returns colors for ggplot graphic
#' @author joe.amoroso@@rsginc.com and matt.landis@@rsginc.com
#' @seealso [get_rsg_palette()], [get_rsg_colors()]
#' @name scale_color_rsg
NULL
#' @rdname scale_color_rsg
scale_color_rsg <- function(palette="qualitative", discrete=TRUE, reverse=FALSE, ...) {
  
  aesthetic = 'color'
  
  pal <- get_rsg_palette(palette = palette, reverse = reverse)
  pal <- colorRampPalette(pal)
  
  if (!discrete & palette != 'qualitative') {
    scale_color_gradientn(colours = pal(256), ...)
  } else {
    discrete_scale(aesthetic, paste0("rsg_", palette), palette = pal, ...)
  }
}

#' @rdname scale_color_rsg
scale_fill_rsg <- function(palette="qualitative", discrete=TRUE, reverse=FALSE, ...) {
  
  aesthetic = 'fill'
  
  pal <- get_rsg_palette(palette = palette, reverse = reverse)
  pal <- colorRampPalette(pal)
  
  if (!discrete & palette != 'qualitative') {
    scale_fill_gradientn(colours = pal(256), ...)
  } else {
    discrete_scale(aesthetic, paste0("rsg_", palette), palette = pal, ...)
  }
}


