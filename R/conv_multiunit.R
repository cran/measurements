#' Convert Units of Measurement Composed of Multiple Units
#'
#' Converts complex units of measurement that are joined by " / " or " * ". This function supports all dimensions in \code{conv_unit_options} except for coordinates.
#'
#' @param x a numeric vector giving the measurement value in its original units. Default is 1.
#' @param from,to a string defining the multiunit with subunits separated by " / " or " * ".
#'
#' @author Matthew A. Birk, \email{matthewabirk@@gmail.com}
#' @seealso \code{\link{conv_unit}}, \code{\link{conv_unit_options}}, \code{\link{conv_dim}}
#'
#' @examples
#' conv_multiunit(x = 10, from = "ft / hr * F", to = "m / min * C")
#' conv_multiunit(x = 1:100, from = "gal_per_min * ft / psi * hp", to = "l_per_hr * km / kPa * kW")
#'
#' @encoding UTF-8
#' @export

conv_multiunit = function(x = 1, from, to){
	ops_f = gsub(pattern = '[^/\\*]', replacement = '', from)
	ops_t = gsub(pattern = '[^/\\*]', replacement = '', to)
	if(ops_f != ops_t) stop('The order the units in "from" and "to" must be equivalent.')
	ops = c(unlist(strsplit(ops_f, split = '')), '')
	froms = unlist(strsplit(from, split = ' / | \\* '))
	froms_bad = froms[!froms %in% unlist(conv_unit_options)]
	if(length(froms_bad) > 0) stop(paste(froms_bad, 'is not supported in the "from" argument. See conv_unit_options for supported units.'))
	tos = unlist(strsplit(to, split = ' / | \\* '))
	tos_bad = tos[!tos %in% unlist(conv_unit_options)]
	if(length(tos_bad) > 0) stop(paste(tos_bad, 'is not supported in the "to" argument. See conv_unit_options for supported units.'))
	convs = sapply(1:length(froms), function(i){
		conv_unit(1, froms[i], tos[i])
	})
	conv_val = paste(c(rbind(convs, ops)), collapse = ' ')
	conv_val = eval(parse(text = conv_val))
	return(x * conv_val)
}