#' @rdname ggortho
#' @title Plot Orthographic view of an image using \code{ggplot2}
#' @description Plotting function for an orthographic view of a 3-dimensional image
#'
#' @param img object that can be coerced to a \code{nifti} using
#' \code{\link{check_nifti}}
#' @param xyz coordinates to be plotted
#' @param ... arguments to pass to \code{\link{img_colour_df}}
#'
#' @return \code{construct_ggortho} returns a list of the constructed plot,
#' the full data for the image, the image for the plot and the \code{xyz} coordinate.
#' \code{ggortho} simply returns the plot
#' @note \code{ggortho2} is a duplicate of \code{ggortho} to agree with the
#' \code{\link{ortho2}} of \code{neurobae}
#'
#' @export
#'
#' @examples
#' img = oro.nifti::nifti(array(rnorm(10^3), dim = rep(10, 3)))
#' L = construct_ggortho(img)
#' ggortho(img)
#' @importFrom neurobase check_nifti slice_colour_df check_nifti img_colour_df
#' @importFrom ggplot2 ggplot geom_tile facet_wrap scale_fill_identity aes aes_string
construct_ggortho = function(img, xyz = NULL, ...) {
  img = check_nifti(img)
  img_df = img_colour_df(img = img, ...)
  if (is.null(xyz)) {
    xyz = ceiling(dim(img) / 2)
  }
  slice_df = slice_colour_df(img_df, xyz = xyz)
  out = ggortho_slice_df(slice_df)
  L = list(plot = out,
           df = img_df,
           sliced_df = slice_df,
           xyz = xyz)
  return(L)
}

#' @rdname ggortho
#' @export
ggortho = function(img, xyz = NULL, ...) {
  L = construct_ggortho(img = img, xyz = xyz, ...)
  return(L$plot)
}

#' @rdname ggortho
#' @export
ggortho2 = function(...) {
  return(ggortho(...))
}



#' @rdname ggortho
#' @param img_df Image \code{data.frame} constructed from \code{\link{img_colour_df}}
#' @export
ggortho_img_df = function(img_df, xyz = NULL) {
  slice_df = slice_colour_df(img_df, xyz = xyz)
  return(ggortho_slice_df(slice_df))
}

#' @rdname ggortho
#' @param slice_df Image \code{data.frame} constructed from \code{\link{slice_colour_df}}
#' @export
ggortho_slice_df = function(slice_df) {
  out = ggplot(
    data = slice_df,
    aes_string(x = "x", y = "y", fill = "colour")) +
    geom_tile() +
    facet_wrap(~ plane2, nrow = 2, ncol = 2,
               scales = "free")
  out = out + theme_ortho() +
    scale_fill_identity()
  return(out)
}
