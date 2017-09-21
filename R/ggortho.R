#' @rdname ggortho
#' @title Plot Orthographic view of an image using \code{ggplot2}
#' @description Plotting function for an orthographic view of a 3-dimensional image
#'
#' @param img object that can be coerced to a \code{nifti} using
#' \code{\link{check_nifti}}
#' @param overlay object that can be coerced to a \code{nifti} using
#' \code{\link{check_nifti}}
#' @param xyz coordinates to be plotted
#' @param crosshairs should crosshairs be added?
#' @param col Colors to map intensities for \code{img},
#' passed to \code{\link{img_colour_df}}
#' @param col.overlay Colors to map intensities for \code{overlay},
#' passed to \code{\link{img_colour_df}}
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
#' @importFrom ggplot2 geom_hline geom_vline
construct_ggortho = function(
  img, overlay = NULL,
  xyz = NULL, crosshairs = TRUE,
  col = gray(0:64/64),
  col.overlay = oro.nifti::hotmetal(),
  ...) {

  img = check_nifti(img, allow.array = TRUE)
  img_df = img_colour_df(img = img, col = col, ...)

  if (!is.null(overlay)) {
    overlay = check_nifti(overlay, allow.array = TRUE)
    overlay[ overlay == 0 ] = NA
    overlay_df = img_colour_df(
      img = overlay,
      col = col.overlay, ...)
  }

  if (is.null(xyz)) {
    xyz = ceiling(dim(img) / 2)
  }
  slice_df = slice_colour_df(img_df, xyz = xyz)
  if (!is.null(overlay)) {
    overlay_slice_df = slice_colour_df(overlay_df, xyz = xyz)
  } else {
    overlay_slice_df = NULL
  }
  out = ggortho_slice_df(
    slice_df,
    slice_overlay_df = overlay_slice_df,
    crosshairs = crosshairs)
  L = list(
    plot = out,
    df = img_df,
    sliced_df = slice_df,
    xyz = xyz)
  return(L)
}

#' @rdname ggortho
#' @export
ggortho = function(img, overlay = NULL, xyz = NULL, crosshairs = TRUE, ...) {
  L = construct_ggortho(img = img, overlay = overlay, xyz = xyz, ...)
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
ggortho_img_df = function(
  img_df,
  overlay_df = NULL,
  xyz = NULL,
  crosshairs = TRUE) {

  slice_df = slice_colour_df(img_df, xyz = xyz)
  if (!is.null(overlay_df)) {
    slice_overlay_df = slice_colour_df(overlay_df, xyz = xyz)
  }
  return(ggortho_slice_df(
    slice_df = slice_df,
    slice_overlay_df = slice_overlay_df,
    crosshairs = crosshairs))
}


#' @rdname ggortho
#' @export
ggortho_base = function() {
  out = ggplot() + facet_wrap(~ plane2, nrow = 2, ncol = 2,
                              scales = "free")
  out = out + theme_ortho() + scale_fill_identity()
  out
}

#' @rdname ggortho
#' @param slice_df Image \code{data.frame} constructed from \code{\link{slice_colour_df}}
#' @param slice_overlay_df Image \code{data.frame} constructed from
#' \code{\link{slice_colour_df}} to overlay
#'
#' @importFrom ggplot2 "%+%"
#' @export
ggortho_slice_df = function(
  slice_df,
  slice_overlay_df = NULL,
  crosshairs = TRUE) {

  out = ggortho_base()
  out = out %+% slice_df
  out = out + geom_tile(aes_string(x = "x", y = "y", fill = "colour"))
  if (!is.null(slice_overlay_df)) {
    out = out + geom_tile(
      data = slice_overlay_df,
      aes_string(
        x = "x",
        y = "y",
        fill = "colour"))
  }
  if (crosshairs) {
    int_df = unique(
      slice_df[, c("plane", "plane2",
                   "xintercept", "yintercept")])
    out = out +
      geom_vline(data = int_df,
                 aes_string(xintercept = "xintercept"),
                 colour = "red") +
      geom_hline(data = int_df,
                 aes_string(yintercept = "yintercept"),
                 colour = "red")
  }
  return(out)
}

