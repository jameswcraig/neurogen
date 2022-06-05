
#' Create gif from brain image data
#'
#' @param file Character; File path of extension \code{.nii.gz}
#' @param color_palette Character vector of color codes
#' @param color_palette Character vector of color codes
#' @param loop Logical; If \code{TRUE} images will play then reverse
#' @param delay Numeric; Time delay for each image in seconds. Increase to slow gif
#' @param trim_start Numeric; Increasing value will remove frames at beginning of
#' gif e.g., empty frames with only background color
#' @param output_dir Character; If missing, folder will be created in current working directory.
#' @param gif_file Character; File name of gif
#' @param clean_files Logical; Set to \code{TRUE} to remove png files in \code{output_dir}
#'
#' @return
#' @export
#'
#' @examples
#' library(neurogen)
#' nii_path <- system.file(package = "neurogen", "data", "Ty_brain.nii.gz")
#' brain_warp(file = nii_path,
#'            color_palette = RColorBrewer::brewer.pal(name = "Set3", n = 8))
#'            )
#'
brain_warp <- function(file,
                       color_palette = RColorBrewer::brewer.pal(name = "Set3", n = 8),
                       color_background = "#000000",
                       loop = TRUE,
                       delay = 0.05,
                       trim_start = 0.25,
                       output_dir = "brain_warp",
                       gif_file = "brain_warp.gif",
                       clean_files = TRUE) {

  if (file.exists(file)) {
    nii <- oro.nifti::readNIfTI(file, reorient = FALSE)
  } else {
    stop(file, " not found")
  }

  if (!dir.exists(output_dir)) {
    output_dir <- file.path(getwd(), output_dir)
    dir.create(output_dir)
  }

  stopifnot(trim_start %% 1 == trim_start)
  total_frames <- dim(nii)[3]
  z <- round(total_frames * trim_start, 0)

  if (z > total_frames) {
    stop("There are ", total_frames, " total frames available",
    "Decrease `trim_start` value.")
  } else {
    frames <- total_frames - z
  }

  frames_vec <- c(1:frames)
  progress_bar <- txtProgressBar(min=0, max=frames, style = 1, char="=")

  width <- 600
  height <- 800

  col <- c(color_background,color_palette)

  for (i in seq_along(frames_vec)) {
    png(paste0(output_dir, "/brain", i, ".png"), bg = "transparent",
        height = height, width = width, antialias = "none")
    image(nii,
          z = z + i,
          plot.type = "single",
          col = col)
    dev.off()
    setTxtProgressBar(progress_bar, value = i)
  }


  files <- list.files(output_dir,
                      pattern = "\\.png$",
                      full.names = TRUE)

  files_asc <- gtools::mixedsort(files)

  if (loop) {
    files_desc <- gtools::mixedsort(files, decreasing = TRUE)
    files_full <- c(files_asc, files_desc)
  } else {
    files_full <- files_asc
  }

  gif_file_path <- file.path(output_dir, gif_file)

  gifski::gifski(files_full,
                 gif_file = gif_file_path,
                 delay = delay,
                 width = width,
                 height = height)

  if (clean_files) {
    file.remove(files)
  }

  message("File saved to ", gif_file_path)

  return(gif_file_path)
}


#' Create gif from brain image data
#'
#' @param file Character; File path of extension \code{.nii.gz}
#' @param color_palette Character vector of color codes
#' @param color_background Character hex code giving color background
#' @param infuse_rate Numeric; Between 0.5-0.99 e.g., \code{infusion_rate} must be >= 50%.
#' Higher values infuse color at greater intervals.
#' @param loop Logical; If \code{TRUE} images will play then reverse
#' @param delay Numeric; Time delay for each image in seconds. Increase to slow gif
#' @param trim_start Numeric; Increasing value will remove frames at beginning of
#' gif e.g., empty frames with only background color
#' @param output_dir Character; If missing, folder will be created in current working directory.
#' @param gif_file Character; File name of gif
#' @param clean_files Logical; Set to \code{TRUE} to remove png files in \code{output_dir}
#'
#' @return
#' @export
#'
#' @examples
#' library(neurogen)
#' nii_path <- system.file(package = "neurogen", "data", "Ty_brain.nii.gz")
#' brain_infuse(file = nii_path,
#'              color_palette = RColorBrewer::brewer.pal(name = "Set3", n = 8),
#'              infuse_rate = 0.1,
#'              trim_start = 0.31,
#'              trim_end = 0.08)
#'              )
#'
brain_infuse <- function(file,
                       color_palette = RColorBrewer::brewer.pal(name = "Set3", n = 8),
                       color_background = "#000000",
                       infuse_rate = 0.9,
                       loop = TRUE,
                       delay = 0.05,
                       trim_start = 0.25,
                       trim_end = 0.05,
                       output_dir = "brain_infuse",
                       gif_file = "brain_infuse.gif",
                       clean_files = TRUE) {

  stopifnot(infuse_rate >= 0.5)

  if (file.exists(file)) {
    nii <- oro.nifti::readNIfTI(file, reorient = FALSE)
  } else {
    stop(file, " not found")
  }

  if (!dir.exists(output_dir)) {
    output_dir <- file.path(getwd(), output_dir)
    dir.create(output_dir)
  }

  stopifnot(trim_start %% 1 == trim_start)

  dimensions <- dim(nii)
  total_frames <- dimensions[3]
  z <- round(total_frames * trim_start, 0)

  if (z > total_frames) {
    stop("There are ", total_frames, " total frames available",
         "Decrease `trim_start` value.")
  } else {
    frames <- total_frames - z
  }

  stopifnot(trim_end %% 1 == trim_end)
  frames <- round(frames * (1-trim_end))

  x <- 1:dimensions[1]
  y <- 1:dimensions[2]

  frames_vec <- c(1:frames)
  progress_bar <- txtProgressBar(min=0, max=frames, style = 1, char="=")

  width <- 600
  height <- 800

  grey_layers_vec <- get_grey_layers(frames_vec, infuse_rate)

  layers_length <- length(grey_layers_vec)

  frames_length <- length(frames_vec)

  if(layers_length != frames_length) {
    grey_layers_fill_val <- grey_layers_vec[[layers_length]]
    n_to_fill <- frames_length - layers_length
    grey_layers_vec <- c(grey_layers_vec,
                         rep(grey_layers_fill_val, n_to_fill))
  }

  for (i in seq_along(frames_vec)) {
    png(paste0(output_dir, "/brain", i, ".png"), bg = "transparent",
        height = height, width = width, antialias = "none")

    z_matrix <- nii[,, z + i]

    if (grey_layers_vec[[i]]) {
      col <- c(color_background, gray(0:64/64))
    } else {
      col <- c(color_background,color_palette)
    }

    image(x,
          y,
          z_matrix,
          col=col,
          xlab="",ylab="",
          xaxt="n",yaxt="n",
          labels = FALSE)
    dev.off()
    setTxtProgressBar(progress_bar, value = i)
  }


  files <- list.files(output_dir,
                      pattern = "\\.png$",
                      full.names = TRUE)

  files_asc <- gtools::mixedsort(files)

  if (loop) {
    files_desc <- gtools::mixedsort(files, decreasing = TRUE)
    files_full <- c(files_asc, files_desc)
  } else {
    files_full <- files_asc
  }

  gif_file_path <- file.path(output_dir, gif_file)

  gifski::gifski(files_full,
                 gif_file = gif_file_path,
                 delay = delay,
                 width = width,
                 height = height)

  if (clean_files) {
    file.remove(files)
  }

  message("File saved to ", gif_file_path)

  return(gif_file_path)
}

addNoise <- function(mtx, noise) {
  if (!is.matrix(mtx)) {
    mtx <- matrix(mtx, byrow = TRUE, nrow = 1)
  }

  random.stuff <- matrix(runif(
    prod(dim(mtx)),
    min = 1, max = noise * 10),
    nrow = dim(mtx)[1])

  return(random.stuff + mtx)
}

get_grey_layers <- function(frames_vec, infuse_rate) {

  increment <- 1- infuse_rate

  qrange <- as.numeric(quantile(frames_vec, probs = seq(0,1,increment)))

  qlength <- length(qrange)

  qrepeat <- qrange[[2]] - qrange[[1]]

  rep(c(rep(TRUE, qrepeat), rep(FALSE, qrepeat)),  qlength/2)

}

