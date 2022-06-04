
#' Create gif from brain image data
#'
#' @param file Character; File path of extension \code{.nii.gz}
#' @param color_palette Character vector of color codes
#' @param loop Logical; If \code{TRUE} images will play then reverse
#' @param delay Numeric; Time delay for each image in seconds. Increase to slow gif
#' @param z_offset Numeric; Increasing value will remove frames at beginning of
#' gif e.g., empty frames with only background color
#' @param output_dir Character; If missing, folder will be created in current working directory.
#' @param gif_file Character; File name of gif
#' @param clean_files Logical; Set to \code{TRUE} to remove png files in \code{output_dir}
#'
#' @return
#' @export
#'
#' @examples
brain_warp <- function(file,
                       color_palette = RColorBrewer::brewer.pal(name = "Set3", n = 8),
                       loop = TRUE,
                       delay = 0.05,
                       z_offset = 0.25,
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

  stopifnot(z_offset %% 1 == z_offset)
  total_frames <- dim(nii)[3]
  z <- round(total_frames * z_offset, 0)

  if (z > total_frames) {
    stop("There are ", total_frames, " total frames available",
    "Decrease `z_offset` value.")
  } else {
    frames <- total_frames - z
  }

  frames_vec <- c(1:frames)
  progress_bar <- txtProgressBar(min=0, max=frames, style = 1, char="=")

  width <- 600
  height <- 800

  for (i in seq_along(frames_vec)) {
    png(paste0(output_dir, "/brain", i, ".png"), bg = "transparent",
        height = height, width = width, antialias = "none")
    image(nii,
          z = z + i,
          plot.type = "single",
          col = color_palette)
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
