#' R library summary
#'
#' Provide the brief summary of package
#' libraries on your machine
#'
#' @param sizes logical indicating whether or not to calculate
#'    library sizes. Default is `FALSE`.
#'
#' @return A 'data.frame' containing the count of packages in each
#'          of the users libraries
#' @export
#'
#' @examples
#' lib_summary()
lib_summary <- function(sizes = FALSE) {
  if (!is.logical(sizes)) {
    stop("'sizes' must be TRUE or FALSE.")
  }

  pkgs <- utils::installed.packages()
  pkg_tbl <- table(pkgs[, "LibPath"])
  pkg_df <- as.data.frame(pkg_tbl, stringsAsFactors = FALSE)
  names(pkg_df) <- c("Library", "n_packages")

  if(sizes) {
    pkg_df$lib_size <- sapply(
      pkg_df$Library,
      function(x) {
        sum(fs::file_size(fs::dir_ls(x, recurse = TRUE)[1:10]))
      }
    )
  }

  pkg_df
}
