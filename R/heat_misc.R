#' Yet another heatmap function
#'
#' \code{heat_misc} Produces a very clean looking heatmap and allows for lots
#' of customization. Born out of my frustration with bloated heatmap functions
#' like \code{NMF::aheatmap} and \code{gplots::heatmap.2} that make the creation
#' of nice looking heatmaps possible, but do now allow for much customization.
#'
#' @param mat A matrix of values to plot in a heatmap.
#' @param pheno_list A named list of annotation attributes to add as annotation
#'    tracks. Similar to \code{gplots::heatmap.2} \code{colSideColors}.
#' @param z_score Should the rows of the matrix should be z-scored?
#' @param row_clust Sould the rows of the matrix be clustered?
#' @param col_clust Sould the columns of the matrix be clustered?
#' @param rang The maximum and minimum values to be plotted. Values exceeding
#'    this range will be thresholded to the min/max values provided.
#' @param axis_scale Value to pass to \code{cex.axis} to change the size of the
#'    axis text
#' @param show_grid Should the heatmap cells be outlined with a grid?
#' @param grid_lty Integer to select base graphics line type
#'    for \code{show_grid}
#' @param row_names Should the row names of the matrix be used as labels?
#' @param col_names Should the column names of the matrix be used as labels?
#' @param mar_padding Values to alter default \code{par()$mar} values.
#'    \code{c(bottom, left, top, right)}
#' @param pals A vector of valid \pkg{RColorBrewer} pallete names to use as
#'    colors for \code{pheno_list}
#' @param leg A logical vector indicating which of the \code{pheno_list}
#'    annotations should have a legend included.
#' @param cfunc A \code{colorRampPalette} to color the heatmap with.
#' @param na_col Color to assign to \code{NA} values.
#' @export
#' @examples
#' # a matrix
#' test = matrix(rnorm(100), ncol = 10)
#' colnames(test) = rownames(test) = letters[1:10]
#'
#' # basic usage
#' heat_misc(test)
#' heat_misc(test, z_score = FALSE)
#' heat_misc(test, row_clust = FALSE)
#'
#' # adding annotation
#' pl = list('pheno' = rep(c('a', 'b', 'c'), length.out = 10))
#' heat_misc(test, pheno_list = pl)
#' heat_misc(test, pheno_list = pl, leg = TRUE)
#' heat_misc(test, pheno_list = pl, leg = TRUE, show_grid = TRUE)

heat_misc  <- function(mat, pheno_list = NULL, z_score = TRUE, row_clust = TRUE,
                       col_clust = TRUE, rang = c(-3, 3), axis_scale = 1,
                       show_grid = F, grid_lty = 3, row_names = T,
                       col_names = T, mar_padding = c(3, 1, -4, 0),
                       pals = NULL, leg = NULL, cfunc = NULL, na_col = 'grey60',
                       ...) {


  # par settings...
  def = c(5.1, 4.1, 4.1, 2.1)
  par(mar = def + mar_padding)


  # flip our rows so image acts like we want -----------------------------------
  tp = mat[nrow(mat):1,]

  # global helpers -------------------------------------------------------------
  image_x_max = ncol(mat)
  image_y_max = nrow(mat)

  # z-scoring ------------------------------------------------------------------
  if (z_score) {
    tp = t(scale(t(tp)))
  }

  # clustering -----------------------------------------------------------------
  if (col_clust) {
    col_order = fastcluster::hclust(dist(t(tp), method = "euc"))$order
  } else {
    col_order = 1:ncol(tp)
  }

  if (row_clust) {
    row_order = fastcluster::hclust(dist(tp, method = "euc"))$order
  } else {
    row_order = 1:nrow(tp)
  }


  tp = tp[row_order, col_order]

  # color stuff...
  if (is.null(cfunc)) {
    cfunc = colorRampPalette(c("blue", "white", "red"))
  }

  breakers = seq(from = rang[1], to = rang[2], by = 0.1)

  # floor anything outside of our range
  tp[tp < rang[1]] = rang[1]
  tp[tp > rang[2]] = rang[2]


  # phenotype handling ---------------------------------------------------------

  if (!is.null(pheno_list)) {

    # set our new image params
    np = length(pheno_list)
    image_y_max = image_y_max + np

    # two matrix approach for plotting this...
    tp_bak = tp
    tp = matrix(NA, nrow = image_y_max, ncol = image_x_max)
    tpp = matrix(NA, nrow = image_y_max, ncol = image_x_max)

    rownames(tp) = c(rownames(tp_bak), rev(names(pheno_list)))
    rownames(tpp) = c(rownames(tp_bak), rev(names(pheno_list)))
    colnames(tp) = colnames(tp_bak)
    colnames(tpp) = colnames(tp_bak)

    # fill up expression and mask
    tp[1:(nrow(tp) - np), ] = tp_bak
    for (z in seq(pheno_list)) {
      tpp[nrow(tpp) + 1 - z,] = pheno_list[[z]][col_order]
    }

    tpp[is.na(tpp)] = "grey"

    # get all our levels...
    all_levs = c('grey')
    for (z in seq(pheno_list)) {
      all_levs = unique(c(all_levs, unique(pheno_list[[z]])))
    }

    # big factor vector
    ff = factor(tpp, levels = all_levs, labels = all_levs)
    fx = matrix(
      data = as.numeric(ff),
      nrow = nrow(tpp),
      ncol = ncol(tpp)
    )

    # generate as many colors as we need
    #     color_gen = colorRampPalette(
    #       brewer.pal(n = min(length(all_levs), 12),
    #                  name = 'Paired')
    #     )

    # get colors working right
    my_pals = RColorBrewer::brewer.pal.info
    if (is.null(pals)) {
      g_pals = c('Dark2', 'Set1', 'Paired', 'Set2')
      qual_pals = rownames(my_pals)[my_pals$category == 'qual']
      if (length(pheno_list) > length(qual_pals)) {
        stop("You can only provide 8 phenotypes to plot...")
      }
      pals = sample(qual_pals, size = length(pheno_list))
    } else {
      if (!all(pals %in% rownames(my_pals))) {
        stop("Please provide valid RColorBrewer color pallete names!")
      }
    }

    # use labels to assign colors
    my_cols = c(na_col)
    for (z in seq(pheno_list)) {
      pheno_levs = unique(pheno_list[[z]])
      pal_len = my_pals[pals[z], 'maxcolors']
      if (length(pheno_levs) > pal_len) {
        pfunc = colorRampPalette(RColorBrewer::brewer.pal(n = pal_len,
                                                          name = pals[z]))
        my_cols = c(my_cols, pfunc(length(pheno_levs)))
      } else {
        my_cols = c(my_cols, RColorBrewer::brewer.pal(
          n = max(length(pheno_levs), 3), name = pals[z])[1:length(pheno_levs)])
      }
    }

    pheno_colors = eval(
      parse(
        text = paste0(
          "c('",
          paste(all_levs, my_cols, sep = "' = '", collapse = "', '"),
          "')"
        )
      )
    )

    # if they want a legend, sketch out some plotting area for it and draw one
    if (!is.null(leg)) {
      if (length(leg) != length(pheno_list))
        stop('Legend selector must be same length as pheno_list.')

      leg_vals = unique(unlist(pheno_list[leg]))
      leg_cols = pheno_colors[leg_vals]


      par(fig=c(0, 1, .92, 1))
      par(mar = rep(.1, 4))

      plot(x = 1:length(leg_cols), y = rep(0, 1, length.out = length(leg_cols)),
           type = 'n', axes = F, xlab = '', ylab = '')
      legend('center', legend = names(leg_cols), col = leg_cols, pch = 15,
             horiz=TRUE, bty = 'n')

      # region for the annoted heatmap
      par(fig=c(0, 1, 0, .92), new = T)
      par(mar = def + mar_padding)

    }



    # plot our phenotype colors first
    image(x = 1:image_x_max, y = 1:image_y_max, z = t(fx), col = pheno_colors,
          axes = FALSE, xlab="", ylab="", breaks=(1:(nlevels(ff)+1))-.5, ...)

    image(x = 1:image_x_max, y = 1:image_y_max, z = t(tp),
          axes = F, xlab = '', ylab = '', col = cfunc(length(breakers)-1),
          breaks=breakers, add = T)

    # NA colors
#     na_mat = ifelse(is.na(tp), 1, NA)
#     image(1:ncol(tp), 1:nrow(tp), t(na_mat), axes = FALSE,
#           xlab = "", ylab = "", col = na_col, add = T)

    axis(side = 1, labels = if(col_names) colnames(tp) else FALSE,
         at = if(col_names) 1:ncol(tp) else FALSE, las=2, cex.axis = axis_scale,
         font=3, tck=0, mgp=c(3, 0.3, 0))

    axis(side = 2, labels = if(row_names) rownames(tp) else FALSE,
         at = if(row_names) 1:image_y_max else FALSE, las=2,
         cex.axis = axis_scale, font=3, tck=0, mgp=c(3, 0.3, 0))

    box(lwd=2)

    if (show_grid) grid(ncol(tp), nrow(tp), col = "slategrey", lty = grid_lty)
    abline(h=(image_y_max - np + 0.5), lwd=2)
    par(mar = def)
    invisible(pheno_colors)
  } else {

    ## just draw our normal heatmap if we don't have any phenos...

    image(x = 1:ncol(mat), y = 1:nrow(mat), z = t(tp),
          axes = F, xlab = '', ylab = '', col = cfunc(length(breakers)-1),
          breaks=breakers, ...)

    # NA colors
    na_mat = ifelse(is.na(tp), 1, NA)
    image(1:ncol(mat), 1:nrow(mat), t(na_mat), axes = FALSE,
          xlab = "", ylab = "", col = na_col, add = T)

    axis(side = 1,labels = if(col_names) colnames(tp) else FALSE,
         at = if(col_names) 1:ncol(tp) else FALSE,
         las=2,
         cex.axis = axis_scale,
         font=3,
         tck=0,
         mgp=c(3, 0.3, 0)
    )

    axis(
      side = 2,
      labels = if(row_names) rownames(tp) else FALSE,
      at = if(row_names) 1:nrow(tp) else FALSE,
      las=2,
      cex.axis = axis_scale,
      font=3,
      tck=0,
      mgp=c(3, 0.3, 0)
    )

    box(lwd=2)

    if (show_grid) grid(ncol(mat), nrow(mat), col = "slategrey", lty = grid_lty)

  }

  par(mar = def)
}

