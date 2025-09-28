#' Ternary Plot
#'
#' @param data the data
#' @param labels axis labels
#' @param facets facets with data
#' @param grid.spacing grid spacing
#' @param tick.length tick length
#' @param tick.label.spacing tick label spacing
#' @param axis.label.spacing axis label spacing
#' @param lims
#'
#' @returns a ternary plot
#' @export
ternit <- function(data, labels, facets,
                   grid.spacing = 0.2,
                   tick.length = 0.03,
                   tick.label.spacing = tick.length * 2,
                   axis.label.spacing = tick.length * 4,
                   lims) {

  if(missing(lims)) {
    lims <- rbind(c(0,1),c(0,1),c(0,1))
  }

  .frame <- tern_frame(lims)
  .labels <- tern_labels(.frame, labels, axis.label.spacing)
  .ticks <- tern_ticks(grid.spacing, lims, tick.length, tick.label.spacing)
  .grid <- tern_grid(grid.spacing, lims)

  if(!missing(facets)) {
    .frame <- .frame |> cross_join(facets)
    .labels <- .labels |> cross_join(facets)
    .ticks <- .ticks |> cross_join(facets)
    .grid <- .grid |> cross_join(facets)
  }

  ggplot(data) + aes(x, y) + coord_equal() +
    # grid
    geom_segment(aes(x=x1, y=y1, xend=x2, yend=y2), data=.grid, inherit.aes = FALSE, linewidth=0.25, color="gray80") +
    # ticks
    geom_segment(aes(x=x1, y=y1, xend=x2, yend=y2), data=.ticks, inherit.aes = FALSE, linewidth=0.25) +
    # frame
    geom_polygon(aes(x, y), data=.frame, inherit.aes = FALSE, color="black", fill=NA) +
    # tick labels
    geom_text(aes(x=x, y=y, label=txt), data=.ticks, inherit.aes = FALSE, size=6, size.unit="pt") +
    # axis labels
    geom_text(aes(x=x, y=y, label=txt), data=.labels, inherit.aes = FALSE, size=8/.pt) +
    theme_void() +
    theme(
      strip.text.x = element_text(margin = margin(3, 0, 3, 0)),
      strip.text.y = element_text(angle = -90, margin = margin(0, 3, 0, 3)),
      strip.background = element_rect(fill = "gray90"),
      legend.position = "bottom",
      plot.margin = margin(6, 6, 6, 6, "pt"))
}

