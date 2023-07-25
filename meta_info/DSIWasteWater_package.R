#' @importFrom stats as.formula stepfun ccf dgamma lm na.omit na.pass quantile sd weighted.mean
#' @importFrom dplyr left_join full_join case_when %>% ungroup group_split n arrange desc bind_rows group_by sym summarise select mutate rename pull filter lead lag across
#' @importFrom tidyr pivot_wider pivot_longer all_of drop_na
#' @importFrom zoo rollapply rollsumr rollmean
#' @importFrom limma loessFit
#' @importFrom rlang :=
#' @importFrom ggplot2 ggplot aes geom_point geom_col geom_vline element_text scale_x_date theme scale_fill_manual scale_fill_gradient element_blank facet_grid geom_line geom_rect geom_tile
#' @importFrom patchwork plot_layout wrap_plots
#' @importFrom rsample bootstraps analysis assessment
#' @importFrom partykit lmtree
#' @importFrom data.table transpose
#' @importFrom forecast ets
#' @importFrom gridExtra grid.arrange
NULL