#' @importFrom stats predict as.formula stepfun ccf dgamma lm na.omit na.pass quantile sd weighted.mean cor var terms.formula time var
#' @importFrom dplyr count left_join full_join case_when %>% ungroup group_split n arrange desc bind_rows group_by sym summarise select mutate rename pull filter lead lag across
#' @importFrom plyr join_all
#' @importFrom tidyr pivot_wider pivot_longer all_of drop_na
#' @importFrom zoo rollapply rollsumr rollmean
#' @importFrom limma loessFit
#' @importFrom rlang := .data
#' @importFrom ggplot2 scale_fill_gradientn scale_y_discrete labs geom_bar scale_y_continuous scale_fill_gradient2 ylab xlab ggtitle geom_text ggplot aes geom_point geom_col geom_vline element_text scale_x_date theme scale_fill_manual scale_fill_gradient element_blank facet_grid geom_line geom_rect geom_tile
#' @importFrom plotly ggplotly 
#' @importFrom patchwork plot_layout wrap_plots
#' @importFrom rsample bootstraps analysis assessment
#' @importFrom partykit lmtree
#' @importFrom data.table transpose
#' @importFrom forecast ets
#' @importFrom gridExtra grid.arrange
#' @importFrom grDevices rainbow
#' @importFrom RcppRoll roll_mean roll_sum
#' @importFrom methods new show
#' @importFrom reshape2 melt
#' @importFrom tidyselect where any_of starts_with everything
#' @importFrom randomForest na.roughfix
NULL