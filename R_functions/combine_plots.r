#@ title: combine plots in a pic list
#@ import ggstatsplot 
#@ import dplyr
#' Description: using this function to compare biogram length through all age groups 
#

p_list <-
   dataset %>%
   dplyr::filter(.data = ., age_BL != "<18") %>%
   dplyr::group_by(.data = ., age_BL) %>%
   dplyr::group_map(.f = ~ ggstatsplot::ggbetweenstats(data = ., x = raw_seafood_BL, y = RES,
      ylab="Antibiogram length" ,type = "np", pairwise.comparisons = TRUE,ggtheme = ggplot2::theme_gray() ))
 ggstatsplot::combine_plots(
   plotlist = p_list,
   annotation.args = list(tag_levels = list(paste0("Age:",as.vector(rlang::set_names(levels(as.factor(dataset$age_BL)))))),
                          title= "Frequency of sea food consuming over age groups"),
   plotgrid.args = list(nrow = 3, ncol = 1)                  
 ) 
