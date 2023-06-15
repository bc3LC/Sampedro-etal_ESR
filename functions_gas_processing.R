rename_scen<- function(df){
  
  df<- df %>%
    mutate(scenario = if_else(scenario == "NewGasCalib_EUpre55CP", "CP_Default", scenario),
           scenario = if_else(scenario == "NewGasCalib_EUpre55CP_NDC", "NDC_Default", scenario),
           scenario = if_else(scenario == "NewGasCalib_EUpre55CP_noRusGas", "CP_NoRus", scenario),
           scenario = if_else(scenario == "NewGasCalib_EUpre55CP_NDC_noRusGas", "NDC_NoRus", scenario))
  
  return(invisible(df))
  
}

rename_filter_regions <- function(df, regions){
  df %>% 
    right_join(regions, by = join_by(region)) %>%
    select(-region) %>% 
    rename(region = region_rewrite)
}

symmetrise_scale <- function(p, axis = "x"){
  gb <- ggplot_build(p)
  panel <- switch(axis, "x" = "panel_scales_x", "y" = "panel_scales_y")
  
  fname <- setdiff(names(gb$layout$layout), c("PANEL", "ROW", "COL",  "SCALE_X", "SCALE_Y"))  
  facets <- gb$layout$layout[ ,fname, drop=FALSE][[fname]]
  
  range <- sapply(sapply(gb$layout[[panel]], "[[", "range"), "[[", "range")
  range2 <- as.vector(t(tcrossprod(apply(abs(range), 2, max), c(-1,1))))
  dummy <- setNames(data.frame(rep(facets, each=2), range2), c(fname, axis))
  switch(axis, 
         "x" = p + geom_blank(data=dummy, aes(x=x, y=Inf), inherit.aes = FALSE), 
         "y" = p + geom_blank(data=dummy, aes(x=Inf, y=y), inherit.aes = FALSE))
}

diff_plot <- function(df, colors, fill, title, ylab, sum_line_lab, 
                      pct = F, x_aes = "scen_policy", y_aes = "diff"){
  sum_bars <- df %>% 
    filter(region %in% selected_regions,
           year == 2030) %>% 
    group_by(scen_policy, region) %>% 
    summarise(sum_diff = sum(get(y_aes))) %>% 
    ungroup
  
  plot_data <- df %>% 
    filter(region %in% selected_regions,
           year == 2030) %>% 
    left_join_error_no_match(sum_bars, by = join_by(scen_policy, region))
  
  plot <- ggplot(plot_data, aes(.data[[x_aes]], y = .data[[y_aes]],
                                fill = factor(get(fill), names(colors)))) +
    geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
    geom_errorbar(aes(y = sum_diff, ymin = sum_diff, ymax = sum_diff, color = Units),
                  linetype = "longdash", linewidth = 0.8) +
    facet_wrap(~ region, scales = "free") +
    theme_bw() +
    labs(x = "", y = ylab) +
    guides(fill = guide_legend(nrow = 1)) +
    geom_hline(yintercept = 0,  linewidth = 0.75) +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          legend.background = element_rect(fill = "grey85"),
          strip.text = element_text(size = 10),
          axis.title.y = element_text(size = 10),
          axis.text.x = element_text(size = 9, vjust = 0.5),
          axis.text.y = element_text(size = 9)) +
    scale_color_manual(values = "red", labels = sum_line_lab,
                       guide = guide_legend(keywidth = 4 )) +
    scale_fill_manual(values = colors) +
    ggtitle(title)
  
  if (pct){
    plot <- plot + scale_y_continuous(labels = scales::percent)
  }

  return(plot)
}

df_process_diff <- function(df){
  df_diff <- df %>% 
    separate(scenario, into = c("scen_policy", "scen_gas"), sep = "_") %>% 
    pivot_wider(names_from = scen_gas) %>% 
    mutate(diff = noRus - Default)  %>% 
    group_by(scen_policy, region, year) %>% 
    mutate(total_Default = sum(Default)) %>% 
    ungroup %>% 
    mutate(diff_prop = diff / total_Default)
  
  return(df_diff)
}
