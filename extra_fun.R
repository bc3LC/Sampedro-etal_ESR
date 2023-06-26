new_colors_oy = function(datt) {
  m = min_production(datt)
  M = max_production(datt)
  
  new_colors = c("transparent","transparent","black","black","black","transparent")
  if (m < -0.5) {
    new_colors[2] = 'black'
      if (m < -1) {
        new_colors[1] = 'black'
      }
  }
  if (M > 0.5) {
    new_colors[6] = 'black'
  }
  
  return(new_colors)
}

min_production = function(datt) {
  m = datt %>%
    dplyr::group_by(region,year) %>%
    dplyr::summarise('min' = sum(production[production<0], na.rm = TRUE)) %>%
    pull(min) %>% min()
  return(m)
}
max_production = function(datt) {
  M = datt %>%
    dplyr::group_by(region,year) %>%
    dplyr::summarise('max' = sum(production[production>0], na.rm = TRUE)) %>%
    pull(max) %>% max()
  return(M)
}