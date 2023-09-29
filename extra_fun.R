new_colors_oy = function(datt,y) {
  m = min_consumption(datt)
  M = max_consumption(datt)
  
  if (y == 2025) {
    new_colors = c("transparent","transparent","black","transparent","transparent")
    if (m < -0.5) {
      new_colors[2] = 'black'
        if (m < -1) {
          new_colors[1] = 'black'
        }
    }
    if (M > 0.5) {
      new_colors[4] = 'black'
        if (M > 1) {
          new_colors[5] = 'black'
        }
      
    }
  } else { # year == 2030
    new_colors = c("black","black","black")
    if (m > -0.5) {
      new_colors[1] = 'transparent'
    }
    if (M < 0.5) {
      new_colors[3] = 'transparent'
    }
  }
  
  return(new_colors)
}

min_consumption = function(datt) {
  m = datt %>%
    dplyr::group_by(region,year) %>%
    dplyr::summarise('min' = sum(consumption[consumption<0], na.rm = TRUE)) %>%
    pull(min) %>% min()
  return(m)
}
max_consumption = function(datt) {
  M = datt %>%
    dplyr::group_by(region,year) %>%
    dplyr::summarise('max' = sum(consumption[consumption>0], na.rm = TRUE)) %>%
    pull(max) %>% max()
  return(M)
}