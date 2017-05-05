#IVA 5.5.17
layout <- list(
  autosize = F, 
  dragmode = "zoom", 
  hovermode = "x", #"closest", 
  legend = list(
    x = 0.393915787863, 
    y = -0.310102134663, 
    orientation = "h"),
  showlegend = T, 
  margin = list( r = 100, t = 100, b = 80, l = 80 ),
  title = "PrecTemp", 
  xaxis = list(
    autorange = TRUE, 
    domain = c(0, 1), 
    range = c(1, 366), 
    showspikes = TRUE, 
    title = "days", 
    type = "linear"
  ), 
  yaxis = list(
    autorange = TRUE, 
    range = c(-574.5, 315.5), 
    showspikes = TRUE, 
    title = "temp", 
    type = "linear"
  ), 
  yaxis2 = list(
    anchor = "x", 
    autorange = TRUE, 
    overlaying = "y", 
    range = c(-2, 300), 
    showspikes = TRUE, 
    title = "prec", 
    side = "right", 
    type = "linear"
  )
)