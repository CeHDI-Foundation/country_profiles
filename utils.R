# Define a custom function to relabel NA
relabel_na <- function(x) {
  x[is.na(x)] <- "No available data" # Check for both R's NA and string "NA"
  return(x)
}



fun_mlm_plot <- function(df, x, y = estimate, fill_var, facet_var, title_text = "Estimate over time", xlab = "Year", ylab = "Estimate"){
  p1<-df |> 
    ggplot(aes(x={{x}}, y = {{y}}, color = {{fill_var}}, fill = {{fill_var}}))+
    geom_line()+
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA)+
    theme_bw()+
    labs(x = xlab, y = ylab, 
         title = str_wrap(title_text, width = 60),
         fill = NULL, color = NULL)+
    theme(
      panel.grid.minor = element_blank(),
      axis.title.x = element_text(size = 20),
      axis.text = element_text(size = 15),
      axis.title.y = element_text(size = 20),
      title = element_text(size = 15),
      legend.key.size = unit(1, "cm"),
      legend.text = element_text(size = 12)
    )+
    facet_wrap(.~{{facet_var}})
  print(p1)
}