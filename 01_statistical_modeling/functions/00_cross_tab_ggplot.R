# function to generate descriptive plots
cross_tab_ggplot = function(x, 
                            g = 'covid_vax_attitude',
                            data = d) {
  
  # get data
  xx = d[,c(g, x)]
  
  # unique levels
  nl = length(unique(xx[,g]))
  
  # table
  x_tab = table(xx)
  dx = data.frame(x_tab)
  dx$proportion = data.frame( prop.table(x_tab, 1))[,3]
  
  # chi square test
  chi_sq_r = chisq.test(x_tab,
                        simulate.p.value = T,
                        B = 1e4)
  
  # Cramer's V
  V = round( sqrt( chi_sq_r$statistic / (sum(x_tab) * min(dim(x_tab)-1)) ), 2)
  
  # test results into string
  test_res = paste0('V = ', V, 
                    ifelse(round(chi_sq_r$p.value, 3) == 0, 
                           ', p < .001',
                           paste(', p = ', round(chi_sq_r$p.value, 3))))
  
  # figure
  colnames(dx)[1:2] = c('x', 'y')
  plt = ggplot(data = dx, 
               mapping = aes(x = y,
                             y = Freq)) +
    geom_bar(mapping = aes(fill = x),
             position = "stack", 
             stat = "identity") +
    scale_fill_manual(name = g, 
                      values = viridis::inferno( nl, alpha = .5)) +
    xlab(x) +
    ggtitle(test_res) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 12))
  
  # add frequencies
  frq = data.frame(table(xx[,2]) / nrow(xx))
  frq$Freq = round(frq$Freq * 100, 1)
  plt = plt + geom_text(data = frq,
                        mapping = aes(x = Var1,
                                      y = -10,
                                      label = Freq),
                        col = 'red')
  
  # 
  return(plt)
  
}