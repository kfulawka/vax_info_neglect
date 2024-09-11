# function to generate descriptive plots
cross_tab_ggplot = function(x, 
                            g = 'covid_vax_attitude',
                            g_n = g,
                            x_n = x,
                            tilt_x_txt = F,
                            proportion = F,
                            data = d,
                            g_cols = viridis::viridis(3, .5)) {
  
  # get data
  xx = d[,c(g, x)]
  
  # unique levels
  nl = length(unique(xx[,g]))
  
  # table
  x_tab = table(xx)
  dx = data.frame(x_tab)
  dx$proportion = data.frame( prop.table(x_tab, 2))[,3]
  
  # chi square test
  chi_sq_r = chisq.test(x_tab,
                        simulate.p.value = T,
                        B = 1e4)
  
  # Cramer's V
  V = round( sqrt( chi_sq_r$statistic / (sum(x_tab) * min(dim(x_tab)-1)) ), 2)
  
  # test results into string
  test_res = paste0('V = ', V, 
                    ifelse(round(chi_sq_r$p.value, 3) == 0, 
                           ', p < 0.001',
                           paste(', p = ', round(chi_sq_r$p.value, 3))))
  
  # figure
  colnames(dx)[1:2] = c('x', 'y')
  
  if(proportion) dx$yy = dx$proportion else dx$yy = dx$Freq

  plt = ggplot(data = dx, 
               mapping = aes(x = y,
                             y = yy)) +
    geom_bar(mapping = aes(fill = x),
             position = "stack", 
             stat = "identity") +
    scale_fill_manual(name = g_n, 
                      values = g_cols) +
    ylab(ifelse(proportion, 'proportion', 'frequency')) +
    xlab(x_n) +
    ggtitle(test_res) +
    theme_bw() 
  
  if(tilt_x_txt) plt = plt + theme(axis.text.x = element_text(angle = 12))
  
  # add frequencies
  if(!proportion) {
    
    frq = data.frame(table(xx[,2]) / nrow(xx))
    frq$Freq = round(frq$Freq * 100, 1)
    plt = plt + geom_text(data = frq,
                          mapping = aes(x = Var1,
                                        y = max(dx$Freq),
                                        label = Freq),
                          col = 'black')
    
  }

  # 
  return(plt)
  
}