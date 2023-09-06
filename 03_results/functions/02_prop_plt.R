prop_plt = function(y, 
                    dd,
                    lt = 'Decisions with full information neglect',
                    labs = c('At least one', 'Zero'),
                    lpos = c(.75, .2),
                    g = 'covid_vax_attitude') {
  
  pt = prop.table( table(dd[,g] , dd[,y]), 1)
  pt = round(pt*100, 2)
  xx = data.frame(pt)
  xx$ll = paste0( round(xx$Freq), '%')
  
  ggplot(data = xx,
         aes(x = Var1,
             y = Freq,
             fill = Var2)) +
    geom_bar(stat = 'identity') +
    geom_text(aes(label = ll),
              position = position_stack(vjust = .5)) +
    scale_fill_manual(name = lt,
                      values = viridis(3, .7),
                      labels = labs) +
    scale_y_continuous('Percentage of participants',
                       breaks = seq(0, 100, by = 20)) +
    xlab('Vaccination attitude') +
    theme_bw() +
    theme(legend.position = lpos,
          legend.background = element_rect(fill = 'lightgrey'),
          legend.key = element_rect(fill = 'lightgrey'))
  
}