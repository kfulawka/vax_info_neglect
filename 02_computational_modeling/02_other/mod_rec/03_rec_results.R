rm(list = ls())

library(ggplot2)
library(data.table)
library(patchwork)

load("02_computational_modeling/02_other/mod_rec/rec_res.RData")

mod_n = c(rb = 'rb', oh = 'oh', pt = 'pt')

att = c(anti = 'anti', neu = 'neu', pro = 'pro')

# no of iteretions within group
L = dim(rec_res)[[1]]

# confusion matrix --------------------------------------------------------

cm = lapply(att, function(a) {
  
  a = as.data.frame(rec_res[,,a])
  
  r = sapply(mod_n, function(m) {
    
    # turn into factor first
    a[,m] = factor(a[,m],
                   levels = c('rb', 'oh', 'pt'),
                   ordered = T)
    
    # confusion matrix
    table(a[,m]) / L
    
  } )
  
  return( t(r) )
  
})


# inverse confusion matrix ------------------------------------------------

icm = lapply(cm, function(a) {
  
  r = sapply(mod_n, function(m) {
    
    a[,m] / sum(a[,m])
    
  } )
  
  round(r, 2)
  
})


# visualize ---------------------------------------------------------------

# function to visualize a matrix
cm_pl = function(cm) {
  
  # into data frame
  d = data.frame(cm)
  d$simulated = factor(rownames(d), ordered = T)
  
  # into long format
  d = melt(data.table(d), 
           id.vars = 'simulated',
           variable.name = 'fitted')
  d = data.frame(d)
  
  d[,'simulated'] = factor(d[,'simulated'],
                           levels = mod_n[3:1],
                           ordered = T)
  
  d[,'fitted'] = factor(d[,'fitted'],
                        levels = mod_n,
                        ordered = T)
  
  # ggplot 
  plt = ggplot(data = d) +
    geom_tile(mapping = aes(x = fitted, 
                            y = simulated, 
                            fill = value)) +
    geom_text(mapping = aes(x = fitted,
                            y = simulated, 
                            label = value),
              col = rgb(.8, .1, 0, .9)) +
    scale_fill_gradient(limits = c(0, 1),
                        low = "white", 
                        high = "steelblue") +
    theme_minimal()
  
  # 
  return(plt)
  
}

# apply to confusion matrices
cm_plts = lapply(names(cm), function(x) cm_pl(cm[[x]]) + ggtitle(x) )
cm_plt = wrap_plots(cm_plts, nrow = 1, ncol = 3)

# apply to inverse confusion matrices
icm_plts = lapply(names(icm), function(x) cm_pl(icm[[x]]) + ggtitle(x) )
icm_plt = wrap_plots(icm_plts, nrow = 1, ncol = 3)

plt = (cm_plt/icm_plt) + plot_layout(guides = 'collect')

source('03_results/functions/99_fig_to_pdf.R')
pdf_save(path = '04_online_supplement/03_computational_modeling/Fig03b_model_recovery.pdf',
         fig = plt,
         height = 9, 
         width = 16,
         scale = 1.5)