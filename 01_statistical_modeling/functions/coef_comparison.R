source('results/appendix/comp_par_mods.R')

# search effort -----------------------------------------------------------

load("analyses/03_search_ord_m.RData")

search_plt = comp_par_mods(mods)

# save to file
ggsave('results/appendix/coef_comp_search.jpg',
       plot = search_plt,
       units = 'cm',
       height = 9,
       width = 16,
       dpi = 700,
       scale = 1.5)

rm(mods, search_plt)

# probability neglect -----------------------------------------------------

load("analyses/05_prob_neglect.Rdata")

lapply(names(mods), function(x) {
  
  pn_plt = comp_par_mods(mods[[x]])
  
  # save to file
  ggsave(paste0('results/appendix/coef_comp_', x, '.jpg'),
         plot = pn_plt,
         units = 'cm',
         height = 10,
         width = 16,
         dpi = 700,
         scale = 1.5)
  
})

rm(mods)

# prob neg and choice -----------------------------------------------------

load("analyses/06_choice_search_sep.Rdata")

lapply(names(mods), function(mm) {
  
  lapply(names(mods[[mm]]), function(m) {
    
    pn_plt = comp_par_mods(mods[[mm]][[m]])
    
    # save to file
    ggsave(paste0('results/appendix/coef_comp_choice_', m, '_', mm, '.jpg'),
           plot = pn_plt,
           units = 'cm',
           height = ifelse(m == 'search_ord', 10, 12),
           width = 16,
           dpi = 700,
           scale = 1.5)
    
  })
  
})


# prob neg and search -----------------------------------------------------

load("analyses/06_choice_pn_sep.Rdata")
load("analyses/06_choice_search_sep.Rdata")

lapply(names(mods), function(mm) {
  
  lapply(names(mods[[mm]])[2], function(m) {
    
    pn_plt = comp_par_mods(list(m_full = mods[[mm]][[m]]$m_main,
                                m_main = pn_m[[mm]][[m]]$m_main))
    
    # save to file
    ggsave(paste0('results/appendix/coef_pn_choice_', m, '_', mm, '.jpg'),
           plot = pn_plt,
           units = 'cm',
           height = 10,
           width = 16,
           dpi = 700,
           scale = 1.5)
    
  })
  
})
