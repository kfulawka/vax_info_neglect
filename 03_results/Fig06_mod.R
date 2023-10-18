rm(list = ls())

library(ggplot2)
library(patchwork)

load("03_results/Fig06_top.RData")
load("03_results/Fig06_mb.RData")

# Fig06 -------------------------------------------------------------------

fig06t = (rb_plt + vax_plt + sv_plt) + plot_layout(widths = c(2.7, 4, 5))

des_b = c('ABCD\nEFGH')

fig06b = (plp_plts[[2]] + v_plts[[1]] + v_plts[[2]] + v_plts[[3]] +
          plp_plts[[1]] + pwf_plts[[1]] + pwf_plts[[2]] + pwf_plts[[3]]) +
  plot_layout(design = des_b)


fig06 = (fig06t / fig06b) +
  plot_layout(heights = c(1, 2.3)) +
  plot_annotation(tag_levels = 'a') &
  theme(plot.tag.position = c(.1, 1),
        plot.tag = element_text(face = 'bold'))

# save to file
ggsave('03_results/figures/Fig06.pdf',
       plot = fig06,
       units = 'cm',
       height = 12,
       width = 16,
       device = 'pdf',
       scale = 1.5)

# # spudm -------------------------------------------------------------------
# 
# fig06t = (rb_plt + vax_plt + sv_plt) + plot_layout(widths = c(2.7, 4, 5))
# 
# # save to file
# ggsave('03_results/spudm/Fig_mod_comp.jpg',
#        plot = fig06t,
#        units = 'cm',
#        height = 4.5,
#        width = 16,
#        dpi = 700,
#        scale = 1.5)
# 
# des_b = c('ABCD\nEFGH')
# 
# fig06b = (plp_plts[[2]] + v_plts[[1]] + v_plts[[2]] + v_plts[[3]] +
#             plp_plts[[1]] + pwf_plts[[1]] + pwf_plts[[2]] + pwf_plts[[3]]) + 
#   plot_layout(design = des_b) 
# 
# # save to file
# ggsave('03_results/spudm/Fig_vf_pwf.jpg',
#        plot = fig06b,
#        units = 'cm',
#        height = 8,
#        width = 16,
#        dpi = 700,
#        scale = 1.5)
# 
# # Fig06i -------------------------------------------------------------------
# 
# des = c(
#   'ABCC
#   DEFG
#   HIJK'
# )
# 
# fig06i = (rb_plt + vax_plt + sv_plt +
#            plp_plts[[2]] + iv_plts[[1]] + iv_plts[[2]] + iv_plts[[3]] +
#            plp_plts[[1]] + ipwf_plts[[1]] + ipwf_plts[[2]] + ipwf_plts[[3]]) + 
#   plot_layout(design = des) +
#   plot_annotation(tag_levels = 'a') &
#   theme(plot.tag.position = c(.1, 1),
#         plot.tag = element_text(face = 'bold'))
# 
# # save to file
# ggsave('results/figures/Fig06i.jpg',
#        plot = fig06i,
#        units = 'cm',
#        height = 12,
#        width = 16,
#        dpi = 700,
#        scale = 1.5)
# 
# # figure for the paper ----------------------------------------------------
# 
# # elpd_w_plt = elpd_w_plt + guides(fill = 'none')
# 
# # another one using desing matrix
# des = c(
#   'AAABBBCC
#   DDDDDDEE
#   FFFFFFGG'
# )
# 
# fig06 = (elpds_plt + elpd_d_plt + plp_plts[[1]] + 
#            v_plt + plp_plts[[3]] +
#            pwf_plt + plp_plts[[2]]) + 
#   plot_layout(design = des) +
#   plot_annotation(tag_levels = 'a') &
#   theme(plot.tag.position = c(0, 1),
#         plot.tag = element_text(face = 'bold'))
# # fig06
# 
# # save to file
# ggsave('results/figures/Fig06.jpg',
#        plot = fig06,
#        units = 'cm',
#        height = 12,
#        width = 16,
#        dpi = 700,
#        scale = 1.5)