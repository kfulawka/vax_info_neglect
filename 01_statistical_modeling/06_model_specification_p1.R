library(brms)
library(ggplot2)
library(patchwork)
library(data.table)

source('03_results/functions/99_fig_to_pdf.R')

# models  -----------------------------------------------------------------

# info neglect
load("01_statistical_modeling/02_information_neglect.RData")

# choice and info neglect
load("01_statistical_modeling/03_choice_info_neglect.RData")


# function to create the figure -------------------------------------------

source('01_statistical_modeling/functions/comp_par_mods.R')

# path to save
save_p = '04_online_supplement/02_statistical_modeling/model_specification_analyses/'

# information neglect -----------------------------------------------------

# total info neglect
in_plt = comp_par_mods(m02_in$ineg,
                       outcome = 'Information neglect (Fig. 4a)')

# pdf_save(path = paste0(save_p, 'Fig4a_covid_attitude_demo_info_neglect.pdf'),
#          fig = in_plt,
#          height = 9,
#          width = 16,
#          scale = 1.5)

# APN side effects
se_pn_plt = comp_par_mods(m02_in$se_pn,
                       outcome = 'APN side effects (Fig. 5a)')

se_pn_plt = se_pn_plt + theme(axis.text.y = element_blank(),
                              axis.title.y = element_blank())

# # save to file
# pdf_save(path = paste0(save_p, 'Fig5a_covid_attitude_demo_apn_se.pdf'),
#          fig = se_pn_plt,
#          height = 9,
#          width = 16,
#          scale = 1.5)

# APN side effects
b_pn_plt = comp_par_mods(m02_in$b_pn,
                         outcome = 'APN benefits (Fig. 5a)')

b_pn_plt = b_pn_plt + theme(axis.text.y = element_blank(),
                            axis.title.y = element_blank())

# # save to file
# pdf_save(path = paste0(save_p, 'Fig5a_covid_attitude_demo_apn_ben.pdf'),
#          fig = b_pn_plt,
#          height = 9,
#          width = 16,
#          scale = 1.5)


#

in_ca_plt = (in_plt | se_pn_plt | b_pn_plt) +
  plot_layout(guides = 'collect') & 
  theme(legend.position='bottom')

# save to file
pdf_save(path = paste0(save_p, 'Fig4a_5a_covid_attitude_x_in.pdf'),
         fig = in_ca_plt,
         height = 9,
         width = 16,
         scale = 1.5)

# info neglect and choice -------------------------------------------------

# APN side effects
accept_in = comp_par_mods(m03_co_in[c(2, 4)],
                          outcome = 'Vax accept and info neglect (Fig. 4b and 5b)')

# save to file
pdf_save(path = paste0(save_p, 'Fig_covid_attitude_demo_accept_infoNeglect.pdf'),
         fig = accept_in,
         height = 10,
         width = 16,
         scale = 1.5)