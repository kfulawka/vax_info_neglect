library(brms)
library(ggplot2)
library(patchwork)
library(data.table)

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
ggsave(paste0(save_p, 'Fig_covid_attitude_demo_info_neglect.jpg'),
       plot = in_plt,
       units = 'cm',
       height = 9,
       width = 16,
       dpi = 700,
       scale = 1.5)

# APN side effects
se_pn_plt = comp_par_mods(m02_in$se_pn,
                       outcome = 'APN side effects (Fig. 5a)')

# save to file
ggsave(paste0(save_p, 'Fig_covid_attitude_demo_apn_se.jpg'),
       plot = se_pn_plt,
       units = 'cm',
       height = 9,
       width = 16,
       dpi = 700,
       scale = 1.5)


# APN side effects
b_pn_plt = comp_par_mods(m02_in$b_pn,
                         outcome = 'APN benefits (Fig. 5a)')

# save to file
ggsave(paste0(save_p, 'Fig_covid_attitude_demo_apn_ben.jpg'),
       plot = b_pn_plt,
       units = 'cm',
       height = 9,
       width = 16,
       dpi = 700,
       scale = 1.5)


# info neglect and choice -------------------------------------------------

# APN side effects
accept_in = comp_par_mods(m03_co_in[c(2, 4)],
                          outcome = 'Vax accept and info neglect (Fig. 4b and 5b)')

# save to file
ggsave(paste0(save_p, 'Fig_covid_attitude_demo_accept_infoNeglect.jpg'),
       plot = accept_in,
       units = 'cm',
       height = 13,
       width = 16,
       dpi = 700,
       scale = 1.5)