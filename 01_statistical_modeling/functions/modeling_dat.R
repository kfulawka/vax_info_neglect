modeling_dat = function(pr_ds, ar_ds, vax_d) {
  
  # pr_d is process data
  # ar are affect ratings
  
  # vector with event names
  events = c('P_SE1', 'P_SE2', 'P_SE3', 'SE1', 'SE2', 'SE3',
             'P_CRE1', 'P_CRE2', 'P_CRE3', 'CRE1', 'CRE2', 'CRE3')

  # extract relevant columns from pr_d
  pr_ds = pr_ds[,c('sub', 'covid_vax_attitude', 'vax', 'choice',
                   grep('_aqc', colnames(pr_ds), value = T))]
  
  # set data for the subject
  vs = vax_d[,c('vax', events)]
  
  # order both by vax just to be sure
  pr_ds = pr_ds[order(pr_ds$vax), ]
  vs = vs[order(vs$vax), ]

  # map affect ratings  -----------------------------------------------------
  
  # extract ses from vax data
  se = as.matrix(vs[,c('SE1', 'SE2', 'SE3')])
  
  # map ar data of harms
  for(i in ar_ds$event) {
    
    se[which(se == i)] = ar_ds$ar[ar_ds$event == i] - 1
    
  }
  # insert into data
  vs[,c('SE1', 'SE2', 'SE3')] = as.numeric(se)
  
  # extract benefits from vax data
  ben = as.matrix(vs[,c('CRE1', 'CRE2', 'CRE3')])
  
  # map ar data of benefits
  for(i in ar_ds$event) {
    
    ben[which(ben == i)] = ar_ds$ar[ar_ds$event == i] + 1
    
  }
  
  # insert into data
  vs[,c('CRE1', 'CRE2', 'CRE3')] = as.numeric(ben)
  
  # map neglect data onto ar-prob data --------------------------------------
  
  # all unseen info is set to zero
  vs[,events][pr_ds[, paste0(events, '_aqc') ] == 0 ] = 0
  
  # info not provided is also set to zero (i.e., ag death protection for some of the vax)
  vs[,events][ is.na(pr_ds[, paste0(events, '_aqc') ]) ] = 0
  


  # output ------------------------------------------------------------------

  md = merge(pr_ds[,c('sub', 'covid_vax_attitude', 'vax', 'choice')], vs, 
             by = 'vax')
  
  return(md)

}