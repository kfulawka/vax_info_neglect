# function to extract no of events per information type 
# within vax and subject
search_extract = function(x, agr_fun = length) {
  
  # vector with vaccine names
  vax = c('AstraZeneca', 'Moderna', 'BioNTech/Pfizer', 'Johnson & Johnson',
          'Sinovac', 'Bharat Biotech', 'Novavax', 'CanSino Biologics')
  
  # vector with event names
  events = c('P_SE1', 'P_SE2', 'P_SE3', 'SE1', 'SE2', 'SE3',
             'P_CRE1', 'P_CRE2', 'P_CRE3', 'CRE1', 'CRE2', 'CRE3')
  
  # number of events per information type
  ds = aggregate(duration ~ choice + event + vax + covid_vax_attitude,
                 FUN = agr_fun,
                 data = x)
  ds$vax = as.character(ds$vax)
  
  # set empty data 
  dn = data.frame(vax = c(rep(vax[1:5], each = 12), 
                          rep(vax[6:8], each = 10)),
                  event = c(rep(events, times = 5),
                            rep(events[-c(9, 12)], times = 3)),
                  duration = NA)
  
  # fill the data with counts
  for(i in 1:nrow(ds)) {
    
    dn[dn$vax == ds$vax[i] & dn$event == ds$event[i], 'duration'] = ds$duration[i]
    
  }
  
  # fill missing entries with zeros
  dn$duration[is.na(dn$duration)] = 0 
  
  # ads subject no
  dn$sub = unique(x$sub)
  
  # ads choice info
  dn = merge(dn, 
             unique(ds[,c('choice', 'vax', 'covid_vax_attitude')]),
             by = 'vax')
  
  # into wide format
  dw = dcast( data.table(dn),
              sub + covid_vax_attitude + vax + choice ~ event,
              value.var = 'duration')
  
  dw = data.frame(dw)
  
  # reorder columns :)
  dw = dw[,c('sub', 'covid_vax_attitude', 'vax',  'choice', events)]
  
  # information neglect -----------------------------------------------------
  
  # categorize each trial into: 
  # (1) complete neglect; (2) selective neglect; (3) no neglect
  dw$ineg = apply(dw[,events], 1, function(y) {
    
    # max sum
    ms = 12 - sum( is.na(y) ) # three vaccines had 2 benefits
    
    ine = cut( sum(y == 0, na.rm = T), 
               breaks = c(-1, 0, ms-1, ms),
               labels = c('no', 'selective', 'full'),
               ordered_result = T)
    
  })
  

  # probability neglect -----------------------------------------------------
  
  # SE and BEN
  pn = apply(dw[, events ], 1, function(y) {
    
    # counts of probabilities acquisition
    ps = y[ c( paste0( rep( c('P_SE', 'P_CRE'), each = 3), 1:3) ) ]
    
    # counts of outcomes aquisition
    os = y[ c( paste0( rep( c('SE', 'CRE'), each = 3), 1:3) ) ]
    
    # probability neglect is when count of ps is zero, and greater than zero for coresponding outcome
    r = as.numeric( (ps == 0) & (os > 0) )
    
    return( r )
    
  }); pn = data.frame( t(pn) ) 
  
  # add colnames
  colnames(pn) = paste0( rep( c('P_SE', 'P_CRE'), each = 3), 1:3, '_pn')
  
  # combine
  dw = cbind(dw, pn)
  
  colnames(dw)[ colnames(dw) %in% events ] = paste0(events, '_aqc') # add note that these are acquisition counts
  
  # output
  return(dw)
  
}