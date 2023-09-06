# function plotting predicted p(vax-accept) and data (proportions)
plt_p_pred_data = function(m,
                           conditions,
                           pred,
                           colors = viridis(3, .7),
                           b_col = 'black',
                           contr = '',
                           y_ph_val = .5,
                           add_data = T,
                           y = 'choice',
                           plt_title = NULL) {
  
  # data used for estimation
  dat = m$data
  
  # get the predicted posterior probs with 50%, 80% and 95% CI
  pp = lapply(c(.5, .8, .95), function(p) {
    
    d = conditional_effects(m,
                            conditions = conditions,
                            # method = 'posterior_predict',
                            effects = pred,
                            prob = p)[[1]]
    
    # add ci range 
    colnames(d)[ colnames(d) == 'lower__' ] = paste0('li_', gsub('0.', '', p))
    colnames(d)[ colnames(d) == 'upper__' ] = paste0('ui_', gsub('0.', '', p))
    
    #
    return(d)
    
  })
  
  # put the data together
  pd = cbind(pp[[1]][, c(grep('effect', colnames(pp[[1]]), value = T), 
                         'estimate__', 'li_5', 'ui_5') ],
             pp[[2]][, c('li_8', 'ui_8')],
             pp[[3]][, c('li_95', 'ui_95')])
  
  # depending on whether it's and interaction or not
  if(grepl(':', pred)) { # interaction will have : sign in the pred variable
    
    # get the names of both predictors
    preds = strsplit(pred, ':')[[1]]
    
    # set them as names
    colnames(pd)[1:2] = preds
    
    # get the frequencies
    props = prop.table(table(dat[,preds[1]], dat[,preds[2]], dat[,y]), 1:2)[,,'1']
    props = as.data.frame(props)
    
    # add the colnames
    colnames(props)[1:2] = preds
    
    # combine with pred. probs
    pd = merge(pd, props, by = preds)
    
    # factor coding from the data
    for(i in 1:2) {
      pd[,preds[i]] = factor(pd[,preds[i]],
                             levels = levels(dat[,preds[i]]),
                             ordered = is.ordered(dat[,preds[i]]))
    }
    
    x_pred = preds[1]
    c_pred = preds[2]
    
  } else {
    
    # set pred name
    colnames(pd)[1] = pred
    
    # get the frequencies
    props = prop.table(table(dat[,pred], dat[,y]), 1)[,'1']
    props = as.data.frame(props)
    
    props[,pred] = rownames(props)
    colnames(props)[1] = 'Freq'
    
    pd = merge(pd, props, by = pred)
    
    # add the pred as a column
    pd[,pred] = factor(pd[,pred],
                       levels = levels(dat[,pred]),
                       ordered = is.ordered(dat[,pred]))
    
    #
    x_pred = pred
    c_pred = NULL
    
  }
  
  # if x_pred is vax then reorder it
  if(x_pred == 'vax') {
    
    pd$vax <- factor(pd$vax, 
                     levels = c('Sinovac', 'CanSino Biologics', 'Johnson & Johnson', 'AstraZeneca', 
                                'Novavax', 'Moderna', 'Bharat Biotech', 'BioNTech/Pfizer'), 
                     ordered = T)
    
  }
  
  # ggplot ------------------------------------------------------------------
  
  if(!is.null(c_pred)) {
    
    plt = ggplot(data = pd,
                 mapping = aes(x = .data[[x_pred]],
                               color = .data[[c_pred]],
                               group = .data[[c_pred]])) +
      # geom_linerange(mapping = aes(ymin = li_5, 
      #                              ymax = ui_5),
      #                linewidth = 1.5,
      #                position = position_dodge(.5),
      #                alpha = .7,
      #                show.legend = F) +
      # geom_linerange(mapping = aes(ymin = li_8, 
      #                              ymax = ui_8),
      #                linewidth = 1,
      #                position = position_dodge(.5),
      #                alpha = .7,
      #                show.legend = F) +
      geom_linerange(mapping = aes(ymin = li_95, 
                                   ymax = ui_95),
                     linewidth = .5,
                     position = position_dodge(.5),
                     alpha = .7,
                     show.legend = F) +
      geom_point(mapping = aes(y = estimate__,
                               shape = 'Model'),
                 position = position_dodge(.5))
    
  } else {
    
    plt = ggplot(data = pd,
                 mapping = aes(x = .data[[x_pred]])) +
      # geom_linerange(mapping = aes(ymin = li_5, 
      #                              ymax = ui_5),
      #                linewidth = 1.5,
      #                alpha = .7,
      #                color = b_col,
      #                show.legend = F) +
      # geom_linerange(mapping = aes(ymin = li_8, 
      #                              ymax = ui_8),
      #                linewidth = 1,
      #                alpha = .7,
      #                show.legend = F,
      #                color = b_col) +
      geom_linerange(mapping = aes(ymin = li_95, 
                                   ymax = ui_95),
                     linewidth = .5,
                     alpha = .7,
                     show.legend = F,
                     color = b_col) +
      geom_point(mapping = aes(y = estimate__,
                               shape = 'Model'),
                 color = b_col) 
    
  }
  
  #
  plt = plt + 
    scale_y_continuous('P(accept)',
                       breaks = seq(0, 1, .2),
                       limits = 0:1) +
    theme_bw()
  
  # change colors if not null
  if(!is.null(c_pred)) {
    
    plt = plt + scale_color_manual(values = colors) +
      theme(legend.position = 'right')
    
  }
  
  # add observed frequencies
  if(add_data) {
    
    plt = plt + 
      geom_point(mapping = aes(y = Freq,
                               shape = 'Data'),
                 position = position_dodge(.5),
                 alpha = .7) +
      scale_shape_manual(values = c(4, 3)) + 
      labs(shape = '') +
      theme(legend.position = c(.25, .8),
            legend.title = element_blank())
  }
  
  # add titile 
  if(!is.null(plt_title)) {
    
    plt = plt + ggtitle(plt_title)
    
  }
  
  plt = plt + theme(legend.background = element_rect(fill = NA),
                    legend.key = element_rect(fill = NA))
  
  # OUTPUT ------------------------------------------------------------------
  
  return(plt)
  
}