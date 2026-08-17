

#Survivorship waterfall plot
waterfall_plot_df<- input_for_df_age %>%
  mutate_all(~ as.numeric(.)) %>%
  mutate(Condition = as.factor(case_when(TimeAlive == PotentialTimeAlive ~ "Alive", 
                                         TimeAlive != PotentialTimeAlive ~ "Dead"))) %>% #Add condition back in since I need it to color the lines to differentiate between things which died and things which are still alive
  mutate(rank = row_number(desc(TimeAlive))) %>% #Making a column with a ranked value for each individual so I can offset each individual by a small amount on my yaxis AND have the individuals appear in order by longest time alive at the top of the graph  
  mutate(yval = 19.5 - rank*.0075) %>% #setting the value for the horizontal line for each individual, with a max value of 19.5 and then descending by rank 
  rowwise() %>% #I don't know why I need this but without it the uniform distribution call below outputs the same value for every row
  mutate(xval = TimeAlive + runif(1, min = -5, max = 5)) #setting the value for the vertical line for each individual by jittering a small amount from the real TimeAlive value (via sampling from a uniform distribution) 

#Making a df with just dead individuals so that I can not plot vertical lines for the individuals that are still alive
waterfall_plot_df_Dead <- waterfall_plot_df%>%
  filter(Condition == "Dead")


waterfall_plot_df %>%
  ggplot(aes(x = TimeAlive)) +
  geom_segment(aes(x = 0, xend = xval, y=yval, yend=yval, color = Condition), linewidth = .25, alpha = .5) + #Makes the horizontal line for each individual
  geom_segment(data = waterfall_plot_df_Dead, aes(x = xval, xend = xval, y=yval, yend=.5, color = Condition), alpha = .25) + #Makes the vertical (death) line for each individual
  scale_color_manual(values = c("olivedrab", "black")) + #sets the colors for the lines based on the Condition with living inds being green
  ylab("") + 
  scale_y_continuous(breaks = c(2.5, 16), #adds tick only for Dead and Alive 
                     labels = c('Dead', 'Alive'), 
                     limits = c(0,20)) + #set y lim 
  scale_x_continuous(name = 'Time', 
                     breaks = c(0, 182.5, 365, 547.5, 730, 912.5), #adds tick marks at 6 month intervals
                     labels = c('0', '0.5yr', '1yr', '1.5yrs','2yrs', '2.5yrs')) +
  geom_vline(xintercept = 0, linetype="dashed", color='red') +  #marks 0 year
  geom_vline(xintercept = 365, linetype="dashed", color='red') +  #marks 1 year
  geom_vline(xintercept = (365*2), linetype="dashed", color='red') + #marks 2 years
  theme_classic()
