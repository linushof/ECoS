
cols <- scico(n=2, begin = .1, end=.9, palette = 'managua')
binwidth <- 0.05  # choose something appropriate

means <- s1_choices |> 
  group_by(goal) |> 
  summarise(ms=mean(switch_rate))
ms <- means$ms

s1_choices |> 
  mutate(piece = switch_rate==1) |> 
  group_by(goal, piece) |> 
  summarise(count = n())

test <- s1_choices |>
  mutate(bin = cut(switch_rate, breaks = seq(0, 1.05, by = binwidth), include.lowest = TRUE, labels=F, right=FALSE)) |> 
  mutate(bin = (bin*binwidth)-.001) |> 
  group_by(goal, bin) |> 
  summarise(n = n()) |> 
  pivot_wider(names_from = goal, values_from = n, values_fill = 0) |>
  mutate(long_rel = long/sum(long), 
         short_rel = short/sum(short),
         diff=long_rel - short_rel) 

ggplot(data=test, aes(x=bin, fill=factor(diff>0, labels = c("Short", "Long")))) + 
  geom_bar(aes(y=long_rel), stat = 'identity', fill=cols[2], alpha = .3, just = 1) +
  geom_bar(aes(y=-short_rel), stat = 'identity', fill=cols[1], alpha = .3, just=1) +
  geom_bar(aes(y=diff), stat='identity', just=1) +
  
  geom_segment(aes(x = ms[1], y = 0, xend = ms[1], yend = .2), color=cols[2], linewidth = 1, linetype = 'dashed') +
  geom_segment(aes(x = ms[2], y = 0, xend = ms[2], yend = .2), color=cols[1], linewidth = 1, linetype = 'dashed') +
  
  geom_segment(aes(x = 1, y = .16, xend = .8, yend = .16),
               arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
               color="gray", linewidth=.5) +
  geom_segment(aes(x = 1, y = .16, xend = 1.2, yend = .16),
               arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
               color="gray", linewidth=.5) +
  geom_segment(aes(x = 1, y = .15, xend = 1, yend = .17), 
               color="gray", linewidth=.5) +
  
  annotate(
    "label",
    x = c(.93,1.07) ,
    y = 0.18,
    label = c("S < 1", "S = 1"),
    size = 3.5,
    label.size = 0 , 
    color='gray'
  ) +
  
  scale_y_continuous(limits=c(-.4,.4)) +
  scale_x_continuous(limits=c(0,1.2), breaks=seq(0,1,length.out=3)) +

  theme_bw() +
  #theme(axis.text.x = element_blank(),
        #axis.ticks.x =  element_blank()) +
  scale_color_scico_d(palette='managua', begin=.1, end=.9) +
  scale_fill_scico_d(palette='managua', begin=.1, end=.9) +
  
  labs(x="Switch Rate (S)",
       y="Rel. Frequency",
       fill = "Goal",
       color='Goal')
