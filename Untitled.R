library(gganimate)
library(gifski)
library(transformr)


y <- gc_rils |>
  select(petal_area_mm)|>
  filter(!is.na(petal_area_mm)) |>
  mutate(grand_mean = mean(petal_area_mm),
         diff_from_mean = petal_area_mm - grand_mean,
         squared_diff = diff_from_mean^2,
         group = factor(1:n()))|>
  mutate(bigger = ifelse(diff_from_mean >=0 , petal_area_mm, grand_mean),
         smaller = ifelse(diff_from_mean <=0 , petal_area_mm, grand_mean),
         tmp1 =  round(petal_area_mm,digits=1),
         tmp2 =  round(grand_mean,digits=1),
         tmp3 =  round(diff_from_mean,digits=1),
         tmp4 =  round(diff_from_mean^2,digits=1),
         to_print = paste( "Deviation² \n-----------------\n(",tmp1,"-",tmp2,")² \n = (",tmp3,")² \n = ",tmp4,sep = ""))

  
grand_mean_petal_area <- pull(y,grand_mean)|>unique()
min_petal_area <- pull(y,petal_area_mm)|>min()
max_petal_area <- pull(y,petal_area_mm)|>max()

anim_squared_dev <- ggplot(y, aes(x = petal_area_mm, y = petal_area_mm)) +
  geom_hline(yintercept = c(20.9 ,103.2 ), alpha = 0)+
  geom_point(size=3)+
  geom_vline(xintercept = grand_mean_petal_area)+
  geom_hline(yintercept = grand_mean_petal_area)+
  geom_rect( aes(xmax = bigger,xmin = smaller, ymax = bigger, ymin = smaller),
             alpha = .8, fill = "pink") +
  geom_segment( aes(x = bigger,xend = smaller,  y = smaller, yend = smaller))+
  geom_segment( aes(y = bigger,yend = smaller,  x = smaller, xend = smaller))+
  geom_segment( aes(x = smaller,xend = bigger,  y = bigger, yend = bigger))+
  geom_segment( aes(y = smaller,yend = bigger,  x = bigger, xend = bigger))+
  geom_label(aes(label = to_print), size = 6.8,
            x = 21, y = max_petal_area, hjust = 0, vjust = 1)+
  scale_y_continuous(breaks = grand_mean_petal_area+c(-25,0,25), 
                     labels = round(grand_mean_petal_area+c(-25,0,25)))+
  theme_light()+
  theme(axis.text = element_text(size= 20),
        axis.title = element_text(size= 20))+
  coord_cartesian(xlim = c(20.9,103.2))+
  transition_manual(group)  # Transition over the frame variable (group)

# Render the animation as a gif
animate(anim_squared_dev,fps = .5,width = 375,height = 375)
anim_save("figs/summarizing_data/univariate_summaries/petal_area_sqrd_dev", 
          animate(anim_squared_dev,fps = .5,width = 375,height = 375, renderer = gifski_renderer()))



grand_mean_petal_area <- pull(y,grand_mean)|>unique()
min_petal_area <- pull(y,petal_area_mm)|>min()
max_petal_area <- pull(y,petal_area_mm)|>max()


y  <- y |> mutate(x = group) |>
  mutate(to_print_a = paste( "Deviation\n-----------------\n(",tmp1," - ",tmp2,") \n  = ",tmp3,sep = ""))
y2 <- y |> select(- group)

y  <- y |> mutate()|> mutate(z = petal_area_mm - grand_mean_petal_area)
y2 <- y |> select(- group)|> mutate(z = petal_area_mm - grand_mean_petal_area)

anim_squared_dev_a <- ggplot(y,aes(x = x , y = z))+
  geom_hline(yintercept = c(20.9 - grand_mean_petal_area,103.2 - grand_mean_petal_area), alpha = 0)+
  geom_segment(data = y2, 
               aes(xend = x, y =0,yend = petal_area_mm - grand_mean_petal_area), alpha = .3)+
  geom_label(aes(label = to_print_a), size = 6.8,
             x = 0, y = max_petal_area - grand_mean_petal_area, 
             hjust = 0, vjust = 1)+
  geom_segment(data = y, color = "pink", linewidth = 2,
               aes(xend = x, yend = 0, y=petal_area_mm- grand_mean_petal_area))+
  geom_point(data = y, size = 4, color = "pink")+
  theme_light()+
  geom_label(y = grand_mean_petal_area -max_petal_area+5, 
             aes(label = paste("i =",x)), size = 6.8)+
  scale_y_continuous(
    breaks = c(-25,0,25), 
    name = "Deviation\n(Petal area - mean petal area)",
    sec.axis = sec_axis(
      transform = ~ . + grand_mean_petal_area,
      breaks = round(grand_mean_petal_area+c(-25,0,25)),
      name = "") ) +
  geom_hline(yintercept = 0)+
  theme_light()+
  theme(axis.text = element_text(size= 20),
        axis.title = element_text(size= 20),
        panel.grid.major.x = element_line(color = "snow"))+
  coord_cartesian(ylim = c(20.9 - grand_mean_petal_area,103.2 - grand_mean_petal_area), x = c(-5,104))+
  scale_x_discrete(name = "i", 
                   labels = function(x) ifelse(x %in% as.character(c(seq(5,100,30))), x, ""))+
  transition_manual(group) 

# Render the animation as a gif
animate(anim_squared_dev_a,fps = .5,width = 375,height = 375)
anim_save("figs/summarizing_data/univariate_summaries/petal_area_sqrd_dev_a", 
          animate(anim_squared_dev_a,fps = .5,width = 375,height = 375, renderer = gifski_renderer()))




anim_squared_dev_c <- ggplot(y,aes(x = x , y = (z^2)))+
  geom_hline(yintercept = c(20.9 - grand_mean_petal_area,103.2 - grand_mean_petal_area), alpha = 0)+
  geom_segment(aes(xend = x, yend = 0 ), linewidth = 2, color = "pink")+
  geom_point(size= 4,)+
  geom_segment(data = y2, 
               aes(xend = x, y =0,yend = petal_area_mm - grand_mean_petal_area), alpha = .3)+
  theme_light()+
  geom_label(y = -50, 
             aes(label = paste("i =",x)), size = 6.8)+
  scale_y_continuous(
    name = "Squared Deviation",
    sec.axis = sec_axis(
      transform = ~ . + 0,
      name = "Deviation") ) +
  geom_hline(yintercept = 0)+
  theme_light()+
  theme(axis.text = element_text(size= 20),
        axis.title = element_text(size= 20),
        panel.grid.major.x = element_line(color = "snow"),
        axis.title.y.right = element_text(color = "grey90"),
        axis.text.y.right = element_blank(),
        axis.title.y.left = element_text(color = "pink"))+
  scale_x_discrete(name = "i", 
                   labels = function(x) ifelse(x %in% as.character(c(seq(5,100,30))), x, ""))+
  coord_cartesian(x = c(-5,104))+
  transition_manual(group) 

  transition_manual(group) 
  scale_y_continuous(
    breaks = c(-25,0,25), 
    name = "Deviation\n(Petal area - mean petal area)",
    sec.axis = sec_axis(
      transform = ~ . + grand_mean_petal_area,
      breaks = round(grand_mean_petal_area+c(-25,0,25)),
      name = "") ) +
  geom_hline(yintercept = 0)+
  theme_light()+
  theme(axis.text = element_text(size= 20),
        axis.title = element_text(size= 20),
        axis.title = element_text(size= 20),
        panel.grid.major.x = element_line(color = "snow"))+
  coord_cartesian(ylim = c(20.9 - grand_mean_petal_area,103.2 - grand_mean_petal_area), x = c(-5,104))+
  scale_x_discrete(name = "i", 
                   labels = function(x) ifelse(x %in% as.character(c(seq(5,100,30))), x, ""))+
  transition_manual(group) 

# Render the animation as a gif
animate(anim_squared_dev_a,fps = .5,width = 375,height = 375)
anim_save("figs/summarizing_data/univariate_summaries/petal_area_sqrd_dev_c", 
          animate(anim_squared_dev_a,fps = .5,width = 375,height = 375, renderer = gifski_renderer()))















library(magick)
gif1 <- image_read("figs/summarizing_data/univariate_summaries/petal_area_sqrd_dev_a")
gif2 <- image_read("figs/summarizing_data/univariate_summaries/petal_area_sqrd_dev")

combined <- image_append(c(gif1[1], gif2[1]))  # init
for (i in 2:length(gif1)) {
  frame <- image_append(c(gif1[i], gif2[i]))
  combined <- c(combined, frame)
}



image_write(combined, "3-column-animation.gif")
ggplot(y, aes(x=x, y = petal_area_mm))+
  geom_point()
