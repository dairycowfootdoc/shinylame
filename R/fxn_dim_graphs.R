#labels
lact.labs <- c("1" = "1st Lactation", 
               "2" = "2nd Lactation",
               "3" = "3rd Lactation",
               "4" = "4th Lactation",
               "5" = "5+ Lactations")


# DIM function for graphs
days_graph <- function(.df = lame4,
                       days = ftdim,
                       farms,
                       facet_var,
                       lesion,
                       lifex = lifexlame){
  dimles <- .df |> 
    filter(lifexlame > 0 & {{lesion}} == 1 & 
             farm %in%farms) %>% # only cows with lesions
    mutate(ftdim = as.numeric({{ days }}),
           lctgp = case_when(lact == 1 ~ "1",
                             lact == 2 ~ "2",
                             lact == 3 ~ "3",
                             lact == 4 ~ "4",
                             TRUE ~ "5"),
           lifexcat = case_when({{ lifex }} == 1 ~ "1st Case",
                                {{ lifex }} == 2 ~ "2nd Case",
                                TRUE ~ "Chronic"), 
           dimcat = case_when(ftdim<31 ~ "30",
                              ftdim>30 & ftdim <61 ~ "60",
                                       ftdim>60 & ftdim <91 ~ "90",
                                       ftdim>90 & ftdim <121 ~ "120",
                                       ftdim>120 & ftdim <151 ~ "150",
                                       ftdim>150 & ftdim <181 ~ "180",
                                       ftdim>180 & ftdim <211 ~ "210",
                                       ftdim>210 & ftdim <241 ~ "240",
                                       ftdim>240 & ftdim <271 ~ "270",
                                       ftdim>270 & ftdim <301 ~ "300",
                                       ftdim>300 & ftdim <331 ~ "330",
                                       ftdim>330 & ftdim <361 ~ "360",
                                       ftdim>360 & ftdim <391 ~ "390",
                                       TRUE ~ "420")) %>%
  mutate(lctgp = as_factor(lctgp),
         dimcat = as.numeric(dimcat),
         lifexcat = as_factor(lifexcat)) %>% 
  select(farm, cowid, lctgp, ftdim, lifexcat, dimcat, year) 

# create facet in text
  facet_text <- rlang::as_string(ensym(facet_var))

# need to summarize counts across dim categories, lact and lifexdd
# add group by farm here if code changed
dimlescat <- dimles %>% 
  group_by(lifexcat) %>% 
  count(dimcat, {{facet_var}}, .drop = FALSE) %>%
  ungroup()

# calc total for each category 
# then change to %
dimlestot <- dimles %>% 
  group_by(lifexcat, {{facet_var}}) %>% 
  count() %>%
  ungroup() |> 
  rename(total = n) |> 
  right_join(dimlescat, by = c("lifexcat", facet_text)) |> 
  mutate(prop = (n/total)*100)  |> 
  select({{facet_var}}, dimcat, lifexcat, prop) |> 
  rename(n = prop) # to maintain code below

# to get totals  
dimlest <- dimles %>% 
  # add {{facet_var}} if changing data set
  group_by({{facet_var}}, dimcat) %>%
  count() %>% 
  mutate(lifexcat = "Total") |> 
  ungroup()

# calc total for each category 
# then change to %
dimlessum <- dimles %>% 
  group_by({{facet_var}}) %>% 
  count() %>%
  ungroup() |> 
  rename(total = n) |> # split here to get n for lactation
  right_join(dimlest, by = c(facet_text)) |> 
  mutate(prop = (n/total)*100) |> 
  select({{facet_var}}, dimcat, prop) |> 
  mutate(lifexcat = "Total") |> 
  rename(n = prop) |> # to maintain code below
  bind_rows(dimlestot) %>% 
  # need to pivot wide
  pivot_wider(names_from = lifexcat, values_from = n) %>% 
  clean_names() %>%
  # creates repeat % probably needs and overall one
  mutate(first = x1st_case,
         second = x2nd_case) %>% 
  select(-c(x1st_case, x2nd_case))

# clean space
rm(dimlest, dimlestot)

# set max limit
max <- dimlessum |> 
  mutate(max_total = ceiling(total),
         max_first = ceiling(first),
         max_second = ceiling(second),
         max = case_when(max_total > max_first & 
                           max_first < max_second ~ max_total,
                         max_first > max_total &
                           max_second < max_first ~ max_first,
                         TRUE ~ max_second)
  )|>
  arrange(desc(max)) |> 
  slice_head() |> 
  select(max) |> 
  mutate(max = ceiling(max/10)*10)
# extract max value
max_limit <- max$max[1]  

# needed for graph
offset <- 7.5

# system
dimg_farm1 <- dimlessum %>%
  #mutate(lctgp = fct_inseq(lctgp)) |> 
  ggplot(aes(dimcat, total)) +
  geom_linerange(aes(dimcat - offset, ymin = 0, ymax = total), 
                 color = "#440154ff", alpha = .8, linewidth = 1.4) +
  geom_point(aes(dimcat - offset, total), 
             color = "#440154ff", size = 3) +
  geom_linerange(aes(dimcat, ymin = 0, ymax = first), 
                 color = "#FDE725FF", alpha = .8, linewidth = 1.4) +
  geom_linerange(aes(dimcat + offset, ymin = 0, ymax = second), 
                 color = "#21908CFF", alpha = .8, linewidth = 1.4) +
  geom_point(aes(dimcat , first), 
             color = "#FDE725FF", size = 3) +
  geom_point(aes(dimcat + offset, second), 
             color = "#21908CFF", size = 3) +
  facet_wrap(vars({{facet_var}}), ncol = 1, strip.position = "top", 
             labeller = labeller(lctgp = lact.labs),
             scales = "free"
  ) +
  coord_cartesian(expand = FALSE) +
  labs(x = NULL, y = "% of Cases in Category",
       title = "DIM of Lesion Peaks at 120 and Dry-off are to be avoided",
       caption = "Lifetime Case Type: <span style='color:#440154ff;'>All</span> • <span style='color:#FDE725FF;'>1st</span> • <span style='color:#21908CFF;'>2nd </span>")+
  scale_x_continuous(breaks = seq (30, 420, by = 30), limits = c(0,430))+
  scale_y_continuous(breaks = seq(0, max_limit, by = 5), 
                     limits = c(0, max_limit),
                     position = "left") +
  theme_minimal() +
  theme(
    text = element_text(color = "black"),
    axis.text.x = element_text(color = "grey45", margin = margin(t = 1, b = 1)),
    axis.text.y = element_text(color = "grey 45", margin = margin(l = 7)),
    plot.title = element_markdown(hjust = 0.5, color = "black"),
    plot.title.position = "plot",
    plot.caption = element_markdown(color = "black", hjust = 0.5),
    plot.caption.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.spacing = unit(.4, "lines"),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "grey15", linewidth = .3),
  ) 
}


# test code
# # all lesions
# dim <- lame4 |> 
#   mutate(lesion = if_else(trimonly == 1,0,1)) |> 
#   days_graph(facet_var = farm,
#              farms = stevens,
#                   lesion = lesion)
# 
# one lesion
# dim <- days_graph(facet_var = lctgp,
#                   farms = stevens,
#              lesion = dd)
# 
# # all farms in region
# lame5 <- lame4 |> 
#   mutate(lesion = if_else(trimonly == 1,0,1))
# dim <- map(stevens, \(.x) days_graph(.df = lame5,
#     facet_var = lctgp, farms = .x,
#     lesion = lesion)
# )

  