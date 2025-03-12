# Graph Lesion data setup

# Year:Lesion occurrence----

region_lesion_sum_data <- function(denominator = mall,
                                   data = lame4,
                                   lctgp = all,
                                   lact_filter = c(1:20),
                                   group = "System"){
  
# get average pop/year
mallavg <- denominator %>%  
  group_by(farm, year) %>% 
  summarise(across(c(contains("milking")), \(x)
                   mean(x, na.rm = TRUE))
            )%>%
  mutate(across(c(contains("milking")),\(x) round(x)))

# count # of lesions/during the year
ygerror <- data %>% 
  filter(lifexlame >0) %>% # only cows with lesions
  filter(ftdat < enddat) %>%
  filter(lact %in% lact_filter) |> 
  mutate(lesion = recode(trimonly, '0'= 1, '1' = 0)) %>% 
  group_by(farm, year) %>% 
  summarise(across(all_of(les_variables), \(x) sum(x, na.rm = TRUE))
            ) %>% 
  left_join(mallavg, by = c("farm", "year")) %>% 
  # get totals/month
  select(-c(all_of(lctgp))) |> 
  rename(allmilking = matches("*milking"))

# system summary
ygerrort <- ygerror %>%
  group_by(year) |> 
  summarise(across(c(allmilking, all_of(les_variables)),
                   \(x) sum(x, na.rm = TRUE))) %>% 
  mutate(farm = {{group}})

#puts the 2 files together
ygerrorall<-bind_rows(ygerror, ygerrort)  %>% 
  # make longer format for graphing and calculations
  pivot_longer(cols = c(les_variables),
               names_to = "lestype",
               values_to = "counts") %>% 
  mutate(cases = round(counts/allmilking*100,1)) # to replace binconinf not working

# merge
ygerrorall3 <- ygerrorall %>% 
  rename(all_counts = counts)

# Graphn1:just 1st case ever for any lesion (ie no other history before)
yg1error <- data %>% 
  filter(lifexlame == 1) %>% # only cows with lesions
  filter(ftdat < enddat) %>%
  filter(lact %in% lact_filter) |> 
  mutate(lesion = recode(trimonly, '0'= 1, '1' = 0)) %>% 
  group_by(farm, year) %>% 
  summarise(across(all_of(les_variables),
                   \(x) sum(x, na.rm = TRUE))) %>% 
  left_join(mallavg, by = c("farm", "year")) %>% 
  # get totals/month
  select(-c(all_of(lctgp))) |> 
  rename(allmilking = matches("*milking"))

# system summary
yg1errort <- yg1error %>%
  group_by(year) |> 
  summarise(across(c(allmilking, all_of(les_variables)),
                   \(x) sum(x, na.rm = TRUE))) %>% 
  mutate(farm = {{group}})


yg1errorall <- bind_rows(yg1error, yg1errort) %>% 
  #make longer format for graphing and calculations
  pivot_longer(cols = c(les_variables),
               names_to = "lestype",
               values_to = "counts") %>% 
  mutate(n1cases = round(counts/allmilking*100,1)) # to replace binconinf not working

# merge
yg1errorall3 <- yg1errorall %>% 
  #  left_join(df, by =c("farm", "counts","allmilking", "lestype"))%>% 
  rename(n1_counts = counts)

# join total with n1
toterrorall <- ygerrorall3 %>% 
  left_join(yg1errorall3, by =c("farm", "year", "allmilking", "lestype")) |> 
  mutate(farm = as_factor(farm)) |> 
  ungroup()
}

region_les_graph <- function(.data, 
                             group = "System",
                             facet_col = year,
                             plot_var = lestype,
                             lesions = lesions,
                             years,
                             farms){
  # filter data
  df <- .data |> 
    filter(all_counts != 0) %>% 
    filter(year %in% years) |> 
    filter(lestype %in% lesions & farm %in% farms)
  
  # define labels
  all_les_labels <- c(inf = "Infectious",
                    noninf = "Non-Infectious",
                    soleulcer = "Sole Ulcer",
                    wld = "White Line Disease",
                    dd = "Digital Dermatitis",
                    footrot = "Foot Rot",
                    thin = "Thin Sole",
                    cork = "Cork Screw",
                    injury = "Upper Leg",
                    solefract = "Sole Fracture",
                    toe = "Toe Lesion",
                    other = "Other Lesion",
                    hem = "Hemorrhage",
                    axial = "Axial Wall Crack",
                    lesion = "Any Lesion"
                    )
  # calculate min/max for graphs
  min <- df |> 
  mutate(min = min(n1cases, na.rm = TRUE)
         ) |> 
  slice_head() |>
  mutate((min = floor(min))) |> 
  select(min) 

min_limit <- min$min[1] 

max <- df |> 
  mutate(max = max(cases, na.rm = TRUE) 
         )|> 
  slice_head() |> 
  select(max) |> 
  mutate(max =ceiling(max)
  )
# extract max value
max_limit <- max$max[1]  

break_graph <- case_when(max_limit < 6 ~ 1,
                         max_limit > 6 & max_limit <11 ~ 2,
                         max_limit > 10 & max_limit < 51 ~ 5,
                         max_limit > 50 & max_limit < 101 ~ 10,
                         max_limit >101 ~ 20)

#graph
les_graph <- df %>%
#  group_by(farm, year) %>% 
  mutate(x_var = fct_reorder({{plot_var}}, cases), 
         x_var = fct_relevel(x_var, group, after = 0)) %>% 
  ggplot() +
  geom_segment(aes(x = x_var, xend = x_var, 
                   y = cases, yend = n1cases, 
                   color = "Repeats"), 
               linewidth = 2) +
  geom_point(aes(x = x_var, y = cases, color = "All"), size = 6.5) +
  geom_text(aes(x_var, cases, label = round(cases, digits = 1)), 
            nudge_x = +.01, 
            color = "white", size = 2.25,
            show.legend = FALSE) +
  geom_point(aes(x = x_var, y = n1cases, color = "1st"), size = 6.5) +
  geom_text(aes(x_var, n1cases, label = round(n1cases, digits = 1)), 
            nudge_x = +.01,  
            color = "white", size = 2.25) +
  coord_flip() +
  theme_minimal() +
  xlab("") +
  ylab("# of cases per 100 cows") + 
  scale_y_continuous(limits = c(0, max_limit),
                     breaks = seq(0, max_limit, by = break_graph),
                     expand = c(0.02, 0.02)) +
  scale_x_discrete(expand = c(0.05, 0.05),
                   labels = all_les_labels) +
  scale_color_manual(name = "# of Cases", 
                     values = c("Repeats" = "#FDE725FF", #viridis colours
                                "All" = "#440154FF", 
                                "1st" = "#7ad151ff"),
                     labels = c("1st", "Repeats", "All"),
                     breaks = c("1st", "Repeats", "All")
  )+
  guides(color = guide_legend(override.aes = list(shape = c(19, NA, 19),
                                                  linetype = c(1, 1, 1)
  ),
  label.position = "bottom",
  title.position = "top",
  title.hjust = 0.5)
  )+
  theme(legend.position = "top")+
  facet_wrap(vars({{facet_col}}), ncol = 1,
             labeller = labeller(lestype = all_les_labels))+
  theme(strip.text = element_text(face = "bold"))
}

# function test code
# ignore warnings about mutate as due to group variable 
# need to fix and create 2 versions
# with all lesions
# les_data <- region_lesion_sum_data() |> 
#   region_les_graph(lesions = les_variables,
#                    years = c(2023, 2024),
#                    farms = "System")
# 
# # just one lesions
# les_data <- region_lesion_sum_data() |> 
#   region_les_graph(lesions = "dd",
#                    years = c(2023, 2024),
#                    farms = "System")
# 
# multiple farms by regions
# multiple lesions
# les_data <- region_lesion_sum_data() |> 
#   region_les_graph(lesions = c("dd", "footrot"),
#                    facet_col = farm,
#                    years = 2023,
#                    farms = swift)
# 
# # reverse version of above
# les_data <- region_lesion_sum_data(group = "Region") |> 
#   region_les_graph(lesions = c("dd", "footrot"),
#                    group = "Region",
#                    plot_var = farm,
#                    facet_col = lestype,
#                    years = 2024,
#                    farms = c(swift, "Region"))
# 
# # all farms
# les_data <- region_lesion_sum_data(group = "System") |> 
#   region_les_graph(lesions = "lesion",
#                    group = "System",
#                    plot_var = farm,
#                    facet_col = year,
#                    years = c(2023, 2024),
#                    farms = c(report, "System"))
