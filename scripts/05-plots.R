####################### Run after 04-projection ################################
################### Need both Male & Female out data ###########################
# Combined plot: Male & Female -------------------------------------------------

df.f <- read_csv(here("out_data", "Female.csv"))
df.m <- read_csv(here("out_data", "Male.csv"))

df.fm <- df.f %>% 
  left_join(df.m, by = "year", suffix = c(".female", ".male")) %>% 
  pivot_longer(cols = 2:3, names_to = "sex", names_prefix = "cases.", values_to = "cases")

# Plot - both sexes
p1 <- ggplot(df.fm, aes(year, cases, colour = sex)) +
  geom_line() +
  ylim(1500, 4500) + scale_x_continuous(breaks = seq(2020,2030,2)) +
  labs(x="Year", y="Number of people with anxiety disorders (thousands)") + 
  theme_bw(base_size = 7) +
  theme(legend.position = c(.11, .9), legend.key.size = unit(0.3, "cm"),
        legend.box.background = element_rect(colour = "grey"))
ggsave("Figure2.tiff", p1, path = "figures", width = 8.5, height = 8.5, 
       units = "cm", dpi = 300)
