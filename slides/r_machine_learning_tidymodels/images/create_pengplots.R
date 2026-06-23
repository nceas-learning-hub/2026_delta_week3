library(palmerpenguins)
library(tidyverse)

peng_df <- penguins %>% drop_na() %>% filter(species %in% c('Adelie', 'Gentoo'))

ggplot(peng_df, aes(x = bill_depth_mm, y = body_mass_g)) +
  geom_point() +
  theme_minimal() +
  labs(x = 'bill depth (mm)', y = 'body mass (g)')

ggsave('penguins1.jpg', width = 5, height = 3, dpi = 150)

ggplot(peng_df %>% mutate(group = ifelse(species == 'Gentoo', 'A', 'B')),
       aes(x = bill_depth_mm, y = body_mass_g)) +
  geom_point(aes(color = group)) +
  theme_minimal() +
  theme(legend.position = c(.8, .8)) +
  scale_color_manual(values = c('magenta2', 'darkgreen')) +
  labs(x = 'bill depth (mm)', y = 'body mass (g)')

ggsave('penguins2.jpg', width = 5, height = 3, dpi = 150)

ggplot(peng_df,
       aes(x = bill_depth_mm, y = body_mass_g)) +
  geom_point(aes(color = species)) +
  theme_minimal() +
  theme(legend.position = c(.8, .8)) +
  scale_color_manual(values = c('purple', 'darkorange')) +
  labs(x = 'bill depth (mm)', y = 'body mass (g)')

ggsave('penguins3.jpg', width = 5, height = 3, dpi = 150)
