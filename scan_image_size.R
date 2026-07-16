library(magick)
library(here)
library(tidyverse)


image_fs <- list.files(here('.'), pattern = '.png|.jpg|.webp',
  recursive = TRUE, full.names = TRUE)

df <- lapply(image_fs, function(f) {
  info <- image_info(image_read(f))
  data.frame(file = f, width = info$width, height = info$height)
}) %>% bind_rows()

large_fs <- df %>%
  filter(width > 1200 | height > 1200)

for (f in large_fs$file) {
  img <- image_read(f)
  img_resized <- image_resize(img, "1200x1200>")
  image_write(img_resized, f)
}
