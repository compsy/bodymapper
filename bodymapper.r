# Bodymapper visualization
# ------------------------
# Requires png package.
# First run:
# install.packages('png')
rm(list = ls(pos = '.GlobalEnv', all.names = TRUE), pos = '.GlobalEnv')

packages <- c('png')

lapply(packages, function(pkg)  {
  if (!(pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
  if (!(pkg %in% 'gplots')) { # Might conflict with ggplots
    library(pkg, character.only = TRUE)
  }
})

INPUT_FILE <- './responses_ikia_2019-05-13.csv'
OUTPUT_DIR <- './output'
COLOR_STRONGER <- '#e57373'
COLOR_WEAKER <- '#64b5f6'

DEBUG <- FALSE
FACTOR <- 3
RADIUS <- 0.027
DENSITY <- 40
WIDTH <- 258
HEIGHT <- 536
SCALEFACTOR <- 2.22

library("png")

getRandomFloat <- function(min, max) {
  return(runif(1) * (max - min) + min)
}

if (!DEBUG) {
  unlink(OUTPUT_DIR, recursive = TRUE, force = TRUE)
  dir.create(OUTPUT_DIR)
  graphics.off()
}

plot_bodymap <- function(input_data, filename, color) {
  if (is.na(input_data)) return()
  new_input <- input_data
  new_input <- gsub('[[]', '', new_input, perl = TRUE)
  new_input <- gsub('[]]', '', new_input, perl = TRUE)
  new_input <- gsub(',$', '', new_input, perl = TRUE)
  new_input <- paste('c(',new_input,')', sep = '')
  new_input <- eval(parse(text = new_input))
  new_input <- matrix(new_input, ncol = 2, byrow = TRUE)

  pp <- readPNG("bodymap.png")
  if (!DEBUG) {
    png(filename = paste(OUTPUT_DIR , '/', filename, sep = ''),
        width = WIDTH*FACTOR,
        height = HEIGHT*FACTOR,
        pointsize = 20)
  }
  newer_input <- NULL
  for (i in 1:nrow(new_input)) {
    x <- new_input[i,1]
    y <- new_input[i,2]
    for (j in 1:DENSITY) {
      angle <- getRandomFloat(0, pi * 2);
      cradius <- getRandomFloat(0, RADIUS);
      newer_input <- c(newer_input, x + cradius * cos(angle) * SCALEFACTOR, y + cradius * sin(angle))
    }
  }
  newer_input <- matrix(newer_input, ncol = 2, byrow = TRUE)
  plot.new()
  rasterImage(pp,0,0,1,1)
  points(newer_input[,1],1 - newer_input[,2], col = color, pch = 20, cex = 0.35) # y coord is flipped
  if (!DEBUG) dev.off()
}

responses <- read.csv2(INPUT_FILE, stringsAsFactors = FALSE, na.strings = c("", "NA"))

for (i in 1:dim(responses)[1]) {
  response <- responses[i,]
  if (is.na(response$completed_at)) next
  plot_bodymap(response$v3, paste(response$filled_out_by_id, '_sterker.png', sep = ''), COLOR_STRONGER)
  plot_bodymap(response$v4, paste(response$filled_out_by_id, '_zwakker.png', sep = ''), COLOR_WEAKER)
}

cat('All done!\n')
