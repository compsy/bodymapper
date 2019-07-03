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

RESPONSES_FILE <- '/Users/ando/owncloud/vault/IKIA/Prototype\ testing\ april\ 2019/responses_demo_2019-05-02.csv'
QUESTIONNAIRE_DEFINITION_FILE <- '/Users/ando/owncloud/vault/IKIA/Prototype\ testing\ april\ 2019/questionnaire_demo_2019-05-02.csv'
OUTPUT_DIR <- '/Users/ando/owncloud/vault/IKIA/Prototype\ testing\ april\ 2019/drawings'
if (length(list.files(OUTPUT_DIR, "\\.csv$")) != 0) {
  stop('The OUTPUT_DIR contains data (.csv files). Please set the OUTPUT_DIR to a directory that doesn\'t contain .csv files.')
}

COLORS <- c('#e57373', '#64b5f6')

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

drawing_ids <- function(questionnaire_definition) {
  return(questionnaire[questionnaire$type %in% 'drawing', 'question_id'])
}

responses <- read.csv2(RESPONSES_FILE, stringsAsFactors = FALSE, na.strings = c("", "NA"))
questionnaire <- read.csv2(QUESTIONNAIRE_DEFINITION_FILE, stringsAsFactors = FALSE, na.strings = c("", "NA"))

quest_drawing_ids <- drawing_ids(questionnaire)
for (i in 1:dim(responses)[1]) {
  response <- responses[i,]
  if (is.na(response$completed_at)) next
  drawing_idx <- 0
  for (qid in quest_drawing_ids) {
    plot_bodymap(response[[qid]], paste(response$filled_out_by_id, '_', qid, '.png', sep = ''), COLORS[1 + (drawing_idx %% length(COLORS))])
    drawing_idx <- drawing_idx + 1
  }
}

cat('All done!\n')
