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

RESPONSES_FILE <- 'responses_dagboek_kinderen_8_2021-10-12.csv'
QUESTIONNAIRE_DEFINITION_FILE <- 'questionnaire_dagboek_kinderen_8_2021-10-12.csv'
OUTPUT_DIR <- 'output'
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
PRECISION <- 0.001
PLOT_POINTS_OUTSIDE_THE_BODY <- FALSE
# coordinates on the scaled image
BODY_OUTLINE = matrix(c(98,49,
                        54,56,
                        9,94,
                        106,172,
                        173,113,
                        142,49), ncol =2, byrow = TRUE)/240 # 240x240 = dimensions of image


library("png")

getRandomFloat <- function(min, max) {
  return(runif(1) * (max - min) + min)
}

if (!DEBUG) {
  unlink(OUTPUT_DIR, recursive = TRUE, force = TRUE)
  dir.create(OUTPUT_DIR)
  graphics.off()
}


# Given three collinear points p, q, r, the function checks if
# point q lies on line segment 'pr'
# x = index 1, y = index 2
on_segment <- function(p, q, r) {
  if (q[1] <= max(p[1], r[1]) && q[1] >= min(p[1], r[1]) &&
      q[2] <= max(p[2], r[2]) && q[2] >= min(p[2], r[2]))
    return(TRUE)
  return(FALSE)
}

# To find orientation of ordered triplet (p, q, r).
# The function returns following values
# 0 --> p, q and r are collinear
# 1 --> Clockwise
# 2 --> Counterclockwise
orientation <- function(p, q, r) {
  val <- (q[2] - p[2]) * (r[1] - q[1]) - (q[1] - p[1]) * (r[2] - q[2])
  if (abs(val) < PRECISION) return(0) # collinear
  if (val > 0) return(1) # clockwise
  return(2) # counterclockwise
}

# The function that returns true if line segment 'p1q1'
# and 'p2q2' intersect.
do_intersect <- function(p1, q1, p2, q2) {
  # Find the four orientations needed for general and
  # special cases
  o1 <- orientation(p1, q1, p2)
  o2 <- orientation(p1, q1, q2)
  o3 <- orientation(p2, q2, p1)
  o4 <- orientation(p2, q2, q1)

  # General case
  if (abs(o1 - o2) > PRECISION && abs(o3 - o4) > PRECISION)
    return(TRUE)

  # Special Cases
  # p1, q1 and p2 are collinear and p2 lies on segment p1q1
  if (abs(o1) < PRECISION && on_segment(p1, p2, q1)) return(TRUE)

  # p1, q1 and p2 are collinear and q2 lies on segment p1q1
  if (abs(o2) < PRECISION && on_segment(p1, q2, q1)) return(TRUE)

  # p2, q2 and p1 are collinear and p1 lies on segment p2q2
  if (abs(o3) < PRECISION && on_segment(p2, p1, q2)) return(TRUE)

  # p2, q2 and q1 are collinear and q1 lies on segment p2q2
  if (abs(o4) < PRECISION && on_segment(p2, q1, q2)) return(TRUE)

  return(FALSE) # Doesn't fall in any of the above cases
}

# Returns true if the point p lies inside the polygon[] with n vertices
is_inside <- function(polygon, n, p) {
  # There must be at least 3 vertices in polygon[]
  if (n < 3) return(FALSE)

  # Create a point for line segment from p to infinite
  extreme <- c(1000, p[2])

  # Count intersections of the above line with sides of polygon
  count <- 0
  i <- 0
  repeat {
    nxt <- (i+1)%%n

    # Check if the line segment from 'p' to 'extreme' intersects
    # with the line segment from 'polygon[i]' to 'polygon[next]'
    if (do_intersect(polygon[i+1,], polygon[nxt+1,], p, extreme)) {
      # If the point 'p' is collinear with line segment 'i-next',
      # then check if it lies on segment. If it lies, return true,
      # otherwise false
      if (orientation(polygon[i+1,], p, polygon[nxt+1,]) == 0)
        return(on_segment(polygon[i+1,], p, polygon[nxt+1,]))

      count <- count + 1
    }
    i <- nxt
    if (i == 0) {
      break
    }
  }

  # Return true if count is odd, false otherwise
  return(count%%2 == 1)
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
    # Don't plot points outside the body
    if (!PLOT_POINTS_OUTSIDE_THE_BODY && !is_inside(BODY_OUTLINE, dim(BODY_OUTLINE)[1], c(x,y))) next
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
