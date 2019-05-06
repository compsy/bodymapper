# Bodymapper visualization
# ------------------------
# Requires png package.
# First run:
# install.packages('png')

INPUT <- "[.481,.103],[.481,.103],[.481,.103],[.481,.103],[.481,.103],[.081,.556],[.081,.556],[.081,.556],[.081,.556],[.081,.556],[.902,.556],[.902,.556],[.902,.556],[.902,.556],[.902,.556],[.902,.556],[.377,.821],[.377,.823],[.377,.825],[.377,.831],[.377,.836],[.381,.840],[.381,.844],[.381,.848],[.377,.853],[.377,.861],[.377,.864],[.377,.868],[.377,.874],[.377,.879],[.377,.885],[.381,.892],[.381,.894],[.381,.896],[.381,.898],[.381,.902],[.377,.905],[.377,.907],[.377,.909],[.373,.913],[.369,.917],[.361,.922],[.352,.926],[.340,.931],[.323,.935],[.319,.935],[.311,.937],[.298,.939],[.277,.939],[.265,.941],[.252,.943],[.244,.943],[.244,.943],[.236,.945],[.231,.945],[.223,.945],[.219,.945],[.219,.945],[.219,.945],[.219,.945],[.219,.945],[.219,.945],[.219,.945],[.694,.749],[.694,.749],[.694,.752],[.694,.756],[.694,.760],[.698,.769],[.698,.777],[.698,.782],[.702,.788],[.702,.792],[.702,.797],[.706,.803],[.711,.808],[.715,.814],[.719,.821],[.723,.829],[.727,.834],[.731,.840],[.736,.846],[.740,.851],[.744,.857],[.744,.862],[.744,.868],[.744,.874],[.748,.877],[.748,.883],[.748,.887],[.748,.892],[.748,.896],[.748,.900],[.748,.904],[.748,.909],[.748,.917],[.748,.920],[.752,.924],[.756,.928],[.761,.931],[.761,.935],[.765,.939],[.769,.943],[.769,.946],[.773,.948],[.773,.948],[.777,.952],[.781,.954],[.786,.956],[.790,.958],[.794,.958],[.798,.959],[.802,.961],[.811,.963],[.811,.963],[.811,.963],[.811,.963],[.811,.963],[.811,.963],[.811,.963],[.811,.963],[.811,.963],[.815,.963],[.815,.963],[.815,.963],[.815,.963],[.815,.963],[.481,.254],[.481,.254],[.481,.254],[.481,.254],[.481,.254],[.481,.254],[.481,.254],[.481,.254],[.481,.254],[.481,.254],[.481,.254],[.481,.254],[.481,.254],[.481,.254],[.481,.254],[.481,.254],[.481,.254],[.481,.254],[.481,.254],[.481,.254],[.481,.254],[.481,.254],[.481,.254],[.481,.254],[.481,.254],[.481,.254],[.481,.254],[.481,.254],[.481,.254],[.481,.254],[.481,.254],[.481,.254],[.481,.254],[.481,.254],[.481,.254],[.481,.254],[.481,.254],[.481,.254],[.481,.254],[.481,.254],[.481,.254],[.481,.254],[.481,.254],[.481,.254],[.481,.254],[.481,.254],[.481,.254],[.481,.254],[.273,.368],[.277,.368],[.277,.368],[.286,.368],[.298,.368],[.315,.368],[.336,.368],[.356,.368],[.386,.368],[.411,.366],[.436,.366],[.461,.366],[.477,.366],[.494,.364],[.506,.364],[.515,.364],[.523,.364],[.536,.364],[.544,.364],[.556,.364],[.569,.364],[.577,.364],[.590,.364],[.602,.364],[.615,.364],[.627,.364],[.640,.366],[.648,.366],[.656,.366],[.665,.366],[.669,.366],[.673,.366],[.677,.366],[.681,.366],[.686,.364],[.690,.364],[.698,.364],[.698,.364],[.702,.364],[.702,.364],[.702,.364],[.702,.364],[.702,.364],[.702,.364],[.702,.364],[.702,.364],[.702,.364],"
COLOR <- '#e57373'
# uncomment the line below for blue color:
# COLOR <- '#64b5f6'

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

new_input <- INPUT
new_input <- gsub('[[]', '', new_input, perl = TRUE)
new_input <- gsub('[]]', '', new_input, perl = TRUE)
new_input <- gsub(',$', '', new_input, perl = TRUE)
new_input <- paste('c(',new_input,')', sep = '')
new_input <- eval(parse(text = new_input))
new_input <- matrix(new_input, ncol = 2, byrow = TRUE)
# print(new_input)
pp <- readPNG("bodymap.png")
if (!DEBUG) {
  png(filename = 'bodymap_output.png',
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
points(newer_input[,1],1 - newer_input[,2], col = COLOR, pch = 20, cex = 0.35) # y coord is flipped
if (!DEBUG) dev.off()
