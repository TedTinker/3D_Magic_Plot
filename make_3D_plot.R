library(jsonlite)
library(dplyr)

# "AllPrintings" from mtgjson.com.
mtg_cards <- fromJSON("AllCards.json")

columns_of_interest = c(
  "Name",
  "White",
  "Blue",
  "Black",
  "Red",
  "Green",
  "Mana Cost",
  "Type",
  "Power",
  "Toughness")

rows = length(mtg_cards)
columns = length(columns_of_interest)
mtg_matrix <- matrix(ncol = columns, nrow = rows)
colnames(mtg_matrix) <- columns_of_interest

i = 1

for (card in mtg_cards) {
  # Skip the "joke" sets
  if(!"UGL" %in% card$printings && !"UNH" %in% card$printings && !"UST" %in% card$printings) {
    mtg_matrix[i,"Name"] <- card$name
    id <- card$colorIdentity
    if("W" %in% id) {
      mtg_matrix[i,"White"] <- 1
    } else {
      mtg_matrix[i,"White"] <- 0
    }
    if("U" %in% id) {
      mtg_matrix[i,"Blue"] <- 1
    } else {
      mtg_matrix[i,"Blue"] <- 0
    }
    if("B" %in% id) {
      mtg_matrix[i,"Black"] <- 1
    } else {
      mtg_matrix[i,"Black"] <- 0
    }
    if("R" %in% id) {
      mtg_matrix[i,"Red"] <- 1
    } else {
      mtg_matrix[i,"Red"] <- 0
    }
    if("G" %in% id) {
      mtg_matrix[i,"Green"] <- 1
    } else {
      mtg_matrix[i,"Green"] <- 0
    }
    mtg_matrix[i, "Mana Cost"] <- as.integer(card$convertedManaCost)
    mtg_matrix[i, "Type"] <- card$types[1]
    if(!is.null(card$power)) {
      mtg_matrix[i, "Power"] <- card$power
    }
    if(!is.null(card$toughness)) {
      mtg_matrix[i, "Toughness"] <- card$toughness
    }
  }
  i = i + 1
}

mtg_matrix <- mtg_matrix[rowSums(is.na(mtg_matrix)) != ncol(mtg_matrix), ]

mtg_df <- data.frame(mtg_matrix, stringsAsFactors = FALSE)
mtg_df$White <- as.numeric(as.character(mtg_df$White))
mtg_df$Blue <- as.numeric(as.character(mtg_df$Blue))
mtg_df$Black <- as.numeric(as.character(mtg_df$Black))
mtg_df$Red <- as.numeric(as.character(mtg_df$Red))
mtg_df$Green <- as.numeric(as.character(mtg_df$Green))
mtg_df$Mana.Cost <- as.numeric(as.character(mtg_df$Mana.Cost))
mtg_df$Power <- as.numeric(as.character(mtg_df$Power))
mtg_df$Toughness <- as.numeric(as.character(mtg_df$Toughness))
mtg_df$Times.Printed <- as.numeric(as.character(mtg_df$Times.Printed))
sapply(mtg_df, mode)

### Make new dataset for 3D plotting!

# Check if card is monocolored or colorless.
monocolor <- function(card) {
  color_count <- sum(card[,c("White", "Blue", "Black", "Red", "Green")])
  return(color_count == 1 || color_count == 0)
}

# If a card is monocolored/colorless, return its color ("Yellow" for colorless).
which_color <- function(card) {
  if(card$White == 1) {
    return("White")
  }
  if(card$Blue == 1) {
    return("Blue")
  }
  if(card$Black == 1) {
    return("Black")
  }
  if(card$Red == 1) {
    return("Red")
  }
  if(card$Green == 1) {
    return("DarkGreen")
  }
  return("Yellow")
}

# List of monocolor/colorless-indexes.
is_it_monocolor <- c()
for (i in 1:nrow(mtg_df)) {
  is_it_monocolor <- c(is_it_monocolor, monocolor(mtg_df[i,]))
}

# New dataset of only monocolor/colorless cards, labeled with color.
mtg_monocolor <- mtg_df[is_it_monocolor,]
which_monocolor <- c()
for (i in 1:nrow(mtg_monocolor)) {
  which_monocolor <- c(which_monocolor, which_color(mtg_monocolor[i,]))
}
mtg_monocolor$Color <- which_monocolor

# Table the data to plot frequencies.
cpt_table <- table(mtg_monocolor[,c("Power", "Toughness", "Mana.Cost", "Color")])
cpt_df <- as.data.frame(cpt_table)

# Dummy-data so make a little legend. 
legend_dummies <- data.frame(
  c(15,15,15,15),
  c(15,15,15,15),
  c(5,4.8,4.5,4.1),
  c("Yellow", "Yellow", "Yellow", "Yellow"),
  c(1,5,50,500)
)
names(legend_dummies) <- c("Power", "Toughness", "Mana.Cost", "Color", "Freq")
cpt_df <- rbind(legend_dummies, cpt_df)

# Take logarithm of frequencies.
cpt_df$Power <- as.numeric(as.character(cpt_df$Power))
cpt_df$Toughness <- as.numeric(as.character(cpt_df$Toughness))
cpt_df$Mana.Cost <- as.numeric(as.character(cpt_df$Mana.Cost))
extra <- 2
cpt_df$log_Freq <- log(cpt_df$Freq + extra)
cpt_df$log_Freq[cpt_df$Freq == 0] <- 0

# Bump power/toughness by color to make pentagons. 
for (i in 1:nrow(cpt_df)) {
  if(cpt_df[i,"Color"] == "White") {
    edit_p <- 0
    edit_t <- 1
  } else if(cpt_df[i,"Color"] == "Blue") {
    edit_p <- .95
    edit_t <- .3
  } else if(cpt_df[i,"Color"] == "Black") {
    edit_p <- .6
    edit_t <- -.8
  } else if(cpt_df[i,"Color"] == "Red") {
    edit_p <- -.6
    edit_t <- -.8
  } else if(cpt_df[i,"Color"] == "DarkGreen"){
    edit_p <- -.95
    edit_t <- .3
  } else {
    edit_p <- 0
    edit_t <- 0
  }
  edit_p <- edit_p * .25
  edit_t <- edit_t * .25
  cpt_df[i,"Power"] = cpt_df[i,"Power"] + edit_p
  cpt_df[i,"Toughness"] = cpt_df[i,"Toughness"] + edit_t
}

cpt_df <- rbind(
    cpt_df[cpt_df$Color == "White",],
    cpt_df[cpt_df$Color == "Blue",],
    cpt_df[cpt_df$Color == "Black",],
    cpt_df[cpt_df$Color == "Red",],
    cpt_df[cpt_df$Color == "DarkGreen",],
    cpt_df[cpt_df$Color == "Yellow",]
)

##### Now plot it in 3D
library(rgl)

# Nice axes-function from http://www.sthda.com/english/wiki/a-complete-guide-to-3d-visualization-device-system-in-r-r-software-and-data-visualization
rgl_add_axes <- function(x, y, z, axis.cols = c("grey", "grey", "grey"),
                         xlab = "", ylab="", zlab="", show.plane = TRUE, 
                         show.bbox = FALSE, bbox.col = c("#333377","black"))
{ 
  
  lim <- function(x){c(0, max(abs(x))) * 1.1}
  # Add axes
  xlim <- lim(x); ylim <- lim(y); zlim <- lim(z)
  rgl.lines(xlim, c(0, 0), c(0, 0), color = axis.cols[1])
  rgl.lines(c(0, 0), ylim, c(0, 0), color = axis.cols[2])
  rgl.lines(c(0, 0), c(0, 0), zlim, color = axis.cols[3])
  
  # Add a point at the end of each axes to specify the direction
  axes <- rbind(c(xlim[2], 0, 0), c(0, ylim[2], 0), 
                c(0, 0, zlim[2]))
  rgl.points(axes, color = "black", size = 3)
  
  # Add axis labels
  rgl.texts(axes, text = c(xlab, ylab, zlab), color = "black",
            adj = c(0.5, -0.8), size = 2)
}


open3d()

rgl_add_axes(
  cpt_df$Power, 
  cpt_df$Toughness, 
  cpt_df$Mana.Cost, 
  xlab = "Power", 
  ylab = "Toughness", 
  zlab = "Mana Cost",
  show.plane = FALSE,
  axis.cols = c("Yellow", "Yellow", "Black")
)

axis3d('x', pos=c( NA, 0, 0 ), col = "Yellow")
axis3d('y', pos=c( 0, NA, 0 ), col = "Yellow")
axis3d('z', pos=c( 0, 0, NA ), col = "Black")

for (i in 1:length(cpt_df$Power)) {
  if(cpt_df$log_Freq[i] != 0) {
    shade3d( 
      translate3d(
        x = cpt_df$Power[i],
        y = cpt_df$Toughness[i],
        z = cpt_df$Mana.Cost[i],
        obj = scale3d(
          x = .03*cpt_df$log_Freq[i],
          y = .03*cpt_df$log_Freq[i],
          z = .03*cpt_df$log_Freq[i],
          obj = icosahedron3d(col = cpt_df$Color[i])
        )
      ) 
    )
  }
}

### Export 3D model as PLY-file
writePLY(
  "RGL.ply", 
  withColors = TRUE, 
  format = "little_endian",
  pointRadius = .001
  ) 


