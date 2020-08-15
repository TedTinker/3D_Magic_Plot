library(jsonlite)
library(ggplot2)
library(wordcloud)
library(dplyr)

# "AllPrintings" from mtgjson.com.
mtg_cards <- fromJSON(readLines("AllCards.json"))

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

subtype_corpus <- c()

# Iterate over JSON file to make a matrix
i = 1

for (card in mtg_cards) {
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
    for (subtype in card$subtypes) {
      subtype_corpus <- c(subtype_corpus, subtype)
    }
    if(!is.null(card$power)) {
      mtg_matrix[i, "Power"] <- card$power
    }
    if(!is.null(card$toughness)) {
      mtg_matrix[i, "Toughness"] <- card$toughness
    }
  }
  i = i + 1
}

# Remove empty rows
mtg_matrix <- mtg_matrix[rowSums(is.na(mtg_matrix)) != ncol(mtg_matrix), ]

# If there's a power/toughness, but it doesn't read as a number, mark it down as irregular
irregular_powers <- c()
irregular_toughnesses <- c()

add_weird_power_toughness <- function (card) {
  if(!is.na(card["Power"])) {
    if(is.na(as.numeric(card["Power"]))) {
      irregular_powers <<- c(irregular_powers, card["Power"])
    }
  }
  if(!is.na(card["Toughness"])) {
    if(is.na(as.numeric(card["Toughness"]))) {
      irregular_toughnesses <<- c(irregular_toughnesses, card["Toughness"])
    }
  }
}

f <- apply(mtg_matrix, 1, function(card) add_weird_power_toughness(card))

# Make it a dataframe
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








# HISTOGRAM OF IRREGULAR POWERS
power_table <- table(irregular_powers)
power_table <- sort(power_table, decreasing = TRUE)
toughness_table <- table(irregular_toughnesses)
toughness_table <- toughness_table[c(names(power_table),"*+1", "7-*")]

par(mfrow = c(1,2))

barplot(
  power_table,
  ylim = c(0,150),
  main = "Counts of Irregular Power",
  col = "red",
  ylab = "Count",
  xlab = "Power"
)
barplot(
  toughness_table,
  ylim = c(0,150),
  main = "Counts of Irregular Toughness",
  col = "blue",
  xlab = "Toughness"
)







# Check if card is monocolored or colorless.
monocolor <- function(card) {
  color_count <- sum(card[,c("White", "Blue", "Black", "Red", "Green")])
  return(color_count == 1 || color_count == 0)
}

# If a card is monocolored/colorless, return its color
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
    return("DarkGreen") # Dark Green looked better to me. 
  }
  return("gray")
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
cpt_table <- table(mtg_monocolor[,c("Power", "Toughness", "Color")])
cpt_df <- as.data.frame(cpt_table)

# Dummy-data so make a little legend. 
legend_dummies <- data.frame(
  c(14,14,14,14,14,14,14,14,14,14),
  c(10,9.5,8.8,8,3,3,3,3,3,3),
  c("Black", "Black", "Black", "Black", "White", "Blue", "Black", "Red", "DarkGreen", "gray"),
  c(1,10,100,1000,50,50,50,50,50,50)
)
names(legend_dummies) <- c("Power", "Toughness", "Color", "Freq")
cpt_df <- rbind(legend_dummies, cpt_df)

# Take logarithm of frequencies.
cpt_df$Power <- as.numeric(as.character(cpt_df$Power))
cpt_df$Toughness <- as.numeric(as.character(cpt_df$Toughness))
cpt_df$log_Freq <- log(cpt_df$Freq + 5)
cpt_df$log_Freq[cpt_df$Freq == 0] <- -Inf

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
  }
  edit_p <- edit_p * .25
  edit_t <- edit_t * .25
  cpt_df[i,"Power"] = cpt_df[i,"Power"] + edit_p
  cpt_df[i,"Toughness"] = cpt_df[i,"Toughness"] + edit_t
}

cpt_df$Outline <- rep("NULL", nrow(cpt_df))

for (i in 1:nrow(cpt_df)) {
  if(cpt_df[i,"Color"] == "White") {
    cpt_df[i,"Outline"] <- "gray"
  } else if(cpt_df[i,"Color"] == "Blue") {
    cpt_df[i,"Outline"] <- "Blue"
  } else if(cpt_df[i,"Color"] == "Black") {
    cpt_df[i,"Outline"] <- "Black"
  } else if(cpt_df[i,"Color"] == "Red") {
    cpt_df[i,"Outline"] <- "Red"
  } else if(cpt_df[i,"Color"] == "DarkGreen"){
    cpt_df[i,"Outline"] <- "DarkGreen"
  } else {
    cpt_df[i,"Outline"] <- "gray"
  }
}

# I'd like it to draw the gray circles last
cpt_df <- rbind(
  cpt_df[cpt_df$Color == "White",],
  cpt_df[cpt_df$Color == "Blue",],
  cpt_df[cpt_df$Color == "Black",],
  cpt_df[cpt_df$Color == "Red",],
  cpt_df[cpt_df$Color == "DarkGreen",],
  cpt_df[cpt_df$Color == "gray",]
)

##### 2D plot of power and toughness vs color!
ggplot(cpt_df) +
  aes(x = Power, y = Toughness) +
  geom_point(
    pch = 21,
    fill = cpt_df$Color, 
    colour = cpt_df$Outline,
    size = 1.4*cpt_df$log_Freq,
    alpha = .6) +
  ggtitle("Power/Toughness Frequency by Color") +
  scale_x_continuous(breaks = seq(0, 16, by = 1)) +
  scale_y_continuous(breaks = seq(0, 16, by = 1)) + 
  geom_abline(slope=1, intercept=0, col = "gray") 