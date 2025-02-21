library(tidyverse)

dat = read_tsv("data_adj.tsv")
dat6 = read_tsv("data_adj6.tsv", quote = "")


# Regular expression to match numbers with optional minus sign,
# commas or dots as decimal separators, and thousands separators.
# Handles variations like "-123.45", "123,456.78", "-1.234,56"
regex_numeric <- "^-?\\d{1,3}(?:[.,]\\d{3})*(?:[.,]\\d+)?$"
regex_uppercase <- "^[A-Z]+$"
regex_multiplier <- "^\\d+X$"
regex_special <- "^[^a-zA-Z0-9]+$"
regex_any_letter <- "[a-zA-Z]"
regex_any_number <- "[0-9]"

only_uppercase <- function(x) grepl(regex_uppercase, x)
no_lowercase <- function(x) x == toupper(x)
is_multiplier <- function(x) grepl(regex_multiplier, x)
is_special <- function(x) grepl(regex_special, x)
contains_letter <- function(x) grepl(regex_any_letter, x)
contains_number <- function(x) grepl(regex_any_number, x)

count_letters <- function(x) nchar(gsub("[^a-zA-Z]", "", x))
count_numbers <- function(x) nchar(gsub("[^0-9]", "", x))



# convert a string to numeric:
# - anything is accept that contains at least one number and at most one , or . inside the string (otherwise it is not clear where the decimal symbol would be)
# - note: heading/training decimals symbols will be disregarded, because these only would appear if there is a number next to it. In case of two neighboring numbers, the second one is regarded as number behind the decimal symbol
convert_to_numeric <- function(x) {
	x <- gsub(",", ".", x)
	x <- gsub("^[.]+|[.]+$", "", x) # remove head/trail .
	x <- gsub("[^0-9.]", "", x) # Remove non-number characters (except .)
	y = suppressWarnings(as.numeric(x))
	ifelse(is.na(y), NA_character_, x)
}

convert_multi = function(x) {

	if (length(x) > 1) {
		x = sapply(x, convert_to_numeric)
		y = as.numeric(x)
		if (any(y != round(y))) {
			paste0(x, collapse = "")
		} else {
			paste0(x, collapse = ".")
		}
	} else {
		convert_to_numeric(x)
	}
}



multiplier_values <- function(x) {
	as.integer(ifelse(is_multiplier(x),
		gsub("X", "", x), # Remove "X" and convert to numeric
		NA_character_
	))
}


dat7 = dat6 |>
	filter(!is.na(text), text != "", !is_special(text)) |>
	mutate(num = convert_to_numeric(text),
		   only_upper = only_uppercase(text),
		   any_letter = contains_letter(text),
		   any_num = contains_number(text),
		   n_letters = count_letters(text),
		   n_numbers = count_numbers(text),
		   n_total = nchar(text),
		   is_num = !is.na(num) & (n_numbers > (n_total - 3)),
		   is_mult = is_multiplier(text),
		   nchar = nchar(text),
		   no_lower = no_lowercase(text),
		   num_mult = multiplier_values(text),
		   dist_totaal = stringdist::stringdist(tolower(text), "totaal"),
		   dist_omschrijving = stringdist::stringdist(tolower(text), "omschrijving"),
		   dist_korting = stringdist::stringdist(tolower(text), "korting"),
		   dist_bedrag = stringdist::stringdist(tolower(text), "bedrag"),
		   dist_statiegeld = stringdist::stringdist(tolower(text), "statiegeld"))



########
## descriptives

# data data frame with the columns 'left' and 'line_num', and var
# var the color-coding variable, a column in data
plot_receipt = function(data, var, hlines = NULL, vlines = NULL) {
	data$col = data[[var]]
	g = ggplot(data, aes(x = left, y = -line_num)) +
		geom_text(mapping = aes(label = text, color = col), hjust = "left") +
		ggplot2::labs(color = var) +
		theme_minimal()

	if (!is.null(hlines)) {
		g = g + geom_hline(yintercept = -hlines)
	}
	if (!is.null(vlines)) {
		g = g + geom_vline(xintercept = vlines)
	}
	g
}

plot_receipt(dat7, "is_num")
plot_receipt(dat7, "is_mult")



find_kde_peak <- function(x, bandwidth = NULL, grid_size = 1024) {
	# Check if x is numeric
	if (!is.numeric(x)) {
		stop("Input 'x' must be a numeric vector.")
	}

	# Remove NA values
	x <- na.omit(x)

	# If no bandwidth is provided, use the default (Sheather-Jones)
	if (is.null(bandwidth)) {
		bandwidth <- "SJ"  # Could also use "nrd0" (Silverman's rule), "ucv", "bcv", or "SJ-ste"
	}

	# Perform Kernel Density Estimation
	dens <- density(x, bw = bandwidth, n = grid_size)

	# Find the x-value corresponding to the maximum density
	peak_x <- dens$x[which.max(dens$y)]
	peak_y <- max(dens$y)

	# Return a list containing the peak x-value, the density object, and the peak density
	return(list(peak_x = peak_x, density = dens, peak_y = peak_y))
}


# find products and price 'columns'
left_prods = find_kde_peak(dat7$left[dat7$only_upper])$peak_x # column with most upper case is probably 'product' column
left_price = find_kde_peak(dat7$left[dat7$is_num])$peak_x   # column with most numbers is probably 'price' column

# find lines that contain the price data
line_start = dat7$line_num[which.min(dat7$dist_omschrijving)] # row with 'omschrijving' is start row
line_end = dat7$line_num[which.min(dat7$dist_totaal)]         # row with 'total' is end row

plot_receipt(dat7, "is_num", hlines = c(line_start, line_end), vlines = c(left_prods, left_price))
plot_receipt(dat7, "only_upper", hlines = c(line_start, line_end), vlines = c(left_prods, left_price))



# filter those lines
dat8 = dat7 |>
	filter(line_num >= line_start,
		   line_num <= line_end)

# find product rows
prod_lns = dat8 |>
	filter(left > left_prods - 50,
		   left < left_price,
		   dist_statiegeld >= 3, # exclude statiegeld rule
		   nchar >= 3,
		   no_lower & any_letter & n_letters > (n_numbers + 2))

text_per_line = split(prod_lns$text, prod_lns$line_num)

text_lines = sapply(text_per_line, paste, collapse = " ")

prod_ln_nums = as.integer(names(text_lines))

D_prod = data.frame(line_num = prod_ln_nums,
					prod = unname(text_lines))


###############################################
# find prices
###############################################

D_price = dat8 |>
	filter(line_num %in% prod_ln_nums,
		   is_num,
		   left > left_price - 50,
		   left < left_price + 100) |>
	group_by(line_num) |>
	reframe(price = convert_multi(text))

###############################################
# find discout
###############################################

discount_lns = dat8 |>
	filter(dist_korting < 3)
disc_ln_nums = unique(discount_lns$line_num)

D_discount = dat8 |>
	filter(line_num %in% disc_ln_nums,
		   is_num,
		   left > left_price - 50,
		   left < left_price + 100) |>
	group_by(line_num) |>
	reframe(discount = convert_multi(text)) |>
	mutate(line_num = line_num - 1) # assume that it belongs to the product line above

###############################################
# find total
###############################################

left_total = dat8$left[which.min(dat8$dist_totaal)]

total_nums = dat8 |>
	filter(line_num == line_end,
		   left > left_total + 50,
		   is_num) |>
	arrange(left)
D_total = convert_multi(total_nums$text)

# Methods question: how to edit this sum
# asked in slack channel R_help, tagging Mark and Edwin

if (FALSE) {
	# dput(D$price)
	x = c("2.25", "2.99", "2.00", "2.00", "4585", "4874", "9.38", "4.38",
		  "8.58", "4.29", "2.79", "2.49", "4.98", "2.09", "2.59", "3.69"
	)

	# dput(D$discount)
	y = c("0.25", NA, NA, NA, "1885", "5", "3.38", NA, "2.58", "1.29",
		  NA, "1.49", "2.98", "1.09", "1.59", NA)

	total = "44.80"

	library(deductive)
	library(validate)

	correct_typos()

	xint = as.integer(as.numeric(x) * 100)
	yint = as.integer(as.numeric(y) * 100)
	ysel = !is.na(yint)

	df = as.data.frame(t(as.matrix(structure(c(xint, yint[ysel]), names = c(paste0("x", 1:length(x)), paste0("y", which(ysel)))))))

	# voorbeeld Mark vd Loo
	regel <- paste(names(iris[1:3]),collapse="+")
	regel <- paste(regel, "==",names(iris[4]))
	L <- list(parse(text=regel)[[1]])
	v <- do.call(validator, L)

}



###############################################
# find 'aantallen'
###############################################

mult_lns = dat8 |>
	filter(left < left_prods,
		   is_num)

left_mult = find_kde_peak(mult_lns$left)$peak_x   # column with most numbers is probably 'price' column


D_mult = mult_lns |>
	filter(left > left_mult - 20,
		   left < left_mult + 20) |>
	group_by(line_num) |>
	reframe(mult = num[1])


###############################################
# combine data
###############################################

D = D_prod |>
	left_join(D_price, by = "line_num") |>
	left_join(D_mult, by = "line_num") |>
	left_join(D_discount, by = "line_num")
