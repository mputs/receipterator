library(tidyverse)
dat6 = read_tsv("data_adj6.tsv", quote = "")

dat6 = read_tsv("data_adj6_cropped.tsv", quote = "")


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


## sum
D2 = D |>
	mutate(discount_num = as.numeric(discount),
		   discount_num = ifelse(is.na(discount_num), 0 , discount_num),
		   price_num = as.numeric(price))

sum(D2$price_num - D2$discount_num)

###############################################
# editing
###############################################

library(deductive)
library(validate)

D2 = D |>
	mutate(id = 1:n(),
		   p_id = paste0("p", id),
		   d_id = paste0("d", id))

D3 = rbind(
	D2 |>
		select(price, p_id) |>
		rename(x = price, val = p_id) |>
		mutate(type = "price"),
	D2 |>
		select(discount, d_id) |>
		filter(!is.na(discount)) |>
		rename(x = discount, val = d_id) |>
		mutate(type = "discount"))




dat = structure(data.frame(t(D3$x)), names = D3$val)
dat$y = D_total

eq1 = paste0(paste(D3$val[D3$type == "price"], collapse = "+"), "-", paste(D3$val[D3$type == "discount"], collapse = "-"), " == y")
eqk = paste0(D2$p_id, ">", D2$d_id)[!is.na(D2$discount)]

eqs = c(eq1, eqk)

L = lapply(eqs, function(eq) {
	parse(text=eq)[[1]]
})
v <- do.call(validator, L)

dat_num = dat
dat_num[] = lapply(dat, as.numeric)
dat_num[] = lapply(dat_num, function(x) as.integer(x * 100))

dat_num

dput(dat_num)

reprex::reprex({
library(deductive)
library(validate)
dat = structure(list(p1 = 225L, p2 = 299L, p3 = 200L, p4 = 200L, p5 = 458500L,
					 p6 = 487400L, p7 = 938L, p8 = 438L, p9 = 858L, p10 = 429L,
					 p11 = 279L, p12 = 249L, p13 = 498L, p14 = 209L, p15 = 259L,
					 p16 = 369L, d1 = 25L, d5 = 188500L, d6 = 500L, d7 = 338L,
					 d9 = 258L, d10 = 129L, d12 = 149L, d13 = 298L, d14 = 109L,
					 d15 = 159L, y = 4480L), row.names = 1L, class = "data.frame")
eqs = c("p1+p2+p3+p4+p5+p6+p7+p8+p9+p10+p11+p12+p13+p14+p15+p16-d1-d5-d6-d7-d9-d10-d12-d13-d14-d15 == y",
  "p1>d1", "p5>d5", "p6>d6", "p7>d7", "p9>d9", "p10>d10", "p12>d12",
  "p13>d13", "p14>d14", "p15>d15")

L = lapply(eqs, function(eq) {
	parse(text=eq)[[1]]
})
v <- do.call(validator, L)

res = correct_typos(dat, v, eps = 1e-3, maxdist = "a")
identical(res, dat)
})
