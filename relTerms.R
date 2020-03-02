relTerms <- function(DTM){

DTM_reduced <- as.matrix(DTM[, terms_to_observe])

counts_per_year <- aggregate(DTM_reduced, by = list(year = lexiscorpus$documents$year), sum)

# give x and y values beautiful names
year <- counts_per_year$year
frequencies <- counts_per_year[, terms_to_observe]

# plot multiple frequencies
matplot(year, frequencies, type = "l")

# add legend to the plot
l <- length(terms_to_observe)
legend('topleft', legend = terms_to_observe, col=1:l, text.col = 1:l, lty = 1:l)  
}
