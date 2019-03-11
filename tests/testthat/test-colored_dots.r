context("test-colored_dots")
set.seed(42)

rows_picking <- c(1:5, 25:30)
dend <- (iris[rows_picking,-5]*10) %>% dist %>% hclust %>% as.dendrogram 
odd_numbers <- rows_picking %% 2
cols <- c("gold", "grey")[odd_numbers+1]
# scale is off
plot(dend)
colored_bars(cols, dend)
# move and scale a bit
plot(dend)
colored_bars(cols, dend, y_shift = -1,
             rowLabels = "Odd\n numbers")
# Now let's cut the tree and add that info to the plot:
k2 <- cutree(dend, k = 2)
cols2 <- c("#0082CE", "#CC476B")[k2]
a <- colored_dots(cbind(cols2, cols), dend, 
                  rowLabels = c("2 clusters", "Odd numbers"), horiz = F)
b <- colored_dots(cbind(cols2, cols), dend, 
                  rowLabels = c("2 clusters", "Odd numbers"), horiz = T)

b <- colored_dots(cbind(cols2, cols), dend, 
                  rowLabels = c("2 clusters", "Odd numbers"), circ = T)

#This test really only gives back colors :/

test_that("colored dots works", {
  testthat::expect_known_hash(a, "030df7d4f8")
})
