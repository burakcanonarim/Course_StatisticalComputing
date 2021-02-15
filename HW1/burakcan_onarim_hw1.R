MADcalculator <- function(my.vector) {
  mad.value = median(abs(my.vector - median(my.vector)))
  return(mad.value)
}
my.list1 = c(4,2,5,3,0,4)
MADcalculator(my.list1)




CorrelationAnalyzer <- function(vector1, vector2) {
  plot(vector1, vector2, main = "Scatterplot of These Vectors",
       xlab = "First vector", ylab = "Second Vector")
  
  pcc = (sum((vector1 - mean(vector1))*(vector2 - mean(vector2)))) / (sqrt((sum((vector1 - mean(vector1))^2))*(sum((vector2 - mean(vector2))^2))))
  print(pcc)
    if (pcc < 0.5){
    print("Correlation is WEAK")
  } else if (pcc >= 0.5 && pcc < 0.8) {
    print("Correlation is GOOD ENOUGH")
  } else {
    print("Correlation is STRONG")
  }
}
my.list2 = c(17,13,12,15,16,14,16,16,18,19)
my.list3 = c(94,73,59,80,93,85,66,79,77,91)
CorrelationAnalyzer(my.list2, my.list3)





MissingValueReplacer <- function(my.vector) {
  my.vector[is.na(my.vector)] <- mean(my.vector, na.rm = T)
  round(my.vector, digits = 1)
}
my.list4 <- c(2,5,9,8,NA,7,10,4,NA)
MissingValueReplacer(my.list4)

