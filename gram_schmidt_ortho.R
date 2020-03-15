# function that creates matrix from user input ------ 'nxn' -------
matrix_maker <- function(){
  # input number of rows
  x <- as.integer(readline("Number of rows: "))
  # input number of columns
  y <- as.integer(readline("Number of cols: "))
  # declaring null matrix of x rows and y columns
  M <- matrix(0,x,y)
  # creating 2d loop / adding [x,y] elements to matrix
  for(i in 1:y){
    for(j in 1:x){
      # filling the matrix M with values
      # paste0 pozwala na wyświetlanie wartości w wypisanym stringu pod zmiennymi, tj. pod j oraz i
      M[j, i] <- as.integer(readline(paste0("Add [", j, "-row, ", i, "-column] element of matrix: ")))
    }
  }
  # returning the M matrix with values from user input
  return(M)
}

# V matrix is the input matrix / return from matrix_maker function
V <- matrix_maker()

# declaring the number of rows & columns of V matrix
row <- nrow(V)
col <- ncol(V)

# creating U matrix of zeros and size row x col / the same size as V matrix
U <- matrix(0,row,col)

# dot product function
dot_product <- function(A,B){
  # suma jest równa zeru
  sum <- 0
  # przemnożenie kolejnych współczynników wektorów i ich sumowanie
  for(i in 1:length(A)){
    sum <- sum + A[i] * B[i]
  }
  return(sum)
}

# projection operator function
projection <- function(B,A){
  value_up <- dot_product(A,B)
  value_down <- dot_product(A,A)
  exit_value <- (value_up/value_down)*A
  return(exit_value)
}

# ----------------- G - S ORTHOGONALIZATION ------------------------ 
# X i Y to macierze -> V i U

gram_schmidt <- function(X,Y) {
  
  #-------------ORTHOGONALIZATION---------------------------------
  # zadeklarowanie i zainicjowanie zmiennej sum na zero
  sum <- 0
  # length(X[,1]) czyli liczba rzędów pierwszej kolumny macierzy X -> przyszłe V
  # lecz my już pierwszy wektor z X ustawiliśmy na pierwszy wektor z Y, stąd zaczynamy iterację od 2
  # a następnie iterujemy po wszystkich kolumnach, zatem jest ich N-1, bo ten 1 już zużyty był wcześniej
  for(i in 1:length(X[,1])){
    #jeśli i jest wieksze o 1, to dopiero wtedy dokonuję operacji rzutowania
    if(i > 1){
      # zwiększamy naszą sumę, czyli rzutowanie wektora Yn na Xn-1 czyli np. V2 na U1
      sum <- sum + projection(Y[,i],X[,(i-1)])
    }
    # odejmuj od każdego wektora Yn sumę i przypisz ją do wektora Un
    # X - zbiór wektorów ortogonalnych
    X[,i] <- Y[,i] - sum
  }
  # po pierwszej pętli otrzymujemy zbiór wektorów ortogonalnych
  # teraz należy je znormalizować
  
  #------------NORMALIZATION------------------------------------
  # lecimy po wszystkich wektorach, czyli pętla od 1 do len(X[,1])
  for(i in 1:length(X[,1])){
    # każdy wektro X[,i] dzielimy przez normę, czyli długość tego wektora = dot_product(X[,i],X[,i])^0.5
    X[,i] <- X[,i]/(dot_product(X[,i],X[,i]))^0.5
    #każdemu elementowi X[,i], którego element ma wartość NaN, nadaj wartość 0
    X[,i][is.nan(X[,i])] <- 0
  }
  # zwracam ortonormalną macierz, zawierającą wektory jako kolumny tej macierzy
  return(X)
}

#---------------------------- MAIN -------------------------
print(gram_schmidt(U,V))

# --- TEST NUM. STABILITY IN ANY WAY YOU WANT -------------
# i am using testthat library
install.packages("testthat")
library(testthat)

# If erroe doesn't appear than the code is  - OK
# Example from page : https://pl.wikipedia.org/wiki/Ortogonalizacja_Grama-Schmidta
test_that("gram schmidt orto", {
  expect_equal(gram_schmidt(matrix(0,2,2), matrix(c(3,1,2,2),2,2)), matrix(c((1/(sqrt(10)))*3,(1/(sqrt(10)))*1,(1/(sqrt(10)))*(-1),(1/(sqrt(10)))*3),2,2))
})

# The error should appear if the function works properly
test_that("gram schmidt orto", {
  expect_equal(gram_schmidt(matrix(0,2,2),matrix(c(3,1,2,2),2,2)), matrix(c((1/(sqrt(10)))*5,(1/(sqrt(10)))*5,(1/(sqrt(10)))*5,(1/(sqrt(10)))*5),2,2))
})
