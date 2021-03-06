# clear workspace
rm(list = ls())

set.seed(1234567)   # set random seed
N <- 7              # number of units
m <- 2              # number of treated units
sel <- rnorm(N)     # N standard Normal deviates
Tr <- order(sel)    # ordering permutation which orders the deviates
Tr[1:m]             # the first m indexes are treated units


load('../../data/chapter02/GerberGreenBook_Chapter2_HowToRandomize_7treat2.RData')   # load object data1 
N <- nrow(data1)        # number of units
m <- 2                  # number of treated units
sel <- rnorm(N)         # N standard Normal deviates
sel <- order(sel, decreasing = TRUE) # ordering permutation which orders the deviates
data1$Tr <- 0           # create treatment dummy variable in dataset
data1$Tr[sel[1:m]] <- 1 # the observations corresponding to the m largest deviates are assigned to treatment

data1					# display treatment assignment
