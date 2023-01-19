# Recap Exercises


# 1. generate a vector rn with 10 random values drawn from a normal distribution
# hint: use rnorm() with mean = 0 and sd  = 1

rn = rnorm(10)

# 2. create a function 'rollDice' that takes as an argument 'n' the number of times
# the dice should be rolled, and that outputs n random values between 1-6

rollDice = function(n = 1){
  sample(1:6, size = n, replace = T)
}

# 3. "roll the dice" 10 times and store the results in a vector rd
rd = rollDice(10)

# 4. combine rn and rd into a dataframe, called 'dat'
dat = data.frame(rn,rd)

# 5. compute the mean of each column and the median of each row
apply(dat,2,mean)
apply(dat,2,median)

# 6. replace all values > 3 in the rd column with a value of 3
dat$rd = ifelse(dat$rd>3,3,dat$rd)
dat$rd[dat$rd>3] = 3

# 7. replicate steps 1-6 1000 times, 
#   at each iteration, compute the sum of the dataframe
#   store the resulting value in a vector if it is larger than 25
#   otherwise store an NA value


# res = rep(NA,100000)
res = c()
# rd_2 = rollDice(10*100000)
# rd_2 = matrix(rd_2,ncol = 10)
# rd_2[rd_2>3] = 3
  
for(i in 1:1000){
  rn = rnorm(10)
  rd = rollDice(10)
  dat = data.frame(rn,rd)
  dat$rd[dat$rd>3] = 3
  s_ = sum(dat)
  # s_ = sum(rn, rd_2[i,])
  if(s_ > 25){
    res = c(res, s_)
    # res[i] = s_
  } else {
    res = c(res, NA)
  }
}

# 8. how many NA's have we generated?
sum(is.na(res))