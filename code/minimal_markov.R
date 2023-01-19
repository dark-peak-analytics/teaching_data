
# 1 minimal markov model demo
# 3 states: healthy, sick, dead

horizon = 10
costs = c(10, 100, 0)
utilities = c(0.9, 0.5, 0)
discount_rate = 0.035

transition_mat = matrix(
  data = c(
    0.5, 0.3, 0.2,
    0.1, 0.6, 0.3,
    0,     0,   1
  ),
  nrow = 3, 
  byrow = T
)

markov_trace = matrix(
  data = NA, 
  nrow = horizon, 
  ncol = nrow(transition_mat)
  )

markov_trace[1,] <- c(1,0,0)

for(i in 2:horizon){
  markov_trace[i,] <- markov_trace[i-1,] %*% transition_mat
}


# check
apply(markov_trace,1,sum)

# discount rate
discount_weight = 1 / (1 + discount_rate) ^ (0:(horizon-1))

# apply discount rate
markov_trace_discounted = markov_trace * discount_weight

# sum over columns
col_sums = apply(markov_trace_discounted,2,sum)

# costs
costs_by_state = col_sums * costs
sum(col_sums * costs)
# col_sums %*% costs

# utilities
qalys_by_state = col_sums * utilities
sum(qalys_by_state)


# 2 Minimal PSA  
library(gtools)

horizon = 10
costs = c(10, 100, 0)
utilities = c(0.9, 0.5, 0)
discount_rate = 0.035

# modelling uncertainty around transistion probs 
# in the 'sick' state using rdirichlet:
rdirichlet(1, c(10, 60, 30))



transMatGenerator = function(shapes = c(10, 60, 30)){
  require(gtools)
  sick_transitions = rdirichlet(1, shapes)
  transition_mat = matrix(
    data = c(
      0.5, 0.3, 0.2,
      sick_transitions,
      0,     0,   1
    ),
    nrow = 3, 
    byrow = T
  )
  return(transition_mat)
}




n_psa = 5
res_costs = c()
res_qalys = rep(NA,n_psa)

for(j in 1:n_psa){
  
  transition_mat = transMatGenerator(c(10,60,30))
  
  markov_trace = matrix(
    data = NA, 
    nrow = horizon, 
    ncol = nrow(transition_mat)
  )
  
  markov_trace[1,] <- c(1,0,0)
  
  for(i in 2:horizon){
    markov_trace[i,] <- markov_trace[i-1,] %*% transition_mat
  }
  
  # discount rate
  discount_weight = 1 / (1 + discount_rate) ^ (0:(horizon-1))
  
  # apply discount rate
  markov_trace_discounted = markov_trace * discount_weight
  
  # sum over columns
  col_sums = colSums(markov_trace_discounted)
  
  # costs
  costs_by_state = col_sums * costs
  col_sums %*% costs
  res_costs = c(res_costs, sum(col_sums * costs))
  
  # utilities
  qalys_by_state = col_sums * utilities
  res_qalys[j] = sum(qalys_by_state)

}

# results
res_costs
mean(res_costs)
res_qalys
mean(res_qalys)
