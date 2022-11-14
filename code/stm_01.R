# basic information:
Strategies     <- c("No Treatment", "Treatment")     # strategy names
n_age_init     <- 25                                 # age at baseline
n_age_max      <- 55                                 # maximum age of follow up
n_t            <- n_age_max - n_age_init             # time horizon, number of cycles
d_r            <- 0.035                              # equal discount of costs and QALYs by 3%

# Transition probabilities (per cycle)
p_HD    <- 0.005           # probability to die when healthy
p_HS1   <- 0.15          	 # probability to become sick when healthy
p_S1H   <- 0.5           	 # probability to become healthy when sick
p_S1S2  <- 0.105         	 # probability to become sicker when sick
hr_S1   <- 3             	 # hazard ratio of death in sick vs healthy
hr_S2   <- 10            	 # hazard ratio of death in sicker vs healthy

# Cost and utility inputs
c_H     <- 2000            # cost of remaining one cycle in the healthy state
c_S1    <- 4000            # cost of remaining one cycle in the sick state
c_S2    <- 15000           # cost of remaining one cycle in the sicker state
c_Trt   <- 12000           # cost of treatment(per cycle)
c_D     <- 0               # cost of being in the death state
u_H     <- 1               # utility when healthy
u_S1    <- 0.75            # utility when sick
u_S2    <- 0.5             # utility when sicker
u_D     <- 0               # utility when dead
u_Trt   <- 0.95            # utility when being treated

# rate of death in healthy
r_HD    <- - log(1 - p_HD)

# rate of death in sick
r_S1D   <- hr_S1 * r_HD
# rate of death in sicker
r_S2D   <- hr_S2 * r_HD

# probability of death in sick
p_S1D   <- 1 - exp(-r_S1D)
# probability of death in sicker
p_S2D   <- 1 - exp(-r_S2D)

# calculate discount weight for each cycle
v_dwe <- v_dwc <- 1 / (1 + d_r) ^ (0:(n_t-1))  # discount weight (equal discounting is assumed for costs and effects)

v_n  <- c("H", "S1", "S2", "D")               # the 4 states of the model: Healthy (H), Sick (S1), Sicker                                                  (S2), Dead (D)
n_states <- length(v_n)                            # number of health states

#transition probability matrix for NO treatment
m_P <- matrix(data = NA,
              nrow = n_states,
              ncol = n_states,
              dimnames = list(v_n, v_n))
### From Healthy
m_P["H", ]  <- c(1 - (p_HS1 + p_HD), p_HS1, 0, p_HD)
### From Sick
m_P["S1", ]  <- c(p_S1H, 1 - (p_S1H + p_S1S2 + p_S1D), p_S1S2, p_S1D)
### From Sicker
m_P["S2", ]   <- c(0, 0, 1 - p_S2D, p_S2D)
### From Dead
m_P["D", ] <- c(0, 0, 0, 1)

# create empty Markov trace
m_TR <- matrix(data = NA,
               nrow = n_t,
               ncol = n_states,
               dimnames = list(1:n_t, v_n))
