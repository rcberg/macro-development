olg_lumpsum_tax <- 
  function(a = 0.3 ,
           b = 0.95 ,
           d = 0 , 
           g = 0.03 ,
           k0 = 1,
           L0 = 1,
           N ,
           A0 = 10,
           A_g = 0.03 , 
           alpha_shock =0, 
           beta_shock =0, 
           depr_shock =0, 
           n_shock =0, 
           tech_shock =0, 
           shock_time = N/2 ,
           eps = 0,
           paygo_tax = 0 ,
           seed = 414
  ){
    set.seed(seed)
    n_young <- 
      rep(0,N+1)
    n_young[1] <- 
      L0
    n_old <- 
      rep(0,N+1)
    lifetime_utility_old <- 
      rep(0,N+1)
    c_young <- 
      rep(0,N+1)
    c_old <- 
      rep(0,N+1)
    k <- 
      rep(0,N+1)
    A <- 
      rep(0,N+1)
    A[1] <- 
      A0
    
    k[1] <- 
      k0
    
    y <- 
      A*(k^a)
    
    r <- 
      a*(k^(a-1))
    
    w <- 
      y-r*k
    
    s <- 
      w*b/(1+b)
    
    c_young <- 
      w-s
    n_pop <- g
    lifetime_utility_old <- 
      rep(0,N+1)
    adj_lifetime_utility_old <- 
      rep(0,N+1)
    generation  <-  
      rep(0,N+1)
    old_generation  <-  
      rep(0,N+1)
    tax_amount <-
      rep(0,N+1)
    e <- 
      c(0,rnorm(n=N))
    
    for(t in 2:(N+1)){
      if(t<shock_time){
        alpha  <-  a
        beta  <-  b 
        delta  <-  d
        n  <-  g
        g_a  <-  A_g
        tax_sum  <-  0 
        
      }else{
        alpha  <-  a+alpha_shock
        beta  <-  b+beta_shock
        delta  <-  d+depr_shock
        n  <-  g+n_shock
        g_a  <-  A_g+tech_shock
        tax_sum  <-  paygo_tax
      }
      if(t<=N){
        n_pop <- n
        n_old[t] <- 
          n_young[t-1]
        n_young[t] <- 
          (1+n_pop)*n_old[t]
      }else{
        n_pop=0 
        n_old[t] <- 
          n_young[t-1]
        n_young[t] <-
          0 
      }
      net_inc <- (1-tax_sum)
      e[t] <- sample(x = c(1,-1), size = 1)*eps
      A[t]  <-  (1+g_a)*A[t-1]
      k[t] <- 
        s[t-1]/(1+n_pop) - k[t-1]*delta/(1+n_pop)
      y[t] <- 
        A[t]*k[t]^alpha
      r[t] <- 
        alpha*(A[t]*k[t]^(alpha-1))
      w[t] <- 
        y[t]-r[t]*k[t]
      s[t] <- 
        0.5*(w[t]*beta/(1+beta) + sqrt(w[t]^2 - 4*(1+beta)*((net_inc/(1+r[t]))*eps)^2)/(1+beta))
      c_young[t] <- 
        w[t] - s[t]
      c_old[t] <- 
        (1+r[t])*s[t-1] + net_inc*e[t]
      lifetime_utility_old[t] <- 
        log(c_young[t-1]) + beta*log(c_old[t])
      generation[t]  <-  (t-1)
      old_generation[t]  <-  (t-2)
      
    }
    
    output_df <- 
      data.frame( generation = generation ,
                  old_generation = old_generation ,
                  n_old = n_old ,
                  n_young = n_young ,
                  A = A ,
                  k = k , 
                  y = y , 
                  w = w , 
                  s = s , 
                  r = r ,
                  c_young = c_young , 
                  c_old = c_old ,
                  olds_lifetime_u = lifetime_utility_old,
                  tax_rev = tax_amount*n_young )
    
    return(output_df)
    
  }

tax_sim_f_55 <- 
  function(i,t){
    olg_lumpsum_tax_df <- 
      olg_lumpsum_tax(N=200, paygo_tax = t, A_g = 0, b = 0.55, eps = 3, seed = i)
    
    olg_lumpsum_baseline_df <-  
      olg_lumpsum_tax(N=200, A_g = 0, b = 0.55, eps = 3, seed = i)
    
    sim_df <- 
      data.frame( no_tax_utility = sum(olg_lumpsum_baseline_df$olds_lifetime_u) , 
                  tax_utility = sum(olg_lumpsum_tax_df$olds_lifetime_u))
    return(sim_df)
  }
tax_vary_f_55 <- 
  function(indx){
    tax <- indx/100
    tmp_mat <- t(mapply(i=1:1000, FUN=tax_sim_f_55, t=tax))
    tmp_df <- data.frame(no_tax_utility = unlist(tmp_mat[,1]), tax_utility = unlist(tmp_mat[,2]))
    output_df <- data.frame(diff = mean(tmp_df$tax_utility - tmp_df$no_tax_utility), utility_notax = mean(tmp_df$no_tax_utility), rate = tax)
    return(output_df)
  }
tax_sim_f_75 <- 
  function(i,t){
    olg_lumpsum_tax_df <- 
      olg_lumpsum_tax(N=200, paygo_tax = t, A_g = 0, b = 0.75, eps = 3, seed = i)
    
    olg_lumpsum_baseline_df <-  
      olg_lumpsum_tax(N=200, A_g = 0, b = 0.75, eps = 3, seed = i)
    
    sim_df <- 
      data.frame( no_tax_utility = sum(olg_lumpsum_baseline_df$olds_lifetime_u) , 
                  tax_utility = sum(olg_lumpsum_tax_df$olds_lifetime_u))
    return(sim_df)
  }
tax_vary_f_75 <- 
  function(indx){
    tax <- indx/100
    tmp_mat <- t(mapply(i=1:1000, FUN=tax_sim_f_75, t=tax))
    tmp_df <- data.frame(no_tax_utility = unlist(tmp_mat[,1]), tax_utility = unlist(tmp_mat[,2]))
    output_df <- data.frame(diff = mean(tmp_df$tax_utility - tmp_df$no_tax_utility), utility_notax = mean(tmp_df$no_tax_utility), rate = tax)
    return(output_df)
  }
tax_sim_f_95 <- 
  function(i,t){
    olg_lumpsum_tax_df <- 
      olg_lumpsum_tax(N=200, paygo_tax = t, A_g = 0, b = 0.95, eps = 3, seed = i)
    
    olg_lumpsum_baseline_df <-  
      olg_lumpsum_tax(N=200, A_g = 0, b = 0.95, eps = 3, seed = i)
    
    sim_df <- 
      data.frame( no_tax_utility = sum(olg_lumpsum_baseline_df$olds_lifetime_u) , 
                  tax_utility = sum(olg_lumpsum_tax_df$olds_lifetime_u))
    return(sim_df)
  }
tax_vary_f_95 <- 
  function(indx){
    tax <- indx/100
    tmp_mat <- t(mapply(i=1:1000, FUN=tax_sim_f_95, t=tax))
    tmp_df <- data.frame(no_tax_utility = unlist(tmp_mat[,1]), tax_utility = unlist(tmp_mat[,2]))
    output_df <- data.frame(diff = mean(tmp_df$tax_utility - tmp_df$no_tax_utility), utility_notax = mean(tmp_df$no_tax_utility), rate = tax)
    return(output_df)
  }

library(purrr)
library(furrr)
library(ggplot2)
library(hrbrthemes)

# basic model
#try out basic lumpsum tax with risk
olg_lumpsum_tax_df <- 
  olg_lumpsum_tax(N=200, paygo_tax = 1, A_g = 0, eps = 3)

olg_lumpsum_baseline_df <-  
  olg_lumpsum_tax(N=200, A_g = 0, eps = 3)

# showing utility effect of lumpsum tax with risk
ggplot( ) + 
  geom_line( data = olg_lumpsum_tax_df , 
             aes( x = generation ,
                  y = olds_lifetime_u,
                  color = 'Tax regime') ,
             size=1,
             alpha=0.9
  ) +
  geom_line( data = olg_lumpsum_baseline_df , 
             aes( x = generation ,
                  y = olds_lifetime_u,
                  color = 'Baseline')
  ) + 
  scale_color_manual(values = c("Tax regime"="red",
                                "Baseline"="black"))+
  labs( x = "Time" , 
        y = "Lifetime utility",
        title = "Welfare effect of lump-sum tax",
        color = "Model"
  ) +
  hrbrthemes::theme_ipsum_rc()

plan(multiprocess)

utility_gain_55 <- future_map_dfr(1:100, tax_vary_f_55, .options = furrr_options(seed = NULL))
utility_gain_75 <- future_map_dfr(1:100, tax_vary_f_75, .options = furrr_options(seed = NULL))
utility_gain_95 <- future_map_dfr(1:100, tax_vary_f_95, .options = furrr_options(seed = NULL))
utility_gain_55$b <- rep(0.55, nrow(utility_gain_55))
utility_gain_75$b <- rep(0.75, nrow(utility_gain_75))
utility_gain_95$b <- rep(0.95, nrow(utility_gain_95))

utility_gain_discount_vary <- 
  dplyr::bind_rows(
    utility_gain_55,
    utility_gain_75,
    utility_gain_95
  )

saveRDS(utility_gain_discount_vary, "data/export/soc_insurance_gains_sim_variable_discount_rate.rds")

ggplot(data = utility_gain_discount_vary , 
       aes(x = rate, y = diff, color = as.factor(b))) + 
  geom_line() +
  labs( x = "Tax Rate",
        y = "Utility gain",
        color = "Discount rate") +
  theme_ipsum_rc()
