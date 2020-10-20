olg_lumpsum_tax <- 
  function(a = 0.3 ,
           b = 0.95 ,
           d = 0 , 
           g = 0.03 ,
           k0 = 1,
           L0 = 1,
           N ,
           A0 = 1,
           A_g = 0.03 , 
           alpha_shock =0, 
           beta_shock =0, 
           depr_shock =0, 
           n_shock =0, 
           tech_shock =0, 
           shock_time = N/2 , 
           paygo_tax = 0 
  ){
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
    n_pop=g
    lifetime_utility_old <- 
      rep(0,N+1)
    adj_lifetime_utility_old <- 
      rep(0,N+1)
    generation = 
      rep(0,N+1)
    old_generation = 
      rep(0,N+1)
    tax_amount <-
      rep(0,N+1)
    
    for(t in 2:(N+1)){
      if(t<shock_time){
        alpha = a
        beta = b 
        delta = d
        n = g
        g_a = A_g
        tax_sum = 0 
        
      }else{
        alpha = a+alpha_shock
        beta = b+beta_shock
        delta = d+depr_shock
        n = g+n_shock
        g_a = A_g+tech_shock
        tax_sum = paygo_tax
      }
      if(t<=N){
        n_pop = n
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
      tax_amount[t] <- tax_sum
      A[t] = (1+g_a)*A[t-1]
      k[t] <- 
        s[t-1]/(1+n_pop) - k[t-1]*delta/(1+n_pop)
      y[t] <- 
        A[t]*k[t]^alpha
      r[t] <- 
        alpha*(A[t]*k[t]^(alpha-1))
      w[t] <- 
        y[t]-r[t]*k[t]
      s[t] <- 
        w[t]*beta/(1+beta) - tax_sum*(1+n+beta*(1+r[t]))/((1+beta)*(1+r[t]))
      c_young[t] <- 
        w[t]- s[t] - tax_sum
      c_old[t] <- 
        (1+r[t])*s[t-1] + (1+n_pop)*tax_sum
      lifetime_utility_old[t] <- 
        log(c_young[t-1]) + beta*log(c_old[t])
      generation[t] = (t-1)
      old_generation[t] = (t-2)
      
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

olg_lumpsum_tax_df <- 
  olg_lumpsum_tax(N=200, a = 0.1 , paygo_tax = 0.5, g=0.5 , A_g = 0, A0=10)
olg_lumpsum_baseline_df <-  
  olg_lumpsum_tax(N=200, a = 0.1 , g=0.5 , A_g = 0, A0=10)

#library(ggplot2)
## showing utility effect of tax
#ggplot( ) + 
#  geom_line( data = olg_lumpsum_tax_df[90:(nrow(olg_capital_tax_df)-70),] , 
#             aes( x = generation ,
#                  y = olds_lifetime_u,
#                  color = 'Tax regime') ,
#             size=1,
#             alpha=0.9
#  ) +
#  geom_line( data = olg_lumpsum_baseline_df[90:(nrow(olg_capital_tax_df)-70),] , 
#             aes( x = generation ,
#                  y = olds_lifetime_u,
#                  color = 'Baseline')
#  ) + 
#  scale_color_manual(values = c("Tax regime"="red",
#                                "Baseline"="black"))+
#  labs( x = "Time" , 
#        y = "Lifetime utility",
#        title = "Welfare effect of lump-sum tax",
#        color = "Model"
#  ) +
#  hrbrthemes::theme_ipsum_rc()

olg_capital_tax <- 
  function(a = 0.3 ,
           b = 0.95 ,
           d = 0 , 
           g = 0.03 ,
           k0 = 1,
           L0 = 1,
           N ,
           A0 = 10,
           A_g = 0 , 
           alpha_shock =0, 
           beta_shock =0, 
           depr_shock =0, 
           n_shock =0, 
           tech_shock =0, 
           shock_time = N/2 , 
           cap_tax = 0 
  ){
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
    n_pop=g
    lifetime_utility_old <- 
      rep(0,N+1)
    adj_lifetime_utility_old <- 
      rep(0,N+1)
    generation = 
      rep(0,N+1)
    old_generation = 
      rep(0,N+1)
    tax_amount <-
      rep(0,N+1)
    
    for(t in 2:(N+1)){
      if(t<shock_time){
        alpha = a
        beta = b 
        delta = d
        n = g
        g_a = A_g
        tax_sum = 0 
        
      }else{
        alpha = a+alpha_shock
        beta = b+beta_shock
        delta = d+depr_shock
        n = g+n_shock
        g_a = A_g+tech_shock
        tax_sum = cap_tax
      }
      if(t<=N){
        n_pop = n
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
      tax_amount[t] <- tax_sum
      A[t] = (1+g_a)*A[t-1]
      k[t] <- 
        s[t-1]/(1+n_pop) - k[t-1]*(delta)/(1+n_pop)
      y[t] <- 
        A[t]*k[t]^alpha
      r[t] <- 
        (1-tax_sum)*alpha*A[t]*(k[t]^(alpha-1))
      w[t] <- 
        y[t]-alpha*A[t]*(k[t]^(alpha))
      s[t] <- 
        w[t]*beta/(1+beta) - tax_sum*alpha*A[t]*(k[t]^(alpha))/((1+beta)*(1+r[t]))
      c_young[t] <- 
        w[t] - s[t]
      c_old[t] <- 
        (1+r[t])*s[t-1] + tax_sum*alpha*A[t]*(k[t]^(alpha))
      lifetime_utility_old[t] <- 
        log(c_young[t-1]) + beta*log(c_old[t])
      generation[t] = (t-1)
      old_generation[t] = (t-2)
      
    }
    
    
    olg_optimal_capital_tax_function <- 
      function(n,b,a){
        y = ((1+n)*(1+b)-n*b*((1-a)/a))/((1+n)*(1+b)-n/(1+n))
        return(y)
      }
    
    tax_solve <- 
      mapply( olg_optimal_capital_tax_function, n=g,b=b,a=a , SIMPLIFY = T)
    
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
                  tax_rev = tax_amount*alpha*A*(k^alpha) ,
                  tax_optim = 
                    rep(
                      tax_solve, 
                      length(generation)
                    ) 
      )
    
    return(output_df)
    
  }

olg_capital_tax_df <- 
  olg_capital_tax(
    N=200 , a = 0.25 , g=0.01 , b = 0.96 , cap_tax = 0.99
  )
olg_capital_baseline_df <- 
  olg_capital_tax(
    N=200, a = 0.25 , g=0.01 , b = 0.96
  )

#ggplot( ) + 
#  geom_line( data = olg_capital_tax_df[90:(nrow(olg_capital_tax_df)-70),] , 
#             aes( x = generation ,
#                  y = olds_lifetime_u,
#                  color = 'Tax regime') ,
#             size=1,
#             alpha=0.9
#  ) +
#  geom_line( data = olg_capital_baseline_df[90:(nrow(olg_capital_tax_df)-70),] , 
#             aes( x = generation ,
#                  y = olds_lifetime_u,
#                  color = 'Baseline')
#  ) + 
#  scale_color_manual(values = c("Tax regime"="red",
#                                "Baseline"="black"))+
#  labs( x = "Time" , 
#        y = "Lifetime utility",
#        title = "Welfare effect of capital tax",
#        color = "Model"
#  ) +
#  hrbrthemes::theme_ipsum_rc()