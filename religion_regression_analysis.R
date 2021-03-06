library(pacman)
p_load( tidyverse , 
        ggplot2 , 
        dplyr , 
        estimatr , 
        usmap )

setwd("D:/Economics/Projects/macro-development")
### THE DATA

data_df = readRDS("protestant_america_project_data.rds") %>%
  mutate( logk = log(k_per_capita) ,
          logw = log(wage_weekly) 
          )

### REGRESSIONS 

## education
# protestant
prot_ed_reg_h = lm_robust( data = data_df , 
             formula = educ ~ prot_imp_rate +
               hispanic_rate +
               log(population) ,
             fixed_effects = ~state*year ,
             clusters = state )

prot_ed_reg_nh = lm_robust( data = data_df , 
                           formula = educ ~ prot_imp_rate +
                             log(population) ,
                           fixed_effects = ~state*year ,
                           clusters = state )

# catholic
cath_ed_reg = lm_robust( data = data_df , 
             formula = educ ~ cath_rate +
               hispanic_rate +
               log(population) ,
             fixed_effects = ~state*year ,
             clusters = state )

## capital income (interest, dividends, rent)
# protestant
prot_k_reg_h = lm_robust( data = data_df , 
             formula = log(k_per_capita) ~ prot_imp_rate +
               hispanic_rate +
               log(population),
             fixed_effects = ~state*year ,
             clusters = state )

prot_k_reg_nh = lm_robust( data = data_df , 
                        formula = log(k_per_capita) ~ prot_imp_rate +
                          log(population),
                        fixed_effects = ~state*year ,
                        clusters = state )

# catholic
cath_k_reg = lm_robust( data = data_df , 
             formula = log(k_per_capita) ~ cath_rate +
               hispanic_rate +
               log(population),
             fixed_effects = ~state*year ,
             clusters = state )

## wages
#protestant
prot_w_reg_h = lm_robust( data = data_df , 
             formula = log(wage_weekly) ~ prot_imp_rate +
               hispanic_rate +
               log(population),
             fixed_effects = ~state*year ,
             clusters = state )

prot_w_reg_nh = lm_robust( data = data_df , 
                        formula = log(wage_weekly) ~ prot_imp_rate +
                          log(population),
                        fixed_effects = ~state*year ,
                        clusters = state )

#catholic
cath_w_reg = lm_robust( data = data_df , 
             formula = log(wage_weekly) ~ cath_rate +
               hispanic_rate +
               log(population),
             fixed_effects = ~state*year ,
             clusters = state )


##protestant table
prot_tbl = data.frame( variable = c( "protestant rate" , 
                                     "protestant rate se" , 
                                     "log population" , 
                                     "log population se" , 
                                     "hispanic rate" , 
                                     "hispanic rate se" ,
                                     "N" ) ,
                       ed_nh = c( prot_ed_reg_nh$coefficients[1] ,
                                  prot_ed_reg_nh$std.error[1] ,
                                  prot_ed_reg_nh$coefficients[2] ,
                                  prot_ed_reg_nh$std.error[2] ,
                                  NA ,
                                  NA  ,
                                  prot_ed_reg_nh$N ) ,
                       ed_h = c( prot_ed_reg_h$coefficients[1] ,
                                 prot_ed_reg_h$std.error[1] ,
                                 prot_ed_reg_h$coefficients[3] ,
                                 prot_ed_reg_h$std.error[3] ,
                                 prot_ed_reg_h$coefficients[2] ,
                                 prot_ed_reg_h$std.error[2] ,
                                 prot_ed_reg_h$N) ,
                       kap_nh = c( prot_k_reg_nh$coefficients[1] ,
                                   prot_k_reg_nh$std.error[1] ,
                                   prot_k_reg_nh$coefficients[2] ,
                                   prot_k_reg_nh$std.error[2] ,
                                  NA ,
                                  NA ,
                                  prot_k_reg_nh$N ) ,
                       kap_h = c( prot_k_reg_h$coefficients[1] ,
                                  prot_k_reg_h$std.error[1] ,
                                  prot_k_reg_h$coefficients[3] ,
                                  prot_k_reg_h$std.error[3] ,
                                  prot_k_reg_h$coefficients[2] ,
                                  prot_k_reg_h$std.error[2] ,
                                  prot_k_reg_h$N ), 
                       wage_nh = c( prot_w_reg_nh$coefficients[1] ,
                                    prot_w_reg_nh$std.error[1] ,
                                    prot_w_reg_nh$coefficients[2] ,
                                    prot_w_reg_nh$std.error[2] ,
                                  NA ,
                                  NA ,
                                  prot_w_reg_nh$N ) ,
                       wage_h = c( prot_w_reg_h$coefficients[1] ,
                                   prot_w_reg_h$std.error[1] ,
                                   prot_w_reg_h$coefficients[3] ,
                                   prot_w_reg_h$std.error[3] ,
                                   prot_w_reg_h$coefficients[2] ,
                                   prot_w_reg_h$std.error[2] ,
                                   prot_w_reg_h$N ) 
                       
                       )

##catholic table
cath_tbl = data.frame( variable = c( "catholic rate" , 
                                     "catholic rate se" , 
                                     "log population" , 
                                     "log population se" , 
                                     "hispanic rate" , 
                                     "hispanic rate se" ,
                                     "N" ) ,
                       ed_h = c( cath_ed_reg$coefficients[1] ,
                                 cath_ed_reg$std.error[1] ,
                                 cath_ed_reg$coefficients[3] ,
                                 cath_ed_reg$std.error[3] ,
                                 cath_ed_reg$coefficients[2] ,
                                 cath_ed_reg$std.error[2] ,
                                 cath_ed_reg$N) ,
                       kap_h = c( cath_k_reg$coefficients[1] ,
                                  cath_k_reg$std.error[1] ,
                                  cath_k_reg$coefficients[3] ,
                                  cath_k_reg$std.error[3] ,
                                  cath_k_reg$coefficients[2] ,
                                  cath_k_reg$std.error[2] ,
                                  cath_k_reg$N ), 
                       wage_h = c( cath_w_reg$coefficients[1] ,
                                   cath_w_reg$std.error[1] ,
                                   cath_w_reg$coefficients[3] ,
                                   cath_w_reg$std.error[3] ,
                                   cath_w_reg$coefficients[2] ,
                                   cath_w_reg$std.error[2] ,
                                   cath_w_reg$N ) 
                       
)

## testing equality 
library(car)
H_edu = lm_robust( data = data_df , 
           formula = educ ~ cath_rate +
             prot_imp_rate +
             log(population),
           fixed_effects = ~state+year ,
           clusters = state )
linearHypothesis( H_edu ,
                  c("cath_rate = prot_imp_rate"))

H_capital = lm_robust( data = data_df , 
           formula = log(k_per_capita) ~ cath_rate +
             prot_imp_rate +
             hispanic_rate +
             cath_rate:hispanic_rate +
             log(population),
           fixed_effects = ~state+year ,
           clusters = state )
linearHypothesis( H_capital ,
                  c("cath_rate = prot_imp_rate"))

H_wage = lm_robust( data = data_df , 
           formula = log(wage_weekly) ~ cath_rate +
             prot_imp_rate +
             hispanic_rate +
             cath_rate:hispanic_rate +
             log(population),
           fixed_effects = ~state+year ,
           clusters = state )
linearHypothesis( H_wage ,
                  c("cath_rate = prot_imp_rate"))

### PLOTS 
protestant_measure_comparison = readRDS( "protestant_measure_comparison.rds")
# we can see that there are issues with the protestant rate, but that these issues are largely deterministic
protestant_measure_comparison %>%
  ggplot( aes( x = comp_protestant , 
               y = protestant ) ) + 
  geom_point() +
  geom_abline( slope = 1 ) +
  geom_smooth(method = 'lm')+
  labs( title = "2010 Longitudinal vs. Cross-Sectional Protestant Adherent Count" , 
        x = "Longitudinal Measure"  , 
        y = "Cross-Sectional Measure" ) +
  theme_bw() +
  theme( text = element_text(size = 15))

protestant_measure_comparison %>%
  ggplot( aes( x = comp_catholic , 
               y = catholic ) ) + 
  geom_point() +
  geom_abline( slope = 1 ) +
  geom_smooth(method = 'lm')+
  labs( title = "2010 Longitudinal vs. Cross-Sectional Catholic Adherent Count" , 
        x = "Longitudinal Measure"  , 
        y = "Cross-Sectional Measure" ) +
  theme_bw() +
  theme( text = element_text(size = 15))

# want to plot residual regression to show the main results on a graph

relig_resid = lm(  prot_imp_rate ~
                     log(population) +
                     as.factor(state) + 
                     as.factor( year ),
                   data = data_df %>%
                     filter( prot_imp_rate != is.na(1) ,
                             educ != is.na(1) 
                             ) 
                   )$resid

y_resid_ed = lm( educ ~
                log(population) +
                as.factor(state) + 
                as.factor( year ),
              data = data_df %>%
                filter( prot_imp_rate != is.na(1),
                        educ != is.na(1) 
                        )
              )$resid


reg_plot_ed = bind_cols( educ = y_resid_ed , relig = relig_resid )

relig_resid = lm(  prot_imp_rate ~
                     log(population) +
                     as.factor(state) + 
                     as.factor( year ),
                   data = data_df %>%
                     filter( prot_imp_rate != is.na(1) 
                     ) 
)$resid

y_resid_k = lm( log(k_per_capita) ~
                   log(population) +
                   as.factor(state) + 
                   as.factor( year ),
                 data = data_df %>%
                   filter( prot_imp_rate != is.na(1)
                   )
)$resid

y_resid_wage = lm( log(wage_weekly) ~
                   log(population) +
                   as.factor(state) + 
                   as.factor( year ),
                 data = data_df %>%
                   filter( prot_imp_rate != is.na(1)
                   )
)$resid

reg_plot_k = bind_cols( k = y_resid_k , relig = relig_resid )
reg_plot_w = bind_cols( wage = y_resid_wage , relig = relig_resid )


ggplot( data = reg_plot_ed , 
        aes( x = relig ,
             y = educ ) ) +
  geom_point( ) +
  geom_smooth( method = 'lm' ) +
  labs( x = "Protestant Share Residual" , 
        y = "Educational Attainment Residual" ,
        title = "Protestant Share of the Population and Educational Attainment" ,
        caption = "(Data are residuals from a regression of each variable on state and year fixed effects.)"
        ) +
  theme_bw()

ggplot( data = reg_plot_k , 
        aes( x = relig ,
             y = k ) ) +
  geom_point( ) +
  geom_smooth( method = 'lm' ) +
  labs( x = "Protestant Share Residual" , 
        y = "(Log) Per Capita Capital Income Residual" ,
        title = "Protestant Share of the Population and (Log) Per Capita Capital Income" ,
        caption = "(Data are residuals from a regression of each variable on state and year fixed effects.)"
  ) +
  theme_bw()

ggplot( data = reg_plot_w , 
        aes( x = relig ,
             y = wage ) ) +
  geom_point( ) +
  geom_smooth( method = 'lm' ) +
  labs( x = "Protestant Share Residual" , 
        y = "(Log) Avg. Weekly Wage Residual" ,
        title = "Protestant Share of the Population and (Log) Avg Weekly Wage" ,
        caption = "(Data are residuals from a regression of each variable on state and year fixed effects.)"
  ) +
  theme_bw()

data_df %>%
  filter( year == 2010) %>%
  ggplot( aes( x = log(population) ,
               y = log(wage_weekly) 
               )
          ) + 
  geom_point() + 
  geom_smooth( method = 'lm' ) + 
  theme_bw()

## protestant settlement

plot_usmap( regions = "counties" ,
            data = data_df %>%
              filter( year == 2010),
            values = "cath_rate"   ) +
  labs( title = "Catholic Share by County (2010)",
        fill = "% of Population" , 
        caption = "Sources: US Census Bureau ; Relig. Cong. Membership Files . (Grey indicates missing or incomplete data)" ) +
  scale_fill_continuous( low = "red" , 
                         high = "white" ) +
  theme( text = element_text( size = 15))


## hispanic 

county_pop_with_hispanic = readRDS("county_pop_with_hispanic.rds")

plot_usmap( regions = "counties" ,
            data = county_pop_with_hispanic %>%
              filter( year == 2010),
            values = "hispanic_rate"   ) +
  labs( title = "Hispanic Share of Population by County (2010)",
        fill = "% of Population" , 
        caption = "Sources: US Census Bureau (Grey indicates missing or incomplete data)" ) +
  scale_fill_continuous( low = "dark red" , 
                         high = "white" ) +
  theme( text = element_text( size = 15))
