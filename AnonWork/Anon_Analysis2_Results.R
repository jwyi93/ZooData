## Results from Anon_Analysis2.R
## Corey Jackson, School of Information Studies, Syracuse Unviersity

## RESULTS 1
Call:
glm(formula = FutureSession ~ Annotations + Anon_Annotations + 
    Time, family = "binomial", data = Anon_Identified_First_Future)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.9565  -0.7561  -0.6912   0.9608   1.9367  

Coefficients:
                   Estimate Std. Error z value Pr(>|z|)    
(Intercept)      -1.375e+00  4.364e-02 -31.510  < 2e-16 ***
Annotations      -3.764e-03  9.246e-04  -4.070 4.69e-05 ***
Anon_Annotations  2.058e-02  4.393e-03   4.684 2.81e-06 ***
Time              2.827e-04  3.212e-05   8.801  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 5723.6  on 5019  degrees of freedom
Residual deviance: 5563.1  on 5016  degrees of freedom
AIC: 5571.1

Number of Fisher Scoring iterations: 5

# For every one unit change in Annotations, the log odds of future sessions (versus drop-out) decreases by 3.76
# For every one unit change in Anonymous Annotations, the log odds of future sessions (versus drop-out) increases by 2.06
# For every one unit change in Time(Seconds), the log odds of future sessions (versus drop-out) increases by 2.83


## RESULTS 2 
# Predicting future work based on first session activities. With a focus on anonymous work

# Outcome: Total Time

Call:
lm(formula = Total_Time ~ Anon_Annotations + Annotations + Time, 
    data = Anon_Identified_First_Future)

Residuals:
     Min       1Q   Median       3Q      Max 
-1996662  -352872  -299912  -272076 16439124 

Coefficients:
                  Estimate Std. Error t value Pr(>|t|)    
(Intercept)      269819.18   29047.42   9.289  < 2e-16 ***
Anon_Annotations   -311.53    1722.82  -0.181  0.85651    
Annotations        -156.24     646.12  -0.242  0.80894    
Time                 74.46      23.10   3.223  0.00128 ** 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1649000 on 5016 degrees of freedom
Multiple R-squared:  0.004664,	Adjusted R-squared:  0.004069 
F-statistic: 7.835 on 3 and 5016 DF,  p-value: 3.248e-05

# Outcome:  Total Sessions
Call:
lm(formula = Total_sessions ~ Anon_Annotations + Annotations + 
    Time, data = Anon_Identified_First_Future)

Residuals:
    Min      1Q  Median      3Q     Max 
-10.215  -0.887  -0.509  -0.331 247.202 

Coefficients:
                   Estimate Std. Error t value Pr(>|t|)    
(Intercept)       1.330e+00  9.876e-02  13.468  < 2e-16 ***
Anon_Annotations -8.622e-03  5.857e-03  -1.472    0.141    
Annotations       8.025e-04  2.197e-03   0.365    0.715    
Time              5.081e-04  7.854e-05   6.469 1.08e-10 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 5.606 on 5016 degrees of freedom
Multiple R-squared:  0.02225,	Adjusted R-squared:  0.02167 
F-statistic: 38.05 on 3 and 5016 DF,  p-value: < 2.2e-16

# Outcome: Total Annotations
Call:
lm(formula = Additional_Annotations ~ Anon_Annotations + Annotations + 
    Time, data = Anon_Identified_First_Future)

Residuals:
    Min      1Q  Median      3Q     Max 
 -917.3   -40.8    -7.7     8.5 13563.2 

Coefficients:
                  Estimate Std. Error t value Pr(>|t|)    
(Intercept)      -12.35847    5.20580  -2.374  0.01763 *  
Anon_Annotations  -0.80535    0.30876  -2.608  0.00912 ** 
Annotations        0.79967    0.11580   6.906 5.61e-12 ***
Time               0.02470    0.00414   5.966 2.59e-09 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 295.5 on 5016 degrees of freedom
Multiple R-squared:  0.06921,	Adjusted R-squared:  0.06866 
F-statistic: 124.3 on 3 and 5016 DF,  p-value: < 2.2e-16

### Most of this makes sense. All outcomes are impacted by the predictor time in the first session. 
