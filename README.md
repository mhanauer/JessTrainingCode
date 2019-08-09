---
title: "Jess Training Code"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
T-test Power
```{r}
n_power = 20
mean_base = .2
sd_base = .1
mean_follow = .4
sd_follow = .2
FISCH_power = function(){
  base = rnorm(n = n_power, mean = mean_power, sd = sd_power)
  follow = rnorm(n = n_power, mean = mean_follow, sd = sd_follow)
  t_results = t.test(follow, base, paired = TRUE)
  p_results= ifelse(t_results$p.value < .05,1,0)
  p_results
}
```
Now run the results like 500 times (should do like 10,000)
```{r}
reps = 500
power = replicate(reps, FISCH_power())
power = sum(power)/reps
power
```
Here is an example of how to do it with different n's
Now let us do it with differing numbers of n's
```{r}
FISCH_power_n = function(){
  n = c(11,20,30,40)
  base = list()
  follow = list()
  t_results = list()
  for(i in 1:length(n)){
  base[[i]] = rnorm(n[[i]], mean = mean_fisch[1], sd = sd_fisch[1])
  follow[[i]] = rnorm(n[[i]], mean = mean_fisch[2], sd = sd_fisch[2])
  t_results[[i]] = t.test(follow[[i]], base[[i]], paired = TRUE)
  t_results[[i]]= ifelse(t_results[[i]]$p.value< .05,1,0)
  }
  return(t_results)
}
```
Now run the results like 500 times (should do like 10,000) Get the data into a fomrat that has 500 columns and rows represent the results for the different n's
```{r}
reps = 500
power_rep = replicate(reps, FISCH_power_n())
power_unlist= unlist(power_rep)
power_matrix = matrix(power_unlist, ncol = reps, nrow = length(n), byrow = FALSE)
power = apply(power_matrix, 1, sum)/reps
power
```




