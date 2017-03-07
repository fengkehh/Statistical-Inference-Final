# README
Keh-Harng Feng  
March 6, 2017  



## Part 1: Simulation Exercise

### Introduction

Exponential distribution is a probability distribution that has a pdf described by the function

$$ 
f(x) = 
\begin{cases}
    \lambda e^{- \lambda x}, & x \ge 0\\
    0, & x\lt 0
\end{cases}
$$
where \lambda is the only parameter of the distribution.  It is often referred to as the *rate*. 

This part of the report uses a computer simulation in R to generate a set of random data following the exponential distribution in order to test the **central limit theorem**  (CLT).

### Data Generation

```r
lambda <- 0.2
```
