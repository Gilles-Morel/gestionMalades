
# gestionMalades
<!-- badges: start -->
<!-- badges: end -->


#The goal of gestionMalades is to manage the optimization of queues in hospitals using the conjugate gradient algorithm. The package makes it possible to model the resource management problem, to optimize the allocation of doctors for given number of patients, and visualize the results of resource allocations

## Installation

#You can install the development version of gestionMalades like so: From Github using the devtools package

``` r
# install.packages("devtools")
devtools::install_github("Gilles-Morel/gestionMalades")
```

## Example

#This is a basic example which shows you how to solve a common problem:

``` r
## library(gestionMalades)

# Define the number of resources (doctors)
n <- 5

# Generate the coefficient matrix for queue management
A <- generer_matrice(n)

# Create the source vector representing patient arrival rates
b <- creer_vecteur_source(n, arrivee_constante = 10)

# Solve the system Ax = b to obtain the optimal resource allocation
x <- gradient_conjugue(A, b)

# Display the optimal resource allocations
afficher_resultats(x)

# Visualize resource allocations with a bar plot
tracer_graphique(x)

```

