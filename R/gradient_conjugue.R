utils::globalVariables(c("Medecins", "Allocations"))

# Chargement des bibliothèques nécessaires
library(ggplot2) # Pour la visualisation des résultats

# 1) Fonction pour résoudre le système linéaire Ax=b avec la méthode du gradient conjugué

#' Gradient Conjugate Method
#'
#' This function implements the conjugate gradient method to solve the linear system Ax=b
#'
#' @param A  Coefficient matrix
#' @param b Right-hand side vector
#' @param tol Tolerance for convergence
#' @param max_iter Maximum number of iterations
#'
#' @return Solution vector
#' @examples
#' # A <- matrix(c(4, 1, 1, 3), nrow=2, byrow=TRUE)
#' # b <- c(1, 2)
#' # gradient_conjugue(A, b) # Résout le système Ax=b
#'
#' @export
gradient_conjugue <- function(A, b, tol=1e-8, max_iter=1000) {
  x <- rep(0, length(b))  # Initialisation du vecteur solution avec des zéros
  r <- b - (A %*% x)      # Calcul du résidu initial (b - Ax)
  p <- r                  # Initialisation de la direction de recherche avec le résidu
  rs_old <- sum(r^2)      # Calcul de la norme du résidu initial

  # Boucle jusqu'à max_iter ou jusqu'à convergence
  for (i in 1:max_iter) {
    Ap <- A %*% p                        # Produit de la matrice A par la direction p
    alpha <- rs_old / sum(p * Ap)        # Calcul du pas alpha
    x <- x + (alpha * p)                 # Mise à jour de la solution x
    r <- r - (alpha * Ap)                # Mise à jour du résidu
    rs_new <- sum(r^2)                   # Calcul de la nouvelle norme du résidu

    # Vérification de la convergence
    if (sqrt(rs_new) < tol) {
      break  # Sortie de la boucle si la convergence est atteinte
    }

    p <- r + ((rs_new / rs_old) * p)     # Mise à jour de la direction
    rs_old <- rs_new                     # Mise à jour de l'ancienne norme
  }
  return(x)  # Retourne le vecteur solution trouvé
}


# 2) Fonction pour générer une matrice A (tridiagonale) symétrique et définie positive

#' Generate Coefficient Matrix for Queue Management
#'
#' This function generates the coefficient matrix A for queue management based on the number of resources (doctors) n
#'
#' @param n Number of resources (doctors)
#'
#' @return Coefficient matrix A
#' @examples
#' # n <- 5
#' # generer_matrice(n) # Génère une matrice A tridiagonale symétrique et définie positive
#'
#' @export
generer_matrice <- function(n) {
  # Création de la diagonale principale avec des 3 pour assurer la positivité définie stricte
  diag_main <- rep(4, n)

  # Création des matrices pour les diagonales supérieure et inférieure
  diag_upper <- matrix(0, nrow=n, ncol=n)
  for (i in 1:(n-1)) {
    diag_upper[i, i+1] <- -1  # Affecte des -1 pour garantir la positivité définie
  }

  diag_lower <- matrix(0, nrow=n, ncol=n)
  for (i in 1:(n-1)) {
    diag_lower[i+1, i] <- -1  # Affecte des -1 pour garantir la positivité définie
  }

  # Création de la matrice tridiagonale A symétrique et définie positive
  A <- diag(diag_main) + diag_upper + diag_lower
  return(A)  # Retourne la matrice tridiagonale A
}


# 3) Fonction pour créer un vecteur source b pour l'arrivée des patients

#' Create Source Vector for Patients' Arrival
#'
#' This function creates a source vector b for patients' arrival rates based on the number of resources (doctors) n
#'
#' @param n Number of resources (doctors)
#' @param arrivee_constante Constant arrival rate for patients
#'
#' @return Vector b representing patients' arrival rates
#' @examples
#' # n <- 5
#' # Créons- un vecteur source b avec un taux d'arrivée constant de 10
#' # creer_vecteur_source(n, arrivee_constante=10)
#'
#' @export
creer_vecteur_source <- function(n, arrivee_constante=10) {
  b <- rep(arrivee_constante, n)  # Vecteur d'arrivées constantes de patients
  return(b)  # Retourne le vecteur b
}


# 4) Fonction pour afficher les résultats

#' Display Results of the Optimization
#'
#' This function displays the solution vector x representing the resource allocation
#'
#' @param x Solution vector representing resource allocation
#'
#' @examples
#' # x <- c(5, 3, 4, 2, 6)
#' # afficher_resultats(x) # Affiche le vecteur solution x représentant les allocations de ressources
#'
#' @export
afficher_resultats <- function(x) {
  cat("Allocations des ressources (Medecins) :", "\n")  # Affiche un titre
  print(x)  # Affiche les allocations trouvées
}


# 5) Fonction pour tracer le graphique des allocations

#' Plot Resource Allocation Graph
#'
#' This function draws a resource allocation graph
#'
#' @param x Solution vector representing resource allocation
#'
#' @examples
#' # x <- c(5, 3, 4, 2, 6)
#' # tracer_graphique(x) # Trace un graphique du vecteur solution x
#'
#' @export
tracer_graphique <- function(x) {

  # Création d'un DataFrame contenant les indices des ressources et leurs allocations
  df <- data.frame(Medecins = seq_along(x),
                   Allocations = x)

  # Création d'un graphique avec ggplot en spécifiant les axes pour le graphique
  ggplot2::ggplot(df, ggplot2::aes(x = Medecins, y = Allocations)) +
    ggplot2::geom_bar(stat = "identity", fill = "steelblue") +  # Crée un graphique à barres
    ggplot2::labs(title = "Allocations des ressources (Medecins)",
                  x = "Medecins",
                  y = "Allocations") +  # Ajoute des labels
    ggplot2::theme_minimal()  # Applique un thème minimal pour un rendu plus esthétique
}
