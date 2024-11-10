# tests/testthat/test_fonctions.R

# Chargement de la bibliothèque testthat pour effectuer les tests
library(testthat)

# Chargeons notre package gestionMalades
library(gestionMalades)

# Tests pour la fonction gradient_conjugue
test_that("gradient_conjugue resout correctement Ax=b", {

    # Création d'un petit système linéaire connu pour le test
    A <- matrix(c(4, 1, 1, 3), nrow=2, byrow=TRUE)  # Matrice symétrique et définie positive
    b <- c(1, 2)  # Vecteur de termes constants

    # Exécution de la fonction `gradient_conjugue` pour obtenir le résultat
    result <- gradient_conjugue(A, b, tol=1e-8, max_iter=1000)

    # Vérification 1 : Résultat doit être numérique
    # La fonction doit retourner un vecteur numérique
    expect_true(is.numeric(result))

    # Vérification 2 : Le résultat doit avoir la même longueur que le vecteur `b`
    # Si `b` a deux éléments, la solution `result` doit également avoir deux éléments
    expect_length(result, length(b))

    # Vérification 3 : Calcul de l'erreur résiduelle entre `Ax` et `b`
    # On vérifie que le résultat `result` respecte la tolérance définie
    # Ici, `erreur` représente la norme de `Ax - b`, et elle doit être inférieure à `1e-8`
    erreur <- sqrt(sum((A %*% result - b)^2))
    expect_true(erreur < 1e-8)  # La tolérance est définie par votre fonction
  })


# Tests pour la fonction generer_matrice
test_that("generer_matrice genere correctement une matrice tridiagonale symetrique et definie positive", {

    # Test pour n = 3, une petite matrice pour valider la structure
    n <- 3
    A <- generer_matrice(n)  # Appel de la fonction pour générer la matrice

    # Vérification 1 : La matrice doit être de taille nxn
    expect_equal(dim(A), c(n, n))  # Vérifie que la taille est bien 3x3

    # Vérification 2 : La matrice doit être symétrique
    expect_true(all(A == t(A)))  # Vérifie que la matrice est égale à sa transposée

    # Vérification 3 : Les valeurs sur la diagonale principale doivent être égales à 4
    expect_true(all(diag(A) == 4))  # Vérifie que les éléments de la diagonale sont égaux à 4

    # Vérification 4 : Les éléments hors diagonale doivent être égaux à -1
    # La matrice tridiagonale a des -1 sur les diagonales supérieures et inférieures
    for (i in 1:(n-1)) {
      expect_equal(A[i, i+1], -1)  # Vérifie les éléments au-dessus de la diagonale principale
      expect_equal(A[i+1, i], -1)  # Vérifie les éléments en-dessous de la diagonale principale
    }
  })


# Tests pour la fonction creer_vecteur_source
test_that("creer_vecteur_source genere correctement un vecteur source", {

    # Test pour n = 5, un cas avec 5 ressources (médecins)
    n <- 5
    arrivee_constante <- 10  # Taux d'arrivée constant des patients
    b <- creer_vecteur_source(n, arrivee_constante)  # Appel de la fonction pour générer le vecteur

    # Vérification 1 : Le vecteur doit avoir la même longueur que le nombre de ressources (n)
    expect_length(b, n)  # Vérifie que la longueur de b est bien égale à n

    # Vérification 2 : Tous les éléments du vecteur doivent être égaux à la constante d'arrivée
    expect_true(all(b == arrivee_constante))  # Vérifie que tous les éléments de b sont égaux à 10
  })


# Tests pour la fonction afficher_resultats
test_that("afficher_resultats affiche correctement les allocations des ressources", {

    # Création d'un vecteur de résultats fictifs pour les allocations
    resultats <- c(5, 3, 4, 2, 6)  # Exemple de solution d'allocation de ressources

    # Vérification 1 : La fonction doit afficher les résultats sous forme de texte
    # Utilisation de `expect_output` pour vérifier que la sortie est correctement imprimée à la console
    expect_output(afficher_resultats(resultats), "Allocations des ressources (Medecins) :", fixed = TRUE) # Vérifie que le texte d'introduction est affiché
    expect_output(afficher_resultats(resultats), paste(resultats, collapse = " "))  # Vérifie que les valeurs du vecteur sont affichées
  })


# Tests pour la fonction tracer_graphique
test_that("tracer_graphique trace correctement le graphique des allocations", {

    # Création d'un vecteur de résultats fictifs pour les allocations
    resultats <- c(5, 3, 4, 2, 6)  # Exemple de solution d'allocation de ressources

    # Vérification 1 : La fonction ne doit pas retourner d'erreur lorsqu'elle est exécutée
    # On s'assure que le graphique est généré sans problème
    expect_silent(tracer_graphique(resultats))  # Vérifie que l'exécution de la fonction ne produit pas d'erreur

    # Vérification 2 : Vérification du type d'objet retourné
    # On s'attend à ce que le résultat de la fonction soit un objet ggplot (car `ggplot2` est utilisé)
    plot <- tracer_graphique(resultats)  # Génère le graphique
    expect_true(inherits(plot, "gg"))  # Vérifie que le graphique est bien un objet ggplot
  })

