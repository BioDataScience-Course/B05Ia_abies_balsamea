# Vérifications de abies_notebook.qmd
abies <- parse_rmd("../../abies_notebook.qmd",
  allow_incomplete = TRUE, parse_yaml = TRUE)

test_that("Le bloc-notes est-il compilé en un fichier final HTML ?", {
  expect_true(is_rendered("abies_notebook.qmd"))
  # La version compilée HTML du carnet de notes est introuvable
  # Vous devez créer un rendu de votre bloc-notes Quarto (bouton 'Rendu')
  # Vérifiez aussi que ce rendu se réalise sans erreur, sinon, lisez le message
  # qui s'affiche dans l'onglet 'Travaux' et corrigez ce qui ne va pas dans
  # votre document avant de réaliser à nouveau un rendu HTML.
  # IL EST TRÈS IMPORTANT QUE VOTRE DOCUMENT COMPILE ! C'est tout de même le but
  # de votre analyse que d'obtenir le document final HTML.

  expect_true(is_rendered_current("abies_notebook.qmd"))
  # La version compilée HTML du document Quarto existe, mais elle est ancienne
  # Vous avez modifié le document Quarto après avoir réalisé le rendu.
  # La version finale HTML n'est sans doute pas à jour. Recompilez la dernière
  # version de votre bloc-notes en cliquant sur le bouton 'Rendu' et vérifiez
  # que la conversion se fait sans erreur. Sinon, corrigez et regénérez le HTML.
})

test_that("La structure du document est-elle conservée ?", {
  expect_true(all(c("Introduction et but", "Matériel et méthodes",
    "Résultats", "Étude descriptive", "Modélisation",
    "Discussion & conclusion", "Références")
    %in% (rmd_node_sections(abies) |> unlist() |> unique())))
  # Les sections (titres) attendues du bloc-notes ne sont pas toutes présentes
  # Ce test échoue si vous avez modifié la structure du document, un ou
  # plusieurs titres indispensables par rapport aux exercices ont disparu ou ont
  # été modifié. Vérifiez la structure du document par rapport à la version
  # d'origine dans le dépôt "template" du document (lien au début du fichier
  # README.md).

  expect_true(all(c("setup", "import", "plot1", "plot2", "plot2comment",
    "plot3", "plot3comment", "plot4", "plot4comment", "logheight", "vb",
    "gompertz", "weibull", "chartnls", "aic", "aiccomment")
    %in% rmd_node_label(abies)))
  # Un ou plusieurs labels de chunks nécessaires à l'évaluation manquent
  # Ce test échoue si vous avez modifié la structure du document, un ou
  # plusieurs chunks indispensables par rapport aux exercices sont introuvables.
  # Vérifiez la structure du document par rapport à la version d'origine dans
  # le dépôt "template" du document (lien au début du fichier README.md).

  expect_true(any(duplicated(rmd_node_label(abies))))
  # Un ou plusieurs labels de chunks sont dupliqués
  # Les labels de chunks doivent absolument être uniques. Vous ne pouvez pas
  # avoir deux chunks qui portent le même label. Vérifiez et modifiez le label
  # dupliqué pour respecter cette règle. Comme les chunks et leurs labels sont
  # imposés dans ce document cadré, cette situation ne devrait pas se produire.
  # Vous avez peut-être involontairement dupliqué une partie du document ?
})

test_that("L'entête YAML a-t-il été complété ?", {
  expect_true(abies[[1]]$author != "___")
  expect_true(!grepl("__", abies[[1]]$author))
  expect_true(grepl("^[^_]....+", abies[[1]]$author))
  # Le nom d'auteur n'est pas complété ou de manière incorrecte dans l'entête
  # Vous devez indiquer votre nom dans l'entête YAML à la place de "___" et
  # éliminer les caractères '_' par la même occasion.

  expect_true(grepl("[a-z]", abies[[1]]$author))
  # Aucune lettre minuscule n'est trouvée dans le nom d'auteur
  # Avez-vous bien complété le champ 'author' dans l'entête YAML ?
  # Vous ne pouvez pas écrire votre nom tout en majuscules. Utilisez une
  # majuscule en début de nom et de prénom, et des minuscules ensuite.

  expect_true(grepl("[A-Z]", abies[[1]]$author))
  # Aucune lettre majuscule n'est trouvée dans le nom d'auteur
  # Avez-vous bien complété le champ 'author' dans l'entête YAML ?
  # Vous ne pouvez pas écrire votre nom tout en minuscules. Utilisez une
  # majuscule en début de nom et de prénom, et des minuscules ensuite.
})

test_that("Chunks 'import' : importation et filtre des données", {
  expect_true(is_identical_to_ref("import", "names"))
  # Les colonnes dans le tableau `abies` importé ne sont pas celles attendues
  # Votre jeu de données de départ n'est pas correct. Ce test échoue si vous
  # n'avez pas bien rempli le code du chunk 'import'.

  expect_true(is_identical_to_ref("import", "classes"))
  # La nature des variables (classe) dans le tableau `abies` est incorrecte
  # Vérifiez le chunk d'importation des données `import`.

  expect_true(is_identical_to_ref("import", "nrow"))
  # Le nombre de lignes dans le tableau `abies` est incorrect
  # Le filtre sur les lignes n'est pas correct. Relisez la consigne pour 
  # appliquer le filtre souhaité sur l'altitude au quel les arbres sont mesurés.
})

test_that("Chunks 'plot1', 'plot2', 'plot2comment' : description graphique des données", {
  expect_true(is_identical_to_ref("plot1"))
  # Le graphique combiné de deux histogrammes par le chunk 'plot1' n'est pas 
  # celui attendu. Avez-vous utilisé un label en français pour l'axe Y des 
  # deux graphiques ?
  
  expect_true(is_identical_to_ref("plot2"))
  # Le nuage de points produit par le chunk 'plot2' n'est pas celui attendu.
  # Lisez bien la consigne et corrigez l'erreur. La transparence des points est
  # réalisée en utilisant alpha = 0.5.

  expect_true(is_identical_to_ref("plot2comment"))
  # L'interprétation de la description graphique des données est (partiellement) 
  # fausse dans le chunk 'plot2comment'
  # Vous devez cocher les phrases qui décrivent les graphiques et la table d'un
  # 'x' entre les crochets [] -> [x]. Ensuite, vous devez recompiler la version
  # HTML du bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les
  # résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Chunks 'plot3', 'plot3comment' : transformation double-log", {
  expect_true(is_identical_to_ref("plot3"))
  # Le nuage de points produit par le chunk 'plot3' n'est pas celui attendu.
  # Lisez bien la consigne et corrigez l'erreur. Vous devez transformer à la
  # fois le diamètre et la hauteur avec la fonction log().
  
  expect_true(is_identical_to_ref("plot3comment"))
  # L'interprétation de la description graphique des données est (partiellement) 
  # fausse dans le chunk 'plot3comment'
  # Vous devez cochez les phrases qui décrivent les graphiques et la table d'un
  # 'x' entre les crochets [] -> [x]. Ensuite, vous devez recompiler la version
  # HTML du bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les
  # résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Chunks 'plot4', 'plot4comment' : transformation log pour la hauteur seulement", {
  expect_true(is_identical_to_ref("plot4"))
  # Le nuage de points produit par le chunk 'plot4' n'est pas celui attendu.
  # Lisez bien la consigne et corrigez l'erreur. Vous devez seulement
  # transformer la hauteur en log() dans ce graphique et laisser le diamètre tel
  # quel.
  
  expect_true(is_identical_to_ref("plot4comment"))
  # L'interprétation de la description graphique des données est (partiellement) 
  # fausse dans le chunk 'plot3comment'
  # Vous devez cocher les phrases qui décrivent les graphiques et la table d'un
  # 'x' entre les crochets [] -> [x]. Ensuite, vous devez recompiler la version
  # HTML du bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les
  # résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Chunks 'vb', 'gompertz', 'weibull' & 'chartnls' : modèles non-linéaires", {
  expect_true(is_identical_to_ref("vb"))
  # Le modèle de von Bertalanffy n'est pas celui attendu. 
  # Lisez bien la consigne et corrigez l'erreur. Le modèle à utiliser ici est
  # asymptotique et est le plus général des trois ( à 3 paramètres). Vérifiez
  # que vous travaillez bien avec logheight et diameter.
  
  expect_true(is_identical_to_ref("gompertz"))
  # Le modèle de Gompertz  n'est pas celui attendu. 
  # Lisez bien la consigne et corrigez l'erreur. Vérifiez que vous modéliser
  # bien logheight en fonction de diameter.
  
  expect_true(is_identical_to_ref("weibull"))
  # Le modèle de Weibull  n'est pas celui attendu. 
  # Lisez bien la consigne et corrigez l'erreur. Utilisez-vous le modèle de
  # Weibull self-start pour prédire logheight en fonction de diameter ?
  
  expect_true(is_identical_to_ref("chartnls"))
  # Le nuage de points produit par le chunk 'chartnls' n'est pas celui
  # attendu.
  # Lisez bien la consigne et corrigez l'erreur. Assurez-vous d'avoir 
  # des labels en français. Les trois modèles doivent être présents sur le 
  # même graphique. Le label pour la couleur doit être 'Modèles non linéaires'.
})

test_that("Chunks 'aic', 'aiccomment', Comparaison des modèles", {
  expect_true(is_identical_to_ref("aic", "names"))
  # Les colonnes dans le tableau `aic` ne sont pas celles attendues.
  # Vous devez obtenir un seul tableau comtenant les valeurs d'Akaike des trois
  # modèles.

  expect_true(is_identical_to_ref("import", "nrow"))
  # Le nombre de lignes dans le tableau du critère d'Akaike est incorrect
  # Ce tableau doit contenir trois lignes, une pour chaque modèle.

  expect_true(is_identical_to_ref("aiccomment"))
  # La comparaison des trois modèles est (partiellement) fausse
  # Vous devez cocher les phrases qui décrivent le modèle d'un 'x' entre les
  # crochets [] -> [x]. Ensuite, vous devez recompiler la version HTML du
  # bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Le code pour l'équation paramétrée du modèle du meilleur modèle est-il correct ?", {
  expect_true(rmd_select(abies, by_section("Modélisation")) |>
      as_document() |> is_display_param_equation("vb"))
  # Le code pour générer l'équation paramétrée du meilleur modèle est incorrect. 
  # Vous avez appris à faire cela dans plusieurs projets, dont le projet
  # B04Ia_23M_lungcap. Vérifiez comment l'équation se présente dans le 
  # document final généré avec le bouton ('Rendu').
})

test_that("La partie discussion et conclusion est-elle remplie ?", {
  expect_true(!(rmd_select(abies, by_section("Discussion & conclusion")) |>
      as_document() |> grepl("...Votre discussion à la place de ce texte...",
        x = _, fixed = TRUE) |> any()))
  # La discussion et la conclusion ne sont pas faites
  # Remplacez "...Votre discussion à la place de ce texte..." par vos phrases de
  # commentaires libres (à noter que le contenu de cette section n'est pas
  # évalué automatiquement, mais il le sera par vos enseignants).
})
