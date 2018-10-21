
  /*============================================================*/
  /* Générer un mot aléatoire de longueur comprise entre 1 et L */
  /* L’insérer dans un tableau Tab non ordonné                  */
  /*  L’insérer dans la structure s’il n’existe pas             */
  /*============================================================*/
   SOIENT
       Tete , Maillon , Suiv , Prec , P DES LISTEBI DE ARB DE CHAINES ;
       Tab UN TABLEAU ( 1000 ) DE CHAINES ;
       N DES ARB DE CHAINES ;
       Erreur , Stop UN BOOLEEN ;
       Maxi , I , J , L , Long DES ENTIERS ;
       Mot UNE CHAINE ;
       Recherche_maillon , Insert_arb , Sup_maillon , Sup_noeud , Existe , Insert_maillon , Inordre : ACTION ;
       Presentation , Recherche_sup : ACTION ;
      
   DEBUT
       APPEL Presentation ;
       L := 31 ;
       Stop := FAUX ;
       ALLOUER ( Tete ) ;
       Tete := NIL ;
       Maxi := 50 ;
       POUR I := 1 , Maxi
           Long := ALEANOMBRE ( L ) + 1 ;
           SI ( Long <> 0 )
               Mot := ALEACHAINE ( Long ) ;
               AFF_ELEMENT ( Tab [ I ] , Mot ) ;
               ECRIRE ( '* MOT:  ' , Mot ) ;
               ECRIRE ( '* TAILLE:  ' , LONGCHAINE ( Mot ) ) ;
               ECRIRE ( '=========================================' ) ;
               APPEL Recherche_maillon ( Tete , Mot , Maillon , Suiv , Prec ) ;
               SI ( Maillon = NIL )
                   APPEL Insert_maillon ( Tete , Mot , Maillon , Suiv , Prec )
               SINON
                   APPEL Insert_arb ( Maillon , Mot , Erreur ) ;
                   SI ( Erreur = VRAI )
                       ECRIRE ( 'INSERTION IMPOSSIBLE : CHAINE DEJA PRESENTE' ) ;
                       Erreur := FAUX ;
                      
                   FSI ;
                  
               FSI ;
              
           SINON
               I := I - 1
           FSI ;
          
       FINPOUR ;
       P := Tete ;
       ECRIRE ( '-------------------------------------------------------------------------------------------- ' ) ;
       ECRIRE ( '      RANGEMENT DES MOTS GENERES DANS LA STRUCTURE (par ordre lexicographique) :' ) ;
       ECRIRE ( '--------------------------------------------------------------------------------------------' ) ;
       TANTQUE ( ( NON Stop ) ET ( P <> NIL ) )
           ECRIRE ( '-------> Maillon de taille :' , LONGCHAINE ( INFO ( VALEUR ( P ) ) ) ) ;
           APPEL Inordre ( VALEUR ( P ) ) ;
           ECRIRE ( '=====================================================' ) ;
           P := SUIVANT ( P ) ;
           SI ( P = Tete )
               Stop := VRAI
           FSI ;
          
       FINTANTQUE ;
      /*                                                                                            */
      /* SUPPRESSION: nous allons parcourir le tableau dans le sens inverse dans le but d'accelerer */
      /*                la suppression en ayant un seul cas possible: le suppression de fueilles    */
      /*                                                                                            */
       ECRIRE ( '' ) ;
       ECRIRE ( '-------------------------------------------------------------------------------------------- ' ) ;
       ECRIRE ( '                       SUPPRESSION DES MOTS DE LA STRUCTURE:' ) ;
       ECRIRE ( '--------------------------------------------------------------------------------------------' ) ;
       ECRIRE ( '' ) ;
       ECRIRE ( '=====================================================' ) ;
       J := Maxi ;
       POUR I := 1 , Maxi
           Mot := ELEMENT ( Tab [ J ] ) ;
           ECRIRE ( 'MOT:  ' , Mot ) ;
           ECRIRE ( 'TAILLE:  ' , LONGCHAINE ( Mot ) ) ;
           APPEL Recherche_sup ( Tete , Mot , Maillon , Suiv , Prec ) ;
           APPEL Sup_noeud ( Maillon , Mot , Erreur ) ;
           SI ( Erreur = VRAI )
               ECRIRE ( '---> MOT DEJA SUPPRIME ' ) ;
               Erreur := FAUX ;
              
           FSI ;
           SI ( VALEUR ( Maillon ) = NIL )
               APPEL Sup_maillon ( Maillon , Tete ) ;
              
           FSI ;
           J := J - 1 ;
           ECRIRE ( '=====================================================' ) ;
          
       FINPOUR ;
      /* VERIFICATION QUE LA STRUCTURE EST VIDE */
       SI ( Tete = NIL )
           ECRIRE ( '' ) ;
           ECRIRE ( '        ******* STRUCTURE BIEN VIDE ******' ) ;
           ECRIRE ( '' )
       FSI ;
      
   FIN
  /******************************************************************************/
  /* On lui donne le noeud à inserer                                            */
  /* ERREUR pour confirmer si l'insertion s'est bien déroulée                   */
  /******************************************************************************/
   ACTION Insert_arb ( Mail , Moot , Error )
   SOIENT
       Mail UNE LISTEBI DE ARB DE CHAINES ;
       Noeud UN ARB DE CHAINE ;
       Res , Father DES ARB DE CHAINES ;
       Moot UNE CHAINE ;
       Error UN BOOLEEN ;
      
   DEBUT
       Error := FAUX ;
       CREERNOEUD ( Noeud ) ;
       AFF_INFO ( Noeud , Moot ) ;
       SI ( VALEUR ( Mail ) = NIL )
           AFF_VAL ( Mail , Noeud ) ;
          
       SINON
           APPEL Existe ( VALEUR ( Mail ) , Father , Res , Moot ) ;
           SI ( Res <> NIL )
               Error := VRAI ;
              
           SINON
               SI ( INFO ( Noeud ) < INFO ( Father ) )
                   AFF_FG ( Father , Noeud ) ;
                  
               SINON
                   AFF_FD ( Father , Noeud ) ;
                  
               FSI ;
              
           FSI ;
          
       FSI ;
      
   FIN
  /*=============================================================*/
  /*Recherche Q correspondant au maillon contenant l'arbre ayant */
  /*des chaines de longueur égale à celle de ch                  */
  /* Q = NIL : Elle renvoie le suivant et précédent              */
  /* Q # NIL : Elle renvoie Q                                    */
  /*=============================================================*/
   ACTION Recherche_maillon ( Head , Word , Qi , Suv , Prc )
   SOIENT
       Qi , Pi , Head , Suv , Prc DES LISTEBI DE ARB DE CHAINES ;
       Chr , Word DES CHAINE ;
       Existant UN BOOLEEN ;
       Tail UN ENTIER ;
      
   DEBUT
       Existant := FAUX ;
       ALLOUER ( Qi ) ;
       Suv := NIL ;
       Prc := NIL ;
       Tail := LONGCHAINE ( Word ) ;
       SI ( Head = NIL )
           Qi := NIL
       SINON
           Pi := Head ;
           SI ( SUIVANT ( Head ) = NIL )
               SI ( LONGCHAINE ( INFO ( VALEUR ( Pi ) ) ) = Tail )
                   Qi := Pi ;
                  
               SINON
                   Qi := NIL ;
                   Suv := Pi ;
                   Prc := Pi ;
                  
               FSI ;
              
           SINON
               SI ( ( Tail < LONGCHAINE ( INFO ( VALEUR ( Head ) ) ) ) OU ( Tail > LONGCHAINE ( INFO ( VALEUR (
               PRECEDENT ( Head ) ) ) ) ) )
                   Qi := NIL ;
                   Suv := Head ;
                   Prc := PRECEDENT ( Head ) ;
                  
               SINON
                   SI ( Tail < ( L / 2 ) )
                       Prc := Pi ;
                       Suv := SUIVANT ( Pi ) ;
                       Qi := NIL ;
                       Chr := INFO ( VALEUR ( Pi ) ) ;
                       TANTQUE ( ( ( LONGCHAINE ( Chr ) ) <= Tail ) ET ( NON Existant ) )
                           SI ( LONGCHAINE ( Chr ) = Tail )
                               Existant := VRAI ;
                               Qi := Pi ;
                              
                           SINON
                               Qi := NIL ;
                               Prc := Pi ;
                               Pi := SUIVANT ( Pi ) ;
                               Suv := Pi ;
                              
                           FSI ;
                           Chr := INFO ( VALEUR ( Pi ) ) ;
                          
                       FINTANTQUE ;
                      
                   SINON
                       Suv := Pi ;
                       Pi := PRECEDENT ( Pi ) ;
                       Prc := Pi ;
                       Qi := NIL ;
                       Chr := INFO ( VALEUR ( Pi ) ) ;
                       TANTQUE ( ( LONGCHAINE ( Chr ) >= Tail ) ET ( NON Existant ) )
                           SI ( LONGCHAINE ( Chr ) = Tail )
                               Existant := VRAI ;
                               Qi := Pi ;
                              
                           SINON
                               Qi := NIL ;
                               Suv := Pi ;
                               Pi := PRECEDENT ( Pi ) ;
                               Prc := Pi ;
                              
                           FSI ;
                           Chr := INFO ( VALEUR ( Pi ) ) ;
                          
                       FINTANTQUE ;
                      
                   FSI ;
                  
               FSI ;
              
           FSI ;
          
       FSI ;
      
   FIN
  /******************************************************************************/
  /* Renvoie le maillon créé, dans P.                                           */
  /* Si la liste était vide, il sera dans Tete et Q sera à NIL                  */
  /******************************************************************************/
   ACTION Insert_maillon ( Header , Ch , Pp , Siv , Pec )
   SOIENT
       Racine UN ARB DE CHAINE ;
       Header , Pp , Qq , Siv , Pec DES LISTEBI DE ARB DE CHAINE ;
       Ch UNE CHAINE ;
      
   DEBUT
       CREERNOEUD ( Racine ) ;
       AFF_INFO ( Racine , Ch ) ;
       AFF_FD ( Racine , NIL ) ;
       AFF_FG ( Racine , NIL ) ;
       SI ( Pp = NIL )
           SI ( Header = NIL )
               ALLOUER ( Qq ) ;
               AFF_ADRD ( Qq , NIL ) ;
               AFF_ADRG ( Qq , NIL ) ;
               AFF_VAL ( Qq , Racine ) ;
               Header := Qq ;
              
           SINON
               ALLOUER ( Pp ) ;
               AFF_VAL ( Pp , Racine ) ;
               AFF_ADRG ( Siv , Pp ) ;
               AFF_ADRD ( Pp , Siv ) ;
               AFF_ADRG ( Pp , Pec ) ;
               AFF_ADRD ( Pec , Pp ) ;
               SI ( LONGCHAINE ( INFO ( VALEUR ( Pp ) ) ) < LONGCHAINE ( INFO ( VALEUR ( Header ) ) ) )
                   Header := Pp ;
                  
               FSI ;
              
           FSI ;
          
       FSI ;
      
   FIN
  /************************************************************************/
  /* Parcours innordre d'un arbre binaire de recherche                    */
  /************************************************************************/
   ACTION Inordre ( Rb )
   SOIT
       Rb UN ARB DE CHAINES ;
      
   DEBUT
       SI ( Rb <> NIL )
           APPEL Inordre ( FG ( Rb ) ) ;
           ECRIRE ( INFO ( Rb ) ) ;
           APPEL Inordre ( FD ( Rb ) ) ;
          
       FSI ;
      
   FIN
  /************************************************************************/
  /*          Supprime le maillon "P" de la liste (Re-chainage)           */
  /************************************************************************/
   ACTION Sup_maillon ( Lis , Ras )
   SOIT
       Lis , Ras UNE LISTEBI DE ARB DE CHAINE ;
      
   DEBUT
       SI ( Lis = Ras )
           Ras := SUIVANT ( Lis ) ;
          
       FSI ;
       SI ( SUIVANT ( Lis ) <> NIL )
           SI ( SUIVANT ( SUIVANT ( Lis ) ) = Lis )
               AFF_ADRD ( SUIVANT ( Lis ) , NIL ) ;
               AFF_ADRG ( PRECEDENT ( Lis ) , NIL ) ;
              
           SINON
               SI ( PRECEDENT ( Lis ) # NIL )
                   AFF_ADRD ( PRECEDENT ( Lis ) , SUIVANT ( Lis ) ) ;
                  
               FSI ;
               SI ( SUIVANT ( Lis ) # NIL )
                   AFF_ADRG ( SUIVANT ( Lis ) , PRECEDENT ( Lis ) ) ;
                  
               FSI ;
              
           FSI ;
          
       FSI ;
       AFF_ADRD ( Lis , NIL ) ;
       AFF_ADRG ( Lis , NIL ) ;
       LIBERER ( Lis ) ;
       ECRIRE ( '' ) ;
       ECRIRE ( '* Racine a nil => Maillon correspondant supprimé' ) ;
      
   FIN
  /*===============================================================================*/
  /*Renvoie le noeud dont l'info = string (NIL s'il n'existe pas). En plus du pere */
  /*===============================================================================*/
   ACTION Existe ( Tree , Ascendent , Nod , String )
   SOIENT
       Tree UN ARB DE CHAINES ;
       Ascendent UN ARB DE CHAINES ;
       Nod UN ARB DE CHAINES ;
       String UNE CHAINE ;
       Cherch UN BOOLEEN ;
      
   DEBUT
       Cherch := VRAI ;
       Nod := Tree ;
       Ascendent := NIL ;
       TANTQUE ( ( Cherch ) ET ( Nod <> NIL ) )
           SI ( INFO ( Nod ) = String )
               Cherch := FAUX ;
              
           SINON
               Ascendent := Nod ;
               SI ( INFO ( Nod ) > String )
                   Nod := FG ( Nod ) ;
                  
               SINON
                   Nod := FD ( Nod ) ;
                  
               FSI ;
              
           FSI ;
          
       FINTANTQUE ;
      
   FIN
  /*=============================================================*/
  /*Rend vrai si la chaine String existe dans l'arbre R          */
  /*=============================================================*/
  /* ACTION Existe ( Tree , String , Nod ) 
   SOIENT 
       Tree , Nod UN ARB DE CHAINES ; 
       String UNE CHAINE ; 
   DEBUT 
       SI ( Tree = NIL ) 
           Nod := NIL ; 
       SINON 
           SI ( String = INFO ( Tree ) ) 
               Nod := Tree ; 
           SINON 
               SI ( String < INFO ( Tree ) ) 
                   APPEL Existe ( FG ( Tree ) , String ) 
               SINON 
                   APPEL Existe ( FD ( Tree ) , String ) 
               FSI 
           FSI 
       FSI 
   FIN   */
  /*************************************************************************/
  /*               Gère la suppression des noeuds de l'arbre               */
  /* Le parcours du tableau se fera à l'envers, donc on aura juste à suppr-*/
  /* -imer des feuilles.                                                   */
  /*************************************************************************/
   ACTION Sup_noeud ( Maillo , Chain , Err )
   SOIENT
       Maillo UNE LISTEBI DE ARB DE CHAINE ;
       Chain UNE CHAINE ;
       Nd , Padre , Root , Test DES ARB DE CHAINE ;
       Err UN BOOLEEN ;
      
   DEBUT
       Err := FAUX ;
       Test := NIL ;
       Root := VALEUR ( Maillo ) ;
       APPEL Existe ( Root , Padre , Nd , Chain ) ;
      /*on recherche le noeud et son pere*/
       SI ( Nd <> NIL )
           SI ( Padre = NIL )
               AFF_VAL ( Maillo , Test ) ;
              /*si le noeud a supprimer c'est la tete*/
              
           SINON
               SI ( FG ( Padre ) = Nd )
                   AFF_FG ( Padre , Test ) ;
                  
               SINON
                   AFF_FD ( Padre , Test ) ;
                  
               FSI ;
              
           FSI ;
           LIBERERNOEUD ( Nd ) ;
           ECRIRE ( '' ) ;
           ECRIRE ( '* Mot supprime avec succes' ) ;
          
       SINON
           Err := VRAI ;
          
       FSI ;
      
   FIN
   ACTION Recherche_sup ( Latete , Lemot , Qointe , Suivi , Prece )
   SOIENT
       Pointe , Qointe , Latete , Suivi , Prece DES LISTEBI DE ARB DE CHAINES ;
       Chn , Lemot DES CHAINE ;
       Exis UN BOOLEEN ;
       Tai UN ENTIER ;
      
   DEBUT
       Exis := FAUX ;
       ALLOUER ( Qointe ) ;
       Suivi := NIL ;
       Prece := NIL ;
       Tai := LONGCHAINE ( Lemot ) ;
       SI ( Latete = NIL )
           Qointe := NIL
       SINON
           Pointe := Latete ;
           SI ( SUIVANT ( Latete ) = NIL )
               SI ( LONGCHAINE ( INFO ( VALEUR ( Latete ) ) ) = Tai )
                   Qointe := Pointe ;
                  
               FSI ;
              
           SINON
               SI ( Tai < ( L / 2 ) )
                   Prece := PRECEDENT ( Pointe ) ;
                   Suivi := SUIVANT ( Pointe ) ;
                   Qointe := NIL ;
                   Chn := INFO ( VALEUR ( Pointe ) ) ;
                   TANTQUE ( ( ( LONGCHAINE ( Chn ) ) <= Tai ) ET ( NON Exis ) )
                       SI ( LONGCHAINE ( Chn ) = Tai )
                           Exis := VRAI ;
                           Prece := PRECEDENT ( Pointe ) ;
                           Qointe := Pointe ;
                           Suivi := SUIVANT ( Pointe ) ;
                          
                       SINON
                           Qointe := NIL ;
                           Pointe := SUIVANT ( Pointe ) ;
                          
                       FSI ;
                       Chn := INFO ( VALEUR ( Pointe ) ) ;
                      
                   FINTANTQUE ;
                  
               SINON
                   Suivi := Pointe ;
                   Pointe := PRECEDENT ( Pointe ) ;
                   Prece := PRECEDENT ( Pointe ) ;
                   Qointe := NIL ;
                   Chn := INFO ( VALEUR ( Pointe ) ) ;
                   TANTQUE ( ( LONGCHAINE ( Chn ) >= Tai ) ET ( NON Exis ) )
                       SI ( LONGCHAINE ( Chn ) = Tai )
                           Exis := VRAI ;
                           Suivi := SUIVANT ( Pointe ) ;
                           Qointe := Pointe ;
                           Prece := PRECEDENT ( Pointe ) ;
                          
                       SINON
                           Qointe := NIL ;
                           Pointe := PRECEDENT ( Pointe ) ;
                          
                       FSI ;
                       Chn := INFO ( VALEUR ( Pointe ) ) ;
                      
                   FINTANTQUE ;
                  
               FSI ;
              
           FSI ;
          
       FSI ;
      
   FIN
   ACTION Presentation DEBUT
       ECRIRE ( '================================================================================================' ) ;
       ECRIRE ( '                       ECOLE NATIONALE SUPERIEURE D''INFORMATIQUE  ' ) ;
       ECRIRE ( '                                           ' ) ;
       ECRIRE ( '                   ALGORITHMIQUE ET STRUCTURES DE DONNEES DYNAMIQUES ' ) ;
       ECRIRE ( '                          TP2:    Exploration de textes  ' ) ;
       ECRIRE ( '================================================================================================' ) ;
       ECRIRE ( '          Realisé par:                                                                ' ) ;
       ECRIRE ( '                        ○ AOUAOUCHE Louiza                    GROUPE 02 1CPI-A 2017/2018    ' ) ;
       ECRIRE ( '                        ○ MECHOUEK Lounes    ' ) ;
       ECRIRE ( '                                                         ' ) ;
       ECRIRE ( '          Encadrés par:                                                       ' ) ;
       ECRIRE ( '                  ○Dr ZEGOUR                  ○Dr KERMI                  ○Dr HIDOUCI   ' ) ;
       ECRIRE ( '                   ' ) ;
       ECRIRE ( '================================================================================================' ) ;
       ECRIRE ( '' ) ;
       ECRIRE ( '-------------------------------------------------------------------------------------------- ' ) ;
       ECRIRE ( '                        INSERTION ALEATOIRE :' ) ;
       ECRIRE ( '--------------------------------------------------------------------------------------------' ) ;
       ECRIRE ( '' ) ;
       ECRIRE ( '    MOTS GENERES ET RANGES DANS LE TABLEAU: ' ) ;
       ECRIRE ( '' ) ;
       ECRIRE ( '=========================================' ) ;
      
   FIN
