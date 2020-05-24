Module ModuleKalaha   'on ajoute un module, nommé  ModuleKalaha.vb

    Public Function Initial_interface(ByVal i As Integer) 'on utilise une procédure Function pour renvoyer une valeur au code appelant, cette function s'appelle Initial_interface; 
        Dim list As New List(Of Integer)({3, 3, 3, 3, 3, 3, 0, 3, 3, 3, 3, 3, 3, 0}) 'Créer une nouvelle liste, cette fonction va permettre d'initialisé les bouton à 3 lorsque la partie démarre, les panies sont initialisé à 14.
        Return list.Item(i) 'retourner la valeur dans l'interface
    End Function

    Public Sub KalahaGame(ByVal nb As Byte, ByVal tabdebut As Button(), ByVal valeur As Byte) ' on déclare le paramètre ByVal nb comme entier,tabdebut comme boutton, valeur comme entier.

        Dim i As Integer 'variable i comme entier

        If (nb + valeur) <= 14 Then  ' premier cas ou on répartie les jetons
            For i = 1 To valeur
                tabdebut(nb + i).Text += 1 ' si le nb c'est à dire l'indice de la case + la valeur de la case(nombre de jetons à ce moment là) est inferieur ou égal à 14 alors on répartie les jetons en ajoutant + 1 à toutes les cases qui suivent
            Next
        ElseIf (nb + valeur) <= 28 Then ' deuxiéme cas
            For i = 1 To 14 - nb            ' (on a mis 28 dans le cas ou le bouton 13 a une valeur de 15 (qui est le maiximum) la répartion des jetons se fait tel que l'on répartie sur deux tour dans l'ensemble des cases). 
                tabdebut(nb + i).Text += 1
            Next
            For i = 1 To valeur - (14 - nb) '  et quand la répartition des jetons dépasse le nombre de cases c'est à dire (entre 15 et 28) il faut indiquer au tableau qu'il faut reprendre la répartion à partir du bouton 1 car sinon il s'arrète au bouton 14 qui est la dernière case du tableau.
                tabdebut(i).Text += 1
            Next
        Else  'troisième cas quand on clique sur une case les jetons correspondant après avoir été réparti du coté du joueur courant puis du coté du joueur adverse on revient du coté du joueur courant pour continué la répartition.
            For i = 1 To 14 - nb
                tabdebut(nb + i).Text += 1
            Next
            For i = 1 To 14
                tabdebut(i).Text += 1
            Next
            For i = 1 To valeur - (14 - nb) - 14  'la distribution des derniers jetons reprend pour la 3eme fois à partir de la case 1.
                tabdebut(i).Text += 1
            Next
        End If

    End Sub
    Public Function joueur_choisi(ByVal nb As Byte) As Boolean ' la fOnction de joueur_choisi est bouléenne (Vrai ou faux), le paramètre ByVal nb comme entier
        Dim etat As Boolean  'variable etat est bouléenne également

        If nb >= 1 And nb < 7 Then 'si nb est superieur à 0 et inferieur à 7, alors l'état est vrai c'est le joueur 1 qui a la main ( cela permettra de maintenir activé ou non les boutons du joueur 1 quand cette fonction sera appelé)
            etat = True
        Else                       'sinon, etat est faux (c'est les boutons du joueur 2 qui sont activé )
            etat = False
        End If
        Return etat 'retourne la valeur de l'etat c'est à dire vrai ou faux

    End Function
    Public Sub joueur_Etat(ByVal nb As Byte, ByVal valeur As Byte, ByVal tabdebut As Button(), ByRef res As Byte, ByRef resultat As Byte) ' on déclare le paramètre ByVal nb comme entier,tabdebut comme boutton, valeur comme entier, resultat comme entier
        Dim somme1 As Integer 'somme1  et somme2 des variables qui indiqueront le nombre de case à valeur nulle.
        Dim somme2 As Integer
        Dim etat As Boolean

        somme1 = 0 : somme2 = 0 : resultat = 0 'on initialise les deux variables somme et resultat à zéro;  

        For i As Integer = 1 To 6 'variable i va parcourir les cases du tableau de 1 à 6 
            If tabdebut(i).Text = 0 Then 'quand la valeur d'une case dans le tableau est égale à zéro alors on ajoute +1 à somme pour indiquer qu'il y'a une case nulle en plus.
                somme1 += 1
            End If
        Next

        For i = 8 To 13
            If tabdebut(i).Text = 0 Then
                somme2 += 1 'Compte le nombre de cases sans jetons chez le joueur 2.
            End If
        Next

        etat = joueur_choisi(nb)  'on appele la fonction de joueur_choisi, et l'indice de la case qui est nb.

        If etat = True Then  'si etat = true, c'est à dire nb est superieur à 0 et inferieur à 7, en d'autres termes, c'est le joueur 1 qui vient de jouer

            If (valeur + nb = 7) And (somme1 = 6) Then ' ici le joueur 1 ne peut pas rejouer même s'il a réparti son dernier jeton dans le panier il ne lui reste plus de jetons à repartir pour le prochain tour. 
                res = 0
            ElseIf (valeur + nb = 7) Or (valeur + nb = 21) Or (somme2 = 6) Then
                res = 1 'ici il rejoue s'il vient de répartie son dernier jeton dans son panier au premier tour(valeur + nb = 7) ou en jouant il a une valeur suffisante pour faire deux fois le tour et mettre deux points dans son panier tout en répartissant son dernier jeton dans le panier (valeur + nb = 21) ou bien si le joueur adverse n'a pas de jetons (somme2 = 6) alors il peut garder la main et res=1.
            Else
                res = 0 'sinon la main passe au joueur 2.
            End If

        Else
            If (valeur + nb = 14) And (somme2 = 6) Then ' le joueur 2 ne peut pas jouer s'il réparti son dernier jeton dans le panier mais qu'il n'a plus de jetons à repartir pour le prochain tour. 
                res = 1 'le joueur 1 prend la main
            ElseIf (valeur + nb = 14) Or (valeur + nb = 28) Or (somme1 = 6) Then
                res = 0 'dans le cas ou il joue et répartie son dernier jeton dans son panier au premier tour(valeur + nb = 14)ou en jouant il a une valeur suffisante pour faire deux fois le tour et mettre deux points dans son panier tout en répartissant son dernier jeton dans le panier  (valeur + nb = 28) ou bien le joueur 1 n'a pas de jetons à jouer alors il peut garder la main et le res=0.
            Else
                res = 1 'le joueur 1 garde la main et joue.
            End If
        End If

        If somme1 + somme2 = 12 Then 'si la somme des deux sommes 1 et 2 qui ont compatabilisé le nombre de cases à valeur nulle pour chaque joueur est égale à 12, c'est a dire que les deux joueurs n'ont plus de jeton à distribuer le jeu se termine et le resultat envoyé à l'interface = 1.
            resultat = 1
        End If
    End Sub

End Module
