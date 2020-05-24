Public Class Kalaha  'Nous pouvons déclarer avec Public pour conférer un tel accès illimité à un élément. Une instruction Class définit un nouveau type de données. Kalaha est le nom de form.

    Private Sub Kalaha_load(sender As Object, e As EventArgs) Handles MyBase.Load  'initialise la form Kalaha 

        For i As Integer = 1 To 14 'le i parcours le tableau, les 14 boutons 
            Me.Controls("btn" & i.ToString).Enabled = False 'Me.Controls ("nom du contrôle") dans VB peut afficher les propriétés du contrôle
            'false signifieque les boutons sont inactivé
            Me.Controls("btn" & i.ToString).Text = "" 'lorsque la page est chargé mis à part le btn play les autres boutons sont "vide" il n'y a rien qui apparait dessus
        Next

        btnPlay.Enabled = True 'lorsque le formulaire est chargé tous les btn du jeux sont inactivé, il faut appuyer sur le bouton  play pour les activer (play c'est le seul bouton activé)

        Me.Show() 'permet de charger la page la forme en cours et l'afficher 
    End Sub

    Private Sub btnplay_Click(sender As Object, e As EventArgs) Handles btnPlay.Click 'Le mot clé Private dans une instruction de déclaration spécifie que les éléments ne sont accessibles qu'à partir de cette classe. 
        MsgBox("le jeu commence et le joueur1 à la main")  'Lorsque le bouton de PLAY est cliqué, une boîte de dialogue apparaît et affiche "le jeu commence et le joueur1 à la main"
        Dim list As New List(Of Integer) 'on déclare la variable de type list et on précise la contrainte, elle sera composé que de nombres (integer), la list est introduit par le mot As

        For i As Integer = 1 To 14 'la variable i qui va parcourir toute les cases, du bouton 1 à 14
            Me.Controls("btn" & i.ToString).Text = Initial_interface(i - 1)  'on utilise une fonction du module ( Initial_interface), cela donne une valeur de 3 à toutes les cases représenté par les boutons, ce sera la répartition des jetons sauf pour le bouton 7 et 14 qui sont à 0
            If i >= 1 And i < 7 Then 'ici on considère les cases de 1 à 6 du premier joueur, i va parcourir les cases entre le bouton 1 et 6.
                Me.Controls("btn" & i.ToString).Enabled = True 'enable= true permet d'activer les boutons du joueur 1.
            ElseIf i >= 8 And i < 14 Then 'on considère maintenant les boutons du joueur 2 
                Me.Controls("btn" & i.ToString).Enabled = False 'on désactive les boutons 8 à 13
            End If
        Next

    End Sub

    Private Sub joueur_Click(sender As Object, e As EventArgs) Handles btn1.Click, btn2.Click, btn3.Click, btn4.Click, btn5.Click, btn6.Click, btn8.Click, btn9.Click, btn10.Click, btn11.Click, btn12.Click, btn13.Click ' c'est une procédure pour gérer les click sur les cases du joueur(le joueur1 et joueur 2 sont compris dedans).

        Dim nb As Byte 'variable nb comme entier
        Dim valeur As Integer ' variable valeur comme entier
        Dim tabdebut As Button() = New Button(14) {} 'la variable tabdebut qui est déclaré comme un bouton, on a créé 14 nouveaux boutons
        Dim mybutton As Button = DirectCast(sender, Button) 'utiliser la functionne de DirectCast pour obtenir le nom du bouton lorsque nous cliquons sur le bouton
        Dim res As Byte 'variable res comme entier
        Dim resultat As Byte 'variable resultat comme entier

        For i As Integer = 1 To 14
            tabdebut(i) = Me.Controls("btn" & i.ToString) 'Attribuez la valeur du bouton nommé btn & i dans le contrôle aux 14 nouveaux boutons
        Next

        valeur = mybutton.Text 'lorsqu'on clique sur un bouton, on récupère la valeur qu'on y trouve
        mybutton.Text = 0 'on indique que la nouvelle valeur de la case est zero, cela correspond au fait que tous les jetons sont récupéré pour les répartir dans les suivante et que donc notre case sera "vide"
        nb = CInt(Mid(sender.Name, 4)) 'c'est pour obtenir le numero du button et l'utiliser comme indice i, ex. btn1, on veut avoir le quatrieme charactere qui est 1; mid = "retour à cette valeur" permet de recuperer "btn1" avec le sender ; cint = Convertir le type en integer, car le quatrieme caractère est considéré comme du text il font donc convertir

        Call KalahaGame(nb, tabdebut, valeur) 'on appel la fonction kalahaGame qui démarre le tour 
        Call joueur_Etat(nb, valeur, tabdebut, res, resultat) ' on appel la procédure joueur_Etat qui vérifie s'il peut jouer ou non. Déterminer si le statut du joueur remplit les conditions pour jouer deux tours; puis juge si c'est la fin du jeu ou non.

        If res = 1 Then 'on passe la main au joueur 1, seulement si le joueur2 n'a plus de jetons à répartir ou bien si le joueur1 qui vient de jouer a déposé dans son panier le dernier jeton qu'il a réparti 
            For j As Integer = 1 To 13  ' j parcours toutes les cases de 1 à 13
                If j >= 1 And j < 7 Then
                    Me.Controls("btn" & j.ToString).Enabled = True ' controle que les cases représenté par leurs boutons et l'indice j correspondant allant de 1 à 6 sont bien activées.
                ElseIf j >= 8 And j < 14 Then
                    Me.Controls("btn" & j.ToString).Enabled = False ' les cases de 8 à 13 sont inactivé
                End If
            Next

        Else 'si res = 0 ici on désactive les boutons du joueur 1 et on passe la main au joueur 2, seulement si le joueur1 n'a plus de jetons à répartir ou si le joueur2 qui vient de jouer a déposé dans son panier le dernier jeton qu'il a réparti et donc garde la main.
            'il y a aussi un cas, lorsque le dernier jeton du joueur 1 est placé dans son panier, mais celui ci n'a plus de jetons à repartir, à ce moment le joueur 2 continuera de joueur.
            For j As Integer = 1 To 13
                If j >= 1 And j < 7 Then
                    Me.Controls("btn" & j.ToString).Enabled = False 'controle que les cases représenté par leurs boutons et l'indice j correspondant allant de 1 à 6 sont inactivé
                ElseIf j >= 8 And j < 14 Then
                    Me.Controls("btn" & j.ToString).Enabled = True ' les cases de 8 à 13 sont activé
                End If
            Next
        End If

        For t As Integer = 1 To 13  ' S'il n'y a plus de jetons sur la tablette, c'est-à-dire 0 dans les cases des joueurs, les boutons ne peuvent pas être cliqué pour l'instant
            If Me.Controls("btn" & t.ToString).Text = 0 Then
                Me.Controls("btn" & t.ToString).Enabled = False
            End If
        Next

        If resultat = 1 Then  'si le res=1 alors la partie est terminé 
            If btn7.Text > btn14.Text Then 'on compare les valeurs des deux paniers, si le panier 7 a une valeur superieur au panier 14 le joueur 1 gagne, un message s'affiche pour l'indiquer 
                MsgBox("Joueur1 gagne")
            ElseIf btn7.Text < btn14.Text Then 'si la valeur du panier 7 est inferieur à celle du panier 14 le joueur deux gagne 
                MsgBox("Joueur2 gagne")
            ElseIf btn7.Text = btn14.Text Then ' les deux joueurs gagnent 
                MsgBox("Personne gagne")
            End If
        End If

    End Sub

End Class