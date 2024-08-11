
{-# OPTIONS_GHC -Wall #-}
--
-- Ce fichier défini les fonctionalités suivantes:
-- - Analyseur lexical
-- - Analyseur syntaxique
-- - Pretty printer
-- - Implantation du langage

---------------------------------------------------------------------------
-- Importations de librairies et définitions de fonctions auxiliaires    --
---------------------------------------------------------------------------

import Text.ParserCombinators.Parsec -- Bibliothèque d'analyse syntaxique.
import Data.Char                -- Conversion de Chars de/vers Int et autres.
import System.IO                -- Pour stdout, hPutStr

---------------------------------------------------------------------------
-- 1ère représentation interne des expressions de notre langage           --
---------------------------------------------------------------------------
data Sexp = Snil                        -- La liste vide
          | Scons Sexp Sexp             -- Une paire
          | Ssym String                 -- Un symbole
          | Snum Int                    -- Un entier
          -- Génère automatiquement un pretty-printer et une fonction de
          -- comparaison structurelle.
          deriving (Show, Eq)

-- Exemples:
-- (+ 2 3)  ==  (((() . +) . 2) . 3)
--          ==>  Scons (Scons (Scons Snil (Ssym "+"))
--                            (Snum 2))
--                     (Snum 3)
--                   
-- (/ (* (- 68 32) 5) 9)
--     ==  (((() . /) . (((() . *) . (((() . -) . 68) . 32)) . 5)) . 9)
--     ==>
-- Scons (Scons (Scons Snil (Ssym "/"))
--              (Scons (Scons (Scons Snil (Ssym "*"))
--                            (Scons (Scons (Scons Snil (Ssym "-"))
--                                          (Snum 68))
--                                   (Snum 32)))
--                     (Snum 5)))
--       (Snum 9)

---------------------------------------------------------------------------
-- Analyseur lexical                                                     --
---------------------------------------------------------------------------

pChar :: Char -> Parser ()
pChar c = do { _ <- char c; return () }

-- Les commentaires commencent par un point-virgule et se terminent
-- à la fin de la ligne.
pComment :: Parser ()
pComment = do { pChar ';'; _ <- many (satisfy (\c -> not (c == '\n')));
                (pChar '\n' <|> eof); return ()
              }
-- N'importe quelle combinaison d'espaces et de commentaires est considérée
-- comme du blanc.
pSpaces :: Parser ()
pSpaces = do { _ <- many (do { _ <- space ; return () } <|> pComment);
               return () }

-- Un nombre entier est composé de chiffres.
integer     :: Parser Int
integer = do c <- digit
             integer' (digitToInt c)
          <|> do _ <- satisfy (\c -> (c == '-'))
                 n <- integer
                 return (- n)
    where integer' :: Int -> Parser Int
          integer' n = do c <- digit
                          integer' (10 * n + (digitToInt c))
                       <|> return n

-- Les symboles sont constitués de caractères alphanumériques et de signes
-- de ponctuations.
pSymchar :: Parser Char
pSymchar = alphaNum <|> satisfy (\c -> c `elem` "!@$%^&*_+-=:|/?<>")
pSymbol :: Parser Sexp
pSymbol= do { s <- many1 (pSymchar);
              return (case parse integer "" s of
                        Right n -> Snum n
                        _ -> Ssym s)
            }

---------------------------------------------------------------------------
-- Analyseur syntaxique                                                  --
---------------------------------------------------------------------------

-- La notation "'E" est équivalente à "(shorthand-quote E)"
-- La notation "`E" est équivalente à "(shorthand-backquote E)"
-- La notation ",E" est équivalente à "(shorthand-comma E)"
pQuote :: Parser Sexp
pQuote = do { c <- satisfy (\c -> c `elem` "'`,"); pSpaces; e <- pSexp;
              return (Scons
                      (Scons Snil
                             (Ssym (case c of
                                     ',' -> "shorthand-comma"
                                     '`' -> "shorthand-backquote"
                                     _   -> "shorthand-quote")))
                      e) }

-- Une liste (Tsil) est de la forme ( [e .] {e} )
pTsil :: Parser Sexp
pTsil = do _ <- char '('
           pSpaces
           (do { _ <- char ')'; return Snil }
            <|> do hd <- (do e <- pSexp
                             pSpaces
                             (do _ <- char '.'
                                 pSpaces
                                 return e
                              <|> return (Scons Snil e)))
                   pLiat hd)
    where pLiat :: Sexp -> Parser Sexp
          pLiat hd = do _ <- char ')'
                        return hd
                 <|> do e <- pSexp
                        pSpaces
                        pLiat (Scons hd e)

-- Accepte n'importe quel caractère: utilisé en cas d'erreur.
pAny :: Parser (Maybe Char)
pAny = do { c <- anyChar ; return (Just c) } <|> return Nothing


-- Une Sexp peut-être une liste, un symbol ou un entier.
pSexpTop :: Parser Sexp
pSexpTop = do { pTsil <|> pQuote <|> pSymbol
                <|> do { x <- pAny;
                         case x of
                           Nothing -> pzero
                           Just c -> error ("Unexpected char '" ++ [c] ++ "'")
                       }
              }


-- On distingue l'analyse syntaxique d'une Sexp principale de celle d'une
-- sous-Sexp: si l'analyse d'une sous-Sexp échoue à EOF, c'est une erreur de
-- syntaxe alors que si l'analyse de la Sexp principale échoue cela peut être
-- tout à fait normal.
pSexp :: Parser Sexp
pSexp = pSexpTop <|> error "Unexpected end of stream"

-- Une séquence de Sexps.
pSexps :: Parser [Sexp]
pSexps = do pSpaces
            many (do e <- pSexpTop
                     pSpaces
                     return e)

-- Déclare que notre analyseur syntaxique peut-être utilisé pour la fonction
-- générique "read".
instance Read Sexp where
    readsPrec _ s = case parse pSexp "" s of
                      Left _ -> []
                      Right e -> [(e,"")]

---------------------------------------------------------------------------
-- Sexp Pretty Printer                                                   --
---------------------------------------------------------------------------

showSexp' :: Sexp -> ShowS
showSexp' Snil = showString "()"
showSexp' (Snum n) = showsPrec 0 n
showSexp' (Ssym s) = showString s
showSexp' (Scons e1 e2) = showHead (Scons e1 e2) . showString ")"
    where showHead (Scons Snil e') = showString "(" . showSexp' e'
          showHead (Scons e1' e2')
            = showHead e1' . showString " " . showSexp' e2'
          showHead e = showString "(" . showSexp' e . showString " ."

-- On peut utiliser notre pretty-printer pour la fonction générique "show"
-- (utilisée par la boucle interactive de GHCi).  Mais avant de faire cela,
-- il faut enlever le "deriving Show" dans la déclaration de Sexp.
{-
instance Show Sexp where
    showsPrec p = showSexp'
-}

-- Pour lire et imprimer des Sexp plus facilement dans la boucle interactive
-- de Hugs/GHCi:
readSexp :: String -> Sexp
readSexp = read
showSexp :: Sexp -> String
showSexp e = showSexp' e ""

---------------------------------------------------------------------------
-- Représentation intermédiaire Lexp                                     --
---------------------------------------------------------------------------

type Var = String


data Lexp = Lnum Int            -- Constante entière.
          | Lvar Var            -- Référence à une variable.
          | Lproc Var Lexp      -- Fonction anonyme prenant un argument.
          | Ldo Lexp Lexp       -- Appel de fonction, avec un argument.
          | Lnull               -- Constructeur de liste vide.
          | Lnode [Lexp]    -- Constructeur de liste.
          | Lcase Lexp Lexp Var Var Lexp -- Expression conditionnelle.
          -- Déclaration d'une liste de variables qui peuvent être
          -- mutuellement récursives.
          | Ldef [(Var, Lexp)] Lexp 

          -- Declaration de variables (plus simple que version du prof)
          | Ldefv Var Lexp Lexp
          | Lseq [Lexp] --
          deriving (Show, Eq)

-- Première passe simple qui analyse une Sexp et construit une Lexp équivalente.
s2l :: Sexp -> Lexp
s2l (Snum n) = Lnum n
s2l (Ssym "null") = Lnull
s2l (Snil) = Lnull
s2l (Ssym s) = Lvar s
-- ¡¡COMPLÉTER ICI!!
s2l (Scons e1 e2) = let expr = parcoursPrefixe (Scons e1 e2) in
    if (expr!!0 == (Ssym "def")) || (expr!!0 == (Ssym "proc")) 
    || (expr!!0 == (Ssym "seq")) || (expr!!0 == (Ssym "node")) 
    || (expr!!0 == (Ssym "case"))

        then case expr!!0 of 
            -- Parcours de l'arbre de maniere préfixe (racine gauche droit)
            (Ssym "def") -> case (isDefFonction (tail expr) 
                            && isConsFonction (tail expr)) of
                False -> trPrefix ( tail (parcoursPrefixe (Scons e1 e2))) 
                True ->  Ldo (lexpDefFonction (
                                    chercherSconsDef e1)) (s2l (last (
                                         parcoursPrefixe e2)))

            -- Parcours de l'arbre de maniere préfixe (racine gauche droit)
            (Ssym "proc")-> trPrefixProc (expr)  

            -- Transformation de la liste sexp préfixe en une séquence 
            (Ssym "seq")-> lexpALseq(
                                sexpALexp(
                                    retirerSsym(
                                        tail(parcoursPrefixe(
                                            Scons e1 e2)))))

            -- Tranformation de la liste sexp préfixe en une node ne
            -- dépassant pas les 2 éléments
            (Ssym "node")-> if length (retirerSsym(
                                            tail (parcoursPrefixe(
                                                        Scons e1 e2)))) < 3 
            then lexpALseq(sexpALexp(retirerSsym(
                                        tail (parcoursPrefixe(Scons e1 e2)))))
            else error "node peut avoir que 2 éléments au maximum"

            (Ssym "case") -> lexpALcase(
                                listeCase(
                                    lecteurCase(
                                        sexpToLexpC(
                                            retirerCase(
                                                tail(parcoursPrefixe (
                                                        Scons e1 e2)))))))


            -- Erreur ou le type de l'expression n'existe pas
            _ -> error("Expression inexistante")

                            else case e1 of
                                    Snil -> s2l e2
                                    _ -> let e1' = s2l e1 
                                             e2' = s2l e2
                                         in Ldo e1' e2'





---------------------------------------------------------------------------
-- Fonction auxiliaires de s2l                             --
---------------------------------------------------------------------------

-- Fonction qui transforme une liste de Sexp en une liste de Lexp
sexpALexp :: [Sexp] -> [Lexp]
sexpALexp [] = []
sexpALexp [Scons _ _] = [] 
sexpALexp (x: xs) = sexpLexp x : sexpALexp xs

-- Fonction qui tranforme un Sexp en un Lexp
sexpLexp :: Sexp -> Lexp
sexpLexp (Snum n) = Lnum n
sexpLexp (Ssym s) = Lvar s
sexpLexp (Snil) = Lnull
sexpLexp (Scons _ _) = Lnull

-- Fonction qui transforme une liste de Lexp en une Lseq 
lexpALseq :: [Lexp] -> Lexp
lexpALseq [] = Lnull
lexpALseq (x: xs) = Lseq (x:xs)

-- Fonction qui transforme une liste de Lexp en une Lnode
-- tout en vérifiant que la node ne dépasse pas 2 éléments
lexpALnode :: [Lexp] -> Lexp
lexpALnode [] = Lnull
lexpALnode (x: xs) = if length (x:xs) < 3 then Lnode (x:xs) 
                     else error "La node peut avoir que 2 éléments au maximum"


-- Fonction qui transforme un Lexp en Value
lexpAValue :: Lexp -> Value
lexpAValue Lnull = Vnil
lexpAValue (Lnum n) = Vnum n 
lexpAValue _ = Vnil

-- Fonction qui transforme une liste de Lexp en une value
lexpAnode :: [Lexp] -> Value
lexpAnode [] = Vnil
lexpAnode (x : xs) = Vcons (lexpAValue x) (lexpAnode xs)

-- Fonction qui prends une liste de Sexp et qui la renvoie
-- en retirant les éléments de type Ssym
retirerSsym :: [Sexp] -> [Sexp]
retirerSsym = filter isSnum
  where
    isSnum (Ssym _) = False
    isSnum _        = True

-- Fonction qui retire le premier identificarteur de type Ssym "case"
retirerCase :: [Sexp] -> [Sexp]
retirerCase = filter isCase
  where
    isCase (Ssym "case") = False
    isCase _ = True

-- Fonction qui transforme une liste de Sexp d'un case en une liste de Lexp
-- ainsi que tout ses éléments
sexpToLexpC :: [Sexp] -> [Lexp]
sexpToLexpC [] = []
sexpToLexpC (x : xs) = case x of
  Snum n -> Lnum n : sexpToLexpC xs
  Ssym "seq" ->
    let (seqElements, remaining) = allerASymbole xs
     in Lseq (map s2l seqElements) : sexpToLexpC remaining
  Ssym "node" ->
    let (nodeElements, remaining) = allerASymbole xs
     in Lnode (map s2l nodeElements) : sexpToLexpC remaining
  Ssym "null" ->
    let (nullElements, remaining) = allerASymbole xs
     in Lnode (Lnull : map s2l nullElements) : sexpToLexpC remaining
  Ssym s -> [Ldo (Lvar s) (lexpALseq(sexpALexp(retirerSsym(xs))))]
  Snil -> Lnull : sexpToLexpC xs
  _ -> error "Expression non reconnue"

-- Fonction qui tranforme un Lexp en un Lvar
lexpAVar :: Lexp -> Lexp
lexpAVar Lnull = Lnull
lexpAVar (other) = Lvar (show other)

-- Fonction qui prends une liste de Lexp et qui la renvoie
-- de manière à ce qu'elle convienne pour Lcase
lecteurCase :: [Lexp] -> [Lexp]
lecteurCase [Lnull,_,_,Lnull,e] = [e]
lecteurCase [lexp1,lexp2,var1,var2,lexp3] = [lexp1,lexp2,lexpAVar(lireListe(valeurNode(var1))),lexpAVar(var2),lexp3]
lecteurCase _ = [Lnull]

-- Fonction qui prends les valeur d'une liste et qui construit
-- avec un Lcase 
lexpALcase :: [Lexp] -> Lexp
lexpALcase [l1,l2,(Lvar v1),(Lvar v2),l3] = Lcase l1 l2 v1 v2 l3
lexpALcase _ = Lnull

-- Fonction qui renvoie la valeur de la liste donnée
lireListe :: [Lexp] -> Lexp
lireListe [Lnull] = Lnull
lireListe [a] = a 
lireListe _ = Lnull  

-- Fonction qui transforme une liste de Lexp
-- en une qui convient pour Lcase
listeCase :: [Lexp] -> [Lexp]
listeCase [a,b,c,d,e] = [a,lireListe(valeurNode(b)),c,d,e]
listeCase _ = [Lnull]

-- Fonction qui renvoie la liste contenant la valeur d'un noeud pour case
-- Par exemple pour (null 1) elle renvoie 1
-- et pour (node 1:2 3) elle renvoie [2,3]
valeurNode :: Lexp -> [Lexp]
valeurNode (Lnull) = [Lnull]
valeurNode (Lnode [x]) = [x]
valeurNode (Lnode (_: xs)) = xs
valeurNode _ = [Lnull]

-- Fonction pour collecter les éléments jusqu'à un autre symbole
allerASymbole :: [Sexp] -> ([Sexp], [Sexp])
allerASymbole [] = ([], [])
allerASymbole (Ssym s : rest) = ([], Ssym s : rest)
allerASymbole (x : xs) =
  let (collected, remaining) = allerASymbole xs
   in (x : collected, remaining)


-- Fonction pour transforme une liste de Sexp en une Lexp dans
-- le cas d'une définition d'une procédure anonyme
trPrefixProc :: [Sexp] -> Lexp
trPrefixProc [Snil] = Lnull
trPrefixProc [Ssym s] = Lvar s
trPrefixProc (x:xs) = case (last xs) of
                        Snum n -> Ldo (trPrefixProc (init (x:xs))) (Lnum n)

                        _ -> case x of
                                Ssym "proc" -> (trPrefixProc xs)
                                Ssym "*" -> (trPrefix (x:xs))
                                Ssym "/" -> (trPrefix (x:xs))
                                Ssym "+" -> (trPrefix (x:xs))
                                Ssym "-" -> (trPrefix (x:xs))

                                Ssym s -> Lproc s (trPrefixProc xs)
-- Cas qui n'arriverons jamais
                                _ -> Lnull
trPrefixProc _ = Lnull


-- Fonction qui recherche le premier mot de la liste préfixe
searchBranchLeft :: Sexp -> Sexp
searchBranchLeft (Scons Snil e2) = (e2)
searchBranchLeft (Scons e1 _) = searchBranchLeft (e1)
searchBranchLeft _ = Snil

-- Parcours préfixe de l'arbre binaire des representations 
parcoursPrefixe :: Sexp -> [Sexp]
parcoursPrefixe (Snil) = []
parcoursPrefixe (Snum n) = [Snum n]
parcoursPrefixe (Ssym s) = [Ssym s]
parcoursPrefixe (Scons e1 e2) =  parcoursPrefixe (e1) ++ parcoursPrefixe (e2)

-- Fonction qui renvoie la Lvar se trouvant dans un objet Ldo
lecteurDo1 :: Lexp -> Lexp
lecteurDo1 (Ldo a _) = a
lecteurDo1 _ = Lnull

-- Fonction qui renvoie l'élément à évaluer se trouvant dans le Ldo
lecteurDo2 :: Lexp -> Lexp
lecteurDo2 (Ldo _ b) = case b of
                Lseq [Lnum n] -> Lnum n
                Lseq (Lnum x:_) -> Lnum x
                _ -> error "argument invalide"
lecteurDo2 _ = Lnull  




-- chercher le noeud (Scons) qui contient tous les elements de la fonction 
-- definie par le def
chercherSconsDef :: Sexp -> Sexp
chercherSconsDef (Scons (Scons (Snil) (Ssym "def")) (e2)) = e2
chercherSconsDef (Scons e1 _) = chercherSconsDef e1

--Cas qui n'arriverons jamais
chercherSconsDef _ = Snil


-- cherche si l'expression contient au moin une definition de fonction.
isDefFonction :: [Sexp] -> Bool
isDefFonction [] = False
isDefFonction [Snum _] = False
isDefFonction [Ssym s] = case (length s) of
                            0 -> False
                            1 -> False
                            _ -> True
isDefFonction (x:xs) = (isDefFonction [x]) || (isDefFonction xs)


-- verifie si l'expression contient un case 
isConsFonction :: [Sexp] -> Bool
isConsFonction [] = True
isConsFonction [Snum _] = True
isConsFonction [Ssym s] = case s of 
                            "case" -> False
                            _ -> True
isConsFonction (x:xs) = (isConsFonction [x]) && (isConsFonction xs)

-- fonction qui s'occupe de parcours le sous arbre des définitions de fonction
-- et de variable, et qui retourne le lexp approprié.
lexpDefFonction :: Sexp -> Lexp
lexpDefFonction (Snum n) = Lnum n
lexpDefFonction (Ssym s) = Lvar s
lexpDefFonction (Scons e1 e2) = 
        case (parcoursPrefixe e1)!!0  of
         (Ssym se1) -> case (parcoursPrefixe e1)!!1  of
          (Snum n) -> case (parcoursPrefixe e2)!!0  of
           (Ssym se2) -> case (parcoursPrefixe e2)!!1  of
            (Ssym _) -> Ldefv se1 (Lnum n) (Ldefv se2(Lnum n) (trPrefixProc2(
                tail (parcoursPrefixe ( e2) ))))


         -- ces autres cas n'occurerons pas d'apres la structure de def dans 
         -- l'arbre de syntaxe de Sexp.
            _ -> Lnull
           _ -> Lnull
          _ -> Lnull
         _ -> Lnull
lexpDefFonction (Snil) = Lnull


-- determine la lexp d'une definition anonyme (Proc) a partir du parcours
-- prefixe de l'arbre
trPrefixProc2 :: [Sexp] -> Lexp
trPrefixProc2 [Snil] = Lnull
trPrefixProc2 [Ssym s] = Lvar s
trPrefixProc2 (x:xs) =  case x of
                                Ssym "proc" -> (trPrefixProc2 xs)
                                Ssym "*" -> (trPrefix (x:xs) )
                                Ssym "/" -> (trPrefix (x:xs) )
                                Ssym "+" -> (trPrefix (x:xs) )
                                Ssym "-" -> (trPrefix (x:xs) )

                                Ssym s -> Lproc s (trPrefixProc2 xs)
                                
                -- Cas qui n'ariverons pas
                                _ -> Lnull
trPrefixProc2 _ = Lnull

-- Fonction pour transformer une liste de Sexp en une Lexp dans
-- le cas d'une définition de variable
trPrefix :: [Sexp] -> Lexp
trPrefix [Snil] = Lnull
trPrefix (x:xs) = case x of
    Snum n -> Lnum n
    Ssym s -> if length xs == 0 
        then Lvar s 
        else case s of



-- Vérification des opérateurs dans Ldef

            "+" -> case xs!!0 of
                Ssym "+" -> Ldo (Ldo (Lvar "+") (trPrefix( 
                                    (init xs) ))) (trPrefix( [last xs] ))
                Ssym "-" -> Ldo (Ldo (Lvar "+") (trPrefix( 
                                    (init xs) ))) (trPrefix( [last xs] ))
                Ssym "/" -> Ldo (Ldo (Lvar "+") (trPrefix(
                                    (init xs) ))) (trPrefix( [last xs] ))
                Ssym "*" -> Ldo (Ldo (Lvar "+") (trPrefix(
                                    (init xs) ))) (trPrefix( [last xs] ))
                _ -> Ldo(Ldo(Lvar "+")(trPrefix([xs!!0])))((trPrefix(tail xs)))


            "-" -> case xs!!0 of
                Ssym "+" -> Ldo (Ldo (Lvar "-") (trPrefix( 
                                    (init xs) ))) (trPrefix( [last xs] ))
                Ssym "-" -> Ldo (Ldo (Lvar "-") (trPrefix( 
                                    (init xs) ))) (trPrefix( [last xs] ))
                Ssym "/" -> Ldo (Ldo (Lvar "-") (trPrefix(
                                    (init xs) ))) (trPrefix( [last xs] ))
                Ssym "*" -> Ldo (Ldo (Lvar "-") (trPrefix( 
                                    (init xs) ))) (trPrefix( [last xs] ))
                _ -> Ldo (Ldo(Lvar "-")(trPrefix([xs!!0])))((trPrefix(tail xs)))


            "/" -> case xs!!0 of
                Ssym "+" -> Ldo (Ldo (Lvar "/") (trPrefix(
                                    (init xs) ))) (trPrefix( [last xs] ))
                Ssym "-" -> Ldo (Ldo (Lvar "/") (trPrefix(
                                    (init xs) ))) (trPrefix( [last xs] ))
                Ssym "/" -> Ldo (Ldo (Lvar "/") (trPrefix( 
                                    (init xs) ))) (trPrefix( [last xs] ))
                Ssym "*" -> Ldo (Ldo (Lvar "/") (trPrefix( 
                                    (init xs) ))) (trPrefix( [last xs] ))
                _ -> Ldo(Ldo(Lvar "/")(trPrefix([xs!!0])))((trPrefix(tail xs))) 


            "*" -> case xs!!0 of
                Ssym "+" -> Ldo (Ldo (Lvar "*") (trPrefix( 
                                    (init xs) ))) (trPrefix( [last xs] ))
                Ssym "-" -> Ldo (Ldo (Lvar "*") (trPrefix( 
                                    (init xs) ))) (trPrefix( [last xs] ))
                Ssym "/" -> Ldo (Ldo (Lvar "*") (trPrefix(
                                    (init xs) ))) (trPrefix( [last xs] ))
                Ssym "*" -> Ldo (Ldo (Lvar "*") (trPrefix( 
                                    (init xs) ))) (trPrefix( [last xs] ))
                _ -> Ldo(Ldo(Lvar "*")(trPrefix([xs!!0])))((trPrefix(tail xs)))

            _ -> case xs!!1 of

                Snum _ -> Ldefv s (trPrefix([xs!!0])) (trPrefix(tail xs))

                _ -> Ldefv s (trPrefix([xs!!0])) (trPrefix(tail xs))

    -- cas qui n'arrivera jamais
    _ -> Lnull
trPrefix _ = Lnull
---------------------------------------------------------------------------
-- Représentation du contexte d'exécution                                --
---------------------------------------------------------------------------

-- Type des valeurs manipulées à l'exécution.
data Value = Vnum Int
           | Vnil
           | Vcons Value Value
           | Vop (Value -> Value)
           | Vclosure VEnv Var Lexp


-- ptet partie qui affiche une Value jj
instance Show Value where
    showsPrec p (Vnum n) = showsPrec p n
    showsPrec _ Vnil = showString "[]"
    showsPrec p (Vcons v1 v2) =
        let showTail Vnil = showChar ']'
            showTail (Vcons v1' v2') =
                showChar ' ' . showsPrec p v1' . showTail v2'
            showTail v = showString " . " . showsPrec p v . showChar ']'
        in showChar '[' . showsPrec p v1 . showTail v2
    showsPrec _ (Vop _) = showString "<primitive>"
    showsPrec _ (Vclosure _ _ _) = showString "<fonction>"

type VEnv = [(Var, Value)]

-- L'environnement initial qui contient les fonctions prédéfinies.
env0 :: VEnv
env0 = let binop :: (Int -> Int -> Int) -> Value
           binop op = Vop (\v1 -> case v1 of
                                   (Vnum n1)
                                    -> Vop (\v2 -> case v2 of
                                                    (Vnum n2)
                                                     -> Vnum (n1 `op` n2)
                                                    _ -> error "Pas un nombre")
                                   _ -> error "Pas un nombre")

          in [("+", binop (+)),
              ("*", binop (*)),
              ("/", binop div),
              ("-", binop (-))]

---------------------------------------------------------------------------
-- Évaluateur                                                            --
---------------------------------------------------------------------------

eval :: VEnv -> Lexp -> Value
-- ¡¡ COMPLETER !!

-- Évaluation d'une expression de type Lnum
eval _ (Lnum n) = Vnum n


-- Évaluer variable en regardant la reference dans l'environnement statique
eval env (Lvar x)= case lookup x env of
    Just v -> v
    Nothing -> error ("Variable " ++ x ++ " non definie")

-- Évaluation d'une expression de type Ldo 
eval env (Ldo e1 e2) = case eval env e1 of
    Vop f -> f (eval env e2)
    Vclosure env' var lexp -> let ve2 = eval env' e2 in case ve2 of
                                    Vnil ->   eval env' lexp
                                    Vnum _ -> eval ((var,eval env' e2): env') lexp
                                    _ -> Vnil -- Cas impossible
    _ -> error "pas une fonction"

-- Évaluation d'une déclaration
eval env (Ldefv x e1 e2) = case e1 of
    Lproc var lexp -> eval((x,Vclosure env var lexp):env) e2

    _ -> let v1 = eval env e1
             env' = (x, v1) : env
        in eval env' e2
--

-- Évaluation d'une expression de type Lproc
eval env (Lproc var lexp) = Vclosure env var lexp

-- Évaluation d'une expression vide
eval _ (Lnull) = Vnil

-- Évaluation d'une séquence
eval _ (Lseq e) = lexpAnode e

-- Évaluation d'une node
eval _ (Lnode e) = lexpAnode e

-- Évaluation d'un case
-- Si le premier élément de Lcase est une liste vide , on exécute
-- l'évaluation de l'expresion dans l'élément de type (null e)
eval env (Lcase (Lnode [Lnull]) (Lnum ex) _ _ _) = eval env (Lnum ex)
-- Sinon on évalue l'expression conditionelle du dernier Lexp
-- avec comme argument le reste de la liste fournie
eval env (Lcase _ _ _ _ e) = let nom = lecteurDo2(e) in
                             let valeur = lecteurDo1(e) in
                             eval env (Ldo valeur nom)

eval _ _ = Vnil
---------------------------------------------------------------------------
-- Toplevel                                                              --
---------------------------------------------------------------------------

evalSexp :: Sexp -> Value
evalSexp = eval env0 . s2l

-- Lit un fichier contenant plusieurs Sexps, les évalues l'une après
-- l'autre, et renvoie la liste des valeurs obtenues.
run :: FilePath -> IO ()
run filename =
    do inputHandle <- openFile filename ReadMode 
       hSetEncoding inputHandle utf8
       s <- hGetContents inputHandle
       (hPutStr stdout . show)
           (let sexps s' = case parse pSexps filename s' of
                             Left _ -> [Ssym "#<parse-error>"]
                             Right es -> es
            in map evalSexp (sexps s))
       hClose inputHandle

sexpOf :: String -> Sexp
sexpOf = read

lexpOf :: String -> Lexp
lexpOf = s2l . sexpOf

valOf :: String -> Value 
valOf = evalSexp . sexpOf 

