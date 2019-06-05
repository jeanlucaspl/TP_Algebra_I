-------------------------TRABAJO PRÁCTICO ÁLGEBRA I ----------------------

type Set a = [a]
type Usuario = (Integer, String)
type Relacion = (Usuario, Usuario)
type Publicacion = (Usuario, String, Set Usuario)
type RedSocial = (Set Usuario, Set Relacion, Set Publicacion)

---------------------------------------------------------------------------

usuarios :: RedSocial -> Set Usuario

usuarios (a,b,c)=a


---------------------------------------------------------------------------

relaciones :: RedSocial -> Set Relacion

relaciones (a,b,c)=b

---------------------------------------------------------------------------

publicaciones :: RedSocial -> Set Publicacion

publicaciones (a,b,c)=c

---------------------------------------------------------------------------

idDeUsuario :: Usuario -> Integer

idDeUsuario (a,b)= a


---------------------------------------------------------------------------

nombreDeUsuario :: Usuario -> String
nombreDeUsuario (a,b)=b

---------------------------------------------------------------------------

usuarioDePublicacion :: Publicacion -> Usuario

usuarioDePublicacion  (a,b,c)= a


---------------------------------------------------------------------------

likesDePublicacion :: Publicacion -> Set Usuario  

likesDePublicacion (a,b,c)=c


---------------------------------------------------------------------------

nombresDeUsuarios :: RedSocial -> Set String
nombresDeUsuarios ([],b,c)=[]
nombresDeUsuarios ([(x,y)],b,c)= y: []

nombresDeUsuarios ((x,y):as,b,c) = y : nombresDeUsuarios(as,b,c)

---------------------------------------------------------------------------
amigosDe :: RedSocial -> Usuario -> Set Usuario
amigosDe (a,[],c) u=[]
amigosDe (a,(x,y):bs,c) u | u==x = y : (amigosDe(a,bs,c) u)
			  | u==y = x : (amigosDe(a,bs,c) u)	
			  | otherwise= (amigosDe(a,bs,c) u)

---------------------------------------------------------------------------

------------Función Aux-------------------
contarSetUsuario :: Set Usuario -> Int
contarSetUsuario []=0
contarSetUsuario [x]=1
contarSetUsuario (x:xs)= 1 + contarSetUsuario(xs)
------------------------------------------

cantidadDeAmigos :: RedSocial -> Usuario -> Int
	
cantidadDeAmigos (a,b,c) u = contarSetUsuario (amigosDe (a,b,c) u)

---------------------------------------------------------------------------

------------Función Aux-------------------
usuarioConMasAmigosL :: RedSocial -> Set Usuario
usuarioConMasAmigosL([],_,_)=[(0,"No Hay Usuarios")]
usuarioConMasAmigosL ([x],b,c)= [x]
usuarioConMasAmigosL ((x:y:as),b,c) | (cantidadDeAmigos((x:y:as),b,c) x)>=(cantidadDeAmigos((x:y:as),b,c) y) = usuarioConMasAmigosL(x:as,b,c)
				    | otherwise= usuarioConMasAmigosL(y:as,b,c)	
------------------------------------------

usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos (a,b,c)= head (usuarioConMasAmigosL(a,b,c))

---------------------------------------------------------------------------

estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos ([],b,c) = False
estaRobertoCarlos (x:as,b,c) | (cantidadDeAmigos (x:as,b,c) x) > 1000000 = True 
                             | otherwise = False || (estaRobertoCarlos(as,b,c))

---------------------------------------------------------------------------

publicacionesDe :: RedSocial -> Usuario -> Set Publicacion
publicacionesDe (a,b,[]) u= []
publicacionesDe (a,b,(x,y,z):cs) u | x==u = (x,y,z) : (publicacionesDe (a,b,cs) u)
                                   | otherwise = publicacionesDe (a,b,cs) u

---------------------------------------------------------------------------

------------Función Aux-------------------

dioLike :: Publicacion -> Usuario -> Bool
dioLike (a,b,[]) u = False
dioLike (a,b,(z:cs)) u | z==u = True
		       |otherwise = False || (dioLike (a,b,cs) u) 	

------------------------------------------

publicacionesQueLeGustanA :: RedSocial -> Usuario -> Set Publicacion
publicacionesQueLeGustanA (a,b,[]) u=[]
publicacionesQueLeGustanA (a,b,x:cs) u | (dioLike x u)==True = x : (publicacionesQueLeGustanA (a,b,cs) u) 
                                       | otherwise = publicacionesQueLeGustanA (a,b,cs) u      

---------------------------------------------------------------------------

--hay que suponer que no pueden haber publicaciones repetidas?

------------Función Aux-------------------

contElemSetPub :: Set Publicacion -> Integer
contElemSetPub [] = 0
contElemSetPub (x:xs) = 1 + contElemSetPub (xs) 

estaEnSetPub :: Publicacion -> Set Publicacion -> Bool 
estaEnSetPub x [] = False
estaEnSetPub x (y:ys) | x == y = True
		      | otherwise= False || (estaEnSetPub x ys)	 

comparaPub :: Set Publicacion -> Set Publicacion -> Bool
comparaPub [] y = True
comparaPub (x:xs) y | ((contElemSetPub (x:xs) == contElemSetPub y) && (estaEnSetPub x y))==True = True && (comparaPub xs y) 
                    | otherwise= False
------------------------------------------

lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool

lesGustanLasMismasPublicaciones x u v = comparaPub (publicacionesQueLeGustanA x u) (publicacionesQueLeGustanA x v)

---------------------------------------------------------------------------

--Uso publicacionesDe

------------Función Aux-------------------

dioLikeAtodas :: Set Publicacion -> Usuario -> Bool
dioLikeAtodas [] u = True
dioLikeAtodas (x:xs) u | (dioLike x u == True) = True &&  (dioLikeAtodas xs u)
		       | otherwise = False

------------------------------------------

tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel ([],b,c) u = False

tieneUnSeguidorFiel (a:as,b,c) u | ((a /= u) && (dioLikeAtodas (publicacionesDe (a:as,b,c) u) a))== True= True || (tieneUnSeguidorFiel (as,b,c) u) 
                                 | otherwise = False || (tieneUnSeguidorFiel (as,b,c) u) 	
