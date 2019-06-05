
type Set a = [a]
type Usuario = (Integer, String)
type Relacion = (Usuario, Usuario)
type Publicacion = (Usuario, String, Set Usuario)
type RedSocial = (Set Usuario, Set Relacion, Set Publicacion)

usuarios :: RedSocial -> Set Usuario
usuarios (a,b,c)=a

relaciones :: RedSocial -> Set Relacion
relaciones (a,b,c)=b

publicaciones :: RedSocial -> Set Publicacion
publicaciones (a,b,c)=c

idDeUsuario :: Usuario -> Integer
idDeUsuario (a,b)= a

nombreDeUsuario :: Usuario -> String
nombreDeUsuario (a,b)=b

usuarioDePublicacion :: Publicacion -> Usuario
usuarioDePublicacion  (a,b,c)= a

likesDePublicacion :: Publicacion -> Set Usuario  
likesDePublicacion (a,b,c)=c

nombresDeUsuarios :: RedSocial -> Set String
nombresDeUsuarios ([a],b,c)= a
nombresDeUsuarios (a:as,b,c) = a : nombresDeUsuarios(as,b,c)

