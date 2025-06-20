# Proyecto Laberinto

**Autores:** Nasim Hosam Benyacoub Terki & David Vázquez Rivas  
**Fecha:** 26-06-2025  
**Asignatura:** Lenguajes de programación  
**Convocatoria:** Extraordinaria  
**Profesores:** Antoni Oliver Tomàs & Francesc Xavier Gaya Morey

---

## Indicaciones

- Para iniciar la aplicación, carga el punto de entrada con `(load "main.lsp")` desde la ruta correspondiente (`/src/main.lsp`).

```lisp
(load "main.lsp")
```

- Para generar laberintos, utiliza la función `create-maze` pasando el número de filas, columnas y el nombre del archivo donde se guardará el laberinto.

```lisp
(create-maze "laberinto1.txt" 20 30)
```

- Para jugar, llama a la función `start-game` pasando como parámetro el archivo del laberinto que deseas jugar. Si el archivo no existe, se generará automáticamente un laberinto por defecto 25x25.

```lisp
(start-game "laberinto1.txt")
```

- Para consultar estadísticas del laberinto, utiliza la función `display-stats` con el archivo correspondiente.

```lisp
(display-stats "laberinto1.txt")
```

---

## Aspectos opcionales

- Generar el laberinto con la entrada y la salida en los bordes de este, sin permitir que aparezcan en la esquina, además de aparecer en esquinas opuestas para estar alejados.

- Generar laberinto no cuadrado, con diferentes número de filas y columnas y estas personalizables por el usuario. El mínimo de ambas dimensiones es 10 y el máximo es 45.

- Modo "viewport" y "minimapa", de forma que se pueda jugar teniendo una visión de 10x10 alrededor del jugador o con el laberinto completo.

- Añadir "cola" en las casillas por las que ha pasado el jugador recorriendo el laberinto.

- Mostrar no únicamente el laberinto sino una pequeña UI con las instrucciones.

- Gran parte de las funcionalidades son configurables desde el archivo `/src/const/constants.lsp`, como por ejemplo los controles, las instrucciones que recibe el usuario. Los colores del laberinto, tamaño del viewport o número de partidas a mostrar en la clasificación, entre otros.

- Incluir clasificación por tiempo además de la clasificación predeterminada por número de pasos utilizados.

---

## Breve explicación

_TODO: Revisar la teoría y el resultado_
