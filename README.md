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

- Para generar laberintos, utiliza la función `generate` pasando el número de filas, columnas y el nombre del archivo donde se guardará el laberinto.

```lisp
(generate 20 30 "laberinto1.txt")
```

- Para jugar, llama a la función `play` pasando como parámetro el archivo del laberinto que deseas jugar. Si el archivo no existe, se generará automáticamente un laberinto por defecto 25x25.

```lisp
(play "laberinto1.txt")
```

- Para consultar estadísticas del laberinto, utiliza la función `stats` con el archivo correspondiente.

```lisp
(stats "laberinto1.txt")
```

---

## Aspectos opcionales

- Generar el laberinto con la entrada y la salida en los bordes de este, sin permitir que aparezcan en la esquina, además de aparecer en esquinas opuestas para estar alejados.

- Generar laberinto no cuadrado, con diferentes número de filas y columnas y estas personalizables por el usuario. El mínimo de ambas dimensiones es 10 y el máximo es 50.

---

## Breve explicación

_TODO: Revisar la teoría y el resultado_
