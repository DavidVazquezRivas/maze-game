;; Proyecto Laberinto
;;
;; Autores: Nasim Hosam Benyacoub Terki & David Vázquez Rivas
;; Fecha: 26-06-2025
;; Asignatura: Lenguajes de programación
;; Convocatoria: Extraordinaria
;; Profesores: Antoni Oliver Tomàs & Francesc Xavier Gaya Morey
;;
;; ---
;;
;; Indicaciones
;;
;; - Para iniciar la aplicación, carga el punto de entrada con (load "main.lsp") desde la ruta correspondiente (/src/main.lsp).
;; 
;;   (load "main.lsp")
;;
;; - Para generar laberintos, utiliza la función create-maze pasando el número de filas, columnas y el nombre del archivo donde se guardará el laberinto.
;;
;;   (create-maze "laberinto1.txt" 20 30)
;;
;; - Para jugar, llama a la función start-game pasando como parámetro el archivo del laberinto que deseas jugar. Si el archivo no existe, se generará automáticamente un laberinto por defecto 25x25.
;;
;;   (start-game "laberinto1.txt")
;;
;; - Para consultar estadísticas del laberinto, utiliza la función display-stats con el archivo correspondiente.
;;
;;   (display-stats "laberinto1.txt")
;;
;; ---
;;
;; Aspectos opcionales
;;
;; - Entrada y salida en los bordes, evitando esquinas y en esquinas opuestas.
;; - Laberintos no cuadrados, dimensiones personalizables (mínimo 10, máximo 45).
;; - Modos "viewport" y "minimapa" para visión parcial o completa.
;; - Añadir "cola" en casillas visitadas.
;; - Mostrar UI con instrucciones junto al laberinto.
;; - Configuraciones desde /src/const/constants.lsp (controles, colores, viewport, clasificación...).
;; - Clasificación por tiempo además de por pasos.
;;
;; ---
;;
;; Breve explicación
;;
;; Estructura del código:
;; - /data: archivos generados y usados (laberintos, estadísticas).
;; - /src: código fuente organizado en módulos:
;;    - main.lsp: punto de entrada.
;;    - const/constants.lsp: constantes configurables.
;;    - core/: utilidades generales.
;;    - graphics/: funciones de dibujo y UI.
;;    - maze/: lógica del laberinto.
;;    - game/: gestión de partida e interacción.
;;    - stats/: estadísticas y almacenamiento.
;;
;; Estructuras de datos:
;; - Cell: lista con campos type, visible (no usado), current, visited.
;; - Maze: lista con grid (matriz de celdas), current-row, current-col, minimap.
;; Cambios de estado mediante funciones puras, sin mutabilidad directa.
;;
;; Generación del laberinto:
;; 1. Usuario indica tamaño y archivo.
;; 2. Validación de tamaño (10-45).
;; 3. Inicializar grid con paredes y current en (0,0).
;; 4. Selección aleatoria del borde para la entrada (evitando esquinas).
;; 5. Colocación de entrada y actualización de current.
;; 6. Colocación de salida en borde opuesto (evitando esquinas).
;; 7. Algoritmo recursivo para crear caminos, reinicia si no conecta la salida (~10% casos).
;; 8. Codificación de cada celda en caracteres.
;; 9. Creación del archivo de laberinto.
;; 10. Generación de archivo de estadísticas.
;;
;; Pintado del laberinto:
;; 1. Pintar instrucciones una vez al inicio.
;; 2. Pintar laberinto según modo (minimapa o viewport).
;;    - Calcular márgenes para centrar.
;;    - Obtener área visible (completa o submatriz).
;;    - Pintar fila a fila.
;;    - Determinar color según tipo y estado.
;;    - Pintar solo si ha cambiado (optimización con caché).
;;    - Uso de variable global para último estado pintado (mejora rendimiento).
;;
;; Ejecución del juego:
;; 1. Limpiar pantalla y pintar instrucciones básicas.
;; 2. Iniciar cronómetro para estadísticas.
;; 3. Comprobar si jugador llegó a la meta; si sí, finalizar.
;; 4. Leer tecla:
;;    - Si movimiento, actualizar posición, repintar y repetir desde paso 3.
;;    - Si escape, terminar ejecución.
;; 5. Al terminar, guardar estadísticas (tiempo y pasos).
;; 6. Mostrar clasificación actualizada (top N) del laberinto jugado.

;; CONSTANTS
(load "./const/constants.lsp")

;; CORE
(load "./core/file.lsp")
(load "./core/utils.lsp")
(load "./core/cell.lsp")

;; MAZE
(load "./maze/maze.lsp")
(load "./maze/generate.lsp")

;; GRAPHICS
(load "./graphics/graphics.lsp")

;; STATS
(load "./stats/stats.lsp")

;; GAME
(load "./game/game.lsp")