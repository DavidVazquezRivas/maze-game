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

### Estructura del código

El proyecto está organizado en dos directorios principales: `/data` y `/src`.  
- El directorio `/data` contiene los archivos generados o utilizados por la aplicación (laberintos, estadísticas, etc.).
- El directorio `/src` alberga el código fuente, dividido en módulos para facilitar la organización y la comprensión del flujo de la aplicación.

Dentro de `/src`, los archivos se estructuran de la siguiente manera:

- **`main.lsp`**: punto de entrada de la aplicación. Se encarga de cargar todos los módulos necesarios y poner en marcha el sistema.
- **`const/constants.lsp`**: contiene todas las constantes configurables, como controles, colores del laberinto, tamaño del viewport, o el número de partidas mostradas en la clasificación.
- **`core/`**: conjunto de utilidades y funciones generales que se utilizan de forma transversal en distintos módulos.
- **`graphics/`**: incluye funciones de representación visual, como el dibujo del laberinto o la UI básica.
- **`maze/`**: contiene la lógica relacionada con la estructura y generación del laberinto.
- **`game/`**: gestiona la ejecución de una partida, incluyendo el movimiento del jugador y la interacción con el laberinto.
- **`stats/`**: se encarga del almacenamiento, recuperación y visualización de estadísticas de juego.

Cada módulo ha sido diseñado para ser lo más autónomo posible, favoreciendo una arquitectura modular y mantenible. Los nombres de los archivos y funciones son descriptivos para facilitar su comprensión y navegación por el código.

### Estructuras de datos

Aunque el proyecto sigue un enfoque puramente funcional, se han definido estructuras de datos organizadas para mantener el código limpio, legible y fácilmente extensible. Estas estructuras se gestionan en un archivo dedicado, lo que permite centralizar su definición y manejo sin recurrir a programación orientada a objetos (OOP), aunque algunas decisiones de diseño recuerdan a dicha filosofía por motivos prácticos de organización.

Las principales estructuras utilizadas son:

- **Celda (`Cell`)**: representa una unidad del laberinto. Se define como una lista con los siguientes campos:
  - `type`: cadena que indica el tipo de celda (por ejemplo, muro, camino, entrada, salida...).
  - `visible`: booleano que indica si la celda es visible para el jugador. **Este campo quedó sin uso, ya que finalmente no se implementó la funcionalidad de "fog of war".**
  - `current`: booleano que señala si la celda está actualmente activa (jugador o celda en proceso de generación).
  - `visited`: booleano que marca si la celda ya ha sido recorrida.

- **Laberinto (`Maze`)**: estructura que representa el estado completo del laberinto. También se define como una lista con los siguientes elementos:
  - `grid`: una lista bidimensional de celdas (`Cell`), que forma el mapa del laberinto.
  - `current-row`: fila de la celda activa (jugador o proceso de generación).
  - `current-col`: columna de la celda activa.
  - `minimap`: booleano que indica si el minimapa está activado.

Estas estructuras permiten mantener la lógica funcional sin mutabilidad directa, y todo cambio de estado se maneja a través de funciones puras que devuelven nuevas versiones modificadas de las estructuras.


### Generación del laberinto

El proceso de generación del laberinto sigue una serie de pasos definidos que garantizan una estructura válida y jugable, respetando restricciones de tamaño y posicionamiento. El procedimiento completo es el siguiente:

1. **Entrada del usuario**: el usuario llama a la generación del laberinto indicando tamaño (número de filas y columnas) y archivo a guardar.
2. **Validación del tamaño**: se comprueba que las dimensiones estén dentro de los límites permitidos (mínimo 10, máximo 45). En caso contrario, se ajustan automáticamente al valor permitido más cercano.
3. **Inicialización del grid**: se genera una cuadrícula vacía compuesta únicamente por celdas de tipo *pared*, y se inicializa la posición `current` en (0,0).
4. **Selección de entrada**: se elige aleatoriamente uno de los bordes del laberinto (norte, sur, este u oeste) para posicionar la entrada.
5. **Colocación de la entrada**: se escoge una casilla aleatoria del borde seleccionado, evitando esquinas, y se marca como entrada. La posición `current` del laberinto se actualiza a esta celda.
6. **Colocación de la salida**: se selecciona una casilla aleatoria del borde opuesto (también evitando esquinas) y se marca como salida.
7. **Algoritmo de generación del camino**:
   7.1. Desde la celda `current`, se obtienen sus vecinos candidatos.
   7.2. La lista de vecinos se mezcla aleatoriamente para generar trayectorias diferentes en cada ejecución.
   7.2. Se exploran los vecinos de forma recursiva:
     - Si un vecino tiene como máximo un vecino marcado como camino y no está en el borde, se acepta como nuevo camino.
     - El `current` se mueve a dicha celda, que se marca como camino, y se repite el proceso.
   7.4. En caso de que no se logre conectar con la salida (situación que ocurre en aproximadamente un 10% de los casos), se reinicia la generación. Esta comprobación es necesaria debido a que se fuerza la salida en un borde opuesto, lo que puede generar configuraciones sin solución válida.
8. **Codificación del laberinto**: se transforma cada celda del grid en su carácter correspondiente, según el tipo asignado (pared, camino, entrada, salida...).
9. **Creación del archivo**: se genera un archivo de texto que contiene el laberinto codificado.
10. **Generación de estadísticas**: se crea un archivo adicional para almacenar las estadísticas del laberinto generado.

### Pintado del laberinto

1. **Pintado de las instrucciones**: Se dibujan una vez al iniciar la partida y no se repintan para evitar trabajo gráfico innecesario.

2. **Pintado del laberinto**: Depende del modo de visualización activo.

   2.1. **Cálculo de márgenes**: se calcula el espacio para centrar el laberinto horizontal y verticalmente, manteniendo la proporción cuadrada.

   2.2. **Determinación del área visible**  
   - En **modo minimapa**: se pinta todo el laberinto.  
   - En **modo viewport**: se obtiene una submatriz fija (ej. 10x10) centrada en la posición del jugador.

   2.3. **Pintado del grid**: se pinta el área visible fila por fila, de forma recursiva.

   2.4. **Cálculo del color de cada celda**: se define según el tipo de celda, si es la celda activa/visitada y la configuración en las constantes.

   2.5. **Renderizado de la celda**: se pinta solo si el estado visual de la celda cambió desde la última vez.

   2.6. **Optimización mediante caché**: Se almacena en una variable global (mediante `setq`) el estado del último grid pintado.  
     Aunque esto rompe parcialmente con el paradigma funcional, esta decisión se tomó por motivos de rendimiento. En laberintos grandes, especialmente en modo minimapa, redibujar toda la estructura en cada ciclo tiene un coste significativo. Con este enfoque, solo se repintan las celdas que realmente han cambiado, lo que reduce drásticamente la carga gráfica sin afectar la lógica funcional del juego.

### Ejecución del juego

El flujo de ejecución del juego es el siguiente:

1. Limpiar la pantalla y pintar las instrucciones básicas.  
2. Iniciar el cronómetro para el registro de estadísticas.  
3. Comprobar si la posición actual del jugador coincide con la meta (salida del laberinto).  
   - Si es así, finalizar la partida.  
4. Leer la tecla pulsada por el usuario.  
   - Si es una tecla de movimiento, actualizar la posición del jugador en la dirección indicada, repintar el laberinto y volver al paso 3.  
   - Si es la tecla de escape, terminar la ejecución del juego.  
5. Al finalizar la partida, guardar las estadísticas: tiempo transcurrido y número de pasos realizados.  
6. Mostrar la clasificación actualizada (top N) con las estadísticas del laberinto jugado.
