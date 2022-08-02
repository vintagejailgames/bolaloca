# TSUNAMI (LA BOLA LOCA)
Copyright (c) 1999 JAIL GAMER ENTERTAINMENT, All rights reserved
<img width="752" src="https://user-images.githubusercontent.com/110221325/182409432-b4db5196-44b0-4d2a-8afe-d1cac9379e0f.png">
## Tecles

### Menú:

 * `W` amunt
 * `S` avall
 * `Enter` comnençar
 * `Esc` Eixir de joc (sols despres d'haver començat una partida)
	
### Jugador 1:
 * `W` amunt
 * `S` avall
 * `A` enrrere
 * `D` avant

### Jugador 2:
 * `cursor amunt` amunt
 * `cursor avall` avall
 * `cursor dreta` enrrere
 * `cursor esquerra` avant
          
## Regles 
 * Enviar la pilota a la porteria del rival.
 * No exir del area.
 * No retindre la pilota a la teua area

## Taules propies
### Creant

La taula te el seguent format:
 * Arxiu, format PCX amb 256 colors i 320x200 pixels
 * Un borde de taula de tres pixel
 * Dos proteries de tres pixels d'ample situades sobre el borde en (0,72) i (317,72)
 * Dos arees de 68x195 pixels situades en (3,2) i (250,2)
 * A mes a mes pots possar qualevol dibuix que et semble.
    
### Jugant

Per jugar en la propia taula, quan vajes a jugar has de possar:
       
``` bola [nom de la taula] ```

Per exemple, si tenim una taula que es diu `"Espai.pcx"` per jugar possare:
        
``` bola espai.pcx ```

```
    Nota: Quan carregem una taula propia sols podrem jugar en eixa taula.
```
<img width="752" src="https://user-images.githubusercontent.com/110221325/182409653-735a47e1-3d6c-4df0-b782-b64963b3ab70.png">

## Com jugar hui en dia

Amb DosBox. Pareix que funciona correcte sense aumentar cicles.

## Com compilar hui en dia

Turbo Pascal 7 desde DosBox. Activar "286 Instructions" i "8087/80287", en "Options -> Compiler"

![turbopascalx86](https://user-images.githubusercontent.com/110221325/181739514-656e6aa9-eda0-4f85-b6a5-43e1558f080a.png)
