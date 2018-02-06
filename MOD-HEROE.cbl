       IDENTIFICATION DIVISION.
       PROGRAM-ID. MOD-HEROE.
      ******************************************************************
      * JUEGO: DUNGEON CRAWLER                                          *
      ******************************************************************
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-CONSTANTS.
           05 WS-GAME-NAME PIC X(15)   VALUE "Dungeon Crawler".
       01 HEROES-MENU.
           05 WS-H-OPTION            PIC X(01) VALUE SPACE.
               88 WS-H-OP-CONTINUE     VALUE SPACE.
               88 WS-H-OP-EXIT         VALUE "0".
               88 WS-H-OP-STRENGTH     VALUE "1".
               88 WS-H-OP-AGILITY     VALUE "2".
               88 WS-H-OP-LEVEL       VALUE "3".
               88 WS-H-OP-HP       VALUE "4".
           05 H-MENU-TITLE.
               10 WS-HM-HEADING    PIC X(17)
                 VALUE "MODIFICAR HEROE: ".
               10 FILLER           PIC X(01) VALUE X"0A".
               10 FILLER           PIC X(17) VALUE ALL "-".
           05 H-MENU-CONTENT.
               10 STRENGTH         PIC X(11) VALUE "1- Fuerza: ".
               10 FILLER           PIC X(01) VALUE X"0A".
               10 AGILITY          PIC X(13) VALUE "2- Agilidad: ".
               10 FILLER           PIC X(01) VALUE X"0A".
               10 LEVEL            PIC X(10) VALUE "3- Nivel: ".
               10 FILLER           PIC X(01) VALUE X"0A".
               10 HP               PIC X(16) VALUE "4- Puntos Vida: ".
           05 H-MENU-FOOTER.
               10 FILLER           PIC X(01) VALUE X"0A".
               10 FILLER           PIC X(09) VALUE "0- Salir".
               10 FILLER           PIC X(01) VALUE X"0A".
               10 FILLER           PIC X(01) VALUE X"0A".
               10 FILLER           PIC X(21)
                 VALUE "Escoge una opcion: ".
       01 NEW-VALUES-HEROE.
           05 NEW-VALUE         PIC 9(02).
      ******************************************************************
       LINKAGE SECTION.
       01 LS-HEROE.
           05 LS-H-ID                PIC 9(02).
           05 LS-H-STRENGTH          PIC 9(02).
           05 LS-H-AGILITY           PIC 9(02).
           05 LS-H-LEVEL             PIC 9(02).
           05 LS-H-HP                PIC S9(02).
      ******************************************************************
       PROCEDURE DIVISION USING LS-HEROE.
       MAIN-PROCEDURE.
           PERFORM DISPLAY-H-ALL-CONTENTS.
           PERFORM DISPLAY-HEROES-MENU UNTIL WS-H-OP-EXIT.
           EXIT PROGRAM.
      ******************************************************************
       DISPLAY-H-ALL-CONTENTS.
           DISPLAY H-MENU-TITLE.
           DISPLAY STRENGTH LS-H-STRENGTH.
           DISPLAY AGILITY LS-H-AGILITY.
           DISPLAY LEVEL LS-H-LEVEL.
           DISPLAY HP NO ADVANCING.
           IF LS-H-HP < 0 THEN
               DISPLAY "00"
           ELSE
               DISPLAY LS-H-HP
           END-IF.
           DISPLAY H-MENU-FOOTER.
           ACCEPT WS-H-OPTION.
      ******************************************************************
       DISPLAY-HEROES-MENU.
           EVALUATE TRUE
               WHEN WS-H-OP-STRENGTH
                   DISPLAY "[" WS-GAME-NAME "] "
                     "Selecciona el nuevo valor de Fuerza: "
                   DISPLAY "- Valor antiguo: " LS-H-STRENGTH
                   ACCEPT NEW-VALUE
                   DISPLAY "- Nuevo valor: " NEW-VALUE
                   SET LS-H-STRENGTH TO NEW-VALUE
                   PERFORM DISPLAY-H-ALL-CONTENTS
               WHEN WS-H-OP-AGILITY
                   DISPLAY "[" WS-GAME-NAME "] "
                     "Selecciona el nuevo valor de Agilidad: "
                   DISPLAY "- Valor antiguo: " LS-H-AGILITY
                   ACCEPT NEW-VALUE
                   DISPLAY "- Nuevo valor: " NEW-VALUE
                   SET LS-H-AGILITY TO NEW-VALUE
                   PERFORM DISPLAY-H-ALL-CONTENTS
               WHEN WS-H-OP-LEVEL
                   DISPLAY "[" WS-GAME-NAME "] "
                     "Selecciona el nuevo valor de Nivel: "
                   DISPLAY "- Valor antiguo: " LS-H-LEVEL
                   ACCEPT NEW-VALUE
                   DISPLAY "- Nuevo valor: " NEW-VALUE
                   SET LS-H-LEVEL TO NEW-VALUE
                   PERFORM DISPLAY-H-ALL-CONTENTS
               WHEN WS-H-OP-HP
                   DISPLAY "[" WS-GAME-NAME "] "
                     "Selecciona el nuevo valor de Vida: "
                   DISPLAY "- Valor antiguo: " LS-H-HP
                   ACCEPT NEW-VALUE
                   DISPLAY "- Nuevo valor: " NEW-VALUE
                   SET LS-H-HP TO NEW-VALUE
                   PERFORM DISPLAY-H-ALL-CONTENTS
               WHEN WS-H-OP-EXIT
                    EXIT PROGRAM
           END-EVALUATE.
