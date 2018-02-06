       IDENTIFICATION DIVISION.
       PROGRAM-ID. MOD-HEROE.
      ******************************************************************
      * JUEGO: DUNGEON CRAWLER                                          *
      ******************************************************************
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-CONSTANTS.
           05 WS-GAME-NAME             PIC X(15)
                                       VALUE "Dungeon Crawler".
       01 WS-VALID-OPTION              PIC X(28)   VALUES ALL SPACES.
           88 WS-RESET-VALID-OPTION      VALUE ALL SPACES.
           88 WS-INVALID-OPTION
             VALUE "[Escoge una opcion correcta]".
       01 WS-HEROES-MENU.
           05 WS-HM-OPTION             PIC X(01) VALUE SPACE.
               88 WS-HM-OP-CONTINUE      VALUE SPACE.
               88 WS-HM-OP-EXIT          VALUE "0".
               88 WS-HM-OP-STRENGTH      VALUE "1".
               88 WS-HM-OP-AGILITY       VALUE "2".
               88 WS-HM-OP-LEVEL         VALUE "3".
               88 WS-HM-OP-HP            VALUE "4".
           05 WS-HM-TITLE.
               10 FILLER               PIC X(17)
                                         VALUE "MODIFICAR HEROE: ".
               10 WS-HM-ERROR          PIC X(28) VALUE ALL SPACES.
               10 FILLER               PIC X(01) VALUE X"0A".
               10 FILLER               PIC X(17) VALUE ALL "-".
           05 WS-HM-CONTENT.
               10 FILLER               PIC X(11) VALUE "1- Fuerza: ".
               10 WS-HM-C-STRENGTH     PIC 9(02) VALUE ZEROES.
               10 FILLER               PIC X(01) VALUE X"0A".
               10 FILLER               PIC X(13) VALUE "2- Agilidad: ".
               10 WS-HM-C-AGILITY      PIC 9(02) VALUE ZEROES.
               10 FILLER               PIC X(01) VALUE X"0A".
               10 FILLER               PIC X(10) VALUE "3- Nivel: ".
               10 WS-HM-C-LEVEL        PIC 9(02) VALUE ZEROES.
               10 FILLER               PIC X(01) VALUE X"0A".
               10 FILLER               PIC X(16)
                                         VALUE "4- Puntos Vida: ".
               10 WS-HM-C-HP           PIC 9(02) VALUE ZEROES.
           05 WS-HM-FOOTER.
               10 FILLER               PIC X(01) VALUE X"0A".
               10 FILLER               PIC X(09) VALUE "0- Salir".
               10 FILLER               PIC X(01) VALUE X"0A".
               10 FILLER               PIC X(01) VALUE X"0A".
               10 FILLER               PIC X(21)
                                         VALUE "Escoge una opcion: ".
      ******************************************************************
       LINKAGE SECTION.
       01 LS-HEROE.
           05 LS-H-ID                  PIC 9(02).
           05 LS-H-STRENGTH            PIC 9(02).
           05 LS-H-AGILITY             PIC 9(02).
           05 LS-H-LEVEL               PIC 9(02).
           05 LS-H-HP                  PIC S9(02).
      ******************************************************************
       PROCEDURE DIVISION USING LS-HEROE.
       MAIN-PROCEDURE.
           SET WS-HM-OP-CONTINUE TO TRUE.
           SET WS-RESET-VALID-OPTION TO TRUE
           PERFORM DISPLAY-MENU UNTIL WS-HM-OP-EXIT.
           EXIT PROGRAM.
      ******************************************************************
       DISPLAY-MENU.
           MOVE LS-H-STRENGTH TO WS-HM-C-STRENGTH.
           MOVE LS-H-AGILITY TO WS-HM-C-AGILITY.
           MOVE LS-H-LEVEL TO WS-HM-C-LEVEL.
           IF LS-H-HP < 0 THEN
               MOVE 0 TO WS-HM-C-HP
           ELSE
               MOVE LS-H-HP TO WS-HM-C-HP
           END-IF.

           PERFORM SET-MENU-TO-ERROR.
           DISPLAY WS-HM-TITLE.
           DISPLAY WS-HM-CONTENT.
           DISPLAY WS-HM-FOOTER.

           SET WS-RESET-VALID-OPTION TO TRUE
           ACCEPT WS-HM-OPTION.

           EVALUATE TRUE
               WHEN WS-HM-OP-STRENGTH
                   DISPLAY "["WS-GAME-NAME"] "
                     "Selecciona el nuevo valor de Fuerza: "
                   DISPLAY "- Valor antiguo: "WS-HM-C-STRENGTH
                   DISPLAY "- Nuevo valor: "
                   ACCEPT LS-H-STRENGTH

               WHEN WS-HM-OP-AGILITY
                   DISPLAY "["WS-GAME-NAME"] "
                     "Selecciona el nuevo valor de Agilidad: "
                   DISPLAY "- Valor antiguo: "WS-HM-C-AGILITY
                   DISPLAY "- Nuevo valor: "
                   ACCEPT LS-H-AGILITY
               WHEN WS-HM-OP-LEVEL
                   DISPLAY "["WS-GAME-NAME"] "
                     "Selecciona el nuevo valor de Nivel: "
                   DISPLAY "- Valor antiguo: "WS-HM-C-LEVEL
                   DISPLAY "- Nuevo valor: "
                   ACCEPT LS-H-LEVEL
               WHEN WS-HM-OP-HP
                   DISPLAY "["WS-GAME-NAME"] "
                     "Selecciona el nuevo valor de Vida: "
                   DISPLAY "- Valor antiguo: "WS-HM-C-HP
                   DISPLAY "- Nuevo valor: "
                   ACCEPT LS-H-HP
               WHEN OTHER
                   SET WS-INVALID-OPTION TO TRUE
           END-EVALUATE.
      ******************************************************************
       SET-MENU-TO-ERROR.
           MOVE WS-VALID-OPTION TO WS-HM-ERROR.
      ******************************************************************
       END PROGRAM MOD-HEROE.
