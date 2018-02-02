       IDENTIFICATION DIVISION.
       PROGRAM-ID. DUNGCRWL.
      *>  AUTHOR.                    JUAN RAMON/JUANJO.
      ******************************************************************
      * JUEGO: DUGEON CRAWLER                                          *
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
            FILE-CONTROL.
            SELECT HEROES-FILE ASSIGN TO 'HEROES.TXT'
            ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD HEROES-FILE.
       01 HEROES-REG.
           05 HEROES-R-ID             PIC 9(02) VALUE ZERO.
           05 HEROES-R-FUE            PIC 9(02) VALUE ZERO.
           05 HEROES-R-AGI            PIC 9(02) VALUE ZERO.

       WORKING-STORAGE SECTION.
       01 WS-CONSTANTS.
           05 WS-VALID-OPTION  PIC X(28) VALUES ALL SPACES.
               88 WS-RESET-VALID-OPTION
                    VALUE ALL SPACES.
               88 WS-INVALID-OPTION
                    VALUE "[Escoge una opcion correcta]".
       01 WS-MAIN-MENU.
           05 WS-MM-OPTION     PIC X(01) VALUE SPACE.
               88 WS-MM-OP-EXIT    VALUE "0".
               88 WS-MM-OP-SELECT  VALUE "1".
               88 WS-MM-OP-MODIFY  VALUE "2".
               88 WS-MM-OP-PLAY    VALUE "3".
           05 WS-MM.
               10 WS-MM-HEADING    PIC X(05) VALUE "MENU ".
               10 WS-MM-ERROR      PIC X(28) VALUE ALL SPACES.
               10 FILLER           PIC X(01) VALUE X"0A".
               10 FILLER           PIC X(04) VALUE ALL "-".
               10 FILLER           PIC X(01) VALUE X"0A".
               10 FILLER           PIC X(20)
                 VALUE "1- Seleccionar heroe".
               10 FILLER           PIC X(01) VALUE X"0A".
               10 FILLER           PIC X(18) VALUE "2- Modificar heroe".
               10 FILLER           PIC X(01) VALUE X"0A".
               10 FILLER           PIC X(16) VALUE "3- A la batalla!".
               10 FILLER           PIC X(01) VALUE X"0A".
               10 FILLER           PIC X(01) VALUE X"0A".
               10 FILLER           PIC X(09) VALUE "0- Salir.".
              01 WS-HEROES.
           05 WS-ID            PIC 9(02) VALUE ZERO.
       01 WS-HEROES-MENU.
           05 WS-OPTION                PIC X(01) VALUE SPACE.
               88 WS-OP-EXIT           VALUE "0".
           05 WS-HEROES-MENU-TITLE.
               10 WS-HM-HEADING        PIC X(17)
                 VALUE "LISTADO DE HEROES".
               10 FILLER               PIC X(01) VALUE X"0A".
               10 FILLER               PIC X(17) VALUE ALL "-".
               10 FILLER               PIC X(01) VALUE X"0A".
               10 FILLER               PIC X(50)
             VALUE "   ID  Fuerza  Agilidad  Nivel  P. Vida  Profesion".
               10 FILLER               PIC X(01) VALUE X"0A".
               10 FILLER               PIC X(50)
             VALUE "   --  ------  --------  -----  -------  ---------".
           05 WS-HEROES-MENU-CONTENT.
               10 FILLER               PIC X(46)
                 VALUE "1- 01    01       01      01      01        01".
               10 FILLER               PIC X(01) VALUE X"0A".
               10 FILLER               PIC X(46)
                 VALUE "2- 02    01       01      01      01        01".
               10 FILLER               PIC X(01) VALUE X"0A".
               10 FILLER               PIC X(46)
                 VALUE "3- 03    01       01      01      01        01".
               10 FILLER               PIC X(01) VALUE X"0A".
               10 FILLER               PIC X(09) VALUE "0- Salir".
               10 FILLER               PIC X(01) VALUE X"0A".
           05 WS-HEROES-MENU-FOOTER.
               10 FILLER               PIC X(24)
                 VALUE "Escoge un heroe (1 - 7):".
      ******************************************************************
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           SET WS-RESET-VALID-OPTION TO TRUE.
           PERFORM SET-MAIN-MENU-ERROR.
           PERFORM DISPLAY-MAIN-MENU UNTIL WS-MM-OP-EXIT.

           GO TO STOP-RUN.
      ******************************************************************
       DISPLAY-MAIN-MENU.
           DISPLAY WS-MM.
           ACCEPT WS-MM-OPTION.

           EVALUATE TRUE
           WHEN WS-MM-OP-SELECT
               PERFORM DISPLAY-SELECT-HERO
           WHEN WS-MM-OP-MODIFY
               PERFORM DISPLAY-MODIFY-HERO
           WHEN WS-MM-OP-PLAY
               PERFORM PLAY
           WHEN WS-MM-OP-EXIT
               PERFORM EXIT-GAME
           WHEN OTHER
               SET WS-INVALID-OPTION TO TRUE
               PERFORM SET-MAIN-MENU-ERROR
           END-EVALUATE.
      ******************************************************************
       SET-MAIN-MENU-ERROR.
           MOVE WS-VALID-OPTION TO WS-MM-ERROR.
      ******************************************************************
       DISPLAY-SELECT-HERO.
           PERFORM DISPLAY-HEROES-MENU UNTIL WS-OP-EXIT.
      ******************************************************************
       DISPLAY-HEROES-MENU.
           PERFORM DISPLAY-HEROES-MENU-TITLE.
           PERFORM DISPLAY-HEROES-MENU-CONTENT.
           PERFORM DISPLAY-HEROES-MENU-FOOTER.
      ******************************************************************
       DISPLAY-HEROES-MENU-TITLE.
           DISPLAY WS-HEROES-MENU-TITLE.
      ******************************************************************
       DISPLAY-HEROES-MENU-CONTENT.
           DISPLAY WS-HEROES-MENU-CONTENT.
      ******************************************************************
       DISPLAY-HEROES-MENU-FOOTER.
           DISPLAY WS-HEROES-MENU-FOOTER.
           ACCEPT WS-OPTION.
      ******************************************************************
       DISPLAY-MODIFY-HERO.
           DISPLAY "MODIFICAR HEROE".
      ******************************************************************
       PLAY.
           DISPLAY "A LA BATALLA!".
      ******************************************************************
       EXIT-GAME.
           DISPLAY "SALIR - [GUARDAR CAMBIOS]".
      ******************************************************************
       STOP-RUN.
           STOP RUN.
       END PROGRAM DUNGCRWL.
