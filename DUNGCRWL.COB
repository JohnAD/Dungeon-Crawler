       IDENTIFICATION DIVISION.
       PROGRAM-ID. DUNGCRWL.
      *>  AUTHOR.                    JUAN RAMON/JUANJO.

      ******************************************************************

      *================================================================*
      * FICHERO ENTRADA: .dat                                          *
      *                                                                *
      *----------------------------------------------------------------*
      *
      ******************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
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
           DISPLAY "SELECCIONAR HEROE".


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
