       IDENTIFICATION DIVISION.
       PROGRAM-ID. DUNGCRWL.
      *>  AUTHOR.                    JUAN RAMON/JUANJO.
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
           PERFORM DISPLAY-HEROES-MENU UNTIL WS-OP-EXIT.
           GO TO STOP-RUN.
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
       STOP-RUN.
           STOP RUN.
       END PROGRAM DUNGCRWL.
