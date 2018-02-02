       IDENTIFICATION DIVISION.
       PROGRAM-ID. DUNGCRWL.
       *>AUTHOR. JUAN RAMON/JUANJO.
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
            FILE-CONTROL.
            SELECT HEROES-FILE ASSIGN TO "HEROES.TXT"
            FILE STATUS IS WS-HEROES-FS
            ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD HEROES-FILE.
       01 HEROES-REG.
           05 HEROES-R-ID              PIC 9(02) VALUE ZERO.
           05 HEROES-R-STRENGTH             PIC 9(02) VALUE ZERO.
           05 HEROES-R-AGILITY             PIC 9(02) VALUE ZERO.
           05 HEROES-R-LEVEL             PIC 9(02) VALUE ZERO.
           05 HEROES-R-HP              PIC 9(02) VALUE ZERO.
           05 HEROES-R-PROFESSION            PIC 9(02) VALUE ZERO.
       WORKING-STORAGE SECTION.
       01 WS-HEROES-FILE.
           05 WS-HEROES-FS             PIC X(02) VALUE ZEROES.
               88 WS-H-FS-OK           VALUE "00".
               88 WS-H-FS-EOF          VALUE HIGH-VALUE.
           05 WS-HEROES-REG.
               10 WS-H-R-LONG          PIC 9(01) VALUE ZERO.
               10 WS-H-R-INDEX         PIC 9(01) VALUE ZERO.
               10 WS-HEROES-R OCCURS 7 TIMES.
                   15 WS-H-R-ID                PIC 9(02) VALUE ZERO.
                   15 WS-H-R-STRENGTH          PIC 9(02) VALUE ZERO.
                   15 WS-H-R-AGILITY           PIC 9(02) VALUE ZERO.
                   15 WS-H-R-LEVEL             PIC 9(02) VALUE ZERO.
                   15 WS-H-R-HP                PIC 9(02) VALUE ZERO.
                   15 WS-H-R-PROFESSION        PIC 9(02) VALUE ZERO.
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
               10 WS-HMC-INDEX         PIC 9(01).
               10 FILLER               PIC X(02) VALUE "- ".
               10 WS-HMC-ID                PIC 9(02) .
               10 FILLER               PIC X(04) VALUE "    ".
               10 WS-HMC-STRENGTH      PIC 9(02).
               10 FILLER               PIC X(07) VALUE "       ".
               10 WS-HMC-AGILITY       PIC 9(02).
               10 FILLER               PIC X(06) VALUE "      ".
               10 WS-HMC-LEVEL       PIC 9(02).
               10 FILLER               PIC X(06) VALUE "      ".
               10 WS-HMC-HP                PIC 9(02).
               10 FILLER               PIC X(08) VALUE "        ".
               10 WS-HMC-PROFESSION        PIC 9(02).
           05 WS-HEROES-MENU-FOOTER.
               10 FILLER               PIC X(01) VALUE X"0A".
               10 FILLER               PIC X(09) VALUE "0- Salir".
               10 FILLER               PIC X(01) VALUE X"0A".
               10 FILLER               PIC X(24)
                 VALUE "Escoge un heroe (1 - 7):".

      ******************************************************************
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT HEROES-FILE.
           IF NOT WS-H-FS-OK THEN
               PERFORM ERROR-OPENING-HEROES
           ELSE
               PERFORM DISPLAY-HEROES-MENU UNTIL WS-OP-EXIT
               CLOSE HEROES-FILE
           END-IF.

           GO TO STOP-RUN.
      ******************************************************************
       READ-FILE-HEROES.
           READ HEROES-FILE INTO WS-HEROES-R(WS-H-R-INDEX)
           AT END
               SET WS-H-FS-EOF TO TRUE
           NOT AT END
               MOVE WS-H-R-INDEX TO WS-HMC-INDEX
               MOVE WS-H-R-ID(WS-H-R-INDEX) TO WS-HMC-ID
               MOVE WS-H-R-STRENGTH(WS-H-R-INDEX) TO WS-HMC-STRENGTH
               MOVE WS-H-R-AGILITY(WS-H-R-INDEX) TO WS-HMC-AGILITY
               MOVE WS-H-R-LEVEL(WS-H-R-INDEX) TO WS-HMC-LEVEL
               MOVE WS-H-R-HP(WS-H-R-INDEX) TO WS-HMC-HP
               MOVE WS-H-R-PROFESSION(WS-H-R-INDEX) TO WS-HMC-PROFESSION
               DISPLAY WS-HEROES-MENU-CONTENT
               ADD 1 TO WS-H-R-LONG
           END-READ.
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
           PERFORM READ-FILE-HEROES VARYING WS-H-R-INDEX FROM 1 BY 1
             UNTIL WS-H-FS-EOF OR WS-H-R-INDEX > 7.
      ******************************************************************
       DISPLAY-HEROES-MENU-FOOTER.
           DISPLAY WS-HEROES-MENU-FOOTER.
           ACCEPT WS-OPTION.
      ******************************************************************
       ERROR-OPENING-HEROES.
           DISPLAY "[Dungeon Crawler] Fichero HEROE no disponible.".
      ******************************************************************
       STOP-RUN.
           STOP RUN.
       END PROGRAM DUNGCRWL.
