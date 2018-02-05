       IDENTIFICATION DIVISION.
       PROGRAM-ID. DUNGCRWL.
      ******************************************************************
      * JUEGO: DUNGEON CRAWLER                                         *
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT HEROES-FILE ASSIGN TO "HEROES.TXT"
             FILE STATUS IS WS-HEROES-FS
             ORGANIZATION IS LINE SEQUENTIAL.
           SELECT MONSTERS-FILE ASSIGN TO "DUNGEON.TXT"
             FILE STATUS IS WS-MONSTERS-FS
             ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD HEROES-FILE.
       01 HEROES-REG.
           05 HEROES-R-ID              PIC 9(02) VALUE ZERO.
           05 HEROES-R-STRENGTH        PIC 9(02) VALUE ZERO.
           05 HEROES-R-AGILITY         PIC 9(02) VALUE ZERO.
           05 HEROES-R-LEVEL           PIC 9(02) VALUE ZERO.
           05 HEROES-R-HP              PIC 9(02) VALUE ZERO.
           05 HEROES-R-PROFESSION      PIC 9(02) VALUE ZERO.
       FD MONSTERS-FILE.
       01 MONSTERS-REG.
           05 MONSTER-R-ID             PIC 9(02) VALUE ZERO.
           05 MONSTER-R-STRENGTH       PIC 9(02) VALUE ZERO.
           05 MONSTER-R-AGILITY        PIC 9(02) VALUE ZERO.
           05 MONSTER-R-LEVEL          PIC 9(02) VALUE ZERO.
           05 MONSTER-R-HP             PIC 9(02) VALUE ZERO.
           05 MONSTER-R-PROFESSION     PIC 9(02) VALUE ZERO.
       WORKING-STORAGE SECTION.
       01 WS-CONSTANTS.
           05 WS-GAME-NAME             PIC X(15)
                                         VALUE "Dungeon Crawler".
           05 WS-MAX-HEROES            PIC 9(02) VALUE 7.
           05 WS-MAX-MONSTERS          PIC 9(02) VALUE 10.
       01 WS-VALID-OPTION  PIC X(28)   VALUES ALL SPACES.
           88 WS-RESET-VALID-OPTION    VALUE ALL SPACES.
           88 WS-INVALID-OPTION
             VALUE "[Escoge una opcion correcta]".
       01 WS-MAIN-MENU.
           05 WS-MM-OPTION             PIC X(01) VALUE SPACE.
               88 WS-MM-OP-EXIT          VALUE "0".
               88 WS-MM-OP-SELECT        VALUE "1".
               88 WS-MM-OP-MODIFY        VALUE "2".
               88 WS-MM-OP-PLAY          VALUE "3".
           05 WS-MM.
               10 WS-MM-HEADING    PIC X(15) VALUE "MENU PRINCIPAL ".
               10 WS-MM-ERROR      PIC X(28) VALUE ALL SPACES.
               10 FILLER           PIC X(01) VALUE X"0A".
               10 FILLER           PIC X(14) VALUE ALL "-".
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
       01 WS-HEROES-FILE.
           05 WS-HEROES-FS         PIC X(02) VALUE ZEROES.
               88 WS-H-FS-OK         VALUE "00".
               88 WS-H-FS-EOF        VALUE HIGH-VALUE.
           05 WS-HEROES-REG.
               10 WS-H-R-LENGTH    PIC 9(01) VALUE ZERO.
               10 WS-H-R-INDEX     PIC 9(01) VALUE ZERO.
               10 WS-H-R-CURRENT   PIC 9(01) VALUE ZERO.
      * WS-HEROES-R OCCURS **WS-MAX-HEROES** TIMES
               10 WS-HEROES-R OCCURS 7 TIMES.
                   15 WS-H-R-ID                PIC 9(02) VALUE ZERO.
                   15 WS-H-R-STRENGTH          PIC 9(02) VALUE ZERO.
                   15 WS-H-R-AGILITY           PIC 9(02) VALUE ZERO.
                   15 WS-H-R-LEVEL             PIC 9(02) VALUE ZERO.
                   15 WS-H-R-HP                PIC S9(02) VALUE ZERO.
                   15 WS-H-R-PROFESSION        PIC 9(02) VALUE ZERO.
       01 WS-MONSTERS-FILE.
           05 WS-MONSTERS-FS       PIC X(02) VALUE ZEROES.
               88 WS-M-FS-OK       VALUE "00".
               88 WS-M-FS-EOF      VALUE HIGH-VALUE.
           05 WS-MONSTERS-REG.
               10 WS-M-R-LENGTH    PIC 9(01) VALUE ZERO.
               10 WS-M-R-INDEX     PIC 9(01) VALUE ZERO.
               10 WS-M-R-CURRENT   PIC 9(01) VALUE ZERO.
      * WS-MONSTERS-R OCCURS **WS-MAX-MONSTERS** TIMES
               10 WS-MONSTERS-R OCCURS 10 TIMES.
                   15 WS-M-R-ID                PIC 9(02) VALUE ZERO.
                   15 WS-M-R-STRENGTH          PIC 9(02) VALUE ZERO.
                   15 WS-M-R-AGILITY           PIC 9(02) VALUE ZERO.
                   15 WS-M-R-LEVEL             PIC 9(02) VALUE ZERO.
                   15 WS-M-R-HP                PIC S9(02) VALUE ZERO.
                   15 WS-M-R-PROFESSION        PIC 9(02) VALUE ZERO.
       01 WS-HEROES-MENU.
           05 WS-H-OPTION            PIC X(01) VALUE SPACE.
               88 WS-H-OP-CONTINUE     VALUE SPACE.
               88 WS-H-OP-EXIT         VALUE "0".
           05 WS-HEROES-MENU-TITLE.
               10 WS-HM-HEADING    PIC X(17)
                 VALUE "LISTADO DE HEROES".
               10 FILLER           PIC X(01) VALUE X"0A".
               10 FILLER           PIC X(17) VALUE ALL "-".
               10 FILLER           PIC X(01) VALUE X"0A".
               10 FILLER           PIC X(50)
             VALUE "   ID  Fuerza  Agilidad  Nivel  P. Vida  Profesion".
               10 FILLER           PIC X(01) VALUE X"0A".
               10 FILLER           PIC X(50)
             VALUE "   --  ------  --------  -----  -------  ---------".
           05 WS-HEROES-MENU-CONTENT.
               10 WS-HMC-INDEX     PIC 9(01).
               10 FILLER           PIC X(02) VALUE "- ".
               10 WS-HMC-ID        PIC 9(02) .
               10 FILLER           PIC X(04) VALUE "    ".
               10 WS-HMC-STRENGTH  PIC 9(02).
               10 FILLER           PIC X(07) VALUE "       ".
               10 WS-HMC-AGILITY   PIC 9(02).
               10 FILLER           PIC X(06) VALUE "      ".
               10 WS-HMC-LEVEL     PIC 9(02).
               10 FILLER           PIC X(06) VALUE "      ".
               10 WS-HMC-HP        PIC 9(02).
               10 FILLER           PIC X(08) VALUE "        ".
               10 WS-HMC-PROFESSION    PIC 9(02).
           05 WS-HEROES-MENU-FOOTER.
               10 FILLER           PIC X(01) VALUE X"0A".
               10 FILLER           PIC X(09) VALUE "0- Salir".
               10 FILLER           PIC X(01) VALUE X"0A".
               10 FILLER           PIC X(21)
                 VALUE "Escoge un heroe (1 - ".
               10 WS-HMF-LENGTH    PIC 9(01).
               10 FILLER           PIC X(02) VALUE "):".
       77 OPCION PIC X VALUE SPACES.
       77 PAUSAR PIC X VALUE SPACES.
      ******************************************************************
       SCREEN SECTION.
       01 INTRO.
           03 LINE 04 COL 10 VALUE "______".
           03 LINE 05 COL 10 VALUE "|  _  \".
           03 LINE 06 COL 10 VALUE
             "| | | |_   _ _ __   __ _  ___  ___  _ __".
           03 LINE 07 COL 10 VALUE
             "| | | | | | | '_ \ / _` |/ _ \/ _ \| '_ \".
           03 LINE 08 COL 10 VALUE
             "| |/ /| |_| | | | | (_| |  __/ (_) | | | |".
           03 LINE 09 COL 10 VALUE
             "|___/  \__,_|_| |_|\__, |\___|\___/|_| |_|".
           03 LINE 10 COL 10 VALUE "                    __/ |".
           03 LINE 11 COL 10 VALUE "                   |___/".
           03 LINE 12 COL 10 VALUE "   _____                    _".
           03 LINE 13 COL 10 VALUE "  /  __ \                  | |".
           03 LINE 14 COL 10 VALUE
             "  | /  \/_ __ __ ___      _| | ___ _ __".
           03 LINE 15 COL 10 VALUE
             "  | |   | '__/ _` \ \ /\ / / |/ _ \ '__|".
           03 LINE 16 COL 10 VALUE
            "  | \__/\ | | (_| |\ V  V /| |  __/ |".
           03 LINE 17 COL 10 VALUE
             "   \____/_|  \__,_| \_/\_/ |_|\___|_|".
           03 LINE 21 COL 40 VALUE "Made by Juan Ramon, Juanjo Sanchez".
           03 LINE 25 COL 20 VALUE
             "PULSE CUALQUIER TECLA PARA CONTINUAR...".
           03 PIC X USING OPCION.
       01 LIMPIAR-PANTALLA.
           03 BLANK SCREEN.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY INTRO.
           ACCEPT INTRO.
           DISPLAY LIMPIAR-PANTALLA.
           SET WS-RESET-VALID-OPTION TO TRUE
           PERFORM SET-MAIN-MENU-ERROR
           PERFORM DISPLAY-MAIN-MENU UNTIL WS-MM-OP-EXIT

           GO TO STOP-RUN.
      ******************************************************************
       DISPLAY-MONSTERS.
           DISPLAY "LOADED MONSTERS FILE:"
           DISPLAY "--------"
           PERFORM VARYING WS-M-R-INDEX FROM 1 BY 1
               UNTIL WS-M-R-INDEX > WS-M-R-LENGTH
               DISPLAY WS-MONSTERS-R(WS-M-R-INDEX)
           END-PERFORM
           DISPLAY "--------".

      ******************************************************************
      * == [DISPLAY-MAIN-MENU] ===================================BEGIN=
       DISPLAY-MAIN-MENU.
           DISPLAY WS-MM.
           ACCEPT WS-MM-OPTION.

           EVALUATE TRUE
           WHEN WS-MM-OP-SELECT
               PERFORM DISPLAY-SELECT-HERO
      *         DISPLAY "SELECTED HERO: "WS-H-R-CURRENT
           WHEN WS-MM-OP-MODIFY
               PERFORM DISPLAY-MODIFY-HERO
           WHEN WS-MM-OP-PLAY
               PERFORM INIT--WS-MONSTERS-R--CONTENT
               IF WS-M-FS-OK THEN
                   PERFORM DISPLAY-MONSTERS
                   PERFORM PLAY
               END-IF
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
           OPEN INPUT HEROES-FILE.
           IF NOT WS-H-FS-OK THEN
               PERFORM ERROR-OPENING-HEROES
           ELSE
               PERFORM DISPLAY-HEROES-MENU UNTIL WS-H-OP-EXIT
                 OR (1 <= WS-H-OPTION AND WS-H-OPTION <= WS-H-R-LENGTH)
               MOVE WS-H-OPTION TO WS-H-R-CURRENT
               SET WS-H-OP-CONTINUE TO TRUE
               CLOSE HEROES-FILE
           END-IF.
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
               ADD 1 TO WS-H-R-LENGTH
           END-READ.
      ******************************************************************
       DISPLAY-HEROES-MENU.
           PERFORM DISPLAY-HEROES-MENU-TITLE.
           PERFORM DISPLAY-HEROES-MENU-CONTENT.
           PERFORM DISPLAY-HEROES-MENU-FOOTER.
           ACCEPT WS-H-OPTION.
      ******************************************************************
       DISPLAY-HEROES-MENU-TITLE.
           DISPLAY WS-HEROES-MENU-TITLE.
      ******************************************************************
       DISPLAY-HEROES-MENU-CONTENT.
           PERFORM READ-FILE-HEROES VARYING WS-H-R-INDEX FROM 1 BY 1
             UNTIL WS-H-FS-EOF OR WS-H-R-INDEX > WS-MAX-HEROES.
      ******************************************************************
       DISPLAY-HEROES-MENU-FOOTER.
           MOVE WS-H-R-LENGTH TO WS-HMF-LENGTH.
           DISPLAY WS-HEROES-MENU-FOOTER.
      ******************************************************************
       ERROR-OPENING-HEROES.
           DISPLAY "["WS-GAME-NAME"] Fichero HEROES no disponible.".
      ******************************************************************
       DISPLAY-MODIFY-HERO.
           IF WS-H-R-CURRENT > 0 THEN
               CALL "MOD-HEROE" USING WS-HEROES-R(WS-H-R-CURRENT)
           ELSE
               DISPLAY "["WS-GAME-NAME"] "
                 "Primero debes seleccionar un heroe!"
           END-IF.
      ******************************************************************
       PLAY.
           IF WS-H-R-CURRENT > 0 THEN
               IF WS-M-R-LENGTH > 0 THEN
                   DISPLAY "["WS-GAME-NAME"] A LA BATALLA!"
                   MOVE 1 TO WS-M-R-CURRENT
                   PERFORM UNTIL
                     NOT (WS-H-R-HP(WS-H-R-CURRENT) > 0
                     AND WS-M-R-CURRENT <= WS-M-R-LENGTH)
                       PERFORM FIGHT-AGAINST-MONSTER
                       ADD 1 TO WS-M-R-CURRENT
                   END-PERFORM
                   IF WS-H-R-HP(WS-H-R-CURRENT) > 0 THEN
                       DISPLAY "["WS-GAME-NAME"] Has Ganado!!"
                   ELSE
                       DISPLAY "["WS-GAME-NAME"] Has Perdido :("
                   END-IF
               ELSE
                   DISPLAY "["WS-GAME-NAME"] "
                   "No quedan mas monstruos contra los que luchar"
               END-IF
           ELSE
               DISPLAY "["WS-GAME-NAME"] "
                 "Debes de seleccionar un heroe primero!"
           END-IF.
      ******************************************************************
       FIGHT-AGAINST-MONSTER.
           PERFORM UNTIL WS-H-R-HP(WS-H-R-CURRENT) < 0
             OR WS-M-R-HP(WS-M-R-CURRENT) < 0
           DISPLAY "WS-H-R-HP("WS-H-R-CURRENT"): "
             WS-H-R-HP(WS-H-R-CURRENT)
           DISPLAY "WS-M-R-HP("WS-M-R-CURRENT"): "
             WS-M-R-HP(WS-M-R-CURRENT)
               DISPLAY "** LUCHAS CONTRA: "
                   WS-M-R-PROFESSION(WS-M-R-CURRENT)" ** "
                   " CON FUERZA: "WS-M-R-STRENGTH(WS-M-R-CURRENT)
               DISPLAY "TIENES HP: "WS-H-R-HP(WS-H-R-CURRENT)
                   " CON FUERZA: "
                   WS-H-R-STRENGTH(WS-H-R-CURRENT)

               IF WS-H-R-STRENGTH(WS-H-R-CURRENT)
                   > WS-M-R-STRENGTH(WS-M-R-CURRENT) THEN
                   COMPUTE WS-M-R-HP(WS-M-R-CURRENT) =
                       WS-M-R-HP(WS-M-R-CURRENT)
                       - WS-H-R-STRENGTH(WS-H-R-CURRENT)
                   DISPLAY "HAS HERIDO AL MONSTRUO, TIENE HP: "
                       WS-M-R-HP(WS-M-R-CURRENT)
               ELSE
                   COMPUTE WS-H-R-HP(WS-H-R-CURRENT) =
                       WS-H-R-HP(WS-H-R-CURRENT)
                       - WS-M-R-STRENGTH(WS-M-R-CURRENT)
                   DISPLAY "TE HA HERIDO EL MONSTRUO, TIENES HP: "
                       WS-H-R-HP(WS-H-R-CURRENT)
               END-IF
           END-PERFORM.
      ******************************************************************
       EXIT-GAME.
           DISPLAY "SALIR - [GUARDAR CAMBIOS]".
      * == [DISPLAY-MAIN-MENU] =====================================END=

      ******************************************************************
      * == [INIT--WS-MONSTERS-R--CONTENT] ========================BEGIN=
       INIT--WS-MONSTERS-R--CONTENT.
           OPEN INPUT MONSTERS-FILE.
           IF NOT WS-M-FS-OK THEN
               PERFORM ERROR-OPENING-MONSTERS
           ELSE
               PERFORM SET--WS-MONSTERS-R--CONTENT
               CLOSE MONSTERS-FILE
           END-IF.
      ******************************************************************
       SET--WS-MONSTERS-R--CONTENT.
           PERFORM READ-FILE-MONSTERS VARYING WS-M-R-INDEX FROM 1 BY 1
             UNTIL WS-M-FS-EOF OR WS-M-R-INDEX > WS-MAX-MONSTERS.
      ******************************************************************
       READ-FILE-MONSTERS.
           READ MONSTERS-FILE INTO WS-MONSTERS-R(WS-M-R-INDEX)
           AT END
               SET WS-M-FS-EOF TO TRUE
           NOT AT END
               ADD 1 TO WS-M-R-LENGTH
           END-READ.
      ******************************************************************
       ERROR-OPENING-MONSTERS.
           DISPLAY "["WS-GAME-NAME"] Fichero MONSTERS no disponible.".
      * == [INIT--WS-MONSTERS-R--CONTENT] ==========================END=

      ******************************************************************
       STOP-RUN.
           STOP RUN.
       END PROGRAM DUNGCRWL.
