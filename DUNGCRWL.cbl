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
           05 WS-INPUT_CURSOR_SCREEN_POS.
               10 WS-ICSP-1-LINE         PIC 9(02) VALUE 4.
               10 WS-ICSP-1-COL        PIC 9(02) VALUE 20.
               10 WS-ICSP-2-LINE       PIC 9(02) VALUE 7.
               10 WS-ICSP-2-COL        PIC 9(02) VALUE 14.
           05 WS-DISPLAY-SHIFT.
               10 WS-HEROES-MENU-TITLE-SHIFT PIC 9(02) VALUE 6.
               10 WS-HEROES-MENU-CONTENT-SHIFT PIC 9(02) VALUE 2.
               10 WS-MHM-TITLE-SHIFT   PIC 9(02) VALUE 6.
           05 WS-MAX-ANIMATION-CYCLES  PIC 9(01) VALUE 3.
           05 WS-MAX-IMG-FIGHT-LENGTH  PIC 9(01) VALUE 9.
           05 WS-INI-IMG-LINE          PIC 9(02) VALUE 12.
           05 WS-MAX-ARR               PIC 9(02) VALUE 88.
           05 WS-SCREEN-LINE-POS       PIC 9(02) VALUE 1.
       01 WS-AUX.
           05 WS-AUX-NUMBER            PIC S9(05) VALUE ZERO.
           05 WS-AUX-ALPHA             PIC X(01) VALUE SPACE.
       01 WS-IMG-FIGHT.
           05 W-IMG-FIGHT-MONSTER.
               10 WS-IMG-FIGHT-M PIC X(29) OCCURS 9 TIMES VALUE SPACES.
      *>          10 FILLER VALUE "                             ".
      *>          10 FILLER VALUE "        /-/--\               ".
      *>          10 FILLER VALUE "      (@~@)   )/\            ".
      *>          10 FILLER VALUE "  ___/--      \  |           ".
      *>          10 FILLER VALUE " (oo)__ _      )_/           ".
      *>          10 FILLER VALUE "  ^^___/       \             ".
      *>          10 FILLER VALUE "        \       |/-\         ".
      *>          10 FILLER VALUE "         (      )   |        ".
      *>          10 FILLER VALUE "         |       \_/         ".
           05 WS-IMG-FIGHT-SWORD.
               10 WS-IMG-FIGHT-S PIC X(29) OCCURS 9 TIMES VALUE SPACES.
      *>          10 FILLER VALUE "           _____   _____     ".
      *>          10 FILLER VALUE "          /     \ /     \    ".
      *>          10 FILLER VALUE "     ,   |       '       |   ".
      *>          10 FILLER VALUE "     I __L________       L__ ".
      *>          10 FILLER VALUE "O====IE__________/     ./___>".
      *>          10 FILLER VALUE "     I      \.       ./      ".
      *>          10 FILLER VALUE "     `        \.   ./        ".
      *>          10 FILLER VALUE "                \ /          ".
      *>          10 FILLER VALUE "                 '           ".
           05 WS-IMG-FIGHT-PHOTOGRAM   PIC 9(01) VALUE ZERO.
           05 WS-IMG-FIGHT-LINE-I      PIC 9(02) VALUE ZERO.
       01 WS-HEROES-FILE.
           05 WS-HEROES-FS         PIC X(02) VALUE ZEROES.
               88 WS-H-FS-OK         VALUE "00".
               88 WS-H-FS-EOF        VALUE HIGH-VALUE.
           05 WS-HERO-PROFESSION   PIC X(02) VALUE SPACES.
               88 WS-H-P-GUERRERO    VALUE "34".
               88 WS-H-P-ARQUERO     VALUE "44".
               88 WS-H-P-MAGO        VALUE "45".
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
                   15 WS-H-R-HP                PIC 9(02) VALUE ZERO.
                   15 WS-H-R-PROFESSION        PIC 9(02) VALUE ZERO.
       01 WS-MONSTERS-FILE.
           05 WS-MONSTERS-FS       PIC X(02) VALUE ZEROES.
               88 WS-M-FS-OK         VALUE "00".
               88 WS-M-FS-EOF        VALUE HIGH-VALUE.
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
                   15 WS-M-R-HP                PIC 9(02) VALUE ZERO.
                   15 WS-M-R-PROFESSION        PIC 9(02) VALUE ZERO.
       01 WS-VALID-OPTION  PIC X(37)   VALUES ALL SPACES.
           88 WS-RESET-VALID-OPTION    VALUE ALL SPACES.
           88 WS-INVALID-OPTION
             VALUE "[Choose a correct option]".
           88 WS-MISSING-STEPS-OPTION
             VALUE "[You must first select a hero!]".
       01 WS-SHOW-SELECTED-HERO-OPTION PIC X(08) VALUE ALL SPACES.
           88 WS-RESET-SELECTED-HERO-OPTION   VALUE ALL SPACES.
           88 WS-SELECTED-HERO-OPTION   VALUE "w / ID: ".
       01 WS-MAIN-MENU.
           05 WS-MM-OPTION             PIC X(01) VALUE SPACE.
               88 WS-MM-OP-EXIT          VALUE "0".
               88 WS-MM-OP-SELECT        VALUE "1".
               88 WS-MM-OP-MODIFY        VALUE "2".
               88 WS-MM-OP-PLAY          VALUE "3".
           05 WS-MM.
               10 FILLER               PIC X(15)
                                         VALUE "MAIN MENU".
               10 WS-MM-ERROR      PIC X(37) VALUE ALL SPACES.
               10 FILLER           PIC X(01) VALUE X"0A".
               10 FILLER           PIC X(14) VALUE ALL "-".
               10 FILLER           PIC X(01) VALUE X"0A".
               10 FILLER           PIC X(01) VALUE X"0A".
               10 FILLER           PIC X(18) VALUE "Choose an option:".
               10 FILLER           PIC X(01) VALUE X"0A".
               10 FILLER           PIC X(01) VALUE X"0A".
               10 FILLER           PIC X(20)
                 VALUE "1- Select hero".
               10 FILLER           PIC X(01) VALUE X"0A".
               10 FILLER           PIC X(19)
                 VALUE "2- Edit hero ".
               10 WS-MM-PREFIX     PIC X(08) VALUE SPACES.
               10 WS-MM-HERO-ID    PIC X(02) VALUE SPACES.
               10 FILLER           PIC X(01) VALUE X"0A".
               10 FILLER           PIC X(16) VALUE "3- To battle!".
               10 FILLER           PIC X(01) VALUE X"0A".
               10 FILLER           PIC X(01) VALUE X"0A".
               10 FILLER           PIC X(09) VALUE "0- Exit.".
       01 WS-HEROES-MENU.
           05 WS-H-OPTION            PIC X(01) VALUE SPACE.
               88 WS-H-OP-CONTINUE     VALUE SPACE.
               88 WS-H-OP-EXIT         VALUE "0".
           05 WS-HEROES-MENU-TITLE.
               10 WS-HM-HEADING    PIC X(18)
                 VALUE "LIST OF HEROES ".
               10 WS-HM-ERROR      PIC X(28) VALUE ALL SPACES.
               10 FILLER           PIC X(01) VALUE X"0A".
               10 FILLER           PIC X(17) VALUE ALL "-".
               10 FILLER           PIC X(01) VALUE X"0A".
               10 FILLER           PIC X(01) VALUE X"0A".
               10 FILLER           PIC X(18) VALUE "Choose an option: ".
               10 FILLER           PIC X(01) VALUE X"0A".
               10 FILLER           PIC X(01) VALUE X"0A".
               10 FILLER           PIC X(52)
           VALUE "     ID  Force   Agility   Level  Life Pt  Profess'n".
               10 FILLER           PIC X(01) VALUE X"0A".
               10 FILLER           PIC X(52)
           VALUE "     --  ------  --------  -----  -------  ---------".
           05 WS-HEROES-MENU-CONTENT.
               10 WS-HMC-SELECTED  PIC X(01) VALUE SPACE.
               10 FILLER           PIC X(01) VALUE SPACE.
               10 WS-HMC-INDEX     PIC 9(01).
               10 FILLER           PIC X(02) VALUE "- ".
               10 WS-HMC-ID        PIC 9(02) .
               10 FILLER           PIC X(04) VALUE SPACES.
               10 WS-HMC-STRENGTH  PIC 9(02).
               10 FILLER           PIC X(07) VALUE SPACES.
               10 WS-HMC-AGILITY   PIC 9(02).
               10 FILLER           PIC X(06) VALUE SPACES.
               10 WS-HMC-LEVEL     PIC 9(02).
               10 FILLER           PIC X(06) VALUE SPACES.
               10 WS-HMC-HP        PIC 9(02).
               10 FILLER           PIC X(06) VALUE SPACES.
               10 WS-HMC-PROFESSION    PIC X(08).
           05 WS-HEROES-MENU-FOOTER.
               10 FILLER           PIC X(01) VALUE X"0A".
               10 FILLER           PIC X(09) VALUE "0- Exit".
       01 WS-MOD-HEROES-MENU.
           05 WS-MHM-OPTION            PIC X(01) VALUE SPACE.
               88 WS-MHM-OP-CONTINUE     VALUE SPACE.
               88 WS-MHM-OP-EXIT         VALUE "0".
               88 WS-MHM-OP-STRENGTH     VALUE "1".
               88 WS-MHM-OP-AGILITY      VALUE "2".
               88 WS-MHM-OP-LEVEL        VALUE "3".
               88 WS-MHM-OP-HP           VALUE "4".
           05 WS-MHM-TITLE.
               10 FILLER           PIC X(16)
                                     VALUE "MODIFY HERO ".
               10 WS-MHM-ERROR     PIC X(28) VALUE ALL SPACES.
               10 FILLER           PIC X(01) VALUE X"0A".
               10 FILLER           PIC X(15) VALUE ALL "-".
               10 FILLER           PIC X(01) VALUE X"0A".
               10 FILLER           PIC X(01) VALUE X"0A".
               10 FILLER           PIC X(18) VALUE "Choose an option:".
               10 FILLER           PIC X(01) VALUE X"0A".
               10 FILLER           PIC X(01) VALUE X"0A".
           05 WS-MHM-SELECTION-TITLE.
               10 FILLER           PIC X(16)
                                     VALUE "MODIFY HERO ".
               10 FILLER           PIC X(01) VALUE X"0A".
               10 FILLER           PIC X(15) VALUE ALL "-".
               10 FILLER           PIC X(01) VALUE X"0A".
               10 FILLER           PIC X(01) VALUE X"0A".
               10 FILLER           PIC X(30)
                 VALUE "Select the new value of: ".
               10 WS-MHM-SEL-TIT-MODIFYING  PIC X(11) VALUE ALL SPACES.
               10 FILLER           PIC X(01) VALUE X"0A".
               10 FILLER           PIC X(01) VALUE X"0A".
               10 FILLER           PIC X(15) VALUE "Old value: ".
               10 WS-MHM-SEL-TIT-NEWVALUE PIC X(10) VALUE ALL SPACES.
               10 FILLER           PIC X(01) VALUE X"0A".
               10 FILLER           PIC X(13) VALUE "New value: ".
           05 WS-MHM-CONTENT.
               10 FILLER               PIC X(11) VALUE "1- Force: ".
               10 WS-MHM-C-STRENGTH    PIC 9(02) VALUE ZEROES.
               10 FILLER               PIC X(01) VALUE X"0A".
               10 FILLER               PIC X(13) VALUE "2- Agility: ".
               10 WS-MHM-C-AGILITY     PIC 9(02) VALUE ZEROES.
               10 FILLER               PIC X(01) VALUE X"0A".
               10 FILLER               PIC X(10) VALUE "3- Level: ".
               10 WS-MHM-C-LEVEL       PIC 9(02) VALUE ZEROES.
               10 FILLER               PIC X(01) VALUE X"0A".
               10 FILLER               PIC X(16)
                                         VALUE "4- Life Points: ".
               10 WS-MHM-C-HP          PIC 9(02) VALUE ZEROES.
           05 WS-MHM-FOOTER.
               10 FILLER               PIC X(01) VALUE X"0A".
               10 FILLER               PIC X(09) VALUE "0- Exit".
       01 WS-PAUSE-MECHANISM.
           05 WS-PM-NOW-1.
               10 WS-PM-NOW-1-DATE     PIC 9(08) VALUE ZERO.
               10 WS-PM-NOW-1-TIME     PIC 9(08) VALUE ZERO.
           05 WS-PM-NOW-2.
               10 WS-PM-NOW-2-DATE     PIC 9(08) VALUE ZERO.
               10 WS-PM-NOW-2-TIME     PIC 9(08) VALUE ZERO.
           05 WS-PM-WRK-ONE-DAY        PIC 9(08) VALUE ZERO.
           05 WS-PM-DELTA-TIME         PIC 9(08) VALUE ZERO.
       01 WS-CREDITS.
           05 WS-C-ARR                PIC X(42) OCCURS 88 TIMES
                                        VALUE SPACES.
           05 WS-C-POS                PIC 9(02) VALUE 1.
           05 WS-C-I                  PIC 9(02) VALUE 1.
           05 WS-C-SCREEN-LINE        PIC 9(02) VALUE 1.
           05 WS-C-FINAL-POS          PIC 9(02) VALUE 0.
       SCREEN SECTION.
       01 SS-CLEAR-SCREEN.
           05 BLANK SCREEN.
       01 SS-INTRO.
           05 SS-TITLE-1.
               10 LINE 02 COL 10 VALUE "______" FOREGROUND-COLOR IS 6.
               10 LINE 03 COL 10 VALUE "|  _  \" FOREGROUND-COLOR IS 6.
               10 LINE 04 COL 10 VALUE
                 "| | | |_   _ _ __   __ _  ___  ___  _ __"
                 FOREGROUND-COLOR IS 6.
               10 LINE 05 COL 10 VALUE
                 "| | | | | | | '_ \ / _` |/ _ \/ _ \| '_ \"
                 FOREGROUND-COLOR IS 6.
               10 LINE 06 COL 10 VALUE
                 "| |/ /| |_| | | | | (_| |  __/ (_) | | | |"
                 FOREGROUND-COLOR IS 6.
               10 LINE 07 COL 10 VALUE
                 "|___/  \__,_|_| |_|\__, |\___|\___/|_| |_|"
                 FOREGROUND-COLOR IS 6.
               10 LINE 08 COL 10 VALUE "                    __/ |"
               FOREGROUND-COLOR IS 6.
               10 LINE 09 COL 10 VALUE "                   |___/"
               FOREGROUND-COLOR IS 6.
           05 SS-TITLE-2.
               10 LINE 10 COL 10 VALUE
                 "           _____                    _"
                 FOREGROUND-COLOR IS 6.
               10 LINE 11 COL 10 VALUE
                 "          /  __ \                  | |"
                 FOREGROUND-COLOR IS 6.
               10 LINE 12 COL 10 VALUE
                 "          | /  \/_ __ __ ___      _| | ___ _ __ ©"
                 FOREGROUND-COLOR IS 6.
               10 LINE 13 COL 10 VALUE
                 "          | |   | '__/ _` \ \ /\ / / |/ _ \ '__|"
                 FOREGROUND-COLOR IS 6.
               10 LINE 14 COL 10 VALUE
                 "          | \__/\ | | (_| |\ V  V /| |  __/ |"
                 FOREGROUND-COLOR IS 6.
               10 LINE 15 COL 10 VALUE
                 "           \____/_|  \__,_| \_/\_/ |_|\___|_|"
                 FOREGROUND-COLOR IS 6.
           05 SS-TITLE-3.
               10 LINE 18 COL 40 VALUE
                 "Written by Juan Ramon & Juanjo Sanchez"
                 FOREGROUND-COLOR IS 2.
           05 SS-TITLE-4.
               10 LINE 20 COL 15 VALUE
                 "              />" FOREGROUND-COLOR IS 3.
               10 LINE 21 COL 15 VALUE
                 " ()          //----------------------------------("
                 FOREGROUND-COLOR IS 3.
               10 LINE 22 COL 15 VALUE
                 "(*)OXOXOX(*>      PRESS ENTER TO CONTINUE         \".
               10 LINE 23 COL 15 VALUE
                  "()          \\------------------------------------)"
                  FOREGROUND-COLOR IS 3.
               10 LINE 24 COL 15 VALUE
                 "              \>" FOREGROUND-COLOR IS 3 BEEP.
           05 PIC X USING WS-AUX-ALPHA.
      ******************************************************************
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INIT--WS-HEROES-R--CONTENT
           IF WS-M-FS-OK THEN
               IF WS-M-FS-OK THEN
                   PERFORM INI--WS-IMG-FIGHT
                   PERFORM INI-CREDITS-ARRAY
                   PERFORM DISPLAY-INTRO
                   PERFORM DISPLAY-MAIN-MENU UNTIL WS-MM-OP-EXIT
               END-IF
           END-IF

           GO TO STOP-RUN.
      ******************************************************************
      * == [DISPLAY-MAIN-MENU] ===================================BEGIN=
       DISPLAY-MAIN-MENU.
           PERFORM SET-MAIN-MENU-ERROR
           IF WS-H-R-CURRENT > 0 THEN
               SET WS-SELECTED-HERO-OPTION TO TRUE
               MOVE WS-SHOW-SELECTED-HERO-OPTION TO WS-MM-PREFIX
               MOVE WS-H-R-ID(WS-H-R-CURRENT) TO WS-MM-HERO-ID
           ELSE
               SET WS-RESET-SELECTED-HERO-OPTION TO TRUE
               MOVE WS-SHOW-SELECTED-HERO-OPTION TO WS-MM-PREFIX
               MOVE SPACE TO WS-MM-HERO-ID
           END-IF.
           DISPLAY WS-MM LINE 1 COL 1.

           SET WS-RESET-VALID-OPTION TO TRUE
           ACCEPT WS-MM-OPTION LINE WS-ICSP-1-LINE COL WS-ICSP-1-COL.
           DISPLAY SS-CLEAR-SCREEN.

           EVALUATE TRUE
           WHEN WS-MM-OP-SELECT
               PERFORM DISPLAY-SELECT-HERO
           WHEN WS-MM-OP-MODIFY
               IF WS-H-R-CURRENT > 0 THEN
                   PERFORM DISPLAY-MODIFY-HERO
               ELSE
                   SET WS-MISSING-STEPS-OPTION TO TRUE
               END-IF
           WHEN WS-MM-OP-PLAY
               IF WS-H-R-CURRENT > 0 THEN
                   PERFORM PLAY
               ELSE
                   SET WS-MISSING-STEPS-OPTION TO TRUE
               END-IF
           WHEN WS-MM-OP-EXIT
               PERFORM EXIT-GAME
           WHEN OTHER
               SET WS-INVALID-OPTION TO TRUE
           END-EVALUATE
           DISPLAY SS-CLEAR-SCREEN.
      ******************************************************************
       SET-MAIN-MENU-ERROR.
           MOVE WS-VALID-OPTION TO WS-MM-ERROR.
      ******************************************************************
       DISPLAY-SELECT-HERO.
           SET WS-RESET-VALID-OPTION TO TRUE.
           PERFORM DISPLAY-HEROES-MENU UNTIL WS-H-OP-EXIT OR
             (1 <= WS-H-OPTION AND WS-H-OPTION <= WS-H-R-LENGTH)
           IF NOT WS-H-OP-EXIT THEN
               MOVE WS-H-OPTION TO WS-H-R-CURRENT
           END-IF
           SET WS-H-OP-CONTINUE TO TRUE.
           SET WS-RESET-VALID-OPTION TO TRUE.
      ******************************************************************
       DISPLAY--WS-HEROES-R.
           IF WS-H-R-INDEX = WS-H-R-CURRENT THEN
               MOVE "*" TO WS-HMC-SELECTED
           ELSE
               MOVE SPACE TO WS-HMC-SELECTED
           END-IF

           MOVE WS-H-R-INDEX TO WS-HMC-INDEX
           MOVE WS-H-R-ID(WS-H-R-INDEX) TO WS-HMC-ID
           MOVE WS-H-R-STRENGTH(WS-H-R-INDEX) TO WS-HMC-STRENGTH
           MOVE WS-H-R-AGILITY(WS-H-R-INDEX) TO WS-HMC-AGILITY
           MOVE WS-H-R-LEVEL(WS-H-R-INDEX) TO WS-HMC-LEVEL
           MOVE WS-H-R-HP(WS-H-R-INDEX) TO WS-HMC-HP

           MOVE WS-H-R-PROFESSION(WS-H-R-INDEX)
             TO WS-HERO-PROFESSION

           EVALUATE TRUE
           WHEN WS-H-P-GUERRERO
               MOVE "WARRIOR " TO WS-HMC-PROFESSION
           WHEN WS-H-P-ARQUERO
               MOVE " ARCHER " TO WS-HMC-PROFESSION
           WHEN WS-H-P-MAGO
               MOVE "  MAGE  " TO WS-HMC-PROFESSION
           END-EVALUATE

           ADD 1 TO WS-AUX-NUMBER.
           DISPLAY WS-HEROES-MENU-CONTENT
             LINE WS-AUX-NUMBER COL 1.
      ******************************************************************
       DISPLAY-HEROES-MENU.
           PERFORM SET-LIST-HEROES-MENU-TO-ERROR.
           PERFORM DISPLAY-HEROES-MENU-TITLE.
           PERFORM DISPLAY-HEROES-MENU-CONTENT.
           PERFORM DISPLAY-HEROES-MENU-FOOTER.
           ACCEPT WS-H-OPTION LINE WS-ICSP-1-LINE COL WS-ICSP-1-COL.

           IF NOT (1 <= WS-H-OPTION AND WS-H-OPTION <= WS-H-R-LENGTH)
             THEN
               SET WS-INVALID-OPTION TO TRUE
           ELSE
               SET WS-RESET-VALID-OPTION TO TRUE
           END-IF
           DISPLAY SS-CLEAR-SCREEN.
      ******************************************************************
       DISPLAY-HEROES-MENU-TITLE.
           DISPLAY WS-HEROES-MENU-TITLE LINE 1 COL 1.
      ******************************************************************
       DISPLAY-HEROES-MENU-CONTENT.
           MOVE WS-HEROES-MENU-TITLE-SHIFT TO WS-AUX-NUMBER.
           ADD WS-HEROES-MENU-CONTENT-SHIFT TO WS-AUX-NUMBER.

           PERFORM DISPLAY--WS-HEROES-R VARYING WS-H-R-INDEX
             FROM 1 BY 1 UNTIL WS-H-R-INDEX > WS-H-R-LENGTH.
      ******************************************************************
       DISPLAY-HEROES-MENU-FOOTER.
           COMPUTE WS-AUX-NUMBER = WS-HEROES-MENU-TITLE-SHIFT
             + WS-HEROES-MENU-CONTENT-SHIFT + WS-H-R-LENGTH + 1.
           DISPLAY WS-HEROES-MENU-FOOTER
             LINE WS-AUX-NUMBER COL 1.
      ******************************************************************
       SET-LIST-HEROES-MENU-TO-ERROR.
          MOVE WS-VALID-OPTION TO WS-HM-ERROR.
      ******************************************************************
       DISPLAY-MODIFY-HERO.
           SET WS-MHM-OP-CONTINUE TO TRUE
           SET WS-RESET-VALID-OPTION TO TRUE
           PERFORM DISPLAY-MOD-HEROES-MENU UNTIL WS-MHM-OP-EXIT
           SET WS-RESET-VALID-OPTION TO TRUE.
      ******************************************************************
       PLAY.
           PERFORM INIT--WS-MONSTERS-R--CONTENT
           IF WS-H-R-CURRENT > 0 THEN
               IF WS-M-R-LENGTH > 0 THEN
                   MOVE 1 TO WS-M-R-CURRENT
                   PERFORM UNTIL
                     NOT (WS-H-R-HP(WS-H-R-CURRENT) > 0
                     AND WS-M-R-CURRENT <= WS-M-R-LENGTH)
                       DISPLAY SS-CLEAR-SCREEN
                       PERFORM FIGHT-MONSTER
                       ADD 1 TO WS-M-R-CURRENT
                   END-PERFORM

                   IF WS-H-R-HP(WS-H-R-CURRENT) > 0 THEN
                       PERFORM WIN
                   ELSE
                       PERFORM LOSE
                   END-IF
               ELSE
                   DISPLAY
                     "There are no more monsters to fight"
                     LINE 22 COL 1

                     PERFORM PRESS-KEY-TO-CONTINUE
               END-IF
           ELSE
               DISPLAY "You must select a hero first!"
                 LINE 22 COL 1

                 PERFORM PRESS-KEY-TO-CONTINUE
           END-IF.
      ******************************************************************
       FIGHT-MONSTER.
           PERFORM UNTIL WS-H-R-HP(WS-H-R-CURRENT) = 0
             OR WS-M-R-HP(WS-M-R-CURRENT) = 0
               DISPLAY "FIGHT FOR YOUR LIFE !!"
                 LINE 1 COL 1
               DISPLAY "----------------------"
                 LINE 2 COL 1
               DISPLAY "FIGHT AGAINST THE MONSTER W/ ID: "
                 LINE 4 COL 1
               DISPLAY WS-M-R-PROFESSION(WS-M-R-CURRENT)
                 LINE 4 COL 35
               DISPLAY ", HARDNESS:   "
                 LINE 4 COL 37
               DISPLAY WS-M-R-STRENGTH(WS-M-R-CURRENT)
                 LINE 4 COL 51
               DISPLAY "YOUR HP:   "
                 LINE 5 COL 1
               DISPLAY WS-H-R-HP(WS-H-R-CURRENT)
                 LINE 5 COL 12
               DISPLAY ", HARDNESS:   "
                 LINE 5 COL 14
               DISPLAY WS-H-R-STRENGTH(WS-H-R-CURRENT)
                 LINE 5 COL 28

               IF WS-H-R-STRENGTH(WS-H-R-CURRENT)
                   > WS-M-R-STRENGTH(WS-M-R-CURRENT) THEN
                   COMPUTE WS-AUX-NUMBER =
                       WS-M-R-HP(WS-M-R-CURRENT)
                       - WS-H-R-STRENGTH(WS-H-R-CURRENT)
                   IF WS-AUX-NUMBER < 0 THEN
                       MOVE 0 TO WS-M-R-HP(WS-M-R-CURRENT)
                   ELSE
                       MOVE WS-AUX-NUMBER TO WS-M-R-HP(WS-M-R-CURRENT)
                   END-IF
                            
                   DISPLAY "YOU HAVE HURT THE MONSTER, HAS HP:"
                     LINE 6 COL 1
                   DISPLAY WS-M-R-HP(WS-M-R-CURRENT)
                     LINE 6 COL 35
               ELSE
                   COMPUTE WS-AUX-NUMBER =
                       WS-H-R-HP(WS-H-R-CURRENT)
                       - WS-M-R-STRENGTH(WS-M-R-CURRENT)
                   IF WS-AUX-NUMBER < 0 THEN
                       MOVE 0 TO WS-H-R-HP(WS-H-R-CURRENT)
                   ELSE
                       MOVE WS-AUX-NUMBER TO WS-H-R-HP(WS-H-R-CURRENT)
                   END-IF

                   DISPLAY "THE MONSTER HAS HURT YOU, YOUR HP: "
                     LINE 6 COL 1
                   DISPLAY WS-H-R-HP(WS-H-R-CURRENT)
                       LINE 6 COL 37
               END-IF
               PERFORM FIGHT-MONSTER-ANIMATION
           END-PERFORM.
      ******************************************************************
       EXIT-GAME.
           PERFORM DISPLAY-CREDITS-MOVE.
      * == [DISPLAY-MAIN-MENU] =====================================END=

      ******************************************************************
      * == [INIT--WS-HEROES-R--CONTENT] ==========================BEGIN=
       INIT--WS-HEROES-R--CONTENT.
           OPEN INPUT HEROES-FILE.
           IF NOT WS-H-FS-OK THEN
               PERFORM ERROR-OPENING-HEROES
           ELSE
               PERFORM SET--WS-HEROES-R--CONTENT
               CLOSE HEROES-FILE
           END-IF.
      ******************************************************************
       SET--WS-HEROES-R--CONTENT.
           MOVE 0 TO WS-H-R-LENGTH.
           PERFORM READ-FILE-HEROES VARYING WS-H-R-INDEX FROM 1 BY 1
             UNTIL WS-H-FS-EOF OR WS-H-R-INDEX > WS-MAX-HEROES.
      ******************************************************************
       READ-FILE-HEROES.
           READ HEROES-FILE INTO WS-HEROES-R(WS-H-R-INDEX)
           AT END
               SET WS-H-FS-EOF TO TRUE
           NOT AT END
               ADD 1 TO WS-H-R-LENGTH
           END-READ.
      ******************************************************************
       ERROR-OPENING-HEROES.
           DISPLAY "["WS-GAME-NAME"] HEROES file not available.".
      * == [INIT--WS-HEROES-R--CONTENT] ============================END=

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
           MOVE 0 TO WS-M-R-LENGTH.
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
           DISPLAY "["WS-GAME-NAME"] MONSTERS file not available.".
      * == [INIT--WS-MONSTERS-R--CONTENT] ==========================END=

      ******************************************************************
       DISPLAY-MOD-HEROES-MENU.
           MOVE WS-H-R-STRENGTH((WS-H-R-CURRENT)) TO WS-MHM-C-STRENGTH.
           MOVE WS-H-R-AGILITY(WS-H-R-CURRENT) TO WS-MHM-C-AGILITY.
           MOVE WS-H-R-LEVEL(WS-H-R-CURRENT) TO WS-MHM-C-LEVEL.
           MOVE WS-H-R-HP(WS-H-R-CURRENT) TO WS-MHM-C-HP.

           PERFORM SET-MENU-ERROR.
           DISPLAY WS-MHM-TITLE LINE 1 COL 1.

           COMPUTE WS-AUX-NUMBER = WS-MHM-TITLE-SHIFT.
           DISPLAY WS-MHM-CONTENT LINE WS-AUX-NUMBER COL 1.

           COMPUTE WS-AUX-NUMBER = WS-MHM-TITLE-SHIFT + 4.
           DISPLAY WS-MHM-FOOTER LINE WS-AUX-NUMBER COL 1.

           SET WS-RESET-VALID-OPTION TO TRUE
           ACCEPT WS-MHM-OPTION LINE WS-ICSP-1-LINE COL WS-ICSP-1-COL.

           DISPLAY SS-CLEAR-SCREEN.

           EVALUATE TRUE
           WHEN WS-MHM-OP-STRENGTH
               MOVE WS-H-R-STRENGTH(WS-H-R-CURRENT)
                 TO WS-MHM-SEL-TIT-NEWVALUE

               MOVE "Force" TO WS-MHM-SEL-TIT-MODIFYING
               DISPLAY WS-MHM-SELECTION-TITLE LINE 1 COL 1

               ACCEPT WS-H-R-STRENGTH(WS-H-R-CURRENT)
                 LINE WS-ICSP-2-LINE COL WS-ICSP-2-COL
           WHEN WS-MHM-OP-AGILITY
               MOVE WS-H-R-AGILITY(WS-H-R-CURRENT)
                 TO WS-MHM-SEL-TIT-NEWVALUE

               MOVE "Agility" TO WS-MHM-SEL-TIT-MODIFYING
               DISPLAY WS-MHM-SELECTION-TITLE LINE 1 COL 1

               ACCEPT WS-H-R-AGILITY(WS-H-R-CURRENT)
                 LINE WS-ICSP-2-LINE COL WS-ICSP-2-COL
           WHEN WS-MHM-OP-LEVEL
               MOVE WS-H-R-LEVEL(WS-H-R-CURRENT)
                 TO WS-MHM-SEL-TIT-NEWVALUE

               MOVE "Level" TO WS-MHM-SEL-TIT-MODIFYING
               DISPLAY WS-MHM-SELECTION-TITLE LINE 1 COL 1

               ACCEPT WS-H-R-LEVEL(WS-H-R-CURRENT)
                 LINE WS-ICSP-2-LINE COL WS-ICSP-2-COL
           WHEN WS-MHM-OP-HP
               MOVE WS-H-R-HP(WS-H-R-CURRENT)
                 TO WS-MHM-SEL-TIT-NEWVALUE

               MOVE "Life Points" TO WS-MHM-SEL-TIT-MODIFYING
               DISPLAY WS-MHM-SELECTION-TITLE LINE 1 COL 1

               ACCEPT WS-H-R-HP(WS-H-R-CURRENT)
                 LINE WS-ICSP-2-LINE COL WS-ICSP-2-COL
           WHEN OTHER
               SET WS-INVALID-OPTION TO TRUE
           END-EVALUATE
           DISPLAY SS-CLEAR-SCREEN.
      ******************************************************************
       SET-MENU-ERROR.
           MOVE WS-VALID-OPTION TO WS-MHM-ERROR.
      *****************************************************************
       PAUSE.
           ACCEPT WS-PM-NOW-1-DATE FROM DATE.
           ACCEPT WS-PM-NOW-1-TIME FROM TIME.
           MOVE 0 TO WS-PM-WRK-ONE-DAY.
           PERFORM UNTIL WS-PM-DELTA-TIME > 00000050
               ACCEPT WS-PM-NOW-2-DATE FROM DATE
               IF WS-PM-NOW-2-DATE > WS-PM-NOW-1-DATE
                   MOVE 24000000 TO WS-PM-WRK-ONE-DAY
               END-IF
               ACCEPT WS-PM-NOW-2-TIME FROM TIME
               COMPUTE WS-PM-DELTA-TIME = (WS-PM-NOW-2-TIME +
               WS-PM-WRK-ONE-DAY - WS-PM-NOW-1-TIME)
           END-PERFORM.
           PERFORM RESETEAR.
      ******************************************************************
       RESETEAR.
           MOVE 0 TO WS-PM-DELTA-TIME.
      ******************************************************************
       INI--WS-IMG-FIGHT.
      * TODO : Read from file
           MOVE "                             " TO WS-IMG-FIGHT-M(01).
           MOVE "        /-/--\               " TO WS-IMG-FIGHT-M(02).
           MOVE "      (@~@)   )/\            " TO WS-IMG-FIGHT-M(03).
           MOVE "  ___/--      \  |           " TO WS-IMG-FIGHT-M(04).
           MOVE " (oo)__ _      )_/           " TO WS-IMG-FIGHT-M(05).
           MOVE "  ^^___/       \             " TO WS-IMG-FIGHT-M(06).
           MOVE "        \       |/-\         " TO WS-IMG-FIGHT-M(07).
           MOVE "         (      )   |        " TO WS-IMG-FIGHT-M(08).
           MOVE "         |       \_/         " TO WS-IMG-FIGHT-M(09).

           MOVE "           _____   _____     " TO WS-IMG-FIGHT-S(01).
           MOVE "          /     \ /     \    " TO WS-IMG-FIGHT-S(02).
           MOVE "     ,   |       '       |   " TO WS-IMG-FIGHT-S(03).
           MOVE "     I __L________       L__ " TO WS-IMG-FIGHT-S(04).
           MOVE "O====IE__________/     ./___>" TO WS-IMG-FIGHT-S(05).
           MOVE "     I      \.       ./      " TO WS-IMG-FIGHT-S(06).
           MOVE "     `        \.   ./        " TO WS-IMG-FIGHT-S(07).
           MOVE "                \ /          " TO WS-IMG-FIGHT-S(08).
           MOVE "                 '           " TO WS-IMG-FIGHT-S(09).
      ******************************************************************
       FIGHT-MONSTER-ANIMATION.
           PERFORM WS-MAX-ANIMATION-CYCLES TIMES
               MOVE WS-INI-IMG-LINE TO WS-IMG-FIGHT-LINE-I
               MOVE 1 TO  WS-IMG-FIGHT-PHOTOGRAM
               PERFORM WS-MAX-IMG-FIGHT-LENGTH TIMES
                   DISPLAY WS-IMG-FIGHT-M(WS-IMG-FIGHT-PHOTOGRAM)
                     AT LINE WS-IMG-FIGHT-LINE-I COL 10
                   ADD 1 TO WS-IMG-FIGHT-PHOTOGRAM
                   ADD 1 TO WS-IMG-FIGHT-LINE-I
               END-PERFORM
               PERFORM PAUSE

               MOVE WS-INI-IMG-LINE TO WS-IMG-FIGHT-LINE-I
               MOVE 1 TO WS-IMG-FIGHT-PHOTOGRAM
               PERFORM WS-MAX-IMG-FIGHT-LENGTH TIMES
                   DISPLAY  WS-IMG-FIGHT-S(WS-IMG-FIGHT-PHOTOGRAM)
                     AT LINE WS-IMG-FIGHT-LINE-I COL 10
                   ADD 1 TO WS-IMG-FIGHT-PHOTOGRAM
                   ADD 1 TO WS-IMG-FIGHT-LINE-I
               END-PERFORM
               PERFORM PAUSE
           END-PERFORM.
      ******************************************************************
       PRESS-KEY-TO-CONTINUE.
           DISPLAY "Press ENTER to continue ... "
             LINE 25 COL 1.
           ACCEPT WS-AUX-ALPHA
             LINE 25 COL 36.
      ******************************************************************
       DISPLAY-INTRO.
           DISPLAY SS-TITLE-1.
           PERFORM PAUSE.
           DISPLAY SS-TITLE-2.
           PERFORM PAUSE.
           DISPLAY SS-TITLE-3.
           PERFORM PAUSE.
           DISPLAY SS-TITLE-4.
           ACCEPT SS-INTRO.
           DISPLAY SS-CLEAR-SCREEN.
      ******************************************************************
       DISPLAY-CREDITS-MOVE.
           DISPLAY SS-CLEAR-SCREEN.
           PERFORM VARYING WS-C-POS FROM 1 BY 1
             UNTIL WS-C-POS > WS-MAX-ARR
               MOVE WS-SCREEN-LINE-POS TO WS-C-SCREEN-LINE
               PERFORM VARYING WS-C-I FROM WS-C-POS BY 1
                 UNTIL WS-C-I > WS-MAX-ARR
                   DISPLAY WS-C-ARR(WS-C-I) LINE WS-C-SCREEN-LINE COL 25
                   ADD 1 TO WS-C-SCREEN-LINE
               END-PERFORM
               COMPUTE WS-C-FINAL-POS = WS-C-POS - 1
               PERFORM VARYING WS-C-I FROM 1 BY 1
                 UNTIL WS-C-I > WS-C-FINAL-POS
                   DISPLAY "                                           "
                   LINE WS-C-SCREEN-LINE COL 25
                   ADD 1 TO WS-C-SCREEN-LINE
               END-PERFORM
               PERFORM PAUSE
           END-PERFORM.
      ******************************************************************
       WIN.
           DISPLAY SS-CLEAR-SCREEN.
      *****YOU
           DISPLAY
           "8b        d8  ad8888ba    88        88"
           AT LINE 2 COL 10.
           DISPLAY
           " Y8      8P d8b      d8b  88        88"
           AT LINE 3 COL 10.
           DISPLAY
           "  Y8    8P d8          8b 88        88"
           AT LINE 4 COL 10.
           DISPLAY
           "   8aaaa8  88          88 88        88"
           AT LINE 5 COL 10.
           DISPLAY
           "     88    88          88 88        88"
           AT LINE 6 COL 10.
           DISPLAY
           "     88    Y8          8P 88        88"
           AT LINE 7 COL 10.
           DISPLAY
           "     88     Y8a      a8P  Y8a      a8P"
           AT LINE 8 COL 10.
           DISPLAY
           "     88       Y888888Y      Y888888Y"
           AT LINE 9 COL 10.
      *****WIN!
           DISPLAY
           "I8         8         8I 88 888b      88       88"
           AT LINE 12 COL 13.
           DISPLAY
           " 8b       d8b       d8  88 8888b     88       88"
           AT LINE 13 COL 13.
           DISPLAY
           "  8       8 8       8   88 88  8b    88       88"
           AT LINE 14 COL 13.
           DISPLAY
           "  Y8     8P Y8     8P   88 88   8b   88       88"
           AT LINE 15 COL 13.
           DISPLAY
           "   8b   d8   8b   d8    88 88    8b  88       88"
           AT LINE 16 COL 13.
           DISPLAY
           "    8a a8     8a a8     88 88     8b 88"
           AT LINE 17 COL 13.
           DISPLAY
           "     8a8       8a8      88 88      8888       aa"
           AT LINE 18 COL 13.
           DISPLAY
           "      8         8       88 88       888       88"
           AT LINE 19 COL 13.
           DISPLAY "YOU WIN!!!!"
           AT LINE 22 COL 35.
           PERFORM PRESS-KEY-TO-CONTINUE.
           DISPLAY SS-CLEAR-SCREEN.
      ******************************************************************
       LOSE.
           DISPLAY SS-CLEAR-SCREEN.
      *****GAME
           DISPLAY
           "  ad8888ba          db        88b           d88 88888888888"
           AT LINE 2 COL 10.
           DISPLAY
           " d8      8b        d88b       888b         d888 88"
           AT LINE 3 COL 10.
           DISPLAY
           "d8                d8  8b      88 8b       d8 88 88"
           AT LINE 4 COL 10.
           DISPLAY
           "88               d8    8b     88  8b     d8  88 88aaaaa"
           AT LINE 5 COL 10.
           DISPLAY
           "88      88888   d8YaaaaY8b    88   8b   d8   88 88"
           AT LINE 6 COL 10.
           DISPLAY
           "Y8         88  d8        8b   88    8b d8    88 88"
           AT LINE 7 COL 10.
           DISPLAY
           " Y8a      a88 d8          8b  88     888     88 88"
           AT LINE 8 COL 10.
           DISPLAY
           "  Y88888888P  d8          8b  88      8      88 88888888888"
           AT LINE 9 COL 10.
      *****OVER
           DISPLAY
           "  ad8888ba  8b           d8 88888888888 88888888ba"
           AT LINE 12 COL 13.
           DISPLAY
           " d8      8b  8b         d8  88          88       8b"
           AT LINE 13 COL 13.
           DISPLAY
           "d8        8b  8b       d8   88          88       8P"
           AT LINE 14 COL 13.
           DISPLAY
           "88        88   8b     d8    88aaaaa     88aaaaaa8P"
           AT LINE 15 COL 13.
           DISPLAY
           "88        88    8b   d8     88          88    88"
           AT LINE 16 COL 13.
           DISPLAY
           "Y8        8P     8b d8      88          88     8b"
           AT LINE 17 COL 13.
           DISPLAY
           " Y8a     a8P      888       88          88      8b"
           AT LINE 18 COL 13.
           DISPLAY
           "  Y8888888Y        8        88888888888 88       8b"
           AT LINE 19 COL 13.
           DISPLAY "YOU LOSE!   "
           AT LINE 22 COL 35.
           PERFORM PRESS-KEY-TO-CONTINUE.
           DISPLAY SS-CLEAR-SCREEN.
      ******************************************************************
       INI-CREDITS-ARRAY.
           MOVE "        ORIGINAL CONCEPT         " TO WS-C-ARR(40).
           MOVE "        ----------------         " TO WS-C-ARR(41).
           MOVE "        Albert Llaurado          " TO WS-C-ARR(42).
           MOVE "                                 " TO WS-C-ARR(43).
           MOVE "           DESIGNERS             " TO WS-C-ARR(44).
           MOVE "           ---------             " TO WS-C-ARR(45).
           MOVE "       Juan Ramon Espuny         " TO WS-C-ARR(46).
           MOVE "       Juan Jose Sanchez         " TO WS-C-ARR(47).
           MOVE "                                 " TO WS-C-ARR(48).
           MOVE "    LEVEL / SCENARIO DESIGN      " TO WS-C-ARR(49).
           MOVE "    -----------------------      " TO WS-C-ARR(50).
           MOVE "       Juan Jose Sanchez         " TO WS-C-ARR(51).
           MOVE "       Juan Ramon Espuny         " TO WS-C-ARR(52).
           MOVE "                                 " TO WS-C-ARR(53).
           MOVE "          PROGRAMMING            " TO WS-C-ARR(54).
           MOVE "          -----------            " TO WS-C-ARR(55).
           MOVE "       Juan Jose Sanchez         " TO WS-C-ARR(56).
           MOVE "       Juan Ramon Espuny         " TO WS-C-ARR(57).
           MOVE "                                 " TO WS-C-ARR(58).
           MOVE "    LEVEL / SCENARIO DESIGN      " TO WS-C-ARR(59).
           MOVE "    -----------------------      " TO WS-C-ARR(60).
           MOVE "       Lead Programming:         " TO WS-C-ARR(61).
           MOVE "       Juan Ramon Espuny         " TO WS-C-ARR(62).
           MOVE "     Additional Programming:     " TO WS-C-ARR(63).
           MOVE "       Juan Jose Sanchez         " TO WS-C-ARR(64).
           MOVE "                                 " TO WS-C-ARR(65).
           MOVE "GAME ENGINE / DEVELOPMENT SYSTEM " TO WS-C-ARR(66).
           MOVE "-------------------------------- " TO WS-C-ARR(67).
           MOVE "         OpenCobol IDE           " TO WS-C-ARR(68).
           MOVE "                                 " TO WS-C-ARR(69).
           MOVE "      GRAPHICS PROGRAMMING       " TO WS-C-ARR(70).
           MOVE "    -----------------------      " TO WS-C-ARR(71).
           MOVE "       Juan Jose Sanchez         " TO WS-C-ARR(72).
           MOVE "       Juan Ramon Espuny         " TO WS-C-ARR(73).
           MOVE "                                 " TO WS-C-ARR(74).
           MOVE "       SOUND PROGRAMMING         " TO WS-C-ARR(75).
           MOVE "    -----------------------      " TO WS-C-ARR(76).
           MOVE "       Juan Jose Sanchez         " TO WS-C-ARR(77).
           MOVE "                                 " TO WS-C-ARR(78).
           MOVE "                                 " TO WS-C-ARR(79).
           MOVE "      Thanks for playing!        " TO WS-C-ARR(80).
           MOVE "                                 " TO WS-C-ARR(81).
           MOVE "                                 " TO WS-C-ARR(82).
           MOVE "Special Thanks to Albert Llaurado" TO WS-C-ARR(83).
      ******************************************************************
       STOP-RUN.
           STOP RUN.
       END PROGRAM DUNGCRWL.
