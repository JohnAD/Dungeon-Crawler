       IDENTIFICATION DIVISION.
       PROGRAM-ID. MOD-HEROE.
      ******************************************************************
      * JUEGO: DUNGEON CRAWLER                                          *
      ******************************************************************
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 HEROES-MENU.
           05 H-MENU-TITLE.
               10 WS-HM-HEADING    PIC X(17)
                 VALUE "MODIFICAR HEROE: ".
               10 FILLER           PIC X(01) VALUE X"0A".
               10 FILLER           PIC X(17) VALUE ALL "-".
               10 FILLER           PIC X(01) VALUE X"0A".
               10 FILLER           PIC X(01) VALUE X"0A".
           05 H-MENU-CONTENT.
               10 STRENGTH         PIC X(11) VALUE "1- Fuerza: ".
               10 FILLER           PIC X(01) VALUE X"0A".
               10 AGILITY          PIC X(13) VALUE "2- Agilidad: ".
               10 FILLER           PIC X(01) VALUE X"0A".
               10 LEVEL            PIC X(10) VALUE "3- Nivel: ".
               10 FILLER           PIC X(01) VALUE X"0A".
               10 HP               PIC X(16) VALUE "4- Puntos Vida: ".
               10 FILLER           PIC X(01) VALUE X"0A".
               10 PROFESSION       PIC X(14) VALUE "5- Profesion: ".
           05 H-MENU-FOOTER.
               10 FILLER           PIC X(01) VALUE X"0A".
               10 FILLER           PIC X(09) VALUE "0- Salir".
               10 FILLER           PIC X(01) VALUE X"0A".
               10 FILLER           PIC X(21)
                 VALUE "Escoge una opcion: ".
       LINKAGE SECTION.
       01 LS-HEROE.
           05 LS-H-ID                PIC 9(02).
           05 LS-H-STRENGTH          PIC 9(02).
           05 LS-H-AGILITY           PIC 9(02).
           05 LS-H-LEVEL             PIC 9(02).
           05 LS-H-HP                PIC 9(02).
           05 LS-H-PROFESSION        PIC 9(02).
       PROCEDURE DIVISION USING LS-HEROE.
       MAIN-PROCEDURE.
            DISPLAY H-MENU-TITLE.
            PERFORM DISPLAY-H-MENU-CONTENT.
      ******************************************************************
       DISPLAY-H-MENU-CONTENT.
           DISPLAY STRENGTH LS-H-STRENGTH.
           DISPLAY AGILITY LS-H-AGILITY.
           DISPLAY LEVEL LS-H-LEVEL.
           DISPLAY HP LS-H-HP.
           DISPLAY PROFESSION LS-H-PROFESSION.
       EXIT PROGRAM.
