       IDENTIFICATION DIVISION.
       PROGRAM-ID. MOD-HEROE.
      ******************************************************************
      * JUEGO: DUNGEON CRAWLER                                          *
      ******************************************************************
       DATA DIVISION.
       FILE SECTION.
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
            PERFORM DISPLAY-MOD-H-TITLE.
       DISPLAY-MOD-H-TITLE.

       EXIT PROGRAM.
