      *    CSCI3180 Principles of Programming Languages
      *
      *    --- Declaration ---
      *
      *    I declare that the assignment here submitted is original except for source
      *    material explicitly acknowledged. I also acknowledge that I am aware of
      *    University policy and regulations on honesty in academic work, and of the
      *    disciplinary guidelines and procedures applicable to breaches of such policy
      *    and regulations, as contained in the website
      *    http://www.cuhk.edu.hk/policy/academichonesty/
      *
      *    Assignment 1
      *    Name : Ling Leong
      *    Student ID : 1155062557
      *    Email Addr : alanalan0124@yahoo.com.hk
      *
       IDENTIFICATION DIVISION.
       PROGRAM-ID.   LIFE.
      *
      *The game of life

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO 'input.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO OUTPUT-FILENAME
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-FILE-LINE PIC X(90).
       FD OUTPUT-FILE.
       01 OUTPUT-PATTERN-LINE.
           02 OUTPUT-PATTERN-CHAR PIC X OCCURS 1 TO 80 TIMES
               DEPENDING ON COLUMN-SIZE.
       01 OUTPUT-STATUS-LINE.
           02 OUTPUT-STATUS-CHAR PIC X OCCURS 1 TO 100 TIMES
               DEPENDING ON STATUS-SIZE.
       WORKING-STORAGE SECTION.
      *>  Input file
       01 PATTERN-NAME PIC X(90).
       01 GENERATION-NUMBER PIC X(5).
       01 ROW-AND-COLUMN PIC X(10).
       01 PATTERN-LINE PIC X(90).
       01 OUTPUT-FILENAME PIC X(100).
       01 ROW-SIZE PIC Z(3).
       01 COLUMN-SIZE PIC Z(3).
       01 STATUS-SIZE PIC Z(3).
       01 STRING-POINTER PIC Z(3).
       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           OPEN INPUT INPUT-FILE
           READ INPUT-FILE INTO PATTERN-NAME
           MOVE 1 TO STRING-POINTER
           STRING PATTERN-NAME DELIMITED BY SPACE
               'cob.txt' DELIMITED BY SIZE
               INTO OUTPUT-FILENAME
           OPEN OUTPUT OUTPUT-FILE
           READ INPUT-FILE INTO GENERATION-NUMBER
           READ INPUT-FILE INTO ROW-AND-COLUMN.
           UNSTRING ROW-AND-COLUMN DELIMITED BY SPACE
               INTO ROW-SIZE, COLUMN-SIZE
           END-UNSTRING
           READ INPUT-FILE INTO PATTERN-LINE
           DISPLAY PATTERN-NAME
           DISPLAY GENERATION-NUMBER
           DISPLAY ROW-AND-COLUMN
           DISPLAY ROW-SIZE
           DISPLAY COLUMN-SIZE
           DISPLAY PATTERN-LINE
           MOVE PATTERN-LINE TO OUTPUT-PATTERN-LINE
           WRITE OUTPUT-PATTERN-LINE
           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE
           STOP RUN.
