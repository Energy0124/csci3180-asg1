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
       01 GENERATION-NUMBER PIC 9(5).
       01 ROW-AND-COLUMN PIC X(10).
       01 PATTERN-LINE PIC X(90).
       01 OUTPUT-FILENAME PIC X(100).
       01 ROW-SIZE PIC 9(3).
       01 COLUMN-SIZE PIC 9(3).
       01 STATUS-SIZE PIC 9(3).
       01 STRING-POINTER PIC 9(3).
       01 PATTERN-TABLE.
           02 PATTERN-ROW OCCURS 1 TO 100 TIMES
               DEPENDING ON ROW-SIZE.
               03 PATTERN-CELL PIC X OCCURS 80 TIMES.
       01 PREVIOUS-PATTERN-TABLE.
           02 PREVIOUS-PATTERN-ROW OCCURS 1 TO 100 TIMES
               DEPENDING ON ROW-SIZE.
               03 PREVIOUS-PATTERN-CELL PIC X OCCURS 80 TIMES.
       01 PATTERN-READ-COUNTER PIC 9(3).
       01 PATTERN-PRINT-COUNTER PIC 9(3).
       01 COPY-PATTERN-LOOP-COUNTER PIC 9(3).
       01 GENERATION-COUNTER PIC 9(5).
       01 COMPARE-PATTERN-COUNTER PIC 9(3).
      *>  01 COMPARE-PATTERN-RESULT.
       01 COMPARE-PATTERN-IS-SAME PIC 9 VALUE 1.

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

           PERFORM READ-PATTERN

      *>   PRINT DEBUG MESSAGE
           DISPLAY PATTERN-NAME
           DISPLAY GENERATION-NUMBER
           DISPLAY ROW-AND-COLUMN
           DISPLAY ROW-SIZE
           DISPLAY COLUMN-SIZE

           PERFORM PRINT-PATTERN
           PERFORM COPY-PATTERN
           PERFORM COMPARE-PATTERN

           IF(COMPARE-PATTERN-IS-SAME=1) THEN
               DISPLAY "IS SAME"
           END-IF
           IF(COMPARE-PATTERN-IS-SAME=0) THEN
               DISPLAY "NOT IS SAME"
           END-IF

           MOVE PATTERN-LINE TO OUTPUT-PATTERN-LINE
           WRITE OUTPUT-PATTERN-LINE
           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE
           STOP RUN.

       PRINT-PATTERN.
      *>   LOOP
           MOVE 1 TO PATTERN-PRINT-COUNTER
           PERFORM PRINT-PATTERN-LOOP.
      *>   END LOOP
       PRINT-PATTERN-LOOP.
           IF (PATTERN-PRINT-COUNTER <= ROW-SIZE) THEN
               DISPLAY PATTERN-ROW(PATTERN-PRINT-COUNTER)(1:COLUMN-SIZE)
               ADD 1 TO PATTERN-PRINT-COUNTER
               GO TO PRINT-PATTERN-LOOP
           END-IF.

       READ-PATTERN.
      *>   LOOP
           MOVE 1 TO PATTERN-READ-COUNTER
           PERFORM READ-PATTERN-LOOP.
      *>   END LOOP
       READ-PATTERN-LOOP.
           IF (PATTERN-READ-COUNTER <= ROW-SIZE) THEN
               READ INPUT-FILE INTO PATTERN-ROW(PATTERN-READ-COUNTER)
               MOVE PATTERN-ROW(PATTERN-READ-COUNTER)
                   TO PREVIOUS-PATTERN-ROW(PATTERN-READ-COUNTER)
               ADD 1 TO PATTERN-READ-COUNTER
               GO TO READ-PATTERN-LOOP
           END-IF.

       COPY-PATTERN.
           MOVE 1 TO COPY-PATTERN-LOOP-COUNTER
           PERFORM COPY-PATTERN-LOOP.
       COPY-PATTERN-LOOP.
           IF(COPY-PATTERN-LOOP-COUNTER <= ROW-SIZE) THEN
               MOVE PATTERN-ROW(COPY-PATTERN-LOOP-COUNTER)
                   TO PREVIOUS-PATTERN-ROW(COPY-PATTERN-LOOP-COUNTER)
               ADD 1 TO COPY-PATTERN-LOOP-COUNTER
               GO TO COPY-PATTERN-LOOP
           END-IF.

       PATTERN-SIMULATION.
           MOVE 0 TO GENERATION-COUNTER
           PERFORM PATTERN-SIMULATION-LOOP.
       PATTERN-SIMULATION-LOOP.
      *>      TODO: CHECK IF I SHOULD USE < OR <=
      *>      NEED TO SIMULATION ONE EXTRA GENERATION TO SEE IF IT IS STILL LIFE
           IF(GENERATION-COUNTER <= GENERATION-NUMBER) THEN
               PERFORM COPY-PATTERN
               ADD 1 TO GENERATION-COUNTER
               GO TO PATTERN-SIMULATION-LOOP
           END-IF.
       COMPARE-PATTERN.
           MOVE 1 TO COMPARE-PATTERN-COUNTER
           PERFORM COMPARE-PATTERN-LOOP.
       COMPARE-PATTERN-LOOP.
              IF(COMPARE-PATTERN-COUNTER <= ROW-SIZE) THEN
                   IF(PATTERN-ROW(COMPARE-PATTERN-COUNTER) NOT EQUAL TO
                   PREVIOUS-PATTERN-ROW(COMPARE-PATTERN-COUNTER)) THEN
                       MOVE 0 TO COMPARE-PATTERN-IS-SAME
                       DISPLAY "LUL"
                   END-IF
               ADD 1 TO COMPARE-PATTERN-COUNTER
               GO TO COMPARE-PATTERN-LOOP
           END-IF.
