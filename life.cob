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
       01 PATTERN-WRITE-COUNTER PIC 9(3).
       01 PATTERN-PRINT-COUNTER PIC 9(3).
       01 COPY-PATTERN-LOOP-COUNTER PIC 9(3).
       01 GENERATION-COUNTER PIC 9(5).
       01 COMPARE-PATTERN-COUNTER PIC 9(3).
      *>  01 COMPARE-PATTERN-RESULT.
       01 COMPARE-PATTERN-IS-SAME PIC 9 VALUE 0.
       01 COUNT-CELLS-ROW-OFFSET PIC S9.
       01 COUNT-CELLS-COLUMN-OFFSET PIC S9.
       01 COUNT-CELLS-CURRENT-ROW PIC 9(3).
       01 COUNT-CELLS-TEMP-ROW PIC 9(3).
       01 COUNT-CELLS-CURRENT-COLUMN  PIC 9(3).
       01 COUNT-CELLS-TEMP-COLUMN  PIC 9(3).
       01 COUNT-CELLS-RESULT PIC 9.
       01 SIMULATION-CURRENT-ROW PIC 9(3).
       01 SIMULATION-CURRENT-COLUMN  PIC 9(3).
       01 STILL-LIFE-GENERATION PIC 9(5) VALUE 0.
       01 IS-STILL-LIFE PIC 9 VALUE 0.
       01 STATUS-STRING PIC X(100).
       01 STATUS-LENGTH PIC 9(3) VALUE 1.
       01 STATUS-CASE PIC 9.
       01 STILL-LIFE-GENERATION-STRING PIC Z(6).
       01 GENERATION-NUMBER-STRING PIC Z(6).
       01 GENERATION-NUMBER-TRIMED PIC X(6).
       01 STILL-LIFE-GENERATION-TRIMED PIC X(6).
       01 TEMP-NUMBER PIC X(5).


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
      *>      DISPLAY PATTERN-NAME
      *>      DISPLAY GENERATION-NUMBER
      *>      DISPLAY ROW-AND-COLUMN
      *>      DISPLAY ROW-SIZE
      *>      DISPLAY COLUMN-SIZE

      *>      PERFORM PRINT-PATTERN
      *>      PERFORM COPY-PATTERN

           PERFORM PATTERN-SIMULATION
      *>      DISPLAY STILL-LIFE-GENERATION
           PERFORM WRITE-PATTERN

           MOVE GENERATION-NUMBER TO GENERATION-NUMBER-STRING
           UNSTRING GENERATION-NUMBER-STRING DELIMITED BY ALL SPACE
               INTO TEMP-NUMBER, GENERATION-NUMBER-TRIMED
      *>      DISPLAY GENERATION-NUMBER-STRING
      *>      DISPLAY GENERATION-NUMBER-TRIMED

           MOVE STILL-LIFE-GENERATION TO STILL-LIFE-GENERATION-STRING
           UNSTRING STILL-LIFE-GENERATION-STRING DELIMITED BY ALL SPACE
               INTO TEMP-NUMBER, STILL-LIFE-GENERATION-TRIMED
      *>      DISPLAY STILL-LIFE-GENERATION-STRING
      *>      DISPLAY STILL-LIFE-GENERATION-TRIMED

           IF(IS-STILL-LIFE=1 AND STILL-LIFE-GENERATION=0) THEN
               STRING "It is a still life initially." DELIMITED BY SIZE
                   INTO STATUS-STRING
                   WITH POINTER STATUS-LENGTH
               END-STRING
           END-IF
           IF(IS-STILL-LIFE=1 AND STILL-LIFE-GENERATION =1) THEN
               STRING "It is a still life after " DELIMITED BY SIZE
                   STILL-LIFE-GENERATION-TRIMED DELIMITED BY SPACE
                   " step." DELIMITED BY SIZE
                   INTO STATUS-STRING
                   WITH POINTER STATUS-LENGTH
               END-STRING
           END-IF
           IF(IS-STILL-LIFE=1 AND STILL-LIFE-GENERATION >1) THEN
               STRING "It is a still life after " DELIMITED BY SIZE
                   STILL-LIFE-GENERATION-TRIMED DELIMITED BY SPACE
                   " steps." DELIMITED BY SIZE
                   INTO STATUS-STRING
                   WITH POINTER STATUS-LENGTH
               END-STRING
           END-IF
           IF(IS-STILL-LIFE=0 AND GENERATION-NUMBER <=1) THEN
               STRING "It is still not a still life even after "
                   DELIMITED BY SIZE
                   GENERATION-NUMBER-TRIMED DELIMITED BY SPACE
                   " step." DELIMITED BY SIZE
                   INTO STATUS-STRING
                   WITH POINTER STATUS-LENGTH
               END-STRING
           END-IF
           IF(IS-STILL-LIFE=0 AND GENERATION-NUMBER >1) THEN
               STRING "It is still not a still life even after "
                   DELIMITED BY SIZE
                   GENERATION-NUMBER-TRIMED DELIMITED BY SPACE
                   " steps." DELIMITED BY SIZE
                   INTO STATUS-STRING
                   WITH POINTER STATUS-LENGTH
               END-STRING
           END-IF


           MOVE STATUS-LENGTH TO STATUS-SIZE
      *>      DISPLAY STATUS-LENGTH
      *>      DISPLAY STATUS-SIZE
           MOVE STATUS-STRING TO OUTPUT-STATUS-LINE
           WRITE OUTPUT-STATUS-LINE

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

       WRITE-PATTERN.
      *>   LOOP
           MOVE 1 TO PATTERN-WRITE-COUNTER
           PERFORM WRITE-PATTERN-LOOP.
      *>   END LOOP
       WRITE-PATTERN-LOOP.
           IF (PATTERN-WRITE-COUNTER <= ROW-SIZE) THEN
               MOVE PREVIOUS-PATTERN-ROW(PATTERN-WRITE-COUNTER)
                   TO OUTPUT-PATTERN-LINE
               WRITE OUTPUT-PATTERN-LINE
               ADD 1 TO PATTERN-WRITE-COUNTER
               GO TO WRITE-PATTERN-LOOP
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
           MOVE 1 TO GENERATION-COUNTER
           PERFORM PATTERN-SIMULATION-LOOP.
       PATTERN-SIMULATION-LOOP.
      *>      NEED TO SIMULATION ONE EXTRA GENERATION TO SEE IF IT IS STILL LIFE
           IF(GENERATION-COUNTER <= GENERATION-NUMBER + 1 AND
                   IS-STILL-LIFE NOT= 1) THEN
               PERFORM COPY-PATTERN
               MOVE 1 TO SIMULATION-CURRENT-ROW
               MOVE 1 TO SIMULATION-CURRENT-COLUMN
               PERFORM SIMULATION-ROW-LOOP
      *>          DISPLAY GENERATION-COUNTER
      *>          PERFORM PRINT-PATTERN
               PERFORM COMPARE-PATTERN
               IF(COMPARE-PATTERN-IS-SAME=1) THEN
                   MOVE 1 TO IS-STILL-LIFE
                   COMPUTE STILL-LIFE-GENERATION
                       = GENERATION-COUNTER - 1
               END-IF
               ADD 1 TO GENERATION-COUNTER
               GO TO PATTERN-SIMULATION-LOOP
           END-IF.
       SIMULATION-ROW-LOOP.
           IF(SIMULATION-CURRENT-ROW <= ROW-SIZE) THEN
               MOVE SIMULATION-CURRENT-ROW TO
                   COUNT-CELLS-CURRENT-ROW
               PERFORM SIMULATION-COLUMN-LOOP
               ADD 1 TO SIMULATION-CURRENT-ROW
               MOVE 1 TO SIMULATION-CURRENT-COLUMN
               GO TO SIMULATION-ROW-LOOP
           END-IF.
       SIMULATION-COLUMN-LOOP.
           IF(SIMULATION-CURRENT-COLUMN <= COLUMN-SIZE) THEN
               MOVE SIMULATION-CURRENT-COLUMN TO
                   COUNT-CELLS-CURRENT-COLUMN
               PERFORM COUNT-CELLS
               IF(COUNT-CELLS-RESULT=3)THEN
                   MOVE '*' TO PATTERN-CELL(SIMULATION-CURRENT-ROW,
                       SIMULATION-CURRENT-COLUMN)
               END-IF
               IF(COUNT-CELLS-RESULT NOT=3 AND
                   COUNT-CELLS-RESULT NOT=2)THEN
                   MOVE '0' TO PATTERN-CELL(SIMULATION-CURRENT-ROW,
                       SIMULATION-CURRENT-COLUMN)
               END-IF
               ADD 1 TO SIMULATION-CURRENT-COLUMN
               GO TO SIMULATION-COLUMN-LOOP
           END-IF.

       COMPARE-PATTERN.
           MOVE 1 TO COMPARE-PATTERN-COUNTER
           MOVE 1 TO COMPARE-PATTERN-IS-SAME
           PERFORM COMPARE-PATTERN-LOOP.
       COMPARE-PATTERN-LOOP.
              IF(COMPARE-PATTERN-COUNTER <= ROW-SIZE) THEN
                   IF(PATTERN-ROW(COMPARE-PATTERN-COUNTER) NOT EQUAL TO
                   PREVIOUS-PATTERN-ROW(COMPARE-PATTERN-COUNTER)) THEN
                       MOVE 0 TO COMPARE-PATTERN-IS-SAME
                   END-IF
               ADD 1 TO COMPARE-PATTERN-COUNTER
               GO TO COMPARE-PATTERN-LOOP
           END-IF.

      *>      NEED TO SET COUNT-CELLS-CURRENT-ROW AND COUNT-CELLS-CURRENT-COLUMN
      *>      BEFORE CALLING THIS FUNCTION
       COUNT-CELLS.
           MOVE -1 TO COUNT-CELLS-ROW-OFFSET
           MOVE -1 TO COUNT-CELLS-COLUMN-OFFSET
           MOVE 0 TO COUNT-CELLS-RESULT
           PERFORM COUNT-CELLS-LOOP.
       COUNT-CELLS-LOOP.
           IF(COUNT-CELLS-ROW-OFFSET<=1) THEN
               PERFORM COUNT-CELLS-COLUMN-LOOP
               MOVE -1 TO COUNT-CELLS-COLUMN-OFFSET
               ADD 1 TO COUNT-CELLS-ROW-OFFSET
               GO TO COUNT-CELLS-LOOP
           END-IF.
       COUNT-CELLS-COLUMN-LOOP.
           IF(COUNT-CELLS-COLUMN-OFFSET<=1) THEN
               COMPUTE COUNT-CELLS-TEMP-ROW =
                   COUNT-CELLS-CURRENT-ROW + COUNT-CELLS-ROW-OFFSET
               COMPUTE COUNT-CELLS-TEMP-COLUMN =
                   COUNT-CELLS-CURRENT-COLUMN
                   + COUNT-CELLS-COLUMN-OFFSET

               IF(COUNT-CELLS-TEMP-ROW >= 1
                   AND COUNT-CELLS-TEMP-ROW <= ROW-SIZE AND
                   COUNT-CELLS-TEMP-COLUMN >= 1
                   AND COUNT-CELLS-TEMP-COLUMN <= COLUMN-SIZE
                   AND NOT(COUNT-CELLS-ROW-OFFSET = 0 AND
                   COUNT-CELLS-COLUMN-OFFSET = 0)
                   AND PREVIOUS-PATTERN-CELL(
                   COUNT-CELLS-TEMP-ROW, COUNT-CELLS-TEMP-COLUMN)
                   = '*'
                   )
               THEN
                   ADD 1 TO COUNT-CELLS-RESULT
               END-IF
               ADD 1 TO COUNT-CELLS-COLUMN-OFFSET
               GO TO COUNT-CELLS-COLUMN-LOOP
           END-IF.
