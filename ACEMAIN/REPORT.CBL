       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACEREPT.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT INST-FILE    ASSIGN TO INSTFILE
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS INST-NUM
               FILE STATUS IS WS-INST-STATUS.
            SELECT STU-FILE     ASSIGN TO STUFILE
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS STU-NUM
               FILE STATUS IS WS-STU-STATUS.
            SELECT REPORT-FILE  ASSIGN TO RPTFILE
               ORGANIZATION IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  INST-FILE.
       01  INST-RECORD.
           05 INST-NUM      PIC X(05).
           05 INST-NAME     PIC X(20).
           05 INST-SALARY   PIC 9(05).
           05 INST-STATUS   PIC X(01).
           05 FILLER        PIC X(49).
       FD  STU-FILE.
       01  STU-RECORD.
          05 STU-NUM        PIC X(05).
          05 STU-NAME       PIC X(20).
          05 STU-ADDRESS    PIC X(20).
          05 STU-START-DATE PIC X(10).
          05 STU-INST-NUM   PIC X(5).
          05 STU-STATUS     PIC X(1).
          05 STU-PF-DATE    PIC X(10).
          05 FILLER         PIC X(09).
       FD  REPORT-FILE
           RECORDING MODE F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 132 CHARACTERS.
       01  REPORT-LINE             PIC X(132).
       WORKING-STORAGE SECTION.
       01  WS-INST-STATUS          PIC X(02) VALUE SPACES.
       01  WS-STU-STATUS           PIC X(02) VALUE SPACES.
       01  WS-COUNTERS.
           05  WS-PAGE-NUM         PIC S9(05)  COMP-3 VALUE ZEROS.
           05  WS-LINE-COUNT       PIC S9(05)  COMP-3 VALUE ZEROS.
           05  WS-TOTAL-STU        PIC S9(07)  COMP-3 VALUE ZEROS.
           05  WS-ACTIVE-STU       PIC S9(07)  COMP-3 VALUE ZEROS.
           05  WS-PASSIVE-STU      PIC S9(07)  COMP-3 VALUE ZEROS.
           05  WS-INST-COUNT       PIC S9(05)  COMP-3 VALUE ZEROS.
       01  WS-COMP-FIELDS.
           05  WS-STU-NUM-C        PIC S9(05)  COMP.
           05  WS-INST-NUM-C       PIC S9(05)  COMP.
       01  WS-MAX-LINES            PIC S9(05)  COMP-3 VALUE 15.
       01  WS-LINES-USED           PIC S9(05)  COMP-3 VALUE 99.
       01  WS-INST-TABLE.
           05  WS-INST-ENTRY       OCCURS 100 TIMES
                                   INDEXED BY WS-INST-IDX.
               10  WS-TAB-INST-NUM PIC X(05).
               10  WS-TAB-INST-NAM PIC X(20).
               10  WS-TAB-INST-SAL PIC 9(05).
               10  WS-TAB-INST-STS PIC X(01).
       01  WS-INST-MAX             PIC S9(05)  COMP-3 VALUE ZEROS.
       01  WS-FOUND-INST-NAME      PIC X(20)  VALUE SPACES.
       01  WS-DATE.
           05  WS-YEAR             PIC X(04).
           05  FILLER              PIC X(01).
           05  WS-MONTH            PIC X(02).
           05  FILLER              PIC X(01).
           05  WS-DAY              PIC X(02).
       01  WS-RPT-DATE             PIC X(10).
       01  HDR-LINE-1.
           05  FILLER              PIC X(01)  VALUE SPACES.
           05  HDR-DATE            PIC X(10).
           05  FILLER              PIC X(05)  VALUE SPACES.
           05  FILLER              PIC X(28)
               VALUE 'ACE DRIVING SCHOOL REPORT'.
           05  FILLER              PIC X(20)  VALUE SPACES.
           05  FILLER              PIC X(05)  VALUE 'PAGE:'.
           05  HDR-PAGE            PIC Z,ZZ9.
           05  FILLER              PIC X(59)  VALUE SPACES.
       01  HDR-LINE-2.
           05  FILLER              PIC X(133) VALUE SPACES.
       01  HDR-LINE-3.
           05  FILLER              PIC X(01)  VALUE SPACES.
           05  FILLER              PIC X(07)  VALUE 'STUDENT'.
           05  FILLER              PIC X(05)  VALUE SPACES.
           05  FILLER              PIC X(07)  VALUE 'STUDENT'.
           05  FILLER              PIC X(13)  VALUE SPACES.
           05  FILLER              PIC X(10)  VALUE 'INSTRUCTOR'.
           05  FILLER              PIC X(08)  VALUE SPACES.
           05  FILLER              PIC X(07)  VALUE 'STUDENT'.
           05  FILLER              PIC X(76)  VALUE SPACES.
       01  HDR-LINE-4.
           05  FILLER              PIC X(01)  VALUE SPACES.
           05  FILLER              PIC X(06)  VALUE 'NUMBER'.
           05  FILLER              PIC X(06)  VALUE SPACES.
           05  FILLER              PIC X(04)  VALUE 'NAME'.
           05  FILLER              PIC X(14)  VALUE SPACES.
           05  FILLER              PIC X(04)  VALUE 'NAME'.
           05  FILLER              PIC X(14)  VALUE SPACES.
           05  FILLER              PIC X(06)  VALUE 'STATUS'.
           05  FILLER              PIC X(78)  VALUE SPACES.
       01  HDR-LINE-5.
           05  FILLER              PIC X(133) VALUE ALL '-'.
       01  DETAIL-LINE.
           05  FILLER              PIC X(01)  VALUE SPACES.
           05  DL-STU-NUM          PIC X(5).
           05  FILLER              PIC X(07)  VALUE SPACES.
           05  DL-STU-NAME         PIC X(20).
           05  FILLER              PIC X(03)  VALUE SPACES.
           05  DL-INST-NAME        PIC X(20).
           05  FILLER              PIC X(03)  VALUE SPACES.
           05  DL-STATUS           PIC X(01).
           05  FILLER              PIC X(79)  VALUE SPACES.
       01  TOTAL-LINE-1.
           05  FILLER              PIC X(01)  VALUE SPACES.
           05  FILLER              PIC X(25)
               VALUE 'TOTAL STUDENTS PROCESSED:'.
           05  TOT-STUDENTS        PIC ZZ,ZZ9.
           05  FILLER              PIC X(101) VALUE SPACES.
       01  TOTAL-LINE-2.
           05  FILLER              PIC X(01)  VALUE SPACES.
           05  FILLER              PIC X(25)
               VALUE 'ACTIVE STUDENTS (C):'.
           05  TOT-ACTIVE          PIC ZZ,ZZ9.
           05  FILLER              PIC X(101) VALUE SPACES.
       01  TOTAL-LINE-3.
           05  FILLER              PIC X(01)  VALUE SPACES.
           05  FILLER              PIC X(25)
               VALUE 'PASSIVE STUDENTS (P/F):'.
           05  TOT-PASSIVE         PIC ZZ,ZZ9.
           05  FILLER              PIC X(101) VALUE SPACES.
       01  WS-EOF-INST             PIC X(01) VALUE 'N'.
       01  WS-EOF-STU              PIC X(01) VALUE 'N'.
       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INIT
           PERFORM 2000-LOAD-INST-ARRAY
           PERFORM 3000-PROCESS-STUDENTS
               UNTIL WS-EOF-STU = 'Y'
           PERFORM 4000-PRINT-TOTALS
           PERFORM 5000-CLOSE
           STOP RUN.
       1000-INIT.
           OPEN INPUT  INST-FILE
                INPUT  STU-FILE
                OUTPUT REPORT-FILE
           MOVE FUNCTION CURRENT-DATE(1:4)  TO WS-YEAR
           MOVE FUNCTION CURRENT-DATE(5:2)  TO WS-MONTH
           MOVE FUNCTION CURRENT-DATE(7:2)  TO WS-DAY
           STRING WS-MONTH '/' WS-DAY '/' WS-YEAR
               DELIMITED SIZE INTO WS-RPT-DATE.
       2000-LOAD-INST-ARRAY.
           READ INST-FILE
               AT END MOVE 'Y' TO WS-EOF-INST
           END-READ
           PERFORM UNTIL WS-EOF-INST = 'Y'
               COMPUTE WS-INST-MAX = WS-INST-MAX + 1
               SET WS-INST-IDX TO WS-INST-MAX
               MOVE INST-NUM    TO WS-TAB-INST-NUM(WS-INST-IDX)
               MOVE INST-NAME   TO WS-TAB-INST-NAM(WS-INST-IDX)
               MOVE INST-SALARY TO WS-TAB-INST-SAL(WS-INST-IDX)
               MOVE INST-STATUS TO WS-TAB-INST-STS(WS-INST-IDX)
      *        INSPECT TO CLEAN UP INSTRUCTOR NAME
               INSPECT WS-TAB-INST-NAM(WS-INST-IDX)
                   CONVERTING LOW-VALUES TO SPACES
               READ INST-FILE
                   AT END MOVE 'Y' TO WS-EOF-INST
               END-READ
           END-PERFORM
           CLOSE INST-FILE.
       3000-PROCESS-STUDENTS.
           READ STU-FILE
               AT END MOVE 'Y' TO WS-EOF-STU
               NOT AT END PERFORM 3100-PROCESS-ONE-STUDENT
           END-READ.
       3100-PROCESS-ONE-STUDENT.
      *    COMPUTE PAGE BREAK CHECK
           IF WS-LINES-USED >= WS-MAX-LINES
               PERFORM 3200-PRINT-HEADERS
           END-IF
           MOVE SPACES TO WS-FOUND-INST-NAME
           SET WS-INST-IDX TO 1
           PERFORM VARYING WS-INST-IDX FROM 1 BY 1
               UNTIL WS-INST-IDX > WS-INST-MAX
               IF WS-TAB-INST-NUM(WS-INST-IDX) = STU-INST-NUM
                   MOVE WS-TAB-INST-NAM(WS-INST-IDX)
                       TO WS-FOUND-INST-NAME
               END-IF
           END-PERFORM
      *    CLEAN UP STUDENT NAME
           INSPECT STU-NAME
               CONVERTING LOW-VALUES TO SPACES
           MOVE STU-NUM            TO DL-STU-NUM
           MOVE STU-NAME           TO DL-STU-NAME
           MOVE WS-FOUND-INST-NAME TO DL-INST-NAME
           MOVE STU-STATUS         TO DL-STATUS
           WRITE REPORT-LINE FROM DETAIL-LINE
               AFTER ADVANCING 1 LINE
           COMPUTE WS-LINES-USED  = WS-LINES-USED  + 1
           COMPUTE WS-TOTAL-STU   = WS-TOTAL-STU   + 1
           IF STU-STATUS = 'C'
               COMPUTE WS-ACTIVE-STU  = WS-ACTIVE-STU  + 1
           ELSE
               COMPUTE WS-PASSIVE-STU = WS-PASSIVE-STU + 1
           END-IF.
       3200-PRINT-HEADERS.
           COMPUTE WS-PAGE-NUM   = WS-PAGE-NUM + 1
           MOVE WS-RPT-DATE      TO HDR-DATE
           MOVE WS-PAGE-NUM      TO HDR-PAGE
           COMPUTE WS-LINES-USED = 0
           WRITE REPORT-LINE FROM HDR-LINE-1
               AFTER ADVANCING PAGE
           WRITE REPORT-LINE FROM HDR-LINE-2
               AFTER ADVANCING 1 LINE
           WRITE REPORT-LINE FROM HDR-LINE-3
               AFTER ADVANCING 1 LINE
           WRITE REPORT-LINE FROM HDR-LINE-4
               AFTER ADVANCING 1 LINE
           WRITE REPORT-LINE FROM HDR-LINE-5
               AFTER ADVANCING 1 LINE.
       4000-PRINT-TOTALS.
           MOVE WS-TOTAL-STU   TO TOT-STUDENTS
           MOVE WS-ACTIVE-STU  TO TOT-ACTIVE
           MOVE WS-PASSIVE-STU TO TOT-PASSIVE
           WRITE REPORT-LINE FROM HDR-LINE-2
               AFTER ADVANCING 2 LINES
           WRITE REPORT-LINE FROM TOTAL-LINE-1
               AFTER ADVANCING 1 LINE
           WRITE REPORT-LINE FROM TOTAL-LINE-2
               AFTER ADVANCING 1 LINE
           WRITE REPORT-LINE FROM TOTAL-LINE-3
               AFTER ADVANCING 1 LINE.
       5000-CLOSE.
           CLOSE STU-FILE
                 REPORT-FILE.
