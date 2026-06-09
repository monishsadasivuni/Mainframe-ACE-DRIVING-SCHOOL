       IDENTIFICATION DIVISION.
       PROGRAM-ID. DB2ACELD.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INST-FILE ASSIGN TO INSTFILE
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS INS-NUM
               FILE STATUS IS WS-INST-STATUS.
           SELECT STU-FILE  ASSIGN TO STUFILE
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS STUD-NUM
               FILE STATUS IS WS-STU-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  INST-FILE.
       01  INST-RECORD.
           05 INS-NUM      PIC X(05).
           05 INS-NAME     PIC X(20).
           05 INS-SALARY   PIC X(05).
           05 INS-STATUS   PIC X(01).
           05 FILLER        PIC X(49).
       FD  STU-FILE.
       01  STU-RECORD.
          05 STUD-NUM        PIC X(05).
          05 STUD-NAME       PIC X(20).
          05 STUD-ADDRESS    PIC X(20).
          05 STUD-START-DATE PIC X(10).
          05 STUD-INST-NUM   PIC X(5).
          05 STUD-STATUS     PIC X(1).
          05 STUD-PF-DATE    PIC X(10).
          05 FILLER         PIC X(09).
       WORKING-STORAGE SECTION.
            EXEC SQL INCLUDE SQLCA          END-EXEC.
            EXEC SQL INCLUDE INSTTB         END-EXEC.
            EXEC SQL INCLUDE STUDTB         END-EXEC.
       01  WS-INST-STATUS      PIC X(02) VALUE SPACES.
       01  WS-STU-STATUS       PIC X(02) VALUE SPACES.
       01  WS-COUNTERS.
           05  WS-INST-READ    PIC S9(07) COMP-3 VALUE ZEROS.
           05  WS-STU-READ     PIC S9(07) COMP-3 VALUE ZEROS.
           05  WS-INST-INS     PIC S9(07) COMP-3 VALUE ZEROS.
           05  WS-STU-INS      PIC S9(07) COMP-3 VALUE ZEROS.
       01  WS-EOF-INST         PIC X(01) VALUE 'N'.
       01  WS-EOF-STU          PIC X(01) VALUE 'N'.
       PROCEDURE DIVISION.
       0000-MAIN.
            PERFORM 1000-INIT
            PERFORM 2000-LOAD-INSTRUCTOR-TABLE
            PERFORM 3000-LOAD-STUDENT-TABLE
            PERFORM 4000-COMMIT-AND-CLOSE
            STOP RUN.
       1000-INIT.
            OPEN INPUT INST-FILE
            IF WS-INST-STATUS NOT = '00'
               DISPLAY 'ERROR OPEN INST FILE:' WS-INST-STATUS
               MOVE 16 TO RETURN-CODE
               STOP RUN
            END-IF
            OPEN INPUT STU-FILE.
            IF WS-INST-STATUS NOT = '00'
               DISPLAY 'ERROR OPEN STU FILE:' WS-STU-STATUS
               MOVE 16 TO RETURN-CODE
               STOP RUN
            END-IF.
       2000-LOAD-INSTRUCTOR-TABLE.
            READ INST-FILE
                AT END MOVE 'Y' TO WS-EOF-INST
            END-READ
            PERFORM UNTIL WS-EOF-INST = 'Y'
                ADD 1 TO WS-INST-READ
                PERFORM 2100-INSERT-INSTRUCTOR
                READ INST-FILE
                    AT END MOVE 'Y' TO WS-EOF-INST
                END-READ
            END-PERFORM.
       2100-INSERT-INSTRUCTOR.
            MOVE INS-NUM    OF INST-RECORD TO INST-NUM    OF DCLINSTTB
            MOVE INS-NAME   OF INST-RECORD TO INST-NAME   OF DCLINSTTB
            MOVE INS-SALARY OF INST-RECORD TO INST-SALARY OF DCLINSTTB
            MOVE INS-STATUS OF INST-RECORD TO INST-STATUS OF DCLINSTTB
            EXEC SQL
             INSERT INTO INSTTB
             (INST_NUM,
              INST_NAME,
              INST_SALARY,
              INST_STATUS)
             VALUES
             (:INST-NUM,
              :INST-NAME,
              :INST-SALARY,
              :INST-STATUS)
             END-EXEC.
             IF SQLCODE = 0
                 ADD 1 TO WS-INST-INS
             ELSE
                 DISPLAY 'INST INSERT ERROR SQLCODE=' SQLCODE
                 IF SQLCODE < 0
                     DISPLAY 'FATAL DB2 ERROR - ABENDING'
                     MOVE 16 TO RETURN-CODE
                     STOP RUN
                 END-IF
             END-IF.
       3000-LOAD-STUDENT-TABLE.
             READ STU-FILE
                 AT END MOVE 'Y' TO WS-EOF-STU
             END-READ
             PERFORM UNTIL WS-EOF-STU = 'Y'
                 ADD 1 TO WS-STU-READ
                 PERFORM 3100-INSERT-STUDENT
                 READ STU-FILE
                     AT END MOVE 'Y' TO WS-EOF-STU
                 END-READ
             END-PERFORM.
       3100-INSERT-STUDENT.
            MOVE STUD-NUM  OF STU-RECORD TO STU-NUM   OF DCLSTUDTB
            MOVE STUD-NAME OF STU-RECORD TO STU-NAME  OF DCLSTUDTB
            MOVE STUD-ADDRESS OF STU-RECORD TO STU-ADDRESS OF DCLSTUDTB
            MOVE STUD-START-DATE OF STU-RECORD TO STU-START-DATE
                OF DCLSTUDTB
            MOVE STUD-INST-NUM OF STU-RECORD TO STU-INST-NUM
                OF DCLSTUDTB
            MOVE STUD-STATUS OF STU-RECORD TO STU-STATUS OF DCLSTUDTB
            MOVE STUD-PF-DATE OF STU-RECORD TO STU-PF-DATE  OF DCLSTUDTB
            EXEC SQL
                     INSERT INTO STUDTB
                     (STU_NUM,
                      STU_NAME,
                      STU_ADDRESS,
                      STU_START_DATE,
                      STU_INST_NUM,
                      STU_STATUS,
                      STU_PF_DATE)
                     VALUES
                     (:STU-NUM,
                      :STU-NAME,
                      :STU-ADDRESS,
                      :STU-START-DATE,
                      :STU-INST-NUM,
                      :STU-STATUS,
                      :STU-PF-DATE)
            END-EXEC
            IF SQLCODE = 0
                ADD 1 TO WS-STU-INS
            ELSE
                DISPLAY 'STU INSERT ERROR SQLCODE=' SQLCODE
                IF SQLCODE < 0
                    DISPLAY 'FATAL DB2 ERROR - ABENDING'
                    MOVE 16 TO RETURN-CODE
                    STOP RUN
                END-IF
            END-IF.
       4000-COMMIT-AND-CLOSE.
            EXEC SQL COMMIT END-EXEC
            DISPLAY '========================================'
            DISPLAY 'ACE DB2 LOAD PROGRAM - SYSOUT TOTALS   '
            DISPLAY '========================================'
            DISPLAY 'INSTRUCTOR RECORDS READ  : ' WS-INST-READ
            DISPLAY 'INSTRUCTOR ROWS INSERTED : ' WS-INST-INS
            DISPLAY 'STUDENT RECORDS READ     : ' WS-STU-READ
            DISPLAY 'STUDENT ROWS INSERTED    : ' WS-STU-INS
            DISPLAY '========================================'
            CLOSE INST-FILE
                  STU-FILE.
