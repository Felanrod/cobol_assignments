      *NAME: JOEL MURPHY
      *DATE: JULY 21, 2013
      *PURPOSE: TO READ PAYROLL RECORDS AND PRODUCE A REPORT THAT
      *         DISPLAYS A BONUS FOR EMPLOYEES WHO HAVE BEEN
      *         WORKING THERE SINCE BEFORE 1994
      
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ASSIGN2.
      ************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IN-EMPLOYEE-FILE
               ASSIGN TO 'CH0901.DAT'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUT-REPORT-FILE
               ASSIGN TO 'ASSIGN2.RPT'
               ORGANIZATION IS LINE SEQUENTIAL.
      ******************************************************
       DATA DIVISION.
       FILE SECTION.
       FD  IN-EMPLOYEE-FILE.
       01  IN-EMPLOYEE-REC.
           05  IN-EMPLOYEE-NO          PIC X(5).
           05  IN-EMPLOYEE-NAME        PIC X(20).
           05  IN-TERR-NO              PIC XX.
           05  IN-OFFICE-NO            PIC XX.
           05  IN-ANNUAL-SALARY        PIC 9(6).
           05  IN-FIRST-SPACE          PIC X(29).
           05  IN-DATE-HIRED.
               10  IN-MONTH            PIC 99.
               10  IN-DAY              PIC 99.
               10  IN-YEAR             PIC 9(4).
           05  IN-SECOND-SPACE         PIC X(10).
       FD  OUT-REPORT-FILE.
       01  OUT-REPORT-REC              PIC X(80).
       WORKING-STORAGE SECTION.
       01  WS-WORK-AREAS.
           05  ARE-THERE-MORE-RECORDS  PIC X(3)     VALUE 'YES'.
               88  MORE-RECORDS                     VALUE 'YES'.
               88  NO-MORE-RECORDS                  VALUE 'NO '.
           05  FIRST-RECORD            PIC X(3)     VALUE 'YES'.
           05  WS-PAGE-CTR             PIC 99       VALUE ZEROS.
           05  WS-RECORD-CTR           PIC 9(3)     VALUE ZERO.
           05  WS-OFFICE-HOLD          PIC XX       VALUE ZEROS.
           05  WS-TERR-HOLD            PIC XX       VALUE ZEROS.
           05  WS-OUT-BONUS            PIC 9(5)V99  VALUE ZEROS.
           05  WS-T-DATE.
               10  WS-IN-YR            PIC 9(4).
               10  WS-IN-MO            PIC 9(2).
               10  WS-IN-DAY           PIC 9(2).
           05  WS-MAX-YEARS            PIC 9(4).
      *CONSTANTS
       01  WS-CONSTANTS.
           05  WS-BONUS-AMOUNT         PIC 9V9      VALUE 0.1. 
           05  WS-BONUS-YEAR           PIC 9(4)     VALUE 1994.
           05  WS-MAX-MONTHS           PIC 9(2)     VALUE 12.
           05  WS-JAN                  PIC 99       VALUE 01.
           05  WS-MAR                  PIC 99       VALUE 03.
           05  WS-APR                  PIC 99       VALUE 04.
           05  WS-MAY                  PIC 99       VALUE 05.
           05  WS-JUN                  PIC 99       VALUE 06.
           05  WS-JUL                  PIC 99       VALUE 07.
           05  WS-AUG                  PIC 99       VALUE 08.
           05  WS-SEP                  PIC 99       VALUE 09.
           05  WS-OCT                  PIC 99       VALUE 10.
           05  WS-NOV                  PIC 99       VALUE 11.
           05  WS-DEC                  PIC 99       VALUE 12.
           05  WS-MAX-FEB-DAYS         PIC 9(2)     VALUE 29.
           05  WS-MAX-30-DAYS          PIC 9(2)     VALUE 30.
           05  WS-MAX-31-DAYS          PIC 9(2)     VALUE 31.
           05  WS-MIN-YEARS            PIC 9(4)     VALUE 1900.
       01  BLANK-LINE                  PIC X(80)    VALUE SPACES.
       01  HL-HEADING1.
           05                          PIC X(40)    VALUE SPACES.
           05                          PIC X(13)
               VALUE 'BONUS  REPORT'.
           05                          PIC X(7)     VALUE SPACES.
           05                          PIC X(5)
               VALUE 'PAGE '. 
           05  HL-OUT-PAGE             PIC 99.
           05                          PIC X(3)     VALUE SPACES.
           05  HL-TODAYS-DATE.
               10  HL-OUT-MO           PIC 99.
               10                      PIC X        VALUE '/'.
               10  HL-OUT-DAY          PIC 99.
               10                      PIC X        VALUE '/'.
               10  HL-OUT-YR           PIC 9(4).
       01  HL-HEADING2.
           05                          PIC X(10)    VALUE SPACES.
           05                          PIC X(13)
               VALUE 'TERRITORY -- '.
           05  HL-OUT-TERR             PIC XX.
           05                          PIC X(55)    VALUE SPACES.
       01  HL-HEADING3.
           05                          PIC X(20)    VALUE SPACES.
           05                          PIC X(10)
               VALUE 'OFFICE -- '.
           05  HL-OUT-OFFICE           PIC XX.
           05                          PIC X(48)    VALUE SPACES.
       01  HL-HEADING4.
           05                          PIC X(10)    VALUE SPACES.
           05                          PIC X(13)
               VALUE 'EMPLOYEE NAME'.
           05                          PIC X(7)     VALUE SPACES.
           05                          PIC X(5)
               VALUE 'BONUS'.
           05                          PIC X(45)    VALUE SPACES.
       01  DL-NAME-BONUS-LINE.
           05                          PIC X(6)     VALUE SPACES.
           05  DL-OUT-EMPLOYEE-NAME    PIC X(20).
           05                          PIC X(4)     VALUE SPACES.
           05  DL-OUT-BONUS            PIC $ZZ,ZZZ.99
               BLANK WHEN ZERO.
           05                          PIC X(40)     VALUE SPACES.
      ******************************************************
       PROCEDURE DIVISION.
      *MAIN MODULE CONTROLS THE DIRECTION OF THE PROGRAM LOGIC
       100-MAIN-PARA.
           PERFORM 800-INITIALIZATION-RTN
           PERFORM 200-DATE-ACCEPT-RTN
      *PROCESS ALL THE RECORDS OF THE FILE
           PERFORM UNTIL NO-MORE-RECORDS
             READ IN-EMPLOYEE-FILE
             AT END
               MOVE 'NO ' TO ARE-THERE-MORE-RECORDS
             NOT AT END
      *IF THERE ARE MORE RECORDS THEN CHECK THE DATA IN THE
      *RECORD FOR ERRORS
               ADD 1 TO WS-RECORD-CTR
               PERFORM 400-CHECKS-PARA
             END-READ
           END-PERFORM
      *IF THERE ARE NO MORE RECORDS DO THE TERRITORY BREAK
      *CLOSE THE FILES
           PERFORM 600-TERR-BREAK
           PERFORM 900-END-OF-JOB-RTN
           STOP RUN.
      
      *GETS THE CURRENT DATE AND FORMATS IT AS REQUIRED
       200-DATE-ACCEPT-RTN.
           DISPLAY "PLEASE ENTER TODAY'S DATE (YYYYMMDD): "
           ACCEPT WS-T-DATE
           MOVE WS-IN-YR  TO HL-OUT-YR
           MOVE WS-IN-MO  TO HL-OUT-MO
           MOVE WS-IN-DAY TO HL-OUT-DAY.
      
      *PRINTS THE HEADINGS ON A NEW PAGE
       300-HEADING-RTN.
           ADD 1 TO WS-PAGE-CTR
           MOVE WS-PAGE-CTR   TO HL-OUT-PAGE
           MOVE WS-TERR-HOLD  TO HL-OUT-TERR
           MOVE WS-OFFICE-HOLD  TO HL-OUT-OFFICE
           WRITE OUT-REPORT-REC FROM BLANK-LINE
             AFTER ADVANCING PAGE
           WRITE OUT-REPORT-REC FROM HL-HEADING1
             AFTER ADVANCING 2 LINES
           WRITE OUT-REPORT-REC FROM HL-HEADING2
             AFTER ADVANCING 2 LINES
           WRITE OUT-REPORT-REC FROM HL-HEADING3
             AFTER ADVANCING 2 LINES
           WRITE OUT-REPORT-REC FROM HL-HEADING4
             AFTER ADVANCING 3 LINES.
             
      *CHECKING THE RECORDS TO MAKE SURE THEY ARE THE RIGHT TYPES
       400-CHECKS-PARA.
      *IS SALARY A POSITIVE NUMBER?
           IF                     (IN-ANNUAL-SALARY IS NUMERIC)
             IF                   (IN-ANNUAL-SALARY IS POSITIVE)
      *ARE THE YEAR, MONTH, AND DAY HIRED NUMBERS?
               IF                 (IN-YEAR          IS NUMERIC)
                 IF               (IN-MONTH         IS NUMERIC)
                   IF             (IN-DAY           IS NUMERIC)
      *IS DAY A POSITIVE NUMBER?
                     IF           (IN-DAY           IS POSITIVE)
      *IS THE YEAR BETWEEN 1900 AND THE DATE ENTERED BY USER?
                       IF         (IN-YEAR          >= WS-MIN-YEARS AND
                                   IN-YEAR          <= WS-IN-YR)
      *IS THE MONTH BETWEEN 1 AND 12?
                         IF       (IN-MONTH         IS POSITIVE AND
                                   IN-MONTH         <= WS-MAX-MONTHS)
      *IS THE MONTH ONE OF THE MONTHS WITH 31 DAYS?
                           IF     (IN-MONTH         =  WS-JAN OR
                                   IN-MONTH         =  WS-MAR OR
                                   IN-MONTH         =  WS-MAY OR
                                   IN-MONTH         =  WS-JUL OR
                                   IN-MONTH         =  WS-AUG OR
                                   IN-MONTH         =  WS-OCT OR
                                   IN-MONTH         =  WS-DEC)
      *IS THE DAY BETWEEN 1 AND 31? IF IT IS CALCULATE THE BONUSES
                             IF   (IN-DAY           <= WS-MAX-31-DAYS)
                               PERFORM 500-CALC-RTN
                             ELSE
                               DISPLAY 'THE MONTH CANNOT HAVE MORE THAN
      -' 31 DAYS FOR RECORD NUMBER, ', WS-RECORD-CTR
                               STOP RUN
                             END-IF
                           ELSE
      *IS THE MONTH ONE OF THE MONTHS WITH 30 DAYS?
                             IF   (IN-MONTH         =  WS-APR OR
                                   IN-MONTH         =  WS-JUN OR
                                   IN-MONTH         =  WS-SEP OR
                                   IN-MONTH         =  WS-NOV)
      *IS THE DAY BETWEEN 1 AND 30? IF IT IS CALCULATE THE BONUSES
                               IF (IN-DAY           <= WS-MAX-30-DAYS)
                                 PERFORM 500-CALC-RTN
                               ELSE
                                 DISPLAY 'THE MONTH CANNOT HAVE MORE',
      -' THAN 30 DAYS FOR RECORD NUMBER, ', WS-RECORD-CTR
                                 STOP RUN
                               END-IF
      *SINCE THE MONTH IS FEBRUARY IS THE DAY BETWEEN 1 AND 29?
      *IF IT IS CALCULATE THE BONUSES
                             ELSE
                               IF (IN-DAY           <= WS-MAX-FEB-DAYS)
                                 PERFORM 500-CALC-RTN
                               ELSE
                                 DISPLAY 'FEBRUARY CANNOT HAVE MORE',
      -' THAN 29 DAYS FOR RECORD NUMBER, ', WS-RECORD-CTR
                                 STOP RUN
                               END-IF
                             END-IF
                           END-IF
                         ELSE
                           DISPLAY 'MONTH NEEDS TO BE BETWEEN 1 AND ',
      -WS-MAX-MONTHS, ' FOR RECORD NUMBER, ', WS-RECORD-CTR
                           STOP RUN
                         END-IF
                       ELSE
                         DISPLAY 'YEAR NEEDS TO BE BETWEEN ',
      -WS-MIN-YEARS, ' AND ', WS-IN-YR, ' FOR RECORD NUMBER, ',
      -WS-RECORD-CTR
                         STOP RUN
                       END-IF
                     ELSE
                       DISPLAY 'DAY NEEDS TO BE A POSITIVE NUMBER FOR',
      -' RECORD NUMBER, ', WS-RECORD-CTR
                       STOP RUN
                     END-IF
                   ELSE
                     DISPLAY 'DAY NEEDS TO BE NUMERIC FOR RECORD',
      -' NUMBER, ', WS-RECORD-CTR
                     STOP RUN
                   END-IF
                 ELSE
                   DISPLAY 'MONTH NEEDS TO BE NUMERIC FOR RECORD',
      -' NUMBER, ', WS-RECORD-CTR
                   STOP RUN
                 END-IF
               ELSE
                 DISPLAY 'YEAR NEEDS TO BE NUMERIC FOR RECORD',
      -' NUMBER, ', WS-RECORD-CTR
                 STOP RUN
               END-IF
             ELSE
               DISPLAY 'SALARY MUST BE A POSITIVE NUMBER FOR RECORD',
      -' NUMBER, ', WS-RECORD-CTR
               STOP RUN
             END-IF
           ELSE
             DISPLAY 'THE SALARY NEEDS TO BE NUMERIC FOR RECORD',
      -' NUMBER, ', WS-RECORD-CTR
             STOP RUN
           END-IF.
      
      *INITIALIZES THE TERRITORY AND OFFICE NUMBERS WITH THE FIRST
      *RECORD'S. WRITES THE HEADINGS TO THE OUTPUT.       
       500-CALC-RTN.
           EVALUATE TRUE
             WHEN FIRST-RECORD = 'YES'
               MOVE IN-TERR-NO   TO WS-TERR-HOLD
               MOVE IN-OFFICE-NO TO WS-OFFICE-HOLD
               PERFORM 300-HEADING-RTN
               MOVE 'NO '        TO FIRST-RECORD
      *CHECKS TO SEE IF THE RECORD'S TERRITORY NUMBER IS EQUAL TO
      *THE TERRITORY NUMBER VARIABLE, IF IT'S NOT THEN PERFORM
      *THE TERRITORY BREAK
             WHEN IN-TERR-NO NOT = WS-TERR-HOLD
               PERFORM 600-TERR-BREAK
      *CHECKS TO SEE IF THE RECORD'S OFFICE NUMBER IS EQUAL TO
      *THE OFFICE NUMBER VARIABLE, IF IT'S NOT THEN PERFORM
      *THE OFFICE BREAK         
             WHEN IN-OFFICE-NO NOT = WS-OFFICE-HOLD
               PERFORM 700-OFFICE-BREAK
           END-EVALUATE
           MOVE IN-EMPLOYEE-NAME TO DL-OUT-EMPLOYEE-NAME
      *CHECK IF THE RECORD'S YEAR IS LESS THAN 1994
      *IF IT IS CALCULATE THE BONUS, IF IT ISN'T THEN THE BONUS IS 0
           IF IN-YEAR < WS-BONUS-YEAR
              COMPUTE DL-OUT-BONUS = IN-ANNUAL-SALARY * WS-BONUS-AMOUNT
              ON SIZE ERROR
                DISPLAY 'TOTAL BONUS FIELD SHORT AT RECORD '
                'NUMBER, ', WS-RECORD-CTR
                STOP RUN
              END-COMPUTE
           ELSE
              MOVE 0 TO DL-OUT-BONUS
           END-IF
           WRITE OUT-REPORT-REC    FROM DL-NAME-BONUS-LINE
             AFTER ADVANCING 2 LINES.
             
      *CONTROLS THE TERRITORY BREAK
      *EVERY TIME THE TERRITORY CHANGES THE OFFICE DOES TO
      *IF THERE ARE MORE RECORDS TO READ ASSIGN THE NEW TERRITORY
      *NUMBER TO THE TERRITORY NUMBER VARIABLE
       600-TERR-BREAK.
           PERFORM 700-OFFICE-BREAK
           IF MORE-RECORDS
             MOVE IN-TERR-NO     TO WS-TERR-HOLD
             PERFORM 300-HEADING-RTN
           END-IF.
             
      *CONTROLS THE OFFICE BREAK
      *IF THERE ARE MORE RECORDS TO READ ASSIGN THE NEW OFFICE
      *NUMBER TO THE OFFICE NUMBER VARIABLE
       700-OFFICE-BREAK.
           IF MORE-RECORDS
             MOVE IN-OFFICE-NO  TO WS-OFFICE-HOLD
             IF IN-TERR-NO = WS-TERR-HOLD
                PERFORM 300-HEADING-RTN
             END-IF
           END-IF.
           
      *OPENS THE FILES
       800-INITIALIZATION-RTN.
           OPEN     INPUT  IN-EMPLOYEE-FILE
                    OUTPUT OUT-REPORT-FILE.
       
      *CLOSES THE FILES
       900-END-OF-JOB-RTN.
           CLOSE     IN-EMPLOYEE-FILE
                     OUT-REPORT-FILE.