      *NAME: JOEL MURPHY
      *DATE: AUGUST 9, 2013
      *PURPOSE: TO UPDATE OR DELETE A MASTER FILE RECORD IN PLACE USING
      *A TRANSACTION FILE.
      *THERE CAN BE NONE, ONE OR MULTIPLE TRANSACTION RECORD(IF ANY) 
      *CORRESPONDING TO A MASTER RECORD.
      
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ASSIGN2.
      ***********************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE
             ASSIGN TO 'MASTER.DAT'
             ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TRANSACTION-FILE
             ASSIGN TO 'TRANS.DAT'
             ORGANIZATION IS LINE SEQUENTIAL.
           SELECT NEW-MASTER-FILE
             ASSIGN TO 'NMASTR.DAT'
             ORGANIZATION IS LINE SEQUENTIAL.
          
      ***********************************************************
       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE
           RECORD CONTAINS 13 CHARACTERS.
       01  MASTER-REC.
           05 M-ACCT-NO       PIC X(5).
           05 M-AMOUNT        PIC 9(5)V99.  
           05 M-ACTIVE        PIC X.
              88 ACTIVE               VALUE 'Y'.
              88 NOT-ACTIVE           VALUE 'N'.  
           
       FD  TRANSACTION-FILE
           RECORD CONTAINS 13 CHARACTERS.
       01  TRANS-REC.
           05 T-ACCT-NO       PIC X(5).
           05 T-AMOUNT        PIC 9(5)V99.
           05 T-CODE          PIC X.
              88 UPDATE-R              VALUE 'U'.
              88 DELETE-R              VALUE 'D'.
       
       FD  NEW-MASTER-FILE
           RECORD CONTAINS 13 CHARACTERS. 
       01  NEW-MASTER-REC.
           05 N-ACCT-NO       PIC X(5).
           05 N-AMOUNT        PIC 9(5)V99.  
           05 N-ACTIVE        PIC X.
          
       WORKING-STORAGE SECTION.
       01  MORE-RECORDS       PIC X    VALUE 'Y'.   
           
      **********************************************************
       PROCEDURE DIVISION.
      *MAIN PARAGRAPH
      *OPEN FILES, THEN READ TRANSACTION FILE, THEN PERFORM
      *UPDATE-PARA UNTIL THE TRANSACTION FILE REACHES THE END
      *THEN RESET THE MASTER FILE, THEN SETUP THE NEW FILE,
      *THEN COMPARE THE MASTER FILE RECORDS AND WRITE THE ONES
      *THAT ARE ACTIVATED, THEN CLOSE ALL THE FILES.
       100-MAIN-PARA.
           OPEN I-O   MASTER-FILE
                INPUT TRANSACTION-FILE
           PERFORM 200-READ-TRANS
           PERFORM 300-UPDATE-PARA UNTIL MORE-RECORDS = 'N'
           PERFORM 330-CLOSE-OPEN-MASTER
           PERFORM 500-NEW-FILE-SETUP
           PERFORM 600-COMPARE-PARA UNTIL M-ACCT-NO = HIGH-VALUES
           PERFORM 700-CLOSE-ALL
           STOP RUN.
      *----------------------------------------------------------------
      *READ NEXT RECORD FROM TRANSACTION FILE
       200-READ-TRANS.
           READ TRANSACTION-FILE
             AT END
               MOVE 'N' TO MORE-RECORDS
           END-READ.
      *---------------------------------------------------------------
      *UPDATE MASTER RECORD IN PLACE IF THERE IS A CORRESPONDING TRANS
      *RECORD
       300-UPDATE-PARA.
           PERFORM 400-READ-MASTER UNTIL
             M-ACCT-NO = T-ACCT-NO
             OR
             M-ACCT-NO > T-ACCT-NO
             OR
             M-ACCT-NO = HIGH-VALUES
             
      *WHEN THE MASTER RECORD ACCOUNT MATCHES THE TRANS RECORD ACCOUNT
      *NUMBER PERFORM THE UPDATE OR DELETE CHECK.
      *WHEN THE TWO FILES RECORDS DON'T HAVE MATCHING ACCOUNT NUMBERS
      *AND THE TRANS RECORD IS NOT BLANK SPACE SHOW AN ERROR MESSAGE
      *SAYING THE ACCOUNT NUMBER ISN'T ON THE MASTER RECORD
             EVALUATE TRUE
               WHEN M-ACCT-NO = T-ACCT-NO
                 PERFORM 320-UPDATE-OR-DELETE-PARA
               WHEN M-ACCT-NO > T-ACCT-NO AND
                    T-ACCT-NO NOT = SPACES
                 DISPLAY T-ACCT-NO, ' IS NOT ON THE MASTER FILE.'
             END-EVALUATE
      *RESET THE MASTER RECORD SO AS NOT TO GET ANY ERRORS WHEN I
      *REWRITE TO IT, READ THE TRANS RECORD, AND READ THE MASTER
      *RECORD
             PERFORM 330-CLOSE-OPEN-MASTER
             PERFORM 200-READ-TRANS
             PERFORM 400-READ-MASTER.
      *--------------------------------------------------------------
      *THERE IS A CORRESPONDING TRANSACTION RECORD FOR THE MASTER
      *RECORD. SO UPDATE THE AMOUNT IF THE T-CODE IS U, CHANGE THE
      *CORRESPONDING MASTER RECORD TO NOT-ACTIVE IF THE T-CODE IS D,
      *OR SHOW AN ERROR MESSAGE IF IT ISN'T ONE OF THE TWO.
       320-UPDATE-OR-DELETE-PARA.
           EVALUATE TRUE
             WHEN UPDATE-R
               ADD T-AMOUNT TO M-AMOUNT
                 ON SIZE ERROR
                   DISPLAY 'ERROR, MASTER AMOUNT FIELD IS SHORT FOR ',
                     'ACCOUNT NUMBER ', T-ACCT-NO
                 NOT ON SIZE ERROR
                   REWRITE MASTER-REC
               END-ADD      
             WHEN DELETE-R
                 MOVE 'N' TO M-ACTIVE
                 REWRITE MASTER-REC
      
             WHEN OTHER
               DISPLAY 'ERROR IN TRANSACTION CODE WITH ACCOUNT NUMBER ',
               T-ACCT-NO
           END-EVALUATE.
      *---------------------------------------------------------------
      *READ NEXT RECORD FROM MASTER FILE
       400-READ-MASTER.
           READ MASTER-FILE
             AT END
               MOVE HIGH-VALUES TO M-ACCT-NO
           END-READ.
      *--------------------------------------------------------------
      *CLOSES AND OPENS THE MASTER FILE
       330-CLOSE-OPEN-MASTER.
           CLOSE MASTER-FILE
           OPEN  I-O   MASTER-FILE.
      *--------------------------------------------------------------
      *MAKES M-ACCT-NO = TO LOW-VALUES
      *AND OPENS THE NEW-MASTER-FILE
       500-NEW-FILE-SETUP.
           MOVE LOW-VALUES TO M-ACCT-NO
           OPEN OUTPUT NEW-MASTER-FILE.
      *--------------------------------------------------------------
      *READS THE MASTER-FILE RECORD AND IF IT IS ACTIVE WRITES IT TO
      *THE NEW FILE
       600-COMPARE-PARA.
           PERFORM 400-READ-MASTER
           EVALUATE TRUE
             WHEN ACTIVE
               WRITE NEW-MASTER-REC FROM MASTER-REC
           END-EVALUATE.
      *----------------------------------------------------------------
      *CLOSE ALL THE FILES
       700-CLOSE-ALL.
           CLOSE MASTER-FILE
                 TRANSACTION-FILE
                 NEW-MASTER-FILE.
      *****************************************************************