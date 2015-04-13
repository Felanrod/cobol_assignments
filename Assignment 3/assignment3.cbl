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
          
       WORKING-STORAGE SECTION.
       01  MORE-RECORDS       PIC X    VALUE 'Y'.   
           
      **********************************************************
       PROCEDURE DIVISION.
       100-MAIN-PARA.
           OPEN I-O   MASTER-FILE
                INPUT TRANSACTION-FILE
           PERFORM 200-READ-TRANS
           PERFORM 300-UPDATE-PARA UNTIL MORE-RECORDS = 'N'
           CLOSE MASTER-FILE
                 TRANSACTION-FILE
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
             
             EVALUATE TRUE
               WHEN M-ACCT-NO = T-ACCT-NO
                 PERFORM 320-UPDATE-OR-DELETE-PARA
               WHEN M-ACCT-NO > T-ACCT-NO
                 DISPLAY T-ACCT-NO, ' IS NOT ON THE MASTER FILE.'
             END-EVALUATE
             PERFORM 200-READ-TRANS.
      *--------------------------------------------------------------
      *THERE IS A CORRESPONDING TRANSACTION RECORD FOR THE OLD MASTER
      *RECORD. SO UPDATE THE AMOUNT.
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
                 MOVE NOT-ACTIVE TO M-ACTIVE
                 REWRITE MASTER-REC
             WHEN OTHER
               DISPLAY 'ERROR IN TRANSACTION CODE WITH ACCOUNT NUMBER ',
               T-ACCT-NO        
             
           END-EVALUATE
      *---------------------------------------------------------------
      *READ NEXT RECORD FROM MASTER FILE
       400-READ-MASTER.
           READ MASTER-FILE
             AT END
               MOVE HIGH-VALUES TO M-ACCT-NO
           END-READ.
      *--------------------------------------------------------------
            
                                                
                    
                