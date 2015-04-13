      *NAME: ANJU CHAWLA
      *DATE: AUGUST 2, 2013
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
       
            
                                                
                    
                