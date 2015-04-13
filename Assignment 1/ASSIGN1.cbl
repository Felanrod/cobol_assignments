      *NAME: JOEL MURPHY
      *NUMBER: 200203095
      *MAINFRAME USER ID: KC03I27
      *DATE: JUNE 5, 2013
      *PURPOSE: TO CPLLECT A LIST OF EMPLOYEE INFORMATION
      *         RECORDS FROM A DAT FILE AND OUTPUT A 
      *         FORMATTED USER FRIENDLY DAT FILE OF THE INFO.
      
       IDENTIFICATION DIVISION.
       PROGRAM-ID. A1.
      *ALL OTHER FOLLOWING PARAGRAPHS ARE OPTIONAL
       AUTHOR. JOEL MURPHY.
       INSTALLATION. PC.
       DATE-WRITTEN. JUNE 5, 2013.
       DATE-COMPILED. 
       SECURITY. CAN BE USED BY AUTHORISED PERSONNEL ONLY. 
       
       ENVIRONMENT DIVISION.
      *OPTIONAL SECTION
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PC.
       OBJECT-COMPUTER. PC.
       
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-DATA
              ASSIGN TO A1IN
              ORGANIZATION IS SEQUENTIAL.
           SELECT OUTPUT-DATA
              ASSIGN TO A1OUT
              ORGANIZATION IS SEQUENTIAL.
              
                  
       DATA DIVISION.
       FILE SECTION.
       FD  EMPLOYEE-DATA
           RECORD CONTAINS 80 CHARACTERS
           RECORDING MODE IS F.
       01  EMP-IN.
           05  EMPLOYEE-NO-IN            PIC X(5).
           05  EMPLOYEE-NAME-IN          PIC X(20).
           05  LOCATION-CODE.
               10 TERRITORY-NO-IN        PIC X(2).
               10 OFFICE-NO-IN           PIC X(2).
           05  ANNUAL-SALARY-IN          PIC X(6).
           05  SOC-SEC-NO-IN             PIC X(9).
           05  NO-OF-DEPENDS-IN          PIC X(2).
           05  JOB-CLASS-CODE-IN         PIC X(2).
           05  UNUSED-IN                 PIC X(32).
       
       
       FD  OUTPUT-DATA
           RECORD CONTAINS 67 CHARACTERS
           RECORDING MODE IS F.
       01  EMPLOYEE-RECORD.
           05                             PIC X(5).
           05 EMPLOYEE-NO-OUT             PIC X(5).
           05                             PIC X(2).
           05 EMPLOYEE-NAME-OUT           PIC X(20).
           05                             PIC X(1).
           05 TERRITORY-NO-OUT            PIC X(2).
           05                             PIC X(3).
           05 OFFICE-NO-OUT               PIC X(2).
           05                             PIC X(2).
           05 ANNUAL-SALARY-OUT           PIC X(6).
           05                             PIC X(2).
           05 SOC-SEC-NO-OUT              PIC X(9).
           05                             PIC X(2).
           05 NO-OF-DEPENDS-OUT           PIC X(2).
           05                             PIC X(2).
           05 JOB-CLASS-CODE-OUT          PIC X(2).
           
       
       
       WORKING-STORAGE SECTION.
       01  ARE-THERE-MORE-RECORDS         PIC X(3) VALUE "YES".
       
       PROCEDURE DIVISION.
       100-MAIN-MODULE.
           OPEN        INPUT   EMPLOYEE-DATA
                       OUTPUT  OUTPUT-DATA
           
           PERFORM UNTIL ARE-THERE-MORE-RECORDS = "NO "
               READ EMPLOYEE-DATA
                 AT END
                    MOVE "NO " TO ARE-THERE-MORE-RECORDS
                 NOT AT END
                    PERFORM 200-TRANSFER-ROUTINE
               END-READ
           END-PERFORM
           
           CLOSE       EMPLOYEE-DATA
                       OUTPUT-DATA
                       
           STOP RUN.
           
       200-TRANSFER-ROUTINE.
           MOVE SPACES            TO EMPLOYEE-RECORD
           MOVE EMPLOYEE-NO-IN    TO EMPLOYEE-NO-OUT
           MOVE EMPLOYEE-NAME-IN  TO EMPLOYEE-NAME-OUT
           MOVE TERRITORY-NO-IN   TO TERRITORY-NO-OUT
           MOVE OFFICE-NO-IN      TO OFFICE-NO-OUT
           MOVE ANNUAL-SALARY-IN  TO ANNUAL-SALARY-OUT
           MOVE SOC-SEC-NO-IN     TO SOC-SEC-NO-OUT
           MOVE NO-OF-DEPENDS-IN  TO NO-OF-DEPENDS-OUT
           MOVE JOB-CLASS-CODE-IN TO JOB-CLASS-CODE-OUT
           WRITE EMPLOYEE-RECORD.
           
                           
           
                        
       
                
                
                
                
                
                
                
                
                
                
       
       