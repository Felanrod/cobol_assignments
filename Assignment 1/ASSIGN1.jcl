//KC03I27A JOB 1,'Joel Murphy',NOTIFY=&SYSUID                           00010100
//**************************************************                    00011100
//* COMPILE COBOL PROGRAM                                               00012000
//**************************************************                    00021000
//STEP1 EXEC IGYWCLG                                                    00030000
//COBOL.SYSIN        DD DSN=KC03I27.COBOL.SOURCE(ASSIGN1),DISP=SHR      00040001
//GO.A1IN    DD DSN=KC03I27.A1IN,DISP=SHR                               00060001
//GO.A1OUT   DD DSN=KC03I27.A1OUT,UNIT=SYSDA,                           00080005
//           DISP=(NEW,CATLG,DELETE),SPACE=(TRK,(3,3)),                 00090006
//           DCB=(DSORG=PS,RECFM=FB,LRECL=67,BLKSIZE=0)                 00100004
//*GO.A1OUT  DD DSN=KC03I27.A1OUT,DISP=OLD                              00200000
