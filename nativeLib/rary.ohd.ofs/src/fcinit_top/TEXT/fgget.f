C MEMBER FGGET
C
C  DESC LOOK FOR AND LOAD FORECAST GROUP INTO CB /FCFGS/
C
C......................................................................
C
      SUBROUTINE FGGET(GETID,IFREC,IRCODE)
C
C.....................................................................
C
C  THIS SUBROUTINE LOOKS FOR A FORECAST GROUP ON THE FORECAST GROUP
C  STATUS FILE (UNIT# - KFFGST), LOADS THE DEFINITION INTO THE FORECAST
C  GROUP STATUS COMMON BLOCK /FCFGS/ IF THE DEFINITION IS FOUND, AND
C  PROVIDES A RETURN CODE TO INDICATE THE SUCCESS OF THE SEARCH AND
C  LOAD OPERATION.
C...................................................................
C
C  ARGUMENT LIST --
C
C        GETID (INPUT) - IDENTIFIER OF GROUP TO BE SOUGHT
C        IFREC (OUTPUT)- RECORD ON FILE WHERE DEFINITION IS FOUND, IF
C                        FOUND,OR
C                        NEXT AVAILABLE WRITE LOCATION IF DEFINITION IS
C                        NOT FOUND.
C        IRCODE(OUTPUT)- STATUS RETURN CODE: =0, DEFINITION NOT FOUND,
C                                            =1, DEFINITION FOUND.
C
C......................................................................
C
C  SUBROUTINE ORIGINALLY PROGRAMMED BY --
C                    JTOSTROWSKI -- HRL -- 820922
C
C.......................................................................
C
      INCLUDE 'common/where'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fcfgs'
      INCLUDE 'common/fcunit'
      INCLUDE 'common/ionum'
C
      DIMENSION SUBNAM(2),OLDNAM(2),GETID(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_top/RCS/fgget.f,v $
     . $',                                                             '
     .$Id: fgget.f,v 1.2 1999/01/19 21:37:15 page Exp $
     . $' /
C    ===================================================================
C
C
      DATA SUBNAM/4HFGGE,4HT   /
C  DEBUG CODE - FGRP
      DATA FGRP/4HFGRP/
C
C...........................
C  SET WHERE INFORMATION
C
      IOPNUM = 0
      OLDNAM(1) = OPNAME(1)
      OLDNAM(2) = OPNAME(2)
       OPNAME(1) = SUBNAM(1)
      OPNAME(2) = SUBNAM(2)
C
C  TRACE LEVEL = 2
C
       IF (ITRACE.GE.2) WRITE(IODBUG,1600)
 1600 FORMAT(1H0,19H*** ENTER FGGET ***)
C
C SET VALUE OF DEBUG
C
      IBUG = IFBUG(FGRP)
C
      IF (IBUG.GE.1) WRITE(IODBUG,1601) GETID,NFGREC
 1601 FORMAT(1H0,27H*** FGROUP TO BE SOUGHT -- ,2A4,04H ***,/
     .  ,5X,25HNO. OF RECORDS ON FILE = ,I2)
C
C.....................
C  SET RETURN CODE TO 'NOT FOUND' (I.E. = 0)
C
      IRCODE = 0
C
      IF (NFGREC.EQ.0) GO TO 888
      DO 100 I=1,NFGREC
      CALL UREADT(KFFGST,I,FGID,ISTAT)
      IF (FGID(1).NE.GETID(1).OR.FGID(2).NE.GETID(2)) GO TO 100
      IRCODE = 1
      IFREC = I
      GO TO 999
C
  100 CONTINUE
C
C............................
C  NO MATCH FOR GETID IF CONTROL REACHES HERE
C
  888 IFREC = NFGREC+1
C
  999 CONTINUE
      IF (IBUG.GE.1) WRITE(IODBUG,1602) IRCODE,IFREC,GETID,FGID
 1602 FORMAT(1H0,49H*** AFTER SCAN: IRCODE IFREC  GETID     FGID  ***,
     . /20X,I1,5X,I2,2X,2A4,1X,2A4)
      IF (ITRACE.GE.2) WRITE(IODBUG,1610)
 1610 FORMAT(1H0,21H  *** EXIT FGGET *** )
C
C..........................
C  RESET WHERE INFO
C
      OPNAME(1) = OLDNAM(1)
      OPNAME(2) = OLDNAM(2)
C
      RETURN
      END
