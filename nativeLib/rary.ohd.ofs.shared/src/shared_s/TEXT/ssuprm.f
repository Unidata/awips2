C MEMBER SSUPRM
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 05/09/95.12:05:35 BY $WC20SV
C
C @PROCESS LVL(77)
C
C DESC: ROUTINE TO PRINT STATUS OF USER PARAMETER FILE
C
      SUBROUTINE SSUPRM (ISTAT)
C
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/suoptx'
      INCLUDE 'hclcommon/hdflts'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/ssuprm.f,v $
     . $',                                                             '
     .$Id: ssuprm.f,v 1.1 1995/09/17 19:21:37 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,20)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('STAT')
C
      ISTAT=0
C
C  PRINT HEADER
      IF (ISLEFT(10).GT.0) CALL SUPAGE
      WRITE (LP,30)
      CALL SULINE (LP,2)
      WRITE (LP,40)
      IF (IOPOVP.EQ.1) THEN
         WRITE (LP,40)
         WRITE (LP,40)
         ENDIF
      CALL SULINE (LP,0)
C
C  OPEN DATA BASE
      CALL SUDOPN (1,'UPRM',IERR)
      IF (IERR.GT.0) GO TO 10
C
C  PRINT FILE INFORMATION
      WRITE (LP,50) HNAMRF
      CALL SULINE (LP,2)
      WRITE (LP,60) TIME(2),TIME(3)
      CALL SULINE (LP,4)
      IF (LDEBUG.GT.0) THEN
         WRITE (LP,70) METRIC
         CALL SULINE (LP,2)
         WRITE (LP,110) TDATES
         CALL SULINE (LP,2)
         ENDIF
      WRITE (LP,120) LOCAL
      CALL SULINE (LP,2)
      WRITE (LP,130) NLSTZ
      CALL SULINE (LP,2)
      WRITE (LP,140) 'PREPROCESSOR',NHOPDB
      CALL SULINE (LP,2)
      WRITE (LP,140) 'CALIBRATION',NHOCAL
      CALL SULINE (LP,2)
      WRITE (LP,80) CLKZON
      CALL SULINE (LP,2)
      WRITE (LP,90) ZOFF
      CALL SULINE (LP,2)
      WRITE (LP,100) INTDFL
      CALL SULINE (LP,2)
C
10    IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,150)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
20    FORMAT (' *** ENTER SSUPRM')
30    FORMAT ('0')
40    FORMAT ('+*--> USER PARAMETER FILE STATUS')
50    FORMAT ('0USER NAME = ',2A4)
60    FORMAT ('0DIFFERENCE BETWEEN Z-TIME AND USER DEFAULT TIME = ',I2 /
     *   '0DEFAULT TIME ZONE CODE = ',A4)
70    FORMAT ('0ENGLISH/METRIC FLAG = ',I2)
80    FORMAT ('0TIME ZONE CODE OF MACHINE CLOCK = ',A4)
90    FORMAT ('0OFFSET FROM 0Z WHEN THE DATE REFERENCED BY THE ''*'' ',
     *   'HCL DATE CODE TO BE ADVANCED = ',I4)
100   FORMAT ('0DEFAULT INTERVAL FOR USE WITH THE ''#'' ',
     *   'HCL DATE CODE = ',I2)
110   FORMAT ('0TDATES = ',7(I2,1X))
120   FORMAT ('0HOURS OFFSET TO LOCAL TIME = ',I2)
130   FORMAT ('0LOCAL STANDARD TIME CODE = ',I2)
140   FORMAT ('0NUMBER OF HOURS INTERNAL TIME IS OFFSET FROM THE ',A,
     *   ' DATA BASE = ',I2)
150   FORMAT (' *** EXIT SSUPRM')
C
      END
