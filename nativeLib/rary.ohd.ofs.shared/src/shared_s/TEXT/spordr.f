C MODULE SPORDR
C-----------------------------------------------------------------------
C
C  ROUTINE TO PRINT GENERAL ORDER PARAMETERS
C
      SUBROUTINE SPORDR (IPRNT,IVORDR,ICOMPL,
     *   NORDMO,NORDDA,NORDYR,NORDHM,
     *   NAMAP,NAFMAP,NAMAPX,ICBASN,ICFMAP,
     *   UNUSED,ISTAT)
C
      CHARACTER*3 XPRINT
C
      DIMENSION UNUSED(2)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/spordr.f,v $
     . $',                                                             '
     .$Id: spordr.f,v 1.3 1999/07/07 11:17:07 page Exp $
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
      LDEBUG=ISBUG('ORDR')
C
      ISTAT=0
C
C  CHECK NUMBER OF LINES LEFT ON PAGE
      IF (ISLEFT(17).GT.0) CALL SUPAGE
C
      IF (IPRNT.EQ.0) GO TO 10
C
C  PRINT HEADING
      WRITE (LP,30)
      CALL SULINE (LP,2)
      WRITE (LP,40)
      CALL SULINE (LP,2)
      WRITE (LP,*) ' '
      CALL SULINE (LP,1)
C
10    IF (LDEBUG.GT.0) THEN
C     PRINT PARAMETER ARRAY VERSION NUMBER
         WRITE (LP,50) IVORDR
         CALL SULINE (LP,2)
C     PRINT COMPLETE INDICATOR
         IF (ICOMPL.EQ.0) THEN
            WRITE (LP,60) 'INCOMPLETE'
            CALL SULINE (LP,2)
            ENDIF
         IF (ICOMPL.EQ.1) THEN
            WRITE (LP,60) 'COMPLETE'
            CALL SULINE (LP,2)
            ENDIF
         ENDIF
C
C  PRINT DATE AND TIME OF LAST UPDATE
      WRITE (LP,70) NORDMO,NORDDA,NORDYR,NORDHM
      CALL SULINE (LP,2)
C
C  PRINT NUMBER OF NEW AREAS ADDED SINCE ORDER COMMAND LAST RUN
      WRITE (LP,80) 'MAP  AREAS ADDED',NAMAP
      CALL SULINE (LP,2)
      WRITE (LP,80) 'FMAP AREAS ADDED',NAFMAP
      CALL SULINE (LP,2)
      IF (IVORDR.GT.1) THEN
         CALL SULINE (LP,2)
         WRITE (LP,80) 'MAPX AREAS ADDED',NAMAPX
         ENDIF
C
C  PRINT INDICATOR IF BASINS USED BY MAPX AREAS HAVE BEEN 
C  CHANGED SINCE ORDER COMMAND LAST RUN
      IF (IVORDR.GT.2) THEN
         XPRINT='NO'
         IF (ICBASN.EQ.1) XPRINT='YES'
         CALL SULINE (LP,2)
         WRITE (LP,90) 'BASINS USED BY MAPX AREAS',XPRINT
         ENDIF
C
C  PRINT INDICATOR IF FMAP AREAS IDS USED BY MAP AREAS HAVE BEEN 
C  CHANGED SINCE ORDER COMMAND LAST RUN
      IF (IVORDR.GT.3) THEN
         XPRINT='NO'
         IF (ICFMAP.EQ.1) XPRINT='YES'
         CALL SULINE (LP,2)
         WRITE (LP,90) 'FMAP AREA IDENTIFIERS USED BY MAP AREAS',XPRINT
         ENDIF
C
      WRITE (LP,30)
      CALL SULINE (LP,2)
      WRITE (LP,*) ' '
      CALL SULINE (LP,1)
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,110)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
20    FORMAT (' *** ENTER SPORDR')
30    FORMAT ('0',132('-'))
40    FORMAT ('0*--> ORDR PARAMETERS ',
     *   '(GENERAL COMPUTATIONAL ORDER INFORMATION)')
50    FORMAT ('PARAMETER ARRAY VERSION NUMBER = ',I2)
60    FORMAT ('0COMPUTATIONAL ORDER PARAMETER STATUS INDICATOR = ',A)
70    FORMAT ('0DATE LAST UPDATED = ',I2.2,'/',I2.2,'/',I4.4,'.',I4.4)
80    FORMAT ('0NUMBER OF ',A,' ',
     *   'SINCE ORDER COMMAND LAST RUN = ',I4)
90    FORMAT ('0HAVE ANY ',A,' BEEN CHANGED ',
     *   'SINCE ORDER COMMAND LAST RUN? ',A)
110   FORMAT (' *** EXIT SPORDR')
C
      END
