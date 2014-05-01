C MODULE SPXGRD
C-----------------------------------------------------------------------
C
C  ROUTINE TO PRINT XGRD PARAMETERS.
C
      SUBROUTINE SPXGRD (IVXGRD,NXA,NSEGS,IY,IXB,IXE,LEVEL,STAT)
C
      DIMENSION NSEGS(NXA),IY(NXA),IXB(NXA),IXE(NXA)
C
      INCLUDE 'uiox'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/spxgrd.f,v $
     . $',                                                             '
     .$Id: spxgrd.f,v 1.5 2005/03/18 20:59:42 leecr Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SPXGRD'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('XGRD')
C
      ISTAT=0
C
C  CHECK NUMBER OF LINES LEFT ON PAGE
      IF (ISLEFT(10).GT.0) CALL SUPAGE
C
C  PRINT HEADING
      WRITE (LP,40)
      CALL SULINE (LP,2)
      WRITE (LP,50)
      CALL SULINE (LP,2)
      WRITE (LP,*) ' '
      CALL SULINE (LP,1)
C
C  PRINT PARAMETER ARRAY VERSION NUMBER
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*)
     *      ' IVXGRD=',IVXGRD,
     *      ' NXA=',NXA,
     *      ' '
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      WRITE (LP,70) NXA
      CALL SULINE (LP,2)
C
      IF (LEVEL.GT.1) THEN
C     PRINT BASIN SEGMENT INFORMATION
         IPOS=1
         DO 20 I=1,NXA
            IF (ISLEFT(5).GT.0) CALL SUPAGE
            WRITE (LP,80) I,NSEGS(I)
            CALL SULINE (LP,2)
            DO 10 N=1,NSEGS(I)
               WRITE (LP,90) N,IY(IPOS),IXB(IPOS),IXE(IPOS)
               CALL SULINE (LP,1)
               IPOS=IPOS+1
10             CONTINUE
20          CONTINUE
         ENDIF
C
      WRITE (LP,40)
      CALL SULINE (LP,2)
      WRITE (LP,*) ' '
      CALL SULINE (LP,1)
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SPXGRD'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
40    FORMAT ('0',132('-'))
50    FORMAT ('0*-->  XGRD PARAMETERS')
70    FORMAT ('0NUMBER OF MAPX AREAS = ',I5)
80    FORMAT ('0',T5,'MAPX AREA NUMBER ',I4,' HAS ',I3,
     *   ' BASIN LINE SEGMENTS:')
90    FORMAT (T5,I3,': ',
     *   'IY=',I4,3X,'IXB=',I4,3X,'IXE=',I4)
C
      END
