C MEMBER SPMAPX
C-----------------------------------------------------------------------
C
C @PROCESS LVL(77)
C
C  PRINT AREA MAPX PARAMETER RECORD.
C
      SUBROUTINE SPMAPX (IPRNT,IVMAPX,XMAPID,DESCRP,ITIME,
     *   BASNID,FMAPID,UNUSED,ISTAT,NUMB)                   
C
      DIMENSION XMAPID(2),DESCRP(5),FMAPID(2)

C LC changed definition basnid for multiple basins
      CHARACTER *4 BASNID(*)
      CHARACTER *18 HEADING
      CHARACTER *17 FMT
      DIMENSION UNUSED(1)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_print/RCS/spmapx.f,v $
     . $',                                                             '
     .$Id: spmapx.f,v 1.3 2002/10/10 15:57:35 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,10)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('MAPX')
C
      ISTAT=0
      NUMERR=0
C
C  CHECK NUMBER OF LINES LEFT ON PAGE
      IF (ISLEFT(5).GT.0) CALL SUPAGE
C
C  PRINT HEADING
      IF (IPRNT.EQ.2) THEN
         WRITE (LP,30) DESCRP
         CALL SULINE (LP,2)
         ELSE
            WRITE (LP,20) XMAPID
            CALL SULINE (LP,2)
         ENDIF
      WRITE (LP,50)
      CALL SULINE (LP,2)
C
C  PRINT PARAMETER ARRAY VERSION NUMBER
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,60) IVMAPX
         CALL SULINE (IOSDBG,2)
         ENDIF
C
C  PRINT MAPX AREA IDENTIFIER AND DESCRIPTIVE INFORMATION
      WRITE (LP,70) XMAPID,DESCRP
      CALL SULINE (LP,2)
C
C  PRINT BASIN IDENTIFIER AND FUTURE MAP IDENTIFIER
C
      IF(IVMAPX.EQ.1)THEN
         WRITE(LP,*)'0NUMBER OF BASIN(S)= 1'
         WRITE(LP,*)'0BASIN IDENTIFIER=',(BASNID(II),II=1,2)
      ELSE
C ***********changes are made here. kwz 10-01-02
         WRITE(LP,*)'0NUMBER OF BASIN(S)=',NUMB
         HEADING='0BASIN IDENTIFIER='
         FMT='(A18,200(1x,2A4))'
         WRITE(FMT(6:7),'(I2)',IOSTAT=IERR)NUMB
         WRITE (LP,FMT)HEADING,(BASNID(II),II=1,2*NUMB) 
      ENDIF
      WRITE (LP,85) FMAPID                     !cfan
      CALL SULINE (LP,2)
C
C  PRINT TIME INTERVAL
      IF (LDEBUG.GT.0) THEN
         WRITE (LP,90) ITIME
         CALL SULINE (LP,2)
         ENDIF
C
      WRITE (LP,40)
      CALL SULINE (LP,2)
C
      IF (LDEBUG.GT.0.AND.ISTAT.EQ.0) THEN
         WRITE (IOSDBG,100)
         CALL SULINE (IOSDBG,1)
         ENDIF
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
10    FORMAT (' *** ENTER SPMAPX')
20    FORMAT ('0',33('-'),' ID=',2A4,2X,85('-'))
30    FORMAT ('0',26('-'),' DESC=',5A4,2X,78('-'))
40    FORMAT ('0',132('-'))
50    FORMAT ('0*-->  MAPX AREA PARAMETERS')
60    FORMAT ('0PARAMETER ARRAY VERSION NUMBER = ',I2)
70    FORMAT ('0MAPX AREA IDENTIFIER = ',2A4,5X,'DESCRIPTION = ',5A4)
80    FORMAT ('0BASIN IDENTIFIER = ',2A4,5X,
     *   'FUTURE MAP IDENTIFIER = ',2A4)
85    FORMAT ('0FUTURE MAP IDENTIFIER = ',2A4)        !cfan
90    FORMAT ('0TIME STEP = ',I2.2,' HOURS')
100   FORMAT (' *** NOTE - AREA MAPX PARAMETERS SUCCESSFULLY ',
     *     'PRINTED.')
110   FORMAT (' *** EXIT SPMAPX')
C
      END
