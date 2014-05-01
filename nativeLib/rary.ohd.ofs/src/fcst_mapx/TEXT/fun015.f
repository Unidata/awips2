C MODULE FUN015
C-----------------------------------------------------------------------
C
C  MAIN ROUTINE FOR THE MAPX PREPROCESSOR FUNCTION.
C
C  CONVERTS GRIDDED RADAR ESTIMATES OF PRECIPITATION FROM STAGE 3 TO
C  MAPX TIME SERIES.
C
C--------------------------------------------------------
C     ORIGINALLY CODED NOVEMBER 1990, R SHEDD, HRL
C--------------------------------------------------------
C
      SUBROUTINE FUN015
C
      CHARACTER*8 OLDOPN
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/pudbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_mapx/RCS/fun015.f,v $
     . $',                                                             '
     .$Id: fun015.f,v 1.3 1999/07/06 15:50:15 page Exp $
     . $' /
C    ===================================================================
C
C
      IOPNUM=-1
      CALL FSTWHR ('FUN015  ',IOPNUM,OLDOPN,IOLDOP)
C
      IF (IPTRCE .GE. 1) WRITE (IOPDBG,10)
10    FORMAT (' ENTER FUN015')
C
C  GET RUN OPTIONS FROM HCL
      CALL NRDHCL
C
C  READ PREPROCESSOR PARAMETRIC DATA BASE CONTROL INFORMATION
      CALL RPPPCO (ISTAT)
      IF (ISTAT.NE.0) THEN
         WRITE (IPR,20) 'RPPPCO',ISTAT
20    FORMAT ('0**ERROR** PREPROCESSOR PARAMETRIC DATA BASE ',
     *   'CONTROL INFORMATION NOT SUCCESSFULLY READ. ',
     *   A,' STATUS CODE=',I3)
         CALL ERROR
         GO TO 30
         ENDIF
C
C  PERFORM MAPX COMPUTATIONS
      CALL NMAPX
C
30    CALL STOPFN ('MAPX    ')
C
      IF (IPTRCE.GE.1) WRITE (IOPDBG,40)
40    FORMAT (' EXIT FUN015')
C
      CALL FSTWHR (OLDOPN,IOLDOP,OLDOPN,IOLDOP)
C
      RETURN
C
      END
