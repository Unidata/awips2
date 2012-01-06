C MODLUE CGDELB
C-----------------------------------------------------------------------
C
C  ROUTINE TO REMOVE CARRYOVER GROUP NAME FROM FORECAST GROUPS
C
      SUBROUTINE CGDELB (CGIDIN)
C
C  ROUTINE ORIGINALLY WRITTEN BY -- ED JOHNSON - HRL - 11/1979
C
      CHARACTER*8 OPNOLD
      DIMENSION CGIDIN(2)
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fcfgs'
      INCLUDE 'common/fcunit'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_top/RCS/cgdelb.f,v $
     . $',                                                             '
     .$Id: cgdelb.f,v 1.4 2000/03/14 11:56:14 page Exp $
     . $' /
C    ===================================================================
C
      DATA BLANK/4h    /
C
C
      IF (ITRACE.GT.0) WRITE (IODBUG,*) 'ENTER CGDELB'
C
      IBUG=IFBUG('DLTE')+IFBUG('CGDF')
C
      IOPNUM=0
      CALL FSTWHR ('CGDELB  ',IOPNUM,OPNOLD,IOLDOP)
C
      IF (IBUG.GT.0) WRITE (IODBUG,10) CGIDIN
10    FORMAT (' CGIDIN=',2A4)
C
      IF (NFGREC.EQ.0) THEN
         WRITE (IPR,20)
20    FORMAT ('0**WARNING** NO FORECAST GROUPS EXIST.')
         CALL WARN
         GO TO 50
         ENDIF
C
      DO 40 IFGREC=1,NFGREC
         CALL UREADT (KFFGST,IFGREC,FGID,IERR)
         IF (CGIDF(1).EQ.CGIDIN(1).AND.CGIDF(2).EQ.CGIDIN(2)) THEN
            IF (IBUG.GT.0) WRITE (IODBUG,30) FGID
30    FORMAT (' FGID=',2A4)
            CGIDF(1)=BLANK
            CGIDF(2)=BLANK
            ICOSEQ=0
            CALL UWRITT (KFFGST,IFGREC,FGID,IERR)
            ENDIF
40       CONTINUE
C
50    CALL FSTWHR (OPNOLD,IOLDOP,OPNOLD,IOLDOP)
C
      IF (ITRACE.GT.0) WRITE (IPR,*) 'EXIT CGDELB'
C
      RETURN
C
      END
