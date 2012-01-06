C MODULE HWROPT
C-----------------------------------------------------------------------
C
      SUBROUTINE HWROPT (NXO,NOPTRC,ISTAT2)
C
C  THIS ROUTINE WRITES THE OPTION RECORD TO THE RUN-TIME OPTION FILE.
C
      CHARACTER*8 RTNNAM,RTNOLD
C
      DIMENSION NOPTRC(*)
C
      INCLUDE 'uiox'
      INCLUDE 'udebug'
      INCLUDE 'hclcommon/hunits'
      INCLUDE 'hclcommon/hseg1'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hwropt.f,v $
     . $',                                                             '
     .$Id: hwropt.f,v 1.2 2001/06/13 13:43:44 dws Exp $
     . $' /
C    ===================================================================
C
C
      RTNNAM='HWROPT'
C
      IF (IHCLTR.GT.1) WRITE (LP,*) 'ENTER ',RTNNAM
C
      IOPNUM=-1
      CALL FSTWHR (RTNNAM,IOPNUM,RTNOLD,IOLDOP)
C
C  COMPUTE NUMBER OF RECORDS
      N=(NXO+LRECOP-1)/LRECOP
      NOPTRC(1)=N
C
C  WRITE TO FILE
      J=1
      NREC=MAXOPT
      DO 10 I=1,N
         NREC=NREC+1
         CALL UWRITT (KHDFLT,NREC,NOPTRC(J),ISTAT2)
         IF (ISTAT2.NE.0) GO TO 30
         J=J+LRECOP
10       CONTINUE
C
      MAXOPT=NREC
      IF (IHCLDB.GT.1) WRITE (IOGDB,20) NXO,ISTAT2
C
20    FORMAT (' IN HWROPT - NXO=',I4,' ISTAT2=',I3)
      GO TO 50
C
30    WRITE (LP,40) NREC
40    FORMAT ('0**ERROR** IN HWROPT - WRITING OPTIONS - NREC=',I5)
      CALL ERROR
C
50    CALL FSTWHR (RTNOLD,IOLDOP,RTNOLD,IOLDOP)
C
      IF (IHCLTR.GT.1) WRITE (LP,*) 'EXIT ',RTNNAM
C
      RETURN
C
      END
