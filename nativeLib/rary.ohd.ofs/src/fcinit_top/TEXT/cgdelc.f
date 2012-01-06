C MODULE CGDELC
C-----------------------------------------------------------------------
C
C  ROUTINE TO REMOVE CARRYOVER GROUP FROM ALL SEGMENTS
C
      SUBROUTINE CGDELC (INCGID)
C
C  ROUTINE ORIGINALLY WRITTEN BY -- ED JOHNSON - HRL - 11/1979
C
      CHARACTER*8 OPNOLD
      DIMENSION INCGID(2)
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fcsegn'
      INCLUDE 'common/fcsegp'
      INCLUDE 'common/fcunit'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_top/RCS/cgdelc.f,v $
     . $',                                                             '
     .$Id: cgdelc.f,v 1.4 2000/03/14 11:56:39 page Exp $
     . $' /
C    ===================================================================
C
      DATA IBLANK/4H    /
C
C
      IF (ITRACE.GT.0) WRITE(IODBUG,*) 'ENTER CGDELC'
C
      IOPNUM=0
      CALL FSTWHR ('CGDELC  ',IOPNUM,OPNOLD,IOLDOP)
C
      IBUG=IFBUG('CGDF')+IFBUG('DLTE')
C
      IF (IBUG.GT.0) WRITE(IODBUG,800) INCGID
 800  FORMAT (' INCGID=',2A4)
C
      IF (NRSTS.EQ.0) THEN
         WRITE(IPR,901)
 901  FORMAT ('0**WARNING** NO SEGMENTS EXIST.')
         CALL WARN
         GO TO 110
         ENDIF
C
      DO 100 IRSEG=1,NRSTS
         CALL UREADT (KFSGST,IRSEG,IDSEGN,IERR)
         IF (INCGID(1).EQ.ICGID(1).AND.INCGID(2).EQ.ICGID(2)) THEN
            IF (IBUG.GT.0) WRITE (IODBUG,801) IDSEGN
 801  FORMAT (' IDSEGN=',2A4)
            ICGID(1)=IBLANK
            ICGID(2)=IBLANK
            CALL UWRITT (KFSGST,IRSEG,IDSEGN,IERR)
	    ENDIF
 100     CONTINUE
C
 110  CALL FSTWHR (OPNOLD,IOLDOP,OPNOLD,IOLDOP)
C
      IF (ITRACE.GT.0) WRITE (IODBUG,*) 'ENTER CGDELC'
C
      RETURN
C
      END
