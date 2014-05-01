C MODULE CGDELA
C-----------------------------------------------------------------------
C
C  ROUTINE TO DELETE A CARROVER GROUP
C
      SUBROUTINE CGDELA (CGIDIN,IER)
C
C  ROUTINE ORIGINALLY WRITTEN BY - ED JOHNSON - HRL - 11/1979
C
      CHARACTER*8 RTNNAM,OPNOLD
C
      DIMENSION CGIDIN(2)
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fccgd'
      INCLUDE 'common/fcunit'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_top/RCS/cgdela.f,v $
     . $',                                                             '
     .$Id: cgdela.f,v 1.4 2000/03/14 11:54:10 page Exp $
     . $' /
C    ===================================================================
C
C
      RTNNAM='CGDELA'
C
      IF (ITRACE.GT.0) WRITE (IODBUG,*) 'ENTER ',RTNNAM
C
      IOPNUM=0
      CALL FSTWHR (RTNNAM,IOPNUM,OPNOLD,IOLDOP)
C
      IBUG=IFBUG('CGDF')+IFBUG('DLTE')
C
      IER=0
C
      IF (IBUG.GT.0) THEN
         WRITE (IODBUG,10) CGIDIN
10    FORMAT (' IN CGDELA: CGIDIN=',2A4)
         WRITE (IODBUG,20) NCG,((CGIDS(J,I),J=1,2),ICOREC(I),I=1,NCG)
20    FORMAT (' IN CGDELA: NCG=',I5,' IDENTIFIERS AND RECORDS:',
     *  5(/' ',5(2A4,I5,' ')))
         ENDIF
C
C  GET NUMBER OF CARRYOVER GROUPS
      CALL UREADT (KFCGD,1,NSLOTS,IERR)
      IF (NCG.EQ.0) THEN
         WRITE (IPR,30)
30    FORMAT ('0**WARNING** NO CARRYOVER GROUP ARE DEFINED.')
         CALL WARN
         IER=1
         GO TO 90
         ENDIF
      DO 40 ICG=1,NCG
         IF (CGIDIN(1).NE.CGIDS(1,ICG))GO TO 40
         IF (CGIDIN(2).NE.CGIDS(2,ICG))GO TO 40
         GO TO 60
40       CONTINUE
C
      WRITE (IPR,50) CGIDIN
50    FORMAT ('0**WARNING** CARRYOVER GROUP ',2A4,' NOT FOUND.')
      CALL WARN
      IER=1
      GO TO 90
C
C  REMOVE CARRYOVER GROUP FROM FILE FCCOGDEF
60    NCGM1=NCG-1
      IF (ICG.EQ.NCG) GO TO 80
      DO 70 I=ICG,NCGM1
         CGIDS(1,I)=CGIDS(1,I+1)
         CGIDS(2,I)=CGIDS(2,I+1)
         ICOREC(I)=ICOREC(I+1)
70       CONTINUE
80    NCG=NCG-1
      CALL UWRITT (KFCGD,1,NSLOTS,IERR)
      IF (IBUG.GT.0) WRITE (IODBUG,20) NCG,((CGIDS(J,I),J=1,2),
     *  ICOREC(I),I=1,NCG)
C
90    CALL FSTWHR (OPNOLD,IOLDOP,OPNOLD,IOLDOP)
C
      IF (ITRACE.GT.0) WRITE (IODBUG,*) 'EXIT ',RTNNAM
C
      RETURN
C
      END
