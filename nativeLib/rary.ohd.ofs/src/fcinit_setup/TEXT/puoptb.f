C MEMBER PUOPTB
C  (from old member FCPUOPTB)
C-----------------------------------------------------------------------
C                             LAST UPDATE: 10/11/95.13:45:34 BY $WC21DT
C
C @PROCESS LVL(77)
C
C
      SUBROUTINE PUOPTB(P,MP,C,MC,T,MT,TS,MTS)
C.......................................
C     THIS SUBROUTINE PUNCHES THE OPERATIONS TABLE INPUT CARDS FOR A
C         SEGMENT.  OPERATIONS ARE PUNCHED IN EXECUTION ORDER.
C.......................................
C     SUBROUTINE WRITTEN BY...
C        ERIC ANDERSON - HRL  DECEMBER 1980
C.......................................
      DIMENSION P(MP),C(MC),TS(MTS)
      INTEGER T(MT)
      DIMENSION OLDNAM(2),SNAME(2),OPID(2)
C
C     COMMON BLOCKS
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/IONUM/IN,IPR,IPU
      COMMON/WHERE/ISEG(2),IOPNUM,OPNAME(2)
      COMMON/FCOPPT/LOCT,LPM,LCO
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_setup/RCS/puoptb.f,v $
     . $',                                                             '
     .$Id: puoptb.f,v 1.3 2002/10/10 13:48:38 xfan Exp $
     . $' /
C    ===================================================================
C
C
C     DATA STATEMENTS
      DATA SNAME/4HPUOP,4HTB  /
      DATA IBUG/4HSGPU/
      DATA BLANK/4H    /
C.......................................
C     TRACE LEVEL FOR THIS SUBROUTINE=1
      IF(ITRACE.GE.1) WRITE(IODBUG,900)
  900 FORMAT(1H0,17H** PUOPTB ENTERED)
C
      CALL UMEMOV(OPNAME,OLDNAM,2)
C.......................................
C     INITIAL VALUE
      LOCT=1
C.......................................
C     FIND NUMBER OF NEXT OPERATION.
  100 NUMOP=T(LOCT)
C
C     CHECK FOR STOP OPERATION.
      IF(NUMOP.EQ.-1) GO TO 90
CMGM  CHECK FOR MISSING OPERATION
      IF(NUMOP.EQ.0) GO TO 90
      IERR=0
      IF(NUMOP.EQ.4) GO TO 105
C.......................................
C     DETERMINE WHERE OPERATION IS IN P AND C ARRAYS.
C     ALSO SET WHERE COMMON BLOCK VALUES.
      LPM=T(LOCT+2)
      LCO=P(LPM-1)
      IOPNUM=NUMOP
      OPNAME(1)=P(LPM-5)
      OPNAME(2)=P(LPM-4)
      GO TO 110
C
C     SPECIAL OPERATIONS - NO ENTRY IN P OR C ARRAYS.
  105 LPM=0
      LCO=0
      IOPNUM=NUMOP
      OPNAME(1)=BLANK
      OPNAME(2)=BLANK
C.......................................
C     GET OPERATION IDENTIFIER
  110 CALL FOPCDX(OPID,NUMOP)
C
C     CHECK FOR DEBUG OUTPUT
      IF(IFBUG(IBUG).EQ.1)WRITE(IODBUG,901)ISEG,NUMOP,OPNAME,LOCT,
     1 LPM,LCO
  901 FORMAT(1H0,19HPUOPTB DEBUG--ISEG=,2A4,3X,6HNUMOP=,I3,3X,
     1 7HOPNAME=,2A4,3X,5HLOCT=,I5,3X,4HLPM=,I5,3X,4HLCO=,I5)
C.......................................
C     WRITE TITLE FOR OPERATION.
      WRITE(IPU,902)OPID,OPNAME
  902 FORMAT(2A4,4X,2A4)
C.......................................
C     GO TO THE PROPER PUNCH DRIVER FOR THE OPERATION.
      IF (NUMOP.GT.19) GO TO 120
      CALL PUOPT1(P,MP,C,MC,T,MT,TS,MTS,NUMOP,IERR)
      GO TO 129
  120 IF (NUMOP.GT.40) GO TO 121
      CALL PUOPT2(P,MP,C,MC,T,MT,TS,MTS,NUMOP,IERR)
      GO TO 129
  121 CALL PUOPT3(P,MP,C,MC,T,MT,TS,MTS,NUMOP,IERR)
  129 IF(IERR.EQ.0) GO TO 99
C
C     PUT PROPER VALUES INTO THE WHERE COMMON BLOCK.
      IOPNUM=0
      OPNAME(1)=SNAME(1)
      OPNAME(2)=SNAME(2)
C.......................................
C     CALL TO OPERATION DOES NOT EXIST.
      WRITE(IPR,903)OPID
  903 FORMAT(1H0,10X,63H**ERROR** THE PUOPTB SUBROUTINE DOES NOT CONTAIN
     1 A CALL FOR THE,1X,2A4,1X,10HOPERATION.)
      CALL ERROR
      GO TO 99
C.......................................
C     STOP OPERATION--WRITE STOP CARD.
   90 WRITE(IPU,904)
  904 FORMAT(4HSTOP)
      GO TO 199
C.......................................
C     INCREMENT TO THE NEXT OPERATION.
   99 LOCT=T(LOCT+1)
      IF(LOCT.LE.MT) GO TO 100
      GO TO 90
  199 CONTINUE
      CALL UMEMOV(OLDNAM,OPNAME,2)
      RETURN
      END
