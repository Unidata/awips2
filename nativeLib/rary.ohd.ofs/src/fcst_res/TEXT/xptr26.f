C MEMBER XPTR26
C  (from old member FCXPTR26)
C
C DESC DETERMINE LOCATION IN PO AND CO ARRAYS FOR START OF PARMS, TIME-
C DESC  SERIES, AND CARRYOVER FOR A PARTICULAR S/U.
C---------------------------------------------------------------------
      SUBROUTINE XPTR26(SUNUM,PO,IORD,IBASE,LEVEL,LOCPM,LOCTS,LOCCO)
C----------------------------------------------------------------------
C
C  SUBROUTINE TO RETURN THE LOCATION IN THE PO ARRAY OF THE PARMS,
C  TIME-SERIES, AND CARRYOVER FOR A GIVEN S/U (SUNUM).
C
C-----------------------------------------------------------------------
C  WRITTEN BY - JOE OSTROWSKI - HRL - AUGUST 1983
C-----------------------------------------------------------------------
C
      INCLUDE 'common/resv26'
      INCLUDE 'common/exg26'
      INCLUDE 'common/fdbug'
C
      DIMENSION PO(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_res/RCS/xptr26.f,v $
     . $',                                                             '
     .$Id: xptr26.f,v 1.2 1999/04/23 16:56:44 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (IBUG.GE.1) WRITE(IODBUG,1600)
 1600 FORMAT('   *** ENTER XPTR26 ***')
C
      ISUNUM=SUNUM
      IBASE=ISUNUM/10
      LEVEL=ISUNUM-IBASE*10
      DO 550 I=1,NSUDEF
      LOCNUM = LOCPTR + (I-1)*4 + 1
      ISUX=PO(LOCNUM)
      KB=ISUX/10
      KL=ISUX-KB*10
      IF (KB.NE.IBASE.OR.KL.NE.LEVEL) GO TO 550
C
      IORD = I
      LOCPM = PO(LOCNUM+1)
      LOCTS = PO(LOCNUM+2)
      LOCCO = PO(LOCNUM+3)
      GO TO 9000
C
  550 CONTINUE
C
 9000 CONTINUE
      IF (IBUG.GE.2) WRITE(IODBUG,1680) SUNUM,IBASE,LEVEL,IORD,LOCPM,
     .                                  LOCTS,LOCCO
 1680 FORMAT('  *** POINTER INFO FOR S/U NO. ',F6.0/
     .7X,'BASE NO. = ',I5/8X,'LEVEL OF DEFN = ',I5
     ./9X,'ORDER OF DEFN = ',I2/9X,'PARM LOC. = ',
     .I5/10X,'TIME SERIES LOC. = ',I5/11X,'CARRYOVER LOC. = ',I5)
      IF (IBUG.GE.1) WRITE(IODBUG,1699)
 1699 FORMAT('    *** EXIT XPTR26 ***')
      RETURN
      END
