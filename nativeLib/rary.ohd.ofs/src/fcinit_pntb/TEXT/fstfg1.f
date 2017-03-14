C MEMBER FSTFG1
C  (from old member FCPIN1)
C
      SUBROUTINE FSTFG1(PF,CF,NFCO,NXFCO)
C.......................................
C     THIS SUBROUTINE STORES FROZEN GROUND INFORMATION IN THE PL ARRAY
C      FOR THE 'SAC-SMA ' OPERATION.
C.......................................
C     SUBROUTINE WRITTEN BY-ERIC ANDERSON-HRL JUNE 1980
C.......................................
      DIMENSION PF(1),CF(1)
C
C     COMMON BLOCK
      COMMON/FINFG1/TAID(2),TATYPE,ITTA,WEID(2),WETYPE,ITWE,LWE,FIID(2),
     1 ITFI,LFI,FGPM(10),FGCO(6),PTA,PWE
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_pntb/RCS/fstfg1.f,v $
     . $',                                                             '
     .$Id: fstfg1.f,v 1.1 1995/09/17 18:47:57 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................
C     INITIAL VALUE
      LP=8
C.......................................
C     STORE VALUES
      PF(1)=TAID(1)
      PF(2)=TAID(2)
      PF(3)=TATYPE
      PF(4)=ITTA+0.01
      PF(5)=LWE+0.01
      PF(6)=LFI+0.01
      PF(7)=NXFCO+0.01
      DO 100 I=1,10
      J=I-1
  100 PF(LP+J)=FGPM(I)
      IF(LWE.EQ.0) GO TO 101
      PF(LWE)=WEID(1)
      PF(LWE+1)=WEID(2)
      PF(LWE+2)=WETYPE
      PF(LWE+3)=ITWE+0.01
  101 IF(LFI.EQ.0) GO TO 105
      PF(LFI)=FIID(1)
      PF(LFI+1)=FIID(2)
      PF(LFI+2)=ITFI+0.01
C
C     STORE CARRYOVER
  105 DO 106 I=1,NFCO
  106 CF(I)=FGCO(I)
      CF(NFCO+1)=PTA
      IF (LWE.GT.0) CF(NFCO+2)=PWE
C.......................................
C     CONTENTS OF PF ARRAY (PART OF PL ARRAY)
C     POSITION                   CONTENTS
C     1-2    IDENTIFIER FOR AIR TEMPERATURE TIME SERIES
C      3      DATA TYPE CODE-AIR TEMPERATURE
C      4     AIR TEMPERATURE TIME INTERVAL
C      5     LOCATION OF WATER-EQUIVALENT INFRO. IN PF(), =0 IF NONE
C      6     LOCATION OF FROST INDEX INFRO. IN PF(), =0 IF NONE
C      7     NUMBER OF EXTRA CARRYOVER VALUES.
C      8     FROZEN GROUND PARAMETERS--TOTAL OF 10.
C     OPTIONAL INFORMATION CONTAINS...
C     A. WATER-EQUIVALENT INFRO.-4 WORDS
C        2 FOR I.D., PLUS DATA TYPE AND TIME INTERVAL
C      B. FROST INDEX INFRO.-3 WORDS
C       2 FOR I.D., PLUS TIME INTERVAL
C.......................................
      RETURN
      END
