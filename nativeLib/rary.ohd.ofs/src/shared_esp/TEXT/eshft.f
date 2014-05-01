C MEMBER ESHFT
C  (from old member EESHFT)
C
      SUBROUTINE ESHFT(JD,KH,IDT,ISHIFT)
C
C   THIS SUBROUTINE ADJUSTS A JULIAN DAY AND HOUR SUCH THAT
C   IT IS A MULTIPLE OF A DT. THE CLOSEST VALUE IS SELECTED.
C
C   THIS SUBROUTINE WAS ORIGINALLY WRITTEN BY GERALD N DAY (HRL).
C
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/where'
C
      DIMENSION SBNAME(2),OLDOPN(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_esp/RCS/eshft.f,v $
     . $',                                                             '
     .$Id: eshft.f,v 1.1 1995/09/17 19:19:05 dws Exp $
     . $' /
C    ===================================================================
C
      DATA SBNAME/4HESHF,4HT   /
C
      IOLDOP=IOPNUM
      IOPNUM=0
      DO 10 I=1,2
      OLDOPN(I)=OPNAME(I)
   10 OPNAME(I)=SBNAME(I)
C
      IF(ITRACE.GE.1) WRITE(IODBUG,900)
  900 FORMAT(1H0,16H** ESHFT ENTERED)
C
      ISHIFT=0
      NPER=24/IDT
      DO 100 I=1,NPER
      IF(KH.GT.(IDT*I)) GO TO 100
      IF(KH.EQ.(IDT*I)) GO TO 999
      IPER=I
      GO TO 120
  100 CONTINUE
C
      WRITE(IPR,600)
  600 FORMAT(1H0,10X,29H**ERROR** IN ESHFT, IDT MUST ,
     1 19HBE A MULTIPLE OF 24)
      CALL KILLFN(8HESP     )
      GO TO 999
C
  120 IPLUS=KH-(IPER-1)*IDT
      IMINUS=KH-IPER*IDT
      IMOVE=IPLUS
      IF(IABS(IMINUS).LT.IPLUS) IMOVE=IMINUS
      KH=KH-IMOVE
      IF(KH.GT.0) GO TO 200
      JD=JD-1
      KH=24
  200 ISHIFT=-IMOVE
  999 CONTINUE
C
      IOPNUM=IOLDOP
      OPNAME(1)=OLDOPN(1)
      OPNAME(2)=OLDOPN(2)
C
      RETURN
      END
