C MEMBER PUC8
C  (from old member FCPUC8)
C.......................................................................
      SUBROUTINE PUC8(PLOSS,CLOSS)
C
C     SUBROUTINE PUC8 PUNCHES INPUT CARDS FOR THE LOSS
C     OPERATION IN THE FORMAT REQUIRED BY THE PIN8 SUBROUTINE.
C
C.......................................................................
C     PROGRAMMED BY KAY KROUSE     OCTOBER 1979
C.......................................................................
C     VARIABLES IN ARGUMENT LIST:
C                1) PLOSS    -ARRAY CONTAINING PARAMETERS, TIME
C                             SERIES IDENTIFIERS, OPTIONS, ETC.
C                             FOT THE LOSS OPERATION
C
C.......................................................................
      REAL IDFQ,IDPE,NOPE
      DIMENSION PLOSS(1),ANAME(5),IDFQ(2),IDPE(2),CLOSS(1)
C
      INCLUDE 'common/fdbug'
      INCLUDE 'common/ionum'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_puc/RCS/puc8.f,v $
     . $',                                                             '
     .$Id: puc8.f,v 1.1 1995/09/17 18:51:06 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA NOPE,BLANK,DIUR,VARP,VARC,FIXP/4HNOPE,4H    ,4HDIUR,
     1 4HVARP,4HVARC,4HFIXP/
C
      IF(ITRACE.GE.1)WRITE(IODBUG,900)
  900 FORMAT(1H0,15H** PUC8 ENTERED)
C     NO DEBUG INFORMATION NEEDED.
C.......................................................................
C     RETRIEVE INPUT VALUES FROM PLOSS ARRAY
      DO 15 I=2,6
      ANAME(I-1)=PLOSS(I)
   15 CONTINUE
C
      IDFQ(1)=PLOSS(7)
      IDFQ(2)=PLOSS(8)
      QTYPE=PLOSS(9)
      ITQ=PLOSS(10)
      WSAREA=PLOSS(11)
      IDATA=0
      IF(WSAREA.LE.0.)GO TO 20
      IDATA=PLOSS(12)
      IF(IDATA.EQ.0)GO TO 25
      PEDATA=BLANK
      IDPE(1)=PLOSS(13)
      IDPE(2)=PLOSS(14)
      PETYPE=PLOSS(15)
      ITPE=PLOSS(16)
      ICRY=PLOSS(17)
C
      SSOUT=PLOSS(18)
      ISW=PLOSS(19)
      MM=19
      GO TO 30
   20 PEDATA=BLANK
      SSOUT=PLOSS(12)
      ISW=PLOSS(13)
      MM=13
      GO TO 30
   25 PEDATA=NOPE
      SSOUT=PLOSS(13)
      ISW=PLOSS(14)
      MM=14
C.......................................................................
C     PUNCH INPUT CARD 1
   30 IF(ISW.GT.2) GO TO 40
      SSWCH=BLANK
      IF(ISW.EQ.2)SSWCH=FIXP
      WRITE(IPU,800) (ANAME(I),I=1,5),SSWCH,SSOUT,WSAREA,PEDATA
  800 FORMAT(5A4,1X,A4,5X,F10.2,5X,F10.1,1X,A4)
      GO TO 50
   40 SSWCH=VARC
      IF(ISW.EQ.4)SSWCH=VARP
      WRITE(IPU,820) (ANAME(I),I=1,5),SSWCH,WSAREA,PEDATA
  820 FORMAT(5A4,1X,A4,20X,F10.1,1X,A4)
C.......................................................................
C     PUNCH TIME SERIES INPUT (CARD 2)
   50 IF(IDATA.EQ.0)GO TO 55
      WRITE(IPU,830) IDFQ,QTYPE,ITQ,IDPE,PETYPE,ITPE,ICRY
  830 FORMAT(2A4,3X,A4,I5,5X,2A4,3X,A4,2I5)
      GO TO 60
   55 WRITE(IPU,830) IDFQ,QTYPE,ITQ
C.......................................................................
C     PUNCH VARIABLE SSOUT VALUES-OPTIONAL (CARD 3)
   60 IF(ISW.LT.3)GO TO 65
      WRITE(IPU,840) (PLOSS(MM+J),J=1,12)
  840 FORMAT(6F10.2)
      MM=MM+24
C.......................................................................
C     PUNCH EVAPORATION LOSS PARAMETERS-OPTIONAL (CARDS 4-5)
   65 IF(WSAREA.LE.0.)GO TO 75
      ESWCH=BLANK
      ID=PLOSS(MM+1)
      PEADJ=PLOSS(MM+2)
      IF(ID.EQ.1)ESWCH=DIUR
      WRITE(IPU,850) PEADJ,ESWCH
  850 FORMAT(F5.2,6X,A4)
      MM=MM+2
      WRITE(IPU,860) (PLOSS(MM+J),J=1,12)
  860 FORMAT(12F5.2)
C.......................................................................
   75 CONTINUE
C     PUNCH INITIAL CARRYOVER VALUE IF REQUIRED
      IF((IDATA.EQ.1).AND.(ICRY.EQ.1))WRITE(IPU,870) CLOSS(1)
  870 FORMAT(F5.3)
      RETURN
      END
