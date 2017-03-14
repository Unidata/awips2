C MEMBER PUC23
C  (from old member FCPUC23)
C
C  DESC -- CARD PUNCH SUBROUTINE FOR STAGE-Q OPERATION
C
C.......................................................................
C
      SUBROUTINE PUC23(PO,CO)
C
C.......................................................................
C
C  THIS  IS THE CARD PUNCH SUBROUTINE FOR THE STAGE-Q OPERATION
C
C***********************************************************************
C
C   CONTENTS OF THE P ARRAY -
C
C    GENERAL INFORMATION
C
C    POSITION      NAME      DESCRIPTION
C       1          IVERS     VERSION NUMBER
C      2-6         PNTNAM    20-CHAR NAME OF GAGING STATION OR FORECAST
C                            POINT
C       7          ICNVRT    CONVERSION INDICATOR
C                            =1, STAGE TO DISCHARGE
C                            =2, DISCHARGE TO STAGE
C                            (DEFAULT IS 1)
C      8-9         STGID     8-CHAR STAGE T.S. IDENTIFIER
C       10         STGTYP    4-CHAR STAGE T.S. DATA TYPE CODE
C       11         IDTSTG    TIME INTERVAL IN HOURS FOR STAGE T.S.
C     12-13        QDATID    8-CHAR DISCHARGE T.S. IDENTIFIER
C       14         QTYPE     4-CHAR DISCHARGE T.S. DATA TYPE CODE
C       15         IDTQ      TIME INTERVAL IN HOURS FOR DISCHARGE T.S.
C     16-17        RCID      8-CHAR RATING CURVE IDENTIFIER
C       18         ICO       DEFAULT CARRYOVER INDICATOR
C                            =0, DEFAULT CARRYOVER (ALL ZEROES) USED
C                            =1, INITIAL CARRYOVER READ IN
C
C---------------------------------------------------------------------
C
C   CONTENTS OF THE C ARRAY -
C
C    GENERAL INFORMATION
C
C    POSITION      NAME      DESCRIPTION
C       1           HP       PREVIOUS STAGE VALUE
C       2           QP       PREVIOUS DISCHARGE VALUE
C       3           DQ       RATE OF CHANGE IN DISCHARGE (ICNVRT=1)
C                   DH       RATE OF CHANGE IN STAGE  (ICNVRT=2)
C       4           MISING   NO. OF MISSING VALUES IMMEDIATLY PRECEDING
C                            FIRST VALUE OF T.S. TO CONVERT
C
C.......................................................................
C
C  SUBROUTINE ORIGINALLY WRITTEN BY --
C      JONATHAN WETMORE - HRL - 810415
C  THIS IS THE FIRST VERSION OF THIS OPERATION
C
C***********************************************************************
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/pudflt'
C
      DIMENSION PO(1),CO(1)
      DIMENSION SUBNAM(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_puc/RCS/puc23.f,v $
     . $',                                                             '
     .$Id: puc23.f,v 1.1 1995/09/17 18:50:42 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA SUBNAM/4HPUC2,4H3   /
      DATA LTRACE,NOP/1,23/
      DATA READCO,BLANK/4HRDCO,4H    /
C
C
      CALL FPRBUG(SUBNAM,LTRACE,NOP,IBUG)
C
       ICNVRT=PO(7)
       IDTSTG=PO(11)
       IDTQ=PO(15)
       MISING=CO(4)
C
       CODFLT=READCO
       IF(IPDFLT.EQ.1) CODFLT=BLANK
C
C  PUNCH CARDS
      WRITE(IPU,1)(PO(J),J=2,6),(PO(K),K=16,17),ICNVRT,CODFLT
      WRITE(IPU,2)(PO(J),J=8,9),PO(10),IDTSTG,(PO(K),K=12,13),PO(14),
     1IDTQ
C
C  PUNCH CARRYOVER DATA CARDS IF REQUESTED
C
      IF(IPDFLT.EQ.0) WRITE(IPU,3)(CO(K),K=1,3),MISING
C
C  DBUG INFO ON PO AND CO ARRAY REQUESTED?
      IF(IBUG.LT.1) GO TO 999
      WRITE(IODBUG,90) (PO(I),I=1,6)
      WRITE(IODBUG,91) (PO(I),I=7,11)
      WRITE(IODBUG,92) (PO(I),I=12,18)
      WRITE(IODBUG,93) (CO(I),I=1,4)
C
C
C
C   FORMAT STATEMENTS
C
  1   FORMAT(5A4,2X,2A4,9X,I1,1X,A4)
  2   FORMAT(2X,2A4,1X,A4,3X,I2,2X,2A4,1X,A4,3X,I2)
  3   FORMAT(F10.2,F10.0,F10.2,I10)
C
 90   FORMAT(1H0,2X,18HPO ARRAY CONTENTS: //2X,6HPO(1)=,F4.2,3X,07HPO(2)
     1= ,A4,3X,06HPO(3)=,A4,3X,07HPO(4)= ,A4,3X,07HPO(5)= ,A4,3X,07HPO(
     .6)= ,A4)
 91   FORMAT(01H0,2X,07HPO(7)= ,F4.2,3X,07HPO(8)= ,A4,3X,07HPO(9)= ,A4,3
     1X,  08HPO(10)= ,A4,3X,08HPO(11)= ,F5.2)
 92   FORMAT(01H0,2X,07HPO(12)=,A4,2X,07HPO(13)=,A4,2X,07HPO(14)=,A4,
     13X, 07HPO(15)=,F5.2,3X,07HPO(16)=,A4,3X,07HPO(17)=,A4,07HPO(18)=,
     1F5.2)
 93   FORMAT(01H0,2X,19H CO ARRAY CONTENTS: //2X,07HCO(1)= ,F10.2,3X,
     1   07HCO(2)= ,F10.0,3X,07HCO(3)= ,F10.2,3X,07HCO(4)= ,F10.2)
 999   CONTINUE
       IF(ITRACE.GE.1) WRITE(IODBUG,6002)
 6002  FORMAT(1H ,19H *** EXIT PUC23 ***)
C
C
       RETURN
       END
