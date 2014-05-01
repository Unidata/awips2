C     MODULE: FCPUC43     VERSION 1    
c  =====================================================================
c  pgm:  puc43 (po,co)
c
c   in: po     .... parameter array
c   in: co     .... carryover array
c  =====================================================================
      SUBROUTINE PUC43(PO,CO)

C#######################################################################
C
C  THIS ROUTINE PUNCHES CARDS WHICH CAN BE READ INTO THE PIN ROUTINE
C  TO DEFINE OR RE-DEFINE A SEGMENT OR SEGMENTS FOR THE API-HFD
C  OPERATION.
C
C#######################################################################

C  PARAMETRIC DATA IS OBTAINED FROM THE PO ARRAY PASSED INTO THIS
C  ROUTINE.  CARRYOVER DATA IS OBTAINED FROM THE CO ARRAY.  IF DEFAULT
C  CARRYOVER VALUES ARE REQUESTED, CARRYOVER IS NOT PUNCHED.

C#######################################################################

C  THE ORDER OF THE PUNCHED PARAMETRIC DATA IS AS FOLLOWS:

C     CARD 1:  RID, RNAME, IRNUM, RLAT, RLNG
C     CARD 2:  RFCTR, R24, PMAX, ULIMW, BLIMW, ULIMS, BLIMS, TBAR
C     CARD 3:  NREL, IDELTA, NSW, IOFAAA, ICOF
C     CARD 4:  TSIDRM, DTCRM, TSIDRO, DTCRO
C     CARD 5:  TSIDAP, DTCAP, TSIDAT, DTCAT, TSIDRI, DTCRI 

C     NOTE:  CARD 5 AS LISTED ABOVE IS OPTIONAL, DEPENDING ON THE
C            VALUE OF IOFAAA.  IF IOFAAA = 0, THIS CARD
C            IS NOT PUNCHED.

C  THE ORDER OF THE PUNCHED CARRYOVER DATA IS AS FOLLOWS:

C     CARD 6 (OR 5):  TAPI, TATI, TRI, SAPI, SATI, SRI, SRAIM, SRO,
C                     DRAIM, DRO
C     CARD 7 - 8 (OR 6 - 7):  RNSP(I),I=1,24
C
C     CARD 8 -11 (OR 7 -10):  MATI(I),I=1,52

C  FOR DEFINITION OF THESE VARIABLES, SEE THE PIN43 ROUTINE.

C#######################################################################
C  Initially written by
c     Ken Mack   NERFC                                8/10/95
c     Tim Sweeney  HRL                                October 1995
c.......................................................................
c
      include 'common/ionum'
      include 'common/fdbug'
      include 'common/pudflt'
c
c      COMMON /IONUM/ IN,IPR,IPU
c      COMMON /FDBUG/ IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
c      COMMON /PUDFLT/ IPDFLT
c
      DIMENSION PO(*),CO(*),SUBNAM(2),RID(2),RNAME(5),
     +          RNSP(24),MATI(52)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_puc/RCS/puc43.f,v $
     . $',                                                             '
     .$Id: puc43.f,v 1.1 1996/03/21 16:01:11 page Exp $
     . $' /
C    ===================================================================
C
c
      DATA SUBNAM /4HPUC4,4H3   /,NOP/43/,EMPTY/4H    /

C  CALL DEBUG CHECK ROUTINE.

      CALL FPRBUG(SUBNAM,1,NOP,IFDEB)

C  EXTRACT NEEDED INFO FROM THE PO ARRAY.

      RID(1) = PO(2)
      RID(2) = PO(3)
      DO 100 I=1,5
100   RNAME(I)=PO(I+3)
      IRNUM = PO(9)
      RLAT  = PO(10)
      RLNG  = PO(11)
      RFCTR = PO(12)
      R24   = PO(13)
      PMAX  = PO(14)
      ULIMW = PO(15)
      BLIMW = PO(16)
      ULIMS = PO(17)
      BLIMS = PO(18)
      TBAR  = PO(19)
      NREL  = PO(23)
      IDELTA= PO(24)
      NSW   = PO(25)
      NSPER = PO(26)
      LMTS  = PO(32)
      LWKT  = PO(33)
      LOTS  = PO(34)
c
      DO 106 I=1,52
106   MATI(I) = PO(LWKT+I-1)
C
C  SET CARRYOVER READ/NO READ FLAG DEPENDING ON VALUE OF IPDFLT.
C  IF IPDFLT = 1, DEFAULT CARRYOVER WILL BE USED.  IF IPDFLT = 0,
C  ACTUAL CARRYOVER WILL BE PUNCHED.

      IF(IPDFLT.GT.0) THEN
        ICOF = 0
      ELSE
        ICOF = 1
      ENDIF
C
      IOFAAA = 0
      IF(PO(LOTS).NE.EMPTY) IOFAAA = 1
      IF(PO(LOTS+3).NE.EMPTY) IOFAAA = 1
      IF(PO(LOTS+6).NE.EMPTY) IOFAAA = 1

C  PUNCH OUT FIRST 3 CARDS OF PARAMETRIC DATA.

      WRITE(IPU,1000)RID,RNAME,IRNUM,RLAT,RLNG
      WRITE(IPU,1010)RFCTR,R24,PMAX,ULIMW,BLIMW,ULIMS,BLIMS,TBAR
      WRITE(IPU,1020)NREL,IDELTA,NSW,IOFAAA,ICOF

C  PUNCH OUT 4TH CARD WITH INFO ON THE MANDATORY TIME SERIES NEEDED
C  BY THE API-HFD OPERATION.

      WRITE(IPU,1030) (PO(LMTS+I-1),I=1,6)

C  PUNCH OUT 5TH CARD WITH INFO ON THE OPTIONAL TIME SERIES, IF NEEDED.

      IF(IOFAAA.GT.0) WRITE(IPU,1030) (PO(LOTS+I-1),I=1,9)

C  IF CARRYOVER VALUES ARE TO BE DEFAULTED, RETURN.

      IF(ICOF.LE.0) GOTO 300

C  IF CARRYOVER VALUES ARE TO BE CURRENT VALUES FROM CO ARRAY,
C  PUNCH NEXT 2 (OR 3) CARDS WITH CURRENT CARRYOVER VALUES.

210   TAPI  = CO(1)
      TATI  = CO(2)
      TRI   = CO(3)
      SAPI  = CO(4)
      SATI  = CO(5)
      SRI   = CO(6)
      SRAIM = CO(7)
      SRO   = CO(8)
      DRAIM = CO(9)
      DRO   = CO(10)
      DO 220 I=1,NSPER
220   RNSP(I)=CO(10+I)
      WRITE(IPU,1240)TAPI,TATI,TRI,SAPI,SATI,SRI,SRAIM,SRO,DRAIM,DRO
      WRITE(IPU,1250)(RNSP(I),I=1,NSPER)
C
300   WRITE(IPU,1260)(MATI(I),I=1,52)
C
9999  RETURN
c
1000  FORMAT(2A4,6X,5A4,5X,I4,6X,F5.2,5X,F5.2)
1010  FORMAT(3F6.2,5F6.1)
1020  FORMAT(7I6)
1030  FORMAT(2A4,3X,A4,4X,2A4,3X,A4,5X,2A4,3X,A4)
1240  FORMAT(F6.2,F6.1,2F6.2,F6.1,5F6.2)
1250  FORMAT(12F5.2)
1260  FORMAT( 13(1X,I3) )
      END
