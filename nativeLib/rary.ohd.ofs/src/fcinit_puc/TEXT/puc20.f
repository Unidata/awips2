C MEMBER PUC20
C  (from old member FCPUC20)
C-----------------------------------------------------------------------
C                             LAST UPDATE: 10/31/95.09:32:03 BY $WC21DT
C
C @PROCESS LVL(77)
C
      SUBROUTINE PUC20(PO,CO)
C
C ......................................................................
C
C       THIS IS THE CARD PUNCH SUBROUTINE FOR THE CHANGE TIME INTERVAL
C     OPERATION.
C
C ......................................................................
C
C     SUBROUTINE ORIGINALLY WRITTEN BY:
C        ED VANBLARGAN - HRL   MAY,1981
C
C ......................................................................
C
      DIMENSION PO(*),CO(*),ISUBN(2),CHAR(3),CHARTO(3)
C
      COMMON/IONUM/IN,IPR,IPU
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/PUDFLT/IPDFLT
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_puc/RCS/puc20.f,v $
     . $',                                                             '
     .$Id: puc20.f,v 1.2 1996/01/16 23:02:31 page Exp $
     . $' /
C    ===================================================================
C
C
      DATA CHARTO/4H    ,4H    ,2H  /,NTRP/4HNTRP/
      DATA ISUBN,IYES,IBLK/4HPUC2,4H0   ,3HYES,3H   /
C
C     UTILITY SUBROUTINE TO GET TRACE LEVEL AND DEBUG.
C
      CALL FPRBUG(ISUBN,1,20,IBUG)
C
C     SET CONTROL VARIABLES.
C
      ITA=PO(5)
      ITB=PO(9)
C
      ICASE=ABS(PO(12))
      NOPT=IBLK
      IF (PO(12).LT.0.0) NOPT=NTRP
C
      NEEDC=PO(10)
      ICO=PO(11)
      IRDCO=IBLK
      IF (ICO.EQ.1) IRDCO=IYES
C
C     NOW PUNCH CARD INPUT AFTER CHECKING IF CO NEEDED AND IF DEFAULT
C     IS DESIRED.
C
      IF (NEEDC.EQ.0) GO TO 100
      IF (IPDFLT.EQ.0) GO TO 200
C
C     NO CARRYOVER OR DEFAULT USED.
C
100   WRITE (IPU,300) (PO(I),I=2,4),ITA,(PO(I),I=6,8),ITB,NOPT
      RETURN
C
C     ACTUAL CARRYOVER TO BE PUNCHED
C
200   NDEC=9
      IF (CO(1).GT.-10000000..AND.CO(1).LE.-1000000.) NDEC=1
      IF (CO(1).GT.-1000000..AND.CO(1).LE.-100000.) NDEC=2
      IF (CO(1).GT.-100000..AND.CO(1).LE.-10000.) NDEC=3
      IF (CO(1).GT.-10000..AND.CO(1).LE.-1000.) NDEC=4
      IF (CO(1).GT.-1000..AND.CO(1).LE.-100.) NDEC=5
      IF (CO(1).GT.-100..AND.CO(1).LE.-10.) NDEC=6
      IF (CO(1).GT.-10..AND.CO(1).LT.0.) NDEC=7
      IF (CO(1).GE.1..AND.CO(1).LT.10.) NDEC=8
      IF (CO(1).GE.10..AND.CO(1).LT.100.) NDEC=7
      IF (CO(1).GE.100..AND.CO(1).LT.1000.) NDEC=6
      IF (CO(1).GE.1000..AND.CO(1).LT.10000.) NDEC=5
      IF (CO(1).GE.10000..AND.CO(1).LT.100000.) NDEC=4
      IF (CO(1).GE.100000..AND.CO(1).LT.1000000.) NDEC=3
      IF (CO(1).GE.1000000..AND.CO(1).LT.10000000.) NDEC=2
      IF (CO(1).GE.10000000..AND.CO(1).LT.100000000.) NDEC=1
      CALL UFF2A(CO(1),CHAR,1,10,NDEC,0,6,IERR)
      IF (IERR.GE.1) WRITE(IPR,260)
C
      IF (ICASE.NE.7) GO TO 280
      NDEC=9
      IF (CO(2).GT.-10000000..AND.CO(2).LE.-1000000.) NDEC=1
      IF (CO(2).GT.-1000000..AND.CO(2).LE.-100000.) NDEC=2
      IF (CO(2).GT.-100000..AND.CO(2).LE.-10000.) NDEC=3
      IF (CO(2).GT.-10000..AND.CO(2).LE.-1000.) NDEC=4
      IF (CO(2).GT.-1000..AND.CO(2).LE.-100.) NDEC=5
      IF (CO(2).GT.-100..AND.CO(2).LE.-10.) NDEC=6
      IF (CO(2).GT.-10..AND.CO(2).LT.0.) NDEC=7
      IF (CO(2).GE.1..AND.CO(2).LT.10.) NDEC=8
      IF (CO(2).GE.10..AND.CO(2).LT.100.) NDEC=7
      IF (CO(2).GE.100..AND.CO(2).LT.1000.) NDEC=6
      IF (CO(2).GE.1000..AND.CO(2).LT.10000.) NDEC=5
      IF (CO(2).GE.10000..AND.CO(2).LT.100000.) NDEC=4
      IF (CO(2).GE.100000..AND.CO(2).LT.1000000.) NDEC=3
      IF (CO(2).GE.1000000..AND.CO(2).LT.10000000.) NDEC=2
      IF (CO(2).GE.10000000..AND.CO(2).LT.100000000.) NDEC=1
      CALL UFF2A(CO(2),CHARTO,1,10,NDEC,0,6,IERR)
      IF (IERR.GE.1) WRITE(IPR,260)
C
260   FORMAT(1H0,10X,35H**WARNING**CHECK PUNCHED CARRYOVER.,
     *34H  CHARACTER CONVERSION IMPOSSIBLE.)
C
280   WRITE (IPU,300) (PO(I),I=2,4),ITA,(PO(I),I=6,8),ITB,
     $ NOPT,IRDCO,CHAR,CHARTO
300   FORMAT(2X,2A4,1X,A4,3X,I2,2X,2A4,1X,A4,3X,I2,
     $ 1X,A4,2X,A3,2A4,A2,2A4,A2)
      RETURN
      END
