C MEMBER STP226
C  (from old member FCCOX26)
C
C***********************************************************************
C
      SUBROUTINE STP226(PO,LO,CO,LOCCLK)
C
C***********************************************************************
C
C     ACESS 'LAG/K' PARAMETER AND CARRYOVER
C
      DIMENSION PO(1),CO(1)
      LOGICAL FOP7
      COMMON/FATLGK/IATL,C1,C2
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_cox/RCS/stp226.f,v $
     . $',                                                             '
     .$Id: stp226.f,v 1.1 1995/09/17 18:47:37 dws Exp $
     . $' /
C    ===================================================================
C
      LOCCLK = LOCCLK + CO(LOCCLK)
      LR=LO+15
      NUMLGK=PO(LR)
      LOCK1=PO(LR+2) + LO - 1
      LOCK2=LOCK1+1
      IF(.NOT.FOP7(PO(LOCK1),PO(LOCK2)).OR.IATL.EQ.0)GO TO 1734
      NV7LK=PO(LO+NUMLGK)
      NV72LK=PO(LO+NUMLGK+1+2*NV7LK)
      NUMLGK=NUMLGK + 2 + 2*NV7LK + 2*NV72LK
 1734 LO=LO+NUMLGK
      RETURN
      END
