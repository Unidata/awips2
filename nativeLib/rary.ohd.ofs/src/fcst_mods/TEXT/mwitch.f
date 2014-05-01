C MEMBER MWITCH
C  (from old member MCSHFT)
C
C @PROCESS LVL(77)
C
C                             LAST UPDATE: 07/06/95.15:13:41 BY $WC21DT
C
      SUBROUTINE MWITCH(I1,I2,IPOS,ISTAT)
C
      INCLUDE 'common/modrcs'      
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_mods/RCS/mwitch.f,v $
     . $',                                                             '
     .$Id: mwitch.f,v 1.1 1995/11/14 21:33:13 erb Exp $
     . $' /
C    ===================================================================
C
      IF (ISTAT.EQ.1) GOTO 10
      ITEMP=IJHSHF(I1,IPOS)
      LTEMP=LJHSHF(I1,IPOS)
      ISTEMP=ISTYPE(I1,IPOS)
      HTEMP=HNEW(I1,IPOS)
      QTEMP=QNEW(I1,IPOS)
      HLTEMP=HL(I1,IPOS)
      HUTEMP=HU(I1,IPOS)
      CALL UMEMOV(IJHSHF(I2,IPOS),IJHSHF(I1,IPOS),1)
      CALL UMEMOV(LJHSHF(I2,IPOS),LJHSHF(I1,IPOS),1)
10    CALL UMEMOV(ISTYPE(I2,IPOS),ISTYPE(I1,IPOS),1)
      CALL UMEMOV(HNEW(I2,IPOS),HNEW(I1,IPOS),1)
      CALL UMEMOV(QNEW(I2,IPOS),QNEW(I1,IPOS),1)
      CALL UMEMOV(HL(I2,IPOS),HL(I1,IPOS),1)
      CALL UMEMOV(HU(I2,IPOS),HU(I1,IPOS),1)
      IF (ISTAT.EQ.1) GOTO 999
      CALL UMEMOV(ITEMP,IJHSHF(I2,IPOS),1)
      CALL UMEMOV(LTEMP,LJHSHF(I2,IPOS),1)
      CALL UMEMOV(ISTEMP,ISTYPE(I2,IPOS),1)
      CALL UMEMOV(HTEMP,HNEW(I2,IPOS),1)
      CALL UMEMOV(QTEMP,QNEW(I2,IPOS),1)
      CALL UMEMOV(HLTEMP,HL(I2,IPOS),1)
      CALL UMEMOV(HUTEMP,HU(I2,IPOS),1)
999   CONTINUE
      RETURN
      END
