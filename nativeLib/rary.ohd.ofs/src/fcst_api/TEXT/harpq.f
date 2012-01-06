C MEMBER HARPQ
C  (from old member FCEX41)
C
      SUBROUTINE HARPQ(PA,PB,PC,PD,PE,SAI,SRAIM,SRO)
C
C  THIS SUBROUTINE REPRESENTS THE PRECIPITATION QUADRANT
C  OF THE NEW MARFC EVENT-BASED API-TYPE RAINFALL-RUNOFF MODEL.
C  IT DETERMINES STORM RUNOFF (SRO) FROM SAI AND SRAIM.
C
      COMMON /FDBUG/ IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON /IONUM/ IN,IPR,IPU
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_api/RCS/harpq.f,v $
     . $',                                                             '
     .$Id: harpq.f,v 1.1 1995/09/17 18:58:20 dws Exp $
     . $' /
C    ===================================================================
C
      IF(SRAIM.LE.0.0)GO TO 60
      D=PA+PB*SAI
      AN=PC+PD*SAI**PE
      SRO=((SRAIM**AN+D**AN)**(1.0/AN))-D
      IF(SRO.GE.0.0)RETURN
60    SRO=0.0
      RETURN
      END
