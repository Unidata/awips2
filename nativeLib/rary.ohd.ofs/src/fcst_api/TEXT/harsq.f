C MEMBER HARSQ
C  (from old member FCEX41)
C
      SUBROUTINE HARSQ(AEIWET,BWET,CWET,AEIDRY,BDRY,CDRY,SAPI,SAEI,SAI)
C
C  THIS SUBROUTINE REPRESENTS THE SEASON QUADRANT OF THE NEW
C  MARFC EVENT-BASED API-TYPE RAINFALL-RUNOFF MODEL.  IT DETERMINES
C  THE SAI FROM THE GIVEN STATE VARIABLES SAPI AND SAEI.
C
      COMMON /FDBUG/ IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON /IONUM/ IN,IPR,IPU
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_api/RCS/harsq.f,v $
     . $',                                                             '
     .$Id: harsq.f,v 1.1 1995/09/17 18:58:21 dws Exp $
     . $' /
C    ===================================================================
C
      AIW=BWET*CWET**SAPI
      AID=BDRY*CDRY**SAPI
      IF(AID.LT.AIW)AID=AIW
      SAI=AIW+(AID-AIW)*((SAEI-AEIWET)/(AEIDRY-AEIWET))
      RETURN
      END
