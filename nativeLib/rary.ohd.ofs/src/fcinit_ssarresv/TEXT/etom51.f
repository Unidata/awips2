C MEMBER ETOM51
C***********************************************************************
C
C@PROCESS LVL(77)
C
      SUBROUTINE ETOM51(PO)
C
C     ROUTINE ESTABLISHES UNITS FROM ENGLISH TO METRIC FOR
C     SSARRESV RESERVOIR OPERATION.
C
C***********************************************************************
C     PROGRAMMED BY KUANG HSU  OCTOBER 1994
C***********************************************************************
C
      DIMENSION PO(*)
      INCLUDE 'common/fengmt'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/unit51'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_ssarresv/RCS/etom51.f,v $
     . $',                                                             '
     .$Id: etom51.f,v 1.1 1996/03/21 14:45:26 page Exp $
     . $' /
C    ===================================================================
C
C
      DATA UNTL,UNTQ,UNTST/4HM   ,4HCMS ,4HTCUM/
      DATA DIML,DIMQ,DIMST/4HL   ,4HL3/T,4HL3  /
      DATA ACFT/4HACFT/
C
C-------------------------
C  SET DEFAULT CONVERSION.
C
      CONVL = 1.0
      CONVQ = 1.0
C  1 CMSD = 86.4 THOUSAND CUBIC METERS (TCUM)
      CONVST = 86.4
C
C--------------------------
C  SET DEFAULT UNITS FOR CONVERSION
C
      UNITL = UNTL
      UNITQ = UNTQ
      UNITST = UNTST
C
C----------------------------
C  DETERMINE UNITS USED FOR INPUT AND UNITS REQUESTED FOR CONVERSION
C  OUTPUT AND DETERMIN WHAT CONVERSIONS NEED TO BE MADE.
C
      METSAR = PO(8)
      ME = METRIC
      IF (METRIC .EQ. -1) ME=METSAR
C  METRIC IS AN INDECATOR FOR DESIRED CONVERSION UNITS
C  METRIC = 0, ENGLISH UNITS
C         = 1, METRIC UNITS
C         =-1, DEFAULT TO UNITS WHEN SEGMENT WAS INITIALLY DEFINED
C  METESAR IN AN INDECATOR FOR UNITS USED WHEN SEGMENT WAS DEFINED
C  METSAR = 1,  METRIC UNITS USED
C         = 0,  ENGLISH UNITS USED (W/STORAGE=ACFT)
C
      IF (ME .GE. 1) GO TO 9000
C
C----------------------------------
C  COMPUTE CONVERSION FACTORS FOR LENGTH AND FLOW
C
      ZERO=0.
      CALL FCONVT(UNTL,DIML,UNITL,CONVL,ZERO,IER)
      CALL FCONVT(UNTQ,DIMQ,UNITQ,CONVQ,ZERO,IER)
C
C--------------------------------------
C  COMPUTE CONVERSION FACTORS FOR STORAGE
C
C  STORAGE IN ACRE-FEET
C
      UNITST = ACFT
C
C  1 CMSD = 70.055875 ACRE-FEET
C
      CONVST = 70.055875
 9000 CONTINUE
      RETURN
      END
