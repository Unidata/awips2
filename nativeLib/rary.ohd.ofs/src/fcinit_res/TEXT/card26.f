C MEMBER CARD26
C  (from old member FCCARD26)
C
C DESC READS INPUT CARDS FOR RES-SNGL OPERATION AND WRITES TO UNIT 89
C
C...................................................................
C
      SUBROUTINE CARD26
C
C...................................................................
C
C  READS ALL INPUT CARDS FOR RES-SNGL OPERATION (QUITS UPON 'END' CARD),
C  MARKS LOCATION (I.E. LINE NUMBER) OF 'GENERAL', 'SPECIFIC', AND
C  'RCL' LINES IF FOUND. A MISSING KEY COMMAND IS MARKED BY A LOCATION
C  OF ZERO.
C
C  INPUT IS WRITTEN TO UNIT 89 TO ALLOW REWINDING AND BACKSPACING OF
C  INPUT RECORDS.
C
C.......................................................................
C
C  JTOSTROWSKI - HRL - MARCH 1983
C................................................................
      INCLUDE 'uio'
      INCLUDE 'common/err26'
      INCLUDE 'common/read26'
      INCLUDE 'common/fld26'

      DIMENSION LOC(3),TYPE(2,3),NTOTAL(3),ETYPE(2,3)
C
      EQUIVALENCE (LOC(1),LGENL),(LOC(2),LSPEC),(LOC(3),LRCL),
     .   (NTOTAL(1),NGENL),(NTOTAL(2),NSPEC),(NTOTAL(3),NRCL)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_res/RCS/card26.f,v $
     . $',                                                             '
     .$Id: card26.f,v 1.1 1995/09/17 18:51:13 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA TYPE/4HGENE,4HRAL ,4HSPEC,4HIFIC,4HRCL ,4H    /
      DATA ETYPE/4HENDG,4HENL ,4HENDS,4HPEC ,4HENDR,4HCL  /
      DATA SEEK/4HEND /
C
      NSEEK = 1
      NUMERR = 0
C
C  TRANSFER ALL INPUT FOR RES-SNGL FROM UNIT 5 TO UNIT 89
C
      CALL TRAN26(SEEK,NSEEK)
      IF (NUMERR.GT.0) CALL EROT26
C
C  INITIALIZE LOCATIONS AND COUNTERS,
C  LOC(I) AND NTOTAL(I) HAVE ALSO BEEN
C  INTIALIZED THRU EQUIVALENCE HERE.
C
      LGENL = 0
      LSPEC = 0
      LRCL  = 0
      NGENL = -999
      NSPEC = -999
      NRCL  = -999
      MUNI26 = 89
      IOLICD = ICD
      ICD = MUNI26
      NCHAR = 25
C
C  LOOP THROUGH ALL INPUT LOOKING FOR (IN ORDER) THE FIELDS, AS THE
C  FIRST FIELDS ON THE LINES OF INPUT, 'GENERAL', 'SPECIFIC' AND 'RCL'.
C
      DO 100 I=1,3
      REWIND MUNI26
      USEDUP = .FALSE.
      NF26 = 0
      NCARD = 0
C
      DO 90 J=1,NCD26
C
C  IF HEADER CARD FOR SECTION ALREADY FOUND, LOOK FOR END CARD
C
      IF (LOC(I).GT.0) GO TO 25
C
      CALL UFLD26(NF26,IRF)
      IF (IRF.GT.0) GO TO 90
      IF (LEN.GT.8) GO TO 90
C
C  LOOK FOR MATCH WITH FIRST FIELD ON CARD
C
      IF (IUSAME(CHAR(1),TYPE(1,I),2).NE.1) GO TO 90
C
C  MATCH FOUND MARK LOCATION
C
      LOC(I) = NCARD
C
C  NOW LOOK FOR END CARD FOR SECTION
C
   25 CALL UFLD26(NF26,IRF)
      IF (IRF.GT.0) GO TO 90
      IF (LEN.GT.8) GO TO 90
C
      IF (IUSAME(CHAR(1),ETYPE(1,I),2).NE.1) GO TO 90
C
C  SET NUMBER OF CARDS IN THIS SECTION IF MATCH IS FOUND
C
      NTOTAL(I) = NCARD-LOC(I)+1
      GO TO 100
C
   90 CONTINUE
  100 CONTINUE
C
      ICD = IOLICD
C
      RETURN
      END
