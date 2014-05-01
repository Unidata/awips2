C MODULE UFLD51
C---------------------------------------------------------------------
C
      SUBROUTINE UFLD51(NUMFLD,IRC)
C
C DESC USED TO GET NEXT FIELD ON A LINE OF INPUT BY CALLING UFIELD
C
C...................................................................
C
C  ARGUMENT LIST:
C   NUMFLD - SPECIFIES HOW UFIELD IS TO ACT,
C            >0, UFIELD IS TO LOOK FOR NEXT FIELD GOING TO NEXT CARD IF
C                NEED BE,
C            =0, UFIELD IS TO FIND THE NEXT NONCOMMENTED STRING GOING TO
C                NEXT CARD IF NEED BE,
C            =-1, UFIELD IS TO RESCAN CURRENT FIELD
C            =-2, UFIELD IS TO LOOK FOR NEXT FIELD BUT IS NOT TO READ
C                 NEXT RECORD TO FIND FIELD.
C
C      IRC - RETURN CODE,
C            =0, ALL IS OK
C            =1, NULL FIELD OR EMPTY CARD AFTER CURRENT FIELD
C            =2, CAN'T PACK STRING INTO CHAR,
C            =3, END OF INPUT LINES,
C            =4, INVALID REPEAT FACTOR
C
C...................................................................
C
C  KUANG HSU - HRL - OCTOBER 1995
C................................................................
      INCLUDE 'common/fld51'
      INCLUDE 'common/read51'
      INCLUDE 'uiox'
      INCLUDE 'ufreex'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_ssarresv/RCS/ufld51.f,v $
     . $',                                                             '
     .$Id: ufld51.f,v 1.3 1998/07/02 19:42:18 page Exp $
     . $' /
C    ===================================================================
C
C
C  CHANGE UNIT TO POINT TO UNIT 89 FOR SSARRESV INPUT
C
      IOLDCD = ICD
      ICD = MUNI51
C
C
C  SET ACTION SWITCHES FOR UFIELD
C
      IF (NUMFLD.EQ. 0) ISTRT = 1
      IF (NUMFLD.EQ. 0) NFLD = 0
      IF (NUMFLD.EQ.-2) ISTRT = -2
      IF (NUMFLD.EQ.-1) ISTRT = -1
      ICKDAT = 0
C
C  IF NO MORE INPUT ALREADY INDICATED, JUST EXIT W/ PROPER RETURN CODE
C
      IF (USEDUP) GO TO 990
C
      NRDCRD = NCARD
C
      CALL UFIELD (NFLD,ISTRT,LEN,ITYPE,NREP,INTEGR,REAL,
     *   NCHAR,CHAR,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,ISTAT)
C
      NCARD = NRDCRD
      IRC = ISTAT
C
C  IF LAST CARD FOR THIS SUBSECTION HAS BEEN READ IN, SET 'END-OF-INPUT'
C
      IF (IRC .EQ. 3) GO TO 990
C
C  IF LEN = 0 , NO MORE FIELDS WERE FOUND ON THE CURRENT LINE
C
      IF (LEN .EQ. 0) IRC = 1
      GO TO 999
C
C  SET FLAG FOR END OF INPUT
C
  990 CONTINUE
      USEDUP = .TRUE.
      IRC = 3
C
  999 CONTINUE
      ICD = IOLDCD
      RETURN
      END
