C MEMBER SU2126
C  (from old member FCSU2126)
C
      SUBROUTINE SU2126(WORK,IUSEW,LEFTW,IERR)
C
C---------------------------------------------------------------------
C  IF THIS ROUTINE IS CALLED, SIGNAL AN ERROR. NO INPUT IS NEEDED TO
C  DEFINE THIS S/U - INDUCED SURCHARGE UTILITY
C---------------------------------------------------------------------
C  ARGS:
C     WORK - ARRAY TO HOLD INFORMATION
C    IUSEW - NUMBER OF WORDS ALREADY USED IN WORK ARRAY
C    LEFTW - NUMBER OF WORDS LEFT IN WORK ARRAY
C-------------------------------------------------------------------
C  JTOSTROWSKI - HRL - MARCH 1983
C----------------------------------------------------------------
C
      INCLUDE 'common/err26'
C
C
      INCLUDE 'common/fld26'
C
C
      INCLUDE 'common/read26'
C
C
      INCLUDE 'common/suid26'
C
C
      INCLUDE 'common/suin26'
C
C
      INCLUDE 'common/suky26'
C
C
      INCLUDE 'common/warn26'
C
C
C
      DIMENSION ENDSU(3)
      DIMENSION WORK(1)
C
      LOGICAL ENDFND
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_res/RCS/su2126.f,v $
     . $',                                                             '
     .$Id: su2126.f,v 1.1 1995/09/17 18:53:07 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA ENDSU/4HENDE,4HISC ,4H    /
C
C
C  INITIALIZE NO. OF WORDS FOR HOLDING PARMS, TIME-SERIES, AND CARRYOVER
C  FOR THIS SCHEME/UTILITY.
C
      NPARXX = 0
      NTSXX = 0
      NCOXX = 0
C
      NPACK = 3
C
C--------------------------------------------------------------------
C  SET ERROR FOR FINDING INPUT TO 'ENTERISC' AND LOOK FOR 'ENDPASS'
C  KEYWORD OR OTHER SU.
C
      CALL STRN26(53,1,SUID(1,21),3)
C
      USEDUP = .FALSE.
      ENDFND = .FALSE.
C
      IERR = 0
C
C  SU FOUND , LOOKING FOR ENDSU
C
      LPOS = LSPEC + NCARD + 1
      LASTCD = NSPEC -2
C
   10 IF (NCARD .LT. LASTCD) GO TO 20
           IERR = 1
           GO TO 50
   20 NUMFLD = 0
      CALL UFLD26(NUMFLD,IERF)
      IF(IERF .GT. 0 ) GO TO 9000
      ISAME = IUSAME(CHAR,ENDSU,2)
      IF(ISAME .EQ. 1) GO TO  50
C
C  INSTEAD OF ENDSU, WE FIND OTHER SU W/O ().
C
      IF (LLPAR .GT. 0 .AND. LRPAR .GT. 0) GO TO 30
           NUMWD = (LEN -1)/4 + 1
           IDEST = IKEY26(CHAR,NUMWD,SUID,LSUID,NSUID,NDSUID)
           IF (IDEST .EQ. 0) GO TO 10
                IERR = 99
                GO TO 50
C
C  INSTEAD OF ENDSU, WE FIND OTHER SU WITH ().
C
   30 CALL IDWP26(TNAME,NPACK,JNAME,INTVAL,IERID)
      IF (JNAME .EQ. 0) GO TO 10
           IERR = 99
C
   50 LENDSU = NCARD
C
C  ENDSU CARD OR OTHER SU FOUND AT LENDSU,
C  ALSO ERR RECOVRY IF THERE'S NO ENDSU FOUND.
C
      RETURN
C
C--------------------------------------------------------------
C  ERROR IN UFLD26
C
 9000 CONTINUE
      IF (IERF.EQ.1) CALL STER26(19,1)
      IF (IERF.EQ.2) CALL STER26(20,1)
      IF (IERF.EQ.3) CALL STER26(21,1)
      IF (IERF.EQ.4) CALL STER26( 1,1)
C
      IF (NCARD.LT.LASTCD) GO TO 20
      USEDUP = .TRUE.
C
      RETURN
      END
