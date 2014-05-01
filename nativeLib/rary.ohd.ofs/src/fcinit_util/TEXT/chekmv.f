C     MODULE CHEKMV
C     -----------------------------------------------------------------
      SUBROUTINE CHEKMV(TSCODE,MVINDX,INDEX)
C
C     ORIGINALLY CREATED AUG 2007
C        DARRIN SHARP, RIVERSIDE TECHNOLOGY
C
C     THIS SUBROUTINE VALIDATES/CHECKS MULTIVALUE (MV) 
C     TIME SERIES (TS) AND DATA TYPES AGAINST EACH OTHER.
C     IT WILL CHECK AND MAKE SURE A MV TS HAS AN APPROPRIATE
C     MV INDEX; THE TS WILL ALSO BE VALIDATED AGAINST MV TYPES.
C
C     IF A TS TYPE/MV DATA TYPE PAIR IS INVALID, A ZERO
C     INDEX IS RETURNED; IF THE TS IS NOT DEFINED TO HAVE
C     A MV DATA TYPE, AN INDEX OF -999 IS RETURNED.
C
C     INPUT:
C        TSCODE,MVINDX
C
C        TIME SERIES CODE
C        MV INDEX, I.E. INDEX INTO THE MV TIME SERIES
C
C     OUTPUT:
C        INDEX
C
C        RETURN INDEX 
C

      CHARACTER * 4 TSCODE
C     THE MVINDEX IS READ IN AS A CHAR STRING. IN ADDITION TO
C     VALIDATING IT, THIS ROUTINE WILL ALSO CONVERT IT TO A
C     INT INDEX (WHICH IS ESSENTIALLY USED AS AN INDEX INTO
C     A MV DATA TYPE ARRAY).

      CHARACTER * 8 MVINDX
      INTEGER INDEX
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source$
     . $',                                                             '
     .$Id$
     . $' /
C    ===================================================================
C

C     IF MVINDX IS NON-BLANK, TS CODE MUST BE SMZC, 
C     ROCL

      IF (TSCODE.EQ.'SMZC') THEN
         IF (MVINDX.EQ.'UZTDEF') THEN
            INDEX=1
         ELSE IF (MVINDX.EQ.'UZFWC') THEN
            INDEX=2
         ELSE IF (MVINDX.EQ.'LZTDEF') THEN
            INDEX=3
         ELSE IF (MVINDX.EQ.'LZFSC') THEN
            INDEX=4
         ELSE IF (MVINDX.EQ.'LZFPC') THEN
            INDEX=5
         ELSE
C           THIS IS AN ERROR
            INDEX=0
         ENDIF
      ELSE IF (TSCODE.EQ.'ROCL') THEN
         IF (MVINDX.EQ.'TCHANINF') THEN
            INDEX=1
         ELSE IF (MVINDX.EQ.'IMP-RO') THEN
            INDEX=2
         ELSE IF (MVINDX.EQ.'DIR-RO') THEN
            INDEX=3
         ELSE IF (MVINDX.EQ.'SUR-RO') THEN
            INDEX=4
         ELSE IF (MVINDX.EQ.'INTERFLO') THEN
            INDEX=5
         ELSE IF (MVINDX.EQ.'SUPBASE') THEN
            INDEX=6
         ELSE IF (MVINDX.EQ.'PRIMBASE') THEN
            INDEX=7
         ELSE
C           THIS IS AN ERROR
            INDEX=0
         ENDIF
      ELSE
C        THIS IS AN ERROR
         INDEX=-999
      ENDIF
      RETURN
      END
