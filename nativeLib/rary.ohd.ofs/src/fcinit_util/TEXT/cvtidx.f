C     MODULE CVTIDX
C     -----------------------------------------------------------------
      SUBROUTINE CVTIDX(TSCODE,MVINDX,MVDT)
C
C     ORIGINALLY CREATED AUG 2007
C        DARRIN SHARP, RIVERSIDE TECHNOLOGY

C     GIVEN A TIME SERIES CODE AND MULTIVALUE (MV) INDEX (1-9, NUMBEIC) 
C     THIS SUBROUTINE CONVERTS THE INDEX INTO A MV DATA TYPE (ALPHA).
C
C     INPUT:
C        TSCODE,MVINDX
C   
C        TIME SERIES CODE, MULTIVALUE INDEX
C
C     OUTPUT:
C        MVDT
C
C        MULTIVALUE DATA TYPE (MAY RETURN 'INVALID')

      CHARACTER * 4 TSCODE
      INTEGER MVINDX
      CHARACTER * 8 MVDT 

      INCLUDE 'common/ionum'
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

      IF (TSCODE.EQ.'SMZC') THEN
         IF (MVINDX.EQ.1) THEN
            MVDT='UZTDEF'
         ELSE IF (MVINDX.EQ.2) THEN
            MVDT='UZFWC'
         ELSE IF (MVINDX.EQ.3) THEN
            MVDT='LZTDEF'
         ELSE IF (MVINDX.EQ.4) THEN
            MVDT='LZFSC'
         ELSE IF (MVINDX.EQ.5) THEN
            MVDT='LZFPC'
         ELSE
C           THIS IS AN ERROR
            MVDT='INVALID'
         ENDIF
      ELSE IF (TSCODE.EQ.'ROCL') THEN
         IF (MVINDX.EQ.1) THEN
            MVDT='TCHANINF'
         ELSE IF (MVINDX.EQ.2) THEN
            MVDT='IMP-RO'
         ELSE IF (MVINDX.EQ.3) THEN
            MVDT='DIR-RO'
         ELSE IF (MVINDX.EQ.4) THEN
            MVDT='SUR-RO'
         ELSE IF (MVINDX.EQ.5) THEN
            MVDT='INTERFLO'
         ELSE IF (MVINDX.EQ.6) THEN
            MVDT='SUPBASE'
         ELSE IF (MVINDX.EQ.7) THEN
            MVDT='PRIMBASE'
         ELSE
C           THIS IS AN ERROR
            MVDT='INVALID'
         ENDIF
      ELSE
C     SINCE TSCODE IS NOT ONE OF THE MV ONES,
C     THE MV DATATYPE WILL BE INVALID AS WELL
            MVDT='INVALID'
      ENDIF
      RETURN
      END
