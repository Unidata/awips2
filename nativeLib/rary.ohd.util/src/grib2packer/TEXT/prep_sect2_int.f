      SUBROUTINE PREP_SECT2_INT(JS2,NVAL,IB,ID,MINA)
C
C        MAY     2000   LAWRENCE   GSC/TDL   ORIGINAL CODING
C        JANUARY 2001   GLAHN      REMOVED MAXA; COMMENTS
C        JANUARY 2001   GLAHN/LAWRENCE CHANGED 10**IB TO 2**IB;
C                                  CHANGED JS2(K)*JE TO JS2(K)/JE
C        JANAURY 2001   LAWRENCE   CHANGED JE=2**IB TO JE=2**(-IB)
C                                  AND NOW MULTIPLY THE VALUES IN
C                                  JS2( ) BY JE INSTEAD OF DIVIDING
C                                  BY IT.
C        NOVEMBER 2001   GLAHN     COMMENT ABOUT IB 
C
C        PURPOSE
C            PREPARES THE LOCAL DATA TO BE
C            PACKED USING THE SIMPLE PACKING METHOD.  THE
C            PREPARATION OF THE DATA ENCOMPASSES MULTIPLYING
C            ALL OF THE DATA ELEMENTS IN THE FIELD BY
C            THE DECIMAL SCALING FACTOR, REMOVING THE MINIMUM
C            VALUE, AND DIVIDING ALL OF THE DATA ELEMENTS
C            IN THE FIELD BY THE BINARY SCALING FACTOR.
C
C        DATA SET USE
C           NONE
C
C        VARIABLES
C              JS2(K) = CONTAINS THE LOCAL DATA TO BE PACKED USING
C                       THE SIMPLE PACKING METHOD IN THE LOCAL
C                       USE SECTION OF THE GRIB2 MESSAGE (K=1,NVAL).
C                       (INPUT)
C                NVAL = THE NUMBER OF VALUES IN JS2( ).  (INPUT)
C                  IB = THE BINARY SCALING FACTOR.  THIS IS
C                       0 IN THE CALLING PROGRAM PK_SECT2, AND
C                       HAS NO EFFECT ON THE RESULT.  (INPUT)
C                  ID = THE DECIMAL SCALING FACTOR.  (INPUT)
C                MINA = THE FIELD MINIMUM VALUE.  (OUTPUT)
C            
C             LOCAL VARIABLES
C                  JD = THE DECIMAL MULTIPLICATION FACTOR, 10**ID.
C                  JE = THE BINARY MULTIPLICATION FACTOR, 2**IE.
C                   K = LOOP INDEXING VARIABLE.
C
C        NON SYSTEM SUBROUTINES CALLED
C           NONE
C
      DIMENSION JS2(NVAL)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/grib2packer/RCS/prep_sect2_int.f,v $
     . $',                                                             '
     .$Id: prep_sect2_int.f,v 1.1 2004/09/16 16:52:29 dsa Exp $
     . $' /
C    ===================================================================
C
C
C     SCALE THE DATA FIELD BY THE DECIMAL SCALING FACTOR.
C
      IF(ID.NE.0)THEN
         JD=10**ID
C
         DO 10 K=1,NVAL
            JS2(K)=JS2(K)*JD
 10      CONTINUE
C
      ENDIF
C
C        FIND THE MINIMUM OF THE DATA FIELD.
      MINA=JS2(1)
C
      DO 20 K=2,NVAL
C
         IF(JS2(K).LT.MINA)THEN
            MINA=JS2(K)
         ENDIF
C
 20   CONTINUE
C
C        SUBTRACT OUT THE MINIMUM FROM THE DATA
C        FIELD.
C
      DO 30 K=1,NVAL
         JS2(K)=JS2(K)-MINA
 30   CONTINUE
C
C        MULTIPLY THE DATA FIELD BY THE BINARY
C        SCALING FACTOR.
C
      IF(IB.NE.0)THEN
         JE=2**(-IB)
C
         DO 40 K=1,NVAL
            JS2(K)=JS2(K)*JE
 40      CONTINUE
C
      ENDIF
C
      RETURN
      END
