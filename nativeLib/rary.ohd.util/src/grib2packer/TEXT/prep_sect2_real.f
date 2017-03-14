      SUBROUTINE PREP_SECT2_REAL(AS2,JS2,NVAL,IB,ID,RMINA)
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
C              AS2(K) = CONTAINS THE LOCAL DATA TO BE PACKED USING
C                       THE SIMPLE PACKING METHOD IN THE LOCAL USE
C                       SECTION OF THE GRIB2 MESSAGE (K=1,NVAL).
C                       (INPUT)
C              JS2(K) = ONCE THE DATA IN AS2( ) HAS BEEN SCALED AND THE
C                       REFERENCE VALUE HAS BEEN REMOVED, IT IS COPIED
C                       INTO THIS INTEGER ARRAY AND RETURNED TO THE
C                       CALLER OF THIS ROUTINE.  (OUTPUT) 
C                NVAL = THE DIMENSION OF JS2( ) AND AS2( ).  (INPUT)
C                  IB = THE BINARY SCALING FACTOR.  THIS IS
C                       0 IN THE CALLING PROGRAM PK_SECT2, AND
C                       HAS NO EFFECT ON THE RESULT.  (INPUT)
C                  ID = THE DECIMAL SCALING FACTOR.  (INPUT)
C               RMINA = THE FIELD MINIMUM VALUE.  (OUTPUT)
C            
C             LOCAL VARIABLES
C                   D = THE DECIMAL MULTIPLICATION FACTOR, 10.**ID.
C                   E = THE BINARY MULTIPLICATION FACTOR, 2.**IE.
C                   K = LOOP INDEXING VARIABLE.
C
C        NON SYSTEM SUBROUTINES CALLED
C           NONE
C
      DIMENSION AS2(NVAL),JS2(NVAL)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/grib2packer/RCS/prep_sect2_real.f,v $
     . $',                                                             '
     .$Id: prep_sect2_real.f,v 1.1 2004/09/16 16:52:29 dsa Exp $
     . $' /
C    ===================================================================
C
C
C     SCALE THE DATA FIELD BY THE DECIMAL SCALING FACTOR.
C
      IF(ID.NE.0)THEN
         D=10.**(INT(ID))
C
         DO 10 K=1,NVAL
            AS2(K)=AS2(K)*D
 10      CONTINUE
C
      ENDIF
C
C        FIND THE MINIMUM OF THE DATA FIELD.
      RMINA=AS2(1)
C
      DO 20 K=2,NVAL
C
         IF(AS2(K).LT.RMINA)THEN
           RMINA=AS2(K)
         ENDIF
C
 20   CONTINUE
C
C        SUBTRACT OUT THE MINIMUM FROM THE DATA
C        FIELD.
C
      DO 30 K=1,NVAL
         AS2(K)=AS2(K)-RMINA
 30   CONTINUE
C
C        MULTIPLY THE DATA FIELD BY THE BINARY
C        SCALING FACTOR.
C
      IF(IB.NE.0)THEN
         E=2.**(INT(-IB))
C
         DO 40 K=1,NVAL
            AS2(K)=AS2(K)*E
 40      CONTINUE
C
      ENDIF
C
C        COPY THE FLOATING POINT VALUES INTO THE
C        INTEGER OUTPUT ARRAY. 
      DO 50 K=1,NVAL
         JS2(K)=NINT(AS2(K))  
 50   CONTINUE
C
      RETURN
      END
