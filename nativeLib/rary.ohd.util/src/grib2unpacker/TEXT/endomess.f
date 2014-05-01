      LOGICAL FUNCTION ENDOMESS(LNUM,L3264B)
C
C        MARCH   2000   LAWRENCE    GSC/TDL    ORIGINAL CODING
C        JANUARY 2001   GLAHN       COMMENTS
C
C        PURPOSE
C            TESTS FOR THE END OF MESSAGE CONDITION IN A GRIB2
C            MESSAGE INDICATED BY THE CHARACTERS "7777" STORED IN THE 
C            FOUR BYTES OF SECTION 8.  WHEN AN END OF MESSAGE IS FOUND,
C            ENDOMESS RETURNS .TRUE.; OTHERWISE IT RETURNS .FALSE.
C
C            IF THE INTEGER VALUE IN LNUM IS EQUIVALENT TO
C            THE CHARACTER STRING "7777" THEN THIS FUNCTION
C            WILL RETURN A VALUE OF .TRUE.  OTHERWISE, IT
C            WILL RETURN A VALUE OF .FALSE.
C
C        DATA SET USE
C           NONE 
C
C        VARIABLES
C                LNUM = THE INTEGER VALUE WE ARE GOING TO TEST
C                       TO SEE IF IT IS EQUIVALENT TO THE
C                       CHARACTER STRING "7777".  (INPUT)
C              L3264B = THE INTEGER WORD LENGTH IN BITS OF THE MACHINE
C                       BEING USED. VALUES OF 32 AND 64 ARE
C                       ACCOMMODATED. (INPUT)
C
C             LOCAL VARIABLES
C               C7777 = CONTAINS THE CHARACTER STRING "7777".
C               I7777 = THIS VARIABLE IS EQUIVALENCED TO C7777.
C                       IT CONTAINS THE INTEGER VALUE REPRESENTED
C                       BY THE FOUR BYTES THAT COMPRISE C7777.
C               LNUM1 = TEMPORARILY CONTAINS THE VALUE OF LNUM.
C                       THIS ENSURES THAT THE VALUE OF LNUM
C                       IS NOT ALTERED BY THIS FUNCTION. 
C               
C
C        NON SYSTEM SUBROUTINES CALLED
C           NONE
C
      CHARACTER*4 C7777
C
      EQUIVALENCE (C7777,I7777)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/grib2unpacker/RCS/endomess.f,v $
     . $',                                                             '
     .$Id: endomess.f,v 1.1 2004/09/16 16:51:50 dsa Exp $
     . $' /
C    ===================================================================
C
C
      DATA C7777/'7777'/
C
C        IF THIS IS A MACHINE WITH A 64-BIT WORD,
C        WE NEED TO SHIFT THE CONTENTS OF LNUM
C        4 BYTES TO THE LEFT. HOWEVER, WE DON'T WANT TO 
C        AFFECT THE CONTENTS OF LNUM, SO DO ALL
C        PROCESSING ON LNUM1.
      LNUM1=LNUM
      IF(L3264B.EQ.64)LNUM1=ISHFT(LNUM,32)
C
      IF(LNUM1.EQ.I7777)THEN
         ENDOMESS=.TRUE.
      ELSE
         ENDOMESS=.FALSE.
      ENDIF
C
      RETURN
      END
