      SUBROUTINE BOUNDARY(IPOS,LOCN)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/grib2unpacker/RCS/boundary.f,v $
     . $',                                                             '
     .$Id: boundary.f,v 1.1 2004/09/16 16:51:50 dsa Exp $
     . $' /
C    ===================================================================
C
C
C        AUGUST 2000   LAWRENCE    GSC/TDL    ORIGINAL CODING
C
C        PURPOSE
C           THIS ROUTINE DETERMINES IF IPOS AND LOCN POINT TO 
C           A BYTE BOUNDARY. IF THEY DO NOT, THEN THEIR VALUES
C           ARE INCREMENTED SO THAT THEY DO POINT TO A BYTE
C           BOUNDARY.
C
C        DATA SET USE
C           NONE
C
C        VARIABLES
C                IPOS = INDICATES THE BIT POSITION IN A 32-BIT
C                       WORD. (INPUT/OUTPUT)
C                LOCN = INDICATES THE BYTE POSITION IN A 32-BIT
C                       WORD.  (INPUT/OUTPUT)
C
C             LOCAL VARIABLES
C               IFILL = THE NUMBER OF BITS THAT MUST BE ADDED TO
C                       IPOS SO THAT IT POINTS TO A NEW BYTE
C                       BOUNDARY. 
C
C        NON SYSTEM SUBROUTINES CALLED
C           NONE
C
C        MAKE SURE THAT WE ARE POINTING TO A BYTE BOUNDARY.
      IFILL=MOD(33-IPOS,8)
C
C        IF IPOS IS NOT AT THE BEGINNING OF A BYTE (THAT IS
C        IF IT DOES NOT HAVE A VALUE OF 1, 9, 17, OR 25),
C        THEN ADD THE NUMBER OF BITS NECESSARY TO MAKE IT POINT
C        TO A BYTE BOUNDARY.
      IF (IFILL.NE.0) THEN
         IPOS=IPOS+IFILL
C
C           IF THE NEW VALUE FOR IPOS EXCEEDS A WORD BOUNDARY,
C           THEN SET IPOS TO 1 AND INCREMENT THE WORD POINTER
C           LOCN.
         IF(IPOS.GT.32) THEN
           IPOS=1
           LOCN=LOCN+1
         END IF
C
      END IF
C
      RETURN
      END
