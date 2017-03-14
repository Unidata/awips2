C MEMBER COX34
C  (from old member FCCOX34)
C
C  VERSION 1
C
C***********************************************************************
C
C THIS SUBROUTINE TRANSFERS OLD VALUES TO THE NEW CO ARRAY WHEN A
C SEGMENT IS REDEFINED
C THE ONLY MODIFICATION IS MADE TO API IF APILMT HAS BEEN CHANGED
C TO A VALUE ABOVE THE CURRENT API
C
C***********************************************************************
C
C SUBROUTINE INITIALLY WRITTEN BY
C     DON LAURINE - CBRFC     MARCH 1985
C
C***********************************************************************
C
C PRINCIPAL VARIABLES
C
C FOR DEFINITION OF VARIABLES IN COMMON BLOCKS, SEE SECTION IX.3.3C
C OF THE NWSRFS USER'S MANUAL
C
C     API           API VALUE
C     APILMT        API LOWER LIMIT
C     COLD          OLD CARRYOVER VALUES
C     CONEW         NEW CARRYOVER VALUES
C     IFDEB         DEBUG OUTPUT FLAG - 0=NO OUTPUT 1=OUTPUT
C     IVERS         VERSION OF OPERATION
C     NUMCO         NUMBER OF VALUES IN CARRYOVER ARRAY
C     POLD          OLD PARAMETER VALUES
C     PONEW         NEW PARAMETER VALUES
C     SUBNAM        SUBROUTINE NAME
C
C***********************************************************************
C
      SUBROUTINE COX34(POLD,COLD,PONEW,CONEW)
C
      DIMENSION POLD(1),COLD(1),PONEW(1),CONEW(1),SUBNAM(2)
C
      COMMON /FDBUG/ IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_cox/RCS/cox34.f,v $
     . $',                                                             '
     .$Id: cox34.f,v 1.1 1995/09/17 18:47:20 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA SUBNAM/4hCOX3,4h4   /,NOP/34/
C
C CALL DEBUG CHECK ROUTINE
C
      CALL FPRBUG(SUBNAM,1,NOP,IFDEB)
C
C INIT NUMBER OF WORDS IN CARRYOVER ARRAY
C
      NUMCO=4
C
C
C SET VERSION NUMBER
C
      IVERS=1
C
C TRANSFER CO VALUES
C
      DO 1000 I=1,NUMCO
      CONEW(I)=COLD(I)
1000  CONTINUE
C
C CHECK TO SEE IF NEW API CARRY NEED CHANGE
C
      API=CONEW(1)
      APILMT=PONEW(9)
      IF(API.GE.APILMT) GO TO 100
      CONEW(1)=APILMT
C
C CHECK FOR DEBUG OUTPUT
C
100   IF(IFDEB.GT.0) GO TO 200
C
999   RETURN
C
C-----------------------------------------------------------------------
C DEBUG STATEMENTS
C
200   WRITE(IODBUG,900) SUBNAM,IVERS
      IF(API.LT.APILMT) GO TO 205
      WRITE(IODBUG,902)
      GO TO 999
205   WRITE(IODBUG,904) COLD(1),CONEW(1)
      GO TO 999
C
C-----------------------------------------------------------------------
C I/O FORMATS
C
900   FORMAT(/5X,2A4,' DEBUG OUTPUT.',5X,'VERSION:',I4)
902   FORMAT(/10X,'NO CARRYOVER UPDATE')
904   FORMAT(/10X,'API OF (',F6.2,') UPDATED TO (',F6.2,')')
C
      END
