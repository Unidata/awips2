C MEMBER UFDSTR
C (from old member UTIL3)
C----------------------------------------------------------------------
C
       SUBROUTINE UFDSTR (IASTRN,IAP,IUPSTA,MAXA,NUMWA,NUMCA,
     *    IBSTRN,IBP,IUPSTB,MAXB,NUMB,INDEX,ISTAT)
C
C          SUBROUTINE:  UFDSTR
C
C             VERSION:  1.0.0
C
C                DATE:  12-30-81
C
C              AUTHOR:  JIM ERLANDSON
C                       DATA SCIENCES INC
C                       8555 16TH ST, SILVER SPRING, MD 587-3700
C***********************************************************************
C
C          DESCRIPTION:
C
C    SUBROUTINE SEARCHES FOR A STRING IN ANOTHER STRING
C    RETURNS POSITION FOUND (BYTE POSITION IN PACKED STRING) OR 0
C    EITHER STRING CAN BE PACKED OR UNPACKED
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       IASTRN     I     I     ?    INPUT STRING TO SEARCH FOR
C       IAP        I     I     1    PACKED INDICATOR FOR IASTRN
C                                   0=NOT PACKED 1=PACKED
C       IUPSTA     I     O     ?    ARRAY TO HOLD UNPACKED IASTRN
C       MAXA       I     I     1    DIMENSION OF IUPSTA
C       NUMWA      I     I     1    NUMBER OF PACKED WORDS IN IASTRN
C       NUMCA      I     I     1    NUMBER OF CARACTERS IN IASTRN
C       IBSTRN     I     I     ?    ARRAY CONTAINING STRING TO SEARCH
C       IBP        I     I     1    PACKED INDICATOR FOR IBSTRN
C       IUPSTB     I     O     ?    ARRAY TO HOLD UNPACKED IBSTRN
C       MAXB       I     I     1    DIMENSION OF IUPSTB
C       NUMB       I     I     1    NUMBER OF PACKED WORDS IN IBSRTN
C       INDEX      I     O     1    POINTER TO STRING OR 0
C       ISTAT      I     0     1    STATUS INDICATOR
C                                    0=OK 1=BUFFER TOO SMALL
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udsi'
      INCLUDE 'udatas'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      INTEGER IASTRN(1),IBSTRN(1),IUPSTA(1),IUPSTB(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen3/RCS/ufdstr.f,v $
     . $',                                                             '
     .$Id: ufdstr.f,v 1.1 1995/09/17 19:03:42 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
C
C***********************************************************************
C
C
      INDEX=0
C
C  BLANK OUT THE STRINGS
      CALL UMEMST (IBLNK,IUPSTA,MAXA)
      CALL UMEMST (IBLNK,IUPSTB,MAXB)
C
C  UNPACK THE SEARCH STRING
      IF (IAP.EQ.0) GO TO 10
      CALL UNPAKS (IASTRN,IUPSTA,NUMWA,MAXA,ISTAT)
      IF (ISTAT.NE.0) GO TO 80
      GO TO 20
C
10    CALL UMEMOV (IASTRN,IUPSTA,NUMWA)
C
C  UNPACK THE FIND STRING
20    IF (IBP.EQ.0) GO TO 30
      CALL UNPAKS (IBSTRN,IUPSTB,NUMB,MAXB,ISTAT)
      IF (ISTAT.NE.0) GO TO 80
      GO TO 40
C
30    CONTINUE
      CALL UMEMOV (IBSTRN,IUPSTB,NUMB)
C
C  SEARCH THE STRING
40    J=1
      DO 60 I=1,MAXB
         IF (IUPSTA(J).EQ.IUPSTB(I)) GO TO 50
            J=0
50       J=J+1
         IF (J.GT.NUMCA) GO TO 70
60       CONTINUE
C
C  STRING NOT FOUND
      GO TO 80
C
C  FOUND IT
70    INDEX=I-NUMCA+1
C
80    IF (NOBUG.EQ.3) WRITE (LPD,90) INDEX,ISTAT
90    FORMAT (' *** EXIT UFDSTR-INDEX=',I2,3X,'ISTAT=',I2)
C
      RETURN
C
      END
