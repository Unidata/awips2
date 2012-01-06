C MODULE SNSOR2
C-----------------------------------------------------------------------
C
C  ROUTINE TO SWITCH POSITIONS OF ENTRIES IN NETWORK ARRAYS.
C
      SUBROUTINE SNSOR2 (ISORT,I)
C
      DIMENSION STDSNW(1),TEMP(5)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sntwkx'
C
      EQUIVALENCE (STDSNW(1),STIDNW(1,1))
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_network/RCS/snsor2.f,v $
     . $',                                                             '
     .$Id: snsor2.f,v 1.2 1998/07/06 12:18:46 page Exp $
     . $' /
C    ===================================================================
C
C
C
      ITEMP1=PP24NW(I+1)
      PP24NW(I+1)=PP24NW(I)
      PP24NW(I)=ITEMP1
C
      ITEMP1=PPVRNW(I+1)
      PPVRNW(I+1)=PPVRNW(I)
      PPVRNW(I)=ITEMP1
C
      ITEMP1=TA24NW(I+1)
      TA24NW(I+1)=TA24NW(I)
      TA24NW(I)=ITEMP1
C
      ITEMP1=EA24NW(I+1)
      EA24NW(I+1)=EA24NW(I)
      EA24NW(I)=ITEMP1
C
      ITEMP1=STATNW(I+1)
      STATNW(I+1)=STATNW(I)
      STATNW(I)=ITEMP1
C
      IF (ISORT.EQ.2) GO TO 10
         TEMP1=STIDNW(1,I+1)
         TEMP2=STIDNW(2,I+1)
         STIDNW(1,I+1)=STIDNW(1,I)
         STIDNW(2,I+1)=STIDNW(2,I)
         STIDNW(1,I)=TEMP1
         STIDNW(2,I)=TEMP2
         GO TO 20
C
10    IPOS1=(I-1)*5+1
      IPOS2=I*5+1
      DO 11 N=1,5
         TEMP(N)=STDSNW(IPOS2+N-1)
11       CONTINUE
      DO 12 N=1,5
         STDSNW(IPOS2+N-1)=STDSNW(IPOS1+N-1)
12       CONTINUE
      DO 13 N=1,5
         STDSNW(IPOS1+N-1)=TEMP(N)
13       CONTINUE         
      GO TO 30
C
20    ITEMP1=CORDNW(1,I+1)
      CORDNW(1,I+1)=CORDNW(1,I)
      CORDNW(1,I)=ITEMP1
      ITEMP1=CORDNW(2,I+1)
      CORDNW(2,I+1)=CORDNW(2,I)
      CORDNW(2,I)=ITEMP1
C
30    ITEMP1=SFLGNW(I+1)
      SFLGNW(I+1)=SFLGNW(I)
      SFLGNW(I)=ITEMP1
C
      ITEMP1=PCHRNW(I+1)
      PCHRNW(I+1)=PCHRNW(I)
      PCHRNW(I)=ITEMP1
C
      ITEMP1=TF24NW(I+1)
      TF24NW(I+1)=TF24NW(I)
      TF24NW(I)=ITEMP1
C
      ITEMP1=ELEVNW(I+1)
      ELEVNW(I+1)=ELEVNW(I)
      ELEVNW(I)=ITEMP1
C
      ITEMP1=GENLNW(I+1)
      GENLNW(I+1)=GENLNW(I)
      GENLNW(I)=ITEMP1
C
      ITEMP1=GPANW(I+1)
      GPANW(I+1)=GPANW(I)
      GPANW(I)=ITEMP1
C
      RETURN
C
      END
