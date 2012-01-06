C  MODULE MISTDT
C
C  DESC THIS FUNCTION SETS THE JULIAN HOUR ENTERED TO THE
C  DESC NEAREST TIME INTERVAL (ALSO ENTERED)
C
C  THIS FUNCTION IS USED WITHIN THE MODS SUBROUTINES
C  ORIGINALLY WRITTEN BY GEORGE F. SMITH - HRL - AUG 1985
C
      INTEGER FUNCTION MISTDT(IDATE,IDT)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_mods/RCS/mistdt.f,v $
     . $',                                                             '
     .$Id: mistdt.f,v 1.1 1995/09/17 19:03:53 dws Exp $
     . $' /
C    ===================================================================
C
C
      JSTHR=IDATE
      ITMPHR=MOD(JSTHR,24)
      IF(ITMPHR.LT.1)GO TO 10
      IHOUR=(ITMPHR/IDT)*IDT
      LEFTOV=ITMPHR-IHOUR
      IF(LEFTOV.EQ.0)GO TO 10
C
C  HOUR ENTERED NOT EXACTLY ON IDT - MOVE TO CLOSEST DT
C
      IADDHR=IHOUR
      IF(LEFTOV.GE.IDT)IADDHR=IHOUR+IDT
      JSTHR=(JSTHR/24)*24 + IADDHR
C
   10 MISTDT=JSTHR
      RETURN
      END
