C MODULE SBLETR
C---------------------------------------------------------------------
C
      SUBROUTINE SBLETR (IPTYPE,ICOL,NUM,IPLT,LLP,ISTAT)
C
C  THIS ROUTINE FILLS THE PLOT ARRAY WITH THE APPROPRIATE SYMBOL
C  FOR PRINTING BASIN BOUNDARY PLOTS.
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
      CHARACTER*132 IPLT
      CHARACTER*1 LTR(26)
     *   /'A','B','C','D','E','F','G','H','I','J','K','L','M',
     *    'N','O','P','Q','R','S','T','U','V','W','X','Y','Z'/
      CHARACTER*1 NMBR(10)
     *   /'0','1','2','3','4','5','6','7','8','9'/
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_define/RCS/sbletr.f,v $
     . $',                                                             '
     .$Id: sbletr.f,v 1.2 1999/07/06 11:42:18 page Exp $
     . $' /
C    ===================================================================      
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SBLETR'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
      NUMWRN=0
C
      IBASE=10
      IF (IPTYPE.EQ.1) IBASE=26
      ILP=LLP
      IF (IPTYPE.EQ.1.AND.NUM.GT.IBASE) ILP=LLP+1
      IF (IPTYPE.EQ.2.AND.NUM.GE.IBASE) ILP=LLP+1
C
      IF (LLP.LT.1.OR.ILP.GT.ICOL) THEN
         WRITE (LP,70)
         CALL SUWRNS (LP,2,NUMWRN)
         IF (LLP.LT.1) LLP=1
         IF (LLP.GT.ICOL) LLP=ICOL
         IF (LLP.EQ.ICOL.AND.LLP.LT.ILP) LLP=LLP-1
         ENDIF
C
      IF (NUM.LT.IBASE) GO TO 40
         IF (NUM.EQ.IBASE.AND.IPTYPE.EQ.1) GO TO 40
         IF (NUM.EQ.NUM/IBASE*IBASE.AND.IPTYPE.EQ.2) GO TO 20
            IL=(NUM-1)/IBASE
            JL=NUM-(IL*IBASE)
            IPLT(LLP:LLP)=LTR(IL)
            GO TO 30
20       IL=NUM/IBASE
         JL=0
30       IF (IPTYPE.EQ.2) IPLT(LLP:LLP)=NMBR(IL+1)
         IPLT(LLP+1:LLP+1)=LTR(JL)
         IF (IPTYPE.EQ.2) IPLT(LLP+1:LLP+1)=NMBR(JL+1)
         GO TO 50
40    IPLT(LLP:LLP)=LTR(NUM)
      IF (IPTYPE.EQ.2) IPLT(LLP:LLP)=NMBR(NUM+1)
C
50    IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SBLETR'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
70    FORMAT ('0*** WARNING - THE FOLLOWING ROW CONTAINS AT LEAST ',
     *   'ONE POINT WHICH DOES NOT PLOT ON THE PAGE.')
C
      END
