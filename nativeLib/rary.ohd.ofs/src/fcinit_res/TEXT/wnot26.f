C MODULE WNOT26
C-----------------------------------------------------------------------
C
C PRINT OUT WARNING MESSAGES OCCURRING IN PIN26
C
      SUBROUTINE WNOT26
C
C  WARNINGS THAT HAVE OCCURRED ARE HELD IN /WARN26/.  INFO HELD THERE
C  INCLUDES THE NUMBER OF THE WARNING, THE LINE NUMBER IT OCCURRED ON,
C  THE FIELD NUMBER WITHIN THE LINE IT OCCURRED ON, AND THE CHARACTER
C  WITHIN THE STRING WHERE THE WARNING STARTED.
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/warn26'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_res/RCS/wnot26.f,v $
     . $',                                                             '
     .$Id: wnot26.f,v 1.2 2001/06/13 10:13:23 mgm Exp $
     . $' /
C    ===================================================================
C
C
      IF (NUMWRN.EQ.0) GO TO 30
C
      DO 20 I=1,NUMWRN
         WRITE (IPR,10)IWNTYP(I),IWNLN(I),IWNFLD(I),IWNPOS(I)
10    FORMAT ('0**WARNING** WARNING NUMBER ',I4,
     *        ' OCCURRED ON LINE ',I3,
     *        ', FIELD NUMBER ',I3,
     *        ',CHARACTER NUMBER ',I3,
     *        '.')
20       CONTINUE
C
30    RETURN
C
      END
