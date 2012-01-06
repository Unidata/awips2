C MODULE FCDMP3
C-----------------------------------------------------------------------
C
C  PRINT CONTENTS OF COMMON BLOCK FCFGS.
C
      SUBROUTINE FCDMP3
C
C  ROUTINE ORIGINALLY WRITTEN BY -- ED JOHNSON -- HRL -- 11/1979
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fcfgs'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_top/RCS/fcdmp3.f,v $
     . $',                                                             '
     .$Id: fcdmp3.f,v 1.2 2000/03/14 11:54:25 page Exp $
     . $' /
C    ===================================================================
C
      DATA BLANK/4h    /
C
C
      IF (ISPEC.EQ.1) THEN
         WRITE (IPR,10) 'SPECIAL',FGID
10    FORMAT ('0',2X,'DESCRIPTION OF ',A,' FORECAST GROUP ',2A4,':')
            ELSE
               WRITE (IPR,10) 'NORMAL',FGID
            ENDIF
C
      WRITE (IPR,20) DESCR
20    FORMAT ('0',5X,5A4)
C
      WRITE (IPR,30) ICRDTF(1),ICRDTF(2),ICRDTF(3),
     *  ICRDTF(4),ICRDTF(5)
30    FORMAT ('0',5X,'CREATED ',I2.2,'/',I2.2,'/',I4.4,
     *   '-',I4.4,'.',I4.4)
C
      IF (CGIDF(1).EQ.BLANK.AND.CGIDF(2).EQ.BLANK) THEN
         WRITE (IPR,40)
40    FORMAT ('0',5X,'DOES NOT BELONG TO A CARRYOVER GROUP')
         ELSE
            WRITE (IPR,50) ICOSEQ,CGIDF
50    FORMAT ('0',5X,'IS EXECUTED ',I4,' IN CARRYOVER GROUP ',2A4)
         ENDIF
C
      WRITE (IPR,60) MINDTF
60    FORMAT ('0',5X,'CAN BE EXECUTED ON A ',I2,' HOUR TIME STEP')
C
      WRITE (IPR,70) NSEG,IREC
70    FORMAT ('0',5X,'CONTAINS ',I5,' SEGMENTS - ',
     *   'LIST STARTS AT RECORD 'I6,' OF FILE FCFGLIST')
C
      RETURN
C
      END
