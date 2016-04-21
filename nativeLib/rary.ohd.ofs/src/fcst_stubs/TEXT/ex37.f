C MEMBER EX37
C  (from old member FCEX37)
       SUBROUTINE EX37(PO,PX,RM,RO,WE,API,AI,AIADJ,AEI,AESC)
C      DUMMY EXECUTION ROUTINE FOR LIST-MSP
C      WRITTEN BY ERIC ANDERSON-HRL DEC 1987
       COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_stubs/RCS/ex37.f,v $
     . $',                                                             '
     .$Id: ex37.f,v 1.1 1995/09/17 18:57:09 dws Exp $
     . $' /
C    ===================================================================
C
       IF(ITRACE.GE.1) WRITE(IODBUG,900)
900    FORMAT(1H0,22H** ENTER AND EXIT EX37)
       RETURN
       END
