C MEMBER CTRN20
C  (from old member FCPIN20)
C
      SUBROUTINE CTRN20(IDA,IDTA,IDB,IDTB,ITSA,ITSB,IPO)
C ..........................................................
C
C     THIS SUBROUTINE GOES WITH MODULE FCPIN20
C     IT IS FOR TRANSFERRING CHARACTER VALUES INTO THE PO ARRAY
C     SUBROUTINE ORIGINALLY WRITTEN BY
C        ED VANBLARGAN - HRL   APRIL,1981
C ..........................................................
      DIMENSION IDA(1),IDB(1),IPO(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_pntb/RCS/ctrn20.f,v $
     . $',                                                             '
     .$Id: ctrn20.f,v 1.1 1995/09/17 18:47:55 dws Exp $
     . $' /
C    ===================================================================
C
      IPO(1)=IDA(1)
      IPO(2)=IDA(2)
      IPO(3)=IDTA
C ....SKIP IPO(4) SINCE ITA IS IN ALREADY
      IPO(5)=IDB(1)
      IPO(6)=IDB(2)
      IPO(7)=IDTB
C
      IPO(12)=ITSA
      IPO(13)=ITSB
      RETURN
      END
