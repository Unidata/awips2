C MEMBER FRCRDC
C  (from old member FCFRCRDC)
C***********************************************************************
C
      SUBROUTINE FRCRDC(IBUG,IFOUND)
C
C SUBROUTINE TO READ A CARD FOR DEFRC. WILL ALSO ANALYZE THE FREE
C   FORMAT FIELDS (BY CALLING UFREE) AND UPDATE POINTER, NCD.
C ORIGINALLY BY   ED VANBLARGAN - HRL - JULY 1983
C
C ARGUMENT LIST:
C IBUG  - INPUT  - DEBUG PRINT CODE  0=NONE  1=PRINT DEBUG
C IFOUND- OUTPUT - STATUS CODE FROM SUBROUTINE FRCID
C                    =0, NOT AN ID OR END CARD
C                    =1, THIS IS AN 'ID'
C                    =2, THIS IS AN 'END' CARD
C
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'ufreei'
      INCLUDE 'common/frcers'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_top/RCS/frcrdc.f,v $
     . $',                                                             '
     .$Id: frcrdc.f,v 1.1 1995/09/17 18:55:04 dws Exp $
     . $' /
C    ===================================================================
C
C
      READ(IN,8002,END=900) IBUF
      CALL UFREE(1,72)
      NCD=NCD+1
      CALL FRCID(1,IFOUND)
C
C...............DEBUG TIME..............................................
      IF(IBUG.GT.0) WRITE(IODBUG,8905) NCD,NCIS,NFIELD,IBUF,IFSTRT,
     *IFSTOP,IFTYPE
C.......................................................................
C
      GO TO 999
C EOF
900   IFOUND=2
      WRITE(IPR,8100)
      IERSUM=IERSUM+1
      CALL ERROR
C
999   RETURN
C FORMATS
8002  FORMAT(80A1)
8905  FORMAT(1H0,25HNCD,NCIS,NFIELD,IBUF(80)=,3I3,1X,80A1
     * / 1X,11HIFSTRT(40)=,40I3 / 1X,11HIFSTOP(40)=,40I3
     * / 1X,11HIFTYPE(40)=,40I3)
8100  FORMAT(1H0,10X,33H**ERROR** END-OF-FILE ENCOUNTERED,
     $ 21H WITHOUT AN END CARD.)
C
      END
