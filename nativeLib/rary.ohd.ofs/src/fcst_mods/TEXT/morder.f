C MEMBER MORDER
C  (from old member MCSHFT)
C
C @PROCESS LVL(77)
C
C                             LAST UPDATE: 07/06/95.15:13:41 BY $WC21DT
C
      SUBROUTINE MORDER(IKEY)
C
      INCLUDE 'common/modrcs'      
      LOGICAL DONE
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_mods/RCS/morder.f,v $
     . $',                                                             '
     .$Id: morder.f,v 1.1 1995/11/14 21:25:21 erb Exp $
     . $' /
C    ===================================================================
C
      DONE=.TRUE.
10    IF (DONE) THEN
         DONE=.FALSE.
         DO 20 I=1,NSHIFT(IKEY)-1
         IF (IJHSHF(I,IKEY).GT.IJHSHF(I+1,IKEY)) THEN
            DONE=.TRUE.
            IJTEMP=IJHSHF(I,IKEY)
            LJTEMP=LJHSHF(I,IKEY)
            ISTEMP=ISTYPE(I,IKEY)
            HNTEMP=HNEW(I,IKEY)
            QNTEMP=QNEW(I,IKEY)
            HLTEMP=HL(I,IKEY)
            HUTEMP=HU(I,IKEY)
            IJHSHF(I,IKEY)=IJHSHF(I+1,IKEY)
            LJHSHF(I,IKEY)=LJHSHF(I+1,IKEY)
            ISTYPE(I,IKEY)=ISTYPE(I+1,IKEY)
            HNEW(I,IKEY)=HNEW(I+1,IKEY)
            QNEW(I,IKEY)=QNEW(I+1,IKEY)
            HL(I,IKEY)=HL(I+1,IKEY)
            HU(I,IKEY)=HU(I+1,IKEY)
C
            IJHSHF(I+1,IKEY)=IJTEMP
            LJHSHF(I+1,IKEY)=LJTEMP
            ISTYPE(I+1,IKEY)=ISTEMP
            HNEW(I+1,IKEY)=HNTEMP
            QNEW(I+1,IKEY)=QNTEMP
            HL(I+1,IKEY)=HLTEMP
            HU(I+1,IKEY)=HUTEMP
            ENDIF
20       CONTINUE
         GOTO 10
         ENDIF
         END
