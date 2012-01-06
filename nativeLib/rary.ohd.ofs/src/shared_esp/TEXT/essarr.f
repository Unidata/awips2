C     MODULE ESSARR
C-----------------------------------------------------------------------
C
C  ROUTINE TO PERFORM THE SSARREG MOD
C
      SUBROUTINE ESSARR(MP,P,NCARDS,modcrd,IIDATE,NXTOPN,NXTNAM,IHZERO,
     1                  ijdlst,idloop,mecards,ierr)
C
      LOGICAL FIRST
      CHARACTER*8 SLASH,BLANK8,MODNAM
      character modcrd(80,ncards), mecards(80,ncards)
C
C
      INCLUDE 'common/fmodft'
      INCLUDE 'common/ionum'
      INCLUDE 'common/fpwarn'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fctime'
      INCLUDE 'common/fctim2'
      INCLUDE 'ufreex'
C
      DIMENSION OLDOPN(2),P(MP)
      INTEGER JTEMP(64),STEMP(200)
      REAL TEMP(200),TEMP1(64,2),STORE(200,2),FINAL(200,2)
      CHARACTER*8 KWRDS(7),KEYWRD,OPNAME,NXTNAM,ONAME
      CHARACTER*4 UNIT1(7),UNIT2(7),ENGUN
      CHARACTER*20 WORD
      CHARACTER*2 UPSTRM,DOSTRM,US,DS
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_esp/RCS/essarr.f,v $
     . $',                                                             '
     .$Id: essarr.f,v 1.5 2003/03/14 18:52:20 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA KWRDS /'FREEFLOW','SETQ    ','SETH    ','SETS    ',
     1            'SETDQ   ','SETDH   ','SETDS   '/,US/'US'/,DS/'DS'/
      DATA UNIT1 /'NONE','CMS ','M   ','CMSD','CMS ','M   ','CMSD'/
      DATA UNIT2 /'NONE','V   ','L   ','V   ','V   ','L   ','V   '/
      DATA MODNAM/'SSARREG '/,NUMKEY/7/,LWORD/20/,MXTEMP/64/
      DATA SLASH/'/       '/,BLANK8/'        '/
C
      CALL FSTWHR(8HESSARR  ,0,OLDOPN,IOLDOP)
C
C
C
C
C
C     READ CARD - IF COMMAND, LEAVE - IF COMMAND AND 1ST CARD, ERROR
C
cew initialize len variable
      len=0
      len_diff=0
      ishift=0
      nrdcrd_old=0

      FIRST=.TRUE.
cew      IDATE=IABS(IIDATE)

      iold=ickdat
C
      IF (NRDCRD.EQ.NCARDS) GOTO 80
C
70    IF (NRDCRD.EQ.NCARDS) GOTO 5000
C
      IF (MISCMD(NCARDS,MECARDS).EQ.0) GOTO 100
C
      IF (.NOT.FIRST) GOTO 5000
C
C     HAVE FOUND COMMAND AS FIRST SUBSEQUENT CARD - ERROR
C
80    IF (MODWRN.EQ.1) THEN
         WRITE (IPR,90)
         CALL WARN
         GOTO 5000
         ENDIF
90    FORMAT(1H0,10X,'**WARNING** NO SUBSEQUENT CARDS FOUND FOR THE ',
     1  'SSARREG MOD.  PROCESSING CONTINUES')
C
100   FIRST=.FALSE.
C
      NRDCRD=NRDCRD+1
C
C  RE(SET) VARIABLE LTEMP TO 7 AND ZEROING OUT ARRAY STORE
C
      CALL UMEMOV(0,JTEMP(1),64)
      CALL UMEMOV(0,STEMP(1),200)
      CALL UMEMOV(0.0,STORE(1,1),400)
      LTEMP=7
C
C     NOW READ 2ND FIELD - CHECKING FOR AN OPTIONAL 'US' ENTRY
C
      NFLD=1
      ISTRT=-3
      NCHAR=2
      KEYWRD=' '
      JDATE=0
      ickdat=0
C
      CALL UFIEL2(NCARDS,MECARDS,NFLD,ISTRT,LEN,ITYPE,NREP,IVALUE,VALUE,
     1  NCHAR,KEYWRD,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,ISTAT)
C
      IF (ISTRT.NE.-2) GOTO 140
C
C     ERROR - DATA EXPECTED - PROCESS NEXT CARD
C
      IF (MODWRN.EQ.1) THEN
         WRITE (IPR,130) (MECARDS(I,NRDCRD),I=1,80)
         CALL WARN
         GOTO 70
         ENDIF
130   FORMAT(1H0,10X,'**WARNING** IN THE SSARREG MOD - NOT ENOUGH ',
     1  'FIELDS ON SUBSEQUENT CARD.  THE CARD BEING PROCESSED IS:'/
     2  11X,80A1)
C
135   FORMAT(1H0,10X,A,' **WARNING** IN THE SSARREG MOD - INVALID ',
     1  'FIELD ENTERED.  THE CARD BEING PROCESSED IS:'/11X,80A1)
C
C     OPTIONAL 'US' ENTRY FOUND
C
140   IUS=0
      IDS=0
      UPSTRM=KEYWRD
      IF (UPSTRM.EQ.US) IUS=1
      GOTO 143
C
C  ----------------------------------------
C  THIS SECTION IS REQUIRED ONLY FOR SEGMENT THAT HAS AN UPSTREAM
C  RESERVOIR.  THEREFORE, A DOWNSTREAM IS REQUIRED.
C
141   CONTINUE
      IUS=0
      ISTRT=-1
      JDATE=0
      KEYWRD=' '
      ickdat=0
C
      CALL UFIEL2(NCARDS,MECARDS,NFLD,ISTRT,LEN,ITYPE,NREP,IVALUE,VALUE,
     1  NCHAR,KEYWRD,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,ISTAT)
C
      IF (ISTAT.NE.0) GOTO 144

      IF (KEYWRD.EQ.'DS') THEN
         IDS=1
         LTEMP=7
         GOTO 143
         ENDIF

144   IF(MODWRN.EQ.1) THEN
        WRITE(IPR,142) (MECARDS(I,NRDCRD),I=1,80)
        CALL WARN
        GOTO 70
        ENDIF
142   FORMAT(1H0,10X,'**WARNING** IN THE SSARREG MOD - A KEYWORD DS ',
     1  'IS REQUIRED FOR A DOWNSTREAM RESERVOIR.  THE CARD IMAGE IS: ',
     2  /,80A1)
C
143   ISTRT=-3
      IF (IDS.EQ.1) GOTO 145
      IF (KEYWRD.NE.US.AND.KEYWRD.NE.DS) ISTRT=-1
C
C  BEGIN DECODING KEYWORD, DELTA-T (IF ANY), DATE AND DATA VALUES
C
C  DECODE KEYWORD
C
145   IKEYWD=0
      IDELTA=0
      ITIME=0
      IDATA=0
      KEYWRD=' '
      ickdat=0
C
      CALL UFIEL2(NCARDS,MECARDS,NFLD,ISTRT,LEN,ITYPE,NREP,IVALUE,VALUE,
     1  NCHAR,KEYWRD,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,ISTAT)
C
      IF (ISTAT.NE.0) GOTO 155
C
      IKEY=-1
      DO 150 I=1,NUMKEY
         IF (KEYWRD.EQ.KWRDS(I)) THEN
            IKEYWD=1
            IKEY=I-1
            GOTO 170
            ENDIF
150   CONTINUE
C
155   IF(MODWRN.EQ.1) THEN
         WRITE(IPR,160) (MECARDS(I,NRDCRD),I=1,80)
         CALL WARN
         GOTO 70
         ENDIF
C
160   FORMAT(1H0,10X,'**WARNING** IN THE SSARREG MOD - AN INVALID ',
     1  'KEYWORD FOUND ON THE FOLLOWING MOD CARD:'/11X,80A1)
C
170   CONTINUE
C
C  DECODE DELTA-T (IF EXISTS)
C
      ISTRT=-3
      WORD=' '
      ickdat=0
C
      CALL UFIEL2(NCARDS,MECARDS,NFLD,ISTRT,LEN,ITYPE,NREP,INTGER,REAL,
     1  LWORD,WORD,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,ISTAT)
C
      IF (ISTRT.NE.-2) GOTO 180
C
      IF (MODWRN.EQ.1) THEN
         WRITE (IPR,130) (MECARDS(I,NRDCRD),I=1,80)
         CALL WARN
         GOTO 70
         ENDIF
C
C  CHECK IF STRING IS INTEGER
180   IF (ITYPE.NE.0) GO TO 185
C
      IF (INTGER.GT.24) GOTO 185
C
      IF (INTGER.GT.0.AND.INTGER.LE.24) THEN
         IF (MOD(IDELTA,24).EQ.0.OR.INTGER.EQ.1.OR.INTGER.EQ.3) THEN
            IDELTA=INTGER
            GOTO 185
            ENDIF
         ENDIF
C
      IF (MODWRN.EQ.1) THEN
         WRITE (IPR,184) (MODCRD(I,NRDCRD),I=1,20)
         CALL WARN
         GOTO 70
         ENDIF
C
184   FORMAT(1H0,10X,A,' **WARNING** IN THE SSARREG MOD - INVALID ',
     1  'TIME STEP ENTERED.  THE CARD BEING PROCESSED IS:'/11X,80A1)
C
185   ISTRT=-3
C
      IF (ITYPE.NE.0.OR.INTGER.GT.24) ISTRT=-1
C
190   CONTINUE
C
C  DECODE ASSOCIATED DATE(S)
C
      WORD=' '
      ickdat=0
      CALL UFIEL2(NCARDS,MECARDS,NFLD,ISTRT,LEN,ITYPE,NREP,INTGER,REAL,
     1  LWORD,WORD,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,ISTAT)
C
      IF (ISTRT.NE.-2) GOTO 200
C
      IF (MODWRN.EQ.1) THEN
         WRITE (IPR,130) (MECARDS(I,NRDCRD),I=1,80)
         CALL WARN
         GOTO 70
         ENDIF
C
200   CONTINUE
C
      IF (ITYPE.EQ.0.OR.ITYPE.EQ.2) GOTO 210
C
      IF (MODWRN.EQ.1) THEN
         WRITE (IPR,135) '1',(MECARDS(I,NRDCRD),I=1,80)
         CALL WARN
         GOTO 70
         ENDIF
C
210   CALL UDATEA (LWORD,WORD,1,0,1,0,JDA,IHR,ITIME,ISTAT)
      IF (ISTAT.EQ.0) THEN
         ITIME=IABS(ITIME)
         GOTO 220
         ENDIF
C
      IF (MODWRN.EQ.1) THEN
         WRITE (IPR,135) '2',(MECARDS(I,NRDCRD),I=1,80)
         CALL WARN
         GOTO 70
         ENDIF


220   CONTINUE

cew   compute difference between the moddate (jda) and 
cew   and the carryover date (ijdlst).  Then new mod date 
cew   is the current starting histroical year (idloop) plus the difference .


       idiff = jda - ijdlst
       jda_new = idiff + idloop

cew convert julian days to m/d/y/h

        call mdyh2(ijdlst,ihr,ilmonth,ilday,ilyear,ilhour,itz,
     +   idsav,modtzc)

        call mdyh2(jda,ihr,ijmonth,ijday,ijyear,ijhour,itz,
     +   idsav,modtzc)

        call mdyh2(jda_new,ihr,idmonth,idday,idyear,idhour,itz,
     +   idsav,modtzc)


cew after figuring out new date then figure out
cew mod crd length with ulenth
cew because the mecrd is the original mod card and that origingal
cew is updated over and over again into the modcrd array must
cew chack length of mecard strings and NOT the modcrd strings
        call ulenth(mecards(1,nrdcrd),80,length)

cew date cards fill up to 12 characters
cew check if this date has filled all 12 spaces
cew mod field is 2+2+2+2+4 m/d/y/h/tzc
        len_diff=12-len

cew must reset the ishift counter to be 0 everytime you shift mod cards
cew must save value of nrdcrd and check if it has been incremented because it
cew gets incremented in ufiel2
        if (nrdcrd.gt.nrdcrd_old) then
          ishift=0
          nrdcrd_old=nrdcrd
        endif

        if(length+ishift+len_diff.gt.80) then
          write(ipr,*)'**ERROR** THE MODCRD WILL BE TOO LONG AFTER IT IS 
     +REWRITTEN WITH COMPLETE DATE FIELDS.  MOD CARD IS:'

         do 5003 j=1,ncards
              write(ipr,5002)(MECARDS(I,j),I=1,80)
5002          format(80a1)
5003     continue

         write(ipr,*) " "

          call error

        endif

cew shift all of the fields after the current field the difference
cew between 11 and the length of the curernt field.

       if(len_diff.gt.0) then

cew do this loop backwards so that you don't overwirte good info
cew       istrt=start of word in the original modcrd array
cew       iend_word=istrt+ishift+len-1=end of date field in the new modcrd array
cew       length = length of whole card in the original mod card
cew       ishifted_len=length+ishift = length of whole card in the new mod card
cew       len = length of field

       iend_word=istrt+ishift+len-1
       ishifted_len=length+ishift

cew    in the new mod crd the  do 212 limits are equal to iend_word+ishift+len_diff+1
cew    to length+len_diff

          do 212 il=ishifted_len,iend_word+1,-1
212           modcrd(il+len_diff,nrdcrd)=modcrd(il,nrdcrd)

         do 214 il=iend_word+1,iend_word+len_diff
214          modcrd(il,nrdcrd)=' '


       endif

       call ufi2a(idmonth,modcrd(istrt+ishift,nrdcrd),1,-2,1,ipr,
     +             istatuf)
       call ufi2a(idday,modcrd(istrt+ishift+2,nrdcrd),1,-2,1,ipr,
     +             istatuf)
       idyear=mod(idyear,100)
       call ufi2a(idyear,modcrd(istrt+ishift+4,nrdcrd),1,-2,1,ipr,
     +             istatuf)
       call ufi2a(idhour,modcrd(istrt+ishift+6,nrdcrd),1,-2,1,ipr,
     +             istatuf)
       call umemov(modtzc,modcrd(istrt+ishift+8,nrdcrd),1)

       ishift=ishift+len_diff
C
C  BEGIN TO DECODE VALUE(S) ASSOCIATED WITH DATES
C
C
      DO 222 L=1,64
         TEMP1(L,1)=0.
         TEMP1(L,2)=0.
222   CONTINUE
C
      NTEMP1=0
      IPASS=0
C
225   CONTINUE
C
      ISTRT=-3
      WORD=' '
      INEW=0
      ISTART=0
      IALERT=0
      IOVER=0
      ickdat=0
C
      CALL UFIEL2(NCARDS,MECARDS,NFLD,ISTRT,LEN,ITYPE,NREP,INTGER,REAL,
     1  LWORD,WORD,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,ISTAT)
C
      IF (ISTRT.EQ.-2.AND.IKEY.EQ.0) GOTO 240
C
      IF (ISTRT.EQ.-2.AND.IPASS.EQ.0) GOTO 227
C
      IF (ITYPE.EQ.1.AND.IKEY.EQ.0) GOTO 225
C
      IF (ITYPE.EQ.1) GOTO 230
C
      IF (ITYPE.EQ.2.AND.WORD.EQ.'DS') GOTO 240
C
      IF (ITYPE.EQ.2) THEN
         DO 223 L=1,LWORD
            IF (WORD(L:L).EQ.'Z') THEN
               ISTRT=-1
               INEW=1
               GOTO 240
               ENDIF
223      CONTINUE
         ENDIF
C
      IF (ITYPE.EQ.2) THEN
         IF (WORD.EQ.'/') IALERT=1
         ISTRT=-1
         ISTART=0
         DO 226 J=1,NUMKEY
            IF (WORD.EQ.KWRDS(J)) THEN
               ISTART=1
               ENDIF
226      CONTINUE
         GOTO 230
         ENDIF
C
      IF (ITYPE.EQ.0.AND.INTGER.GT.24) THEN
         ISTRT=-1
         INEW=1
         GOTO 240
         ENDIF
C
      IF (IPASS.EQ.1.AND.ISTRT.EQ.-2) GOTO 240
C
227   IF (MODWRN.EQ.1) THEN
         WRITE (IPR,135) '3',(MECARDS(I,NRDCRD),I=1,80)
         CALL WARN
         GOTO 70
         ENDIF
C
230   CONTINUE
C
      IF (ITYPE.EQ.2) GOTO 240
C
      IPASS=1
      IDATA=1
      TEMP1(NTEMP1+1,1)=NREP
      AM2IN=1.0
      IF (IUMGEN.EQ.0.OR.IKEY.EQ.0) GOTO 235
      CALL FCONVT(UNIT1(IKEY+1),UNIT2(IKEY+1),ENGUN,AM2IN,BM2IN,IER)
235   TEMP1(NTEMP1+1,2)=REAL*AM2IN
C     JTEMP(NTEMP1+1)=NREP
      STEMP(NTEMP1+1)=NREP
      NTEMP1=NTEMP1+1
C
      GOTO 225
C
240   CONTINUE
C
      IF (IKEY.EQ.0) THEN
         NTEMP1=1
C        JTEMP(NTEMP1)=1
         STEMP(NTEMP1)=1
         TEMP1(NTEMP1,1)=0.
         TEMP1(NTEMP1,2)=0.
         ENDIF
C
      NEW=0
      DO 245 I=1,NTEMP1
C        IF (JTEMP(I).LE.1) NEW=NEW+1
         IF (STEMP(I).LE.1) NEW=NEW+1
C        IF (JTEMP(I).GE.2) NEW=NEW+2
         IF (STEMP(I).GE.2) NEW=NEW+2
245   CONTINUE
C
C
C
      IF (ITIME.LT.IDATE.AND.ITIME.GE.JDATE) THEN
         IF (JDATE.NE.0) IOVER=1
         JDATE=ITIME
         TEMP1(1,1)=1.0
         TEMP1(1,2)=TEMP1(NTEMP1,2)
C        JTEMP(1)=1
         STEMP(1)=1
         NTEMP1=1
         ENDIF
C
      IF (IOVER.EQ.1) LTEMP=LTEMP-3
C
      NWTIME=0
      DO 280 I=1,NTEMP1
         IPOS=0
C        IF (JTEMP(I).LT.2.OR.IDELTA.LE.0) GOTO 285
         IF (STEMP(I).LT.2.OR.IDELTA.LE.0) GOTO 285
            LTEMP=LTEMP+3
            TEMP(LTEMP-2)=ITIME+.01
            IF (NWTIME.GT.0) TEMP(LTEMP-2)=NWTIME+IDELTA
            TEMP(LTEMP-1)=TEMP1(I,2)
            TEMP(LTEMP-0)=IKEY
C
            IPOS=I
C           NWTIME=ITIME+((JTEMP(1)-1)*IDELTA)
            NWTIME=ITIME+((STEMP(1)-1)*IDELTA)
            IF (NTEMP1.LE.1) GOTO 282
C
            DO 281 INC=2,I
C              NWTIME=NWTIME+(JTEMP(INC)*IDELTA)
               NWTIME=NWTIME+(STEMP(INC)*IDELTA)
281         CONTINUE
C
282         LTEMP=LTEMP+3
            TEMP(LTEMP-2)=NWTIME+.01
            TEMP(LTEMP-1)=TEMP1(I,2)
            TEMP(LTEMP-0)=IKEY
            GOTO 280
C
285      LTEMP=LTEMP+3
         TEMP(LTEMP-2)=ITIME+.01
         TEMP(LTEMP-1)=TEMP1(I,2)
         TEMP(LTEMP-0)=IKEY+.01
         TEMP(1)=LTEMP+.01
280   CONTINUE
C
      IF (WORD.EQ.'DS'.AND.IUS.EQ.1) THEN
         CALL UMEMOV(TEMP,STORE(1,1),200)
         GOTO 141
         ENDIF
C
      IF (ITYPE.EQ.2.AND.ISTART.EQ.1) GOTO 145
C
      IF (IALERT.EQ.1) GOTO 500
C
      IF (ISTRT.EQ.-2) GOTO 500
C
      IF (INEW.EQ.1) GOTO 190
C
cxf
cxf   For Bug r22-1
cxf
cxf   ESP can't handle the SARREG MOD file which has
cxf   no SETQ keywod on each of the regulation line.
cxf   ISTRT=1 means that the position of the card is
cxf           losted at the 1st place.
cxf           (originally ISTRT=-1, this is wrong)
cxf   ITYPE=2 means the card is character
cxf   ISTART=0 means the card is not a keyword.
cxf
      IF (ITYPE.EQ.2.AND.ISTART.EQ.0) THEN
       ISTRT=1
      ENDIF
cxf
C
      GOTO 210
C
C  LOOKING FOR A SLASH AND OPERATION NAME
C
500   CONTINUE
C
      IF (IDS.EQ.0) CALL UMEMOV(TEMP,STORE(1,1),200)
      IF (IDS.EQ.1) CALL UMEMOV(TEMP,STORE(1,2),200)
C
      ISTRT=-3
      WORD=' '
      IF (IALERT.EQ.1) ISTRT=-1
C
      ickdat=0
      CALL UFIEL2(NCARDS,MECARDS,NFLD,ISTRT,LEN,ITYPE,NREP,INTGER,REAL,
     1  LWORD,WORD,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,ISTAT)
C
      IF (ISTRT.EQ.-2) GOTO 320
C
      IF (WORD.EQ.'/') GOTO 300
C
      IF (MODWRN.EQ.1) THEN
         WRITE (IPR,290) (MECARDS(I,NRDCRD),I=1,80)
         CALL WARN
         GOTO 5000
         ENDIF
290   FORMAT(1H0,10X,'**WARNING** IN THE SSARREG MOD - A SLASH ',
     1  'IS REQUIRED BEFORE AN OPERATION NAME IS ALLOWED. THE CARD ',
     2  'IMAGE IS: ',/,11X,80A1)
C
300   CONTINUE
C
      ISTRT=-3
      WORD=' '
      ickdat=0
C
      CALL UFIEL2(NCARDS,MECARDS,NFLD,ISTRT,LEN,ITYPE,NREP,INTGER,REAL,
     1  LWORD,WORD,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,ISTAT)
C
      IF (ISTRT.NE.-2) GOTO 320
C
      IF (MODWRN.EQ.1) THEN
         WRITE (IPR,310) (MECARDS(I,NRDCRD),I=1,80)
         CALL WARN
         GOTO 5000
         ENDIF
310   FORMAT(1H0,10X,'**WARNING** IN THE SSARREG MOD - A OPERATION ',
     1  'NAME IS REQUIRED. THESE CHANGES WILL BE IGNORED.  THE CARD ',
     2  'IMAGE IS: ',/,11X,80A1)
C
320   CONTINUE
C
C
C

      GOTO 70
C
C
C
5000  CALL FSTWHR(OLDOPN,IOLDOP,OLDOPN,IOLDOP)

        write(ipr,*) "NEW SSARREG MOD CARDS ARE:"

         do 5001 j=1,ncards
         write(ipr,5002)(MODCRD(I,j),I=1,80)
5001      continue
       write(ipr,*) " "
C
      RETURN
      END
