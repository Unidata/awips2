      subroutine resjfwrite( str_len, resj_string, unitnum )
      integer unitnum,str_len
      character*240 resj_string
      character*120 tmpString1, tmpString2, tmpString3
C
C    ================================= RCS keyword statements ==========
      CHARACTER*118     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob81/ohd/ofs/src/resj_system/RCS/resjfwrite.f,v $
     . $',                                                             '
     .$Id: resjfwrite.f,v 1.4 2005/06/29 17:27:53 wkwock Exp $
     . $' /
C    ===================================================================
C


      tmpString1 = resj_string
      if  ( str_len.GT.119 ) then
         if  ( str_len.GT.240 ) then
            tmpString2 = resj_string(119:240)
            tmpString3 = resj_string(240:str_len)
            write( unitnum, 9)  tmpString1(1:118) // '\'
            write( unitnum, 9)  tmpString2(1:118) // '\'
            write( unitnum, 9)  tmpString3(1:str_len-236)
         else 
            tmpString2 = resj_string(119:str_len)
            write( unitnum, 9)  tmpString1(1:118) // '\'
            write( unitnum, 9)  tmpString2(1:str_len-118)
         endif
      else 
         write( unitnum, 9)  tmpString1(1:str_len)
      endif

9     format( A )
      end
