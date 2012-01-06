c-----------------------------------------
c intrinsic function ODD is not implemented by g77. 
c 0 is equiv. to .FALSE., any other = .TRUE.
c-----------------------------------------
      Logical*2 Function b1_odd (arg)
      Implicit None
      byte arg
      Integer*4 a
      
      a = arg
c      print *, 'b1_odd (', a, ') = ',  (mod (a,2) .ne. 0)

      b1_odd = (mod (a,2) .ne. 0)
      return
      end

c-----------------------------------------
c intrinsic function ODD is not implemented by g77. 
c 0 is  equiv. to .FALSE., any other = .TRUE. 
c-----------------------------------------
      Logical*2 Function i4_odd (arg) 
      Implicit None
      integer arg
      Integer*4 a

       a = arg
c      print *, 'i4_odd(',a,') = ', (mod (a,2) .ne. 0)

      i4_odd = (mod (a,2) .ne. 0)
      return
      end

c-----------------------------------------
c intrinsic function BITEST is not yet implemented by g77,
c   and is a more restrictive implementation of btest(). 
c-----------------------------------------
      Logical*2 Function bitest (arg1, arg2)
      Implicit None
      integer arg1, arg2
      integer*4 a,b

      a = arg1
      b = arg2
c      print *, 'bitest(',a,',',b,') = ', btest (a,b)

      bitest = btest (a,b)
      return
      end

c-----------------------------------------
c intrinsic function JNINT is not yet implemented by g77,
c  this function is more specific than the generic NINT().
c-----------------------------------------
      Integer*4 Function jnint (arg)
      Implicit None
      real      arg
      real*4    a
      
      if (arg.ge.0) then
        a=arg+0.5
      else
        a=arg-0.5
      endif
      
c      print *, 'jnint(',arg,') = ', int( a)

      jnint = int(a)
      return
      end

c-----------------------------------------
c intrinsic function JINT is not yet implemented by g77,
c  this function is more specific than the generic INT().  
c-----------------------------------------
      Integer*4 Function jint (arg)
      Implicit None
      real    arg
      real*4  a

      a = arg
c      print *, 'jint(',a,') = ', int( a)
      
      jint = int (a)
      return
      end

c-----------------------------------------
c intrinsic function JMAX0 is not yet implemented by g77,
c  this function is more specific than the generic MAX().  
c-----------------------------------------
      Integer*4 Function jmax0 (arg1, arg2)
      Implicit None
      integer arg1, arg2
      integer*4    a,b

      a = arg1
      b = arg2
c      print *, 'jmax0(',a,',',b,') = ', max( a,b)

      jmax0 = max (a,b)
      return
      end

c-----------------------------------------
c intrinsic function JMIN0 is not yet implemented by g77,
c  this function is more specific than the generic MIN().  
c-----------------------------------------
      Integer*4 Function jmin0 (arg1, arg2)
      Implicit None
      integer arg1, arg2
      integer*4    a,b

      a = arg1
      b = arg2
c      print *, 'jmin0(',a,',',b,') = ', min( a,b)

      jmin0 = min (a,b)
      return
      end

c-----------------------------------------
c intrinsic function JISIGN is not yet implemented by g77,
c  this function is more specific than the generic ISIGN().  
c-----------------------------------------
      Integer*4 Function jisign (arg1, arg2)
      Implicit None
      integer arg1, arg2
      integer*4 a, b

      a = arg1
      b = arg2
c      print *, 'jisign(',a,',',b,') = ', isign( a,b)

      jisign = isign (a,b)
      return
      end

c-----------------------------------------
c intrinsic function JIABS is not yet implemented by g77,
c  this function is more specific than the generic IABS().  
c-----------------------------------------
      Integer*4 Function jiabs (arg)
      Implicit None
      integer arg
      integer*4 a

      a = arg
c      print *, 'jiabs(',a,') = ', iabs( a)

      jiabs = iabs (a)
      end

c-----------------------------------------
c intrinsic function IIAND is not yet implemented by g77
c   and is a more restrictive implementation of iand().
c-----------------------------------------
      Integer*2 Function iiand (arg1,arg2)
      Implicit None
      integer arg1, arg2
      Integer*2 a, b

      a = arg1
      b = arg2
c      print *, 'iiand(',a,',',b,') = ', iand( a,b)

      iiand = iand (a,b)
      return
      end

c-----------------------------------------
c intrinsic function IISHFT is not yet implemented by g77
c   and is a more restrictive implementation of ishft().
c-----------------------------------------
      Integer*2 Function iishft (arg1,arg2)
      Implicit None
      integer arg1, arg2
      Integer*2 a, b

      a = arg1
      b = arg2
c      print *, 'iishft(',a,',',b,') = ', ishft( a,b)

      iishft = ishft (a,b)
      return
      end

c-----------------------------------------
c intrinsic function IIOR is not yet implemented by g77
c   and is a more restrictive implementation of ior(). 
c-----------------------------------------
      integer*2 Function iior (arg1,arg2)
      Implicit None
      integer arg1, arg2
      integer*2 a,b

      a = arg1
      b = arg2
c      print *, 'iior(',a,',',b,') = ', ior( a,b) 

      iior = ior (a,b)
      return
      end

c-----------------------------------------
c intrinsic function IMOD is not yet implemented by g77
c   and is a more restrictive implementation of mod(). 
c-----------------------------------------
      integer*2 Function imod (arg1,arg2)
      Implicit None
      integer arg1, arg2
      integer*2 a,b

      a = arg1
      b = arg2
c      print *, 'imod(',a,',',b,') = ', mod( a,b) 

      imod = mod(a,b)
      return
      end

c  This function is an intrinsic function in the gcc-2.95.1 release, SOD 1 Nov.99 
c-----------------------------------------
c intrinsic function DABS is not yet implemented by g77
c   and is a more restrictive implementation of abs(). 
c-----------------------------------------
c      Real*8 Function dabs (a)
c      Implicit None
c      real*8 a
c      
c      dabs = abs (a)
c      end

