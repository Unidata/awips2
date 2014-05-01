c
c	simple write
c
c	write n bytes to unit io

	subroutine wryte(io, n, buf)

	integer n, io
	character buf(n)

	write(io) buf
	return
	end

