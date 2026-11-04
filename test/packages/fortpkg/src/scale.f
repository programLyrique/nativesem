      subroutine scale(x, n, a, ok)
      integer n, i
      double precision x(n), a
      logical ok
      do 10 i = 1, n
         x(i) = a * x(i)
   10 continue
      ok = .true.
      end

      subroutine count(x, n, k)
      integer n, k
      double precision x(n)
      k = n
      end
