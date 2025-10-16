program significan_loss
  implicit none

  real(8) :: big, small, suma ! 8-byte integer (usually 64-bit)
  integer(kind=8) :: n, i

  big=1.0d0
  small=1.0d0
  n=1000
  do i=1, n
    suma=big+small
    print*, small, big, suma
    small=small/2.0d0
    if (suma<=big) then
      print*, " cyklus chcípnul, když proběhla iterace", i, "."
      exit
    end if
  end do





end program significan_loss
