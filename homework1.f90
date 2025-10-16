program factorial_overflow
  implicit none

  integer(kind=2) :: f2 ! 2-byte integer (usually 16-bit)
  integer(kind=4) :: f4 ! 4-byte integer (usually 32-bit)
  integer(kind=8) :: f8 ! 8-byte integer (usually 64-bit)
  integer :: n
  integer(kind=2) :: prev_f2
  integer(kind=4) :: prev_f4
  integer(kind=8) :: prev_f8

  f2 = 1
  f4 = 1
  f8 = 1

  do n = 1, 25
    prev_f2 = f2
    prev_f4 = f4
    prev_f8 = f8

    if (n > 1) then
      f2 = f2 * n
      f4 = f4 * n
      f8 = f8 * n
    end if

    print *, n, f2, f4, f8

    ! OVERFLOWWd) program se mi zastaví na prvním overflow. Aby to bylo pro každý zvlášť, musel bych to otrocky napsat znovu. Ale neznám syntax logických operátorů. nechal jsem si hromadné zastavení vygenerovat kopilotem, ale sem ho dávat nebudu
    if (n > 1) then
      if (f2 / n /= prev_f2) then
        print *, "Overflow detected for int16 at n =", n
        exit
      end if
      if (f4 / n /= prev_f4) then
        print *, "Overflow detected for int32 at n =", n
        exit
      end if
      if (f8 / n /= prev_f8) then
        print *, "Overflow detected for int64 at n =", n
        exit
      end if
    end if
  end do
end program factorial_overflow
