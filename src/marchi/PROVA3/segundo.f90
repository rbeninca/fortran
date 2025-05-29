module ROTINAS
implicit none

contains

  subroutine DADOS(nome, dia, K1, d, N, L, X)
    character(len=50), intent(out) :: nome, dia
    real*8, intent(out) :: K1, d, N, L, X

    open(8, file='ENTRA3.TXT', status='old')
    read(8, '(A)') nome
    read(8, '(A)') dia
    read(8,*) K1, d, N, L, X
    close(8)
  end subroutine DADOS

  subroutine SERIES(d, X, M6, M7)
    integer, intent(in) :: d, X
    real*8, intent(out) :: M6, M7
    integer :: b
    M6 = 0.0
    M7 = 0.0

    do b = 1, d
      M6 = M6 + 1.d0/((b*(b+1)*(b+2)))
    end do

    do b = 0, X
      M7 = M7 + ((-1.d0)**(b))*(2*b + 1)/dble(factorial(2*b + 1))
    end do
  end subroutine SERIES

  real(8) function factorial(n)
    integer, intent(in) :: n
    integer :: i
    factorial = 1
    do i = 2, n
      factorial = factorial * i
    end do
  end function factorial

end module ROTINAS
