module ROTINAS
implicit none

contains

  subroutine  DADOS(Nome, Dia, K1, d, N, L, X)
      character(len=50), intent(out) :: Nome, Dia
      real*8, intent(out) :: K1, N, L
      integer, intent(out) :: d, X

    open(8, file='ENTRA3.TXT', status='old')
    read(8, '(A)') Nome
    read(8, '(A)') Dia
    read(8,*) K1, d, N, L, X
    close(8)
  end subroutine DADOS

  subroutine SERIES(d, X, M6, M7)
    integer, intent(in) :: d, X
    real*8, intent(out) :: M6, M7 
    integer :: b
     real*8 :: termo, fat
    M6 = 0.0
    M7 = 0.0

    do b = 1, d
      M6 = M6 + 1.d0/((b*(b+1)*(b+2)))
    end do

    do b = 0, X
      call fatorial(2*b + 1, fat)
      M7 = M7 + ((-1.d0)**(b))*(2*b + 1)/fat
    end do
  end subroutine SERIES

  subroutine fatorial(j, fat)
      integer, intent(in) :: j
      real*8, intent(out) :: fat
      integer :: k
      fat = 1.d0
      do k = 2, j
         fat = fat * dble(k)
      end do
   end subroutine fatorial

end module ROTINAS
