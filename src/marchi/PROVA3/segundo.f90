module ROTINAS
   use portlib
   implicit none
contains

   subroutine DADOS(Nome, Dia, K1, d, N, L, X)
      character(len=50), intent(out) :: Nome, Dia
      real*8, intent(out) :: K1, N, L
      integer, intent(out) :: d, X

      open(8, file="ENTRA3.TXT")
      read(8,'(A)') Nome
      read(8,'(A)') Dia
      read(8,*) K1
      read(8,*) d
      read(8,*)  N
      read(8,*) L
      read(8,*) X

      close(8)
   end subroutine DADOS

subroutine SERIES(d, X, M6, M7)
    integer, intent(in) :: d, X
    real*8, intent(out) :: M6, M7
    integer :: b
    real*8 :: termo, fat

    M6 = 0.d0
    do b = 1, d
		termo = ((1.0d0) / ( b*(b+1)*(b+2) ))
      M6 = M6 +  termo
    end do

    M7 = 0.d0
    do b = 0, X, 1
      call fatorial(2*b + 1, fat)
      termo = ((-1)**b * (0.5)**(2*b + 1)) / fat
      M7 = M7 + termo
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
