program leiaidade
    implicit none
    integer :: idade

    print *, 'Qual a sua idade?'
    !write (*,*) 'Qual a sua idade?'

    read (*,*) idade

    print *, 'Você tem ', idade, 'anos.'

end program leiaidade