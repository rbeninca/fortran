PROGRAM imprimedata_hora
    USE portlib
    IMPLICIT NONE

    CHARACTER(9) :: data_atual
    CHARACTER(8) :: hora_atual

    data_atual = DATE()
    hora_atual = TIME()

    WRITE(*,*) 'Data atual: ', data_atual
    WRITE(*,*) 'Hora atual: ', hora_atual

END PROGRAM imprimedata_hora
