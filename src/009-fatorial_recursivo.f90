PROGRAM FATORIAL_RECURSIVO
IMPLICIT NONE
!VARS
    INTEGER :: num

!BEGIN CODE
    PRINT *, "Informe o número para calcular o fatorial"
    READ  (*,"(I8)")   num
    WRITE (*,*) fatorial(num)


CONTAINS
!-----------------------------------------------------
RECURSIVE INTEGER FUNCTION fatorial(n) RESULT(res)   !para implementar recurivisade é obrigatório explicitar na declaração
    !args 
    INTEGER, INTENT (IN) :: n 

    !BEGIN CODE
    IF (n>0) THEN 
        res=fatorial(n-1)*n
    ELSE 
        res=1  
    END IF 
END FUNCTION fatorial


END PROGRAM FATORIAL_RECURSIVO