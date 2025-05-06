PROGRAM ordenavetor

IMPLICIT NONE 
REAL ::nrand
INTEGER  :: vetor(10)
INTEGER I;

DO  I=1 ,size(vetor)
	CALL	random_number(nrand)
	VETOR(I)=NINT(nrand*100);
END DO
    ! Imprimindo o vetor original
    CALL printarray(vetor)
    
    ! Ordenando o vetor
    CALL ordenaarray(vetor)

    ! Imprimindo o vetor ordenado
    CALL printarray(vetor)

CONTAINS 

	SUBROUTINE printarray(array)
		
		INTEGER  i, array(:)

		DO I=1, SIZE(array)
			WRITE  (*,"(A,I3,A,I3)") " Array[",I,"]=",array(I);		
		END DO
			
	END SUBROUTINE

	SUBROUTINE ordenaarray(array)
		IMPLICIT NONE
		INTEGER , INTENT(INOUT):: array(:)
		call sort(array)		
	END SUBROUTINE



  SUBROUTINE sort(arr)
        IMPLICIT NONE
        INTEGER :: n
        INTEGER, DIMENSION(:), INTENT(INOUT) :: arr  ! Vetor a ser ordenado
        INTEGER :: i, j
        INTEGER :: temp
        n = SIZE(arr)

        ! Algoritmo Bubble Sort
        DO i = 1, n-1
            DO j = 1, n-i
                IF (arr(j) > arr(j+1)) THEN
                    ! Trocar os elementos
                    temp = arr(j)
                    arr(j) = arr(j+1)
                    arr(j+1) = temp
                END IF
            END DO
        END DO
    END SUBROUTINE sort

END PROGRAM ordenavetor