PROGRAM programa7_1a
USE portlib

!sejam 3 numeros  encontre o maior, menor e o meio

IMPLICIT NONE
INTEGER A, B, C;



A=4;
B=2;
C=0;
CALL ordena (A,B,C)
CALL checa(A,B,C)


A=-1;
B=100;
C=30;
CALL ordena (A,B,C)
CALL checa(A,B,C)

A=10;
B=5;
C=0;
CALL ordena (A,B,C)
CALL checa(A,B,C)

CONTAINS

SUBROUTINE ORDENA(A,B , C)
 IMPLICIT NONE
 INTEGER ,INTENT (INOUT) :: A, B , C
 INTEGER :: TMP

	IF (A>B) THEN 
		TMP=A;
		A=B
		B=TMP;
	END IF
	IF (A>C) THEN 
		TMP=A
		A=C
		C=TMP;
	END IF
	IF (B>C) THEN 
		TMP=B
		B=C
		C=TMP
	END IF
	PRINT *, A, B, C
END SUBROUTINE

SUBROUTINE CHECA(A,B , C)
 INTEGER A, B , C
 IF  (.NOT. (A<B .AND.  B<C)) THEN 
	PRINT *,"Erro na ordenacao"
 ELSE 
	PRINT *,"Correto"
 END IF 


END SUBROUTINE
END PROGRAM