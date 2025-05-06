PROGRAM programa7_1a
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



	
	

END PROGRAM