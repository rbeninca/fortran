PROGRAM programa9_1a

IMPLICIT NONE 
REAL , ALLOCATABLE, DIMENSION(:) :: notas
REAL :: menor, media, maior, soma
INTEGER ::nalunos=10,I
SOMA=0;

ALLOCATE (notas(nalunos));
CALL random_seed()

DO I=1,nalunos
	call random_number(notas(i))
	notas(i)=notas(i)*100
END DO 

DO I=1,nalunos
	PRINT *, "Notas[",i,"]=",notas(i)
	SOMA=SOMA+notas(i)
END DO 

PRINT *, "Minimo=",MINVAL(notas)
PRINT *, "Maximo=",MAXVAL(notas)
PRINT *, "soma=",  SUM(notas)
PRINT *, "media=", (SUM(notas)/size(notas))





END PROGRAM programa9_1a