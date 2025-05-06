PROGRAM programa9_6
	IMPLICIT NONE
	INTEGER TERMO , N, SN, D
	INTEGER, ALLOCATABLE, DIMENSION(:) :: A

	WRITE(*,*) "Todas as variaveis sao do tipo inteiro"
	WRITE(*,*) "Entre com o numero de termos da P.A."
	
	!READ(*,*) N
	N=100

	ALLOCATE ( A(-5:N) )
	

	WRITE(*,*) "Entre com o primeiro termo da P.A."


	!READ(*,*) A(-5)
	call random_seed()
	call random_number(A(-5))
	A(-5)=A(-5)*100;


	WRITE(*,*) "Entre com a diferenca entre dois termos subsequentes"
	!READ(*,*) D
	D=2
	
	DO TERMO = -4, N
		A(TERMO) = A(termo-1) + D
	END DO
			SN = (N+6) * ( A(-5) + A(N) ) / 2
	DO TERMO = -5, N
		WRITE(*,10) TERMO, A(TERMO)
	10 FORMAT(3X, "A(", I3, ") = ", I8 )
	END DO
	WRITE(*,*) "Soma dos termos = ", SN
END PROGRAM programa9_6