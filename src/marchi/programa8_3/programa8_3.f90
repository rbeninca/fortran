PROGRAM progracaoAritimetica
IMPLICIT NONE
REAL A1, D ,N,AN,SN,SOMA
INTEGER I ;
CHARACTER(20) :: ARQUIVO="5_1_PA.TXT"
OPEN (UNIT=10,FILE=ARQUIVO	,STATUS="REPLACE")

A1=1.3;
D=3.9;
N=5;

20 FORMAT (A,I2,A,F5.2)
21 FORMAT (A,A,F5.2)

AN= A1+(N-1)*D
SN=(A1+AN)*(N/2)

DO I=1, N
	WRITE (* ,20) "A",I,"=",A1  
	WRITE (10,20) "A",I,"=",A1 
	SOMA=SOMA+A1;
	PRINT *,SOMA
	IF (I==5) EXIT
	A1=A1+D 
	
END DO 
	WRITE (* ,21) " SN","=",SN  
	WRITE (10,21) " SN","=",SN 
		
CALL SYSTEM("start notepad "//ARQUIVO)


END PROGRAM  progracaoAritimetica