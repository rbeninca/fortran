PROGRAM programa7_1
IMPLICIT NONE

INTEGER ::INTERVALO, A=20 ,B=10 , C=2;

IF (A<B) WRITE  (*,*) "A  menor do que B"
IF (A>B) WRITE  (*,*) "A  maior do que B"
INTERVALO=0;
DO WHILE (INTERVALO<100)	
	IF (A>=INTERVALO .AND. A<INTERVALO+10) THEN
		WRITE (*,*) "A está no interva-lo ["	,INTERVALO, ","  , INTERVALO+10 ,")" 
	END IF 
	INTERVALO=INTERVALO+10;
END DO

IF (A >=B )	THEN 
	IF (A>=C)	THEN 
		IF	(B>=C)	THEN
			PRINT *, "A>=B>=C"
		ELSE 
			PRINT *, "A>=C>=B"
		END IF
	ELSE  !Se a maior que B e C é Maior do que A então C>A>B
		PRINT *, "C>A>B
	END IF
ELSE
	IF (B>C) THEN 
		 IF (A>C) THEN
			PRINT *, "B>A>C"
		 ELSE
			PRINT *, "B>C>A"
		 END IF
	ELSE 
		PRINT *,"C>B>A"
	END IF	
END IF 




	
	





END PROGRAM programa7_1