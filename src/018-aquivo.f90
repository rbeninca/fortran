PROGRAM arquivo

IMPLICIT NONE
INTEGER unidade_saida
unidade_saida=10

OPEN(UNIT=unidade_saida, FILE="./tmp/tmp.txt", STATUS="UNKNOWN");
!move to end of file
!seek(unit=unidade_saida, iostat=ios, pos=0, status="old")

WRITE(unidade_saida,*) "Olá mundo";
CLOSE(UNIT=unidade_saida);


END PROGRAM