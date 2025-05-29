PROGRAM PROGRAM_14_1
   USE PORTLIB
   IMPLICIT NONE
   REAL(8) Pi
   INTEGER UNIT, VER
   LOGICAL  ::debug=.TRUE.
   !se o debug estiver ativo, o programa vai usar os valores de teste
   
   Pi = DACOS(-1.0d0)
   WRITE(*,*) "Entre com o valor de UNIT (inteiro)"
   
   
   IF (.NOT. debug)  READ(*,*) UNIT
   IF (debug) UNIT = 9   
   
   OPEN(9,file="saida14a.txt")
   WRITE(9,11) UNIT, Pi


11 FORMAT( 5X, "UNIT =", I4, 1/, &
      5X, "Pi =", 1PE25.15 )
   CLOSE(9)
   VER = SYSTEM("start Notepad saida14a.txt")
END PROGRAM PROGRAM_14_1
