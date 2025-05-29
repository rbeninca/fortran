PROGRAM PROGRAMA14_2
   USE PORTLIB
   IMPLICIT NONE
   REAL(8) Pi
   INTEGER UNIT, VER
   CHARACTER(50) SAIDA, TEXTO
   LOGICAL  ::debug=.TRUE.
   !se o debug estiver ativo, o programa vai usar os valores de teste


   Pi = DACOS(-1.0d0)  !calcula o arcoseno de -1.0, que é Pi
   WRITE(*,*) "Qual a unidade de saida (inteiro)?"
   IF (.NOT. debug)  READ(*,*) UNIT
   IF (debug) UNIT = 9   !se debug estiver ativo, usa o valor 8
   
   WRITE(*,*) "Qual o nome do arquivo de saida (caracter)?"
   IF (.NOT. debug)  READ(*,*) SAIDA
   IF (debug) SAIDA = "saida14b.txt"  !se debug estiver ativo, usa o valor "saida14b.txt"
   
    
   TEXTO = "Notepad " // SAIDA
    !monta o texto para abrir o arquivo no Notepad, se for Windows

   OPEN(UNIT, file = SAIDA )
   WRITE(UNIT,11) UNIT, Pi, SAIDA, TEXTO
   CLOSE(UNIT)
11 FORMAT( 1/, 5X, "UNIT =", I4, &
      1/, 5X, "Pi =", 1PE25.15, &
      1/, 5X, "SAIDA = ", A, &
      1/, 5X, "TEXTO = ", A )
   VER = SYSTEM( "START "// TEXTO )
END PROGRAM PROGRAMA14_2
