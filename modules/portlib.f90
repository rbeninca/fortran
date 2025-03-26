MODULE portlib
  IMPLICIT NONE
CONTAINS
  FUNCTION DATE() RESULT(data_atual)
    CHARACTER(9) :: data_atual
    INTEGER :: valores(8)
    CALL DATE_AND_TIME(VALUES=valores)
    WRITE(data_atual,'(I2.2,"-",A3,"-",I2.2)') valores(3), MONTH_NAME(valores(2)), MOD(valores(1),100)
  END FUNCTION DATE

  FUNCTION TIME() RESULT(hora_atual)
    CHARACTER(8) :: hora_atual
    INTEGER :: valores(8)
    CALL DATE_AND_TIME(VALUES=valores)
    WRITE(hora_atual,'(I2.2,":",I2.2,":",I2.2)') valores(5), valores(6), valores(7)
  END FUNCTION TIME

  FUNCTION MONTH_NAME(mes)
    INTEGER, INTENT(IN) :: mes
    CHARACTER(3) :: MONTH_NAME
    CHARACTER(3), DIMENSION(12) :: meses = &
      ["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"]
    MONTH_NAME = meses(mes)
  END FUNCTION MONTH_NAME
END MODULE portlib
