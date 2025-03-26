MODULE portlib
  USE ISO_C_BINDING, ONLY: C_INT
  IMPLICIT NONE
CONTAINS

  FUNCTION DATE() RESULT(data_atual)
    CHARACTER(9) :: data_atual
    INTEGER :: valores(8)
    CALL DATE_AND_TIME(VALUES=valores)
    WRITE(data_atual, '(I2.2,"-",A3,"-",I2.2)') valores(3), MONTH_NAME(valores(2)), MOD(valores(1),100)
  END FUNCTION DATE

  FUNCTION TIME() RESULT(hora_atual)
    CHARACTER(8) :: hora_atual
    INTEGER :: valores(8)
    CALL DATE_AND_TIME(VALUES=valores)
    WRITE(hora_atual, '(I2.2,":",I2.2,":",I2.2)') valores(5), valores(6), valores(7)
  END FUNCTION TIME

  FUNCTION RANDOM() RESULT(r)
    REAL :: r
    CALL RANDOM_SEED()
    CALL RANDOM_NUMBER(r)
  END FUNCTION RANDOM

  SUBROUTINE PAUSE()
    CHARACTER(1) :: dummy
    PRINT *, "Pressione Enter para continuar..."
    READ(*,'(A)') dummy
  END SUBROUTINE PAUSE

  SUBROUTINE CLS()
    CALL SYSTEM("clear")
  END SUBROUTINE CLS

  SUBROUTINE BEEP()
    PRINT *, CHAR(7)
  END SUBROUTINE BEEP

  SUBROUTINE SLEEP(ms)
    INTEGER, INTENT(IN) :: ms
    INTEGER :: secs
    CHARACTER(LEN=100) :: cmd
    WRITE(cmd, '(A,I0)') "sleep ", ms / 1000.0  ! Linux sleep aceita frações
    CALL SYSTEM(TRIM(cmd))
  END SUBROUTINE SLEEP

  FUNCTION ENVIRON(varname) RESULT(value)
    CHARACTER(*), INTENT(IN) :: varname
    CHARACTER(256) :: value
    CALL GET_ENVIRONMENT_VARIABLE(TRIM(varname), value)
  END FUNCTION ENVIRON

  SUBROUTINE FILLCHAR(str, c)
    CHARACTER(*), INTENT(OUT) :: str
    CHARACTER(1), INTENT(IN) :: c
    INTEGER :: i
    DO i = 1, LEN(str)
      str(i:i) = c
    END DO
  END SUBROUTINE FILLCHAR

  FUNCTION MONTH_NAME(mes) RESULT(m)
    INTEGER, INTENT(IN) :: mes
    CHARACTER(3) :: m
    CHARACTER(3), DIMENSION(12) :: nomes = &
      ["Jan","Feb","Mar","Apr","May","Jun", &
       "Jul","Aug","Sep","Oct","Nov","Dec"]
    IF (mes >= 1 .AND. mes <= 12) THEN
      m = nomes(mes)
    ELSE
      m = "???"
    END IF
  END FUNCTION MONTH_NAME

END MODULE portlib
