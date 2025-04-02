PROGRAM testefile


OPEN (unit=10 ,file='./tmp/arquivo.txt',status="unknown",action="readwrite");
WRITE (10, "(A)" ,ADVANCE="NO")  "HELLO";
WRITE (10, "(A)",ADVANCE="NO")  "HELLO na mesma linha\n";


END PROGRAM