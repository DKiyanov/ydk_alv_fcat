FUNCTION CONVERSION_EXIT_YDK06_INPUT .
*"--------------------------------------------------------------------
*"*"Локальный интерфейс:
*"  IMPORTING
*"     REFERENCE(INPUT)
*"  EXPORTING
*"     REFERENCE(OUTPUT)
*"--------------------------------------------------------------------
* see FM YDK_CONVERSION_EXIT_REGISTER
  PERFORM input USING '06' input CHANGING output.
ENDFUNCTION.
