FUNCTION CONVERSION_EXIT_YDK39_OUTPUT .
*"--------------------------------------------------------------------
*"*"Локальный интерфейс:
*"  IMPORTING
*"     REFERENCE(INPUT)
*"  EXPORTING
*"     REFERENCE(OUTPUT)
*"--------------------------------------------------------------------
* see FM YDK_CONVERSION_EXIT_REGISTER
  PERFORM output USING '39' input CHANGING output.
ENDFUNCTION.
