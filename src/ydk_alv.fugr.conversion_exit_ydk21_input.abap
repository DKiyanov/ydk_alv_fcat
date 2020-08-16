FUNCTION CONVERSION_EXIT_YDK21_INPUT .
*"--------------------------------------------------------------------
*"*"Локальный интерфейс:
*"  IMPORTING
*"     REFERENCE(INPUT)
*"  EXPORTING
*"     REFERENCE(OUTPUT)
*"--------------------------------------------------------------------
* see FM YDK_CONVERSION_EXIT_REGISTER
  PERFORM input USING '21' input CHANGING output.
ENDFUNCTION.
