FUNCTION CONVERSION_EXIT_YDK29_OUTPUT .
*"--------------------------------------------------------------------
*"*"Локальный интерфейс:
*"  IMPORTING
*"     REFERENCE(INPUT)
*"  EXPORTING
*"     REFERENCE(OUTPUT)
*"--------------------------------------------------------------------
* see FM YDK_CONVERSION_EXIT_REGISTER
  PERFORM output USING '29' input CHANGING output.
ENDFUNCTION.
