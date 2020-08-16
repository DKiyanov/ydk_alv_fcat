FUNCTION conversion_exit_ydk01_output .
*"----------------------------------------------------------------------
*"*"Локальный интерфейс:
*"  IMPORTING
*"     REFERENCE(INPUT)
*"  EXPORTING
*"     REFERENCE(OUTPUT)
*"----------------------------------------------------------------------
* see FM YDK_CONVERSION_EXIT_REGISTER
  PERFORM output USING '01' input CHANGING output.
ENDFUNCTION.
