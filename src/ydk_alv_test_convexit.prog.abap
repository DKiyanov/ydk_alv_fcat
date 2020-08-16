*&---------------------------------------------------------------------*
*& Report  YDK_ALV_TEST_CONVEXIT
*& test for ALV conversion exit using
*&---------------------------------------------------------------------*
*& Autor: Kiyanov Dmitry
*& Email: DKiyanov@mail.ru
*&---------------------------------------------------------------------*

REPORT  ydk_alv_test_convexit.

PARAMETERS: cbconv AS CHECKBOX DEFAULT 'X'.

DATA: BEGIN OF itd OCCURS 0,
        mandt      TYPE t000-mandt,
        mwaer      TYPE t000-mwaer,
        cccopylock TYPE t000-cccopylock,
        cccoractiv TYPE t000-cccoractiv,
      END   OF itd.

CLASS lcl_test DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS conv
      IMPORTING
        !kind   TYPE clike
        !input  TYPE any
      EXPORTING
        !output TYPE any.

ENDCLASS.

CLASS lcl_test IMPLEMENTATION.
  METHOD conv.
    output = input.

    CASE kind.
      WHEN 'I'.
        TRANSLATE output TO UPPER CASE.
      WHEN 'O'.
        TRANSLATE output TO LOWER CASE.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.

DATA: go_test TYPE REF TO lcl_test.

START-OF-SELECTION.
  PERFORM get_data.
  PERFORM alv_show.

FORM get_data.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE itd
    FROM t000.
ENDFORM.                    "get_data

FORM alv_show.
  DATA: fc TYPE lvc_t_fcat.

  CALL FUNCTION 'YDK_ALV_FCAT_BUILD'
    EXPORTING
      alv_strut_key = 'ZBC_ALV_TEST_CONVEXIT'
*     structures    = ''
    TABLES
      alv_tab       = itd
      fcat          = fc.

  DATA: itval TYPE vrm_values.

  itval = VALUE #(
    ( key = abap_false text = 'Выключено' )
    ( key = abap_true text  = 'Включено' )
  ).

  CALL FUNCTION 'YDK_CONVERSION_EXIT_REGISTER'
    EXPORTING
      fieldname  = 'CCCOPYLOCK'
*     CALLBACK_PROGRAM        =
*     CALLBACK_FORM           =
*     CALLBACK_INSTANCE       =
*     CALLBACK_CLASS          =
*     CALLBACK_METHOD         =
*     ROLLNAME   =
*     DOMNAME    =
      values_tab = itval
    CHANGING
      fcat       = fc.

  CALL FUNCTION 'YDK_CONVERSION_EXIT_REGISTER'
    EXPORTING
      fieldname = 'CCCORACTIV'
*     CALLBACK_PROGRAM        =
*     CALLBACK_FORM           =
*     CALLBACK_INSTANCE       =
*     CALLBACK_CLASS          =
*     CALLBACK_METHOD         =
*     ROLLNAME  =
*     DOMNAME   =
*     VALUES_TAB =
    CHANGING
      fcat      = fc.

*  CALL FUNCTION 'YDK_CONVERSION_EXIT_REGISTER'
*    EXPORTING
*      fieldname     = 'MWAER'
**     CALLBACK_PROGRAM        =
*      callback_form = 'CONV'
**     CALLBACK_INSTANCE       =
**     CALLBACK_CLASS          =
**     CALLBACK_METHOD         =
**     ROLLNAME      =
**     DOMNAME       =
**     VALUES_TAB    =
*    CHANGING
*      fcat          = fc.

  CREATE OBJECT go_test.

  CALL FUNCTION 'YDK_CONVERSION_EXIT_REGISTER'
    EXPORTING
      fieldname         = 'MWAER'
*     CALLBACK_PROGRAM  =
*     CALLBACK_FORM     =
      callback_instance = go_test
*     CALLBACK_CLASS    =
      callback_method   = 'CONV'
*     ROLLNAME          =
*     DOMNAME           =
*     VALUES_TAB        =
    CHANGING
      fcat              = fc.


  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      it_fieldcat_lvc = fc[]
    TABLES
      t_outtab        = itd
    EXCEPTIONS
      program_error   = 1
      OTHERS          = 2.
ENDFORM.                    "alv_show

FORM conv USING kind input CHANGING output.
  output = input.

  CASE kind.
    WHEN 'I'.
      TRANSLATE output TO UPPER CASE.
    WHEN 'O'.
      TRANSLATE output TO LOWER CASE.
  ENDCASE.
ENDFORM.
