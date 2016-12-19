*&---------------------------------------------------------------------*
*& Report  ZNAMESPACE_ANALYZER
*&---------------------------------------------------------------------*
*& Jordi Escoda, 09th Nov 2016
*& Namespace analyzer.
*& Tells if it is safe to use a namespace in your system for importing
*& objects
*&---------------------------------------------------------------------*
* Text symbols
*  001  Parameters
*  M01  Unsafe to use namespace
*  M02  Safe to use namespace
*  M03  Listing objects which may collision
*  M04  Do not use wildcard
*
* Selection texts
*  P_NAMSPC	Namespace
*  P_NMSPCC	Namespace for class
*  P_NMSPCX	Namespace for exception class
*&---------------------------------------------------------------------*
REPORT znamespace_analyzer.

*--------------------------------------------------------------------*
* Selection screen
*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: p_namspc TYPE char20 OBLIGATORY. "Namespace
PARAMETERS: p_nmspcc TYPE char20. "Namespace for class
PARAMETERS: p_nmspcx TYPE char20. "Namespace for exceptio class
SELECTION-SCREEN: END OF BLOCK b1.

*----------------------------------------------------------------------*
*       CLASS lcl_namespace_analysis DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_namespace_analysis DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS:
       execute.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_tadir,
        pgmid TYPE tadir-pgmid,
        object TYPE tadir-object,
        obj_name TYPE tadir-obj_name,
        devclass TYPE tadir-devclass,
    END OF ty_tadir.
    CLASS-DATA:
       lt_attr_tadir TYPE STANDARD TABLE OF ty_tadir.
    CLASS-METHODS:
       select_tadir,
       show_alv,
       alv_set_functions
        IMPORTING im_salv_table TYPE REF TO cl_salv_table,
       alv_set_columns
        IMPORTING im_salv_table TYPE REF TO cl_salv_table.

ENDCLASS.                    "lcl_namespace_analysis DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_namespace_analysis IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_namespace_analysis IMPLEMENTATION.
  METHOD execute.
    IF p_namspc CA '*'
      OR p_namspc CA '%'
      OR p_nmspcc CA '*'
      OR p_nmspcc CA '%'
      OR p_nmspcx CA '*'
      OR p_nmspcx CA '%'.
      MESSAGE e368(00) WITH 'Do not use wildcard'(m04).
    ENDIF.

    select_tadir( ).
    IF lt_attr_tadir[] IS NOT INITIAL.
      MESSAGE i398(00) WITH 'Unsafe to use namespace(s)'(m01)
                            p_namspc p_nmspcc p_nmspcx DISPLAY LIKE 'E'.
      show_alv( ).
    ELSE.
      MESSAGE i398(00) WITH 'Safe to use namespace(s)'(m02)
                            p_namspc p_nmspcc p_nmspcx.
    ENDIF.
  ENDMETHOD.                    "execute

  METHOD select_tadir.
    DATA: lv_namespace TYPE string.

*   Namespace in general except classes
    CONCATENATE p_namspc '%' INTO lv_namespace.
    SELECT pgmid object obj_name devclass
      INTO TABLE lt_attr_tadir
    FROM tadir
    WHERE object <> 'CLAS'
      AND obj_name LIKE lv_namespace.

*   Namespace for Lock objects
    CONCATENATE 'E' p_namspc '%' INTO lv_namespace.
    SELECT pgmid object obj_name devclass
      APPENDING TABLE lt_attr_tadir
    FROM tadir
    WHERE object = 'ENQU'
      AND obj_name LIKE lv_namespace.

*   Namespace for classes
    IF p_nmspcc IS NOT INITIAL.
      CONCATENATE p_nmspcc '%' INTO lv_namespace.
      SELECT pgmid object obj_name devclass
        APPENDING TABLE lt_attr_tadir
      FROM tadir
      WHERE object = 'CLAS'
        AND obj_name LIKE lv_namespace.
    ENDIF.

*   Namespace for exception classes
    IF p_nmspcx IS NOT INITIAL.
      CONCATENATE p_nmspcx '%' INTO lv_namespace.
      SELECT pgmid object obj_name devclass
        APPENDING TABLE lt_attr_tadir
      FROM tadir
      WHERE object = 'CLAS'
        AND obj_name LIKE lv_namespace.
    ENDIF.

  ENDMETHOD.                    "select_tadir

  METHOD show_alv.
    DATA: lo_salv_table TYPE REF TO cl_salv_table.
    DATA: lo_cx_salv_msg TYPE REF TO cx_salv_msg.
    DATA: lo_salv_display_settings TYPE REF TO cl_salv_display_settings.
    DATA: lv_text_error TYPE string.

*   Instance ALV
    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = lo_salv_table
          CHANGING
            t_table      = lt_attr_tadir ).

      CATCH cx_salv_msg INTO lo_cx_salv_msg.
        lv_text_error = lo_cx_salv_msg->get_text( ).
        MESSAGE e208(00) WITH lv_text_error.
    ENDTRY.

*   ALV title
    lo_salv_display_settings = lo_salv_table->get_display_settings( ).
    lo_salv_display_settings->set_list_header( 'Listing objects which may collision'(m03) ).

*   Sets ALV functions
    alv_set_functions( lo_salv_table ).

*   Prepares ALV columns
    alv_set_columns( lo_salv_table ).

*   Display ALV
    lo_salv_table->display( ).
  ENDMETHOD.                    "show_alv

  METHOD alv_set_functions.
    DATA: lo_functions TYPE REF TO cl_salv_functions_list.

    lo_functions = im_salv_table->get_functions( ).
    lo_functions->set_all( abap_true ).
  ENDMETHOD.                    "alv_set_functions

  METHOD alv_set_columns.
    DATA: lo_salv_columns TYPE REF TO cl_salv_columns.

    lo_salv_columns = im_salv_table->get_columns( ).
    lo_salv_columns->set_optimize( abap_true ).

  ENDMETHOD.                    "alv_set_columns

ENDCLASS.                    "lcl_namespace_analysis IMPLEMENTATION

*--------------------------------------------------------------------*
* START-OF-SELECTION
*--------------------------------------------------------------------*
START-OF-SELECTION.
lcl_namespace_analysis=>execute( ).