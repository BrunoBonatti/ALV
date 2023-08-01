*&---------------------------------------------------------------------*
*& Report Z_ALV
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_alv.

TABLES: bkpf, bseg, sscrfields.

" Declaration of the global variables
DATA: r_container_01 TYPE REF TO cl_gui_custom_container,
      r_grid_01      TYPE REF TO cl_gui_alv_grid,
      r_container_02 TYPE REF TO cl_gui_custom_container,
      r_grid_02      TYPE REF TO cl_gui_alv_grid.

"Declaration of Internal Tables and Work Areas
DATA: iT_bkpf TYPE TABLE OF bkpf,
      wa_bkpf TYPE bkpf,
      it_bseg TYPE TABLE OF bseg.

"Declaration of sy-ucomm
DATA ok_code TYPE sy-ucomm.


"Declaration of Selection-options for the range of accounting documents
SELECTION-SCREEN BEGIN OF BLOCK part1 WITH FRAME TITLE text-001.
  SELECT-OPTIONS:
                 bukrs FOR bkpf-bukrs OBLIGATORY,
                 belnr FOR bkpf-belnr ,
                 gjahr FOR bkpf-gjahr OBLIGATORY.

SELECTION-SCREEN SKIP.
SELECTION-SCREEN PUSHBUTTON /15(12) TEXT-002 USER-COMMAND PB_CLICK.

SELECTION-SCREEN END OF BLOCK part1.


*----------------------------------------------------------------------*
* CLASS lcl_event_handler DEFINITION
*----------------------------------------------------------------------*

"Definition and implementation of the double-click
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    METHODS: on_double_click FOR EVENT double_click OF cl_gui_alv_grid IMPORTING e_row.
ENDCLASS.

*----------------------------------------------------------------------*
* CLASS lcl_event_handler IMPLEMENTATION
*----------------------------------------------------------------------*

CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_double_click.
    IF it_bseg IS INITIAL.
      READ TABLE it_bkpf INTO wa_bkpf INDEX e_row-index.

      SELECT *
        FROM bseg
        INTO TABLE it_bseg
        WHERE   bukrs = wa_bkpf-bukrs AND
                belnr  = wa_bkpf-belnr AND
                gjahr =  wa_bkpf-gjahr.

      CREATE OBJECT r_container_02
        EXPORTING
          container_name              = 'C2'
        EXCEPTIONS
          cntl_error                  = 1
          cntl_system_error           = 2
          create_error                = 3
          lifetime_error              = 4
          lifetime_dynpro_dynpro_link = 5
          OTHERS                      = 6.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      CREATE OBJECT r_grid_02
        EXPORTING
          i_parent          = r_container_02
        EXCEPTIONS
          error_cntl_create = 1
          error_cntl_init   = 2
          error_cntl_link   = 3
          error_dp_create   = 4
          OTHERS            = 5.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.


      CALL METHOD r_grid_02->set_table_for_first_display
        EXPORTING
          i_structure_name              = 'BSEG'
        CHANGING
          it_outtab                     = it_bseg
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          OTHERS                        = 4.
      IF sy-subrc <> 0.
        MESSAGE TEXT-003 TYPE 'E'.
      ENDIF.

    ELSE .
      CLEAR wa_bkpf.
      CLEAR it_bseg.

      READ TABLE it_bkpf INTO wa_bkpf INDEX e_row-index.

      SELECT *
        FROM bseg
        INTO TABLE it_bseg
        WHERE   bukrs = wa_bkpf-bukrs AND
          belnr  = wa_bkpf-belnr AND
          gjahr =  wa_bkpf-gjahr.

      CALL METHOD R_grid_02->refresh_table_display( ).

    ENDIF.
  ENDMETHOD.
ENDCLASS.



"Definition of Screen Inputs Verification

AT SELECTION-SCREEN ON BLOCK part1 .

  SELECT *
   FROM Bkpf
   INTO TABLE it_bkpf
   WHERE  bukrs IN bukrs  AND
          belnr IN belnr   AND
  gjahr IN gjahr .


  IF sy-subrc <> 0  .
    MESSAGE TEXT-003 TYPE 'E'.
  ENDIF.


"Definition of PUSHBUTTON
IF sscrfields-ucomm = 'PB_CLICK'.
sscrfields-ucomm = 'ONLI'.
ENDIF.


START-OF-SELECTION.

  "Instantiation of the double-click class
  DATA: R_lcl_event_handler TYPE REF TO lcl_event_handler.
  CREATE OBJECT r_lcl_event_handler.
  SET HANDLER r_lcl_event_handler->on_double_click FOR ALL INSTANCES.

  "Screen call and its content
  CALL SCREEN 0100.
  INCLUDE z_alv_o01.
  INCLUDE z_alv_010i01.


*----------------------------------------------------------------------*
***INCLUDE Z_ALV_O01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module SET_ALV OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
*MODULE set_alv OUTPUT.

*CREATE OBJECT r_container_01
*  EXPORTING
*    container_name              = 'C1'
*  EXCEPTIONS
*    cntl_error                  = 1
*    cntl_system_error           = 2
*    create_error                = 3
*    lifetime_error              = 4
*    lifetime_dynpro_dynpro_link = 5
*    others                      = 6
*    .
*IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*ENDIF.
*
*
*CREATE OBJECT r_grid_01
*  EXPORTING
*    i_parent          = r_container_01
*  EXCEPTIONS
*    error_cntl_create = 1
*    error_cntl_init   = 2
*    error_cntl_link   = 3
*    error_dp_create   = 4
*    others            = 5
*    .
*IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*ENDIF.
*
*CALL METHOD r_grid_01->set_table_for_first_display
*  EXPORTING
*    i_structure_name              = 'BKPF'
*  CHANGING
*    it_outtab                     = it_bkpf
*  EXCEPTIONS
*    invalid_parameter_combination = 1
*    program_error                 = 2
*    too_many_lines                = 3
*    others                        = 4
*        .
*IF sy-subrc <> 0.
* MESSAGE Text-002 TYPE 'E'.
*ENDIF.
*ENDMODULE.

*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
*MODULE status_0100 OUTPUT.
* SET PF-STATUS '0100'.
*ENDMODULE.


*PROCESS AFTER INPUT.
*  MODULE user_command_0100.

*----------------------------------------------------------------------*
***INCLUDE Z_ALV_010I01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*MODULE user_command_0100 INPUT.
*  CASE  Ok_code .
*    WHEN  'EXIT'.
*      LEAVE PROGRAM.
*    WHEN 'BACK '.
*      LEAVE TO SCREEN 0 .
*  ENDCASE.
*ENDMODULE.
