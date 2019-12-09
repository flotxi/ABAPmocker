*"* use this source file for your ABAP unit test classes
CLASS ltcl_data_generator DEFINITION DEFERRED.
CLASS zcl_abap_mocker DEFINITION LOCAL FRIENDS ltcl_data_generator.

CLASS lcl_mock_guid DEFINITION DEFERRED.

CLASS lcl_mock_guid DEFINITION.

  PUBLIC SECTION.

    CONSTANTS: gc_raw_1 TYPE raw16 VALUE '005056961A561ED889B97A9E751A40EA' ##NO_TEXT,
               gc_raw_2 TYPE raw16 VALUE '005056961A561ED889B992E97E9760EA' ##NO_TEXT,
               gc_raw_3 TYPE raw16 VALUE '005056961A561ED889B992E97E9760EF' ##NO_TEXT,
               gc_raw_4 TYPE raw16 VALUE '005056961A561ED889B992E97E9760FA' ##NO_TEXT.

    INTERFACES: zif_abap_mocker_uid_generator.

ENDCLASS.

CLASS lcl_mock_guid IMPLEMENTATION.

  METHOD zif_abap_mocker_uid_generator~create_guids.

    zif_abap_mocker_uid_generator~mt_guids = VALUE #( ( gc_raw_1  ) ( gc_raw_2 ) ( gc_raw_3  ) ( gc_raw_4 )  ).

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_data_generator DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_local_struc,
        char10 TYPE char10,
        string TYPE string,
        raw16  TYPE raw16,
        int4   TYPE int4,
        date   TYPE sydatum,
        tstmp  TYPE timestamp,
        flag   TYPE flag,
        numc5  TYPE numc5,
        tims   TYPE tims,


      END OF ty_local_struc,

      tt_local_table TYPE TABLE OF ty_local_struc.

    CLASS-DATA:
      gt_exp_ddic       TYPE TABLE OF zabapmocker_test,
      gs_exp_ddic       TYPE zabapmocker_test,

      gt_exp_local      TYPE tt_local_table,
      gs_exp_local      TYPE ty_local_struc,

      gt_exp_local_item TYPE tt_local_table.

    DATA:
      mo_cut            TYPE REF TO zcl_abap_mocker.


    CLASS-METHODS:
      class_setup.

    METHODS:
      setup,

      simple_generator
        IMPORTING
          i_expected  TYPE any
        CHANGING
          c_generated TYPE any,

      ddic_table
                  FOR TESTING
        RAISING cx_static_check,

      ddic_structure
                  FOR TESTING
        RAISING cx_static_check,

      local_table
                  FOR TESTING
        RAISING cx_static_check,

      local_structure
                  FOR TESTING
        RAISING cx_static_check,
      "! Header and Item Tables with local type and relation
      local_table_w_header_and_item
                  FOR TESTING
        RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_data_generator IMPLEMENTATION.

  METHOD ddic_table.

    DATA: lt_data TYPE TABLE OF ztestt_mock.

    simple_generator( EXPORTING i_expected  = gt_exp_ddic
                       CHANGING c_generated = lt_data ).


  ENDMETHOD.

  METHOD ddic_structure.

    DATA: ls_data TYPE ztestt_mock.

    simple_generator( EXPORTING i_expected  = gs_exp_ddic
                       CHANGING c_generated = ls_data ).


  ENDMETHOD.

  METHOD local_table.

    DATA: lt_data TYPE tt_local_table.

    simple_generator( EXPORTING i_expected  = gt_exp_local
                       CHANGING c_generated = lt_data ).


  ENDMETHOD.

  METHOD local_structure.

    DATA: ls_data TYPE ty_local_struc.

    simple_generator( EXPORTING i_expected  = gs_exp_local
                       CHANGING c_generated = ls_data ).


  ENDMETHOD.

  METHOD setup.

    mo_cut = NEW #( NEW lcl_mock_guid( ) ).

  ENDMETHOD.

  METHOD local_table_w_header_and_item.

    DATA: lt_head     TYPE tt_local_table,
          lt_item     TYPE tt_local_table,
          lt_key_pair TYPE zcl_abap_mocker=>tyt_key_pair.

    lt_key_pair = VALUE #( (
                            parent_key    = 'RAW16'
                            child_key     = 'RAW16'
                            child_2nd_key = VALUE #( ( fieldname = 'INT4' ) )
                            ) ).


    mo_cut->create_test_data(
      CHANGING
        c_data = lt_head )->create_test_data(
                              EXPORTING
                                it_key_pair = lt_key_pair
                              CHANGING
                                c_data     = lt_item ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = lt_head
        exp                  = gt_exp_local
        msg                  = 'Header Tabelle falsch' ).


    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = lt_item
        exp                  = gt_exp_local_item
        msg                  = 'Item Tabelle falsch' ).

  ENDMETHOD.

  METHOD class_setup.

    gs_exp_local-char10 = 'CHAR10_001'.
    gs_exp_local-string = 'ST_1'.
    gs_exp_local-raw16  = lcl_mock_guid=>gc_raw_1.
    gs_exp_local-int4   = '0001'.
    gs_exp_local-date   = '19911025'.
    gs_exp_local-tstmp  = '19911025'.
    gs_exp_local-flag   = 'X'.
    gs_exp_local-numc5  = '00001'.
    gs_exp_local-tims   = '115500'.

    gt_exp_local = VALUE #(
                            (
                              char10 = 'CHAR10_001'
                              string = 'ST_1'
                              raw16  = lcl_mock_guid=>gc_raw_1
                              int4   = '0001'
                              date   = '19911025'
                              tstmp  = '19911025'
                              flag   = 'X'
                              numc5  = '00001'
                              tims   = '115500'
                            )
                            (
                              char10 = 'CHAR10_002'
                              string = 'ST_2'
                              raw16  = lcl_mock_guid=>gc_raw_2
                              int4   = '0002'
                              date   = '19911025'
                              tstmp  = '19911025'
                              flag   = ' '
                              numc5  = '00002'
                              tims   = '115500'
                            )
                            (
                              char10 = 'CHAR10_003'
                              string = 'ST_3'
                              raw16  = lcl_mock_guid=>gc_raw_3
                              int4   = '0003'
                              date   = '19911025'
                              tstmp  = '19911025'
                              flag   = 'X'
                              numc5  = '00003'
                              tims   = '115500'
                            )
                            (
                              char10 = 'CHAR10_004'
                              string = 'ST_4'
                              raw16  = lcl_mock_guid=>gc_raw_4
                              int4   = '0004'
                              date   = '19911025'
                              tstmp  = '19911025'
                              flag   = ' '
                              numc5  = '00004'
                              tims   = '115500'
                            )
                          ).

    gt_exp_local_item = VALUE #(
                                (
                                  char10 = 'CHAR10_001'
                                  string = 'ST_1'
                                  raw16  = lcl_mock_guid=>gc_raw_1
                                  int4   = '0001'
                                  date   = '19911025'
                                  tstmp  = '19911025'
                                  flag   = 'X'
                                  numc5  = '00001'
                                  tims   = '115500'
                                )
                                (
                                  char10 = 'CHAR10_002'
                                  string = 'ST_2'
                                  raw16  = lcl_mock_guid=>gc_raw_1
                                  int4   = '0002'
                                  date   = '19911025'
                                  tstmp  = '19911025'
                                  flag   = ' '
                                  numc5  = '00002'
                                  tims   = '115500'
                                )
                                (
                                  char10 = 'CHAR10_003'
                                  string = 'ST_3'
                                  raw16  = lcl_mock_guid=>gc_raw_1
                                  int4   = '0003'
                                  date   = '19911025'
                                  tstmp  = '19911025'
                                  flag   = 'X'
                                  numc5  = '00003'
                                  tims   = '115500'
                                )
                                (
                                  char10 = 'CHAR10_004'
                                  string = 'ST_4'
                                  raw16  = lcl_mock_guid=>gc_raw_1
                                  int4   = '0004'
                                  date   = '19911025'
                                  tstmp  = '19911025'
                                  flag   = ' '
                                  numc5  = '00004'
                                  tims   = '115500'
                                )
                                (
                                  char10 = 'CHAR10_005'
                                  string = 'ST_5'
                                  raw16  = lcl_mock_guid=>gc_raw_2
                                  int4   = '0001'
                                  date   = '19911025'
                                  tstmp  = '19911025'
                                  flag   = 'X'
                                  numc5  = '00005'
                                  tims   = '115500'
                                )
                                (
                                  char10 = 'CHAR10_006'
                                  string = 'ST_6'
                                  raw16  = lcl_mock_guid=>gc_raw_2
                                  int4   = '0002'
                                  date   = '19911025'
                                  tstmp  = '19911025'
                                  flag   = ' '
                                  numc5  = '00006'
                                  tims   = '115500'
                                )
                                (
                                  char10 = 'CHAR10_007'
                                  string = 'ST_7'
                                  raw16  = lcl_mock_guid=>gc_raw_2
                                  int4   = '0003'
                                  date   = '19911025'
                                  tstmp  = '19911025'
                                  flag   = 'X'
                                  numc5  = '00007'
                                  tims   = '115500'
                                )
                                (
                                  char10 = 'CHAR10_008'
                                  string = 'ST_8'
                                  raw16  = lcl_mock_guid=>gc_raw_2
                                  int4   = '0004'
                                  date   = '19911025'
                                  tstmp  = '19911025'
                                  flag   = ' '
                                  numc5  = '00008'
                                  tims   = '115500'
                                )
                                (
                                  char10 = 'CHAR10_009'
                                  string = 'ST_9'
                                  raw16  = lcl_mock_guid=>gc_raw_3
                                  int4   = '0001'
                                  date   = '19911025'
                                  tstmp  = '19911025'
                                  flag   = 'X'
                                  numc5  = '00009'
                                  tims   = '115500'
                                )
                                (
                                  char10 = 'CHAR10_010'
                                  string = 'ST_0'
                                  raw16  = lcl_mock_guid=>gc_raw_3
                                  int4   = '0002'
                                  date   = '19911025'
                                  tstmp  = '19911025'
                                  flag   = ' '
                                  numc5  = '00010'
                                  tims   = '115500'
                                )
                                (
                                  char10 = 'CHAR10_011'
                                  string = 'ST_1'
                                  raw16  = lcl_mock_guid=>gc_raw_3
                                  int4   = '0003'
                                  date   = '19911025'
                                  tstmp  = '19911025'
                                  flag   = 'X'
                                  numc5  = '00011'
                                  tims   = '115500'
                                )
                                (
                                  char10 = 'CHAR10_012'
                                  string = 'ST_2'
                                  raw16  = lcl_mock_guid=>gc_raw_3
                                  int4   = '0004'
                                  date   = '19911025'
                                  tstmp  = '19911025'
                                  flag   = ' '
                                  numc5  = '00012'
                                  tims   = '115500'
                                )
                                (
                                  char10 = 'CHAR10_013'
                                  string = 'ST_3'
                                  raw16  = lcl_mock_guid=>gc_raw_4
                                  int4   = '0001'
                                  date   = '19911025'
                                  tstmp  = '19911025'
                                  flag   = 'X'
                                  numc5  = '00013'
                                  tims   = '115500'
                                )
                                (
                                  char10 = 'CHAR10_014'
                                  string = 'ST_4'
                                  raw16  = lcl_mock_guid=>gc_raw_4
                                  int4   = '0002'
                                  date   = '19911025'
                                  tstmp  = '19911025'
                                  flag   = ' '
                                  numc5  = '00014'
                                  tims   = '115500'
                                )
                                (
                                  char10 = 'CHAR10_015'
                                  string = 'ST_5'
                                  raw16  = lcl_mock_guid=>gc_raw_4
                                  int4   = '0003'
                                  date   = '19911025'
                                  tstmp  = '19911025'
                                  flag   = 'X'
                                  numc5  = '00015'
                                  tims   = '115500'
                                )
                                (
                                  char10 = 'CHAR10_016'
                                  string = 'ST_6'
                                  raw16  = lcl_mock_guid=>gc_raw_4
                                  int4   = '0004'
                                  date   = '19911025'
                                  tstmp  = '19911025'
                                  flag   = ' '
                                  numc5  = '00016'
                                  tims   = '115500'
                                )
                              ).

    gs_exp_ddic-mandt         = 'M01'.
    gs_exp_ddic-rolle         = 'ROLLE_0001'.
    gs_exp_ddic-process       = 'P01'.
    gs_exp_ddic-group_id      = 'GROUP_ID_0000001'.
    gs_exp_ddic-uname         = 'UNAME_000001'.
    gs_exp_ddic-identifier    = 'IDENTIFIER_000000001'.
    gs_exp_ddic-path          = 'PATH_00000000000000000000000000000000001'.
    gs_exp_ddic-attribute     = 'ATTRIBUTE_000000000000001'.
    gs_exp_ddic-editable      = 'X'.
    gs_exp_ddic-required      = 'X'.
    gs_exp_ddic-visible       = 'X'.

    gt_exp_ddic = VALUE #(
                        (
                          mandt         = 'M01'
                          rolle         = 'ROLLE_0001'
                          process       = 'P01'
                          group_id      = 'GROUP_ID_0000001'
                          uname         = 'UNAME_000001'
                          identifier    = 'IDENTIFIER_000000001'
                          path          = 'PATH_00000000000000000000000000000000001'
                          attribute     = 'ATTRIBUTE_000000000000001'
                          editable      = 'X'
                          required      = 'X'
                          visible       = 'X'
                         )

                        (
                          mandt         = 'M02'
                          rolle         = 'ROLLE_0002'
                          process       = 'P02'
                          group_id      = 'GROUP_ID_0000002'
                          uname         = 'UNAME_000002'
                          identifier    = 'IDENTIFIER_000000002'
                          path          = 'PATH_00000000000000000000000000000000002'
                          attribute     = 'ATTRIBUTE_000000000000002'
                          editable      = ' '
                          required      = ' '
                          visible       = ' '
                         )
                        (
                          mandt         = 'M03'
                          rolle         = 'ROLLE_0003'
                          process       = 'P03'
                          group_id      = 'GROUP_ID_0000003'
                          uname         = 'UNAME_000003'
                          identifier    = 'IDENTIFIER_000000003'
                          path          = 'PATH_00000000000000000000000000000000003'
                          attribute     = 'ATTRIBUTE_000000000000003'
                          editable      = 'X'
                          required      = 'X'
                          visible       = 'X'
                         )

                        (
                          mandt         = 'M04'
                          rolle         = 'ROLLE_0004'
                          process       = 'P04'
                          group_id      = 'GROUP_ID_0000004'
                          uname         = 'UNAME_000004'
                          identifier    = 'IDENTIFIER_000000004'
                          path          = 'PATH_00000000000000000000000000000000004'
                          attribute     = 'ATTRIBUTE_000000000000004'
                          editable      = ' '
                          required      = ' '
                          visible       = ' '
                         )
                    ).



  ENDMETHOD.


  METHOD simple_generator.

    mo_cut->create_test_data(
       CHANGING
         c_data = c_generated ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = c_generated
        exp                  = i_expected ).

  ENDMETHOD.

ENDCLASS.
