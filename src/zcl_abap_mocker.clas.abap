"! <strong>Die Klasse erzeugt Mock Daten</strong>
"! <br/>
"! Für Tabellen, Strukturen und Felder, egal ob der Typ im DDIC oder Lokal definiert ist.
"! <br/>
"! <em>Unterstützte Datentypen:</em>
"! <ul><li>Characters</li>
"! <li>Datum</li>
"! <li>Guids</li>
"! <li>Integer</li>
"! <li>NumC</li>
"! <li>Zeit</li>
"! <li>Timestamps</li>
"! <li>Characters</li></ul>
"! <br/>
"! <em>Abhängige Tabellen:</em>
"! <br/>
"! Es ist auch möglich die Feldinhalte von 2 Tabellen miteinander zu verknüpfen
"! Header - Item Beziehung
"! Siehe Beschreibung zum Feld IT_KEY_PAIR in der Methode CREATE_TEST_DATA
class ZCL_ABAP_MOCKER definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ty_key_pair,
        parent_key    TYPE fieldname,
        child_key     TYPE fieldname,
        child_2nd_key TYPE ztabap_mocker_fieldname,
      END OF ty_key_pair .
  types:
    tyt_key_pair TYPE STANDARD TABLE OF ty_key_pair .

      "! @parameter io_guid_creator | Nur vorhanden für Constructor Injection für UnitTests
  methods CONSTRUCTOR
    importing
      !IO_GUID_CREATOR type ref to ZIF_ABAP_MOCKER_UID_GENERATOR optional .
      "! Hier werden die Testdaten erzeugt
      "! @exception type_error |
      "! @parameter i_lines |   Wie viele Einträge sollen generiert werden? Bei einer Struktur nur 1 mitgeben
      "! @parameter it_key_pair | Mapping Tabelle der abhöngigen Tabellen
      "! Als PARENT_KEY gibt man das Schlüsselfeld der Kopftabelle an
      "! als CHILD_KEY das Schlüsselfeld, welches die Kopf und die Itemtabelle verknüpft
      "! Möchte man nun noch einen Positionszähler haben, gibt man in der Tabelle CHILD_2ND_KEY die zusätzlichen
      "! Schlüsselfelder der ITEM Tabelle an
      "! @parameter c_data |    Das zu füllende Element Tabelle/Feld/Struktur aus DDIC oder lokal
      "! @parameter instance |  Die Instanz selbst für Methodchaneing wenn man Tabellen aufeinander abhängig erzeuge möchte
  methods CREATE_TEST_DATA
    importing
      !I_LINES type INT4 default 4
      !IT_KEY_PAIR type TYT_KEY_PAIR optional
    changing
      !C_DATA type ANY
    returning
      value(INSTANCE) type ref to ZCL_ABAP_MOCKER
    exceptions
      TYPE_ERROR .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA: mv_lines        TYPE int4,
          mo_guid_creator TYPE REF TO zif_abap_mocker_uid_generator,
          mr_parent       TYPE REF TO data,
          mv_index        TYPE syst-index.

    METHODS get_structure
      IMPORTING
        ir_data         TYPE any
      RETURNING
        VALUE(ro_struc) TYPE REF TO cl_abap_structdescr
      EXCEPTIONS
        type_error.

    METHODS fill_values
      IMPORTING
        is_component TYPE abap_compdescr
        iv_index     TYPE i
        it_key_pair  TYPE tyt_key_pair OPTIONAL
        is_parent    TYPE any OPTIONAL
      EXPORTING
        ev_field     TYPE data
      RAISING
        cx_parameter_invalid_range.

    METHODS fill_characters
      IMPORTING
        iv_index     TYPE i
        is_component TYPE abap_compdescr
        is_key_pair  TYPE ty_key_pair
      EXPORTING
        ev_field     TYPE data
      RAISING
        cx_parameter_invalid_range.

    METHODS fill_integer
      IMPORTING
        iv_index    TYPE i
        is_key_pair TYPE ty_key_pair
      EXPORTING
        ev_field    TYPE data.
    METHODS fill_numc
      IMPORTING
        iv_index     TYPE i
        is_component TYPE abap_compdescr
        is_key_pair  TYPE ty_key_pair
      EXPORTING
        ev_field     TYPE data
      RAISING
        cx_parameter_invalid_range.

    METHODS fill_time
      IMPORTING
        is_key_pair TYPE ty_key_pair
      EXPORTING
        ev_field    TYPE data.

    METHODS fill_timestamp
      IMPORTING
        i_length    TYPE i
        is_key_pair TYPE ty_key_pair
      EXPORTING
        ev_field    TYPE data.

    METHODS fill_date
      IMPORTING
        is_key_pair TYPE ty_key_pair
      EXPORTING
        ev_field    TYPE data.

    METHODS fill_guid
      IMPORTING
        iv_index    TYPE i
        is_key_pair TYPE ty_key_pair
      EXPORTING
        ev_field    TYPE data.

ENDCLASS.



CLASS ZCL_ABAP_MOCKER IMPLEMENTATION.


  METHOD constructor.

    IF io_guid_creator IS NOT BOUND.
      mo_guid_creator = NEW zcl_abap_mocker_uid_generator( ).
    ELSE.
      mo_guid_creator = io_guid_creator.
    ENDIF.

  ENDMETHOD.


  METHOD create_test_data.

    DATA: lr_child_table TYPE REF TO data,
          lr_child_struc TYPE REF TO data.

    FIELD-SYMBOLS: <ls_child> TYPE any,
                   <lt_child> TYPE ANY TABLE.

    CLEAR: mv_index, mv_lines.

    mv_lines = i_lines.

    mo_guid_creator->create_guids( i_lines ).

    DATA(lo_struc) = get_structure( c_data ).

    IF cl_abap_datadescr=>get_data_type_kind( c_data )  CA 'uv'.

      CREATE DATA lr_child_struc LIKE c_data.
      ASSIGN lr_child_struc->* TO <ls_child>.

      FIELD-SYMBOLS <ls_parent> TYPE any .

      IF mr_parent IS INITIAL.
        ASSIGN lr_child_struc->* TO <ls_parent>.
      ELSE.
        ASSIGN mr_parent->* TO <ls_parent>.
      ENDIF.

      LOOP AT lo_struc->components ASSIGNING FIELD-SYMBOL(<ls_component>).

        ASSIGN COMPONENT <ls_component>-name OF STRUCTURE <ls_child> TO FIELD-SYMBOL(<lv_field>).

        fill_values(
              EXPORTING
                is_component = <ls_component>
                iv_index     = 1
                it_key_pair  = it_key_pair
                is_parent    = <ls_parent>
              IMPORTING
                ev_field = <lv_field> ).

      ENDLOOP.

      c_data = <ls_child>.

*     remember what we just created
      mr_parent = lr_child_struc.

    ELSE.


      CREATE DATA lr_child_table LIKE c_data.
      ASSIGN lr_child_table->* TO <lt_child>.

      IF it_key_pair IS NOT INITIAL.

        FIELD-SYMBOLS <lt_parent> TYPE ANY TABLE.

        ASSIGN mr_parent->* TO <lt_parent>.

        LOOP AT <lt_parent> ASSIGNING <ls_parent>.

          DO mv_lines TIMES.

            mv_index = sy-index.

            INSERT INITIAL LINE INTO TABLE <lt_child> ASSIGNING FIELD-SYMBOL(<ls_line>).

            LOOP AT lo_struc->components ASSIGNING <ls_component>.

              ASSIGN COMPONENT <ls_component>-name OF STRUCTURE <ls_line> TO <lv_field>.

              fill_values(
                    EXPORTING
                      is_component = <ls_component>
                      iv_index     = lines( <lt_child> )
                      it_key_pair  = it_key_pair
                      is_parent    = <ls_parent>
                    IMPORTING
                      ev_field = <lv_field> ).

            ENDLOOP.

          ENDDO.

        ENDLOOP.



      ELSE.

        DO mv_lines TIMES.

          INSERT INITIAL LINE INTO TABLE <lt_child> ASSIGNING <ls_line>.

          LOOP AT lo_struc->components ASSIGNING <ls_component>.

            ASSIGN COMPONENT <ls_component>-name OF STRUCTURE <ls_line> TO <lv_field>.

            fill_values(
                  EXPORTING
                    is_component = <ls_component>
                    iv_index     = lines( <lt_child> )
                    it_key_pair  = it_key_pair
                  IMPORTING
                    ev_field = <lv_field> ).

          ENDLOOP.

        ENDDO.

      ENDIF.


      c_data = <lt_child>.

*     remember what we just created
      mr_parent = lr_child_table.

    ENDIF.

*   jABAP Script:
    instance = me.


  ENDMETHOD.


  METHOD fill_characters.

    DATA lv_n_length TYPE i.
    DATA lr_n TYPE REF TO data.

    FIELD-SYMBOLS: <lv_n> TYPE data,
                   <lv_i> TYPE data.

    DATA(lv_real_length) =  is_component-length / 2.

    CASE lv_real_length.
      WHEN 1.
        IF iv_index MOD 2 = 0.
          ev_field = ' '.
        ELSE.
          ev_field = 'X'.
        ENDIF.

      WHEN 2.
        ev_field = iv_index.

      WHEN 3.

        DATA(lo_n_descr) = cl_abap_elemdescr=>get_n( p_length = 2 ).

        CREATE DATA lr_n TYPE HANDLE lo_n_descr.
        ASSIGN lr_n->* TO <lv_n>.

        <lv_n> = iv_index.

        ev_field = is_component-name+0(1) && <lv_n>.

      WHEN OTHERS.

        DATA(lv_name_length) = strlen( is_component-name ).

        lv_n_length = lv_real_length - lv_name_length - 1.     " 1 für den Unterstrich
        IF lv_n_length <= 0.
          lv_n_length = 1.
        ENDIF.

        lv_name_length = lv_real_length - lv_n_length - 1.     " 1 für den Unterstrich

        lo_n_descr = cl_abap_elemdescr=>get_n( p_length = lv_n_length ).

        CREATE DATA lr_n TYPE HANDLE lo_n_descr.
        ASSIGN lr_n->* TO <lv_n>.

        <lv_n> = iv_index.

        ev_field = is_component-name+0(lv_name_length) && '_' && <lv_n>.

    ENDCASE.

  ENDMETHOD.


  METHOD fill_date.

    ev_field = '19911025'.

  ENDMETHOD.


  METHOD fill_guid.

    ev_field = mo_guid_creator->mt_guids[ iv_index ].

  ENDMETHOD.


  METHOD fill_integer.

    DATA lr_i TYPE REF TO data.
    FIELD-SYMBOLS <lv_i> TYPE data.


    DATA(lo_i_descr) = cl_abap_elemdescr=>get_i( ).

    CREATE DATA lr_i TYPE HANDLE lo_i_descr.
    ASSIGN lr_i->* TO <lv_i>.

    <lv_i> = iv_index.

    ev_field = <lv_i>.

  ENDMETHOD.


  METHOD fill_numc.

    DATA lr_n TYPE REF TO data.
    FIELD-SYMBOLS <lv_n> TYPE data.

    DATA(lo_n_descr) = cl_abap_elemdescr=>get_n( p_length = is_component-length / 2 ).

    CREATE DATA lr_n TYPE HANDLE lo_n_descr.
    ASSIGN lr_n->* TO <lv_n>.

    <lv_n> = iv_index.

    ev_field = <lv_n>.

  ENDMETHOD.


  METHOD fill_time.

    ev_field = '115500'.

  ENDMETHOD.


  METHOD fill_timestamp.

    DATA(value_long) = '19911025115500'.
    ev_field = value_long(i_length).

  ENDMETHOD.


  METHOD fill_values.


    DATA(lv_index) = iv_index.

    TRY.

        DATA(ls_key_pair) = it_key_pair[ child_key = is_component-name ].

        ASSIGN COMPONENT ls_key_pair-parent_key OF STRUCTURE is_parent TO FIELD-SYMBOL(<lv_parent_key>).
        IF sy-subrc IS INITIAL.
          ev_field = <lv_parent_key>.
          RETURN.
        ENDIF.

      CATCH cx_sy_itab_line_not_found.


        LOOP AT it_key_pair ASSIGNING FIELD-SYMBOL(<ls_key_pair>).

          IF line_exists( <ls_key_pair>-child_2nd_key[ fieldname = is_component-name ] ).

            lv_index = mv_index.
            " mv_lines  aktueller Zeileneintrag in child
            " mv_index  aktuelle Position
            " iv_index

          ENDIF.

        ENDLOOP.


    ENDTRY.

    CASE is_component-type_kind.
      WHEN 'X'.
        fill_guid(
              EXPORTING
                iv_index     = lv_index
                is_key_pair  = ls_key_pair
              IMPORTING
                ev_field = ev_field ).

      WHEN 'D'.
        fill_date(
              EXPORTING
                is_key_pair  = ls_key_pair
              IMPORTING
                ev_field = ev_field  ).

      WHEN 'P'.
        fill_timestamp(
              EXPORTING
                i_length     = is_component-length
                is_key_pair  = ls_key_pair
              IMPORTING
                ev_field = ev_field ).

      WHEN 'T'.
        fill_time(
              EXPORTING
                is_key_pair  = ls_key_pair
              IMPORTING
                ev_field = ev_field ).

      WHEN 'N'.
        fill_numc(
              EXPORTING
                iv_index     = lv_index
                is_component = is_component
                is_key_pair  = ls_key_pair
              IMPORTING
                ev_field = ev_field ).

      WHEN 'I'.
        fill_integer(
              EXPORTING
                iv_index     = lv_index
                is_key_pair  = ls_key_pair
              IMPORTING
                ev_field = ev_field ).

      WHEN 'C' OR 'g'.
        fill_characters(
              EXPORTING
                iv_index     = lv_index
                is_component = is_component
                is_key_pair  = ls_key_pair
              IMPORTING
                ev_field = ev_field ).

      WHEN OTHERS.
    ENDCASE.


  ENDMETHOD.


  METHOD get_structure.

    DATA lr_table TYPE REF TO data.
    DATA lr_struc TYPE REF TO data.

    FIELD-SYMBOLS: <lt> TYPE ANY TABLE,
                   <ls> TYPE any.

    TRY.

        IF cl_abap_datadescr=>get_data_type_kind( ir_data )  CA 'uv'.
          ro_struc ?=  cl_abap_datadescr=>describe_by_data( p_data = ir_data ).

        ELSE.

          CREATE DATA lr_table LIKE ir_data.
          ASSIGN lr_table->* TO <lt>.

          CREATE DATA lr_struc LIKE LINE OF <lt>.
          ASSIGN lr_struc->* TO <ls>.

          ro_struc ?=  cl_abap_datadescr=>describe_by_data( p_data = <ls> ).

        ENDIF.

      CATCH cx_sy_move_cast_error.
        RAISE type_error.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
