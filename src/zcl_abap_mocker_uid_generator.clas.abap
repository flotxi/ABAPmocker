CLASS zcl_abap_mocker_uid_generator DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: zif_abap_mocker_uid_generator.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abap_mocker_uid_generator IMPLEMENTATION.
  METHOD zif_abap_mocker_uid_generator~create_guids.
    TRY.
        DO iv_lines TIMES.
          DATA(lv_guid) = cl_system_uuid=>create_uuid_x16_static( ).
          INSERT lv_guid INTO TABLE zif_abap_mocker_uid_generator~mt_guids.
        ENDDO.
      CATCH cx_uuid_error.
        CLEAR: zif_abap_mocker_uid_generator~mt_guids.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
