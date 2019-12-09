INTERFACE zif_abap_mocker_uid_generator
  PUBLIC .

  DATA: mt_guids TYPE TABLE OF sysuuid_x16.

  METHODS: create_guids
    IMPORTING iv_lines TYPE syindex.

ENDINTERFACE.
