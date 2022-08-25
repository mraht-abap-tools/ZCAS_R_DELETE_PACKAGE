*&---------------------------------------------------------------------*
*& Report zcas_r_delete_package
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zcas_r_delete_package.

INCLUDE zcas_r_delete_package_i00.
INCLUDE zcas_r_delete_package_i01.
INCLUDE zcas_r_delete_package_i02.

##TODO " Error handling + success message
##TODO " Support handling of transport requests

PARAMETERS: p_pckg  TYPE devclass,
            p_subs  AS CHECKBOX DEFAULT abap_true,
            p_tadir AS CHECKBOX DEFAULT abap_false.

START-OF-SELECTION.
  DATA: lv_pckg_parent TYPE parentcl,
        lt_package     TYPE tt_package,
        lt_package_tmp TYPE tt_package,
        lt_object      TYPE tt_object.

  CHECK p_pckg CN ' _0'.

  lcl_program_manager=>execute( iv_pckg  = p_pckg
                                iv_subs  = p_subs
                                iv_tadir = p_tadir ).
