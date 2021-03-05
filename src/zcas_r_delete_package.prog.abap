*&---------------------------------------------------------------------*
*& Report zcas_r_delete_package
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zcas_r_delete_package.

INCLUDE zcas_r_delete_package_i00.
INCLUDE zcas_r_delete_package_i01.
INCLUDE zcas_r_delete_package_i02.

##TODO " Support handling of transport requests

PARAMETERS: p_pckg TYPE devclass,
            p_subs AS CHECKBOX DEFAULT abap_true.

START-OF-SELECTION.
  DATA: lv_pckg_parent TYPE parentcl,
        lt_package     TYPE tt_package,
        lt_package_tmp TYPE tt_package,
        lt_object      TYPE tt_object.

  CHECK p_pckg CN ' _0'.

  lcl_program_manager=>get_instance( )->init( iv_pckg = p_pckg
                                              iv_subs = p_subs ).

  lcl_program_manager=>get_instance( )->determine_packages( ).

  lcl_program_manager=>get_instance( )->determine_objects( ).

  lcl_program_manager=>get_instance( )->delete_objects( ).

  lcl_program_manager=>get_instance( )->delete_packages( ).

  lcl_program_manager=>get_instance( )->update_index( ).
