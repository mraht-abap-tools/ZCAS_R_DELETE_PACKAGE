*&---------------------------------------------------------------------*
*& Report zcas_r_delete_package
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zcas_r_delete_package.

INCLUDE zcas_r_delete_package_top.
INCLUDE zcas_r_delete_package_sel.
INCLUDE zcas_r_delete_package_cls.

START-OF-SELECTION.
  lcl_application=>execute( iv_pckg  = p_pckg
                            iv_subs  = p_subs
                            iv_tadir = p_tadir ).
