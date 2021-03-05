*&---------------------------------------------------------------------*
*&  Include  zcas_r_delete_package_i00
*&---------------------------------------------------------------------*
CONSTANTS: cv_pgmid    TYPE pgmid     VALUE 'R3TR',
           cv_pck_type TYPE trobjtype VALUE 'DEVC'.

TYPES: BEGIN OF s_package,
         name   TYPE devclass,
         parent TYPE parentcl,
       END OF s_package.
TYPES: tt_package TYPE TABLE OF s_package.

TYPES: BEGIN OF s_object,
         name TYPE sobj_name,
         type TYPE trobjtype,
       END OF s_object.
TYPES: tt_object TYPE TABLE OF s_object.
