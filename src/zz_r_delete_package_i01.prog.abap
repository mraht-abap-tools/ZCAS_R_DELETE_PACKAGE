*&---------------------------------------------------------------------*
*&  Include  zz_r_delete_package_i01
*&---------------------------------------------------------------------*
CLASS lcl_object DEFINITION ABSTRACT.

  PUBLIC SECTION.
    DATA: ms_object TYPE s_object.

    METHODS constructor
      IMPORTING
        !is_object TYPE s_object.
    METHODS delete ABSTRACT.

  PROTECTED SECTION.


ENDCLASS.


CLASS lcl_object IMPLEMENTATION.

  METHOD constructor.

    ms_object = is_object.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_object_clas DEFINITION FINAL INHERITING FROM lcl_object.
  PUBLIC SECTION.
    METHODS: delete REDEFINITION.

  PRIVATE SECTION.
    CONSTANTS: mc_obj_type TYPE trobjtype VALUE 'CLAS'.

ENDCLASS.


CLASS lcl_object_clas IMPLEMENTATION.

  METHOD delete.

    DATA(clskey) = VALUE seoclskey( clsname = ms_object-name ).

    " SEO_INTERFACE_DELETE_COMPLETE
    CALL FUNCTION 'SEO_CLASS_DELETE_COMPLETE'
      EXPORTING
        clskey          = clskey
*       genflag         = space            " Generation Flag
        suppress_commit = abap_true
*       suppress_corr   =                  " Suppress Corr-Insert and Corr-Check
*  CHANGING
*       corrnr          =                  " Request/Task
      EXCEPTIONS
        not_existing    = 1
        is_interface    = 2
        db_error        = 3
        no_access       = 4
        other           = 5
        OTHERS          = 6.

  ENDMETHOD.

ENDCLASS.


CLASS lcl_object_intf DEFINITION FINAL INHERITING FROM lcl_object.
  PUBLIC SECTION.
    METHODS: delete REDEFINITION.

  PRIVATE SECTION.
    CONSTANTS: mc_obj_type TYPE trobjtype VALUE 'INTF'.

ENDCLASS.


CLASS lcl_object_intf IMPLEMENTATION.

  METHOD delete.

    DATA(intkey) = VALUE seoclskey( clsname = ms_object-name ).

    " SEO_INTERFACE_DELETE_COMPLETE
    CALL FUNCTION 'SEO_INTERFACE_DELETE_COMPLETE'
      EXPORTING
        intkey          = intkey
*       genflag         = space            " Generation Flag
        suppress_commit = abap_true
*       suppress_corr   =                  " Corr-Insert und Corr-Check unterdrücken
*       suppress_dialog =                  " X = no user interaction
*      CHANGING
*       corrnr          =                  " Request/Task
      EXCEPTIONS
        not_existing    = 1
        is_class        = 2
        db_error        = 3
        no_access       = 4
        other           = 5
        OTHERS          = 6.

  ENDMETHOD.

ENDCLASS.


CLASS lcl_object_fugr DEFINITION FINAL INHERITING FROM lcl_object.
  PUBLIC SECTION.
    METHODS: delete REDEFINITION.

  PRIVATE SECTION.
    CONSTANTS: mc_obj_type TYPE trobjtype VALUE 'FUGR'.

ENDCLASS.


CLASS lcl_object_fugr IMPLEMENTATION.

  METHOD delete.

    DATA(lv_area) = CONV rs38l_area( ms_object-name ).

    CALL FUNCTION 'RS_FUNCTION_POOL_DELETE'
      EXPORTING
        area                   = lv_area
        suppress_popups        = abap_true
        skip_progress_ind      = abap_true
*       corrnum                =
      EXCEPTIONS
        canceled_in_corr       = 1
        enqueue_system_failure = 2
        function_exist         = 3
        not_executed           = 4
        no_modify_permission   = 5
        no_show_permission     = 6
        permission_failure     = 7
        pool_not_exist         = 8
        cancelled              = 9
        OTHERS                 = 10.

  ENDMETHOD.

ENDCLASS.


CLASS lcl_object_prog DEFINITION FINAL INHERITING FROM lcl_object.
  PUBLIC SECTION.
    METHODS: delete REDEFINITION.

  PRIVATE SECTION.
    CONSTANTS: mc_obj_type TYPE trobjtype VALUE 'PROG'.

ENDCLASS.


CLASS lcl_object_prog IMPLEMENTATION.

  METHOD delete.

    CALL FUNCTION 'RS_DELETE_PROGRAM'
      EXPORTING
        program                    = ms_object-name
        suppress_popup             = abap_true
        force_delete_used_includes = abap_true
      EXCEPTIONS
        enqueue_lock               = 1
        object_not_found           = 2
        permission_failure         = 3
        reject_deletion            = 4
        OTHERS                     = 5.

  ENDMETHOD.

ENDCLASS.


CLASS lcl_object_tran DEFINITION FINAL INHERITING FROM lcl_object.
  PUBLIC SECTION.
    METHODS: delete REDEFINITION.

  PRIVATE SECTION.
    CONSTANTS: mc_obj_type TYPE trobjtype VALUE 'TRAN'.

ENDCLASS.


CLASS lcl_object_tran IMPLEMENTATION.

  METHOD delete.

    DATA(lv_transaction) = CONV tcode( ms_object-name ).

    CALL FUNCTION 'RPY_TRANSACTION_DELETE'
      EXPORTING
        transaction      = lv_transaction
      EXCEPTIONS
        not_excecuted    = 1
        object_not_found = 0
        OTHERS           = 3.

  ENDMETHOD.

ENDCLASS.
