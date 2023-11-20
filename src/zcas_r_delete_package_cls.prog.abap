*&---------------------------------------------------------------------*
*&  Include  zcas_r_delete_package_i01
*&---------------------------------------------------------------------*
CLASS lcl_object DEFINITION ABSTRACT.

  PUBLIC SECTION.
    DATA: ms_object TYPE s_object.

    METHODS constructor
      IMPORTING
        !is_object TYPE s_object.
    METHODS delete ABSTRACT.

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


*&---------------------------------------------------------------------*
*&  Include  zcas_r_delete_package_i02
*&---------------------------------------------------------------------*
CLASS lcl_application DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS execute
      IMPORTING
        iv_pckg  TYPE devclass
        iv_subs  TYPE abap_bool
        iv_tadir TYPE abap_bool.

  PROTECTED SECTION.
    CLASS-DATA: mv_pckg        TYPE devclass,
                mv_subs        TYPE abap_bool,
                mv_tadir       TYPE abap_bool,
                mv_pckg_parent TYPE parentcl,
                mt_package     TYPE tt_package,
                mt_object      TYPE tt_object.

    CLASS-METHODS update_index.
    CLASS-METHODS delete_packages.
    CLASS-METHODS delete_objects.
    CLASS-METHODS delete_tadir.
    CLASS-METHODS determine_objects.
    CLASS-METHODS determine_packages.

ENDCLASS.


CLASS lcl_application IMPLEMENTATION.

  METHOD execute.

    CHECK iv_pckg CN ' _0'.

    mv_pckg  = iv_pckg.
    mv_subs  = iv_subs.
    mv_tadir = iv_tadir.

    SELECT SINGLE parentcl
      FROM tdevc
      WHERE devclass EQ @mv_pckg
      INTO @mv_pckg_parent.

    determine_packages( ).

    determine_objects( ).

    delete_objects( ).

    delete_tadir( ).

    delete_packages( ).

    update_index( ).

  ENDMETHOD.


  METHOD update_index.

    CHECK mv_pckg_parent CN ' _0'.

    DATA(lo_wb_crossreference) = NEW cl_wb_crossreference(
      p_name    = CONV #( mv_pckg_parent )
      p_include = CONV #( mv_pckg_parent ) ).

    lo_wb_crossreference->index_actualize( ).

  ENDMETHOD.


  METHOD delete_packages.

    DATA: lt_package_tmp TYPE tt_package.

    CHECK mt_package IS NOT INITIAL.

    DATA(lv_index) = 1.
    SORT mt_package BY parent DESCENDING.

    WHILE mt_package IS NOT INITIAL.

      ASSIGN mt_package[ lv_index ] TO FIELD-SYMBOL(<ls_package>).

      IF lv_index > 1.
        lv_index = 1.
      ENDIF.

      IF   <ls_package>-parent CO ' _0'
        OR <ls_package>-parent EQ mv_pckg
        OR NOT line_exists( mt_package[ name = <ls_package>-parent ] ).

        APPEND <ls_package> TO lt_package_tmp.

      ELSE.

        lv_index = line_index( mt_package[ name = <ls_package>-parent ] ).

      ENDIF.

      CHECK lt_package_tmp IS NOT INITIAL.

      LOOP AT lt_package_tmp ASSIGNING FIELD-SYMBOL(<ls_package_tmp>).

        DATA(ls_devclass) = VALUE trdevclass( devclass = <ls_package_tmp>-name
                                              parentcl = <ls_package_tmp>-parent ).

        CALL FUNCTION 'TRINT_MODIFY_DEVCLASS'
          EXPORTING
            iv_action             = 'DELE'
            iv_dialog             = abap_false
            is_devclass           = ls_devclass
          EXCEPTIONS
            no_authorization      = 1
            invalid_devclass      = 2                " INvalid development class
            invalid_action        = 3                " Invalid action
            enqueue_failed        = 4
            db_access_error       = 5                " Database access error
            system_not_configured = 6
            OTHERS                = 7.

      ENDLOOP.

      DATA(lt_r_package) = VALUE rseloption( FOR <s_package> IN lt_package_tmp
                                               ( sign   = 'I'
                                                 option = 'EQ'
                                                 low    = <s_package>-name ) ).
      DELETE mt_package WHERE name IN lt_r_package.

      CLEAR: lt_package_tmp, lt_r_package.

    ENDWHILE.

  ENDMETHOD.


  METHOD delete_objects.

    CHECK mt_object IS NOT INITIAL.

    LOOP AT mt_object ASSIGNING FIELD-SYMBOL(<ls_object>).

      DATA(lv_class_name) = |lcl_object_{ <ls_object>-type }|.
      TRANSLATE lv_class_name TO UPPER CASE.

      TRY.
          DATA: lo_object_class TYPE REF TO lcl_object.
          CREATE OBJECT lo_object_class TYPE (lv_class_name)
            EXPORTING is_object = <ls_object>.

          lo_object_class->delete( ).

        CATCH cx_root.
          " Object not supported
      ENDTRY.

    ENDLOOP.

    IF mt_object IS NOT INITIAL.
      COMMIT WORK.
    ENDIF.

  ENDMETHOD.


  METHOD delete_tadir.

    CHECK mv_tadir EQ abap_true.

    DELETE FROM tadir WHERE devclass EQ @mv_pckg.
    COMMIT WORK.

  ENDMETHOD.


  METHOD determine_objects.

    CHECK mt_package IS NOT INITIAL.

    DATA(lt_r_package) = VALUE rseloption( FOR <s_package> IN mt_package
                                             ( sign   = 'I'
                                               option = 'EQ'
                                               low    = <s_package>-name ) ).

    SELECT obj_name, object
      FROM tadir
      WHERE pgmid    EQ @cv_pgmid
        AND devclass IN @lt_r_package
      INTO TABLE @mt_object.

  ENDMETHOD.


  METHOD determine_packages.

    APPEND VALUE s_package( name = mv_pckg ) TO mt_package.
    DATA(lt_r_package) = VALUE rseloption( ( sign   = 'I'
                                             option = 'EQ'
                                             low    = mv_pckg ) ).

    IF mv_subs EQ abap_true.

      WHILE lt_r_package IS NOT INITIAL.

        SELECT devclass, parentcl
          FROM tdevc
          WHERE parentcl IN @lt_r_package
          INTO TABLE @DATA(lt_package_tmp).

        APPEND LINES OF lt_package_tmp TO mt_package.

        lt_r_package = VALUE #( FOR <s_package_tmp> IN lt_package_tmp
                                  ( sign   = 'I'
                                    option = 'EQ'
                                    low    = <s_package_tmp>-devclass ) ).

        CLEAR: lt_package_tmp.

      ENDWHILE.

    ENDIF.

  ENDMETHOD.

ENDCLASS.
