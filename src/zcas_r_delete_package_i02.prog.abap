*&---------------------------------------------------------------------*
*&  Include  zcas_r_delete_package_i02
*&---------------------------------------------------------------------*
CLASS lcl_program_manager DEFINITION FINAL.

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

  PRIVATE SECTION.
    CLASS-METHODS update_index.
    CLASS-METHODS delete_packages.
    CLASS-METHODS delete_objects.
    CLASS-METHODS delete_tadir.
    CLASS-METHODS determine_objects.
    CLASS-METHODS determine_packages.


ENDCLASS.


CLASS lcl_program_manager IMPLEMENTATION.

  METHOD execute.

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
