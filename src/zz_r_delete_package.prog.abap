*&---------------------------------------------------------------------*
*& Report zz_r_delete_package
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zz_r_delete_package.

INCLUDE zz_r_delete_package_i00.
INCLUDE zz_r_delete_package_i01.

##TODO " Support handling of transport requests

PARAMETERS: p_pckg TYPE devclass,
            p_subs AS CHECKBOX DEFAULT abap_true.

START-OF-SELECTION.
  DATA: lt_package     TYPE tt_package,
        lt_package_tmp TYPE tt_package,
        lt_object      TYPE tt_object.

  ziot_cl_bs_log=>get_instance( )->init(
    EXPORTING
      iv_subobject = 'BASIS'
      iv_extnumber = 'Delete Package'
      iv_lgnum     = zwmgc_lgnum ).

  CHECK p_pckg CN ' _0'.

  APPEND VALUE s_package( name = p_pckg ) TO lt_package.
  DATA(lt_r_package) = VALUE rseloption( ( sign   = 'I'
                                           option = 'EQ'
                                           low    = p_pckg ) ).

  IF p_subs EQ abap_true.

    WHILE lt_r_package IS NOT INITIAL.

      SELECT devclass, parentcl
        FROM tdevc
        WHERE parentcl IN @lt_r_package
        INTO TABLE @lt_package_tmp.

      APPEND LINES OF lt_package_tmp TO lt_package.

      lt_r_package = VALUE #( FOR <s_package> IN lt_package_tmp
                                ( sign   = 'I'
                                  option = 'EQ'
                                  low    = <s_package>-name ) ).

      CLEAR: lt_package_tmp.

    ENDWHILE.

  ENDIF.

  CHECK lt_package IS NOT INITIAL.

  lt_r_package = VALUE #( FOR <s_package> IN lt_package
                            ( sign   = 'I'
                              option = 'EQ'
                              low    = <s_package>-name ) ).

  SELECT obj_name, object
    FROM tadir
    WHERE pgmid    EQ @cv_pgmid
      AND devclass IN @lt_r_package
    INTO TABLE @lt_object.

  LOOP AT lt_object ASSIGNING FIELD-SYMBOL(<ls_object>).

    ##TODO " Create Factory Class and Method

    CASE <ls_object>-type.
      WHEN 'CLAS'.
        DATA(lo_object_clas) = NEW lcl_object_clas( is_object = <ls_object> ).
        lo_object_clas->delete( ).

      WHEN 'INTF'.
        DATA(lo_object_intf) = NEW lcl_object_intf( is_object = <ls_object> ).
        lo_object_intf->delete( ).

      WHEN 'FUGR'.
        DATA(lo_object_fugr) = NEW lcl_object_fugr( is_object = <ls_object> ).
        lo_object_fugr->delete( ).

      WHEN 'PROG'.
        DATA(lo_object_prog) = NEW lcl_object_prog( is_object = <ls_object> ).
        lo_object_prog->delete( ).

      WHEN 'TRAN'.
        DATA(lo_object_tran) = NEW lcl_object_tran( is_object = <ls_object> ).
        lo_object_tran->delete( ).

    ENDCASE.

  ENDLOOP.

  COMMIT WORK.

  SORT lt_package BY parent DESCENDING.

  DATA(lv_index) = 1.

  WHILE lt_package IS NOT INITIAL.

    ASSIGN lt_package[ lv_index ] TO FIELD-SYMBOL(<ls_package>).

    IF lv_index > 1.
      lv_index = 1.
    ENDIF.

    IF   <ls_package>-parent CO ' _0'
      OR <ls_package>-parent EQ p_pckg
      OR NOT line_exists( lt_package[ name = <ls_package>-parent ] ).

      APPEND <ls_package> TO lt_package_tmp.

    ELSE.

      lv_index = line_index( lt_package[ name = <ls_package>-parent ] ).

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
*         is_fields_for_change  =
*         iv_request            =
*  IMPORTING
*         es_devclass           =
*         ev_something_changed  =
*         ev_request            =
        EXCEPTIONS
          no_authorization      = 1
          invalid_devclass      = 2                " INvalid development class
          invalid_action        = 3                " Invalid action
          enqueue_failed        = 4
          db_access_error       = 5                " Database access error
          system_not_configured = 6
          OTHERS                = 7.

      IF sy-subrc <> 0.

        ziot_cl_bs_log=>get_instance( )->warning(
          EXPORTING
            iv_msg_txt = |Package { <ls_package_tmp>-name } with parent { <ls_package_tmp>-parent } couldn't be deleted.| ).

      ENDIF.

    ENDLOOP.

    lt_r_package = VALUE #( FOR <s_package> IN lt_package_tmp
                              ( sign   = 'I'
                                option = 'EQ'
                                low    = <s_package>-name ) ).
    DELETE lt_package WHERE name IN lt_r_package.

    CLEAR: lt_package_tmp, lt_r_package.

  ENDWHILE.

  ##TODO " Update parent package of selected package

*  DATA: lo_cross TYPE REF TO cl_wb_crossreference.
*
*  CREATE OBJECT lo_cross
*    EXPORTING
*      p_name    = <ls_object>-name
*      p_include = <ls_object>-name.
*
*  lo_cross->index_actualize( ).

  ziot_cl_bs_log=>get_instance( )->save( ).
