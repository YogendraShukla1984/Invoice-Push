*&---------------------------------------------------------------------*
*& Include  ZSCM_INVOICE_PUSHC01
*&---------------------------------------------------------------------*
*& Class Definitions and Implementations
*& All business logic is implemented in classes (OOP approach)
*&---------------------------------------------------------------------*

" BEGIN: Cursor Generated Code

*----------------------------------------------------------------------*
* CLASS lcl_logger DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_logger DEFINITION.
  PUBLIC SECTION.
    METHODS: initialize,
             log_message IMPORTING iv_msgty TYPE sy-msgty
                                   iv_msgid TYPE sy-msgid DEFAULT lc_msgid
                                   iv_msgno TYPE sy-msgno
                                   iv_msgv1 TYPE sy-msgv1 OPTIONAL
                                   iv_msgv2 TYPE sy-msgv2 OPTIONAL
                                   iv_msgv3 TYPE sy-msgv3 OPTIONAL
                                   iv_msgv4 TYPE sy-msgv4 OPTIONAL,
             log_error   IMPORTING iv_text TYPE string,
             log_warning IMPORTING iv_text TYPE string,
             log_success IMPORTING iv_text TYPE string,
             log_info    IMPORTING iv_text TYPE string,
             save_log,
             display_log,
             get_log_handle RETURNING VALUE(rv_handle) TYPE balloghndl.

  PRIVATE SECTION.
    DATA: mv_log_handle TYPE balloghndl.
ENDCLASS.

*----------------------------------------------------------------------*
* CLASS lcl_json_builder DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_json_builder DEFINITION.
  PUBLIC SECTION.
    METHODS: build_invoice_json
               IMPORTING is_invoice       TYPE ty_invoice_data
               RETURNING VALUE(rv_json)   TYPE string.

  PRIVATE SECTION.
    METHODS: escape_json_string
               IMPORTING iv_input         TYPE string
               RETURNING VALUE(rv_output) TYPE string,
             format_date
               IMPORTING iv_date              TYPE datum
               RETURNING VALUE(rv_formatted)  TYPE string,
             format_decimal
               IMPORTING iv_value             TYPE any
               RETURNING VALUE(rv_formatted)  TYPE string.
ENDCLASS.

*----------------------------------------------------------------------*
* CLASS lcl_api_handler DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_api_handler DEFINITION.
  PUBLIC SECTION.
    METHODS: call_api
               IMPORTING iv_json            TYPE string
               RETURNING VALUE(rs_response) TYPE ty_api_response
               RAISING   zcx_api_error.

  PRIVATE SECTION.
    METHODS: create_http_client
               RETURNING VALUE(ro_client)   TYPE REF TO if_http_client
               RAISING   zcx_api_error,
             set_authentication
               IMPORTING io_http_client TYPE REF TO if_http_client,
             parse_ack_number
               IMPORTING iv_response          TYPE string
               RETURNING VALUE(rv_ack_number) TYPE zack_number.
ENDCLASS.

*----------------------------------------------------------------------*
* CLASS lcl_report DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_report DEFINITION.
  PUBLIC SECTION.
    METHODS: authorization_check
               RAISING zcx_no_authority,
             validate_company_code,
             validate_date_range,
             read_configuration
               RAISING zcx_config_missing,
             select_billing_data,
             process_invoices,
             display_summary,
             has_data RETURNING VALUE(rv_has_data) TYPE abap_bool,
             initialize_logger,
             finalize_logger.

  PRIVATE SECTION.
    DATA: lo_logger       TYPE REF TO lcl_logger,
          lo_json_builder TYPE REF TO lcl_json_builder,
          lo_api_handler  TYPE REF TO lcl_api_handler.

    METHODS: check_idempotency
               IMPORTING iv_vbeln         TYPE vbeln_vf
                         iv_fkdat         TYPE fkdat
                         iv_posnr         TYPE posnr
               RETURNING VALUE(rv_action) TYPE char10,
             build_json_payload
               IMPORTING is_invoice    TYPE ty_invoice_data
               RETURNING VALUE(rv_json) TYPE string,
             call_jwms_api
               IMPORTING iv_json            TYPE string
               RETURNING VALUE(rs_response) TYPE ty_api_response
               RAISING   zcx_api_error,
             persist_result
               IMPORTING is_invoice     TYPE ty_invoice_data
                         iv_status      TYPE char1
                         iv_json        TYPE string
                         iv_response    TYPE string OPTIONAL
                         iv_ack_no      TYPE zack_number OPTIONAL
                         iv_http_status TYPE i OPTIONAL,
             commit_package,
             enrich_customer_data,
             read_interface_records.
ENDCLASS.

*----------------------------------------------------------------------*
* CLASS lcl_logger IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_logger IMPLEMENTATION.

  METHOD initialize.
    DATA: lw_log       TYPE bal_s_log,
          lv_extnumber TYPE balnrext.

    " External number: YYYYMMDD_HHMMSS
    CONCATENATE sy-datum '_' sy-uzeit INTO lv_extnumber.

    " Initialize log structure
    lw_log-object     = lc_log_object.
    lw_log-subobject  = lc_log_subobject.
    lw_log-extnumber  = lv_extnumber.
    lw_log-aluser     = sy-uname.
    lw_log-alprog     = sy-repid.

    " Create application log
    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log      = lw_log
      IMPORTING
        e_log_handle = mv_log_handle.

    IF sy-subrc <> 0.
      MESSAGE 'Failed to create application log'(099) TYPE 'E'.
    ENDIF.

  ENDMETHOD.

  METHOD log_message.
    DATA: lw_msg TYPE bal_s_msg.

    " Build message structure
    lw_msg-msgty = iv_msgty.
    lw_msg-msgid = iv_msgid.
    lw_msg-msgno = iv_msgno.
    lw_msg-msgv1 = iv_msgv1.
    lw_msg-msgv2 = iv_msgv2.
    lw_msg-msgv3 = iv_msgv3.
    lw_msg-msgv4 = iv_msgv4.

    " Add message to application log
    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle = mv_log_handle
        i_s_msg      = lw_msg.

    " Also display in foreground if not batch
    IF sy-batch = abap_false.
      MESSAGE ID iv_msgid TYPE iv_msgty NUMBER iv_msgno
        WITH iv_msgv1 iv_msgv2 iv_msgv3 iv_msgv4.
    ENDIF.

  ENDMETHOD.

  METHOD log_error.
    DATA: lw_msg TYPE bal_s_msg.

    lw_msg-msgty = 'E'.
    lw_msg-msgid = '00'.
    lw_msg-msgno = '001'.
    lw_msg-msgv1 = iv_text.

    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle = mv_log_handle
        i_s_msg      = lw_msg.

    IF sy-batch = abap_false.
      MESSAGE iv_text TYPE 'I' DISPLAY LIKE 'E'.
    ENDIF.

  ENDMETHOD.

  METHOD log_warning.
    DATA: lw_msg TYPE bal_s_msg.

    lw_msg-msgty = 'W'.
    lw_msg-msgid = '00'.
    lw_msg-msgno = '001'.
    lw_msg-msgv1 = iv_text.

    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle = mv_log_handle
        i_s_msg      = lw_msg.

    IF sy-batch = abap_false.
      MESSAGE iv_text TYPE 'I' DISPLAY LIKE 'W'.
    ENDIF.

  ENDMETHOD.

  METHOD log_success.
    DATA: lw_msg TYPE bal_s_msg.

    lw_msg-msgty = 'S'.
    lw_msg-msgid = '00'.
    lw_msg-msgno = '001'.
    lw_msg-msgv1 = iv_text.

    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle = mv_log_handle
        i_s_msg      = lw_msg.

    IF sy-batch = abap_false.
      MESSAGE iv_text TYPE 'S'.
    ENDIF.

  ENDMETHOD.

  METHOD log_info.
    DATA: lw_msg TYPE bal_s_msg.

    lw_msg-msgty = 'I'.
    lw_msg-msgid = '00'.
    lw_msg-msgno = '001'.
    lw_msg-msgv1 = iv_text.

    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle = mv_log_handle
        i_s_msg      = lw_msg.

  ENDMETHOD.

  METHOD save_log.
    DATA: lt_log_handle TYPE bal_t_logh.

    APPEND mv_log_handle TO lt_log_handle.

    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_client         = sy-mandt
        i_save_all       = abap_true
        i_t_log_handle   = lt_log_handle
      EXCEPTIONS
        log_not_found    = 1
        save_not_allowed = 2
        numbering_error  = 3
        OTHERS           = 4.

    IF sy-subrc = 0.
      MESSAGE 'Application log saved successfully'(030) TYPE 'S'.
    ELSE.
      MESSAGE 'Failed to save application log'(098) TYPE 'W'.
    ENDIF.

  ENDMETHOD.

  METHOD display_log.
    DATA: lt_log_handle TYPE bal_t_logh.

    APPEND mv_log_handle TO lt_log_handle.

    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
        i_t_log_handle = lt_log_handle.

  ENDMETHOD.

  METHOD get_log_handle.
    rv_handle = mv_log_handle.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* CLASS lcl_json_builder IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_json_builder IMPLEMENTATION.

  METHOD build_invoice_json.
    DATA: lv_json       TYPE string,
          lv_date       TYPE string,
          lv_quantity   TYPE string,
          lv_value      TYPE string,
          lv_kunrg_name TYPE string,
          lv_kunag_name TYPE string.

    " Format values
    lv_date = format_date( is_invoice-fkdat ).
    lv_quantity = format_decimal( is_invoice-fkimg ).
    lv_value = format_decimal( is_invoice-netwr ).
    lv_kunrg_name = escape_json_string( is_invoice-kunrg_name ).
    lv_kunag_name = escape_json_string( is_invoice-kunag_name ).

    " Build JSON payload
    CONCATENATE '{'
      '"AccountId":"' gv_account_id '",'
      '"BusinessId":"' gv_business_id '",'
      '"SubBusinessId":"' gv_sub_business_id '",'
      '"Invoice":{'
        '"BillingDocument":"' is_invoice-vbeln '",'
        '"BillingDate":"' lv_date '",'
        '"CompanyCode":"' is_invoice-bukrs '",'
        '"Division":"' is_invoice-spart '",'
        '"SalesOrganization":"' is_invoice-vkorg '",'
        '"BillToCustomer":"' is_invoice-kunrg '",'
        '"BillToName":"' lv_kunrg_name '",'
        '"SoldToCustomer":"' is_invoice-kunag '",'
        '"SoldToName":"' lv_kunag_name '",'
        '"Items":['
          '{'
            '"ItemNumber":"' is_invoice-posnr '",'
            '"Material":"' is_invoice-matnr '",'
            '"Quantity":"' lv_quantity '",'
            '"UnitOfMeasure":"' is_invoice-vrkme '",'
            '"NetValue":"' lv_value '",'
            '"Currency":"' is_invoice-waerk '"'
          '}'
        ']'
      '}'
    '}' INTO rv_json.

  ENDMETHOD.

  METHOD escape_json_string.
    DATA: lv_output TYPE string.

    lv_output = iv_input.

    " Escape special characters
    REPLACE ALL OCCURRENCES OF '\' IN lv_output WITH '\\'.
    REPLACE ALL OCCURRENCES OF '"' IN lv_output WITH '\"'.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline
      IN lv_output WITH '\n'.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf
      IN lv_output WITH '\n'.

    rv_output = lv_output.

  ENDMETHOD.

  METHOD format_date.
    DATA: lv_year(4)  TYPE c,
          lv_month(2) TYPE c,
          lv_day(2)   TYPE c.

    " Convert SAP date (YYYYMMDD) to JSON format (YYYY-MM-DD)
    lv_year  = iv_date+0(4).
    lv_month = iv_date+4(2).
    lv_day   = iv_date+6(2).

    CONCATENATE lv_year '-' lv_month '-' lv_day INTO rv_formatted.

  ENDMETHOD.

  METHOD format_decimal.
    DATA: lv_string TYPE string.

    lv_string = iv_value.
    CONDENSE lv_string NO-GAPS.

    rv_formatted = lv_string.

  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* CLASS lcl_api_handler IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_api_handler IMPLEMENTATION.

  METHOD call_api.
    DATA: lo_http_client TYPE REF TO if_http_client,
          lv_response    TYPE string,
          lv_http_code   TYPE i.

    " Create HTTP client
    lo_http_client = create_http_client( ).

    " Set authentication
    set_authentication( io_http_client = lo_http_client ).

    " Set content type
    lo_http_client->request->set_header_field(
      name  = 'Content-Type'
      value = 'application/json' ).

    " Set request body
    lo_http_client->request->set_cdata( iv_json ).

    " Set method to POST
    lo_http_client->request->set_method( 'POST' ).

    " Send request
    CALL METHOD lo_http_client->send
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4.

    IF sy-subrc <> 0.
      lo_http_client->close( ).
      RAISE EXCEPTION TYPE zcx_api_error
        EXPORTING
          textid = zcx_api_error=>api_send_failed.
    ENDIF.

    " Receive response
    CALL METHOD lo_http_client->receive
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4.

    IF sy-subrc <> 0.
      lo_http_client->close( ).
      RAISE EXCEPTION TYPE zcx_api_error
        EXPORTING
          textid = zcx_api_error=>api_receive_failed.
    ENDIF.

    " Get response
    lv_response = lo_http_client->response->get_cdata( ).
    lo_http_client->response->get_status(
      IMPORTING
        code = lv_http_code ).

    " Parse response
    rs_response-http_status = lv_http_code.
    rs_response-body = lv_response.
    rs_response-ack_number = parse_ack_number( iv_response = lv_response ).

    " Close connection
    lo_http_client->close( ).

  ENDMETHOD.

  METHOD create_http_client.
    DATA: lv_url TYPE string.

    lv_url = gv_api_endpoint.

    CALL METHOD cl_http_client=>create_by_url
      EXPORTING
        url                = lv_url
      IMPORTING
        client             = ro_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_api_error
        EXPORTING
          textid = zcx_api_error=>http_client_creation_failed.
    ENDIF.

    " Set timeout (30 seconds or from config)
    IF gv_api_timeout IS INITIAL.
      ro_client->propertytype_logon_popup = ro_client->co_disabled.
      ro_client->timeout = 30.
    ELSE.
      ro_client->timeout = gv_api_timeout.
    ENDIF.

  ENDMETHOD.

  METHOD set_authentication.
    DATA: lv_auth_string TYPE string,
          lv_encoded     TYPE string.

    CASE gv_api_auth_type.
      WHEN 'OAUTH'.
        " OAuth Bearer token
        CONCATENATE 'Bearer' gv_api_token INTO lv_auth_string
          SEPARATED BY space.
        io_http_client->request->set_header_field(
          name  = 'Authorization'
          value = lv_auth_string ).

      WHEN 'BASIC'.
        " Basic Authentication
        CONCATENATE gv_api_username ':' gv_api_password INTO lv_auth_string.

        " Base64 encode
        CALL FUNCTION 'SCMS_BASE64_ENCODE_STR'
          EXPORTING
            input  = lv_auth_string
          IMPORTING
            output = lv_encoded.

        CONCATENATE 'Basic' lv_encoded INTO lv_auth_string
          SEPARATED BY space.
        io_http_client->request->set_header_field(
          name  = 'Authorization'
          value = lv_auth_string ).

      WHEN 'APIKEY'.
        " API Key in header
        io_http_client->request->set_header_field(
          name  = 'X-API-Key'
          value = gv_api_token ).

    ENDCASE.

  ENDMETHOD.

  METHOD parse_ack_number.
    DATA: lv_json   TYPE string,
          lv_ack_no TYPE string.

    lv_json = iv_response.

    " Simple JSON parsing (assuming response format: {"ackNumber": "12345"})
    " For production, consider using /UI2/CL_JSON or similar JSON parser
    FIND REGEX '"ackNumber"\s*:\s*"([^"]+)"' IN lv_json
      SUBMATCHES lv_ack_no.

    IF sy-subrc = 0.
      rv_ack_number = lv_ack_no.
    ELSE.
      " Try alternative field name
      FIND REGEX '"acknowledgementNumber"\s*:\s*"([^"]+)"' IN lv_json
        SUBMATCHES lv_ack_no.
      IF sy-subrc = 0.
        rv_ack_number = lv_ack_no.
      ENDIF.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* CLASS lcl_report IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_report IMPLEMENTATION.

  METHOD authorization_check.
    " Check S_TCODE authorization
    AUTHORITY-CHECK OBJECT 'S_TCODE'
                    ID 'TCD' FIELD 'ZSCM_INVOICE_PUSH'.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_no_authority
        EXPORTING
          textid = zcx_no_authority=>not_authorized.
    ENDIF.

  ENDMETHOD.

  METHOD validate_company_code.
    DATA: lv_bukrs TYPE t001-bukrs.

    " Check if company code exists in T001
    SELECT SINGLE bukrs
      FROM t001
      INTO lv_bukrs
      WHERE bukrs = p_bukrs.

    IF sy-subrc <> 0.
      MESSAGE e003(lc_msgid) WITH p_bukrs.
    ENDIF.

  ENDMETHOD.

  METHOD validate_date_range.
    " Validate date range: From <= To
    IF p_fkdat_f > p_fkdat_t.
      MESSAGE e002(lc_msgid).
    ENDIF.

  ENDMETHOD.

  METHOD read_configuration.
    DATA: lt_config    TYPE tt_config,
          lw_config    TYPE ty_config,
          lw_api_config TYPE ty_api_config,
          lv_fkart     TYPE fkart.

    " Read billing types from ZLOG_EXEC_VAR
    SELECT name value value1 value2 active
      FROM zlog_exec_var
      INTO TABLE lt_config
      WHERE name = 'ZSCM_GET_INV_BILLINGTYPE'
        AND active = abap_true.

    IF sy-subrc <> 0 OR lt_config IS INITIAL.
      RAISE EXCEPTION TYPE zcx_config_missing
        EXPORTING
          textid = zcx_config_missing=>billing_types_missing.
    ENDIF.

    " Extract billing types
    LOOP AT lt_config INTO lw_config.
      lv_fkart = lw_config-value.
      APPEND lv_fkart TO gt_billing_types.
    ENDLOOP.

    " Read Account ID, Business ID, Sub-Business ID
    CLEAR lt_config.
    SELECT name value value1 value2 active
      FROM zlog_exec_var
      INTO TABLE lt_config
      WHERE name IN ('ZSCM_ACCOUNTID', 'ZSCM_BUSINESSID', 'ZSCM_SUB_BUSINESSID')
        AND active = abap_true.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_config_missing
        EXPORTING
          textid = zcx_config_missing=>ids_missing.
    ENDIF.

    LOOP AT lt_config INTO lw_config.
      CASE lw_config-name.
        WHEN 'ZSCM_ACCOUNTID'.
          gv_account_id = lw_config-value.
        WHEN 'ZSCM_BUSINESSID'.
          gv_business_id = lw_config-value.
        WHEN 'ZSCM_SUB_BUSINESSID'.
          gv_sub_business_id = lw_config-value.
      ENDCASE.
    ENDLOOP.

    " Validate all IDs are populated
    IF gv_account_id IS INITIAL OR
       gv_business_id IS INITIAL OR
       gv_sub_business_id IS INITIAL.
      RAISE EXCEPTION TYPE zcx_config_missing
        EXPORTING
          textid = zcx_config_missing=>ids_missing.
    ENDIF.

    " Read API configuration from ZWSO2APIDTL
    SELECT SINGLE interface_name endpoint_url auth_type
                  username password token timeout active
      FROM zwso2apidtl
      INTO lw_api_config
      WHERE interface_name = lc_interface_type
        AND active = abap_true.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_config_missing
        EXPORTING
          textid = zcx_config_missing=>api_config_missing.
    ENDIF.

    " Populate global API variables
    gv_api_endpoint   = lw_api_config-endpoint_url.
    gv_api_auth_type  = lw_api_config-auth_type.
    gv_api_username   = lw_api_config-username.
    gv_api_password   = lw_api_config-password.
    gv_api_token      = lw_api_config-token.
    gv_api_timeout    = lw_api_config-timeout.

    " Validate API endpoint
    IF gv_api_endpoint IS INITIAL.
      RAISE EXCEPTION TYPE zcx_config_missing
        EXPORTING
          textid = zcx_config_missing=>api_endpoint_missing.
    ENDIF.

    " Log successful configuration read
    lo_logger->log_info( 'Configuration read successfully' ).

  ENDMETHOD.

  METHOD select_billing_data.
    DATA: lt_vbeln  TYPE STANDARD TABLE OF vbeln_vf,
          lv_vbeln  TYPE vbeln_vf,
          lv_message TYPE string,
          lv_header_count TYPE string,
          lv_item_count   TYPE string.

    " Select billing headers
    SELECT vbeln fkdat bukrs spart vkorg fkart kunrg kunag
      FROM vbrk
      INTO CORRESPONDING FIELDS OF TABLE gt_billing_headers
      WHERE fkart IN gt_billing_types
        AND fkart IN s_fkart
        AND fkdat BETWEEN p_fkdat_f AND p_fkdat_t
        AND bukrs = p_bukrs
        AND vkorg IN s_vkorg
        AND fksto = space.

    IF sy-subrc <> 0 OR gt_billing_headers IS INITIAL.
      lo_logger->log_info( 'No billing headers found' ).
      RETURN.
    ENDIF.

    " Collect VBELN for items selection
    LOOP AT gt_billing_headers INTO gw_billing_header.
      lv_vbeln = gw_billing_header-vbeln.
      APPEND lv_vbeln TO lt_vbeln.
    ENDLOOP.

    SORT lt_vbeln.
    DELETE ADJACENT DUPLICATES FROM lt_vbeln.

    " Select billing items using FOR ALL ENTRIES
    IF lt_vbeln IS NOT INITIAL.
      SELECT vbrp~vbeln vbrp~posnr vbrk~fkdat
             vbrp~matnr vbrp~fkimg vbrp~vrkme
             vbrp~netwr vbrp~waerk
        FROM vbrp
        INNER JOIN vbrk ON vbrp~vbeln = vbrk~vbeln
        INTO CORRESPONDING FIELDS OF TABLE gt_billing_items
        FOR ALL ENTRIES IN lt_vbeln
        WHERE vbrp~vbeln = lt_vbeln-table_line.
    ENDIF.

    IF sy-subrc <> 0 OR gt_billing_items IS INITIAL.
      lo_logger->log_warning( 'No billing items found' ).
      RETURN.
    ENDIF.

    " Enrich customer data
    enrich_customer_data( ).

    " Log data selection summary
    lv_header_count = lines( gt_billing_headers ).
    lv_item_count = lines( gt_billing_items ).

    CONCATENATE 'Selected' lv_header_count 'headers and'
                lv_item_count 'items'
      INTO lv_message SEPARATED BY space.

    lo_logger->log_info( lv_message ).

  ENDMETHOD.

  METHOD enrich_customer_data.
    DATA: lt_kunnr TYPE STANDARD TABLE OF kunnr.

    " Collect unique customer numbers
    LOOP AT gt_billing_headers INTO gw_billing_header.
      APPEND gw_billing_header-kunrg TO lt_kunnr.
      APPEND gw_billing_header-kunag TO lt_kunnr.
    ENDLOOP.

    SORT lt_kunnr.
    DELETE ADJACENT DUPLICATES FROM lt_kunnr.

    " Read customer names
    IF lt_kunnr IS NOT INITIAL.
      SELECT kunnr name1
        FROM kna1
        INTO TABLE gt_customers
        FOR ALL ENTRIES IN lt_kunnr
        WHERE kunnr = lt_kunnr-table_line.

      IF sy-subrc <> 0.
        lo_logger->log_warning( 'No customer data found' ).
      ENDIF.
    ENDIF.

    " Update headers with customer names
    LOOP AT gt_billing_headers ASSIGNING FIELD-SYMBOL(<fs_header>).
      READ TABLE gt_customers INTO gw_customer
        WITH KEY kunnr = <fs_header>-kunrg.
      IF sy-subrc = 0.
        <fs_header>-kunrg_name = gw_customer-name1.
      ENDIF.

      READ TABLE gt_customers INTO gw_customer
        WITH KEY kunnr = <fs_header>-kunag.
      IF sy-subrc = 0.
        <fs_header>-kunag_name = gw_customer-name1.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD has_data.
    IF gt_billing_items IS NOT INITIAL.
      rv_has_data = abap_true.
    ELSE.
      rv_has_data = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD initialize_logger.
    " Create logger instance
    CREATE OBJECT lo_logger.
    lo_logger->initialize( ).

    " Create JSON builder instance
    CREATE OBJECT lo_json_builder.

    " Create API handler instance
    CREATE OBJECT lo_api_handler.

    " Initialize counters
    CLEAR: gv_processed_count,
           gv_success_count,
           gv_failed_count,
           gv_skipped_count,
           gv_package_count.

  ENDMETHOD.

  METHOD finalize_logger.
    " Save log in background mode
    IF sy-batch = abap_true.
      lo_logger->save_log( ).
    ELSE.
      " Display log in foreground if errors occurred
      IF gv_failed_count > 0.
        lo_logger->display_log( ).
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD read_interface_records.
    DATA: lt_vbeln TYPE STANDARD TABLE OF vbeln_vf,
          lv_vbeln TYPE vbeln_vf.

    " Build range table for VBELN
    LOOP AT gt_billing_items INTO gw_billing_item.
      lv_vbeln = gw_billing_item-vbeln.
      APPEND lv_vbeln TO lt_vbeln.
    ENDLOOP.

    SORT lt_vbeln.
    DELETE ADJACENT DUPLICATES FROM lt_vbeln.

    " Bulk read interface records
    IF lt_vbeln IS NOT INITIAL.
      SELECT *
        FROM zscm_invoice_interface
        INTO TABLE gt_interface_records
        FOR ALL ENTRIES IN lt_vbeln
        WHERE interface_type = lc_interface_type
          AND vbeln = lt_vbeln-table_line.
    ENDIF.

    " Sort by key + counter descending for latest record lookup
    SORT gt_interface_records BY vbeln fkdat posnr counter DESCENDING.

  ENDMETHOD.

  METHOD check_idempotency.
    DATA: lw_interface TYPE zscm_invoice_interface.

    " Read latest record for this invoice item
    READ TABLE gt_interface_records INTO lw_interface
      WITH KEY vbeln = iv_vbeln
               fkdat = iv_fkdat
               posnr = iv_posnr
      BINARY SEARCH.

    IF sy-subrc = 0.
      " Record exists - check status
      CASE lw_interface-status.
        WHEN lc_status_success.
          " Successfully pushed - skip
          rv_action = lc_action_skip.

        WHEN lc_status_skipped.
          " Business exclusion - skip
          rv_action = lc_action_skip.

        WHEN lc_status_failed.
          " Failed - check retry count
          IF lw_interface-counter >= lc_max_retry.
            " Max retry reached - skip
            rv_action = lc_action_skip.
            lo_logger->log_warning(
              'Max retry count reached for invoice ' && iv_vbeln ).
          ELSE.
            " Retry allowed
            rv_action = lc_action_retry.
          ENDIF.

        WHEN lc_status_test.
          " Test mode record - can be reprocessed
          rv_action = lc_action_push.

        WHEN OTHERS.
          " Unknown status - treat as new
          rv_action = lc_action_push.

      ENDCASE.

    ELSE.
      " No record exists - push new invoice
      rv_action = lc_action_push.

    ENDIF.

  ENDMETHOD.

  METHOD build_json_payload.
    rv_json = lo_json_builder->build_invoice_json( is_invoice = is_invoice ).
  ENDMETHOD.

  METHOD call_jwms_api.
    rs_response = lo_api_handler->call_api( iv_json = iv_json ).
  ENDMETHOD.

  METHOD persist_result.
    DATA: lw_interface TYPE zscm_invoice_interface,
          lv_counter   TYPE i,
          lv_counter_str TYPE string.

    " Read existing record with highest counter
    SELECT SINGLE *
      FROM zscm_invoice_interface
      INTO lw_interface
      WHERE mandt = sy-mandt
        AND interface_type = lc_interface_type
        AND vbeln = is_invoice-vbeln
        AND fkdat = is_invoice-fkdat
        AND posnr = is_invoice-posnr
      ORDER BY counter DESCENDING.

    IF sy-subrc = 0.
      " Record exists - UPDATE scenario (retry)
      lv_counter = lw_interface-counter + 1.

      " Check max retry limit
      IF lv_counter >= lc_max_retry.
        " Max retry reached - log and return
        lo_logger->log_message(
          iv_msgty = 'E'
          iv_msgno = '050'
          iv_msgv1 = is_invoice-vbeln ).
        RETURN.
      ENDIF.

      " Update existing record
      UPDATE zscm_invoice_interface
        SET status       = iv_status
            counter      = lv_counter
            json_payload = iv_json
            api_response = iv_response
            ack_number   = iv_ack_no
            http_status  = iv_http_status
            aedat        = sy-datum
            aezet        = sy-uzeit
            aenam        = sy-uname
        WHERE mandt = sy-mandt
          AND interface_type = lc_interface_type
          AND vbeln = is_invoice-vbeln
          AND fkdat = is_invoice-fkdat
          AND posnr = is_invoice-posnr
          AND counter = lw_interface-counter.

      IF sy-subrc = 0.
        " Log successful update
        lv_counter_str = lv_counter.
        lo_logger->log_message(
          iv_msgty = 'S'
          iv_msgno = '060'
          iv_msgv1 = is_invoice-vbeln
          iv_msgv2 = lv_counter_str ).
      ELSE.
        " Log update failure
        lo_logger->log_error(
          'Failed to update interface table for invoice ' && is_invoice-vbeln ).
      ENDIF.

    ELSE.
      " No existing record - INSERT scenario (new invoice)
      lv_counter = 1.

      " Build new record
      CLEAR lw_interface.
      lw_interface-mandt            = sy-mandt.
      lw_interface-interface_type   = lc_interface_type.
      lw_interface-vbeln            = is_invoice-vbeln.
      lw_interface-fkdat            = is_invoice-fkdat.
      lw_interface-posnr            = is_invoice-posnr.
      lw_interface-counter          = lv_counter.
      lw_interface-status           = iv_status.
      lw_interface-division         = is_invoice-spart.
      lw_interface-bukrs            = is_invoice-bukrs.
      lw_interface-kunrg            = is_invoice-kunrg.
      lw_interface-kunrg_name       = is_invoice-kunrg_name.
      lw_interface-kunag            = is_invoice-kunag.
      lw_interface-kunag_name       = is_invoice-kunag_name.
      lw_interface-accountid        = gv_account_id.
      lw_interface-businessid       = gv_business_id.
      lw_interface-sub_businessid   = gv_sub_business_id.
      lw_interface-json_payload     = iv_json.
      lw_interface-api_response     = iv_response.
      lw_interface-ack_number       = iv_ack_no.
      lw_interface-http_status      = iv_http_status.
      lw_interface-erdat            = sy-datum.
      lw_interface-erzet            = sy-uzeit.
      lw_interface-ernam            = sy-uname.
      lw_interface-aedat            = sy-datum.
      lw_interface-aezet            = sy-uzeit.
      lw_interface-aenam            = sy-uname.

      " Insert new record
      INSERT zscm_invoice_interface FROM lw_interface.

      IF sy-subrc = 0.
        " Log successful insert
        lo_logger->log_message(
          iv_msgty = 'S'
          iv_msgno = '061'
          iv_msgv1 = is_invoice-vbeln ).
      ELSE.
        " Log insert failure
        lo_logger->log_error(
          'Failed to insert interface table for invoice ' && is_invoice-vbeln ).
      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD commit_package.
    DATA: lv_count_str TYPE string.

    " Commit work
    COMMIT WORK AND WAIT.

    " Log package commit
    lv_count_str = gv_package_count.

    lo_logger->log_message(
      iv_msgty = 'I'
      iv_msgno = '040'
      iv_msgv1 = lv_count_str ).

    " Reset package counter
    gv_package_count = 0.

  ENDMETHOD.

  METHOD process_invoices.
    DATA: lv_action      TYPE char10,
          lv_json        TYPE string,
          lw_response    TYPE ty_api_response,
          lv_status      TYPE char1,
          lw_invoice     TYPE ty_invoice_data,
          lo_exception   TYPE REF TO zcx_api_error,
          lv_percentage  TYPE i,
          lv_total_items TYPE i.

    " Read existing interface records (bulk read for performance)
    read_interface_records( ).

    lv_total_items = lines( gt_billing_items ).

    " Process each billing item
    LOOP AT gt_billing_items INTO gw_billing_item.

      ADD 1 TO gv_processed_count.

      " Show progress indicator in foreground
      IF sy-batch = abap_false.
        lv_percentage = ( gv_processed_count * 100 ) / lv_total_items.
        CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
          EXPORTING
            percentage = lv_percentage
            text       = 'Processing invoice'(004).
      ENDIF.

      " Build invoice data structure
      CLEAR lw_invoice.
      READ TABLE gt_billing_headers INTO gw_billing_header
        WITH KEY vbeln = gw_billing_item-vbeln.

      IF sy-subrc = 0.
        lw_invoice-vbeln      = gw_billing_item-vbeln.
        lw_invoice-fkdat      = gw_billing_item-fkdat.
        lw_invoice-posnr      = gw_billing_item-posnr.
        lw_invoice-bukrs      = gw_billing_header-bukrs.
        lw_invoice-spart      = gw_billing_header-spart.
        lw_invoice-vkorg      = gw_billing_header-vkorg.
        lw_invoice-kunrg      = gw_billing_header-kunrg.
        lw_invoice-kunrg_name = gw_billing_header-kunrg_name.
        lw_invoice-kunag      = gw_billing_header-kunag.
        lw_invoice-kunag_name = gw_billing_header-kunag_name.
        lw_invoice-matnr      = gw_billing_item-matnr.
        lw_invoice-fkimg      = gw_billing_item-fkimg.
        lw_invoice-vrkme      = gw_billing_item-vrkme.
        lw_invoice-netwr      = gw_billing_item-netwr.
        lw_invoice-waerk      = gw_billing_item-waerk.
      ELSE.
        " Header not found - skip
        ADD 1 TO gv_skipped_count.
        lo_logger->log_warning( 'Header not found for item ' && gw_billing_item-vbeln ).
        CONTINUE.
      ENDIF.

      " Skip if customer data missing
      IF lw_invoice-kunrg_name IS INITIAL OR
         lw_invoice-kunag_name IS INITIAL.
        ADD 1 TO gv_skipped_count.
        lo_logger->log_message(
          iv_msgty = 'W'
          iv_msgno = '005'
          iv_msgv1 = lw_invoice-vbeln ).
        CONTINUE.
      ENDIF.

      " Check idempotency
      lv_action = check_idempotency(
        iv_vbeln = lw_invoice-vbeln
        iv_fkdat = lw_invoice-fkdat
        iv_posnr = lw_invoice-posnr ).

      CASE lv_action.
        WHEN lc_action_skip.
          " Already processed or excluded
          ADD 1 TO gv_skipped_count.
          lo_logger->log_message(
            iv_msgty = 'I'
            iv_msgno = '010'
            iv_msgv1 = lw_invoice-vbeln ).
          CONTINUE.

        WHEN lc_action_push OR lc_action_retry.
          " Build JSON payload
          lv_json = build_json_payload( is_invoice = lw_invoice ).

          " Test mode check
          IF p_test = abap_true.
            " Test mode - no API call
            lv_status = lc_status_test.
            persist_result(
              is_invoice  = lw_invoice
              iv_status   = lv_status
              iv_json     = lv_json ).
            ADD 1 TO gv_success_count.

            lo_logger->log_message(
              iv_msgty = 'I'
              iv_msgno = '020'
              iv_msgv1 = lw_invoice-vbeln ).

          ELSE.
            " Live mode - call JWMS API
            TRY.
                lw_response = call_jwms_api( iv_json = lv_json ).

                IF lw_response-http_status = 200 OR
                   lw_response-http_status = 201.
                  " Success
                  lv_status = lc_status_success.
                  ADD 1 TO gv_success_count.

                  " Persist result with acknowledgment number
                  persist_result(
                    is_invoice     = lw_invoice
                    iv_status      = lv_status
                    iv_json        = lv_json
                    iv_response    = lw_response-body
                    iv_ack_no      = lw_response-ack_number
                    iv_http_status = lw_response-http_status ).

                  lo_logger->log_message(
                    iv_msgty = 'S'
                    iv_msgno = '008'
                    iv_msgv1 = lw_invoice-vbeln ).

                ELSE.
                  " API failure (non-success HTTP status)
                  lv_status = lc_status_failed.
                  ADD 1 TO gv_failed_count.

                  " Persist result with error response
                  persist_result(
                    is_invoice     = lw_invoice
                    iv_status      = lv_status
                    iv_json        = lv_json
                    iv_response    = lw_response-body
                    iv_http_status = lw_response-http_status ).

                  lo_logger->log_message(
                    iv_msgty = 'E'
                    iv_msgno = '006'
                    iv_msgv1 = lw_invoice-vbeln ).

                ENDIF.

              CATCH zcx_api_error INTO lo_exception.
                " Exception during API call
                lv_status = lc_status_failed.
                ADD 1 TO gv_failed_count.

                " Persist result with exception message
                persist_result(
                  is_invoice  = lw_invoice
                  iv_status   = lv_status
                  iv_json     = lv_json
                  iv_response = lo_exception->get_text( ) ).

                lo_logger->log_error( lo_exception->get_text( ) ).

            ENDTRY.

          ENDIF.

      ENDCASE.

      " Package processing
      ADD 1 TO gv_package_count.
      IF gv_package_count >= lc_package_size.
        commit_package( ).
      ENDIF.

    ENDLOOP.

    " Final commit for remaining records
    IF gv_package_count > 0.
      commit_package( ).
    ENDIF.

  ENDMETHOD.

  METHOD display_summary.
    DATA: lv_message       TYPE string,
          lv_processed_str TYPE string,
          lv_success_str   TYPE string,
          lv_failed_str    TYPE string,
          lv_skipped_str   TYPE string.

    " Convert counts to strings
    lv_processed_str = gv_processed_count.
    lv_success_str   = gv_success_count.
    lv_failed_str    = gv_failed_count.
    lv_skipped_str   = gv_skipped_count.

    " Build summary message
    CONCATENATE 'Processing complete:'
                'Processed:' lv_processed_str
                'Success:' lv_success_str
                'Failed:' lv_failed_str
                'Skipped:' lv_skipped_str
      INTO lv_message SEPARATED BY space.

    " Display summary
    IF sy-batch = abap_true.
      " Background mode - log to application log
      lo_logger->log_info( lv_message ).
    ELSE.
      " Foreground mode - display message
      MESSAGE lv_message TYPE 'S'.
    ENDIF.

    " Determine run status
    IF gv_failed_count = 0.
      gv_run_status = 'SUCCESS'.
    ELSEIF gv_success_count > 0 AND gv_failed_count > 0.
      gv_run_status = 'PARTIAL'.
    ELSE.
      gv_run_status = 'FAILED'.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

" END: Cursor Generated Code
