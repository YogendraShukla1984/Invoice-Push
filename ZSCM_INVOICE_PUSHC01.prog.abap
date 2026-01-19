*&---------------------------------------------------------------------*
*& Include  ZSCM_INVOICE_PUSHC01
*&---------------------------------------------------------------------*
*& Class Definitions and Implementations
*& All business logic is implemented in classes (OOP approach)
*& Regenerated following ABAP Best Practices and Modern Syntax
*&---------------------------------------------------------------------*

" BEGIN: Regenerated Code with Best Practices

*----------------------------------------------------------------------*
* CLASS lcl_logger DEFINITION
*----------------------------------------------------------------------*
* Purpose: Application log management for invoice push operations
* Responsibilities: Log creation, message logging, display and persistence
*----------------------------------------------------------------------*
CLASS lcl_logger DEFINITION.
  PUBLIC SECTION.
    " Public methods for log management
    METHODS:
      initialize
        RAISING cx_bal_exception,
      
      log_message
        IMPORTING
          iv_msgty TYPE sy-msgty
          iv_msgid TYPE sy-msgid DEFAULT lc_msgid
          iv_msgno TYPE sy-msgno
          iv_msgv1 TYPE sy-msgv1 OPTIONAL
          iv_msgv2 TYPE sy-msgv2 OPTIONAL
          iv_msgv3 TYPE sy-msgv3 OPTIONAL
          iv_msgv4 TYPE sy-msgv4 OPTIONAL
        RAISING cx_bal_exception,
      
      log_error
        IMPORTING
          iv_text TYPE string
        RAISING cx_bal_exception,
      
      log_warning
        IMPORTING
          iv_text TYPE string
        RAISING cx_bal_exception,
      
      log_success
        IMPORTING
          iv_text TYPE string
        RAISING cx_bal_exception,
      
      log_info
        IMPORTING
          iv_text TYPE string
        RAISING cx_bal_exception,
      
      save_log
        RAISING cx_bal_exception,
      
      display_log,
      
      get_log_handle
        RETURNING VALUE(rv_handle) TYPE balloghndl.

  PRIVATE SECTION.
    DATA mv_log_handle TYPE balloghndl.
ENDCLASS.

*----------------------------------------------------------------------*
* CLASS lcl_json_builder DEFINITION
*----------------------------------------------------------------------*
* Purpose: JSON payload construction for invoice data
* Responsibilities: Format conversion, JSON escaping, payload building
*----------------------------------------------------------------------*
CLASS lcl_json_builder DEFINITION.
  PUBLIC SECTION.
    METHODS:
      build_invoice_json
        IMPORTING
          is_invoice     TYPE ty_invoice_data
        RETURNING
          VALUE(rv_json) TYPE string.

  PRIVATE SECTION.
    METHODS:
      escape_json_string
        IMPORTING
          iv_input         TYPE string
        RETURNING
          VALUE(rv_output) TYPE string,
      
      format_date
        IMPORTING
          iv_date              TYPE datum
        RETURNING
          VALUE(rv_formatted)  TYPE string,
      
      format_decimal
        IMPORTING
          iv_value             TYPE any
        RETURNING
          VALUE(rv_formatted)  TYPE string.
ENDCLASS.

*----------------------------------------------------------------------*
* CLASS lcl_api_handler DEFINITION
*----------------------------------------------------------------------*
* Purpose: HTTP API communication handler using Z_SAP_TO_REST_API FM
* Responsibilities: API calls, response parsing, header management
*----------------------------------------------------------------------*
CLASS lcl_api_handler DEFINITION.
  PUBLIC SECTION.
    METHODS:
      call_api
        IMPORTING
          iv_json            TYPE string
          iv_guid            TYPE guid_32 OPTIONAL
        RETURNING
          VALUE(rs_response) TYPE ty_api_response
        RAISING
          zcx_api_error.

  PRIVATE SECTION.
    METHODS:
      build_request_headers
        RETURNING
          VALUE(rt_headers) TYPE tihttpnvp,
      
      parse_ack_number
        IMPORTING
          iv_response          TYPE string
        RETURNING
          VALUE(rv_ack_number) TYPE zack_number.
ENDCLASS.

*----------------------------------------------------------------------*
* CLASS lcl_report DEFINITION
*----------------------------------------------------------------------*
* Purpose: Main report controller for invoice push operations
* Responsibilities: Orchestration, validation, data selection, processing
*----------------------------------------------------------------------*
CLASS lcl_report DEFINITION.
  PUBLIC SECTION.
    METHODS:
      authorization_check
        RAISING zcx_no_authority,
      
      validate_company_code
        RAISING zcx_validation_error,
      
      validate_date_range
        RAISING zcx_validation_error,
      
      read_configuration
        RAISING zcx_config_missing,
      
      select_billing_data,
      
      process_invoices,
      
      display_summary,
      
      has_data
        RETURNING VALUE(rv_has_data) TYPE abap_bool,
      
      initialize_logger,
      
      finalize_logger.

  PRIVATE SECTION.
    " Object references
    DATA:
      mo_logger       TYPE REF TO lcl_logger,
      mo_json_builder TYPE REF TO lcl_json_builder,
      mo_api_handler  TYPE REF TO lcl_api_handler.

    " Private methods
    METHODS:
      check_idempotency
        IMPORTING
          iv_vbeln         TYPE vbeln_vf
          iv_fkdat         TYPE fkdat
          iv_posnr         TYPE posnr
        RETURNING
          VALUE(rv_action) TYPE char10,
      
      build_json_payload
        IMPORTING
          is_invoice     TYPE ty_invoice_data
        RETURNING
          VALUE(rv_json) TYPE string,
      
      call_jwms_api
        IMPORTING
          iv_json            TYPE string
        RETURNING
          VALUE(rs_response) TYPE ty_api_response
        RAISING
          zcx_api_error,
      
      persist_result
        IMPORTING
          is_invoice     TYPE ty_invoice_data
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
    DATA:
      ls_log       TYPE bal_s_log,
      lv_extnumber TYPE balnrext.

    " Build external number: YYYYMMDD_HHMMSS
    lv_extnumber = |{ sy-datum }_{ sy-uzeit }|.

    " Initialize log structure
    ls_log-object    = lc_log_object.
    ls_log-subobject = lc_log_subobject.
    ls_log-extnumber = lv_extnumber.
    ls_log-aluser    = sy-uname.
    ls_log-alprog    = sy-repid.

    " Create application log
    TRY.
        CALL FUNCTION 'BAL_LOG_CREATE'
          EXPORTING
            i_s_log      = ls_log
          IMPORTING
            e_log_handle = mv_log_handle.
            
      CATCH cx_root INTO DATA(lx_error).
        MESSAGE lx_error TYPE 'E'.
    ENDTRY.

  ENDMETHOD.

  METHOD log_message.
    DATA ls_msg TYPE bal_s_msg.

    " Build message structure
    ls_msg-msgty = iv_msgty.
    ls_msg-msgid = iv_msgid.
    ls_msg-msgno = iv_msgno.
    ls_msg-msgv1 = iv_msgv1.
    ls_msg-msgv2 = iv_msgv2.
    ls_msg-msgv3 = iv_msgv3.
    ls_msg-msgv4 = iv_msgv4.

    " Add message to application log
    TRY.
        CALL FUNCTION 'BAL_LOG_MSG_ADD'
          EXPORTING
            i_log_handle = mv_log_handle
            i_s_msg      = ls_msg.
            
      CATCH cx_root INTO DATA(lx_error).
        " Silently handle log errors to not interrupt processing
    ENDTRY.

    " Display in foreground if not batch
    IF sy-batch = abap_false.
      MESSAGE ID iv_msgid TYPE iv_msgty NUMBER iv_msgno
        WITH iv_msgv1 iv_msgv2 iv_msgv3 iv_msgv4.
    ENDIF.

  ENDMETHOD.

  METHOD log_error.
    DATA ls_msg TYPE bal_s_msg.

    ls_msg-msgty = 'E'.
    ls_msg-msgid = '00'.
    ls_msg-msgno = '001'.
    ls_msg-msgv1 = iv_text.

    TRY.
        CALL FUNCTION 'BAL_LOG_MSG_ADD'
          EXPORTING
            i_log_handle = mv_log_handle
            i_s_msg      = ls_msg.
            
      CATCH cx_root INTO DATA(lx_error).
        " Silently handle log errors
    ENDTRY.

    IF sy-batch = abap_false.
      MESSAGE iv_text TYPE 'I' DISPLAY LIKE 'E'.
    ENDIF.

  ENDMETHOD.

  METHOD log_warning.
    DATA ls_msg TYPE bal_s_msg.

    ls_msg-msgty = 'W'.
    ls_msg-msgid = '00'.
    ls_msg-msgno = '001'.
    ls_msg-msgv1 = iv_text.

    TRY.
        CALL FUNCTION 'BAL_LOG_MSG_ADD'
          EXPORTING
            i_log_handle = mv_log_handle
            i_s_msg      = ls_msg.
            
      CATCH cx_root INTO DATA(lx_error).
        " Silently handle log errors
    ENDTRY.

    IF sy-batch = abap_false.
      MESSAGE iv_text TYPE 'I' DISPLAY LIKE 'W'.
    ENDIF.

  ENDMETHOD.

  METHOD log_success.
    DATA ls_msg TYPE bal_s_msg.

    ls_msg-msgty = 'S'.
    ls_msg-msgid = '00'.
    ls_msg-msgno = '001'.
    ls_msg-msgv1 = iv_text.

    TRY.
        CALL FUNCTION 'BAL_LOG_MSG_ADD'
          EXPORTING
            i_log_handle = mv_log_handle
            i_s_msg      = ls_msg.
            
      CATCH cx_root INTO DATA(lx_error).
        " Silently handle log errors
    ENDTRY.

    IF sy-batch = abap_false.
      MESSAGE iv_text TYPE 'S'.
    ENDIF.

  ENDMETHOD.

  METHOD log_info.
    DATA ls_msg TYPE bal_s_msg.

    ls_msg-msgty = 'I'.
    ls_msg-msgid = '00'.
    ls_msg-msgno = '001'.
    ls_msg-msgv1 = iv_text.

    TRY.
        CALL FUNCTION 'BAL_LOG_MSG_ADD'
          EXPORTING
            i_log_handle = mv_log_handle
            i_s_msg      = ls_msg.
            
      CATCH cx_root INTO DATA(lx_error).
        " Silently handle log errors
    ENDTRY.

  ENDMETHOD.

  METHOD save_log.
    DATA lt_log_handle TYPE bal_t_logh.

    APPEND mv_log_handle TO lt_log_handle.

    TRY.
        CALL FUNCTION 'BAL_DB_SAVE'
          EXPORTING
            i_client       = sy-mandt
            i_save_all     = abap_true
            i_t_log_handle = lt_log_handle
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
        
      CATCH cx_root INTO DATA(lx_error).
        MESSAGE lx_error TYPE 'W'.
    ENDTRY.

  ENDMETHOD.

  METHOD display_log.
    DATA lt_log_handle TYPE bal_t_logh.

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
    DATA:
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

    " Build JSON payload using string templates (modern syntax)
    rv_json = |\{ | &&
              |"AccountId":"{ gv_account_id }",| &&
              |"BusinessId":"{ gv_business_id }",| &&
              |"SubBusinessId":"{ gv_sub_business_id }",| &&
              |"Invoice":\{ | &&
                |"BillingDocument":"{ is_invoice-vbeln }",| &&
                |"BillingDate":"{ lv_date }",| &&
                |"CompanyCode":"{ is_invoice-bukrs }",| &&
                |"Division":"{ is_invoice-spart }",| &&
                |"SalesOrganization":"{ is_invoice-vkorg }",| &&
                |"BillToCustomer":"{ is_invoice-kunrg }",| &&
                |"BillToName":"{ lv_kunrg_name }",| &&
                |"SoldToCustomer":"{ is_invoice-kunag }",| &&
                |"SoldToName":"{ lv_kunag_name }",| &&
                |"Items":[| &&
                  |\{ | &&
                    |"ItemNumber":"{ is_invoice-posnr }",| &&
                    |"Material":"{ is_invoice-matnr }",| &&
                    |"Quantity":"{ lv_quantity }",| &&
                    |"UnitOfMeasure":"{ is_invoice-vrkme }",| &&
                    |"NetValue":"{ lv_value }",| &&
                    |"Currency":"{ is_invoice-waerk }"| &&
                  |\}| &&
                |]| &&
              |\}| &&
            |\}|.

  ENDMETHOD.

  METHOD escape_json_string.
    " Escape special characters for JSON
    rv_output = iv_input.

    " Escape backslash first (order matters!)
    REPLACE ALL OCCURRENCES OF '\' IN rv_output WITH '\\'.
    " Escape double quotes
    REPLACE ALL OCCURRENCES OF '"' IN rv_output WITH '\"'.
    " Escape newline
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline
      IN rv_output WITH '\n'.
    " Escape carriage return + line feed
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf
      IN rv_output WITH '\n'.

  ENDMETHOD.

  METHOD format_date.
    " Convert SAP date (YYYYMMDD) to JSON/ISO format (YYYY-MM-DD)
    IF iv_date IS NOT INITIAL.
      rv_formatted = |{ iv_date+0(4) }-{ iv_date+4(2) }-{ iv_date+6(2) }|.
    ENDIF.

  ENDMETHOD.

  METHOD format_decimal.
    " Convert decimal value to string without thousand separators
    rv_formatted = condense( |{ iv_value }| ).

  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* CLASS lcl_api_handler IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_api_handler IMPLEMENTATION.

  METHOD call_api.
    DATA:
      lv_response_raw  TYPE string,
      lv_http_status   TYPE i,
      lt_headers       TYPE tihttpnvp,
      lt_resp_headers  TYPE tihttpnvp,
      lv_guid          TYPE guid_32,
      ls_response_data TYPE ty_api_response_structure.

    " Build request headers with authentication
    lt_headers = build_request_headers( ).

    " Use provided GUID or let FM generate one
    lv_guid = iv_guid.

    TRY.
        " Call standardized REST API function module
        CALL FUNCTION 'Z_SAP_TO_REST_API'
          EXPORTING
            im_url             = gv_api_endpoint
            im_http_method     = 'POST'
            im_payload_json    = iv_json
            im_payload_case    = abap_false
            it_header          = lt_headers
            im_guuid           = lv_guid
            im_tcode           = sy-tcode
            im_track           = 'INVOICE_PUSH'
            im_interface       = lc_interface_type
            im_service_name    = 'JWMS_INVOICE'
          IMPORTING
            ex_guid            = lv_guid
            ex_response_status = lv_http_status
            et_response_header = lt_resp_headers
            ex_reponse_raw     = lv_response_raw
          EXCEPTIONS
            argument_not_found           = 1
            plugin_not_active            = 2
            internal_error               = 3
            http_invalid_state           = 4
            response_failed              = 5
            http_communication_failure   = 6
            http_processing_failed       = 7
            http_invalid_timeout         = 8
            number_range_not_generated   = 9
            error_while_save_text        = 10
            OTHERS                       = 11.

        IF sy-subrc <> 0.
          " Handle exceptions from function module
          RAISE EXCEPTION TYPE zcx_api_error
            EXPORTING
              textid = zcx_api_error=>api_call_failed.
        ENDIF.

        " Build response structure
        rs_response-http_status = lv_http_status.
        rs_response-body = lv_response_raw.
        rs_response-ack_number = parse_ack_number( iv_response = lv_response_raw ).
        rs_response-guid = lv_guid.

      CATCH cx_root INTO DATA(lx_error).
        RAISE EXCEPTION TYPE zcx_api_error
          EXPORTING
            textid   = zcx_api_error=>api_call_failed
            previous = lx_error.
    ENDTRY.

  ENDMETHOD.

  METHOD build_request_headers.
    DATA:
      ls_header      TYPE ihttpnvp,
      lv_auth_string TYPE string,
      lv_credentials TYPE string,
      lv_encoded     TYPE string.

    " Build authentication headers based on configured auth type
    CASE gv_api_auth_type.
      WHEN 'OAUTH'.
        " OAuth Bearer token
        ls_header-name = 'Authorization'.
        ls_header-value = |Bearer { gv_api_token }|.
        APPEND ls_header TO rt_headers.

      WHEN 'BASIC'.
        " Basic Authentication
        lv_credentials = |{ gv_api_username }:{ gv_api_password }|.

        " Base64 encode credentials
        TRY.
            CALL FUNCTION 'SCMS_BASE64_ENCODE_STR'
              EXPORTING
                input  = lv_credentials
              IMPORTING
                output = lv_encoded.

            ls_header-name = 'Authorization'.
            ls_header-value = |Basic { lv_encoded }|.
            APPEND ls_header TO rt_headers.

          CATCH cx_root.
            " Log encoding error but continue
        ENDTRY.

      WHEN 'APIKEY'.
        " API Key authentication
        ls_header-name = 'X-API-Key'.
        ls_header-value = gv_api_token.
        APPEND ls_header TO rt_headers.

    ENDCASE.

    " Add any additional custom headers if needed
    " Content-Type is already handled by the FM

  ENDMETHOD.

  METHOD parse_ack_number.
    DATA lv_ack_no TYPE string.

    " Parse acknowledgment number from JSON response
    " Try primary field name
    FIND REGEX '"ackNumber"\s*:\s*"([^"]+)"' IN iv_response
      SUBMATCHES lv_ack_no.

    IF sy-subrc = 0.
      rv_ack_number = lv_ack_no.
    ELSE.
      " Try alternative field name
      FIND REGEX '"acknowledgementNumber"\s*:\s*"([^"]+)"' IN iv_response
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
    " Validate company code exists in T001
    SELECT SINGLE bukrs
      FROM t001
      INTO @DATA(lv_bukrs)
      WHERE bukrs = @p_bukrs.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_validation_error
        MESSAGE e003(lc_msgid) WITH p_bukrs.
    ENDIF.

  ENDMETHOD.

  METHOD validate_date_range.
    " Validate that From date is not after To date
    IF p_fkdat_f > p_fkdat_t.
      RAISE EXCEPTION TYPE zcx_validation_error
        MESSAGE e002(lc_msgid).
    ENDIF.

  ENDMETHOD.

  METHOD read_configuration.
    DATA:
      lt_config     TYPE tt_config,
      ls_config     TYPE ty_config,
      ls_api_config TYPE ty_api_config,
      lv_fkart      TYPE fkart.

    " Read billing type configuration
    SELECT name, value, value1, value2, active
      FROM zlog_exec_var
      INTO TABLE @lt_config
      WHERE name = 'ZSCM_GET_INV_BILLINGTYPE'
        AND active = @abap_true.

    IF sy-subrc <> 0 OR lt_config IS INITIAL.
      RAISE EXCEPTION TYPE zcx_config_missing
        EXPORTING
          textid = zcx_config_missing=>billing_types_missing.
    ENDIF.

    " Extract billing types into internal table
    LOOP AT lt_config INTO ls_config.
      APPEND ls_config-value TO gt_billing_types.
    ENDLOOP.

    " Read Account, Business, and Sub-Business IDs
    CLEAR lt_config.
    SELECT name, value, value1, value2, active
      FROM zlog_exec_var
      INTO TABLE @lt_config
      WHERE name IN ('ZSCM_ACCOUNTID', 'ZSCM_BUSINESSID', 'ZSCM_SUB_BUSINESSID')
        AND active = @abap_true.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_config_missing
        EXPORTING
          textid = zcx_config_missing=>ids_missing.
    ENDIF.

    " Populate global ID variables
    LOOP AT lt_config INTO ls_config.
      CASE ls_config-name.
        WHEN 'ZSCM_ACCOUNTID'.
          gv_account_id = ls_config-value.
        WHEN 'ZSCM_BUSINESSID'.
          gv_business_id = ls_config-value.
        WHEN 'ZSCM_SUB_BUSINESSID'.
          gv_sub_business_id = ls_config-value.
      ENDCASE.
    ENDLOOP.

    " Validate all required IDs are populated
    IF gv_account_id IS INITIAL OR
       gv_business_id IS INITIAL OR
       gv_sub_business_id IS INITIAL.
      RAISE EXCEPTION TYPE zcx_config_missing
        EXPORTING
          textid = zcx_config_missing=>ids_missing.
    ENDIF.

    " Read API configuration
    SELECT SINGLE interface_name, endpoint_url, auth_type,
                  username, password, token, timeout, active
      FROM zwso2apidtl
      INTO @ls_api_config
      WHERE interface_name = @lc_interface_type
        AND active = @abap_true.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_config_missing
        EXPORTING
          textid = zcx_config_missing=>api_config_missing.
    ENDIF.

    " Populate global API configuration variables
    gv_api_endpoint  = ls_api_config-endpoint_url.
    gv_api_auth_type = ls_api_config-auth_type.
    gv_api_username  = ls_api_config-username.
    gv_api_password  = ls_api_config-password.
    gv_api_token     = ls_api_config-token.
    gv_api_timeout   = ls_api_config-timeout.

    " Validate critical API endpoint
    IF gv_api_endpoint IS INITIAL.
      RAISE EXCEPTION TYPE zcx_config_missing
        EXPORTING
          textid = zcx_config_missing=>api_endpoint_missing.
    ENDIF.

    " Log successful configuration read
    mo_logger->log_info( 'Configuration read successfully' ).

  ENDMETHOD.

  METHOD select_billing_data.
    DATA:
      lt_vbeln        TYPE STANDARD TABLE OF vbeln_vf,
      lv_message      TYPE string,
      lv_header_count TYPE string,
      lv_item_count   TYPE string.

    " Select billing document headers with modern OpenSQL
    SELECT vbeln, fkdat, bukrs, spart, vkorg, fkart, kunrg, kunag
      FROM vbrk
      INTO CORRESPONDING FIELDS OF TABLE @gt_billing_headers
      WHERE fkart IN @gt_billing_types
        AND fkart IN @s_fkart
        AND fkdat BETWEEN @p_fkdat_f AND @p_fkdat_t
        AND bukrs = @p_bukrs
        AND vkorg IN @s_vkorg
        AND fksto = @space.

    IF sy-subrc <> 0 OR gt_billing_headers IS INITIAL.
      mo_logger->log_info( 'No billing headers found for selection criteria' ).
      RETURN.
    ENDIF.

    " Extract document numbers for item selection
    lt_vbeln = VALUE #( FOR ls_header IN gt_billing_headers ( ls_header-vbeln ) ).
    SORT lt_vbeln.
    DELETE ADJACENT DUPLICATES FROM lt_vbeln.

    " Select billing items using FOR ALL ENTRIES (without JOIN)
    IF lt_vbeln IS NOT INITIAL.
      " First, select billing items
      SELECT vbeln, posnr, matnr, fkimg, vrkme, netwr, waerk
        FROM vbrp
        INTO CORRESPONDING FIELDS OF TABLE @gt_billing_items
        FOR ALL ENTRIES IN @lt_vbeln
        WHERE vbeln = @lt_vbeln-table_line.

      " Then, enrich with billing date from header
      IF gt_billing_items IS NOT INITIAL.
        LOOP AT gt_billing_items ASSIGNING FIELD-SYMBOL(<fs_item>).
          READ TABLE gt_billing_headers INTO DATA(ls_header)
            WITH KEY vbeln = <fs_item>-vbeln.
          IF sy-subrc = 0.
            <fs_item>-fkdat = ls_header-fkdat.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

    IF sy-subrc <> 0 OR gt_billing_items IS INITIAL.
      mo_logger->log_warning( 'No billing items found' ).
      RETURN.
    ENDIF.

    " Enrich with customer master data
    enrich_customer_data( ).

    " Log selection summary
    lv_header_count = |{ lines( gt_billing_headers ) }|.
    lv_item_count = |{ lines( gt_billing_items ) }|.
    lv_message = |Selected { lv_header_count } headers and { lv_item_count } items|.
    mo_logger->log_info( lv_message ).

  ENDMETHOD.

  METHOD enrich_customer_data.
    DATA lt_kunnr TYPE STANDARD TABLE OF kunnr.

    " Collect unique customer numbers using VALUE constructor
    lt_kunnr = VALUE #( FOR ls_header IN gt_billing_headers
                        ( ls_header-kunrg )
                        ( ls_header-kunag ) ).
    
    SORT lt_kunnr.
    DELETE ADJACENT DUPLICATES FROM lt_kunnr.

    " Read customer master data
    IF lt_kunnr IS NOT INITIAL.
      SELECT kunnr, name1
        FROM kna1
        INTO TABLE @gt_customers
        FOR ALL ENTRIES IN @lt_kunnr
        WHERE kunnr = @lt_kunnr-table_line.

      IF sy-subrc <> 0.
        mo_logger->log_warning( 'Customer master data not found' ).
      ENDIF.
    ENDIF.

    " Update headers with customer names using field symbols
    LOOP AT gt_billing_headers ASSIGNING FIELD-SYMBOL(<fs_header>).
      " Bill-to customer name
      READ TABLE gt_customers INTO DATA(ls_customer)
        WITH KEY kunnr = <fs_header>-kunrg.
      IF sy-subrc = 0.
        <fs_header>-kunrg_name = ls_customer-name1.
      ENDIF.

      " Sold-to customer name
      READ TABLE gt_customers INTO ls_customer
        WITH KEY kunnr = <fs_header>-kunag.
      IF sy-subrc = 0.
        <fs_header>-kunag_name = ls_customer-name1.
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
    " Create object instances
    CREATE OBJECT mo_logger.
    mo_logger->initialize( ).

    CREATE OBJECT mo_json_builder.
    CREATE OBJECT mo_api_handler.

    " Initialize processing counters
    CLEAR: gv_processed_count,
           gv_success_count,
           gv_failed_count,
           gv_skipped_count,
           gv_package_count.

  ENDMETHOD.

  METHOD finalize_logger.
    " Save or display log based on execution mode
    IF sy-batch = abap_true.
      mo_logger->save_log( ).
    ELSE.
      " Display log in foreground if errors occurred
      IF gv_failed_count > 0.
        mo_logger->display_log( ).
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD read_interface_records.
    DATA lt_vbeln TYPE STANDARD TABLE OF vbeln_vf.

    " Extract document numbers from billing items
    lt_vbeln = VALUE #( FOR ls_item IN gt_billing_items ( ls_item-vbeln ) ).
    SORT lt_vbeln.
    DELETE ADJACENT DUPLICATES FROM lt_vbeln.

    " Bulk read interface tracking records
    IF lt_vbeln IS NOT INITIAL.
      SELECT *
        FROM zscm_invoice_interface
        INTO TABLE @gt_interface_records
        FOR ALL ENTRIES IN @lt_vbeln
        WHERE interface_type = @lc_interface_type
          AND vbeln = @lt_vbeln-table_line.
    ENDIF.

    " Sort by key + counter descending for latest record lookup
    SORT gt_interface_records BY vbeln fkdat posnr counter DESCENDING.

  ENDMETHOD.

  METHOD check_idempotency.
    " Read latest interface record for this invoice item
    READ TABLE gt_interface_records INTO DATA(ls_interface)
      WITH KEY vbeln = iv_vbeln
               fkdat = iv_fkdat
               posnr = iv_posnr
      BINARY SEARCH.

    IF sy-subrc = 0.
      " Record exists - determine action based on status
      CASE ls_interface-status.
        WHEN lc_status_success.
          " Successfully processed - skip
          rv_action = lc_action_skip.

        WHEN lc_status_skipped.
          " Business exclusion - skip
          rv_action = lc_action_skip.

        WHEN lc_status_failed.
          " Failed - check retry limit
          IF ls_interface-counter >= lc_max_retry.
            " Maximum retries reached - skip
            rv_action = lc_action_skip.
            mo_logger->log_warning(
              |Max retry count reached for invoice { iv_vbeln }| ).
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
      " No existing record - push new invoice
      rv_action = lc_action_push.

    ENDIF.

  ENDMETHOD.

  METHOD build_json_payload.
    rv_json = mo_json_builder->build_invoice_json( is_invoice = is_invoice ).
  ENDMETHOD.

  METHOD call_jwms_api.
    " Call JWMS API using standardized REST API handler
    rs_response = mo_api_handler->call_api(
      iv_json = iv_json
      iv_guid = CONV guid_32( sy-datum && sy-uzeit && sy-uname ) ).
  ENDMETHOD.

  METHOD persist_result.
    DATA:
      ls_interface   TYPE zscm_invoice_interface,
      lv_counter     TYPE i,
      lv_counter_str TYPE string.

    " Read existing record with highest counter
    SELECT SINGLE *
      FROM zscm_invoice_interface
      INTO @ls_interface
      WHERE mandt = @sy-mandt
        AND interface_type = @lc_interface_type
        AND vbeln = @is_invoice-vbeln
        AND fkdat = @is_invoice-fkdat
        AND posnr = @is_invoice-posnr
      ORDER BY counter DESCENDING.

    IF sy-subrc = 0.
      " Record exists - UPDATE scenario (retry)
      lv_counter = ls_interface-counter + 1.

      " Check max retry limit
      IF lv_counter >= lc_max_retry.
        mo_logger->log_message(
          iv_msgty = 'E'
          iv_msgno = '050'
          iv_msgv1 = is_invoice-vbeln ).
        RETURN.
      ENDIF.

      " Update existing record
      UPDATE zscm_invoice_interface
        SET status       = @iv_status
            counter      = @lv_counter
            json_payload = @iv_json
            api_response = @iv_response
            ack_number   = @iv_ack_no
            http_status  = @iv_http_status
            aedat        = @sy-datum
            aezet        = @sy-uzeit
            aenam        = @sy-uname
        WHERE mandt = @sy-mandt
          AND interface_type = @lc_interface_type
          AND vbeln = @is_invoice-vbeln
          AND fkdat = @is_invoice-fkdat
          AND posnr = @is_invoice-posnr
          AND counter = @ls_interface-counter.

      IF sy-subrc = 0.
        lv_counter_str = |{ lv_counter }|.
        mo_logger->log_message(
          iv_msgty = 'S'
          iv_msgno = '060'
          iv_msgv1 = is_invoice-vbeln
          iv_msgv2 = lv_counter_str ).
      ELSE.
        mo_logger->log_error(
          |Failed to update interface table for invoice { is_invoice-vbeln }| ).
      ENDIF.

    ELSE.
      " No existing record - INSERT scenario
      lv_counter = 1.

      " Build new record using VALUE constructor
      ls_interface = VALUE #(
        mandt           = sy-mandt
        interface_type  = lc_interface_type
        vbeln           = is_invoice-vbeln
        fkdat           = is_invoice-fkdat
        posnr           = is_invoice-posnr
        counter         = lv_counter
        status          = iv_status
        division        = is_invoice-spart
        bukrs           = is_invoice-bukrs
        kunrg           = is_invoice-kunrg
        kunrg_name      = is_invoice-kunrg_name
        kunag           = is_invoice-kunag
        kunag_name      = is_invoice-kunag_name
        accountid       = gv_account_id
        businessid      = gv_business_id
        sub_businessid  = gv_sub_business_id
        json_payload    = iv_json
        api_response    = iv_response
        ack_number      = iv_ack_no
        http_status     = iv_http_status
        erdat           = sy-datum
        erzet           = sy-uzeit
        ernam           = sy-uname
        aedat           = sy-datum
        aezet           = sy-uzeit
        aenam           = sy-uname ).

      " Insert new record
      INSERT zscm_invoice_interface FROM @ls_interface.

      IF sy-subrc = 0.
        mo_logger->log_message(
          iv_msgty = 'S'
          iv_msgno = '061'
          iv_msgv1 = is_invoice-vbeln ).
      ELSE.
        mo_logger->log_error(
          |Failed to insert interface table for invoice { is_invoice-vbeln }| ).
      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD commit_package.
    " Commit database changes
    COMMIT WORK AND WAIT.

    " Log package commit
    mo_logger->log_message(
      iv_msgty = 'I'
      iv_msgno = '040'
      iv_msgv1 = CONV #( gv_package_count ) ).

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
        mo_logger->log_warning( |Header not found for item { gw_billing_item-vbeln }| ).
        CONTINUE.
      ENDIF.

      " Skip if customer data missing
      IF lw_invoice-kunrg_name IS INITIAL OR
         lw_invoice-kunag_name IS INITIAL.
        ADD 1 TO gv_skipped_count.
        mo_logger->log_message(
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
          mo_logger->log_message(
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

            mo_logger->log_message(
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

                  mo_logger->log_message(
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

                  mo_logger->log_message(
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

                mo_logger->log_error( lo_exception->get_text( ) ).

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
    DATA lv_message TYPE string.

    " Build summary message using string templates
    lv_message = |Processing complete: | &&
                 |Processed: { gv_processed_count } | &&
                 |Success: { gv_success_count } | &&
                 |Failed: { gv_failed_count } | &&
                 |Skipped: { gv_skipped_count }|.

    " Display summary based on execution mode
    IF sy-batch = abap_true.
      mo_logger->log_info( lv_message ).
    ELSE.
      MESSAGE lv_message TYPE 'S'.
    ENDIF.

    " Determine overall run status
    IF gv_failed_count = 0.
      gv_run_status = 'SUCCESS'.
    ELSEIF gv_success_count > 0 AND gv_failed_count > 0.
      gv_run_status = 'PARTIAL'.
    ELSE.
      gv_run_status = 'FAILED'.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

" END: Regenerated Code with Best Practices
