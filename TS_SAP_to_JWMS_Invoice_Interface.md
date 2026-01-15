# Technical Specification: SAP → JWMS Invoice Push Interface

## 1. Document Control

| Attribute | Value |
|----------|-------|
| Document Title | SAP Invoice Interface – JWMS (Technical Specification) |
| Program Name | ZSCM_INVOICE_PUSH |
| Transaction Code | ZSCM_INVOICE_PUSH |
| Package | ZLOG |
| Responsible Team | Ganesh Patil |
| Version | 1.0 |
| Prepared By | Ganesh Patil |
| Date | January 2026 |
| Interface Type | Outbound (SAP → JWMS) |
| Target System | SAP ECC 6.0 / NetWeaver 7.31 |

---

## 2. Technical Overview

The interface is implemented as an **object-oriented ABAP report** that reads billing data from standard SAP tables (VBRK/VBRP), constructs a JSON payload, posts it to JWMS via HTTPS API, and stores the payload, response, and acknowledgment number in a custom tracking table (ZSCM_INVOICE_INTERFACE). 

The program is **re-runnable** and **idempotent** using status-driven logic. It supports both **foreground** (interactive with progress messages and ALV summary) and **background** (batch job with application log) execution modes.

**Key Design Principles:**
- **Object-Oriented**: All business logic in local classes with methods (no FORMs/PERFORM).
- **NetWeaver 7.31 Compatible**: No inline declarations, string templates, or table expressions.
- **Production-Ready**: Complete error handling, logging, authorization checks, and package processing.

---

## 3. Technical Objects

| Object Type | Name | Description |
|------------|------|-------------|
| **Report Program** | `ZSCM_INVOICE_PUSH` | Main program (executable report) |
| **TOP Include** | `ZSCM_INVOICE_PUSHTOP` | Global types, data declarations, constants |
| **SEL Include** | `ZSCM_INVOICE_PUSHSEL` | Selection screen definition |
| **C01 Include** | `ZSCM_INVOICE_PUSHC01` | Local class definitions and implementations |
| **Main Class** | `LCL_REPORT` | Main processing class (all business logic) |
| **Utility Class** | `LCL_JSON_BUILDER` | JSON payload construction |
| **API Handler Class** | `LCL_API_HANDLER` | JWMS API communication |
| **Logger Class** | `LCL_LOGGER` | Application log management |
| **Message Class** | `ZSCM_INV` | Interface-specific messages (001-050) |
| **App Log Object** | `ZSCM` | Application log object |
| **App Log Subobject** | `INV_PUSH` | Application log subobject |
| **Custom Tables** | `ZSCM_INVOICE_INTERFACE` | Invoice tracking and status |
|  | `ZLOG_EXEC_VAR` | Configuration table |
|  | `ZWSO2APIDTL` | API endpoint configuration |
| **Standard Tables** | `VBRK`, `VBRP`, `KNA1`, `T001` | Billing and master data |
| **Package** | `ZLOG` | Development package |
| **Transaction Code** | `ZSCM_INVOICE_PUSH` | Direct program execution |

---

## 4. Selection Screen Design

### 4.1 Parameters

```abap
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
  " Selection Criteria
  PARAMETERS: p_fkdat_f TYPE vbrk-fkdat OBLIGATORY DEFAULT sy-datum,
              p_fkdat_t TYPE vbrk-fkdat OBLIGATORY DEFAULT sy-datum.
  
  PARAMETERS: p_bukrs TYPE vbrk-bukrs OBLIGATORY.
  
  SELECT-OPTIONS: s_fkart FOR vbrk-fkart OBLIGATORY,
                  s_vkorg FOR vbrk-vkorg.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
  " Options
  PARAMETERS: p_test TYPE abap_bool AS CHECKBOX DEFAULT abap_false.
SELECTION-SCREEN END OF BLOCK b2.
```

### 4.2 Text Elements

| Text ID | Description |
|---------|-------------|
| text-001 | Selection Criteria |
| text-002 | Options |

### 4.3 Selection Screen Validations (AT SELECTION-SCREEN)

```abap
AT SELECTION-SCREEN ON p_bukrs.
  " Validate company code exists in T001
  go_report->validate_company_code( ).

AT SELECTION-SCREEN ON p_fkdat_f.
  " Validate date range
  go_report->validate_date_range( ).

AT SELECTION-SCREEN ON p_fkdat_t.
  " Validate date range
  go_report->validate_date_range( ).
```

---

## 5. Data Model

### 5.1 ZSCM_INVOICE_INTERFACE (Custom Table)

**Purpose:** Track invoice push status, payload, response, and retry counter.

**Primary Key:**
- MANDT (Client)
- INTERFACE_TYPE (Interface identifier, e.g., 'JWMS_INV')
- VBELN (Billing document number)
- FKDAT (Billing date)
- POSNR (Billing item number)
- COUNTER (Retry counter)

**Fields:**

| Field Name | Data Element | Type | Length | Description |
|------------|--------------|------|--------|-------------|
| MANDT | MANDT | CLNT | 3 | Client |
| INTERFACE_TYPE | ZINTERFACE_TYPE | CHAR | 20 | Interface identifier |
| VBELN | VBELN_VF | CHAR | 10 | Billing document |
| FKDAT | FKDAT | DATS | 8 | Billing date |
| POSNR | POSNR | NUMC | 6 | Billing item |
| COUNTER | ZCOUNTER | INT4 | 10 | Retry counter |
| STATUS | ZSTATUS | CHAR | 1 | Status (X/F/S/T) |
| DIVISION | SPART | CHAR | 2 | Division |
| BUKRS | BUKRS | CHAR | 4 | Company code |
| KUNRG | KUNRG | CHAR | 10 | Bill-to customer |
| KUNRG_NAME | NAME1 | CHAR | 35 | Bill-to name |
| KUNAG | KUNAG | CHAR | 10 | Sold-to customer |
| KUNAG_NAME | NAME1 | CHAR | 35 | Sold-to name |
| ACCOUNTID | ZACCOUNTID | CHAR | 20 | Account ID (from config) |
| BUSINESSID | ZBUSINESSID | CHAR | 20 | Business ID (from config) |
| SUB_BUSINESSID | ZSUB_BUSINESSID | CHAR | 20 | Sub-Business ID (from config) |
| JSON_PAYLOAD | STRING | STRG | - | JSON request payload |
| API_RESPONSE | STRING | STRG | - | API response body |
| ACK_NUMBER | ZACK_NUMBER | CHAR | 50 | Acknowledgment number from JWMS |
| HTTP_STATUS | ZHTTPSTATUS | NUMC | 3 | HTTP status code |
| ERDAT | ERDAT | DATS | 8 | Creation date |
| ERZET | ERZET | TIMS | 6 | Creation time |
| ERNAM | ERNAM | CHAR | 12 | Created by |
| AEDAT | AEDAT | DATS | 8 | Last change date |
| AEZET | AEZET | TIMS | 6 | Last change time |
| AENAM | AENAM | CHAR | 12 | Changed by |

**Status Values:**
| Status | Meaning |
|--------|---------|
| X | Successfully pushed to JWMS |
| F | Failed (eligible for retry) |
| S | Skipped (business-defined exclusion) |
| T | Test mode (not sent to JWMS) |

---

## 6. Class Design (OOP Structure)

### 6.1 Main Report Class (LCL_REPORT)

**Purpose:** Orchestrate the entire invoice push process.

**Public Methods:**

| Method Name | Purpose | Parameters | Returns |
|-------------|---------|------------|---------|
| `authorization_check` | Check S_TCODE authorization | - | - (raises exception if unauthorized) |
| `validate_company_code` | Validate P_BUKRS exists in T001 | - | - (displays error if invalid) |
| `validate_date_range` | Validate P_FKDAT_F <= P_FKDAT_T | - | - (displays error if invalid) |
| `read_configuration` | Read billing types, IDs, API config | - | - (aborts if config missing) |
| `select_billing_data` | Select VBRK, VBRP, KNA1 | - | - |
| `process_invoices` | Main processing loop | - | - |
| `display_summary` | Display results (ALV or message) | - | - |

**Private Methods:**

| Method Name | Purpose | Parameters | Returns |
|-------------|---------|------------|---------|
| `check_idempotency` | Check ZSCM_INVOICE_INTERFACE status | iv_vbeln, iv_fkdat, iv_posnr | rv_action (PUSH/SKIP/RETRY) |
| `build_json_payload` | Build JSON payload for one invoice | is_header, it_items | rv_json (string) |
| `call_jwms_api` | Call JWMS API | iv_json | es_response (status, body, ack_no) |
| `persist_result` | Insert/update ZSCM_INVOICE_INTERFACE | is_invoice, iv_status, iv_response | - |
| `commit_package` | Commit work after package | - | - |

### 6.2 JSON Builder Class (LCL_JSON_BUILDER)

**Purpose:** Construct JSON payload from SAP data.

**Public Methods:**

| Method Name | Purpose | Parameters | Returns |
|-------------|---------|------------|---------|
| `build_invoice_json` | Build complete JSON payload | is_header, it_items, is_config | rv_json (string) |

**Private Methods:**

| Method Name | Purpose |
|-------------|---------|
| `escape_json_string` | Escape special characters for JSON |
| `format_date` | Convert SAP date (YYYYMMDD) to JSON format (YYYY-MM-DD) |
| `format_decimal` | Format decimal values for JSON |

### 6.3 API Handler Class (LCL_API_HANDLER)

**Purpose:** Handle JWMS API communication.

**Public Methods:**

| Method Name | Purpose | Parameters | Returns |
|-------------|---------|------------|---------|
| `call_api` | POST JSON to JWMS API | iv_endpoint, iv_json, is_auth | es_response (status, body, ack_no) |

**Private Methods:**

| Method Name | Purpose |
|-------------|---------|
| `create_http_client` | Create CL_HTTP_CLIENT instance |
| `set_authentication` | Set auth headers (OAuth/Basic/API key) |
| `parse_response` | Parse API response and extract ack number |

### 6.4 Logger Class (LCL_LOGGER)

**Purpose:** Manage application log (BAL) and foreground messages.

**Public Methods:**

| Method Name | Purpose | Parameters | Returns |
|-------------|---------|------------|---------|
| `initialize` | Create application log | - | - |
| `log_message` | Add message to log | iv_msgty, iv_msgid, iv_msgno, iv_msgv1-4 | - |
| `log_error` | Add error message | iv_text | - |
| `log_warning` | Add warning message | iv_text | - |
| `log_success` | Add success message | iv_text | - |
| `save_log` | Save log to database (background mode) | - | - |
| `display_log` | Display log (foreground mode) | - | - |
| `get_log_handle` | Get log handle | - | rv_handle |

---

## 7. Processing Flow (Detailed)

### 7.1 INITIALIZATION Event

```abap
INITIALIZATION.
  " Create global report object
  CREATE OBJECT go_report.
  
  " Authorization check
  TRY.
    go_report->authorization_check( ).
  CATCH zcx_no_authority INTO lo_exception.
    MESSAGE lo_exception TYPE 'E'.
    LEAVE PROGRAM.
  ENDTRY.
```

### 7.2 AT SELECTION-SCREEN Event

```abap
AT SELECTION-SCREEN ON p_bukrs.
  go_report->validate_company_code( ).

AT SELECTION-SCREEN ON p_fkdat_f.
  go_report->validate_date_range( ).

AT SELECTION-SCREEN ON p_fkdat_t.
  go_report->validate_date_range( ).
```

### 7.3 START-OF-SELECTION Event

```abap
START-OF-SELECTION.
  " Initialize logger
  go_report->initialize_logger( ).
  
  " Read configuration
  TRY.
    go_report->read_configuration( ).
  CATCH zcx_config_missing INTO lo_exception.
    MESSAGE lo_exception TYPE 'E'.
    LEAVE PROGRAM.
  ENDTRY.
  
  " Select billing data
  go_report->select_billing_data( ).
  
  " Check if data exists
  IF go_report->has_data( ) = abap_false.
    MESSAGE 'No invoices found in selection range'(003) TYPE 'I'.
    LEAVE PROGRAM.
  ENDIF.
  
  " Process invoices
  go_report->process_invoices( ).
  
  " Display summary
  go_report->display_summary( ).
```

### 7.4 Main Processing Logic (process_invoices method)

```abap
METHOD process_invoices.
  DATA: lv_package_count TYPE i VALUE 0,
        lv_action TYPE char10,
        lv_json TYPE string,
        lw_response TYPE ty_api_response,
        lv_status TYPE char1.
  
  LOOP AT gt_billing_items INTO gw_billing_item.
    " Progress indicator (foreground only)
    IF sy-batch = abap_false.
      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = ( sy-tabix * 100 ) / lines( gt_billing_items )
          text       = 'Processing invoice'(004).
    ENDIF.
    
    " Check idempotency
    lv_action = check_idempotency( iv_vbeln = gw_billing_item-vbeln
                                    iv_fkdat = gw_billing_item-fkdat
                                    iv_posnr = gw_billing_item-posnr ).
    
    CASE lv_action.
      WHEN 'SKIP'.
        " Already processed or excluded
        ADD 1 TO gv_skipped_count.
        go_logger->log_message( iv_msgty = 'I'
                                iv_msgid = 'ZSCM_INV'
                                iv_msgno = '010'
                                iv_msgv1 = gw_billing_item-vbeln ).
        CONTINUE.
        
      WHEN 'PUSH' OR 'RETRY'.
        " Build JSON payload
        lv_json = build_json_payload( is_header = gw_billing_header
                                       it_items = lt_items ).
        
        " Test mode check
        IF p_test = abap_true.
          " Test mode - log payload only
          go_logger->log_message( iv_msgty = 'I'
                                  iv_msgid = 'ZSCM_INV'
                                  iv_msgno = '020'
                                  iv_msgv1 = gw_billing_item-vbeln ).
          go_logger->log_message( iv_msgty = 'I'
                                  iv_msgid = 'ZSCM_INV'
                                  iv_msgno = '021'
                                  iv_msgv1 = lv_json ).
          lv_status = 'T'.
          persist_result( is_invoice = gw_billing_item
                         iv_status = lv_status
                         iv_json = lv_json ).
          ADD 1 TO gv_success_count.
        ELSE.
          " Live mode - call API
          TRY.
            lw_response = call_jwms_api( iv_json = lv_json ).
            
            IF lw_response-http_status = 200 OR lw_response-http_status = 201.
              " Success
              lv_status = 'X'.
              ADD 1 TO gv_success_count.
              go_logger->log_success( 'Invoice pushed successfully: ' && gw_billing_item-vbeln ).
            ELSE.
              " Failure
              lv_status = 'F'.
              ADD 1 TO gv_failed_count.
              go_logger->log_error( 'API failure for invoice ' && gw_billing_item-vbeln ).
            ENDIF.
            
            " Persist result
            persist_result( is_invoice = gw_billing_item
                           iv_status = lv_status
                           iv_json = lv_json
                           iv_response = lw_response-body
                           iv_ack_no = lw_response-ack_number
                           iv_http_status = lw_response-http_status ).
            
          CATCH zcx_api_error INTO lo_exception.
            " API error
            lv_status = 'F'.
            ADD 1 TO gv_failed_count.
            go_logger->log_error( lo_exception->get_text( ) ).
            persist_result( is_invoice = gw_billing_item
                           iv_status = lv_status
                           iv_json = lv_json
                           iv_response = lo_exception->get_text( ) ).
          ENDTRY.
        ENDIF.
        
    ENDCASE.
    
    " Package processing
    ADD 1 TO lv_package_count.
    IF lv_package_count >= lc_package_size.
      commit_package( ).
      lv_package_count = 0.
    ENDIF.
    
  ENDLOOP.
  
  " Commit remaining
  IF lv_package_count > 0.
    commit_package( ).
  ENDIF.
  
  " Save log
  IF sy-batch = abap_true.
    go_logger->save_log( ).
  ENDIF.
  
ENDMETHOD.
```

---

## 8. JWMS API Integration

### 8.1 API Configuration (from ZWSO2APIDTL)

**Read Logic:**

```abap
METHOD read_api_config.
  DATA: lw_api_config TYPE zwso2apidtl.
  
  SELECT SINGLE *
    FROM zwso2apidtl
    INTO lw_api_config
    WHERE interface_name = 'JWMS_INVOICE_PUSH'
      AND active = 'X'.
  
  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE zcx_config_missing
      EXPORTING
        textid = zcx_config_missing=>api_config_missing.
  ENDIF.
  
  gv_api_endpoint = lw_api_config-endpoint_url.
  gv_api_auth_type = lw_api_config-auth_type.  " 'OAUTH' / 'BASIC' / 'APIKEY'
  gv_api_username = lw_api_config-username.
  gv_api_password = lw_api_config-password.
  gv_api_token = lw_api_config-token.
  
ENDMETHOD.
```

### 8.2 HTTP Client Creation

```abap
METHOD create_http_client.
  DATA: lv_url TYPE string.
  
  lv_url = gv_api_endpoint.
  
  CALL METHOD cl_http_client=>create_by_url
    EXPORTING
      url                = lv_url
    IMPORTING
      client             = ro_http_client
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
  
  " Set timeout (30 seconds)
  ro_http_client->timeout = 30.
  
ENDMETHOD.
```

### 8.3 Authentication Handling

```abap
METHOD set_authentication.
  DATA: lv_auth_string TYPE string,
        lv_encoded TYPE string.
  
  CASE gv_api_auth_type.
    WHEN 'OAUTH'.
      " OAuth Bearer token
      io_http_client->request->set_header_field(
        name  = 'Authorization'
        value = |Bearer { gv_api_token }| ).  " NetWeaver 7.31: No string templates!
      " Use CONCATENATE instead:
      CONCATENATE 'Bearer' gv_api_token INTO lv_auth_string SEPARATED BY space.
      io_http_client->request->set_header_field(
        name  = 'Authorization'
        value = lv_auth_string ).
      
    WHEN 'BASIC'.
      " Basic Authentication
      CONCATENATE gv_api_username ':' gv_api_password INTO lv_auth_string.
      " Base64 encode (use function module or class)
      CALL FUNCTION 'SCMS_BASE64_ENCODE_STR'
        EXPORTING
          input  = lv_auth_string
        IMPORTING
          output = lv_encoded.
      CONCATENATE 'Basic' lv_encoded INTO lv_auth_string SEPARATED BY space.
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
```

### 8.4 API Call and Response Parsing

```abap
METHOD call_api.
  DATA: lo_http_client TYPE REF TO if_http_client,
        lv_response TYPE string,
        lv_http_code TYPE i.
  
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
    RAISE EXCEPTION TYPE zcx_api_error
      EXPORTING
        textid = zcx_api_error=>api_receive_failed.
  ENDIF.
  
  " Get response
  lv_response = lo_http_client->response->get_cdata( ).
  lo_http_client->response->get_status(
    IMPORTING
      code = lv_http_code ).
  
  " Parse response and extract acknowledgment number
  es_response-http_status = lv_http_code.
  es_response-body = lv_response.
  es_response-ack_number = parse_ack_number( iv_response = lv_response ).
  
  " Close connection
  lo_http_client->close( ).
  
ENDMETHOD.
```

### 8.5 Response Parsing (Extract Acknowledgment Number)

```abap
METHOD parse_ack_number.
  DATA: lv_json TYPE string,
        lv_ack_no TYPE string.
  
  lv_json = iv_response.
  
  " Simple JSON parsing (assuming response format: {"ackNumber": "12345"})
  " For production, use /UI2/CL_JSON or similar JSON parser
  
  FIND REGEX '"ackNumber"\s*:\s*"([^"]+)"' IN lv_json
    SUBMATCHES lv_ack_no.
  
  IF sy-subrc = 0.
    rv_ack_number = lv_ack_no.
  ENDIF.
  
ENDMETHOD.
```

---

## 9. JSON Payload Construction

### 9.1 Build Invoice JSON

```abap
METHOD build_invoice_json.
  DATA: lv_json TYPE string,
        lv_items_json TYPE string,
        lv_date TYPE string.
  
  " Header section
  lv_date = format_date( is_header-fkdat ).
  
  CONCATENATE '{'
    '"AccountId":"' is_config-accountid '",'
    '"BusinessId":"' is_config-businessid '",'
    '"SubBusinessId":"' is_config-sub_businessid '",'
    '"Invoice":{'
      '"BillingDocument":"' is_header-vbeln '",'
      '"BillingDate":"' lv_date '",'
      '"CompanyCode":"' is_header-bukrs '",'
      '"Division":"' is_header-spart '",'
      '"SalesOrganization":"' is_header-vkorg '",'
      '"BillToCustomer":"' is_header-kunrg '",'
      '"BillToName":"' escape_json_string( is_header-kunrg_name ) '",'
      '"SoldToCustomer":"' is_header-kunag '",'
      '"SoldToName":"' escape_json_string( is_header-kunag_name ) '",'
      '"Items":['
    INTO lv_json.
  
  " Items section
  LOOP AT it_items INTO lw_item.
    IF sy-tabix > 1.
      CONCATENATE lv_json ',' INTO lv_json.
    ENDIF.
    
    CONCATENATE lv_json '{'
      '"ItemNumber":"' lw_item-posnr '",'
      '"Material":"' lw_item-matnr '",'
      '"Quantity":"' format_decimal( lw_item-fkimg ) '",'
      '"UnitOfMeasure":"' lw_item-vrkme '",'
      '"NetValue":"' format_decimal( lw_item-netwr ) '",'
      '"Currency":"' lw_item-waerk '"'
      '}' INTO lv_json.
  ENDLOOP.
  
  " Close JSON
  CONCATENATE lv_json ']}}' INTO lv_json.
  
  rv_json = lv_json.
  
ENDMETHOD.
```

### 9.2 Helper Methods

```abap
METHOD escape_json_string.
  DATA: lv_output TYPE string.
  
  lv_output = iv_input.
  
  " Escape special characters
  REPLACE ALL OCCURRENCES OF '\' IN lv_output WITH '\\'.
  REPLACE ALL OCCURRENCES OF '"' IN lv_output WITH '\"'.
  REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN lv_output WITH '\n'.
  REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN lv_output WITH '\n'.
  
  rv_output = lv_output.
  
ENDMETHOD.

METHOD format_date.
  " Convert SAP date (YYYYMMDD) to JSON format (YYYY-MM-DD)
  DATA: lv_year(4) TYPE c,
        lv_month(2) TYPE c,
        lv_day(2) TYPE c.
  
  lv_year = iv_date+0(4).
  lv_month = iv_date+4(2).
  lv_day = iv_date+6(2).
  
  CONCATENATE lv_year '-' lv_month '-' lv_day INTO rv_formatted_date.
  
ENDMETHOD.

METHOD format_decimal.
  " Format decimal with 2 decimal places
  DATA: lv_string TYPE string.
  
  lv_string = iv_value.
  CONDENSE lv_string NO-GAPS.
  
  rv_formatted = lv_string.
  
ENDMETHOD.
```

---

## 10. Database Operations - ZSCM_INVOICE_INTERFACE

### 10.1 Persist Result Method (Insert/Update)

**Purpose:** Insert new record or update existing record in `ZSCM_INVOICE_INTERFACE` table.

```abap
METHOD persist_result.
  " Method signature:
  " IMPORTING: is_invoice TYPE ty_invoice_data
  "            iv_status TYPE char1
  "            iv_json TYPE string
  "            iv_response TYPE string OPTIONAL
  "            iv_ack_no TYPE zack_number OPTIONAL
  "            iv_http_status TYPE i OPTIONAL
  
  DATA: lw_interface TYPE zscm_invoice_interface,
        lv_counter TYPE i.
  
  " Read existing record
  SELECT SINGLE *
    FROM zscm_invoice_interface
    INTO lw_interface
    WHERE mandt = sy-mandt
      AND interface_type = 'JWMS_INV'
      AND vbeln = is_invoice-vbeln
      AND fkdat = is_invoice-fkdat
      AND posnr = is_invoice-posnr
    ORDER BY counter DESCENDING.
  
  IF sy-subrc = 0.
    " Record exists - UPDATE scenario (retry)
    lv_counter = lw_interface-counter + 1.
    
    " Check max retry limit
    IF lv_counter >= 1000.
      " Max retry reached - log and mark as final failure
      go_logger->log_error( 'Max retry count reached for invoice ' && is_invoice-vbeln ).
      MESSAGE ID 'ZSCM_INV' TYPE 'E' NUMBER '050' WITH is_invoice-vbeln.
      RETURN.
    ENDIF.
    
    " Update existing record
    UPDATE zscm_invoice_interface
      SET status = iv_status
          counter = lv_counter
          json_payload = iv_json
          api_response = iv_response
          ack_number = iv_ack_no
          http_status = iv_http_status
          aedat = sy-datum
          aezet = sy-uzeit
          aenam = sy-uname
      WHERE mandt = sy-mandt
        AND interface_type = 'JWMS_INV'
        AND vbeln = is_invoice-vbeln
        AND fkdat = is_invoice-fkdat
        AND posnr = is_invoice-posnr
        AND counter = lw_interface-counter.
    
    IF sy-subrc = 0.
      " Log successful update
      go_logger->log_message( iv_msgty = 'S'
                              iv_msgid = 'ZSCM_INV'
                              iv_msgno = '060'
                              iv_msgv1 = is_invoice-vbeln
                              iv_msgv2 = lv_counter ).
    ELSE.
      " Log update failure
      go_logger->log_error( 'Failed to update interface table for invoice ' && is_invoice-vbeln ).
    ENDIF.
    
  ELSE.
    " No existing record - INSERT scenario (new invoice)
    lv_counter = 1.
    
    " Build new record
    CLEAR lw_interface.
    lw_interface-mandt = sy-mandt.
    lw_interface-interface_type = 'JWMS_INV'.
    lw_interface-vbeln = is_invoice-vbeln.
    lw_interface-fkdat = is_invoice-fkdat.
    lw_interface-posnr = is_invoice-posnr.
    lw_interface-counter = lv_counter.
    lw_interface-status = iv_status.
    lw_interface-division = is_invoice-spart.
    lw_interface-bukrs = is_invoice-bukrs.
    lw_interface-kunrg = is_invoice-kunrg.
    lw_interface-kunrg_name = is_invoice-kunrg_name.
    lw_interface-kunag = is_invoice-kunag.
    lw_interface-kunag_name = is_invoice-kunag_name.
    lw_interface-accountid = gv_account_id.
    lw_interface-businessid = gv_business_id.
    lw_interface-sub_businessid = gv_sub_business_id.
    lw_interface-json_payload = iv_json.
    lw_interface-api_response = iv_response.
    lw_interface-ack_number = iv_ack_no.
    lw_interface-http_status = iv_http_status.
    lw_interface-erdat = sy-datum.
    lw_interface-erzet = sy-uzeit.
    lw_interface-ernam = sy-uname.
    lw_interface-aedat = sy-datum.
    lw_interface-aezet = sy-uzeit.
    lw_interface-aenam = sy-uname.
    
    " Insert new record
    INSERT zscm_invoice_interface FROM lw_interface.
    
    IF sy-subrc = 0.
      " Log successful insert
      go_logger->log_message( iv_msgty = 'S'
                              iv_msgid = 'ZSCM_INV'
                              iv_msgno = '061'
                              iv_msgv1 = is_invoice-vbeln ).
    ELSE.
      " Log insert failure
      go_logger->log_error( 'Failed to insert interface table for invoice ' && is_invoice-vbeln ).
    ENDIF.
    
  ENDIF.
  
ENDMETHOD.
```

### 10.2 Check Idempotency Method

**Purpose:** Check invoice processing status and determine action (PUSH/SKIP/RETRY).

```abap
METHOD check_idempotency.
  " Method signature:
  " IMPORTING: iv_vbeln TYPE vbeln_vf
  "            iv_fkdat TYPE fkdat
  "            iv_posnr TYPE posnr
  " RETURNING: VALUE(rv_action) TYPE char10
  
  DATA: lw_interface TYPE zscm_invoice_interface.
  
  " Read latest record for this invoice item
  SELECT SINGLE *
    FROM zscm_invoice_interface
    INTO lw_interface
    WHERE mandt = sy-mandt
      AND interface_type = 'JWMS_INV'
      AND vbeln = iv_vbeln
      AND fkdat = iv_fkdat
      AND posnr = iv_posnr
    ORDER BY counter DESCENDING.
  
  IF sy-subrc = 0.
    " Record exists - check status
    CASE lw_interface-status.
      WHEN 'X'.
        " Successfully pushed - skip
        rv_action = 'SKIP'.
        
      WHEN 'S'.
        " Business exclusion - skip
        rv_action = 'SKIP'.
        
      WHEN 'F'.
        " Failed - retry
        IF lw_interface-counter >= 1000.
          " Max retry reached - skip
          rv_action = 'SKIP'.
          go_logger->log_warning( 'Max retry count reached for invoice ' && iv_vbeln ).
        ELSE.
          " Retry allowed
          rv_action = 'RETRY'.
        ENDIF.
        
      WHEN 'T'.
        " Test mode record - can be reprocessed
        rv_action = 'PUSH'.
        
      WHEN OTHERS.
        " Unknown status - treat as new
        rv_action = 'PUSH'.
        
    ENDCASE.
    
  ELSE.
    " No record exists - push new invoice
    rv_action = 'PUSH'.
    
  ENDIF.
  
ENDMETHOD.
```

### 10.3 Read Existing Records (for Bulk Check)

**Purpose:** Bulk read of existing interface records for performance optimization.

```abap
METHOD read_interface_records.
  DATA: lt_vbeln TYPE TABLE OF ty_vbeln_range,
        lt_interface TYPE TABLE OF zscm_invoice_interface.
  
  " Build range table for VBELN
  LOOP AT gt_billing_items INTO gw_billing_item.
    APPEND gw_billing_item-vbeln TO lt_vbeln.
  ENDLOOP.
  
  SORT lt_vbeln.
  DELETE ADJACENT DUPLICATES FROM lt_vbeln.
  
  " Bulk read interface records
  IF lt_vbeln IS NOT INITIAL.
    SELECT *
      FROM zscm_invoice_interface
      INTO TABLE gt_interface_records
      FOR ALL ENTRIES IN lt_vbeln
      WHERE interface_type = 'JWMS_INV'
        AND vbeln = lt_vbeln-table_line.
  ENDIF.
  
  " Sort by key + counter descending for latest record lookup
  SORT gt_interface_records BY vbeln fkdat posnr counter DESCENDING.
  
ENDMETHOD.
```

### 10.4 Complete Usage Example

**How these methods work together in the main processing loop:**

```abap
METHOD process_invoices.
  DATA: lv_action TYPE char10,
        lv_json TYPE string,
        lw_response TYPE ty_api_response,
        lv_status TYPE char1.
  
  " Bulk read existing interface records (performance optimization)
  read_interface_records( ).
  
  LOOP AT gt_billing_items INTO gw_billing_item.
    " Check idempotency (determines if we should process this invoice)
    lv_action = check_idempotency( iv_vbeln = gw_billing_item-vbeln
                                    iv_fkdat = gw_billing_item-fkdat
                                    iv_posnr = gw_billing_item-posnr ).
    
    CASE lv_action.
      WHEN 'SKIP'.
        " Already processed or excluded - skip
        ADD 1 TO gv_skipped_count.
        CONTINUE.
        
      WHEN 'PUSH' OR 'RETRY'.
        " Build JSON payload
        lv_json = build_json_payload( is_invoice = gw_billing_item ).
        
        " Test mode check
        IF p_test = abap_true.
          " Test mode - no API call
          lv_status = 'T'.
          persist_result( is_invoice = gw_billing_item
                         iv_status = lv_status
                         iv_json = lv_json ).
          ADD 1 TO gv_success_count.
          
        ELSE.
          " Live mode - call JWMS API
          TRY.
            lw_response = call_jwms_api( iv_json = lv_json ).
            
            IF lw_response-http_status = 200 OR lw_response-http_status = 201.
              " Success
              lv_status = 'X'.
              ADD 1 TO gv_success_count.
              
              " Persist result with acknowledgment number
              persist_result( is_invoice = gw_billing_item
                             iv_status = lv_status
                             iv_json = lv_json
                             iv_response = lw_response-body
                             iv_ack_no = lw_response-ack_number
                             iv_http_status = lw_response-http_status ).
              
            ELSE.
              " API failure
              lv_status = 'F'.
              ADD 1 TO gv_failed_count.
              
              " Persist result with error response
              persist_result( is_invoice = gw_billing_item
                             iv_status = lv_status
                             iv_json = lv_json
                             iv_response = lw_response-body
                             iv_http_status = lw_response-http_status ).
            ENDIF.
            
          CATCH zcx_api_error INTO lo_exception.
            " Exception during API call
            lv_status = 'F'.
            ADD 1 TO gv_failed_count.
            
            " Persist result with exception message
            persist_result( is_invoice = gw_billing_item
                           iv_status = lv_status
                           iv_json = lv_json
                           iv_response = lo_exception->get_text( ) ).
          ENDTRY.
          
        ENDIF.
        
    ENDCASE.
    
    " Package processing
    ADD 1 TO gv_package_count.
    IF gv_package_count >= lc_package_size.
      COMMIT WORK AND WAIT.
      gv_package_count = 0.
    ENDIF.
    
  ENDLOOP.
  
  " Final commit
  IF gv_package_count > 0.
    COMMIT WORK AND WAIT.
  ENDIF.
  
ENDMETHOD.
```

---

## 11. Error Handling and Logging

### 10.1 Exception Classes

| Exception Class | Purpose | Messages |
|----------------|---------|----------|
| `ZCX_NO_AUTHORITY` | Authorization check failed | AUTH-001 |
| `ZCX_CONFIG_MISSING` | Configuration missing | CFG-001 |
| `ZCX_INVALID_INPUT` | Invalid selection parameters | SEL-001, SEL-002 |
| `ZCX_DATA_MISSING` | Required data missing | DATA-001 |
| `ZCX_API_ERROR` | API call failed | API-001, API-002 |
| `ZCX_SYSTEM_ERROR` | Unexpected system error | SYS-001 |

### 10.2 Application Log (BAL)

**Create Log:**

```abap
METHOD initialize_logger.
  DATA: lw_log TYPE bal_s_log,
        lv_extnumber TYPE balnrext.
  
  " External number: YYYYMMDD_HHMMSS
  CONCATENATE sy-datum '_' sy-uzeit INTO lv_extnumber.
  
  lw_log-object = 'ZSCM'.
  lw_log-subobject = 'INV_PUSH'.
  lw_log-extnumber = lv_extnumber.
  lw_log-aluser = sy-uname.
  lw_log-alprog = sy-repid.
  
  CALL FUNCTION 'BAL_LOG_CREATE'
    EXPORTING
      i_s_log      = lw_log
    IMPORTING
      e_log_handle = gv_log_handle.
  
ENDMETHOD.
```

**Add Message:**

```abap
METHOD log_message.
  DATA: lw_msg TYPE bal_s_msg.
  
  lw_msg-msgty = iv_msgty.
  lw_msg-msgid = iv_msgid.
  lw_msg-msgno = iv_msgno.
  lw_msg-msgv1 = iv_msgv1.
  lw_msg-msgv2 = iv_msgv2.
  lw_msg-msgv3 = iv_msgv3.
  lw_msg-msgv4 = iv_msgv4.
  
  CALL FUNCTION 'BAL_LOG_MSG_ADD'
    EXPORTING
      i_log_handle = gv_log_handle
      i_s_msg      = lw_msg.
  
  " Also display in foreground if not batch
  IF sy-batch = abap_false.
    MESSAGE ID iv_msgid TYPE iv_msgty NUMBER iv_msgno
      WITH iv_msgv1 iv_msgv2 iv_msgv3 iv_msgv4.
  ENDIF.
  
ENDMETHOD.
```

**Save Log:**

```abap
METHOD save_log.
  CALL FUNCTION 'BAL_DB_SAVE'
    EXPORTING
      i_client         = sy-mandt
      i_save_all       = 'X'
      i_t_log_handle   = VALUE #( ( gv_log_handle ) )
    EXCEPTIONS
      log_not_found    = 1
      save_not_allowed = 2
      numbering_error  = 3
      OTHERS           = 4.
  
  IF sy-subrc = 0.
    MESSAGE 'Application log saved successfully'(030) TYPE 'S'.
  ENDIF.
  
ENDMETHOD.
```

---

## 11. Security and Compliance

### 11.1 Authorization Check

```abap
METHOD authorization_check.
  AUTHORITY-CHECK OBJECT 'S_TCODE'
                  ID 'TCD' FIELD 'ZSCM_INVOICE_PUSH'.
  
  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE zcx_no_authority
      EXPORTING
        textid = zcx_no_authority=>not_authorized.
  ENDIF.
  
ENDMETHOD.
```

### 11.2 No Hardcoding Rule

- All configuration from `ZLOG_EXEC_VAR` and `ZWSO2APIDTL`.
- No hardcoded credentials, URLs, or business constants.
- Use text elements for all user-facing messages.
- Use message class `ZSCM_INV` for structured messages.

---

## 12. Performance Optimization

### 12.1 Package Processing

```abap
CONSTANTS: lc_package_size TYPE i VALUE 1000.

METHOD commit_package.
  COMMIT WORK AND WAIT.
  
  " Log package commit
  go_logger->log_message( iv_msgty = 'I'
                          iv_msgid = 'ZSCM_INV'
                          iv_msgno = '040'
                          iv_msgv1 = lv_package_count ).
  
ENDMETHOD.
```

### 12.2 Optimized Data Selection

```abap
METHOD select_billing_data.
  DATA: lt_vbeln TYPE TABLE OF vbeln_vf.
  
  " Select headers
  SELECT vbeln fkdat bukrs spart vkorg kunrg kunag
    FROM vbrk
    INTO TABLE gt_billing_headers
    WHERE fkart IN s_fkart
      AND fkdat BETWEEN p_fkdat_f AND p_fkdat_t
      AND bukrs = p_bukrs
      AND vkorg IN s_vkorg
      AND fksto = space.
  
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.
  
  " Collect VBELN for items selection
  lt_vbeln = VALUE #( FOR <header> IN gt_billing_headers ( <header>-vbeln ) ).
  
  " Select items using FOR ALL ENTRIES
  IF lt_vbeln IS NOT INITIAL.
    SELECT vbeln posnr matnr fkimg vrkme netwr waerk
      FROM vbrp
      INTO TABLE gt_billing_items
      FOR ALL ENTRIES IN lt_vbeln
      WHERE vbeln = lt_vbeln-table_line.
  ENDIF.
  
  " Enrich customer data
  enrich_customer_data( ).
  
ENDMETHOD.

METHOD enrich_customer_data.
  DATA: lt_kunnr TYPE TABLE OF kunnr,
        lt_kna1 TYPE TABLE OF kna1.
  
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
      INTO TABLE lt_kna1
      FOR ALL ENTRIES IN lt_kunnr
      WHERE kunnr = lt_kunnr-table_line.
  ENDIF.
  
  " Update headers with customer names
  LOOP AT gt_billing_headers ASSIGNING FIELD-SYMBOL(<header>).
    READ TABLE lt_kna1 INTO DATA(lw_kna1) WITH KEY kunnr = <header>-kunrg.
    IF sy-subrc = 0.
      <header>-kunrg_name = lw_kna1-name1.
    ENDIF.
    
    READ TABLE lt_kna1 INTO lw_kna1 WITH KEY kunnr = <header>-kunag.
    IF sy-subrc = 0.
      <header>-kunag_name = lw_kna1-name1.
    ENDIF.
  ENDLOOP.
  
ENDMETHOD.
```

---

## 13. Testing Strategy

### 13.1 Unit Tests

| Test Case | Test Method | Expected Result |
|-----------|-------------|-----------------|
| Authorization check | Test `authorization_check` with authorized/unauthorized user | Exception raised for unauthorized |
| Date range validation | Test `validate_date_range` with invalid range | Error displayed |
| Config missing | Test `read_configuration` with missing config | Exception raised |
| JSON builder | Test `build_invoice_json` with sample data | Valid JSON output |
| Idempotency check | Test `check_idempotency` with different statuses | Correct action returned |

### 13.2 Integration Tests

| Test Scenario | Steps | Expected Outcome |
|--------------|-------|------------------|
| AT-001: Happy path | Run with valid data and config | All invoices pushed, STATUS = X |
| AT-002: Config missing | Remove config entry, run program | Program aborts with CFG-001 |
| AT-003: API failure | Simulate API failure | STATUS = F, error logged |
| AT-004: Duplicate processing | Run twice with same data | Second run skips invoices (STATUS = X) |
| AT-005: Test mode | Run with P_TEST = X | Payload logged, no API call |
| AT-006: Background job | Schedule as background job | Application log created and saved |

---

## 14. Deployment Checklist

- [ ] Create custom tables: `ZSCM_INVOICE_INTERFACE`
- [ ] Create message class: `ZSCM_INV` with messages 001-050
- [ ] Create application log object: `ZSCM`, subobject: `INV_PUSH`
- [ ] Create exception classes: `ZCX_NO_AUTHORITY`, `ZCX_CONFIG_MISSING`, `ZCX_API_ERROR`, etc.
- [ ] Develop report: `ZSCM_INVOICE_PUSH` with includes (TOP, SEL, C01)
- [ ] Create transaction code: `ZSCM_INVOICE_PUSH`
- [ ] Configure `ZLOG_EXEC_VAR` with billing types, IDs
- [ ] Configure `ZWSO2APIDTL` with API endpoint and authentication
- [ ] Create background variant: `ZSCM_INV_PUSH_DEFAULT`
- [ ] Transport all objects to QAS/PRD
- [ ] Execute integration tests
- [ ] Schedule background job (if needed)

---

## 15. Message Class (ZSCM_INV)

| Message No | Type | Message Text |
|-----------|------|--------------|
| 001 | E | Configuration missing: & |
| 002 | E | Invalid date range: From-date must be <= To-date |
| 003 | E | Company code & not found |
| 004 | I | Processing invoice & |
| 005 | W | Customer data missing for invoice & |
| 006 | E | JWMS API call failed: & |
| 007 | E | JWMS API timeout |
| 008 | S | Invoice & pushed successfully |
| 009 | E | Not authorized to run program |
| 010 | I | Invoice & skipped (already processed) |
| 020 | I | Test mode: Invoice & (no API call) |
| 021 | I | JSON Payload: & |
| 030 | S | Application log saved successfully |
| 040 | I | Package committed: & records |
| 050 | E | Max retry count (1000) reached for invoice & |
| 060 | S | Interface table updated for invoice & (retry &) |
| 061 | S | Interface table record created for invoice & |

---

## 16. Open Questions / Assumptions

**All questions resolved. Ready for development.**

---

**End of Technical Specification**
