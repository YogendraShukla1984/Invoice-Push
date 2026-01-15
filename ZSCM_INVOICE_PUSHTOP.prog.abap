*&---------------------------------------------------------------------*
*& Include  ZSCM_INVOICE_PUSHTOP
*&---------------------------------------------------------------------*
*& Global Type Definitions, Data Declarations, and Constants
*&---------------------------------------------------------------------*

" BEGIN: Cursor Generated Code

*----------------------------------------------------------------------*
* Type Definitions
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_billing_header,
         vbeln      TYPE vbrk-vbeln,
         fkdat      TYPE vbrk-fkdat,
         bukrs      TYPE vbrk-bukrs,
         spart      TYPE vbrk-spart,
         vkorg      TYPE vbrk-vkorg,
         fkart      TYPE vbrk-fkart,
         kunrg      TYPE vbrk-kunrg,
         kunag      TYPE vbrk-kunag,
         kunrg_name TYPE kna1-name1,
         kunag_name TYPE kna1-name1,
       END OF ty_billing_header.

TYPES: tt_billing_headers TYPE STANDARD TABLE OF ty_billing_header
                          WITH NON-UNIQUE DEFAULT KEY.

TYPES: BEGIN OF ty_billing_item,
         vbeln TYPE vbrp-vbeln,
         posnr TYPE vbrp-posnr,
         fkdat TYPE vbrk-fkdat,
         matnr TYPE vbrp-matnr,
         fkimg TYPE vbrp-fkimg,
         vrkme TYPE vbrp-vrkme,
         netwr TYPE vbrp-netwr,
         waerk TYPE vbrp-waerk,
       END OF ty_billing_item.

TYPES: tt_billing_items TYPE STANDARD TABLE OF ty_billing_item
                        WITH NON-UNIQUE DEFAULT KEY.

TYPES: BEGIN OF ty_config,
         name   TYPE zlog_exec_var-name,
         value  TYPE zlog_exec_var-value,
         value1 TYPE zlog_exec_var-value1,
         value2 TYPE zlog_exec_var-value2,
         active TYPE zlog_exec_var-active,
       END OF ty_config.

TYPES: tt_config TYPE STANDARD TABLE OF ty_config
                 WITH NON-UNIQUE DEFAULT KEY.

TYPES: BEGIN OF ty_api_config,
         interface_name TYPE zwso2apidtl-interface_name,
         endpoint_url   TYPE zwso2apidtl-endpoint_url,
         auth_type      TYPE zwso2apidtl-auth_type,
         username       TYPE zwso2apidtl-username,
         password       TYPE zwso2apidtl-password,
         token          TYPE zwso2apidtl-token,
         timeout        TYPE zwso2apidtl-timeout,
         active         TYPE zwso2apidtl-active,
       END OF ty_api_config.

TYPES: BEGIN OF ty_api_response,
         http_status TYPE i,
         body        TYPE string,
         ack_number  TYPE zack_number,
       END OF ty_api_response.

TYPES: BEGIN OF ty_customer,
         kunnr TYPE kna1-kunnr,
         name1 TYPE kna1-name1,
       END OF ty_customer.

TYPES: tt_customers TYPE STANDARD TABLE OF ty_customer
                    WITH NON-UNIQUE KEY kunnr.

TYPES: BEGIN OF ty_invoice_data,
         vbeln      TYPE vbrk-vbeln,
         fkdat      TYPE vbrk-fkdat,
         posnr      TYPE vbrp-posnr,
         bukrs      TYPE vbrk-bukrs,
         spart      TYPE vbrk-spart,
         vkorg      TYPE vbrk-vkorg,
         kunrg      TYPE vbrk-kunrg,
         kunrg_name TYPE kna1-name1,
         kunag      TYPE vbrk-kunag,
         kunag_name TYPE kna1-name1,
         matnr      TYPE vbrp-matnr,
         fkimg      TYPE vbrp-fkimg,
         vrkme      TYPE vbrp-vrkme,
         netwr      TYPE vbrp-netwr,
         waerk      TYPE vbrp-waerk,
       END OF ty_invoice_data.

TYPES: tt_interface_records TYPE STANDARD TABLE OF zscm_invoice_interface
                            WITH NON-UNIQUE DEFAULT KEY.

*----------------------------------------------------------------------*
* Global Data
*----------------------------------------------------------------------*
DATA: gt_billing_headers   TYPE tt_billing_headers,
      gw_billing_header    TYPE ty_billing_header,
      gt_billing_items     TYPE tt_billing_items,
      gw_billing_item      TYPE ty_billing_item,
      gt_interface_records TYPE tt_interface_records,
      gw_interface_record  TYPE zscm_invoice_interface,
      gt_customers         TYPE tt_customers,
      gw_customer          TYPE ty_customer.

DATA: gv_account_id        TYPE zaccountid,
      gv_business_id       TYPE zbusinessid,
      gv_sub_business_id   TYPE zsub_businessid,
      gv_api_endpoint      TYPE string,
      gv_api_auth_type     TYPE char10,
      gv_api_username      TYPE string,
      gv_api_password      TYPE string,
      gv_api_token         TYPE string,
      gv_api_timeout       TYPE i.

DATA: gv_log_handle        TYPE balloghndl,
      gv_processed_count   TYPE i,
      gv_success_count     TYPE i,
      gv_failed_count      TYPE i,
      gv_skipped_count     TYPE i,
      gv_package_count     TYPE i,
      gv_run_status        TYPE char10.

DATA: gt_billing_types     TYPE STANDARD TABLE OF fkart WITH EMPTY KEY.

*----------------------------------------------------------------------*
* Constants
*----------------------------------------------------------------------*
CONSTANTS: lc_interface_type TYPE char20 VALUE 'JWMS_INV',
           lc_package_size   TYPE i VALUE 1000,
           lc_max_retry      TYPE i VALUE 1000,
           lc_status_success TYPE char1 VALUE 'X',
           lc_status_failed  TYPE char1 VALUE 'F',
           lc_status_skipped TYPE char1 VALUE 'S',
           lc_status_test    TYPE char1 VALUE 'T'.

CONSTANTS: lc_action_push   TYPE char10 VALUE 'PUSH',
           lc_action_skip   TYPE char10 VALUE 'SKIP',
           lc_action_retry  TYPE char10 VALUE 'RETRY'.

CONSTANTS: lc_msgid         TYPE sy-msgid VALUE 'ZSCM_INV',
           lc_log_object    TYPE balobj_d VALUE 'ZSCM',
           lc_log_subobject TYPE balsubobj VALUE 'INV_PUSH'.

" END: Cursor Generated Code
