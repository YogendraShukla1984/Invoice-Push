*&---------------------------------------------------------------------*
*& Report  ZSCM_INVOICE_PUSH
*&---------------------------------------------------------------------*
*& Purpose: SAP → JWMS Invoice Push Interface
*& Author: Ganesh Patil
*& Creation Date: January 2026
*& Package: ZLOG
*& Transaction: ZSCM_INVOICE_PUSH
*&
*& Description:
*& Outbound interface to push SAP billing invoices to JWMS via API.
*& The program reads billing data (VBRK/VBRP), constructs JSON payload,
*& calls JWMS API, and tracks status in ZSCM_INVOICE_INTERFACE table.
*&
*& Features:
*& - Re-runnable and idempotent (status-driven)
*& - Background and foreground execution
*& - Retry logic with max 1000 attempts
*& - Package processing (1000 records per commit)
*& - Application log (BAL) support
*& - Test mode (no API call)
*&
*& Change History:
*& Date       User           Description
*& ---------- -------------- ------------------------------------------
*& 2026-01-15 Ganesh Patil   Initial creation
*&---------------------------------------------------------------------*
REPORT zscm_invoice_push.

" BEGIN: Cursor Generated Code

" Include files
INCLUDE zscm_invoice_pushtop.  " Global declarations
INCLUDE zscm_invoice_pushsel.  " Selection screen
INCLUDE zscm_invoice_pushc01.  " Class definitions and implementations

*----------------------------------------------------------------------*
* Global Objects
*----------------------------------------------------------------------*
DATA: go_report TYPE REF TO lcl_report.

*----------------------------------------------------------------------*
* Initialization
*----------------------------------------------------------------------*
INITIALIZATION.
  DATA: lo_exception TYPE REF TO zcx_no_authority.

  " Create report object
  CREATE OBJECT go_report.

  " Authorization check
  TRY.
      go_report->authorization_check( ).
    CATCH zcx_no_authority INTO lo_exception.
      MESSAGE lo_exception TYPE 'E'.
      LEAVE PROGRAM.
  ENDTRY.

*----------------------------------------------------------------------*
* At Selection Screen - Company Code Validation
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON p_bukrs.
  go_report->validate_company_code( ).

*----------------------------------------------------------------------*
* At Selection Screen - Date Range Validation
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON p_fkdat_f.
  go_report->validate_date_range( ).

AT SELECTION-SCREEN ON p_fkdat_t.
  go_report->validate_date_range( ).

*----------------------------------------------------------------------*
* Start of Selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
  DATA: lo_config_exception TYPE REF TO zcx_config_missing.

  " Initialize logger
  go_report->initialize_logger( ).

  " Read configuration
  TRY.
      go_report->read_configuration( ).
    CATCH zcx_config_missing INTO lo_config_exception.
      MESSAGE lo_config_exception TYPE 'E'.
      go_report->finalize_logger( ).
      LEAVE PROGRAM.
  ENDTRY.

  " Select billing data
  go_report->select_billing_data( ).

  " Check if data exists
  IF go_report->has_data( ) = abap_false.
    MESSAGE 'No invoices found in selection range'(003) TYPE 'I'.
    go_report->finalize_logger( ).
    LEAVE PROGRAM.
  ENDIF.

  " Process invoices
  go_report->process_invoices( ).

  " Display summary
  go_report->display_summary( ).

  " Finalize logger (save in background)
  go_report->finalize_logger( ).

" END: Cursor Generated Code
