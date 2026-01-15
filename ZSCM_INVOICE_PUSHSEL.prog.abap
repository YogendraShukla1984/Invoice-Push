*&---------------------------------------------------------------------*
*& Include  ZSCM_INVOICE_PUSHSEL
*&---------------------------------------------------------------------*
*& Selection Screen Definition
*&---------------------------------------------------------------------*

" BEGIN: Cursor Generated Code

*----------------------------------------------------------------------*
* Selection Screen - Block 1: Selection Criteria
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

" Billing Date Range
PARAMETERS: p_fkdat_f TYPE vbrk-fkdat OBLIGATORY DEFAULT sy-datum,
            p_fkdat_t TYPE vbrk-fkdat OBLIGATORY DEFAULT sy-datum.

" Company Code
PARAMETERS: p_bukrs TYPE vbrk-bukrs OBLIGATORY.

" Billing Type (Range)
SELECT-OPTIONS: s_fkart FOR vbrk-fkart OBLIGATORY.

" Sales Organization (Optional Range)
SELECT-OPTIONS: s_vkorg FOR vbrk-vkorg.

SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
* Selection Screen - Block 2: Options
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.

" Test Mode Checkbox
PARAMETERS: p_test TYPE abap_bool AS CHECKBOX DEFAULT abap_false.

SELECTION-SCREEN END OF BLOCK b2.

" END: Cursor Generated Code
