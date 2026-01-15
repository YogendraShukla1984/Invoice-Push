
# Function Specification: SAP → JWMS Invoice Push Interface

## 1. Document Control

| Attribute | Value |
|----------|-------|
| Document Title | SAP Invoice Interface – JWMS (Function Specification) |
| Program Name | ZSCM_INVOICE_PUSH |
| Transaction Code | ZSCM_INVOICE_PUSH |
| Package | ZLOG |
| Responsible Team | Ganesh Patil |
| Version | 1.0 |
| Prepared By | Ganesh Patil |
| Date | January 2026 |
| Interface Type | Outbound (SAP → JWMS) |
| Execution Mode | Foreground & Background |

---

## 2. Description

This interface extracts SAP billing header and item data from standard tables (VBRK/VBRP), constructs a JSON payload per configured mapping, and pushes invoices to JWMS via API. The process is **re-runnable**, **idempotent**, and **status-driven**. Already processed invoices (STATUS = X) are not re-sent. Failed invoices (STATUS = F) are automatically retried up to a maximum of 1000 attempts.

The program supports both **foreground** (interactive with progress messages) and **background** (batch job with application log) execution.

---

## 3. Inputs

### 3.1 Selection Screen Parameters

| Field Name | SAP Name | Type | Required | Default | Constraints | Validation Rules |
|------------|----------|------|----------|---------|-------------|------------------|
| Billing Date From | P_FKDAT_F | DATE | Yes | Current Date | VBRK-FKDAT | Must be <= P_FKDAT_T |
| Billing Date To | P_FKDAT_T | DATE | Yes | Current Date | VBRK-FKDAT | Must be >= P_FKDAT_F |
| Company Code | P_BUKRS | BUKRS | Yes | - | VBRK-BUKRS | Must exist in T001 |
| Billing Type | S_FKART | FKART (range) | Yes | - | VBRK-FKART | Must exist in active config |
| Sales Organization | S_VKORG | VKORG (range) | No | - | VBRK-VKORG | Optional filter |
| Test Mode | P_TEST | ABAP_BOOL | No | No (unchecked) | Checkbox | If checked, no API call; log payload only |

### 3.2 Configuration Inputs (from SAP tables)

| Field Name | Source Table | Key/Filter | Required | Description |
|------------|--------------|------------|----------|-------------|
| Billing Types (Active) | ZLOG_EXEC_VAR | NAME = 'ZSCM_GET_INV_BILLINGTYPE', ACTIVE = 'X' | Yes | List of valid billing types |
| Account ID | ZLOG_EXEC_VAR | NAME = 'ZSCM_ACCOUNTID' | Yes | JWMS Account Identifier |
| Business ID | ZLOG_EXEC_VAR | NAME = 'ZSCM_BUSINESSID' | Yes | JWMS Business Identifier |
| Sub-Business ID | ZLOG_EXEC_VAR | NAME = 'ZSCM_SUB_BUSINESSID' | Yes | JWMS Sub-Business Identifier |
| API Endpoint | ZWSO2APIDTL | Lookup by interface name | Yes | HTTPS URL for JWMS API |
| API Authentication | ZWSO2APIDTL | Auth config | Yes | OAuth/Basic/API key (no hardcoding) |

---

## 4. Output

### 4.1 Program Output

| Field Name | Type | Description |
|------------|------|-------------|
| run_status | STRING | Overall run status (SUCCESS / PARTIAL / FAILED) |
| processed_count | INTEGER | Total invoices evaluated |
| success_count | INTEGER | Invoices pushed successfully (STATUS = X) |
| failed_count | INTEGER | Invoices failed to push (STATUS = F) |
| skipped_count | INTEGER | Invoices skipped (STATUS = X or S) |
| log_id | STRING | Application log reference (background) or message summary (foreground) |
| last_error | STRING | Last error message when run_status = FAILED or PARTIAL |

### 4.2 Foreground Output

- Display progress messages during processing
- Show summary ALV or message with counts (success, failed, skipped)
- Display application log on request

### 4.3 Background Output

- Application log (BAL) with detailed messages
- Job log summary with counts

---

## 5. Behavior (Step-by-Step)

1. **Authorization Check**  
   Validate user authorization using `S_TCODE` for transaction code ZSCM_INVOICE_PUSH. Abort if unauthorized.

2. **Selection Screen Validation**  
   - Validate date range: P_FKDAT_F <= P_FKDAT_T.  
   - Validate company code exists in T001.  
   - Validate billing types are provided.

3. **Read Configuration**  
   - Read active billing types from ZLOG_EXEC_VAR (NAME = 'ZSCM_GET_INV_BILLINGTYPE', ACTIVE = 'X').  
   - Read ACCOUNTID, BUSINESSID, SUB_BUSINESSID from ZLOG_EXEC_VAR.  
   - Read API endpoint, authentication method, and credentials from ZWSO2APIDTL.  
   - **Abort if any mandatory configuration is missing** (error CFG-001).

4. **Select Billing Headers (VBRK)**  
   - FKART IN S_FKART (and in active billing types from config).  
   - FKDAT BETWEEN P_FKDAT_F AND P_FKDAT_T.  
   - BUKRS = P_BUKRS.  
   - VKORG IN S_VKORG (if provided).  
   - FKSTO = SPACE (exclude cancelled invoices).

5. **Select Billing Items (VBRP)**  
   - VBELN from selected headers in step 4.

6. **Enrich Customer Data (KNA1)**  
   - Read customer names (NAME1) for KUNRG (bill-to) and KUNAG (sold-to) from KNA1.  
   - **Skip invoice if customer data is missing** and log DATA-001 warning.

7. **Build JSON Payload** (refer to JSON Mapping in section 9)  
   - Map SAP fields to JWMS JSON structure.  
   - Include ACCOUNTID, BUSINESSID, SUB_BUSINESSID from configuration.

8. **Idempotency Check (ZSCM_INVOICE_INTERFACE)**  
   For each invoice item, read ZSCM_INVOICE_INTERFACE by key:  
   - INTERFACE_TYPE = 'JWMS_INV'  
   - VBELN = billing document  
   - FKDAT = billing date  
   - POSNR = item number  
   - COUNTER = current retry count  

   **Decision Matrix:**
   | Status | Action |
   |--------|--------|
   | No record exists | Push invoice (new entry) |
   | STATUS = 'X' | Skip (already pushed successfully) |
   | STATUS = 'F' | Retry (increment COUNTER, push again) |
   | STATUS = 'S' | Skip (business-defined exclusion) |

9. **API Call (if not test mode)**  
   - If P_TEST = 'X': Log payload only, do not call API, set STATUS = 'T' (test).  
   - Else: POST JSON payload to JWMS API endpoint via HTTPS.  
   - Capture HTTP response code, response body, and acknowledgment number.  
   - **Timeout**: 30 seconds (configurable).  
   - **Retry logic**: Immediate retry on network/timeout error (do not wait).

10. **Response Processing**  
    - **Success** (HTTP 200/201):  
      - Set STATUS = 'X'.  
      - Store acknowledgment number from API response.  
    - **Failure** (HTTP 4xx/5xx or timeout):  
      - Set STATUS = 'F'.  
      - Increment COUNTER.  
      - Store error response.  
    - **Max Retry Check**: If COUNTER >= 1000, mark as final failure and log.

11. **Persist Results (ZSCM_INVOICE_INTERFACE)**  
    Insert or update record with:  
    - JSON payload (request)  
    - API response (acknowledgment number or error)  
    - STATUS (X/F/S/T)  
    - COUNTER (retry count)  
    - Timestamps and user info.

12. **Package Processing and Commit**  
    - Process invoices in packages of 1000 records.  
    - `COMMIT WORK AND WAIT` after each package.  
    - Update progress indicator (foreground) or log (background).

13. **Logging**  
    - **Foreground**: Display progress messages and summary ALV.  
    - **Background**: Use application log (BAL) with object 'ZSCM', subobject 'INV_PUSH'.  
    - Log all errors, warnings, and summary counts.

14. **Final Summary**  
    - Return run_status, counts, and log_id.

---

## 6. Errors

| Error Code | Error Message | Trigger Condition | Handling |
|------------|---------------|-------------------|----------|
| CFG-001 | Configuration missing in ZLOG_EXEC_VAR or ZWSO2APIDTL | Required config entries not found | Abort program, log error |
| SEL-001 | Invalid date range: From-date > To-date | P_FKDAT_F > P_FKDAT_T | Stop at selection screen, display error |
| SEL-002 | Company code not found | P_BUKRS not in T001 | Stop at selection screen, display error |
| DATA-001 | Customer data missing for invoice {VBELN} | KUNRG or KUNAG not found in KNA1 | Skip invoice, log warning, continue |
| API-001 | JWMS API call failed: {error details} | HTTP error, timeout, or non-success response | Set STATUS = F, log error, continue |
| API-002 | JWMS API timeout | No response within 30 seconds | Set STATUS = F, retry immediately |
| AUTH-001 | Not authorized to run program | S_TCODE authorization check fails | Abort program, display error |
| SYS-001 | Unexpected system error: {error} | Unhandled runtime exception | Log error, mark run as FAILED |
| RETRY-001 | Max retry count (1000) reached for invoice {VBELN} | COUNTER >= 1000 | Log as final failure, mark STATUS = F, stop retrying |

---

## 7. Success

- All invoices within the selection range are evaluated and processed according to status rules.
- New invoices are pushed to JWMS and marked STATUS = X.
- Failed invoices (STATUS = F) are retried and COUNTER is incremented.
- Already processed invoices (STATUS = X) are skipped.
- Interface tracking table (ZSCM_INVOICE_INTERFACE) is updated with payload, response, acknowledgment number, status, and retry counter.
- Response structure includes run_status, counts, log_id, and last_error (if any).

---

## 8. Edge Cases

| Edge Case | Handling |
|-----------|----------|
| No invoices in selection range | Complete successfully with zero counts; log info message |
| Invoices already marked STATUS = X | Skip without API call; increment skipped_count |
| API partial failure (some succeed, some fail) | Mark failed items STATUS = F, successful items STATUS = X; set run_status = PARTIAL |
| Missing customer data (KUNRG/KUNAG not in KNA1) | Skip invoice, log DATA-001 warning, continue processing |
| Network timeout during API call | Mark STATUS = F, log API-002, retry immediately (no wait) |
| COUNTER reaches 1000 | Log RETRY-001 (final failure), stop retrying this invoice, continue with others |
| Test mode (P_TEST = X) | Log payload without API call, set STATUS = T, display summary |
| Configuration incomplete (missing ACCOUNTID, etc.) | Abort immediately with CFG-001 before data selection |

---

## 9. JSON Payload Mapping (Section 7 Reference)

### 9.1 JSON Structure

```json
{
  "AccountId": "<<from ZLOG_EXEC_VAR>>",
  "BusinessId": "<<from ZLOG_EXEC_VAR>>",
  "SubBusinessId": "<<from ZLOG_EXEC_VAR>>",
  "Invoice": {
    "BillingDocument": "<<VBRK-VBELN>>",
    "BillingDate": "<<VBRK-FKDAT>>",
    "CompanyCode": "<<VBRK-BUKRS>>",
    "Division": "<<VBRK-SPART>>",
    "SalesOrganization": "<<VBRK-VKORG>>",
    "BillToCustomer": "<<VBRK-KUNRG>>",
    "BillToName": "<<KNA1-NAME1 for KUNRG>>",
    "SoldToCustomer": "<<VBRK-KUNAG>>",
    "SoldToName": "<<KNA1-NAME1 for KUNAG>>",
    "Items": [
      {
        "ItemNumber": "<<VBRP-POSNR>>",
        "Material": "<<VBRP-MATNR>>",
        "Quantity": "<<VBRP-FKIMG>>",
        "UnitOfMeasure": "<<VBRP-VRKME>>",
        "NetValue": "<<VBRP-NETWR>>",
        "Currency": "<<VBRP-WAERK>>"
      }
    ]
  }
}
```

### 9.2 Field Mapping Table

| JSON Field | SAP Source | Mandatory | Notes |
|------------|-----------|-----------|-------|
| AccountId | ZLOG_EXEC_VAR (NAME = 'ZSCM_ACCOUNTID') | Yes | From config |
| BusinessId | ZLOG_EXEC_VAR (NAME = 'ZSCM_BUSINESSID') | Yes | From config |
| SubBusinessId | ZLOG_EXEC_VAR (NAME = 'ZSCM_SUB_BUSINESSID') | Yes | From config |
| BillingDocument | VBRK-VBELN | Yes | - |
| BillingDate | VBRK-FKDAT | Yes | Format: YYYY-MM-DD |
| CompanyCode | VBRK-BUKRS | Yes | - |
| Division | VBRK-SPART | Yes | - |
| SalesOrganization | VBRK-VKORG | Yes | - |
| BillToCustomer | VBRK-KUNRG | Yes | - |
| BillToName | KNA1-NAME1 (for KUNRG) | Yes | Skip invoice if missing |
| SoldToCustomer | VBRK-KUNAG | Yes | - |
| SoldToName | KNA1-NAME1 (for KUNAG) | Yes | Skip invoice if missing |
| ItemNumber | VBRP-POSNR | Yes | - |
| Material | VBRP-MATNR | Yes | - |
| Quantity | VBRP-FKIMG | Yes | - |
| UnitOfMeasure | VBRP-VRKME | Yes | - |
| NetValue | VBRP-NETWR | Yes | - |
| Currency | VBRP-WAERK | Yes | - |

---

## 10. Dependencies

- **External Services**: JWMS API (HTTPS endpoint from ZWSO2APIDTL)
- **SAP Standard Tables**: VBRK (billing header), VBRP (billing item), KNA1 (customer master), T001 (company code)
- **Custom Tables**: ZSCM_INVOICE_INTERFACE (tracking), ZLOG_EXEC_VAR (configuration), ZWSO2APIDTL (API configuration)
- **Events**: Background job scheduling (optional, for automated runs)
- **Other**: Application log (BAL_*), HTTP client (CL_HTTP_CLIENT), message class ZSCM_INV

---

## 11. Performance

- **Max Latency**: Batch-oriented; no single long-running API call (use packages).
- **Throughput**: Process in packages of 1000 invoice items per commit.
- **Resource Constraints**: Avoid full-table scans; use indexed keys for selections (VBELN, FKDAT, BUKRS, FKART).
- **Expected Volume**: Up to 100,000 invoices per run (configurable based on date range).
- **Max Runtime**: 30 minutes per background job (configurable).

---

## 12. Security

- **Authentication**: Use secured credentials from ZWSO2APIDTL (OAuth/Basic/API key); no hardcoding.
- **Authorization**: Check S_TCODE authorization at program start.
- **PII Masking**: Not applicable for system-to-system integration unless required by JWMS.
- **Data Encryption**: HTTPS mandatory for all API calls.
- **Business-Level Restrictions**: None (authorization controlled via S_TCODE only).

---

## 13. Logging

### 13.1 Foreground Mode
- Display progress messages during processing (e.g., "Processing invoice 100 of 500...").
- Show summary ALV or message with counts (success, failed, skipped) at end.
- Option to display application log (if needed).

### 13.2 Background Mode
- Use application log (BAL):
  - **Object**: ZSCM
  - **Subobject**: INV_PUSH
  - **External Number**: YYYYMMDD_HHMMSS (timestamp)
- Log all errors (type 'E'), warnings (type 'W'), and success messages (type 'S').
- Save log to database at end of processing.

### 13.3 Message Class
- **Message Class ID**: ZSCM_INV
- **Message Numbers**: 001-050 reserved for invoice interface messages.

---

## 14. Test Mode Behavior

When P_TEST = 'X' (Test Mode checkbox is checked):
- **No API call**: JWMS API is NOT invoked.
- **Payload logging**: JSON payload is constructed and logged to application log or displayed in foreground.
- **Status**: Records marked with STATUS = 'T' (test) in ZSCM_INVOICE_INTERFACE.
- **Purpose**: Validate data selection, payload construction, and configuration without affecting JWMS.

---

## 15. Retry Policy

| Attribute | Value |
|-----------|-------|
| Max Retry Count | 1000 |
| Retry Interval | Immediate (no delay between retries within same run) |
| Retry Logic | Automatic (STATUS = F invoices are retried on every program run) |
| Final Failure | When COUNTER >= 1000, log RETRY-001 and stop retrying |
| Manual Retry | User can reset STATUS to F and COUNTER to 0 to force retry |

---

## 16. Status Management

| Status | Meaning | Set By | Retry Behavior |
|--------|---------|--------|----------------|
| X | Successfully pushed to JWMS | Program (after API success) | Do not retry (skip) |
| F | Failed to push | Program (after API failure) | Retry automatically |
| S | Skipped (business exclusion) | Manual (business user via table maintenance) | Do not retry (skip) |
| T | Test mode (not sent) | Program (when P_TEST = X) | Do not retry (informational) |

**Business-Controlled Status (S):**  
Status 'S' can be set manually by business users or administrators via table maintenance (SM30) to exclude specific invoices from processing without deleting them.

**Failure Retry:**  
Failures (STATUS = F) are retried automatically on every subsequent program run until COUNTER reaches 1000 or status is manually changed.

---

## 17. Error Handling Decisions

| Scenario | Decision | Action |
|----------|----------|--------|
| Customer data missing (KUNRG/KUNAG not in KNA1) | Skip invoice | Log DATA-001 warning, continue processing |
| Configuration missing (ZLOG_EXEC_VAR or ZWSO2APIDTL) | Stop immediately | Abort program with CFG-001 error (no partial processing) |
| API timeout or network error | Retry immediately | Set STATUS = F, increment COUNTER, continue with next invoice |
| HTTP 4xx/5xx error from JWMS API | Retry on next run | Set STATUS = F, store error response, continue processing |
| Max retry count (1000) reached | Final failure | Log RETRY-001, keep STATUS = F, stop automatic retries |

---

## 18. Scheduling

| Attribute | Value |
|-----------|-------|
| Frequency | On-demand (manual) or scheduled (daily/hourly as needed) |
| Background Variant | Yes (variant with default date range and parameters should be delivered) |
| Job Name Convention | ZSCM_INV_PUSH_YYYYMMDD |
| Recommended Schedule | Daily at 6:00 AM (after billing document creation) |

**Variant Delivery:**  
Deliver a background variant named `ZSCM_INV_PUSH_DEFAULT` with:
- P_FKDAT_F = SY-DATUM - 1 (yesterday)
- P_FKDAT_T = SY-DATUM - 1 (yesterday)
- P_BUKRS = (to be filled by business)
- S_FKART = (to be filled by business)
- P_TEST = unchecked (live mode)

---

## 19. Foreground/Background Processing

### 19.1 Selection Screen Layout
```
╔════════════════════════════════════════════════════════════════╗
║  SAP → JWMS Invoice Push Interface                             ║
╠════════════════════════════════════════════════════════════════╣
║  Selection Criteria                                            ║
║  ────────────────────────────────────────────────────────────  ║
║  Billing Date From:   [__________] (mandatory)                 ║
║  Billing Date To:     [__________] (mandatory)                 ║
║  Company Code:        [____] (mandatory)                       ║
║  Billing Type:        [________________] (mandatory, range)    ║
║  Sales Organization:  [________________] (optional, range)     ║
║                                                                ║
║  Options                                                       ║
║  ────────────────────────────────────────────────────────────  ║
║  [ ] Test Mode (no API call)                                   ║
╚════════════════════════════════════════════════════════════════╝
```

### 19.2 Processing Flow by Mode

#### Foreground Mode (Interactive)
1. Display progress messages (e.g., "Reading configuration...", "Selecting invoices...", "Pushing invoice 100 of 500...").
2. Update progress indicator during processing.
3. Display summary message or ALV at end with counts.
4. Allow user to view application log if errors occurred.

#### Background Mode (Batch Job)
1. No user interaction (SY-BATCH = X).
2. Use application log (BAL) for all messages.
3. Commit in packages (1000 records).
4. Save application log to database at end.
5. Job log shows summary counts.

### 19.3 Application Log Handling

**Foreground:**
- Create application log in memory.
- Display log on request (via button or menu option).
- Optional: Save log to database if user requests.

**Background:**
- Create application log and save to database (mandatory).
- Log object: ZSCM, subobject: INV_PUSH.
- External number: YYYYMMDD_HHMMSS.

### 19.4 Commit Strategy

- **Package Size**: 1000 invoice items per package.
- **Commit**: `COMMIT WORK AND WAIT` after each package.
- **Rationale**: Avoid memory issues, ensure data persistence, allow restart from last committed package.

---

## 20. Tests

| Test ID | Test Description | Expected Result |
|---------|------------------|-----------------|
| AT-001 | Process invoices in valid date range with configured billing types (happy path) | All invoices pushed successfully, STATUS = X, acknowledgment numbers stored |
| AT-002 | Missing configuration in ZLOG_EXEC_VAR triggers CFG-001 | Program aborts with error message, no data processing |
| AT-003 | API failure returns API-001 and sets STATUS = F | Failed invoices marked STATUS = F, COUNTER incremented, error logged |
| AT-004 | STATUS = X invoices are skipped | No API call for STATUS = X invoices, skipped_count incremented |
| AT-005 | STATUS = F invoices are retried and COUNTER increments | Retry invoice, COUNTER = COUNTER + 1, STATUS updated based on result |
| AT-006 | No invoices in selection range | Program completes successfully with zero counts, info message logged |
| AT-007 | Test mode (P_TEST = X) logs payload without API call | Payload logged, STATUS = T, no API call, summary displayed |
| AT-008 | Missing customer data (KUNRG not in KNA1) | Invoice skipped, DATA-001 warning logged, processing continues |
| AT-009 | Invalid date range (P_FKDAT_F > P_FKDAT_T) | Selection screen error SEL-001, program stops |
| AT-010 | COUNTER reaches 1000 | RETRY-001 logged, invoice marked as final failure, no further retries |
| AT-011 | Background job execution | Application log created and saved, job log shows summary counts |
| AT-012 | Foreground execution | Progress messages displayed, summary ALV shown at end |

---

## 21. Open Questions (Resolved)

All questions have been addressed with the provided information. If any additional clarification is needed during development, please contact the responsible team (Ganesh Patil).

---

**End of Function Specification**
