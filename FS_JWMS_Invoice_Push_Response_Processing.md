
# Functional Specification  
## JWMS – Invoice Push Response Processing

---

## 1. Purpose

The purpose of this development is to process invoice push response acknowledgements received from an external platform (JWMS) and update the latest processing status in the custom interface table **ZSCM_INVOICE_INTERFACE** based on the API response.

This program will:
- Identify successfully pushed invoice records  
- Call an external API with acknowledgement numbers  
- Update response status returned by the API against corresponding invoice records  

---

## 2. Scope

### In Scope
- Selection of invoice interface records based on date and status  
- API endpoint resolution using configuration tables  
- Outbound API call with acknowledgement numbers  
- Update of interface status based on API response  
- Logging of execution details  

### Out of Scope
- Creation or modification of SAP billing documents  
- Re-pushing invoice data to JWMS  
- Error correction or manual reprocessing logic  

---

## 3. Tables Involved

### 3.1 ZSCM_INVOICE_INTERFACE (Custom)

**Purpose:**  
Stores invoice data sent to JWMS along with processing and response status.

**Primary Key:**
- MANDT  
- INTERFACE_TYPE  
- VBELN  
- FKDAT  
- POSNR  
- COUNTER  

**Processing Fields:**
- ACKNOWLEDGEMENT_NO  
- STATUS  
- CREATED_ON  
- UPDATED_ON  
- RESPONSE_MESSAGE  

---

### 3.2 ZLOG_EXEC_VAR

Stores execution-level logs such as start time, end time, record count, and errors.

---

### 3.3 ZWSO2APIDTL

Stores API endpoint configuration.

**Selection Criteria:**
- PLATFORM = JWMS  
- APINAME = INVPUSHRESP  
- HOST = HTTPS://  
- ACTIVE = X  

---

## 4. Selection Screen

| Parameter | Description |
|----------|-------------|
| P_FROM | Created On – From Date |
| P_TO | Created On – To Date |

---

## 5. Processing Logic

### Step 1: Read Interface Records
- Select records from ZSCM_INVOICE_INTERFACE  
- CREATED_ON between P_FROM and P_TO  
- STATUS = 'X'  
- Extract distinct ACKNOWLEDGEMENT_NO  

### Step 2: Fetch API Endpoint
- Read ZWSO2APIDTL  
- Validate active configuration  
- Retrieve API URL  

### Step 3: Prepare JSON Payload
```json
{
  "acknowledgement_no": "ACK123456"
}
```

### Step 4: API Call
- HTTP POST to JWMS endpoint  
- Capture HTTP status and response  

### Step 5: Response Handling
- Parse success / failure  
- Capture response message  

### Step 6: Update Interface Table
- Update latest record (highest COUNTER)  
- Update STATUS, RESPONSE_MESSAGE, UPDATED_ON  

### Step 7: Logging
- Write execution details to ZLOG_EXEC_VAR  
- Record success & failure counts  

---

## 6. Error Handling

| Scenario | Handling |
|--------|----------|
| No records found | Log & exit |
| Config missing | Log & stop |
| API failure | Mark failed |
| Invalid response | Log parsing error |

---

## 7. Assumptions

- ACKNOWLEDGEMENT_NO is unique per invoice  
- API returns definitive status  
- Network & SSL configured  

---

## 8. Dependencies

- ZSCM_INVOICE_INTERFACE exists  
- API config maintained  
- SSL & HTTP destination configured  
