# Medakovic Inventory System

Medakovic Inventory System is a small terminal inventory application written
in COBOL. It is a fun retro-programming and learning project inspired by the
kind of menu-driven business software that made COBOL famous.

This is not production inventory, accounting, or point-of-sale software. It
does not provide authentication, transaction history, tax handling, concurrent
user controls, or enterprise backup and recovery.

## Features

- add products;
- receive stock at its purchase price and maintain weighted-average cost;
- sell stock using its current weighted-average price;
- display the active inventory;
- report products at or below their reorder level;
- calculate inventory value from quantity times unit price;
- export active inventory to a standards-compliant CSV file;
- edit product names, reorder levels, and prices;
- archive zero-stock products without destroying their records;
- display archived products and their archive dates;
- journal every inventory change and view it chronologically or by product ID.

Product IDs are case-insensitive and limited to 10 characters. IDs are
immutable after creation because they are database keys. Stock changes only
through the Receive and Sell operations.

Each option opens on a clean terminal screen. After completing an action or
viewing a report, press Enter to return to the menu. The main menu is arranged
in two columns so the larger set of options remains compact.

When stock is received, the operator enters both quantity and receipt unit
price. The application recalculates the product price using weighted average:

```text
new average = ((old quantity * old average) +
               (received quantity * receipt price)) / new quantity
```

Selling reduces quantity without changing the average unit price. The sale
screen and journal show the average price used and the resulting sale value.

## Build and run

Install [GnuCOBOL](https://gnucobol.sourceforge.io/), then run on Windows:

```powershell
cobc -x -Wall -o inventory.exe inventory.cob
$gnuCobolBin = Split-Path (Get-Command cobc).Source
$env:PATH = "$gnuCobolBin;$env:PATH"
.\inventory.exe
```

Putting the compiler directory first on `PATH` ensures that the executable
loads GnuCOBOL's own runtime DLLs. This can be necessary when another Windows
application has installed a different `libiconv` or `libintl` DLL.

### Permanent GnuCOBOL PATH on Windows 11

For a machine-wide setup, search Windows for **Edit the system environment
variables**, open **Environment Variables**, and edit `Path`. Add the GnuCOBOL
`bin` directory and place it before directories belonging to applications that
provide conflicting runtime DLLs. A typical per-user installation is:

```text
C:\Users\<username>\AppData\Local\Programs\GnuCOBOL 3.2\bin
```

Editing the System `Path` requires administrator rights. Restart VS Code,
terminals, and other applications afterward because running processes retain
their old environment.

As a non-administrator alternative for PowerShell and VS Code PowerShell
terminals, open the PowerShell profile:

```powershell
$profileDirectory = Split-Path $PROFILE
New-Item -ItemType Directory -Path $profileDirectory -Force | Out-Null
notepad $PROFILE
```

Add these lines to the profile:

```powershell
$gnuCobolBin = "$env:LOCALAPPDATA\Programs\GnuCOBOL 3.2\bin"

if ((Test-Path $gnuCobolBin) -and
    -not $env:PATH.StartsWith("$gnuCobolBin;")) {
    $env:PATH = "$gnuCobolBin;$env:PATH"
}
```

Open a new PowerShell window and verify DLL resolution:

```powershell
where.exe cobc
where.exe libiconv-2.dll
where.exe libintl-8.dll
```

The GnuCOBOL directory should be listed first. Avoid rewriting the complete
`PATH` with `setx PATH`; it can disturb ordering and is unsafe for long values.

On Linux or macOS, omit the `.exe` suffix:

```sh
cobc -x -Wall -o inventory inventory.cob
./inventory
```

The application creates its data files in the current working directory.

## Database structure

### Physical format

`inventory.dat`, `inventory-archive.dat`, and `inventory-journal.dat` are native
GnuCOBOL indexed files, not text, CSV, or simple fixed-record files. On the
Windows GnuCOBOL build used for this project, the indexed-file handler is
Berkeley DB (BDB).

Berkeley DB stores index pages and internal metadata together with the COBOL
records. Consequently:

- the `.dat` files are not meaningfully readable in a text editor;
- their physical size is larger than the sum of their logical records;
- bytes must not be edited manually because that can corrupt the index;
- files created by a different indexed-file handler or incompatible runtime
  are not guaranteed to be portable.

Use menu option 4 to view active records and option 10 to view archived
records. Menu option 7 produces the portable `inventory.csv` representation.

### Change journal

`inventory-journal.dat` is indexed by a monotonically increasing 10-digit
sequence number. It records all successful state-changing operations: Add,
Receive, Sell, Edit, and Archive. CSV export and report viewing are read-only
and therefore are not journaled.

Each journal record stores:

- sequence number and `YYYYMMDDHHMMSS` timestamp;
- product ID and action;
- quantity before, signed quantity change, and quantity after;
- product name before and after;
- reorder level before and after;
- average price before and after;
- receipt/sale transaction price;
- a short action description.

Menu option 11 displays entries in chronological sequence. Option 12 scans the
journal and displays only entries for a requested product ID. If a journal
write fails, the associated inventory operation is rolled back.

### Active inventory record

`inventory.dat` uses `PRODUCT-ID` as its unique index key. Every logical active
record is 61 bytes:

| Field | COBOL picture | Bytes | Description |
|---|---:|---:|---|
| `PRODUCT-ID` | `PIC X(10)` | 10 | Unique product key |
| `PRODUCT-NAME` | `PIC X(30)` | 30 | Product description |
| `PRODUCT-QUANTITY` | `PIC 9(6)` | 6 | Current stock quantity |
| `PRODUCT-REORDER-LEVEL` | `PIC 9(6)` | 6 | Low-stock threshold |
| `PRODUCT-UNIT-PRICE` | `PIC 9(7)V99` | 9 | Price with two implied decimal places |

`V` is an implied decimal point and consumes no byte. For example, the nine
stored display digits `000025000` represent `250.00`.

### Archive record

`inventory-archive.dat` is separately indexed by `ARCHIVE-PRODUCT-ID`. Its
logical record is 69 bytes: the original 61-byte product record followed by:

| Field | COBOL picture | Bytes | Description |
|---|---:|---:|---|
| `ARCHIVED-DATE` | `PIC X(8)` | 8 | Archive date in `YYYYMMDD` format |

A product can be archived only when its quantity is zero and the operator
types `ARCHIVE` to confirm. Archived product IDs remain reserved and cannot be
reused.

### CSV export

Menu option 7 creates or replaces `inventory.csv`. It contains one header row
and these columns:

```text
product_id,product_name,quantity,reorder_level,unit_price
```

Text values are quoted and embedded quotation marks are escaped according to
CSV rules. The file can be opened directly in Excel or another spreadsheet
application.

## Starting over

To start with empty active and archived inventories, close the application and
delete both runtime database files:

```text
inventory.dat
inventory-archive.dat
inventory-journal.dat
```

These files, generated CSV exports, backups, and compiled binaries are excluded
from Git because they are runtime or machine-specific artifacts.
