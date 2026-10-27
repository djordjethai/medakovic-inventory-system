# Medakovic Inventory System

Medakovic Inventory System is a small terminal inventory application written
in COBOL. It is a fun retro-programming and learning project inspired by the
kind of menu-driven business software that made COBOL famous.

This is not production inventory, accounting, or point-of-sale software. It
does not provide authentication, transaction history, tax handling, concurrent
user controls, or enterprise backup and recovery.

## Features

- add products;
- receive and sell stock;
- display the active inventory;
- report products at or below their reorder level;
- calculate inventory value from quantity times unit price;
- export active inventory to a standards-compliant CSV file;
- edit product names, reorder levels, and prices;
- archive zero-stock products without destroying their records;
- display archived products and their archive dates.

Product IDs are case-insensitive and limited to 10 characters. IDs are
immutable after creation because they are database keys. Stock changes only
through the Receive and Sell operations.

Each option opens on a clean terminal screen. After completing an action or
viewing a report, press Enter to return to the menu.

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

On Linux or macOS, omit the `.exe` suffix:

```sh
cobc -x -Wall -o inventory inventory.cob
./inventory
```

The application creates its data files in the current working directory.

## Database structure

### Physical format

`inventory.dat` and `inventory-archive.dat` are native GnuCOBOL indexed files,
not text, CSV, or simple fixed-record files. On the Windows GnuCOBOL build used
for this project, the indexed-file handler is Berkeley DB (BDB).

Berkeley DB stores index pages and internal metadata together with the COBOL
records. Consequently:

- the `.dat` files are not meaningfully readable in a text editor;
- their physical size is larger than the sum of their logical records;
- bytes must not be edited manually because that can corrupt the index;
- files created by a different indexed-file handler or incompatible runtime
  are not guaranteed to be portable.

Use menu option 4 to view active records and option 10 to view archived
records. Menu option 7 produces the portable `inventory.csv` representation.

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
```

These files, generated CSV exports, backups, and compiled binaries are excluded
from Git because they are runtime or machine-specific artifacts.
