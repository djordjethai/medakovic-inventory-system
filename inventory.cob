       >>SOURCE FORMAT FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MEDAKOVIC-INVENTORY.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INVENTORY-FILE ASSIGN TO "inventory.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS PRODUCT-ID
               FILE STATUS IS WS-FILE-STATUS.
           SELECT EXPORT-FILE ASSIGN TO "inventory.csv"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-EXPORT-STATUS.
           SELECT ARCHIVE-FILE ASSIGN TO "inventory-archive.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ARCHIVE-PRODUCT-ID
               FILE STATUS IS WS-ARCHIVE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  INVENTORY-FILE.
       01  PRODUCT-RECORD.
           05 PRODUCT-ID              PIC X(10).
           05 PRODUCT-NAME            PIC X(30).
           05 PRODUCT-QUANTITY        PIC 9(6).
           05 PRODUCT-REORDER-LEVEL   PIC 9(6).
           05 PRODUCT-UNIT-PRICE      PIC 9(7)V99.
       FD  EXPORT-FILE.
       01  EXPORT-RECORD              PIC X(120).
       FD  ARCHIVE-FILE.
       01  ARCHIVE-RECORD.
           05 ARCHIVE-PRODUCT-DATA.
               10 ARCHIVE-PRODUCT-ID      PIC X(10).
               10 ARCHIVE-PRODUCT-NAME    PIC X(30).
               10 ARCHIVE-QUANTITY        PIC 9(6).
               10 ARCHIVE-REORDER-LEVEL   PIC 9(6).
               10 ARCHIVE-UNIT-PRICE      PIC 9(7)V99.
           05 ARCHIVED-DATE               PIC X(8).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS             PIC XX.
       01  WS-EXPORT-STATUS           PIC XX.
       01  WS-ARCHIVE-STATUS          PIC XX.
       01  WS-CHOICE                  PIC X(2).
       01  WS-EXIT-FLAG               PIC X VALUE "N".
           88 EXIT-REQUESTED          VALUE "Y".
       01  WS-END-OF-FILE             PIC X VALUE "N".
           88 END-OF-FILE             VALUE "Y".
       01  WS-INPUT                   PIC X(30).
       01  WS-INPUT-ID                PIC X(10).
       01  WS-AMOUNT                  PIC 9(6).
       01  WS-NEW-QUANTITY            PIC 9(7).
       01  WS-ITEM-VALUE              PIC 9(13)V99.
       01  WS-TOTAL-VALUE             PIC 9(19)V99.
       01  WS-PRODUCT-COUNT           PIC 9(6).
       01  WS-LOW-STOCK-COUNT         PIC 9(6).
       01  WS-QUANTITY-DISPLAY        PIC ZZZ,ZZ9.
       01  WS-REORDER-DISPLAY         PIC ZZZ,ZZ9.
       01  WS-COUNT-DISPLAY           PIC ZZZ,ZZ9.
       01  WS-PRICE-DISPLAY           PIC Z,ZZZ,ZZ9.99.
       01  WS-VALUE-DISPLAY           PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
       01  WS-TOTAL-DISPLAY           PIC ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
       01  WS-EXPORT-FAILED           PIC X VALUE "N".
           88 EXPORT-FAILED           VALUE "Y".
       01  WS-CSV-SOURCE              PIC X(30).
       01  WS-CSV-SOURCE-LENGTH       PIC 9(2) COMP.
       01  WS-CSV-SOURCE-INDEX        PIC 9(2) COMP.
       01  WS-CSV-POSITION            PIC 9(3) COMP.
       01  WS-CSV-QUANTITY            PIC ZZZZZ9.
       01  WS-CSV-REORDER             PIC ZZZZZ9.
       01  WS-CSV-PRICE               PIC ZZZZZZ9.99.
       01  WS-VALUE-PROVIDED          PIC X VALUE "N".
           88 VALUE-PROVIDED          VALUE "Y".
       01  WS-VALID-INPUT             PIC X VALUE "N".
           88 VALID-INPUT             VALUE "Y".

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM OPEN-INVENTORY

           PERFORM UNTIL EXIT-REQUESTED
               PERFORM CLEAR-SCREEN
               PERFORM DISPLAY-MENU
               ACCEPT WS-CHOICE
               EVALUATE WS-CHOICE
                   WHEN "1"
                       PERFORM CLEAR-SCREEN
                       PERFORM ADD-PRODUCT
                       PERFORM PAUSE-FOR-ENTER
                   WHEN "2"
                       PERFORM CLEAR-SCREEN
                       PERFORM RECEIVE-STOCK
                       PERFORM PAUSE-FOR-ENTER
                   WHEN "3"
                       PERFORM CLEAR-SCREEN
                       PERFORM SELL-PRODUCT
                       PERFORM PAUSE-FOR-ENTER
                   WHEN "4"
                       PERFORM CLEAR-SCREEN
                       PERFORM DISPLAY-INVENTORY
                       PERFORM PAUSE-FOR-ENTER
                   WHEN "5"
                       PERFORM CLEAR-SCREEN
                       PERFORM LOW-STOCK-REPORT
                       PERFORM PAUSE-FOR-ENTER
                   WHEN "6"
                       PERFORM CLEAR-SCREEN
                       PERFORM VALUATION-REPORT
                       PERFORM PAUSE-FOR-ENTER
                   WHEN "7"
                       PERFORM CLEAR-SCREEN
                       PERFORM EXPORT-TO-CSV
                       PERFORM PAUSE-FOR-ENTER
                   WHEN "8"
                       PERFORM CLEAR-SCREEN
                       PERFORM EDIT-PRODUCT
                       PERFORM PAUSE-FOR-ENTER
                   WHEN "9"
                       PERFORM CLEAR-SCREEN
                       PERFORM ARCHIVE-PRODUCT
                       PERFORM PAUSE-FOR-ENTER
                   WHEN "10"
                       PERFORM CLEAR-SCREEN
                       PERFORM DISPLAY-ARCHIVE
                       PERFORM PAUSE-FOR-ENTER
                   WHEN "0"
                       SET EXIT-REQUESTED TO TRUE
                   WHEN OTHER
                       DISPLAY "INVALID CHOICE. PLEASE ENTER 0 TO 10."
                       PERFORM PAUSE-FOR-ENTER
               END-EVALUATE
           END-PERFORM

           CLOSE INVENTORY-FILE ARCHIVE-FILE
           DISPLAY "GOODBYE!"
           GOBACK.

       OPEN-INVENTORY.
           OPEN I-O INVENTORY-FILE
           IF WS-FILE-STATUS = "35"
               OPEN OUTPUT INVENTORY-FILE
               IF WS-FILE-STATUS NOT = "00"
                   PERFORM FILE-ERROR
               END-IF
               CLOSE INVENTORY-FILE
               OPEN I-O INVENTORY-FILE
               IF WS-FILE-STATUS NOT = "00"
                   PERFORM FILE-ERROR
               END-IF
           END-IF
           IF WS-FILE-STATUS NOT = "00"
               PERFORM FILE-ERROR
           END-IF

           OPEN I-O ARCHIVE-FILE
           IF WS-ARCHIVE-STATUS = "35"
               OPEN OUTPUT ARCHIVE-FILE
               IF WS-ARCHIVE-STATUS NOT = "00"
                   PERFORM ARCHIVE-FILE-ERROR
               END-IF
               CLOSE ARCHIVE-FILE
               OPEN I-O ARCHIVE-FILE
           END-IF
           IF WS-ARCHIVE-STATUS NOT = "00"
               PERFORM ARCHIVE-FILE-ERROR
           END-IF.

       FILE-ERROR.
           DISPLAY "INVENTORY FILE ERROR. STATUS: " WS-FILE-STATUS
           STOP RUN.

       ARCHIVE-FILE-ERROR.
           DISPLAY "ARCHIVE FILE ERROR. STATUS: " WS-ARCHIVE-STATUS
           STOP RUN.

       DISPLAY-MENU.
           DISPLAY SPACE
           DISPLAY "****************************************"
           DISPLAY "*       MEDAKOVIC INVENTORY SYSTEM     *"
           DISPLAY "*             VERSION 1.3              *"
           DISPLAY "****************************************"
           DISPLAY SPACE
           DISPLAY "1. Add new product"
           DISPLAY "2. Receive stock"
           DISPLAY "3. Sell product"
           DISPLAY "4. Display inventory"
           DISPLAY "5. Low stock report"
           DISPLAY "6. Inventory valuation report"
           DISPLAY "7. Export inventory to CSV"
           DISPLAY "8. Edit product"
           DISPLAY "9. Archive product"
           DISPLAY "10. Display archived products"
           DISPLAY "0. Exit"
           DISPLAY SPACE
           DISPLAY "ENTER YOUR CHOICE: " WITH NO ADVANCING.

       CLEAR-SCREEN.
           DISPLAY FUNCTION CHAR(28) "[2J"
                   FUNCTION CHAR(28) "[H"
                   WITH NO ADVANCING.

       PAUSE-FOR-ENTER.
           DISPLAY SPACE
           DISPLAY "PRESS ENTER TO RETURN TO THE MENU..."
                   WITH NO ADVANCING
           ACCEPT WS-INPUT.


       ADD-PRODUCT.
           DISPLAY SPACE
           DISPLAY "--- ADD NEW PRODUCT ---"
           PERFORM ACCEPT-PRODUCT-ID
           MOVE WS-INPUT-ID TO PRODUCT-ID
           READ INVENTORY-FILE
               INVALID KEY
                   CONTINUE
               NOT INVALID KEY
                   DISPLAY "PRODUCT " WS-INPUT-ID " ALREADY EXISTS."
                   EXIT PARAGRAPH
           END-READ

           MOVE WS-INPUT-ID TO ARCHIVE-PRODUCT-ID
           READ ARCHIVE-FILE
               INVALID KEY
                   CONTINUE
               NOT INVALID KEY
                   DISPLAY "PRODUCT ID " WS-INPUT-ID
                           " IS RESERVED BY AN ARCHIVED PRODUCT."
                   EXIT PARAGRAPH
           END-READ

           INITIALIZE PRODUCT-RECORD
           MOVE WS-INPUT-ID TO PRODUCT-ID
           DISPLAY "PRODUCT NAME: " WITH NO ADVANCING
           ACCEPT PRODUCT-NAME
           PERFORM ACCEPT-QUANTITY
           MOVE WS-AMOUNT TO PRODUCT-QUANTITY
           DISPLAY "REORDER LEVEL: " WITH NO ADVANCING
           PERFORM ACCEPT-NONNEGATIVE-NUMBER
           MOVE WS-AMOUNT TO PRODUCT-REORDER-LEVEL
           PERFORM ACCEPT-PRICE

           WRITE PRODUCT-RECORD
               INVALID KEY
                   DISPLAY "PRODUCT COULD NOT BE SAVED. STATUS: "
                           WS-FILE-STATUS
               NOT INVALID KEY
                   DISPLAY "PRODUCT ADDED SUCCESSFULLY."
           END-WRITE.

       RECEIVE-STOCK.
           DISPLAY SPACE
           DISPLAY "--- RECEIVE STOCK ---"
           PERFORM FIND-PRODUCT
           IF WS-FILE-STATUS NOT = "00"
               EXIT PARAGRAPH
           END-IF
           DISPLAY "PRODUCT: " PRODUCT-NAME
           DISPLAY "QUANTITY RECEIVED: " WITH NO ADVANCING
           PERFORM ACCEPT-POSITIVE-NUMBER
           COMPUTE WS-NEW-QUANTITY = PRODUCT-QUANTITY + WS-AMOUNT
           IF WS-NEW-QUANTITY > 999999
               DISPLAY "QUANTITY WOULD EXCEED THE MAXIMUM OF 999999."
               EXIT PARAGRAPH
           END-IF
           MOVE WS-NEW-QUANTITY TO PRODUCT-QUANTITY
           REWRITE PRODUCT-RECORD
               INVALID KEY
                   DISPLAY "STOCK COULD NOT BE UPDATED. STATUS: "
                           WS-FILE-STATUS
               NOT INVALID KEY
                   MOVE PRODUCT-QUANTITY TO WS-QUANTITY-DISPLAY
                   DISPLAY "STOCK RECEIVED. NEW QUANTITY: "
                           WS-QUANTITY-DISPLAY
           END-REWRITE.

       SELL-PRODUCT.
           DISPLAY SPACE
           DISPLAY "--- SELL PRODUCT ---"
           PERFORM FIND-PRODUCT
           IF WS-FILE-STATUS NOT = "00"
               EXIT PARAGRAPH
           END-IF
           DISPLAY "PRODUCT: " PRODUCT-NAME
           DISPLAY "QUANTITY TO SELL: " WITH NO ADVANCING
           PERFORM ACCEPT-POSITIVE-NUMBER
           IF WS-AMOUNT > PRODUCT-QUANTITY
               DISPLAY "INSUFFICIENT STOCK. AVAILABLE: "
                       PRODUCT-QUANTITY
               EXIT PARAGRAPH
           END-IF
           SUBTRACT WS-AMOUNT FROM PRODUCT-QUANTITY
           REWRITE PRODUCT-RECORD
               INVALID KEY
                   DISPLAY "SALE COULD NOT BE RECORDED. STATUS: "
                           WS-FILE-STATUS
               NOT INVALID KEY
                   MOVE PRODUCT-QUANTITY TO WS-QUANTITY-DISPLAY
                   DISPLAY "SALE RECORDED. REMAINING QUANTITY: "
                           WS-QUANTITY-DISPLAY
           END-REWRITE.

       EDIT-PRODUCT.
           DISPLAY SPACE
           DISPLAY "--- EDIT PRODUCT ---"
           PERFORM FIND-PRODUCT
           IF WS-FILE-STATUS NOT = "00"
               EXIT PARAGRAPH
           END-IF

           MOVE PRODUCT-QUANTITY TO WS-QUANTITY-DISPLAY
           MOVE PRODUCT-REORDER-LEVEL TO WS-REORDER-DISPLAY
           MOVE PRODUCT-UNIT-PRICE TO WS-PRICE-DISPLAY
           DISPLAY "PRODUCT ID:     " PRODUCT-ID
           DISPLAY "NAME:           " PRODUCT-NAME
           DISPLAY "QUANTITY:       " WS-QUANTITY-DISPLAY
           DISPLAY "REORDER LEVEL:  " WS-REORDER-DISPLAY
           DISPLAY "UNIT PRICE:     " WS-PRICE-DISPLAY
           DISPLAY SPACE
           DISPLAY "PRODUCT ID CANNOT BE CHANGED HERE."
           DISPLAY "USE RECEIVE/SELL TO CHANGE STOCK."
           DISPLAY SPACE

           DISPLAY "NEW NAME (ENTER TO KEEP): " WITH NO ADVANCING
           ACCEPT WS-INPUT
           IF WS-INPUT NOT = SPACES
               MOVE WS-INPUT TO PRODUCT-NAME
           END-IF

           DISPLAY "NEW REORDER LEVEL (ENTER TO KEEP): "
                   WITH NO ADVANCING
           PERFORM ACCEPT-OPTIONAL-NONNEGATIVE
           IF VALUE-PROVIDED
               MOVE WS-AMOUNT TO PRODUCT-REORDER-LEVEL
           END-IF

           DISPLAY "NEW UNIT PRICE (ENTER TO KEEP): "
                   WITH NO ADVANCING
           PERFORM ACCEPT-OPTIONAL-PRICE

           REWRITE PRODUCT-RECORD
               INVALID KEY
                   DISPLAY "PRODUCT COULD NOT BE UPDATED. STATUS: "
                           WS-FILE-STATUS
               NOT INVALID KEY
                   DISPLAY "PRODUCT UPDATED SUCCESSFULLY."
           END-REWRITE.

       ARCHIVE-PRODUCT.
           DISPLAY SPACE
           DISPLAY "--- ARCHIVE PRODUCT ---"
           PERFORM FIND-PRODUCT
           IF WS-FILE-STATUS NOT = "00"
               EXIT PARAGRAPH
           END-IF

           IF PRODUCT-QUANTITY NOT = ZERO
               MOVE PRODUCT-QUANTITY TO WS-QUANTITY-DISPLAY
               DISPLAY "PRODUCT CANNOT BE ARCHIVED WHILE STOCK IS "
                       WS-QUANTITY-DISPLAY "."
               DISPLAY "SELL OR OTHERWISE REDUCE STOCK TO ZERO FIRST."
               EXIT PARAGRAPH
           END-IF

           MOVE PRODUCT-UNIT-PRICE TO WS-PRICE-DISPLAY
           DISPLAY "PRODUCT ID: " PRODUCT-ID
           DISPLAY "NAME:       " PRODUCT-NAME
           DISPLAY "PRICE:      " WS-PRICE-DISPLAY
           DISPLAY SPACE
           DISPLAY "TYPE ARCHIVE TO CONFIRM: " WITH NO ADVANCING
           ACCEPT WS-INPUT
           MOVE FUNCTION UPPER-CASE(FUNCTION TRIM(WS-INPUT)) TO WS-INPUT
           IF WS-INPUT NOT = "ARCHIVE"
               DISPLAY "ARCHIVE CANCELLED."
               EXIT PARAGRAPH
           END-IF

           INITIALIZE ARCHIVE-RECORD
           MOVE PRODUCT-RECORD TO ARCHIVE-PRODUCT-DATA
           MOVE FUNCTION CURRENT-DATE(1:8) TO ARCHIVED-DATE
           WRITE ARCHIVE-RECORD
               INVALID KEY
                   DISPLAY "PRODUCT IS ALREADY ARCHIVED OR COULD NOT BE "
                           "SAVED. STATUS: " WS-ARCHIVE-STATUS
                   EXIT PARAGRAPH
           END-WRITE

           DELETE INVENTORY-FILE RECORD
               INVALID KEY
                   DELETE ARCHIVE-FILE RECORD
                       INVALID KEY
                           CONTINUE
                   END-DELETE
                   DISPLAY "ACTIVE PRODUCT COULD NOT BE REMOVED. STATUS: "
                           WS-FILE-STATUS
               NOT INVALID KEY
                   DISPLAY "PRODUCT ARCHIVED SUCCESSFULLY."
           END-DELETE.

       DISPLAY-ARCHIVE.
           DISPLAY SPACE
           DISPLAY "--- ARCHIVED PRODUCTS ---"
           DISPLAY "ID         NAME                           ARCHIVED"
           DISPLAY "---------- ------------------------------ ----------"
           MOVE ZERO TO WS-PRODUCT-COUNT
           MOVE "N" TO WS-END-OF-FILE
           MOVE LOW-VALUES TO ARCHIVE-PRODUCT-ID
           START ARCHIVE-FILE KEY IS GREATER THAN ARCHIVE-PRODUCT-ID
               INVALID KEY
                   SET END-OF-FILE TO TRUE
           END-START

           PERFORM UNTIL END-OF-FILE
               READ ARCHIVE-FILE NEXT RECORD
                   AT END
                       SET END-OF-FILE TO TRUE
                   NOT AT END
                       ADD 1 TO WS-PRODUCT-COUNT
                       DISPLAY ARCHIVE-PRODUCT-ID " "
                               ARCHIVE-PRODUCT-NAME " "
                               ARCHIVED-DATE(1:4) "-"
                               ARCHIVED-DATE(5:2) "-"
                               ARCHIVED-DATE(7:2)
               END-READ
           END-PERFORM

           IF WS-PRODUCT-COUNT = ZERO
               DISPLAY "NO PRODUCTS HAVE BEEN ARCHIVED."
           ELSE
               MOVE WS-PRODUCT-COUNT TO WS-COUNT-DISPLAY
               DISPLAY WS-COUNT-DISPLAY " ARCHIVED PRODUCT(S)."
           END-IF.

       DISPLAY-INVENTORY.
           DISPLAY SPACE
           DISPLAY "--- COMPLETE INVENTORY ---"
           DISPLAY "ID         NAME                           QTY"
                   "       PRICE"
           DISPLAY "---------- ------------------------------ "
                   "------ -----------"
           MOVE ZERO TO WS-PRODUCT-COUNT
           PERFORM START-FROM-BEGINNING
           PERFORM UNTIL END-OF-FILE
               READ INVENTORY-FILE NEXT RECORD
                   AT END
                       SET END-OF-FILE TO TRUE
                   NOT AT END
                       ADD 1 TO WS-PRODUCT-COUNT
                       MOVE PRODUCT-QUANTITY TO WS-QUANTITY-DISPLAY
                       MOVE PRODUCT-UNIT-PRICE TO WS-PRICE-DISPLAY
                       DISPLAY PRODUCT-ID " " PRODUCT-NAME " "
                               WS-QUANTITY-DISPLAY " "
                               WS-PRICE-DISPLAY
               END-READ
           END-PERFORM
           MOVE WS-PRODUCT-COUNT TO WS-COUNT-DISPLAY
           DISPLAY WS-COUNT-DISPLAY " PRODUCT(S).".

       LOW-STOCK-REPORT.
           DISPLAY SPACE
           DISPLAY "--- LOW STOCK REPORT ---"
           DISPLAY "ID         NAME                           QTY"
                   "    REORDER"
           DISPLAY "---------- ------------------------------ "
                   "------ ------"
           MOVE ZERO TO WS-LOW-STOCK-COUNT
           PERFORM START-FROM-BEGINNING
           PERFORM UNTIL END-OF-FILE
               READ INVENTORY-FILE NEXT RECORD
                   AT END
                       SET END-OF-FILE TO TRUE
                   NOT AT END
                       IF PRODUCT-QUANTITY <= PRODUCT-REORDER-LEVEL
                           ADD 1 TO WS-LOW-STOCK-COUNT
                           MOVE PRODUCT-QUANTITY
                               TO WS-QUANTITY-DISPLAY
                           MOVE PRODUCT-REORDER-LEVEL
                               TO WS-REORDER-DISPLAY
                           DISPLAY PRODUCT-ID " " PRODUCT-NAME " "
                                   WS-QUANTITY-DISPLAY " "
                                   WS-REORDER-DISPLAY
                       END-IF
               END-READ
           END-PERFORM
           IF WS-LOW-STOCK-COUNT = ZERO
               DISPLAY "NO PRODUCTS ARE LOW ON STOCK."
           ELSE
               MOVE WS-LOW-STOCK-COUNT TO WS-COUNT-DISPLAY
               DISPLAY WS-COUNT-DISPLAY " PRODUCT(S) NEED ATTENTION."
           END-IF.

       VALUATION-REPORT.
           DISPLAY SPACE
           DISPLAY "--- INVENTORY VALUATION REPORT ---"
           DISPLAY "ID         NAME                              VALUE"
           DISPLAY "---------- ------------------------------ "
                   "------------------"
           MOVE ZERO TO WS-TOTAL-VALUE WS-PRODUCT-COUNT
           PERFORM START-FROM-BEGINNING
           PERFORM UNTIL END-OF-FILE
               READ INVENTORY-FILE NEXT RECORD
                   AT END
                       SET END-OF-FILE TO TRUE
                   NOT AT END
                       ADD 1 TO WS-PRODUCT-COUNT
                       COMPUTE WS-ITEM-VALUE =
                           PRODUCT-QUANTITY * PRODUCT-UNIT-PRICE
                       ADD WS-ITEM-VALUE TO WS-TOTAL-VALUE
                       MOVE WS-ITEM-VALUE TO WS-VALUE-DISPLAY
                       DISPLAY PRODUCT-ID " " PRODUCT-NAME " "
                               WS-VALUE-DISPLAY
               END-READ
           END-PERFORM
           MOVE WS-TOTAL-VALUE TO WS-TOTAL-DISPLAY
           DISPLAY "--------------------------------------------------"
           DISPLAY "TOTAL INVENTORY VALUE: " WS-TOTAL-DISPLAY.

       EXPORT-TO-CSV.
           DISPLAY SPACE
           DISPLAY "--- EXPORT INVENTORY TO CSV ---"
           MOVE "N" TO WS-EXPORT-FAILED
           MOVE ZERO TO WS-PRODUCT-COUNT

           OPEN OUTPUT EXPORT-FILE
           IF WS-EXPORT-STATUS NOT = "00"
               DISPLAY "CSV FILE COULD NOT BE OPENED. STATUS: "
                       WS-EXPORT-STATUS
               EXIT PARAGRAPH
           END-IF

           MOVE SPACES TO EXPORT-RECORD
           MOVE "product_id,product_name,quantity,reorder_level,unit_price"
               TO EXPORT-RECORD
           WRITE EXPORT-RECORD
           IF WS-EXPORT-STATUS NOT = "00"
               SET EXPORT-FAILED TO TRUE
           END-IF

           IF NOT EXPORT-FAILED
               PERFORM START-FROM-BEGINNING
               PERFORM UNTIL END-OF-FILE OR EXPORT-FAILED
                   READ INVENTORY-FILE NEXT RECORD
                       AT END
                           SET END-OF-FILE TO TRUE
                       NOT AT END
                           PERFORM BUILD-CSV-RECORD
                           WRITE EXPORT-RECORD
                           IF WS-EXPORT-STATUS NOT = "00"
                               SET EXPORT-FAILED TO TRUE
                           ELSE
                               ADD 1 TO WS-PRODUCT-COUNT
                           END-IF
                   END-READ
               END-PERFORM
           END-IF

           CLOSE EXPORT-FILE
           IF EXPORT-FAILED
               DISPLAY "CSV EXPORT FAILED. STATUS: " WS-EXPORT-STATUS
           ELSE
               MOVE WS-PRODUCT-COUNT TO WS-COUNT-DISPLAY
               DISPLAY WS-COUNT-DISPLAY
                       " PRODUCT(S) EXPORTED TO inventory.csv."
           END-IF.

       BUILD-CSV-RECORD.
           MOVE SPACES TO EXPORT-RECORD
           MOVE 1 TO WS-CSV-POSITION

           MOVE '"' TO EXPORT-RECORD(WS-CSV-POSITION:1)
           ADD 1 TO WS-CSV-POSITION
           MOVE PRODUCT-ID TO WS-CSV-SOURCE
           PERFORM APPEND-CSV-TEXT
           MOVE '"' TO EXPORT-RECORD(WS-CSV-POSITION:1)
           ADD 1 TO WS-CSV-POSITION
           MOVE "," TO EXPORT-RECORD(WS-CSV-POSITION:1)
           ADD 1 TO WS-CSV-POSITION

           MOVE '"' TO EXPORT-RECORD(WS-CSV-POSITION:1)
           ADD 1 TO WS-CSV-POSITION
           MOVE PRODUCT-NAME TO WS-CSV-SOURCE
           PERFORM APPEND-CSV-TEXT
           MOVE '"' TO EXPORT-RECORD(WS-CSV-POSITION:1)
           ADD 1 TO WS-CSV-POSITION
           MOVE "," TO EXPORT-RECORD(WS-CSV-POSITION:1)
           ADD 1 TO WS-CSV-POSITION

           MOVE PRODUCT-QUANTITY TO WS-CSV-QUANTITY
           MOVE PRODUCT-REORDER-LEVEL TO WS-CSV-REORDER
           MOVE PRODUCT-UNIT-PRICE TO WS-CSV-PRICE
           STRING
               FUNCTION TRIM(WS-CSV-QUANTITY) DELIMITED BY SIZE
               "," DELIMITED BY SIZE
               FUNCTION TRIM(WS-CSV-REORDER) DELIMITED BY SIZE
               "," DELIMITED BY SIZE
               FUNCTION TRIM(WS-CSV-PRICE) DELIMITED BY SIZE
               INTO EXPORT-RECORD
               WITH POINTER WS-CSV-POSITION
           END-STRING.

       APPEND-CSV-TEXT.
           MOVE 30 TO WS-CSV-SOURCE-LENGTH
           PERFORM UNTIL WS-CSV-SOURCE-LENGTH = ZERO OR
                   WS-CSV-SOURCE(WS-CSV-SOURCE-LENGTH:1) NOT = SPACE
               SUBTRACT 1 FROM WS-CSV-SOURCE-LENGTH
           END-PERFORM

           PERFORM VARYING WS-CSV-SOURCE-INDEX FROM 1 BY 1
                   UNTIL WS-CSV-SOURCE-INDEX > WS-CSV-SOURCE-LENGTH
               IF WS-CSV-SOURCE(WS-CSV-SOURCE-INDEX:1) = '"'
                   MOVE '"' TO EXPORT-RECORD(WS-CSV-POSITION:1)
                   ADD 1 TO WS-CSV-POSITION
               END-IF
               MOVE WS-CSV-SOURCE(WS-CSV-SOURCE-INDEX:1)
                   TO EXPORT-RECORD(WS-CSV-POSITION:1)
               ADD 1 TO WS-CSV-POSITION
           END-PERFORM.

       START-FROM-BEGINNING.
           MOVE "N" TO WS-END-OF-FILE
           MOVE LOW-VALUES TO PRODUCT-ID
           START INVENTORY-FILE KEY IS GREATER THAN PRODUCT-ID
               INVALID KEY
                   SET END-OF-FILE TO TRUE
           END-START.

       FIND-PRODUCT.
           PERFORM ACCEPT-PRODUCT-ID
           MOVE WS-INPUT-ID TO PRODUCT-ID
           READ INVENTORY-FILE
               INVALID KEY
                   DISPLAY "PRODUCT " WS-INPUT-ID " WAS NOT FOUND."
           END-READ.

       ACCEPT-PRODUCT-ID.
           MOVE SPACES TO WS-INPUT-ID
           PERFORM UNTIL WS-INPUT-ID NOT = SPACES
               DISPLAY "PRODUCT ID: " WITH NO ADVANCING
               ACCEPT WS-INPUT-ID
               MOVE FUNCTION UPPER-CASE(WS-INPUT-ID) TO WS-INPUT-ID
               IF WS-INPUT-ID = SPACES
                   DISPLAY "PRODUCT ID IS REQUIRED."
               END-IF
           END-PERFORM.

       ACCEPT-QUANTITY.
           DISPLAY "INITIAL QUANTITY: " WITH NO ADVANCING
           PERFORM ACCEPT-NONNEGATIVE-NUMBER.

       ACCEPT-NONNEGATIVE-NUMBER.
           MOVE SPACES TO WS-INPUT
           PERFORM UNTIL FUNCTION TEST-NUMVAL(WS-INPUT) = ZERO
               ACCEPT WS-INPUT
               IF FUNCTION TEST-NUMVAL(WS-INPUT) NOT = ZERO
                   DISPLAY "ENTER A WHOLE NUMBER FROM 0 TO 999999: "
                           WITH NO ADVANCING
               ELSE
                   IF FUNCTION NUMVAL(WS-INPUT) < ZERO OR
                      FUNCTION NUMVAL(WS-INPUT) > 999999 OR
                      FUNCTION NUMVAL(WS-INPUT) NOT =
                          FUNCTION INTEGER(FUNCTION NUMVAL(WS-INPUT))
                       DISPLAY "ENTER A WHOLE NUMBER FROM 0 TO 999999: "
                               WITH NO ADVANCING
                       MOVE SPACES TO WS-INPUT
                   END-IF
               END-IF
           END-PERFORM
           COMPUTE WS-AMOUNT = FUNCTION NUMVAL(WS-INPUT).

       ACCEPT-OPTIONAL-NONNEGATIVE.
           MOVE "N" TO WS-VALUE-PROVIDED WS-VALID-INPUT
           PERFORM UNTIL VALID-INPUT
               ACCEPT WS-INPUT
               IF WS-INPUT = SPACES
                   SET VALID-INPUT TO TRUE
               ELSE
                   IF FUNCTION TEST-NUMVAL(WS-INPUT) = ZERO
                       IF FUNCTION NUMVAL(WS-INPUT) >= ZERO AND
                          FUNCTION NUMVAL(WS-INPUT) <= 999999 AND
                          FUNCTION NUMVAL(WS-INPUT) =
                              FUNCTION INTEGER(FUNCTION NUMVAL(WS-INPUT))
                           COMPUTE WS-AMOUNT = FUNCTION NUMVAL(WS-INPUT)
                           SET VALUE-PROVIDED TO TRUE
                           SET VALID-INPUT TO TRUE
                       ELSE
                           DISPLAY "ENTER 0 TO 999999, OR ENTER TO KEEP: "
                                   WITH NO ADVANCING
                       END-IF
                   ELSE
                       DISPLAY "ENTER 0 TO 999999, OR ENTER TO KEEP: "
                               WITH NO ADVANCING
                   END-IF
               END-IF
           END-PERFORM.

       ACCEPT-OPTIONAL-PRICE.
           MOVE "N" TO WS-VALUE-PROVIDED WS-VALID-INPUT
           PERFORM UNTIL VALID-INPUT
               ACCEPT WS-INPUT
               IF WS-INPUT = SPACES
                   SET VALID-INPUT TO TRUE
               ELSE
                   IF FUNCTION TEST-NUMVAL(WS-INPUT) = ZERO
                       IF FUNCTION NUMVAL(WS-INPUT) >= ZERO AND
                          FUNCTION NUMVAL(WS-INPUT) <= 9999999.99
                           COMPUTE PRODUCT-UNIT-PRICE ROUNDED =
                               FUNCTION NUMVAL(WS-INPUT)
                           SET VALUE-PROVIDED TO TRUE
                           SET VALID-INPUT TO TRUE
                       ELSE
                           DISPLAY "ENTER 0.00 TO 9999999.99, OR ENTER "
                                   "TO KEEP: " WITH NO ADVANCING
                       END-IF
                   ELSE
                       DISPLAY "ENTER 0.00 TO 9999999.99, OR ENTER "
                               "TO KEEP: " WITH NO ADVANCING
                   END-IF
               END-IF
           END-PERFORM.

       ACCEPT-POSITIVE-NUMBER.
           MOVE SPACES TO WS-INPUT
           PERFORM UNTIL FUNCTION TEST-NUMVAL(WS-INPUT) = ZERO
               ACCEPT WS-INPUT
               IF FUNCTION TEST-NUMVAL(WS-INPUT) NOT = ZERO
                   DISPLAY "ENTER A WHOLE NUMBER FROM 1 TO 999999: "
                           WITH NO ADVANCING
               ELSE
                   IF FUNCTION NUMVAL(WS-INPUT) < 1 OR
                      FUNCTION NUMVAL(WS-INPUT) > 999999 OR
                      FUNCTION NUMVAL(WS-INPUT) NOT =
                          FUNCTION INTEGER(FUNCTION NUMVAL(WS-INPUT))
                       DISPLAY "ENTER A WHOLE NUMBER FROM 1 TO 999999: "
                               WITH NO ADVANCING
                       MOVE SPACES TO WS-INPUT
                   END-IF
               END-IF
           END-PERFORM
           COMPUTE WS-AMOUNT = FUNCTION NUMVAL(WS-INPUT).

       ACCEPT-PRICE.
           MOVE SPACES TO WS-INPUT
           DISPLAY "UNIT PRICE (0.00 TO 9999999.99): "
                   WITH NO ADVANCING
           PERFORM UNTIL FUNCTION TEST-NUMVAL(WS-INPUT) = ZERO
               ACCEPT WS-INPUT
               IF FUNCTION TEST-NUMVAL(WS-INPUT) NOT = ZERO
                   DISPLAY "ENTER A VALID PRICE (0.00 TO 9999999.99): "
                           WITH NO ADVANCING
               ELSE
                   IF FUNCTION NUMVAL(WS-INPUT) < ZERO OR
                      FUNCTION NUMVAL(WS-INPUT) > 9999999.99
                       DISPLAY "ENTER A VALID PRICE (0.00 TO 9999999.99): "
                               WITH NO ADVANCING
                       MOVE SPACES TO WS-INPUT
                   END-IF
               END-IF
           END-PERFORM
           COMPUTE PRODUCT-UNIT-PRICE ROUNDED =
               FUNCTION NUMVAL(WS-INPUT).
