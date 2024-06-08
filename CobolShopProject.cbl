      ******************************************************************
      * Author: Nicolas Poulin
      * Date: 5/26/2024 to ???
      * Purpose: Make a wishlist from a list of product and print the
      *          total price when the client want to buy.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ChallengeCobol.

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.

           SELECT FILE-PRODUCT
           ASSIGN TO "C:\Users\mikoa\Desktop\CobolProjects\ProjectA\prod
      -    ".dat"
           ORGANISATION IS LINE SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS IS EMP-PRODUCT-FILESTATUS.

           SELECT FILE-WISHLIST
           ASSIGN TO "C:\Users\mikoa\Desktop\CobolProjects\ProjectA\wish
      -    ".dat"
           ORGANISATION IS LINE SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS IS EMP-WISHLIST-FILESTATUS.

           SELECT FILE-WISHLIST-READ
           ASSIGN TO "C:\Users\mikoa\Desktop\CobolProjects\ProjectA\wish
      -    ".dat"
           ORGANISATION IS LINE SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS IS EMP-WISHLIST-READ-FILESTATUS.

           SELECT FILE-INVOICE
           ASSIGN TO "C:\Users\mikoa\Desktop\CobolProjects\ProjectA\invo
      -    ".dat"
           ORGANISATION IS LINE SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS IS EMP-WISHLIST-READ-FILESTATUS.

           SELECT FILE-WISHLIST-TODELETE
           ASSIGN TO "C:\Users\mikoa\Desktop\CobolProjects\ProjectA\wish
      -    ".dat"
           ORGANISATION IS SEQUENTIAL
           FILE STATUS IS EMP-WISHLIST-READ-FILESTATUS.

       DATA DIVISION.
       FILE SECTION.

       FD  FILE-PRODUCT.
       01  FILE-PRODUCT-OBJ.
           05 ID-PRODUCT                       PIC X(9).
           05 FILLER                           PIC X(1) VALUE SPACES.
           05 NAME-PRODUCT                     PIC X(50).
           05 FILLER                           PIC X(1) VALUE SPACES.
           05 PRICE-PRODUCT                    PIC $ZZZZ.ZZ.

       01  FILE-PRODUCT-TABLE OCCURS 10.
           05 ID-PRODUCT                       PIC X(9).
           05 FILLER                           PIC X(1) VALUE SPACES.
           05 NAME-PRODUCT                     PIC X(50).
           05 FILLER                           PIC X(1) VALUE SPACES.
           05 PRICE-PRODUCT                    PIC $ZZZZ.ZZ.

       FD  FILE-WISHLIST-READ.
       01  SELECTED-OBJECT-READ                PIC X(9).

       FD  FILE-WISHLIST
           RECORD CONTAINS 9  CHARACTERS
           BLOCK  CONTAINS 90 CHARACTERS
           DATA RECORD IS SELECTED-OBJECT.
       01  FILE-WISHLIST-RECORD.
           05 ID-PRODUCT                       PIC X(9).
           05 FILLER                           PIC X(1) VALUE SPACES.
       01  SELECTED-OBJECT                     PIC X(9).

       FD  FILE-INVOICE.
       01  SELECTED-TEXT                       PIC X(70).

       FD  FILE-WISHLIST-TODELETE.
       01  SELECTED-DEFAULTTEXT               PIC X(9) VALUE "WISHLIST".

       WORKING-STORAGE SECTION.
       01  EMP-PRODUCT-FILESTATUS              PIC X(02).
       01  EMP-READ-PRODUCT-FILE-STATUS        PIC X(1).
           88 EMP-END-OF-FILE                              VALUE 'Y'.
           88 NOT-END-OF-FILE                              VALUE 'N'.
       01  EMP-INDEX                           PIC 99(1)   VALUE ZERO.

       01  EMP-WISHLIST-READ-FILESTATUS        PIC X(02).
       01  EMP-WISHLIST-READ-FILE-STATUS       PIC X(1).
           88 EMP-W-END-OF-FILE                            VALUE 'Y'.
           88 NOT-W-END-OF-FILE                            VALUE 'N'.
       01  EMP-WISHLIST-INDEX                  PIC 99(1)   VALUE ZERO.
       01  INDEX-DISPLAY                       PIC 99(1)   VALUE ZERO.
       01  COMPARE-ELEMENT                     PIC X(9).

       01  EMP-WISHLIST-FILESTATUS             PIC X(02).
       01  ID-PRODUCT-CHOICE                   PIC X(9).
       01  INDEX-WISHLIST                      PIC 99(1)   VALUE ZERO.
       01  INPUT-USER                          PIC X(9).

       01  PRICE                               PIC 9(4)V9(2).
       01  TOTAL-PRICE                         PIC 9(5)V9(2) VALUE ZERO.
       01  TOTAL-PRICE-PRINT                   PIC $$$$$9.99.
       01  WS-EOF-SWITCH                       PIC X       VALUE 'N'.

       PROCEDURE DIVISION.
           DISPLAY "Welcome!"

           DISPLAY "Do you wish to reset the wishlist? (Y/N) : "
           ACCEPT INPUT-USER

           IF INPUT-USER = 'Y'
               OPEN OUTPUT FILE-WISHLIST-TODELETE
               MOVE "WISHLIST" TO SELECTED-DEFAULTTEXT
               WRITE SELECTED-DEFAULTTEXT BEFORE ADVANCING 1 LINE
               CLOSE FILE-WISHLIST-TODELETE
           END-IF

           OPEN EXTEND FILE-WISHLIST
           OPEN OUTPUT FILE-INVOICE

           OPEN INPUT FILE-PRODUCT
           EVALUATE TRUE
           WHEN EMP-PRODUCT-FILESTATUS = "35"
               DISPLAY " FILE NOT FOUND "
               DISPLAY " FILE STATUS IS : " EMP-PRODUCT-FILESTATUS

               GO TO CLOSE-PROGRAM
           WHEN EMP-PRODUCT-FILESTATUS = "00"
               DISPLAY "List of available products : "
               DISPLAY "            ID | NAME OF THE PRODUCT | PRICE"

               PERFORM GET-PRODUCTS THRU READ-EXIT UNTIL EMP-END-OF-FILE
               CLOSE FILE-PRODUCT

               IF EMP-INDEX = 0 THEN
                   DISPLAY "ALERT : The file is empty"
               ELSE
                   DISPLAY "Number of products available : " EMP-INDEX
                   DISPLAY SPACE
               END-IF

           WHEN OTHER
               DISPLAY "ERROR : " EMP-PRODUCT-FILESTATUS
           END-EVALUATE

           OPEN INPUT FILE-WISHLIST-READ
           EVALUATE TRUE
           WHEN EMP-PRODUCT-FILESTATUS = "35"
               DISPLAY " ERROR : FILE NOT FOUND "
               DISPLAY " ERROR : FILE STATUS IS : "
               EMP-WISHLIST-READ-FILESTATUS

               GO TO CLOSE-PROGRAM
           WHEN EMP-PRODUCT-FILESTATUS = "00"
               DISPLAY "Your current wishlist : "

               MOVE 0 TO EMP-WISHLIST-INDEX
               MOVE 0 TO TOTAL-PRICE

               PERFORM GET-WISHLIST THRU READ-EXIT
               UNTIL EMP-W-END-OF-FILE
               CLOSE FILE-WISHLIST-READ
               SUBTRACT 2 FROM EMP-WISHLIST-INDEX
               
               IF EMP-INDEX = 0 THEN
                   DISPLAY "ALERT : The file is empty"
               ELSE
                   
                   DISPLAY "Number of wishlist element : "
                   EMP-WISHLIST-INDEX
                   DISPLAY SPACE

                   PERFORM MAKE-WISHLIST
               END-IF
           WHEN OTHER
               DISPLAY "ERROR : " EMP-WISHLIST-READ-FILESTATUS
           END-EVALUATE.

       GET-PRODUCTS.
           READ FILE-PRODUCT INTO FILE-PRODUCT-OBJ
               AT END
                   SET EMP-END-OF-FILE TO TRUE
               NOT AT END
                   ADD 1 TO EMP-INDEX
                   ON SIZE ERROR
                       DISPLAY " SIZE ERROR AT POS "EMP-INDEX
                       CLOSE FILE-PRODUCT
                   END-ADD

                   MOVE FILE-PRODUCT-OBJ
                   TO FILE-PRODUCT-TABLE(EMP-INDEX)

                   DISPLAY "Table["EMP-INDEX"] : "
                   FILE-PRODUCT-TABLE(EMP-INDEX)
           END-READ.

       GET-WISHLIST.
           READ FILE-WISHLIST-READ INTO SELECTED-OBJECT-READ
               AT END
                   SET EMP-W-END-OF-FILE TO TRUE
               NOT AT END
                   ADD 1 TO EMP-WISHLIST-INDEX
                   ON SIZE ERROR
                       DISPLAY " SIZE ERROR AT POS "EMP-WISHLIST-INDEX
                       CLOSE FILE-WISHLIST-READ
                   END-ADD
               
               PERFORM VARYING INDEX-DISPLAY FROM 1 BY 1
               UNTIL INDEX-DISPLAY > 10
               MOVE FILE-PRODUCT-TABLE(INDEX-DISPLAY) TO COMPARE-ELEMENT
                   IF COMPARE-ELEMENT = SELECTED-OBJECT-READ
                     
                       DISPLAY FILE-PRODUCT-TABLE(INDEX-DISPLAY)

                       MOVE FILE-PRODUCT-TABLE(INDEX-DISPLAY)(62:7)
                       TO PRICE
                       ADD PRICE TO TOTAL-PRICE
                   END-IF
               END-PERFORM
               MOVE 0 TO INDEX-DISPLAY
           END-READ.

       READ-EXIT.
           EXIT.

       CLOSE-PROGRAM.
           STOP RUN.

       MAKE-WISHLIST.
           DISPLAY "Wishlist"
           DISPLAY "INFO: Write 'C' to cancel and save wishlist"
           DISPLAY "INFO: Write 'B' to buy everything from the wishlist"
           PERFORM READ-ID-INPUT.

       READ-ID-INPUT.
           MOVE 1 TO INDEX-WISHLIST
           DISPLAY "ENTER ID PRODUCT : "
           ACCEPT INPUT-USER

           IF INPUT-USER = "C" OR INPUT-USER = "B"
               IF INPUT-USER = "C" OR INPUT-USER = "c"
                   CLOSE FILE-WISHLIST

                   DISPLAY "Saving wishlist..."
                   DISPLAY"Goodbye!"

                   CLOSE FILE-INVOICE
                   GO TO CLOSE-PROGRAM
               ELSE IF INPUT-USER = "B" OR INPUT-USER = "b"
                   CLOSE FILE-WISHLIST

                   DISPLAY SPACE
                   DISPLAY "Ty for your purchace! This is your total!"
                   DISPLAY SPACE

                   PERFORM CREATE-INVOICE
                   GO TO CLOSE-PROGRAM
               END-IF
           ELSE
               ADD 1 TO INDEX-WISHLIST
               MOVE INPUT-USER TO SELECTED-OBJECT
               MOVE 0 TO INDEX-DISPLAY

               PERFORM VARYING INDEX-DISPLAY FROM 1 BY 1
               UNTIL INDEX-DISPLAY > 10
                  IF INPUT-USER = FILE-PRODUCT-TABLE(INDEX-DISPLAY)(1:9)              "
                       MOVE FILE-PRODUCT-TABLE(INDEX-DISPLAY)(62:7)
                       TO PRICE
                       ADD PRICE TO TOTAL-PRICE
                   END-IF
               END-PERFORM

               DISPLAY "Result: "SELECTED-OBJECT
               WRITE SELECTED-OBJECT BEFORE ADVANCING 1 LINE
               GO TO READ-ID-INPUT
           END-IF.

       CREATE-INVOICE.
           MOVE "====================" TO SELECTED-TEXT
           DISPLAY SELECTED-TEXT
           WRITE SELECTED-TEXT BEFORE ADVANCING 1 LINE

           MOVE "|     INVOICE      |" TO SELECTED-TEXT
           DISPLAY SELECTED-TEXT
           WRITE SELECTED-TEXT BEFORE ADVANCING 1 LINE
           MOVE "====================" TO SELECTED-TEXT
           DISPLAY SELECTED-TEXT
           WRITE SELECTED-TEXT BEFORE ADVANCING 1 LINE

           MOVE "Total : " TO SELECTED-TEXT
           DISPLAY SELECTED-TEXT
           WRITE SELECTED-TEXT BEFORE ADVANCING 1 LINE

           DISPLAY "$"TOTAL-PRICE
           MOVE TOTAL-PRICE TO TOTAL-PRICE-PRINT
           MOVE TOTAL-PRICE-PRINT TO SELECTED-TEXT
           WRITE SELECTED-TEXT

           MOVE "====================" TO SELECTED-TEXT
           DISPLAY SELECTED-TEXT
           WRITE SELECTED-TEXT BEFORE ADVANCING 1 LINE

           CLOSE FILE-INVOICE

           MOVE "WISHLIST" TO SELECTED-DEFAULTTEXT
           WRITE SELECTED-DEFAULTTEXT BEFORE ADVANCING 1 LINE
           CLOSE FILE-WISHLIST-TODELETE.

           END PROGRAM ChallengeCobol.
