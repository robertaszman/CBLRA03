       identification division.
       program-id.     CBLRA03.
       AUTHOR.         Robert Aszman.
       DATE-WRITTEN.   1/15/2018.
       environment division.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BOAT-SALES
               ASSIGN TO 'C:\COBOL\CBLBOAT1.DAT'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT PRTOUT
               ASSIGN TO 'C:\COBOL\BOATRPT2.PRT'
               ORGANIZATION IS RECORD SEQUENTIAL.
       configuration section.
       data division.
       FILE SECTION.
       FD  BOAT-SALES
           LABEL RECORD IS STANDARD
           DATA RECORD IS I-REC
           RECORD CONTAINS 43 CHARACTERS.
           01	I-REC.
               05  I-LAST-NAME			    PIC X(16).
               05  I-STATE				    PIC X(2).
               05  I-BOAT-COST			    PIC 9(6)V99.
               05  I-PURCHASE-DATE          PIC X(8).	        
               05  I-BOAT-TYPE			    PIC X.
               05  I-ACCESSORY-PACKAGE		PIC 9.
		       05  I-PREP-DELIVERY-COST		PIC 9(5)V99.
       FD  PRTOUT
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 132 CHARACTERS
           DATA RECORD IS PRTLINE
           LINAGE IS 60 WITH FOOTING AT 56.
           01  PRTLINE                     PIC X(132).
       working-storage section.
       01  MISC.
           05  EOF                         PIC X       VALUE 'F'.
           05  CURRENT-DATE-AND-TIME.
               10  CURRENT-YEAR            PIC X(4).
               10  CURRENT-MONTH           PIC XX.
               10  CURRENT-DAY             PIC XX.
               10  CURRENT-TIME            PIC X(11).
           05  C-PCTR                      PIC Z9      VALUE 0.
           05  C-SALECTR                   PIC Z,ZZ9   VALUE 0.
           05  C-ACC-LIT                   PIC X(13).
           05  C-GT-TOT-COST               PIC 999,999,999,999.99
                                           VALUE 0.
           05  C-SUB-NUM-SOLD              PIC 9(4)    VALUE 0.
           05  H-BOAT-TYPE                 PIC X.
           05  C-TOTAL-COST                PIC 9(7)V99.
           05  C-SUB-TOT-COST              PIC 9(9)V99 VALUE 0.
           05  C-GT-NUM-SOLD               PIC 9(7)    VALUE ZEROS.
           05  C-GT-TOT-SALES              PIC 9(12)V99    VALUE 0.
           05  C-MARK-PERC                 PIC 9V999    VALUE 0.
           05  C-MARK-COST                 PIC 9(7)V99 VALUE 0.
           05  C-ACC-COST                  PIC 9(4)V99 VALUE 0.
           05  MAJ-NUM-SOLD                PIC 9(5)    VALUE 0.
           05  MAJ-BOAT-TYPE-LIT           PIC X(13).
           05  H-STATE                     PIC X(2).
       01  HEADING1.
           05  H1-DATE.
               10  H1-MONTH                PIC 99.
               10  FILLER                  PIC X       VALUE '/'.
               10  H1-DAY                  PIC 99.
               10  FILLER                  PIC X       VALUE '/'.
               10  H1-YEAR                 PIC 9999.
           05  FILLER                      PIC X(40)   VALUE SPACES.
           05  FILLER                      PIC X(19)
                                           VALUE 'WILSON S BOATS INC.'.
           05  FILLER                      PIC X(49)   VALUE SPACES.
           05  FILLER                      PIC X(6)    VALUE 'PAGE: '.
           05  H1-PAGE                     PIC 99      VALUE 0.
           05  FILLER                      PIC X(132)  VALUE SPACES.
       01  COL-HEADING1.
           05  FILLER                      PIC X(8)    VALUE 'CUSTOMER'.
           05  FILLER                      PIC X(38)   VALUE SPACES.
           05  FILLER                      PIC X(4)    VALUE 'BOAT'.
           05  FILLER                      PIC X(7)    VALUE SPACES.
           05  FILLER                      PIC X(8)    VALUE 'PURCHASE'.
           05  FILLER                      PIC X(11)   VALUE SPACES.
           05  FILLER                      PIC X(9)
                                           VALUE 'ACCESSORY'.
           05  FILLER                      PIC X(20)   VALUE SPACES.
           05  FILLER                      PIC X(4)    VALUE 'PREP'.
           05  FILLER                      PIC X(18)   VALUE SPACES.
           05  FILLER                      PIC X(5)    VALUE 'TOTAL'.
           05  FILLER                      PIC X(132)  VALUE SPACES.
       01  COL-HEADING2.
           05  FILLER                      PIC X(9)  VALUE 'LAST NAME'.
           05  FILLER                      PIC X(14)   VALUE SPACES.
           05  FILLER                      PIC X(5)    VALUE 'STATE'.
           05  FILLER                      PIC X(18)   VALUE SPACES.
           05  FILLER                      PIC X(4)    VALUE 'COST'.
           05  FILLER                      PIC X(7)    VALUE SPACES.
           05  FILLER                      PIC X(4)    VALUE 'DATE'.
           05  FILLER                      PIC X(15)   VALUE SPACES.
           05  FILLER                      PIC X(7)    VALUE 'PACKAGE'.
           05  FILLER                      PIC X(22)   VALUE SPACES.
           05  FILLER                      PIC X(4)    VALUE 'COST'.
           05  FILLER                      PIC X(19)   VALUE SPACES.
           05  FILLER                      PIC X(4)    VALUE 'COST'.
           05  FILLER                      PIC X(132)  VALUE SPACES.
           05  FILLER                      PIC X(11)
                                           VALUE 'BOAT TYPE: '.
           05  C-BOAT-TYPE-LIT             PIC X(13).
           05  FILLER                      PIC X(108)  VALUE SPACES.
           05  FILLER                      PIC X(132)  VALUE SPACES.
       01  DETAIL-LINE.
           05  O-LAST-NAME                 PIC X(16).
           05  FILLER                      PIC X(8)    VALUE SPACES.
           05  O-STATE                     PIC X(2).
           05  FILLER                      PIC X(14)   VALUE SPACES.
           05  O-BOAT-COST                 PIC $$$,$$$.99.
           05  FILLER                      PIC X(7)    VALUE SPACES.
           05  O-PURCHASE-DATE.            
                  10 PUR-YEAR              PIC 99.
                  10 FILLER                PIC X       VALUE '/'.
                  10 PUR-MONTH             PIC 99.     
                  10 FILLER                PIC X       VALUE '/'.
                  10 PUR-DAY               PIC 99.
           05  FILLER                      PIC X(11)   VALUE SPACES.
           05  O-ACC-LIT                   PIC X(15).
           05  FILLER                      PIC X(9)    VALUE SPACES.
           05  O-PREP-DELIVERY-COST        PIC $$,$$$.99.
           05  FILLER                      PIC X(11)   VALUE SPACES.
           05  O-TOTAL-COST                PIC $,$$$,$$$.99.
       01  SUBTOTAL-LINE.
           05  FILLER                      PIC X(10)   VALUE SPACES.
           05  FILLER                      PIC X(14)
                                           VALUE 'SUBTOTALS FOR '.
           05  MIN-STATE                   PIC X(2).
           05  FILLER                      PIC X(11).
           05  O-BOAT-TYPE-LIT             PIC X(13).
           05  FILLER                      PIC X(10)   VALUE SPACES.
           05  FILLER                      PIC X(15)
                                           VALUE 'NUMBER SOLD:   '.
           05  O-SUB-NUM-SOLD              PIC 9(4).
           05  FILLER                      PIC X(39)   VALUE SPACES.
           05  O-SUB-TOT-COST              PIC $$$,$$$,$$$.99.
       01  MAJORS-LINE.
           05  FILLER                      PIC X(10)   VALUE SPACES.
           05  FILLER                      PIC X(13)
                                           VALUE 'SUBTOTALS FOR'.
           05  FILLER                      PIC X(14)   VALUE SPACES.
           05  O-MAJ-BOAT-TYPE-LIT         PIC X(13).
           05  FILLER                      PIC X(10)   VALUE SPACES.
           05  FILLER                      PIC X(14)
                                           VALUE 'NUMBER SOLD:  '.
           05  O-MAJ-NUM-SOLD              PIC ZZ,ZZ9.
       01  GT-LINE.
           05  FILLER                      PIC X(23)   VALUE SPACES.
           05  FILLER                      PIC X(12)
                                           VALUE 'GRAND TOTALS'.
           05  FILLER                      PIC X(25)   VALUE SPACES.
           05  FILLER                      PIC X(13)
                                           VALUE 'NUMBER SOLD: '.
           05  O-GT-NUM-SOLD               PIC Z(5).
           05  FILLER                      PIC X(35)   VALUE SPACES.
           05  O-GT-TOT-SALES              PIC $$$,$$$,$$$,$$$.99.
           05  FILLER                      PIC X(132)  VALUE SPACES.
       procedure division.
       L1-MAIN.
           PERFORM L2-INIT.
           PERFORM L2-MAINLINE
               UNTIL EOF = 'T'.
           PERFORM L3-CLOSING.
            STOP RUN.
       L2-INIT.
           MOVE FUNCTION current-date      TO CURRENT-DATE-AND-TIME.
           MOVE CURRENT-DAY                TO H1-DAY.
           MOVE CURRENT-MONTH              TO H1-MONTH.
           MOVE CURRENT-YEAR               TO H1-YEAR.
           OPEN INPUT BOAT-SALES.
           OPEN OUTPUT PRTOUT.
           PERFORM L4-HEADINGS.
           PERFORM L3-READ.
           MOVE I-BOAT-TYPE TO H-BOAT-TYPE.
           MOVE I-STATE TO MIN-STATE.
           MOVE I-STATE TO H-STATE.
       L2-MAINLINE.
           IF I-BOAT-TYPE NOT = H-BOAT-TYPE
               PERFORM L2-MAJORS
           END-IF.
           IF I-STATE NOT = H-STATE
               PERFORM L2-MINORS
           END-IF.
           PERFORM L3-CALCS.
           PERFORM L3-MOVES.
           PERFORM L3-READ.
       L3-CALCS.
           EVALUATE I-BOAT-TYPE
               WHEN 'B'
                   MOVE 'BASS BOAT' TO C-BOAT-TYPE-LIT
                   MOVE 0.33 TO C-MARK-PERC
               WHEN 'P'
                   MOVE 'PONTOON' TO C-BOAT-TYPE-LIT
                   MOVE 0.25 TO C-MARK-PERC
               WHEN 'S'
                   MOVE 'SKI BOAT' TO C-BOAT-TYPE-LIT
                   MOVE 0.425 TO C-MARK-PERC
               WHEN 'J'
                   MOVE 'JOHN BOAT' TO C-BOAT-TYPE-LIT
                   MOVE 0.33 TO C-MARK-PERC
               WHEN 'C'
                   MOVE 'CANOE' TO C-BOAT-TYPE-LIT
                   MOVE 0.2 TO C-MARK-PERC
               WHEN 'R'
                   MOVE 'CABIN CRUISER' TO C-BOAT-TYPE-LIT
                   MOVE 0.3 TO C-MARK-PERC
           END-EVALUATE.
           EVALUATE I-ACCESSORY-PACKAGE
               WHEN 1
                   MOVE 'ELECTRONICS' TO C-ACC-LIT
                   MOVE 5415.30 TO C-ACC-COST
               WHEN 2
                   MOVE 'SKI PACKAGE' TO C-ACC-LIT
                   MOVE 3980.00 TO C-ACC-COST
               WHEN 3
                   MOVE 'FISHING    ' TO C-ACC-LIT
                   MOVE  345.45 TO C-ACC-COST
           END-EVALUATE.
           COMPUTE C-SUB-NUM-SOLD = C-SUB-NUM-SOLD + 1.
           COMPUTE C-MARK-COST ROUNDED = I-BOAT-COST * C-MARK-PERC.
           COMPUTE C-TOTAL-COST ROUNDED = (C-MARK-COST + C-ACC-COST +
                   I-BOAT-COST + I-PREP-DELIVERY-COST) * 1.06.
           COMPUTE C-SUB-TOT-COST = C-SUB-TOT-COST + C-TOTAL-COST.
       L3-MOVES.
           MOVE I-LAST-NAME TO O-LAST-NAME.
           MOVE I-STATE TO O-STATE.
           MOVE I-BOAT-COST TO O-BOAT-COST.
           MOVE I-PURCHASE-DATE TO O-PURCHASE-DATE.
           MOVE C-ACC-LIT TO O-ACC-LIT.
           MOVE I-PREP-DELIVERY-COST TO O-PREP-DELIVERY-COST.
           MOVE C-TOTAL-COST TO O-TOTAL-COST.
           MOVE C-BOAT-TYPE-LIT TO O-BOAT-TYPE-LIT.
           WRITE PRTLINE FROM DETAIL-LINE.
       L3-CLOSING.
           MOVE C-GT-TOT-SALES TO O-GT-TOT-SALES.
           MOVE C-GT-NUM-SOLD TO O-GT-NUM-SOLD.
           WRITE PRTLINE FROM GT-LINE.
           CLOSE BOAT-SALES.
           CLOSE PRTOUT.
       L3-READ.
           READ BOAT-SALES
               AT END
                   MOVE 'T' TO EOF. 
       L2-MINORS.
           MOVE C-SUB-TOT-COST TO O-SUB-TOT-COST.
           MOVE C-SUB-NUM-SOLD TO O-SUB-NUM-SOLD.
           COMPUTE C-GT-NUM-SOLD = C-GT-NUM-SOLD + C-SUB-NUM-SOLD.
           COMPUTE C-SUB-NUM-SOLD = 0.
           COMPUTE C-GT-TOT-SALES = C-GT-TOT-SALES + C-SUB-TOT-COST.
           COMPUTE C-SUB-TOT-COST = 0.
           WRITE PRTLINE FROM SUBTOTAL-LINE.
           MOVE I-BOAT-TYPE TO H-BOAT-TYPE.
           MOVE I-STATE TO H-STATE.
       L4-HEADINGS.
           COMPUTE H1-PAGE = H1-PAGE + 1.
           WRITE PRTLINE FROM HEADING1.
           WRITE PRTLINE FROM COL-HEADING1.
           WRITE PRTLINE FROM COL-HEADING2.
       L2-MAJORS.
           MOVE C-SUB-NUM-SOLD TO MAJ-NUM-SOLD.
           MOVE O-BOAT-TYPE-LIT TO MAJ-BOAT-TYPE-LIT.
           MOVE MAJ-NUM-SOLD TO O-MAJ-NUM-SOLD.
           MOVE MAJ-BOAT-TYPE-LIT TO O-MAJ-BOAT-TYPE-LIT.
           PERFORM L2-MINORS.
           WRITE PRTLINE FROM MAJORS-LINE.
           COMPUTE MAJ-NUM-SOLD = 0.