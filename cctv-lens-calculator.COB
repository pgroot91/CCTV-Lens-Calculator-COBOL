
      ******************************************************************
      * DATE-WRITTEN. 01-01-2021.
      * AUTHOR. Patrick Groot.
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. lenscalculator.

       ENVIRONMENT DIVISION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       01 CALC PIC 9(1).
        88 CALC-VALID VALUES ARE 1 THRU 2.
       01 CALC2 PIC 9(1).
        88 CALC2-VALID VALUES ARE 1 THRU 5.
       01 DISTANCE PIC 9(3).
        88 DISTANCE-VALID VALUES ARE 1 THRU 999.
        88 DISTANCE-INVALID VALUES ARE 'A' THRU 'Z'.
       01 WIDTH PIC 9(2).
       01 HEIGHT PIC 9(2).
       01 RESULT PIC 9(3).
       01 GETVAL PIC 9(2)V999.
       01 WS-SENSORBREEDTES.
        05 WS-BREEDTE-1 PIC 9(2)V9 VALUE 12.8.
        05 WS-BREEDTE-2 PIC 9(1)V9 VALUE 8.8.
        05 WS-BREEDTE-3 PIC 9(1)V9 VALUE 6.4.
        05 WS-BREEDTE-4 PIC 9(1)V9 VALUE 4.4.
        05 WS-BREEDTE-5 PIC 9(1)V9 VALUE 3.2.
       01 WS-SENSORHOOGTES.
        05 WS-HOOGTE-1 PIC 9(1)V999 VALUE 9.525.
        05 WS-HOOGTE-2 PIC 9(1)V9 VALUE 6.6.
        05 WS-HOOGTE-3 PIC 9(1)V9 VALUE 4.8.
        05 WS-HOOGTE-4 PIC 9(1)V9 VALUE 3.6.
        05 WS-HOOGTE-5 PIC 9(1)V9 VALUE 2.4.
       01 S PIC A(4).

       SCREEN SECTION.

       PROCEDURE DIVISION.

       START-CALCULATOR.

       DISPLAY '******************************************************'.
       DISPLAY '**************** CCTV LENS CALCULATOR ****************'.
       DISPLAY '******************************************************'.
       DISPLAY '*** The Lens Calculator can be applied in two ways ***'.
       DISPLAY '******************************************************'.
       DISPLAY ' '.
       DISPLAY '1) Calculation to object width.'.
       DISPLAY '2) Calculation to object height.'.
       DISPLAY ' '.

       SELECT-CALCULATOR.

       DISPLAY 'Enter number: ' WITH NO ADVANCING.
       ACCEPT CALC.
       DISPLAY ' '.

       IF CALC-VALID
           GO STEP1
       ELSE
           DISPLAY 'ERROR: Please enter a valid number.'
           DISPLAY ' '
           GO SELECT-CALCULATOR
           STOP RUN
       END-IF.

       STEP1.

       DISPLAY '******************************************************'.
       DISPLAY '*** Step 1: Choose the format CCD or CMOS ************'.
       DISPLAY '******************************************************'.
       DISPLAY ' '.
       DISPLAY '1) 1 Inch'.
       DISPLAY '2) 2/3 Inch'.
       DISPLAY '3) 1/2 Inch'.
       DISPLAY '4) 1/3 Inch'.
       DISPLAY '5) 1/4 Inch'.
       DISPLAY ' '.

       VALUE-STEP1.

       DISPLAY 'Enter number: ' WITH NO ADVANCING.
       ACCEPT CALC2.
       DISPLAY ' '.

       IF CALC2-VALID
           GO STEP2
       ELSE
           DISPLAY 'ERROR: Please enter a valid number.'
           DISPLAY ' '
           GO VALUE-STEP1
           STOP RUN
       END-IF.

       STEP2.

       DISPLAY '******************************************************'.
       DISPLAY '*** Step 2: What is the distance to the object? ******'.
       DISPLAY '******************************************************'.
       DISPLAY '*** IMPORTANT: Use whole meters only *****************'.
       DISPLAY '******************************************************'.
       DISPLAY ' '.

       VALUE-STEP2.

       DISPLAY 'Enter distance: ' WITH NO ADVANCING.
       ACCEPT DISTANCE.
       DISPLAY ' '.

       IF DISTANCE-VALID
           GO STEP3
       ELSE
           DISPLAY 'ERROR: Please enter a valid number.'
           DISPLAY ' '
           GO VALUE-STEP2
           STOP RUN
       END-IF.

       STEP3.

       IF CALC = 1 THEN

        DISPLAY '******************************************************'.
        DISPLAY '*** Step 3: What is the width of the object? *********'.
        DISPLAY '******************************************************'.
        DISPLAY '*** IMPORTANT: Use whole meters only *****************'.
        DISPLAY '******************************************************'.

        DISPLAY ' '
        DISPLAY 'Enter width of object: ' WITH NO ADVANCING
        ACCEPT WIDTH
        DISPLAY ' '

       ELSE

        DISPLAY '******************************************************'.
        DISPLAY '*** Step 3: What is the height of the object? ********'.
        DISPLAY '******************************************************'.
        DISPLAY '*** IMPORTANT: Use whole meters only *****************'.
        DISPLAY '******************************************************'.

        DISPLAY ' '
        DISPLAY 'Enter height of object: ' WITH NO ADVANCING
        ACCEPT HEIGHT
        DISPLAY ' '

       END-IF.

       IF CALC IS EQUAL TO 1 THEN
        IF CALC2 IS EQUAL TO 1
         MOVE WS-BREEDTE-1 TO GETVAL
        END-IF
        IF CALC2 IS EQUAL TO 2
         MOVE WS-BREEDTE-2 TO GETVAL
        END-IF
        IF CALC2 IS EQUAL TO 3
         MOVE WS-BREEDTE-3 TO GETVAL
        END-IF
        IF CALC2 IS EQUAL TO 4
         MOVE WS-BREEDTE-4 TO GETVAL
        END-IF
        IF CALC2 IS EQUAL TO 5
         MOVE WS-BREEDTE-5 TO GETVAL
        END-IF
        DISPLAY '******************************************************'
        DISPLAY '*** RESULT *******************************************'
        DISPLAY '******************************************************'
        DISPLAY ' '
        COMPUTE RESULT = DISTANCE * GETVAL / WIDTH
        DISPLAY 'Use a ' RESULT 'mm lens.'
       END-IF.

       IF CALC IS EQUAL TO 2 THEN
        IF CALC2 IS EQUAL TO 1
         MOVE WS-HOOGTE-1 TO GETVAL
        END-IF
        IF CALC2 IS EQUAL TO 2
         MOVE WS-HOOGTE-2 TO GETVAL
        END-IF
        IF CALC2 IS EQUAL TO 3
         MOVE WS-HOOGTE-3 TO GETVAL
        END-IF
        IF CALC2 IS EQUAL TO 4
         MOVE WS-HOOGTE-4 TO GETVAL
        END-IF
        IF CALC2 IS EQUAL TO 5
         MOVE WS-HOOGTE-5 TO GETVAL
        END-IF
        DISPLAY '******************************************************'
        DISPLAY '*** RESULT *******************************************'
        DISPLAY '******************************************************'
        DISPLAY ' '
        COMPUTE RESULT = DISTANCE * GETVAL / HEIGHT
        DISPLAY 'Use a ' RESULT 'mm lens.'
       END-IF.

       DISPLAY ' '.
       DISPLAY 'DISCLAIMER: Reasonable efforts are made to ensure that'
       DISPLAY 'the information provided by this CCTV Lens Calculator'
       DISPLAY 'is accurate, however, no guarantees for accuracy of'
       DISPLAY 'information are made. Information is provided "as is",'
       DISPLAY 'without warranty of any kind, whether expressed or'
       DISPLAY 'implied.'.
       DISPLAY ' '.

       DISPLAY 'Type "exit" to close the program.'.
       ACCEPT S.

       DISPLAY ' '.
       DISPLAY ' '.
       DISPLAY ' '.
       DISPLAY ' '.
       DISPLAY ' '.

       IF S NOT = "exit" GO TO START-CALCULATOR.

       STOP RUN.

       END PROGRAM lenscalculator.
