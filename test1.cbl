           IDENTIFICATION DIVISION.
            PROGRAM-ID. STRINGHANDLE.
            DATA DIVISION.
              WORKING-STORAGE SECTION.
              01 GROUP.
              	02 STR PIC X
              	03 STR2 PIC X
              01 WS-CNT1 PIC 9(2) VALUE 0.
              01 WS-CNT2 PIC 9(2) VALUE 0.
              01 WS-STRING PIC X(25) VALUE 'ABCDADADADABVDFDFFAF'.
              01 WS-STRING2 PIC X(25) VALUE 'ABCDADADADABVDFDFFAF'.
              01 WS-STRING3 PIC X(25) VALUE 'ABCDADADADABVDFDFFAF'.
              01 WS-STRING-DEST PIC A(30).
              01 WS-STR1 PIC A(15) VALUE 'TUTORIALSPOINT'.
              01 WS-STR2 PIC A(7) VALUE 'WELCOME'.
              01 WS-STR3 PIC A(7) VALUE 'TO AND'.
              01 WS-COUNT PIC 99 VALUE 1.
              01 WS-UNSTR PIC A(30) VALUE 'WELCOME TO TUTORIALSPOINT'.
            PROCEDURE DIVISION.
              *> count the number of chars in string, store in ws-cnt1
              INSPECT WS-STRING TALLYING WS-CNT1 FOR ALL CHARACTERS.
              DISPLAY "WS-CNT1 : "WS-CNT1.
              *> count just the A characters
              INSPECT WS-STRING TALLYING WS-CNT2 FOR ALL 'A'.
              DISPLAY "WS-CNT2 : "WS-CNT2.
              *> replace A chars with X in strings
              DISPLAY "OLD STRING : "WS-STRING.
              INSPECT WS-STRING REPLACING ALL 'A' BY 'X'.
              DISPLAY "NEW STRING : "WS-STRING.
              *> string concatenate
              STRING WS-STR2 DELIMITED BY SIZE
                WS-STR3 DELIMITED BY SPACE
                WS-STR1 DELIMITED BY SIZE
                INTO WS-STRING-DEST
                WITH POINTER WS-COUNT
                ON OVERFLOW DISPLAY 'OVERFLOW!'
              END-STRING.
              DISPLAY 'WS-STRING : 'WS-STRING-DEST.
              DISPLAY 'WS-COUNT : 'WS-COUNT.
              *> string split
              UNSTRING WS-UNSTR DELIMITED BY SPACE
                INTO WS-STR3, WS-STR2, WS-STR1
              END-UNSTRING.
              DISPLAY 'WS-STR1 : 'WS-STR1.
              DISPLAY 'WS-STR2 : 'WS-STR2.
              DISPLAY 'WS-STR3 : 'WS-STR3.
            STOP RUN.
            

IDENTIFICATION DIVISION.

CALL 'gets' USING in-line RETURNING in-line.

000001 working-storage section.
000002 01 return     pic X(8).
000003 01 filler redefines return-code-ws.
000004 $if little-endian defined
000005     05 filler          pic x.
000006     05 high-order-byte pic s9 comp-5.
000007     05 filler          pic x.
000008     05 low-order-byte pic s9 comp-5.
000009 $else
000010     05 filler          pic xx.
000011     05 high-order-byte pic s9 comp-5.
000012     05 low-order-byte pic s9 comp-5.
000013 $end
000014 01 null-terminated-command.
000015     05 command      pic x(2048).
000016     05 filler          pic x value x"00".
000014 01 null-terminated-command2.
02 null-term3.
000015     05 command2      pic x(2048).
000016     05 filler2          pic x value x"00".
01 intovar pic x
000017 01 done-flag pic x value "n".
000018     88 done value "y".
accept command

   STRING WS-STR2 DELIMITED BY SIZE
      WS-STR3 DELIMITED BY SPACE
      command DELIMITED BY SIZE
      INTO intovar
      WITH POINTER WS-COUNT
      ON OVERFLOW DISPLAY 'OVERFLOW!' 
   END-STRING.
display intovar
test.
DISPLAY null-terminated-command

accept null-terminated-command
test.
DISPLAY command

000019 procedure division.
000020 perform get-user-input
000021 perform until done
000022      call "SYSTEM" using null-terminated-command
000022      call "SYSTEM" using null-term3
000022      call "SYSTEM" using return
000023          returning return-code-ws
000024      if low-order-byte = 0
000025          display "return code from shell is: ",
000026               high-order-byte
000027      end-if
000028      perform get-user-input
000029 end-perform.
000030 stop run.
000031
000032 get-user-input.
000033      display spaces
000034      display "Enter command to be executed by shell"
000035      display " (enter 'done' to quit)"
000036      accept command
000036      accept command2
move command2 to return.
display return
move return to filler
display filler
000022      call "SYSTEM" using null-terminated-command
000037      if command = "done" move "y" to done-flag.
000037      if command = "done" move "y" to done-flag.
000037      if command = "done" move "y" to done-flag.
000037      if command = "done" move "y" to done-flag.
000037      if command = "done" move "y" to done-flag.


000022      call "SYSTEM" using null-terminated-command
000037      if command = "done" move "y" to done-flag.
000037      if command = "done" move "y" to done-flag.
000037      if command = "done" move "y" to done-flag.
000037      if command = "done" move "y" to done-flag.
000037      if command = "done" move "y" to done-flag.

