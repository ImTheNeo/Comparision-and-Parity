.INCLUDE "m128def.inc"
.equ fclk = 8000000 ; system clock frequency (for delays)
; register usage
.def temp = R16 ; temporary storage
; LCD interface
.equ lcd_D7_port = PORTC ; lcd D7 connection
.equ lcd_D7_bit = PORTC7
.equ lcd_D7_ddr = DDRC
.equ lcd_D6_port = PORTC ; lcd D6 connection
.equ lcd_D6_bit = PORTC6
.equ lcd_D6_ddr = DDRC
.equ lcd_D5_port = PORTC ; lcd D5 connection
.equ lcd_D5_bit = PORTC5
.equ lcd_D5_ddr = DDRC
.equ lcd_D4_port = PORTC ; lcd D4 connection
.equ lcd_D4_bit = PORTC4
.equ lcd_D4_ddr = DDRC
.equ lcd_E_port = PORTB ; lcd Enable pin
.equ lcd_E_bit = PORTB5
.equ lcd_E_ddr = DDRB
.equ lcd_RS_port = PORTB ; lcd Register Select pin
.equ lcd_RS_bit = PORTB2
.equ lcd_RS_ddr = DDRB
.equ text=0xaa00
.equ text1=0x0200
.equ ZEROS = 0x00
.equ ONES = 0xff
.equ any = 0x500
.equ any1 = 0x501
.equ any2 = 0x502

; LCD module Lines
.equ lcd_LineOne = 0x00 ; line 1
.equ lcd_LineTwo = 0x40 ; line 2
; LCD Defined instructions
.equ lcd_Clear = 0b00000001 ; ASCII 'space' for all characters
.equ lcd_Home = 0b00000010 ; first position on first line
.equ lcd_EntryMode = 0b00000110 ; shift cursor from left to right on read/write
.equ lcd_DisplayOff = 0b00001000 ; turn display off
.equ lcd_DisplayOn = 0b00001100 ; display on, cursor off, don't blink character
.equ lcd_FunctionReset = 0b00110000 ; reset the LCD
.equ lcd_FunctionSet4bit = 0b00101000 ; 4-bit data, 2-line display, 5 x 7 font
.equ lcd_SetCursor = 0b10000000 ; set cursor position
; **************************** Reset Vector *****************************
.org 0x0000
 jmp start ; jump over Interrupt Vectors, Program ID etc.

; **************************** Main Program Code ************************
start:
; initialize the stack pointer to the highest RAM address
 ldi temp,low(RAMEND)
 out SPL,temp
 ldi temp,high(RAMEND)
 out SPH,temp
ldi r20, ZEROS
ldi r21, ONES
; configure the microprocessor pins for the control lines
 sbi lcd_E_ddr, lcd_E_bit ; E line - output
 sbi lcd_RS_ddr, lcd_RS_bit ; RS line - output
; initialize the LCD controller
ldi r16, 0x00 ; Load 0 to R16
out ddra, r16 ; Configure PORTA as input
call lcd_init_4d ; initialize the LCD display for a 4-bit interface


; configure the microprocessor pins for the data lines
 sbi lcd_D7_ddr, lcd_D7_bit ; 4 data lines - output
 sbi lcd_D6_ddr, lcd_D6_bit
 sbi lcd_D5_ddr, lcd_D5_bit
 sbi lcd_D4_ddr, lcd_D4_bit
HERE:out ddra, r20;a is input
out ddrd, r20;d is input
out ddre, r20;e is input
out ddrb, r21; b is output
out ddrc, r21; c is output
in r22,pina ;put the value in pina into r22
	  in r23,pind;put the value in pind into r23
      in R24,pine;put the value in pine into r24

cpi R24, 0x00 ; check if the value entered from pine is 0
breq call_odd_parity ; go to label

cpi R24, 0x01; check if the value entered from pine is 1
breq call_even_parity; go to label

cpi R24, 0x02; check if the value entered from pine is 2
breq call_show_bigger;go to label

cpi R24, 0x03; check if the value entered from pine is 3
breq call_show_diff; go to label




call_odd_parity:
call display_empty_LineTwo;to be second line of lcd always empty
call odd_parity;call odd parity function

jmp HERE ; jump back to here


call_even_parity:
call display_empty_LineTwo;to be second line of lcd always empty
call even_parity;call even parity function
jmp HERE ; jump back to here

call_show_bigger:
call show_bigger;call odd parity function
call display_empty_LineOne;to be first line of lcd always empty
call display2 ; call display in hexadecimal function
jmp HERE ; jump back to here

call_show_diff:
call show_diff;call show diff parity function
call display1;call display in binary function
call display2;call display in hexadecimal function
jmp HERE ; jump back to here






; endless loop
 rjmp HERE
; **************************** End of Main Program Code *****************
; ============================== 4-bit LCD Function Calls ======================
; Name: lcd_init_4d -- initialize the LCD module for a 4-bit data interface
lcd_init_4d:
; Power-up delay
 ldi temp, 100 ; initial 40 mSec delay
 call delayTx1mS
; IMPORTANT - At this point the LCD module is in the 8-bit mode and it is expecting to receive
; 8 bits of data, one bit on each of its 8 data lines, each time the 'E' line is pulsed.
;
; Since the LCD module is wired for the 4-bit mode, only the upper four data lines areconnected to
; the microprocessor and the lower four data lines are typically left open. Therefore, when
; the 'E' line is pulsed, the LCD controller will read whatever data has been set up on theupper
; four data lines and the lower four data lines will be high (due to internal pull-upcircuitry).
;
; Fortunately the 'FunctionReset' instruction does not care about what is on the lower fourbits so
; this instruction can be sent on just the four available data lines and it will beinterpreted
; properly by the LCD controller. The 'lcd_write_4' subroutine will accomplish this if the
; control lines have previously been configured properly.
; Set up the RS and E lines for the 'lcd_write_4' subroutine.
 cbi lcd_RS_port, lcd_RS_bit ; select the Instruction Register (RS low)
 cbi lcd_E_port, lcd_E_bit ; make sure E is initially low
; Reset the LCD controller.
 ldi temp, lcd_FunctionReset ; first part of reset sequence
 call lcd_write_4
 ldi temp, 10 ; 4.1 mS delay (min)
 call delayTx1mS
 ldi temp, lcd_FunctionReset ; second part of reset sequence
 call lcd_write_4
 ldi temp, 200 ; 100 uS delay (min)
 call delayTx1uS
 ldi temp, lcd_FunctionReset ; third part of reset sequence
 call lcd_write_4
 ldi temp, 200 ; this delay is omitted in the data sheet
 call delayTx1uS
; Preliminary Function Set instruction - used only to set the 4-bit mode.
; The number of lines or the font cannot be set at this time since the controller is still inthe
; 8-bit mode, but the data transfer mode can be changed since this parameter is determined byone
; of the upper four bits of the instruction.
 ldi temp, lcd_FunctionSet4bit ; set 4-bit mode
 call lcd_write_4
 ldi temp, 80 ; 40 uS delay (min)
 call delayTx1uS
; Function Set instruction
 ldi temp, lcd_FunctionSet4bit ; set mode, lines, and font
 call lcd_write_instruction_4d
 ldi temp, 80 ; 40 uS delay (min)
 call delayTx1uS
; The next three instructions are specified in the data sheet as part of the initialization routine,
; so it is a good idea (but probably not necessary) to do them just as specified and then redo them
; later if the application requires a different configuration.
; Display On/Off Control instruction
 ldi temp, lcd_DisplayOff ; turn display OFF
 call lcd_write_instruction_4d
 ldi temp, 80 ; 40 uS delay (min)
 call delayTx1uS
; Clear Display instruction
 ldi temp, lcd_Clear ; clear display RAM
 call lcd_write_instruction_4d
 ldi temp, 4 ; 1.64 mS delay (min)
 call delayTx1mS
; Entry Mode Set instruction
 ldi temp, lcd_EntryMode ; set desired shift characteristics
 call lcd_write_instruction_4d
 ldi temp, 80 ; 40 uS delay (min)
 call delayTx1uS
; This is the end of the LCD controller initialization as specified in the data sheet, but the display
; has been left in the OFF condition. This is a good time to turn the display back ON.
; Display On/Off Control instruction
 ldi temp, lcd_DisplayOn ; turn the display ON
 call lcd_write_instruction_4d
 ldi temp, 80 ; 40 uS delay (min)
 call delayTx1uS
 ret
; ---------------------------------------------------------------------------
; Name: lcd_write_string_4d
; Purpose: display a string of characters on the LCD
; Entry: ZH and ZL pointing to the start of the string
; (temp) contains the desired DDRAM address at which to start the display
; Exit: no parameters
; Notes: the string must end with a null (0)
; uses time delays instead of checking the busy flag
lcd_write_string_4d:
; preserve registers
 push ZH ; preserve pointer registers
 push ZL
; fix up the pointers for use with the 'lpm' instruction
;lsl ZL ; shift the pointer one bit left for the lpm instruction
;rol ZH
; set up the initial DDRAM address
 ori temp, lcd_SetCursor ; convert the plain address to a set cursor instruction
 call lcd_write_instruction_4d ; set up the first DDRAM address
 ldi temp, 80 ; 40 uS delay (min)
 call delayTx1uS
; write the string of characters
lcd_write_string_4d_01:
 ld temp, Z+ ; get a character
 cpi temp, 0 ; check for end of string
 breq lcd_write_string_4d_02 ; done
; arrive here if this is a valid character
 call lcd_write_character_4d ; display the character
 ldi temp, 80 ; 40 uS delay (min)
 call delayTx1uS
 rjmp lcd_write_string_4d_01 ; not done, send another character
; arrive here when all characters in the message have been sent to the LCD module
lcd_write_string_4d_02:
 pop ZL ; restore pointer registers
 pop ZH
 ret
; ---------------------------------------------------------------------------
; Name: lcd_write_character_4d
; Purpose: send a byte of information to the LCD data register
; Entry: (temp) contains the data byte
; Exit: no parameters
; Notes: does not deal with RW (busy flag is not implemented)
lcd_write_character_4d:
 sbi lcd_RS_port, lcd_RS_bit ; select the Data Register (RS high)
 cbi lcd_E_port, lcd_E_bit ; make sure E is initially low
 call lcd_write_4 ; write the upper 4-bits of the data
 swap temp ; swap high and low nibbles
 call lcd_write_4 ; write the lower 4-bits of the data
 ret
; ---------------------------------------------------------------------------
; Name: lcd_write_instruction_4d -- Send a byte of information to the LCD instruction register
lcd_write_instruction_4d:
 cbi lcd_RS_port, lcd_RS_bit ; select the Instruction Register (RS low)
 cbi lcd_E_port, lcd_E_bit ; make sure E is initially low
 call lcd_write_4 ; write the upper 4-bits of the instruction
 swap temp ; swap high and low nibbles
 call lcd_write_4 ; write the lower 4-bits of the instruction
 ret
; ---------------------------------------------------------------------------
; Name: lcd_write_4 Send 4-bits of information to the LCD module
; Entry: (temp) contains a byte of data with the desired 4-bits in the upper nibble
; (RS) is configured for the desired LCD register
; (E) is low
; (RW) is low
lcd_write_4:
; set up D7
 sbi lcd_D7_port, lcd_D7_bit ; assume that the D7 data is '1'
 sbrs temp, 7 ; check the actual data value
 cbi lcd_D7_port, lcd_D7_bit ; arrive here only if the data was actually '0'
; set up D6
 sbi lcd_D6_port, lcd_D6_bit ; repeat for each data bit
 sbrs temp, 6
 cbi lcd_D6_port, lcd_D6_bit
; set up D5
 sbi lcd_D5_port, lcd_D5_bit
 sbrs temp, 5
 cbi lcd_D5_port, lcd_D5_bit
; set up D4
 sbi lcd_D4_port, lcd_D4_bit
 sbrs temp, 4
 cbi lcd_D4_port, lcd_D4_bit
; write the data
 ; 'Address set-up time' (40 nS)
 sbi lcd_E_port, lcd_E_bit ; Enable pin high
 call delay1uS ; implement 'Data set-up time' (80 nS) and 'Enable pulse width' (230 nS)
 cbi lcd_E_port, lcd_E_bit ; Enable pin low
 call delay1uS ; implement 'Data hold time' (10 nS) and 'Enable cycle time' (500 nS)
 ret
; ============================== End of 4-bit LCD Subroutines ===============
; ============================== Time Delay Subroutines =====================
; Name: delayYx1mS Delay of (YH:YL) x 1 mS
delayYx1mS:
 call delay1mS ; delay for 1 mS
 sbiw YH:YL, 1 ; update the the delay counter
 brne delayYx1mS ; counter is not zero
; arrive here when delay counter is zero (total delay period is finished)
 ret
; ---------------------------------------------------------------------------
; Name: delayTx1mS Provide a delay of (temp) x 1 mS
delayTx1mS:
 call delay1mS ; delay for 1 mS
 dec temp ; update the delay counter
 brne delayTx1mS ; counter is not zero
; arrive here when delay counter is zero (total delay period is finished)
 ret
; ---------------------------------------------------------------------------
; Name: delay1mS -- Delay of 1 mS
delay1mS:
 push YL ; [2] preserve registers
 push YH ; [2]
 ldi YL, low (((fclk/1000)-18)/4) ; [1] delay counter
 ldi YH, high(((fclk/1000)-18)/4) ; [1]
delay1mS_01:
 sbiw YH:YL, 1 ; [2] update the the delay counter
 brne delay1mS_01 ; [2] delay counter is not zero
; arrive here when delay counter is zero
 pop YH ; [2] restore registers
 pop YL ; [2]
 ret ; [4]
; ---------------------------------------------------------------------------
; Name: delayTx1uS Delay of (temp) x 1 uS with a 8 MHz clock frequency
delayTx1uS:
 call delay1uS ; delay for 1 uS
 dec temp ; decrement the delay counter
 brne delayTx1uS ; counter is not zero
; arrive here when delay counter is zero (total delay period is finished)
 ret
; ---------------------------------------------------------------------------
; Name: delay1uS
; Purpose: Delay of 1 uS with a 8 MHz clock frequency
delay1uS:
 push temp ; [2] Consume clock cycles
 pop temp ; [2]
 ret ; [4]
; ============================== End of Time Delay Subroutines ==============

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~MY SUBROUTINE~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.org 0x300

odd_parity:
			mov r30,r22; copy r22 to r30
			ldi r27,0x07;loading the counter
			ldi r26,ZEROS;clearing r26 count
			ldi r28,ZEROS;clearing r28 counts of 1s
			loop:cp r27,r26;condition check
			breq mod;if met exit and calculate mod
			inc r26 ;increment the counter
		    ror r22;if not rotate
			brcc loop;if c=0 not increment
			inc r28;increment r28 if c = 1
			jmp loop;go back to beginnig

			
			;basic modulus function
			mod:ldi r25,0x02;calculating the modulus of counts of 1's
				 cp r28,r25 ;check until r28 is smaller than 2 
				 brlt lcd ; when it is less jump to lcd
				 sub r28,r25; subtract 2 from r28
				 jmp mod ; jump to label
				 

			lcd:lsl r30 ; first digit was initally empty due to algorithm we have to shift
				dec r28;decrementing r28
				brmi even;if we got a minus number then we have an even number of 1's
			odd:jmp exit ; if it is odd jump to exit
			
			even: subi r30,-1 ; if number is even add 1 to make it odd
				jmp exit
				

			exit:
			call display1 ; call display function
			ret;return
.org 0x360

even_parity:
			mov r30,r22 ; copy r22 to r30
			ldi r27,0x07;loading the counter
			ldi r26,ZEROS;clearing r26 count
			ldi r28,ZEROS;clearing r28 counts of 1s
			loop1:cp r27,r26;condition check
			breq mod1;if met exit and calculate mod
			inc r26;increment the counter
		    ror r22;if not rotate
			brcc loop1;if c=0 not increment
			inc r28;increment r28 if c = 1
			jmp loop1;go back to beginnig

			
			;basic modulus operation
			mod1:ldi r25,0x02;calculating the modulus of counts of 1's
				 cp r28,r25 ; check until r28 is smaller than 2
				 brlt lcd1 ; if yes jump to label
				 sub r28,r25 ; subtract 2 from r28
				 jmp mod1 ;jump to label
				 

			lcd1:lsl r30 ;; first digit was initally empty due to algorithm we have to shift
				dec r28;decrementing r28
				brmi even1;if we got a minus number then we have an even number of 1's
			odd1:subi r30,-1 ; if we have odd number of 1's add 1 to make it even
				jmp exit1 ;jmp to label
			
			even1: jmp exit1;if it is even just jump
				
			exit1:
			call display1;call display function
			ret;return

.org 0x400

show_bigger:
cp r22,r23; check if it is equal
breq eq ; if yes go to eq lbel
cp r22,r23 ; if r23 is bigger 
brlt b_bigger ;go to b bigger
cp r23,r22 ; else 
brlt a_bigger ; go to a bigger
eq:
ldi r21,0 ;this is my carry flag to detect if equal

jmp end ; go to end
a_bigger:;no need to fill because value is already in r22
jmp end 
b_bigger:
mov r22,r23 ; copy r23 into r22 , because in display func i use r22
;lcd ye nas?l gondercegini ogren

jmp end ; jump to end
end:
ret;return

.org 0x500


show_diff:
cp r22,r23;check who is smller
brlt a_smaller ; if a smaller go to label

sub r22,r23 ;else b is smaller , just subract b from a
jmp end_sub ; jump to label

a_smaller:
sub r23,r22 ; subtract a from b
mov r22,r23; copy the result into r22 because, display func is for r22
jmp end_sub ; jump to label


end_sub:
mov r30,r22 ; copy r22 to r30

ret; return


.org 0x550
display1:
		mov r19,r30 ; copy r30 to r19 because r30 is Z pointer
		mov r17,r30 ; same for r17
		ldi ZH,high(0x0100); point to the information that is to be displayed
		ldi ZL, low(0x0100);same above
		ldi XH,high(0x0100);same above
		ldi XL,low(0x0100);same above
		
		ldi r18,9 ; we will display 8 bit
		display_loop:dec r18 ; decrement the counter
		breq display_exit ; go to label
		rol r17 ; rotate left with carry
		brcs display_1 ; if carry is 1 display '1'
		jmp display_0 ;else display '0'


		display_1:
		
		ldi temp,'1' ; put '1' into temp
		st X+,temp ; first put temp into X, then increment x pointer
		jmp display_loop ;go to label

		display_0:
		
		ldi temp,'0' ; put '0' into temp
		st X+,temp ;first put temp into X, then increment x pointer
		jmp display_loop ;go to label
		
		display_exit:

		ldi temp, lcd_LineOne ; point to where the information should be displayed
		call lcd_write_string_4d ;call display function
		ret
.org 0x600
display2:
	ldi ZH,high(0x0150); point to the information that is to be displayed
	ldi ZL, low(0x0150);Same
	ldi XH,high(0x0150); point to the information that is to be displayed
	ldi XL, low(0x0150);same
	ldi r16,0;counter is 0, i wil make 2 display when it becomes 1 it will stop
	cpi r21,0; if r22 is zero , means numbers are equal
	breq print_eq;jump to equal label
	mov r24,r22 ; copy r22 into r24
	swap r24; change first and last 4 bit
	lsr r24; rotate right 4 times
	lsr r24
	lsr r24
	lsr r24
	lsr r22;rotate right 4 times
	lsr r22
	lsr r22
	lsr r22
	cpi r22,10 ; if number is less than
	brmi show_sayi ; it is number , go to number display label
	jmp show_char; else it is a character , go to char display label
	second_val:
	mov r22,r24;this is for the other 4 bits
	inc r16; increment counter;
	cpi r22,10; if it is less than 10
	brmi show_sayi; it is a number
	jmp show_char;else it is a char
	





	show_sayi:subi r22,-48; add 48 to number because it is equal to it is ascii
		st x+,r22 ; put r22 into x pointer than increment x
		cpi r16,1 ; if counter is 1, it means go to display
		breq display2_exit ; if r16 is 1 , then i print 2 characters
		jmp second_val ;else jump for the second 4 bit
	show_char:subi r22,-55 ;add 55 to character becauese A = 55 , B=56 in ascii etc.
		st x+,r22 ;store r22 into x pointer, than increment x
		cpi r16,1 ; if r16 is 1 , then i print 2 characters
		breq display2_exit ; jump to exit
		jmp second_val ; else jump to display second value
	print_eq:
		ldi r16,'E' ; put E into r16
		sts 0x0150,r16 ; store at location 0x0150
		ldi r16,'Q' ; put Q into r16
		sts 0x0151,r16 ; store at location 0x0151
		ldi r21,0xFF;i should fill it again because to fill ddr's again(without these it can work but i didnt try)

	display2_exit:
	ldi temp,lcd_LineTwo ;load the displaying value
	call lcd_write_string_4d ; call the display function
	ret
.org 0x650
display_empty_LineTwo:
	ldi ZH,high(0x0160); point to the information that is to be displayed
	ldi ZL, low(0x0160);Same
	
	ldi r16,' ';(empty string for first digit)
	sts 0x0160,r16;store the empty string at memory
	ldi r16,' ';(empty string for second digit)
	sts 0x161,r16;store the empty string at memory

	ldi temp,lcd_LineTwo;load the displaying value
	call lcd_write_string_4d;call write function
	ret
.org 0x700
display_empty_LineOne:
	ldi ZH,high(0x0170); point to the information that is to be displayed
	ldi ZL, low(0x0170);Same
	
	ldi r16,' ';empty character
	sts 0x0170,r16;load empty to 1. character
	sts 0x0171,r16;load empty to 2. character
	sts 0x0172,r16;load empty to 3. character
	sts 0x0173,r16;load empty to 4. character
	sts 0x0174,r16;load empty to 5. character
	sts 0x0175,r16;load empty to 6. character
	sts 0x0176,r16;load empty to 7. character
	sts 0x0177,r16;load empty to 8. character


	ldi temp,lcd_LineOne; load the value that will be displayed
	call lcd_write_string_4d ; call write function
	ret
