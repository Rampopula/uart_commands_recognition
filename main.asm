; ************************************************************************************************
; ************************************************************************************************
.device ATmega328p
.include "m328Pdef.inc"
; *****************************************
.def zero			= R2
.def one			= R3
.def convert_status = R4
.def compare_status = R5
.def command_start	= R6
.def command_end	= R7
.def temp_addr_l	= R8
.def temp_addr_h	= R9
.def temp_5			= R10
.def temp_uart		= R16
.def temp_sreg		= R17
.def temp_2			= R18
.def temp_3			= R19
.def temp_4			= R20
.def counter		= R21
.def CL				= R24
.def CH				= R25
; *****************************************
.equ BAUD	= 9600   
.equ XTAL	= 16000000 
.equ UBRRN	= XTAL/(16*BAUD)-1 
; *****************************************
.equ START_SYMB		= 0x25
.equ END_SYMB		= 0x2A
.equ command_amount = 0x05
.equ array_size		= 0x10
; *****************************************   
.dseg
	.org 0x0100		
		input_buffer:	.byte 256
		ascii_buffer:	.byte 64
		hex_buffer:		.byte 16
		array_uart:		.byte 16
.cseg
	.org 0x0000			rjmp RESET
	.org URXCaddr		rjmp UART_RX_COMPLETE

; ******************************************************************************
command_00:			.db high(label_00),low(label_00),high(answer_00<<1),low(answer_00<<1),0x00,0x00
command_01:			.db high(label_01),low(label_01),high(answer_01<<1),low(answer_01<<1),0x00,0x00
command_02:			.db high(label_02),low(label_02),high(answer_02<<1),low(answer_02<<1),0x02,0x00
command_03:			.db high(label_03),low(label_03),high(answer_03<<1),low(answer_03<<1),0x00,0x00
command_04:			.db high(label_04),low(label_04),high(answer_04<<1),low(answer_04<<1),0x00,0x00
;command_05:		.db high(label_05),low(label_05),high(answer_05<<1),low(answer_05<<1),0x00,0x00
;command_06:		.db high(label_06),low(label_06),high(answer_06<<1),low(answer_06<<1),0x00,0x00
;command_07:		.db high(label_07),low(label_07),high(answer_07<<1),low(answer_07<<1),0x00,0x00
;command_08:		.db high(label_08),low(label_08),high(answer_08<<1),low(answer_08<<1),0x00,0x00
;command_09:		.db high(label_09),low(label_09),high(answer_09<<1),low(answer_09<<1),0x00,0x00
;command_0A:		.db high(label_0A),low(label_0A),high(answer_0A<<1),low(answer_0A<<1),0x00,0x00
;command_0B:		.db high(label_0B),low(label_0B),high(answer_0B<<1),low(answer_0B<<1),0x00,0x00
;command_0C:		.db high(label_0C),low(label_0C),high(answer_0C<<1),low(answer_0C<<1),0x00,0x00
;command_0D:		.db high(label_0D),low(label_0D),high(answer_0D<<1),low(answer_0D<<1),0x00,0x00
;command_0E:		.db high(label_0E),low(label_0E),high(answer_0E<<1),low(answer_0E<<1),0x00,0x00
;command_0F:		.db high(label_0F),low(label_0F),high(answer_0F<<1),low(answer_0F<<1),0x00,0x00
; ******************************************************************************
answer_ok:			.db	"Ok",				 0x0A,0x0D,0xFF,0x00
error:				.db	"Error",			 0x0A,0x0D,0xFF
wrong_command:		.db	"Wrong command",	 0x0A,0x0D,0xFF
init_ok:			.db	"CPU Init Ok",		 0x0A,0x0D,0xFF
; ******************************************************************************
answer_00:			.db	"Restart...Disabled",0x0A,0x0D,0xFF,0x00
answer_01:			.db	"Pull out array...", 0x0A,0x0D,0xFF
answer_02:			.db	"Write to array...", 0x0A,0x0D,0xFF
answer_03:			.db	"Led On",			 0x0A,0x0D,0xFF,0x00
answer_04:			.db	"Led Off",			 0x0A,0x0D,0xFF
;answer_05:			.db	"",					 0x0A,0x0D,0xFF
;answer_06:			.db	"",					 0x0A,0x0D,0xFF
;answer_07:			.db	"",					 0x0A,0x0D,0xFF
;answer_08:			.db	"",					 0x0A,0x0D,0xFF
;answer_09:			.db	"",					 0x0A,0x0D,0xFF
;answer_0A:			.db	"",					 0x0A,0x0D,0xFF
;answer_0B:			.db	"",					 0x0A,0x0D,0xFF
;answer_0C:			.db	"",					 0x0A,0x0D,0xFF
;answer_0D:			.db	"",					 0x0A,0x0D,0xFF
;answer_0E:			.db	"",					 0x0A,0x0D,0xFF
;answer_0F:			.db	"",					 0x0A,0x0D,0xFF
; ******************************************************************************
		
UART_RX_COMPLETE:
	in temp_sreg,SREG
	lds temp_uart,UDR0
	st X,temp_uart
	inc XL
	out SREG,temp_sreg	
reti

USART_Init:
	ldi temp_2,low(UBRRN)
	sts UBRR0L,temp_2
	ldi temp_2,high(UBRRN)
	sts UBRR0H,temp_2
	ldi temp_2,(1<<RXEN0)|(1<<TXEN0)|(1<<RXCIE0)
	sts UCSR0B,temp_2
	ldi temp_2,(1<<UCSZ01)|(1<<UCSZ00)
	sts UCSR0C,temp_2
ret

ASCII_to_HEX:
	mov convert_status,zero
	cpi temp_2,0x3A
	brlo compare_digit
	cpi temp_2,0x47
	brlo compare_bigchar
	cpi temp_2,0x67	
	brlo compare_char
	rjmp err_convert
	compare_digit:
		cpi temp_2,0x30	
		brsh digit
		rjmp err_convert	
	compare_bigchar:
		cpi temp_2,0x41
		brsh bigchar
		rjmp err_convert	
	compare_char:
		cpi temp_2,0x61
		brsh char
		rjmp err_convert		
	digit:
		subi temp_2,0x30
		ret				 
	bigchar:
		subi temp_2,0x41 - 0x0A
		ret
	char:
		subi temp_2,0x61 - 0x0A
		ret
err_convert:
	mov convert_status,one
ret

HEX_to_ASCII:
	ldi ZL,low(ascii_buffer)
	ldi ZH,high(ascii_buffer)
	mov temp_addr_l,YL
	mov temp_addr_h,YH
	ldi YL,low(array_uart)
	ldi YH,high(array_uart)
	ldi counter,16
	convert_loop:
		ld temp_2,Y+
		mov temp_3,temp_2
		mov convert_status,one
		convert_byte:
			sbrc convert_status,0
			swap temp_2
			andi temp_2,0x0F
			cpi temp_2,0x0A
			brsh convert_char
			subi temp_2,-0x30
			st Z+,temp_2
			rjmp convert_end
		convert_char:
			subi temp_2,-0x37
			st Z+,temp_2
		convert_end:
			cp convert_status,one
				brne exit_convert
			mov convert_status,zero
			mov temp_2,temp_3
			rjmp convert_byte
		exit_convert:
			ldi temp_2,0x20
			st Z+,temp_2
	dec counter
	brne convert_loop
	ldi temp_2,0x0A
	st Z+,temp_2
	ldi temp_2,0x0D
	st Z+,temp_2
	mov YL,temp_addr_l
	mov YH,temp_addr_h
ret

RESET:
	clr temp_2
	ldi ZL,low(SRAM_START)
	ldi ZH,high(SRAM_START)	
	Flush_SRAM:
		st Z+,temp_2
		cpi ZH,high(RAMEND+1)
		brne Flush_SRAM
		cpi ZL,low(RAMEND+1)
		brne Flush_SRAM
		clr ZL
		clr ZH
		ldi ZL,30
		clr ZH
	Flush_Regs:
		dec ZL
		st Z,ZH
		brne Flush_Regs	
	ldi temp_2,low(RAMEND)
	out SPL,temp_2
	ldi temp_2,high(RAMEND)
	out SPH,temp_2

	ldi temp_2,0x01
	mov one,temp_2

	ldi temp_2,(1<<5)
	out DDRB,temp_2

	ldi XL,low(input_buffer)
	ldi XH,high(input_buffer)
	mov YL,XL
	mov YH,XH

	rcall USART_Init

	ldi ZL,low(init_ok<<1)
	ldi ZH,high(init_ok<<1)
	rcall write_answer_uart

	sei

forever:
	cp XL,YL
		breq forever	
	cp compare_status,one
		breq read_command
	ld temp_2,Y
	inc YL
	cpi temp_2,START_SYMB
		breq start_read_command
	rjmp forever
	
	start_read_command:
		mov compare_status,one
		ldi ZL,low(ascii_buffer)
		ldi ZH,high(ascii_buffer)
		rjmp forever

	read_command:
		ld temp_2,Y		
		cpi temp_2,START_SYMB
			breq break_read_command
		inc YL
		cpi temp_2,END_SYMB
			breq end_read_command		
		st Z+,temp_2
		inc counter
		rjmp forever
	
	break_read_command:
		clr counter
		clr compare_status
		rjmp forever

	end_read_command:
		mov temp_addr_l,YL
		mov temp_addr_h,YH
		sbrc counter,0
			rjmp convert_error
		lsr counter
		mov temp_4,counter
		dec temp_4

		ldi ZL,low(ascii_buffer)
		ldi ZH,high(ascii_buffer)
		ldi YL,low(hex_buffer)
		ldi YH,high(hex_buffer)
		convert_loop_m:
			ld temp_2,Z+	
			rcall ASCII_to_HEX
			cp convert_status,one
				breq convert_error
			mov temp_3,temp_2
			swap temp_3
			ld temp_2,Z+
			rcall ASCII_to_HEX
			cp convert_status,one
				breq convert_error
			or temp_3,temp_2
			st Y+,temp_3
			dec counter
			brne convert_loop_m

		ldi YL,low(hex_buffer)
		ldi YH,high(hex_buffer)
		ld temp_2,Y
		cpi temp_2,command_amount
			brsh convert_error
		ldi ZL,low(command_00<<1)
		ldi ZH,high(command_00<<1)
		cp temp_2,zero
			breq load_command_params
		loop_addr:
			adiw Z,6
			dec temp_2
			brne loop_addr
		load_command_params:
			lpm	CH,Z+				; high ijmp 
			lpm	CL,Z+				; low ijmp
			lpm	YH,Z+				; high answer
			lpm	YL,Z+				; low answer
			lpm temp_3,Z+			; amount of params
			cp temp_3,temp_4
				brne convert_error
			mov ZL,YL
			mov ZH,YH
			rcall write_answer_uart
			mov ZL,CL
			mov ZH,CH
			mov YL,temp_addr_l
			mov YH,temp_addr_h
			mov compare_status,zero
			ijmp

		convert_error:
			mov compare_status,zero
			mov counter,zero
			mov YL,temp_addr_l
			mov YH,temp_addr_h
			ldi ZL,low(wrong_command<<1)
			ldi ZH,high(wrong_command<<1)
			rcall write_answer_uart
			rjmp forever

; ************************************************************************************************

	label_00:
		; mcu reset
		rjmp forever

	label_01:	
		rcall HEX_to_ASCII
		ldi ZL,low(ascii_buffer)
		ldi ZH,high(ascii_buffer)
		ldi counter,50
		send_array_uart:
			ld temp_2,Z+
				while_udre_a:
				lds temp_3,UCSR0A
				sbrs temp_3,UDRE0
				rjmp while_udre_a
			sts UDR0,temp_2
			dec counter
			brne send_array_uart
		rcall send_ok_uart
		rjmp forever

	label_02:	
		mov temp_addr_l,YL
		mov temp_addr_h,YH
		ldi YL,low(hex_buffer)+1
		ldi YH,high(hex_buffer)
		ld temp_2,Y+					; address
		cpi temp_2,0x10
			brsh send_error
		subi temp_2,-low(array_uart)
		mov ZL,temp_2
		ldi ZH,high(array_uart)
		ld temp_2,Y+
		st Z+,temp_2
		rcall send_ok_uart
		rjmp label_02_end

		send_error:
			rcall send_error_uart

		label_02_end:
			mov YL,temp_addr_l
			mov YH,temp_addr_h

		rjmp forever

	label_03:
		sbi PORTB,5	
		rcall send_ok_uart
		rjmp forever

	label_04:
		cbi PORTB,5
		rcall send_ok_uart
		rjmp forever

; ************************************************************************************************
send_ok_uart:
	ldi ZL,low(answer_ok<<1)
	ldi ZH,high(answer_ok<<1)
	rcall write_answer_uart
ret

send_error_uart:
	ldi ZL,low(error<<1)
	ldi ZH,high(error<<1)
	rcall write_answer_uart
ret

write_answer_uart:
	send_to_uart:
		lpm temp_4,Z+
		cpi temp_4,0xFF
			breq write_exit
			while_udre:
			lds temp_3,UCSR0A
			sbrs temp_3,UDRE0
			rjmp while_udre
		sts UDR0,temp_4
	rjmp send_to_uart
	write_exit:
ret
; ************************************************************************************************
; ************************************************************************************************