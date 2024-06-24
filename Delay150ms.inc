;*******************************************************************************
;
;       Подпрограмма задержки на 150мс
;
;*******************************************************************************
; Delay = 0.15 seconds
; Clock frequency = 20 MHz

; Actual delay = 0.15 seconds = 750000 cycles
; Error = 0 %


Delay_150
			;749992 cycles
	movlw	0x85
	movwf	d1
	movlw	0xA3
	movwf	d2
	movlw	0x02
	movwf	d3
Delay_1
	decfsz	d1, f
	goto	$+2
	decfsz	d2, f
	goto	$+2
	decfsz	d3, f
	goto	Delay_0

			;4 cycles
	goto	$+1
	goto	$+1

			;4 cycles (including call)
	return

#include "Memory.inc"



