;*******************************************************************************
;
;       ������������ ��������� ����������
;
;*******************************************************************************

Interrupt:
    movwf       W_TEMP_          ;������ ����������� ������������ W � 
                                 ;������� W_TEMP
    swapf       STATUS,W         ;�������� ������� ��������� �������� STATUS �
                                 ;� ������� � W
    clrf        STATUS           ;������� �������� STATUS, ������� 0-� ����
    movwf       STATUS_TEMP      ;������ ����������� ������������ W �
                                 ;������� W_TEMP

                                 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                 ;� ������ ���� ������ ���� ��������� 2048 ����:
    movf        PCLATH,W         ;������ ����������� �������� PCLATH �
    movwf       PCLATH_TEMP      ;������� PCLATH_TEMP
    clrf        PCLATH           ;������� �������� PCLATH
                                 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                                 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                 ;��� ������������� ��������� ���������:
    movf        FSR,W            ;������ ����������� �������� FSR �
    movwf       FSR_osn          ;������� FSR_osn
    movf        FSR_prer,W       ;������ ����������� �������� FSR_prer �
    movwf       FSR              ;������� FSR
                                 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				 
    ;---------------------------------------------------------------------------
    
    btfsc   PIR1, TMR1IF	 ;�������� �� ������������ �� TMR1
    goto    Display
    bcf	    INTCON, T0IF	;������� ��� T0IF ����� �������� ���������� ����� � ����������
    incf    TMR0_inc, 1		;��������� ����������
    bcf	    STATUS, Z
    movf    TMR0_inc, 0		;�������� ����������� TMR0_inc � W
    xorwf   TMR0_max, 0		;��������� W � TMR0_max, ��������� ������� � Z
    btfss   STATUS, Z
    goto    _no			;�� �����
    goto    _yes		;����� - ������ 100 ��
    
    
   
_yes:
    incf    TMR0_user_inc, 1	; ��������� � �������� �� ������������
    bcf	    STATUS, Z		; ��������
    movf    TMR0_user_inc ,0	; 
    xorwf   TMR0_user_max, 0	;
    btfsc   STATUS, Z		;
    
    bsf	    Flags, 0		; ����� ������, ��������� ����
    clrf    TMR0_inc
_no:
    movlw   0x04		; ��������� ������� 0 ��� ����������� �������
    movwf   TMR0		
    goto    rollback
;------------------------------------------------------------------------------    
Display:
    
    bcf	    DATA_LED_2		; ������� ����� ��� ��� �������
    bcf	    ENABLE_LED		;
    
    movlw   .8			; ������� ��� ������ � ��������� ������� 
    movwf   counter		;
    
Display_count: 
    
    bsf	    CLK_B_2		;
    bcf	    CLK_B_2		;
    
    decfsz  counter, 1		;
    goto    Display_count	;
    
    
    movf    cur_digit, w	; �������� 3 ������
    btfss   cur_digit, 1	;
    goto    SEG_2		;
    bsf	    DATA_LED_2		;
    bsf	    CLK_B_2		;  
    bcf	    CLK_B_2		;
    bcf	    DATA_LED_2		;
    bsf	    CLK_B_2		;
    bcf	    CLK_B_2		;
    bsf	    CLK_B_2		;
    bcf	    CLK_B_2		;
    
SEG_2:
    btfss   cur_digit, 0	; �������� 2 ������
    goto    SEG_3		;
    bsf	    DATA_LED_2		;
    bsf	    CLK_B_2		;
    bcf	    CLK_B_2		;
    bcf	    DATA_LED_2		;
    bsf	    CLK_B_2		;
    bcf	    CLK_B_2		;
    
SEG_3:    
    btfss   STATUS, Z		; �������� 1 ������
    goto    SEGMENTS		;
    bsf	    DATA_LED_2		;
    bsf	    CLK_B_2		;
    bcf	    CLK_B_2		;
    
SEGMENTS:			; �������� �������
    
    movlw   .8			; �������� ������� ��� ���������� ��������
    movwf   counter		;
    movf    cur_digit, w	;
    addlw   output_text		;
    movwf   FSR			; FSR = ����� �������� 
    movf    INDF, W		;
    xorlw   0xFF		; ����� �������� (��� ���������� � ����� ������)
    movwf   TEMP		;
    movf    TEMP, W		;
    
COUNT:				; ���������� ���������� ��������
    bsf	    DATA_LED		;	
    btfss   TEMP, 7		;
    bcf	    DATA_LED		;
    bsf	    CLK_B		;
    bcf	    CLK_B		;
    rlf	    TEMP, 1		;
    decfsz  counter, 1		;
    goto    COUNT		;
    
    incf    cur_digit, f	; ������� � ���������� ��������
    movf    cur_digit, w	;
    xorlw   0x3			;
    btfsc   STATUS, Z		;
    clrf    cur_digit		;
    
    bcf	    PIR1, TMR1IF	; ������� ����� ���������� �� TMR1 	    
;-------------------------------------------------------------------------------    
rollback:			; �������������� ���������
    
    bsf	    ENABLE_LED		; �������� ������ ��������� ���������
    
				 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                 ;��� ������� ������������� ��������� ���������:
    movf        FSR,W            ;������ ����������� �������� FSR �
    movwf       FSR_prer         ;������� FSR_prer
    movf        FSR_osn,W        ;������ ����������� �������� FSR_osn �
    movwf       FSR              ;������� FSR
                                 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                                 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                 ;��� ������� ���������� �������� PCLATH:
    movf        PCLATH_TEMP,W    ;�������������� ����������� �������� PCLATH
    movwf       PCLATH           ;
                                 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    swapf       STATUS_TEMP,W    ;�������������� ����������� �������� STATUS
    movwf       STATUS

    swapf       W_TEMP_,F         ;�������������� ����������� ������������ W
    swapf       W_TEMP_,W
    
    retfie
    #include "Memory.inc"


