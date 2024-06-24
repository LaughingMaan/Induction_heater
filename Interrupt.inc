;*******************************************************************************
;
;       Подпрограмма обработки прерываний
;
;*******************************************************************************

Interrupt:
    movwf       W_TEMP_          ;запись содержимого аккумулятора W в 
                                 ;регистр W_TEMP
    swapf       STATUS,W         ;обменять местами полубайты регистра STATUS и
                                 ;с записью в W
    clrf        STATUS           ;очистка регистра STATUS, выбрать 0-й банк
    movwf       STATUS_TEMP      ;запись содержимого аккумулятора W в
                                 ;регистр W_TEMP

                                 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                 ;в случае если размер кода превышает 2048 слов:
    movf        PCLATH,W         ;запись содержимого регистра PCLATH в
    movwf       PCLATH_TEMP      ;регистр PCLATH_TEMP
    clrf        PCLATH           ;очистка регистра PCLATH
                                 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                                 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                 ;при использовании косвенной адресации:
    movf        FSR,W            ;запись содержимого регистра FSR в
    movwf       FSR_osn          ;регистр FSR_osn
    movf        FSR_prer,W       ;запись содержимого регистра FSR_prer в
    movwf       FSR              ;регистр FSR
                                 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				 
    ;---------------------------------------------------------------------------
    
    btfsc   PIR1, TMR1IF	 ;проверка на переполнение от TMR1
    goto    Display
    bcf	    INTCON, T0IF	;очистим бит T0IF чтобы избежать повторного входа в прерывание
    incf    TMR0_inc, 1		;инкремент прерывания
    bcf	    STATUS, Z
    movf    TMR0_inc, 0		;загрузка содержимого TMR0_inc в W
    xorwf   TMR0_max, 0		;сравнение W и TMR0_max, результат смотрим в Z
    btfss   STATUS, Z
    goto    _no			;не равны
    goto    _yes		;равны - прошло 100 мс
    
    
   
_yes:
    incf    TMR0_user_inc, 1	; Инкремент и проверка на переполнение
    bcf	    STATUS, Z		; счетчика
    movf    TMR0_user_inc ,0	; 
    xorwf   TMR0_user_max, 0	;
    btfsc   STATUS, Z		;
    
    bsf	    Flags, 0		; Время прошло, установим флаг
    clrf    TMR0_inc
_no:
    movlw   0x04		; Настройка таймера 0 для дальнейшего отсчета
    movwf   TMR0		
    goto    rollback
;------------------------------------------------------------------------------    
Display:
    
    bcf	    DATA_LED_2		; Сначала гасим все три разряда
    bcf	    ENABLE_LED		;
    
    movlw   .8			; Счетчик для записи в сдвиговый регистр 
    movwf   counter		;
    
Display_count: 
    
    bsf	    CLK_B_2		;
    bcf	    CLK_B_2		;
    
    decfsz  counter, 1		;
    goto    Display_count	;
    
    
    movf    cur_digit, w	; Включаем 3 разряд
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
    btfss   cur_digit, 0	; Включаем 2 разряд
    goto    SEG_3		;
    bsf	    DATA_LED_2		;
    bsf	    CLK_B_2		;
    bcf	    CLK_B_2		;
    bcf	    DATA_LED_2		;
    bsf	    CLK_B_2		;
    bcf	    CLK_B_2		;
    
SEG_3:    
    btfss   STATUS, Z		; Включаем 1 разряд
    goto    SEGMENTS		;
    bsf	    DATA_LED_2		;
    bsf	    CLK_B_2		;
    bcf	    CLK_B_2		;
    
SEGMENTS:			; Зажигаем сегмент
    
    movlw   .8			; Заполним счетчик для сдвигового регистра
    movwf   counter		;
    movf    cur_digit, w	;
    addlw   output_text		;
    movwf   FSR			; FSR = адрес регистра 
    movf    INDF, W		;
    xorlw   0xFF		; Маска инверсии (для индикатора с общим анодом)
    movwf   TEMP		;
    movf    TEMP, W		;
    
COUNT:				; Заполнение сдвигового регистра
    bsf	    DATA_LED		;	
    btfss   TEMP, 7		;
    bcf	    DATA_LED		;
    bsf	    CLK_B		;
    bcf	    CLK_B		;
    rlf	    TEMP, 1		;
    decfsz  counter, 1		;
    goto    COUNT		;
    
    incf    cur_digit, f	; Переход к следующему сегменту
    movf    cur_digit, w	;
    xorlw   0x3			;
    btfsc   STATUS, Z		;
    clrf    cur_digit		;
    
    bcf	    PIR1, TMR1IF	; Очистка флага прерывания от TMR1 	    
;-------------------------------------------------------------------------------    
rollback:			; Восстановление контекста
    
    bsf	    ENABLE_LED		; Включаем выходы сдвиговых регистров
    
				 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                 ;при условии использования косвенной адресации:
    movf        FSR,W            ;запись содержимого регистра FSR в
    movwf       FSR_prer         ;регистр FSR_prer
    movf        FSR_osn,W        ;запись содержимого регистра FSR_osn в
    movwf       FSR              ;регистр FSR
                                 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                                 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                 ;при условии сохранения регистра PCLATH:
    movf        PCLATH_TEMP,W    ;восстановление содержимого регистра PCLATH
    movwf       PCLATH           ;
                                 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    swapf       STATUS_TEMP,W    ;восстановление содержимого регистра STATUS
    movwf       STATUS

    swapf       W_TEMP_,F         ;восстановление содержимого аккумулятора W
    swapf       W_TEMP_,W
    
    retfie
    #include "Memory.inc"


