; PIC16F628A Configuration Bit Settings
; ASM source line config statements
#include "p16F628A.inc"


#define COOL_LED    PORTA, 0
#define CLK_B	    PORTA, 1
#define ENABLE_LED  PORTA, 2
#define DATA_LED    PORTA, 3
#define STOP_B	    PORTA, 4
#define TEMP_IN     PORTA, 7
#define PLUS_B	    PORTA, 5
#define MINUS_B	    PORTA, 6

#define ERROR_LED   PORTB, 0
#define HEAT_LED    PORTB, 1
#define SETUP_B	    PORTB, 2
#define HEAT_B	    PORTB, 3
#define WATER	    PORTB, 4
#define DATA_LED_2  PORTB, 5
#define CLK_B_2	    PORTB, 6
#define	SAVE_B	    PORTB, 7


; CONFIG
    __CONFIG _FOSC_INTOSCIO & _WDTE_OFF & _PWRTE_OFF & _MCLRE_OFF & _BOREN_OFF & _LVP_OFF & _CPD_OFF & _CP_OFF
    
    org	    0x00
    goto    start ;escape interrupt subprogram
    org	    0x04
    goto    Interrupt

start:
    call    init
;--------------------------------------------------------------------------
;временно зададим константы
;--------------------------------------------------------------------------
    bcf	    STATUS, RP0
    bcf	    STATUS, RP1

    call    EEPROM_READ
    call    Bin2Dec
    clrf    TMR1L
    clrf    TMR1H
    bcf	    PIR1, TMR1IF
    bsf	    STATUS, RP0
    errorlevel -302
    bsf	    PIE1, TMR1IE
    errorlevel +302
    bcf	    STATUS, RP0
    bsf	    INTCON, PEIE
    bsf	    INTCON, GIE

    goto    postinit
;*******************************************************************************
;
;	ОСНОВНОЙ ЦИКЛ
;
;*******************************************************************************
postinit_0:
   call	    EEPROM_WRITE	; Запись в ПЗУ после выхода из настройки
postinit:
    clrf    PORTB
    bcf	    COOL_LED
    btfsc   HEAT_B		; Опрос кнопок
    goto    heat		;
    btfsc   SETUP_B		;
    goto    setup		;

    goto    postinit		

heat:
    bcf	    Flags,  7
    bsf	    HEAT_LED		; Начинаем нагрев, запустим TMR0
    clrf    TMR0_user_inc	;
    clrf    TMR0_inc		;
    movlw   0x04		;
    movwf   TMR0		; 
    bsf	    INTCON, T0IE	;


checks:				; Блок проверок во время нагрева
    
    movf    TMR0_user_inc, W	; Преобразование текущего времени
    movwf   work_data		;
    call    Bin2Dec		;
    
    btfsc   WATER		; Опрос датчика наличия воды
    goto    accident		;
    btfsc   STOP_B		; Опрос кнопки СТОП
    goto    accident		;
    btfss   TEMP_IN		; Опрос компаратора
    goto    accident		;

    btfsc   Flags, 0		; Отработано заданое время
    goto    Work_done		;

    goto    checks		; Зациклим проверки

setup:				; Блок настроек
    
    btfss   Flags,  7
    movf    TMR0_user_max, W	; Запуск преобразования настроек для вывода на
    btfsc   Flags,  7		; 7-сегментник
    movf    TMR0_user_max_cool, W;
    movwf   work_data		;
    call    Bin2Dec		;
    
    btfss   SETUP_B		
    goto    postinit_0
    btfsc   PLUS_B
    goto    increment
    btfsc   MINUS_B
    goto    decrement
    btfsc   SAVE_B
    goto    change_mode
    goto    setup
change_mode:
    call    Delay_150
    btfsc   SAVE_B
    goto    setup
    
    bsf	    Flags, 7
    bsf	    ERROR_LED
    call    Delay
    bcf	    ERROR_LED
    goto    setup
increment:			; При нажатии ПЛЮС попадаем сюда
    
    call    Delay_150		; Сглаживание дребезга
    btfss   PLUS_B		;
    goto    setup		;
    btfsc   Flags, 7
    goto    increment_cool
increment_heat:
    movlw   .200		; Проверка на превышение макс. значения
    xorwf   TMR0_user_max, W	;
    btfsc   STATUS, Z		;
    clrf    TMR0_user_max	;
    
    incf    TMR0_user_max ,1
    goto    setup
increment_cool:
    movlw   .200		; Проверка на превышение макс. значения
    xorwf   TMR0_user_max_cool, W	
    btfsc   STATUS, Z		;
    clrf    TMR0_user_max_cool	;
    
    incf    TMR0_user_max_cool ,1
    goto    setup
    
decrement:			; При нажатии минус попадаем сюда

    call    Delay_150		; Сглаживание дребезга
    btfss   MINUS_B		;
    goto    setup		;
    
    btfsc   Flags, 7
    goto    decrement_cool
    decfsz  TMR0_user_max,1	; Проверка на превышение мин. значения
    goto    setup		;
    
    movlw   .200		; Попадаем сюда при TMR0_user_max < 0
    movwf   TMR0_user_max	;
    
    goto    setup
decrement_cool:
    
    decfsz  TMR0_user_max_cool,1	; Проверка на превышение мин. значения
    goto    setup		;
    
    movlw   .200		; Попадаем сюда при TMR0_user_max < 0
    movwf   TMR0_user_max_cool	;
    
    goto    setup
accident:			; Попадаем сюда при аварии
    
    clrf    PORTB		; Остановим нагрев и прерывания по TMR0
    bcf	    INTCON, T0IE	; Зажжем светодиод АВАРИЯ и ожиданием
    bcf	    Flags, 0		; подтверждения от оператора через кнопку НАГРЕВ
    bsf	    ERROR_LED		;
    btfss   HEAT_B		;
    goto    $-1			;
    nop				;
    call    Delay		; Задержка на 1с для исключения дребезга
    
    goto    postinit

Work_done:			; Попадаем сюда если нагрев закончен
    
    bcf	    HEAT_LED		; Выключим нагрев и прерывания от TMR0
    bcf	    INTCON, T0IE	;
    bcf	    Flags, 0		;
    
    bsf	    COOL_LED		; Начинаем охлаждение, запустим TMR0
    clrf    TMR0_user_inc	;
    clrf    TMR0_inc		;
    movlw   0x04		;
    movwf   TMR0		; 
    bsf	    INTCON, T0IE	;
    movf    TMR0_user_max, W	
    movwf   TEMP_C
    movf    TMR0_user_max_cool, W
    movwf   TMR0_user_max
Cool:
   
    movf    TMR0_user_inc, W	; Преобразование текущего времени
    movwf   work_data		;
    call    Bin2Dec		;
    
    btfsc   Flags, 0		; Отработано заданое время
    goto    Cool_done		;
    goto    Cool		;

Cool_done:
    bcf	    COOL_LED		; Выключим охлаждение и прерывания от TMR0
    bcf	    INTCON, T0IE	;
    bcf	    Flags,  0		;
    clrf    TMR0_user_inc
    clrf    TMR0_inc
    movf    TEMP_C,   W
    movwf   TMR0_user_max
    movwf   work_data		;
    call    Bin2Dec		;
    goto    postinit
    
;*******************************************************************************
;
;       ФУНКЦИИ
;
;*******************************************************************************

;*******************************************************************************
;       EEPROM_READ
;-------------------------------------------------------------------------------
; Функция считывает из EEPROM значение для TMR0_user_max
;
;
; На выходе:
;       TMR0_user_max
;-------------------------------------------------------------------------------
EEPROM_READ:
    errorlevel -302
    bsf	    STATUS, RP0
    movlw   0x00			
    movwf   EEADR
    bsf	    EECON1, RD		    
    movf    EEDATA, W		    
    bcf	    STATUS, RP0
    bcf	    STATUS, RP1
    movwf   TMR0_user_max
    
    bsf	    STATUS, RP0
    movlw   0x01			
    movwf   EEADR
    bsf	    EECON1, RD		    
    movf    EEDATA, W		    
    bcf	    STATUS, RP0
    bcf	    STATUS, RP1
    movwf   TMR0_user_max_cool
    
    bsf	    Flags, 3		    ; установка флага - данные ОК
    bsf	    Flags, 4		    ; установка флага - EEPROM Ful
    
    return
;*******************************************************************************
;       EEPROM_WRITE
;-------------------------------------------------------------------------------
; Функция записывает данные TMR0_user_max в EEPROM
;
;
; На входе:
;       TMR0_user_max
;-------------------------------------------------------------------------------
EEPROM_WRITE:
    bsf	    STATUS, RP0
END_WR:
    btfsc   EECON1, WR		    ; проверить завершение операции записи
    goto    END_WR		    ;
    
    bsf	    EECON1, WREN
    movlw   0x00		
    movwf   EEADR
    
    bcf	    STATUS, RP0
    movf    TMR0_user_max, W
    
    bsf	    STATUS, RP0
    movwf   EEDATA
    movlw   0x55
    movwf   EECON2
    movlw   0xAA
    movwf   EECON2
    bsf	    EECON1, WR
    
END_WRC:
    btfsc   EECON1, WR		    ; проверить завершение операции записи
    goto    END_WRC
    
    
    bsf	    EECON1, WREN
    movlw   0x01		
    movwf   EEADR
    
    bcf	    STATUS, RP0
    movf    TMR0_user_max_cool, W
    
    bsf	    STATUS, RP0
    movwf   EEDATA
    movlw   0x55
    movwf   EECON2
    movlw   0xAA
    movwf   EECON2
    bsf	    EECON1, WR
    bcf	    STATUS, RP0
    return
;*******************************************************************************
;       Bin2Dec
;-------------------------------------------------------------------------------
; Функция переводит переменную work_data в массив комбинаций для вывода на
; 7-сегментный индиктор
;
; На выходе:
;       output_text
;-------------------------------------------------------------------------------
Bin2Dec:
    movlw   .100
    call    DivWreg                 ; wreg = сотни, work_data = work_data % 100
    call    GetSegments             ; Получаем соответсвующую комбинацию сегментов
    movwf   output_text + 2
    movlw   .10
    call    DivWreg                 ; wreg = десятки, work_data = work_data % 10
    call    GetSegments             ; Получаем соответсвующую комбинацию сегментов
    addlw   SEG_H		    ; У второго разряда зажигаем точку
    movwf   output_text + 1
    movf    work_data, w            ; wreg = единицы
    call    GetSegments             ; Получаем соответсвующую комбинацию сегментов
    movwf   output_text + 0
    return   ; Bin2Dec

;*******************************************************************************
;       DivWreg
;-------------------------------------------------------------------------------
; Деление двухбайтового числа на wreg методом вычитания
; Использует переменные:
;       div_result
; На входе:
;       work_data - делимое
;       wreg      - делитель
; На выходе:
;       wreg      - частное
;       work_data - остаток
;-------------------------------------------------------------------------------
DivWreg:
    clrf    div_result
DW_Loop:
    incf    div_result, f           ; Увеличиваем счетчик вычитаний
    subwf   work_data, f            ; Производим вычитиние делителя из делимого
    btfss   STATUS, C               ;
    decf    work_data+1, f          ;

    btfss   work_data+1, 7          ; Проверка на отрицательный результат (бит 15=1)
    goto    DW_Loop
;-----------------------------------------------------------------------
    addwf   work_data, f            ; Восстанавливаем остаток
    incf    work_data+1, f          ;

    decf    div_result, w           ; Возвращаем частное

    return

;*******************************************************************************
;       GetSegments
;-------------------------------------------------------------------------------
; Возвращает комбинацию сегментов 7-сегментного индикатора для конкретной цифры
; На входе:
;       wreg - число от 0 до 9
; Навыходе:
;       wreg - комбинация сегментов
;-------------------------------------------------------------------------------
    ORG     0x0400
GetSegments:
    clrf    PCLATH
    bsf     PCLATH, 2
    andlw   0xF
    addwf   PCL, f

    retlw   SEG_A + SEG_B + SEG_C + SEG_D + SEG_E + SEG_F           ; zero
    retlw   SEG_B + SEG_C                                           ; one
    retlw   SEG_A + SEG_B + SEG_D + SEG_E + SEG_G                   ; two
    retlw   SEG_A + SEG_B + SEG_C + SEG_D + SEG_G                   ; three
    retlw   SEG_B + SEG_C + SEG_F + SEG_G                           ; four
    retlw   SEG_A + SEG_C + SEG_D + SEG_F + SEG_G                   ; five
    retlw   SEG_A + SEG_C + SEG_D + SEG_E + SEG_F + SEG_G           ; six
    retlw   SEG_A + SEG_B + SEG_C                                   ; seven
    retlw   SEG_A + SEG_B + SEG_C + SEG_D + SEG_E + SEG_F + SEG_G   ; eight
    retlw   SEG_A + SEG_B + SEG_C + SEG_D + SEG_F + SEG_G           ; nine

    ; Дополняем таблицу до 16 значений, чтобы предотвратить
    ; улет неизвестно куда при неправильном значении wreg на входе
    retlw   0
    retlw   0
    retlw   0
    retlw   0
    retlw   0
    retlw   0

#include "Init.inc"
#include "Interrupt.inc"
#include "Memory.inc"
#include "Delay.inc"
#include "Delay150ms.inc"
    end