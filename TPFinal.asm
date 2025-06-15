;------------------------------------------------------------
; Programa : Medidor de CO - Trabajo Practico Final Electronica Digital 2
; Archivo  : TPFinal.asm
; Autores: Jerez, Facundo - Krede Julian - Mendez Quiroga, Celeste Evangelina
; Fecha    : 19/06/2025
; Descripción: Este programa lee la señal analogica de un sensor de CO, muestra el
;              valor en 4 displays de 7 segmentos y activa una alarma si supera
;              un umbral, fijado previamente mediante un teclado.
;------------------------------------------------------------

		    LIST P=16F887
		    #INCLUDE "P16F887.INC"
	
;-------------------------
; Definiciones y macros
;-------------------------
		    
		    __CONFIG _CONFIG1, _XT_OSC & _WDTE_OFF & _MCLRE_ON & _LVP_OFF
	
		    
		;    VAL_U EQU 0X20 USAR ESTAS POSICIONES SI NO PUEDE EN LOS 16 BYES COMPARTIDOS
		;    VAL_D EQU 0X21
		;    VAL_C EQU 0X22 
		;    VAL_M EQU 0X23
		    CBLOCK	    0x70
			    
			    VAL_ADC
			    VAL_U
			    VAL_D
		   	    VAL_C
		    	    VAL_M
			    
			    COUNT_DISPLAY
			    
			    DELAY1TMR0
			    DELAY2TMR0
			    
			    DELAY1DSPL
			    DELAY2DSPL
			    DELAY3DSPL
			    
			    W_TEMP
			    STATUS_TEMP
		    ENDC

		;MACROS DE CONFIGURACION
CONF_TECLADO	    MACRO
	    
		    BANKSEL	    ANSELH
		    CLRF	    ANSELH	    ; PUERTO B COMO PIN DIGITAL
		    MOVLW	    B'01110000'  
		    MOVWF	    TRISB	    ; <RB0-RB3> SALIDAS <RB4-RB6> ENTRADAS
		    BCF		    OPTION_REG,7
		    BANKSEL	    WPUB	    
		    MOVLW	    0XFF
		    MOVWF	    WPUB	    ; RESISTENCIAS PULL-UPS ACTIVADAS
		    
		    ENDM
		    
CONF_DISPLAY	    MACRO
	    
		    BANKSEL	    ANSEL
		    MOVLW	    B'10000000' 
		    MOVWF	    ANSEL	    ;AN7 ENTRADA ANALOGICA, LOS DEMAS PINES COMO ENTRADAS DIGITALES
		    BANKSEL	    TRISD
		    CLRF	    TRISD	    ;<RD0-RD7> COMO SALIDAS (BUS DE DATOS DEL DISPLAY)
		    MOVLW	    B'11110000'
		    MOVWF	    TRISA	    ;<RA0-RA3> COMO SALIDAS (BUS DE CONTROL DEL DISPLAY)
		    
		    ENDM
		    
CONF_ALARMA	    MACRO
		    
		    BANKSEL	    TRISC
		    BCF		    TRISC, 0	    ;RC0 COMO SALIDA (CONTROL DE ALARMA)
		    BANKSEL	    PORTC
		    BCF		    PORTC, 0	    ;DESACTIVA LA ALARMA PREVENTIVAMENTE
		    
		    ENDM
		    
		    ;(PIN 10 (RE2/AN7) PREVIAMENTE CONFIGURADO COMO ANALOGICO EN CONF_DISPLAY)
CONF_ADC	    MACRO
		    
		    BANKSEL	    TRISE
		    BSF		    TRISE, RE2	    ;RE2 COMO ENTRADA
		    BANKSEL	    ADCON1
		    CLRF	    ADCON1	    ;UTILIZA LAS TENSIONES DE REFERENCIAS INTERNAS DEL PIC Y JUSTICIA HACIA LA IZQUIERDA (VALOR EN ADRESH)
		    BANKSEL	    ADCON0
		    MOVLW	    B'01011101'	    
		    MOVWF	    ADCON0	    ;CLOCK DE CONVERSION FCLK/8 - CANAL ANALOGICO SELECCIONADO: AN7  - ADC ACTIVADO
		    
		    ENDM
		    
		    ;EL MODULO TIMER0 FUNCIONA DE MANERA AUXILIAR AL ADC, POR LO TANTO DEBEN SER CARGADAS SIMULTANEAMENTE
CONF_TIMER0	    MACRO
		    BSF		    STATUS,RP0
		    BSF		    STATUS,RP1
		    MOVLW	    B'01010111'
		    MOVWF	    OPTION_REG	    ;TMR0 CON CLOCK INTERNO Y PS = 1:256
		    MOVLW	    B'10101000'	    
		    MOVWF	    INTCON	    ;INTERRUPCION POR TMR0 Y PUERTO RB ACTIVADO
		    BANKSEL	    TMR0
		    MOVLW	    .61
		    MOVWF	    TMR0	    ;PRECARGAR CON 61D PARA TENER INTERRUPCION CADA 50ms
		    
		    ENDM
	
CONF_COMUNICACION   MACRO
		    BANKSEL	    TXSTA
		    MOVLW	    B'00100100'
		    MOVWF	    TXSTA	    ;Habilita el transmisor Y BRGH modo alta velocidad, recomendada
		    MOVLW	    .25
		    MOVWF	    SPBRG	    ;SPBRG = D'25' (BAUD-RATE = 9600 BAUDIOS)
		    BANKSEL	    RCSTA	    
		    MOVLW	    B'10010000'	    
		    MOVWF	    RCSTA	    ;Habilita recepción y puerto serial
		    BANKSEL	    BAUDCTL
		    BCF		    BAUDCTL, 3	    ;BAUD GENERATOR = 8 BITS
		    
		    ENDM
	
;-------------------------
; Código principal
;-------------------------
		    ORG 0X00
		    GOTO INICIO
		    
		    ORG 0X04
		    GOTO ISR
		    
		    ORG 0X05

INICIO		    ;INICIO PROGRAMA
		    CONF_TECLADO
		    CONF_DISPLAY
		    CONF_ALARMA
		    CONF_ADC
		    CONF_TIMER0
		    CONF_COMUNICACION

		    
DELAY_TMR0	    BANKSEL	    ADCON0
		    DECFSZ	    DELAY1TMR0	    ;SETEAR EN 255
		    RETURN
		    DECFSZ	    DELAY2TMR0	    ;SETEAR EN 5
		    RETURN
		    BSF		    ADCON0, 1	    ; INICIA LA CONVERSION
		    RETURN

CALC_DISPLAY	    BANKSEL	    ADRESH
		    MOVF	    ADRESH, W
		    MOVWF	    VAL_ADC	    ;MUEVO EL VALOR OBTENIDO POR EL ADC A ESTA VARIABLE TEMPORAL
		    
		    CLRF	    VAL_U	    ;UNIDAD DEL VALOR A MOSTRAR
		    CLRF	    VAL_D	    ;UNIDAD DE DECENA DEL VALOR A MOSTRAR
		    CLRF	    VAL_C	    ;UNIDAD DE CENTENA DEL VALOR A MOSTRAR
		    CLRF	    VAL_M	    ;NUNCA VALE DISTINTO DE CERO
		    
TEST_C		    MOVLW	    .100
		    SUBWF	    VAL_ADC,F
		    BTFSC	    STATUS,C
		    GOTO	    ADD_C
		    GOTO	    TEST_D
		    
TEST_D		    MOVLW	    .10
		    SUBWF	    VAL_ADC,F
		    BTFSC	    STATUS,C
		    GOTO	    ADD_D
		    GOTO	    TEST_U	

TEST_U		    MOVLW	    .1
		    SUBWF	    VAL_ADC,F
		    BTFSC	    STATUS,C
		    GOTO	    ADD_U
		    RETURN	
		  
		    
ADD_C		    INCF	    VAL_C
		    GOTO	    TEST_C

ADD_D		    INCF	    VAL_D
		    GOTO	    TEST_D	   
		    
ADD_U		    INCF	    VAL_U
		    GOTO    	    TEST_U

		  
		  
SHOW_DISPLAY1	    BANKSEL	    PORTA
		    MOVLW	    B'00000001'
		    MOVWF	    PORTA
		    MOVF	    VAL_U
		    CALL	    TABLA_DSPL
		    MOVWF	    PORTD
		    CALL	    DELAY5ms
		    GOTO	    SHOW_DISPLAY2
		    
SHOW_DISPLAY2	    BANKSEL	    PORTA
		    MOVLW	    B'00000010'
		    MOVWF	    PORTA
		    MOVF	    VAL_D
		    CALL	    TABLA_DSPL
		    MOVWF	    PORTD
		    CALL	    DELAY5ms
		    GOTO	    SHOW_DISPLAY3

SHOW_DISPLAY3	    BANKSEL	    PORTA
		    MOVLW	    B'00000100'
		    MOVWF	    PORTA
		    MOVF	    VAL_C
		    CALL	    TABLA_DSPL
		    MOVWF	    PORTD
		    CALL	    DELAY5ms
		    GOTO	    SHOW_DISPLAY4
		    
SHOW_DISPLAY4	    BANKSEL	    PORTA
		    MOVLW	    B'00001000'
		    MOVWF	    PORTA
		    MOVF	    VAL_M
		    CALL	    TABLA_DSPL
		    MOVWF	    PORTD
		    CALL	    DELAY5ms
		    GOTO	    SHOW_DISPLAY1
		    
DELAY5ms	    MOVLW	    .5		    ;m=5
		    MOVWF	    DELAY1DSPL
DELAY1		    MOVLW	    .5		    ;n=5
		    MOVWF	    DELAY2DSPL
DELAY2		    MOVLW	    .65		    ;p=65
		    MOVWF	    DELAY3DSPL
DELAY3		    DECFSZ	    DELAY3DSPL
		    GOTO	    DELAY3
		    DECFSZ	    DELAY2DSPL
		    GOTO	    DELAY2
		    DECFSZ	    DELAY1DSPL
		    GOTO	    DELAY1
		    RETURN
		    
TABLA_DSPL
		    ADDWF	    PCL, F
		    RETLW	    0X3F	    ;0
		    RETLW	    0X06	    ;1
		    RETLW	    0X5B	    ;2
		    RETLW	    0X4F	    ;3
		    RETLW	    0X66	    ;4
		    RETLW	    0X6D	    ;5
		    RETLW	    0X7D	    ;6
		    RETLW	    0X07	    ;7
		    RETLW	    0X7F	    ;8
		    RETLW	    0X67	    ;9   
		    
ISR		    ;SALVADO DE CONTEXTO
		    MOVWF	    W_TEMP
		    SWAPF	    STATUS, W
		    MOVWF	    STATUS_TEMP
		    ;INICIO ISR			;FUENTES DE INTERRUPCION: PUERTO B, TIMER0
		    ;
		    ;
		    ;
		    ;
		    ;
		    ;RECUPERACION DE CONTEXTO
		    SWAPF	    STATUS_TEMP, W
		    MOVWF	    STATUS
		    SWAPF	    W_TEMP, F
		    SWAPF	    W_TEMP, F