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
	
		    CBLOCK	    0x70
			    ADC_VALUE
			    DELAY1TMR0
			    DELAY2TMR0
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

		    
DELAY_TMR0	    BANKSEL	   ADCON0 
		    DECFSZ	   DELAY1TMR0	;SETEAR EN 255
		    RETURN
		    DECFSZ	   DELAY2TMR0	;SETEAR EN 5
		    RETURN
		    BSF		   ADCON0, 1	; INICIA LA CONVERSION
		    RETURN

LOAD_DISPLAY	    
		    
LOAD_DSPL1
LOAD_DSPL2
LOAD_DSPL3
LOAD_DSPL4
		    
ISR		    