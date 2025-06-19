;------------------------------------------------------------
; PROGRAMA: Medidor de CO - Prueba del modulo display - Trabajo Práctico Final Electrónica Digital 2
; ARCHIVO:  TPFinal.asm
; AUTORES:  Jerez, Facundo - Krede, Julián - Méndez Quiroga, Celeste Evangelina
; FECHA:    19/06/2025
; MICROCONTROLADOR: PIC16F887
; CLOCK: Oscilador XT (cristal externo) 4[MHz]
;
;------------------------------------------------------------
		    
		    LIST P=16F887
		    #INCLUDE "P16F887.INC"
	
		    __CONFIG _CONFIG1, _XT_OSC & _WDTE_OFF & _MCLRE_ON & _LVP_OFF
	
;-------------------------
; VARIABLES DEL SISTEMA
;-------------------------
		    
; Variables en banco 0 (0x20-0x6F)
DELAY1DSPL    EQU   0x20    ; Contador delay para displays (primer nivel)
DELAY2DSPL    EQU   0x21    ; Contador delay para displays (segundo nivel)
DELAY3DSPL    EQU   0x22    ; Contador delay para displays (tercer nivel)
TEMP_VAL_ADC  EQU   0x23    ; Valor temporal del ADC para realizar la descomposicion

; Variables en banco común (0x70-0x7F)
VAL_ADC       EQU   0x70    ; Valor crudo del ADC (0-255)
VAL_ADC_U     EQU   0x7C    ; Unidades del valor ADC (0-9)
VAL_ADC_D     EQU   0x7D    ; Decenas del valor ADC (0-9)
VAL_ADC_C     EQU   0x7E    ; Centenas del valor ADC (0-9)
VAL_ADC_M     EQU   0x7F    ; Unidades de mil del valor ADC
   
   
;----------------------------------------------------------------
; CONFIGURACIÓN DE DISPLAYS 7 SEGMENTOS
;----------------------------------------------------------------
; Configura los puertos para controlar 4 displays de 7 segmentos:
;   - PORTD como salida (segmentos a-g)
;   - RA0-RA3 como salidas (control de displays)
;   - Configura AN7 como entrada analógica para el sensor
;----------------------------------------------------------------
CONF_DISPLAY	    MACRO
    BANKSEL	    ANSEL
    MOVLW	    B'10000000'  ; AN7 como analógico, demás pines como digital
    MOVWF	    ANSEL

    BANKSEL	    TRISD
    CLRF	    TRISD        ; PORTD completo como salida (segmentos)

    MOVLW	    B'11110000'  ; RA0-RA3 como salidas (control displays)
    MOVWF	    TRISA        ; RA4-RA7 como entradas (no usadas)
    ENDM

;-------------------------
; Código principal
;-------------------------
		    ORG		    0X00
		    GOTO	    INICIO
		    
		    ORG		    0X05

INICIO		    ;INICIO PROGRAMA

		    CONF_DISPLAY

		    MOVLW	    .84		   ; numero a mostrar
		    MOVWF	    VAL_ADC
REFRESH		    
		    CALL	    SHOW_ADC_DISPLAY
		    GOTO	    REFRESH
		    

;-------------------------- 
; Subrutinas del display
;--------------------------
;----------------------------------------------------------
; DESCOMP_VAL_ADC: Descompone valor ADC (0-255) en dígitos
;   Entrada: VAL_ADC (valor a descomponer)
;   Salida:  VAL_ADC_U (unidades)
;            VAL_ADC_D (decenas)
;            VAL_ADC_C (centenas)
;            VAL_ADC_M (unidades de mil, siempre 0)
;   Altera:  W, STATUS
;----------------------------------------------------------

DESCOMP_VAL_ADC	    BCF		    STATUS, RP0
		    BCF		    STATUS, RP1
		    MOVF	    VAL_ADC, W
		    MOVWF	    TEMP_VAL_ADC    ;GUARDA UNA COMPIA DE VAL_ADC
		    CLRF	    VAL_ADC_U	    ;UNIDAD DEL VALOR A MOSTRAR
		    CLRF	    VAL_ADC_D	    ;UNIDAD DE DECENA DEL VALOR A MOSTRAR
		    CLRF	    VAL_ADC_C	    ;UNIDAD DE CENTENA DEL VALOR A MOSTRAR
		    CLRF	    VAL_ADC_M	    ;NUNCA VALE DISTINTO DE CERO
		    
TEST_C		    MOVLW	    .100	    ;CALCULA LA CENTENA
		    SUBWF	    TEMP_VAL_ADC, F
		    BTFSC	    STATUS,C
		    GOTO	    ADD_C
		    
TEST_D		    MOVLW	    .10		    ;CALCULA LA DECENA
		    SUBWF	    TEMP_VAL_ADC, F
		    BTFSC	    STATUS,C
		    GOTO	    ADD_D
		    
TEST_U		    MOVLW	    .1		    ;CALCULA LA UNIDAD
		    SUBWF	    TEMP_VAL_ADC, F
		    BTFSC	    STATUS,C
		    GOTO	    ADD_U
		    RETURN	

ADD_C		    INCF	    VAL_ADC_C, F
		    GOTO	    TEST_C

ADD_D		    INCF	    VAL_ADC_D, F
		    GOTO	    TEST_D	   

ADD_U		    INCF	    VAL_ADC_U, F
		    GOTO	    TEST_U
	
;------------------------------------------------
; SHOW_ADC_DISPLAY: Muestra valor ADC en displays
;   Usa: VAL_ADC_U, VAL_ADC_D, VAL_ADC_C
;   Requiere: TABLA_DSPL inicializada
;   Tiempo: ~20ms (4 displays * 5ms)
;------------------------------------------------
SHOW_ADC_DISPLAY    CALL	    DESCOMP_VAL_ADC
		    
		    BANKSEL	    PORTA	    ;BANCO 0
		    MOVLW	    B'00000001'
		    MOVWF	    PORTA	    ;ACTIVA DISPLAY 1 Y DESACTIVA LOS DEMAS
		    MOVF	    VAL_ADC_U, W
		    CALL	    TABLA_DSPL	    
		    MOVWF	    PORTD	    ;CARGA EL VALOR DE LA UNIDAD EN EL DISPLAY CON SU CORRESPONDIENTE FORMATO
		    CALL	    DELAY5ms
		    
		    MOVLW	    B'00000010'
		    MOVWF	    PORTA	    ;ACTIVA DISPLAY 2 Y DESACTIVA LOS DEMAS
		    MOVF	    VAL_ADC_D, W
		    CALL	    TABLA_DSPL
		    MOVWF	    PORTD	    ;CARGA EL VALOR DE LA DECENA EN EL DISPLAY CON SU CORRESPONDIENTE FORMATO
		    CALL	    DELAY5ms
		    
		    MOVLW	    B'00000100'
		    MOVWF	    PORTA	    ;ACTIVA DISPLAY 3 Y DESACTIVA LOS DEMAS
		    MOVF	    VAL_ADC_C, W
		    CALL	    TABLA_DSPL
		    MOVWF	    PORTD	    ;CARGA EL VALOR DE LA CENTENA EN EL DISPLAY CON SU CORRESPONDIENTE FORMATO
		    CALL	    DELAY5ms
		    
		    MOVLW	    B'00001000'
		    MOVWF	    PORTA	    ;ACTIVA DISPLAY 4 Y DESACTIVA LOS DEMAS
		    MOVF	    VAL_ADC_M, W
		    CALL	    TABLA_DSPL
		    MOVWF	    PORTD	    ;CARGA EL VALOR DE LA UNIDAD DE MIL EN EL DISPLAY CON SU CORRESPONDIENTE FORMATO
		    CALL	    DELAY5ms
		    RETURN
		    
		    ;DELAY DE 5ms USADO PARA MOSTRAR VALORES EN DISPLAY (SHOW_DISPLAY)
DELAY5ms	    BCF		    STATUS, RP0
		    BCF		    STATUS, RP1	    ;BANCO 0
		    MOVLW	    .5		    ;m=5
		    MOVWF	    DELAY1DSPL
DELAY1		    MOVLW	    .5		    ;n=5
		    MOVWF	    DELAY2DSPL
DELAY2		    MOVLW	    .65		    ;p=65
		    MOVWF	    DELAY3DSPL
DELAY3		    DECFSZ	    DELAY3DSPL, F
		    GOTO	    DELAY3
		    DECFSZ	    DELAY2DSPL, F
		    GOTO	    DELAY2
		    DECFSZ	    DELAY1DSPL, F
		    GOTO	    DELAY1
		    RETURN
		    

		    ;TABLA CON PARA ENCENDER LOS SEGMENTOS CORRESPONDIENTES A CATODO COMUN (LOGICA POSITIVA)
TABLA_DSPL	    ADDWF	    PCL, F
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

		    

		    END
