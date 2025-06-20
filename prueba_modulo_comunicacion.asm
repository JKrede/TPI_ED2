;------------------------------------------------------------
; PROGRAMA: Medidor de CO - Prueba del modulo de comunicacion - Trabajo Práctico Final Electrónica Digital 2
; ARCHIVO:  TPFinal.asm
; AUTORES:  Jerez, Facundo - Krede, Julián - Méndez Quiroga, Celeste Evangelina
; FECHA:    19/06/2025
; MICROCONTROLADOR: PIC16F887
; CLOCK: Oscilador XT (cristal externo) 4[MHz]
;
;------------------------------------------------------------

		    
;----------------------------------------------------------------------
; PRUEBA: INICIAR EL SISTEMA Y ENVIAR MEDIANTE EL MODULO UART EL NUMERO
;	141D DE MANERA CONTINUA AL PC
; RESULTADO ESPERADO: EN EL RECEPTOR (PC) DEBE OBSERVARSE LA LLEGA DE
;	MANERA CONTINUA DEL DATO 141D
;----------------------------------------------------------------------
    

		    LIST P=16F887
		    #INCLUDE "P16F887.INC"
	
		    __CONFIG _CONFIG1, _XT_OSC & _WDTE_OFF & _MCLRE_ON & _LVP_OFF

;-------------------------
; VARIABLES DEL SISTEMA
;-------------------------

; Variables en banco común (0x70-0x7F)
W_TEMP        EQU   0x7A    ; Backup de W durante ISR
STATUS_TEMP   EQU   0x7B    ; Backup de STATUS durante ISR
VAL_ADC_U     EQU   0x7C    ; Unidades del valor ADC (0-9)
VAL_ADC_D     EQU   0x7D    ; Decenas del valor ADC (0-9)
VAL_ADC_C     EQU   0x7E    ; Centenas del valor ADC (0-9)
VAL_ADC_M     EQU   0x7F    ; Unidades de mil del valor ADC
   
   
;-------------------------
; MACROS DE CONFIGURACIÓN
;-------------------------


;----------------------------------------------------------------
; CONFIGURACIÓN DE COMUNICACIÓN SERIAL (UART)
;----------------------------------------------------------------
; Configura el módulo EUSART para transmisión serial:
;   - Baud rate 9600 (para 4MHz y BRGH=1)
;   - Solo transmisión (no recepción)
;   - 8 bits de datos, sin paridad
;----------------------------------------------------------------
CONF_COMUNICACION	    MACRO
    BANKSEL	    TXSTA
    MOVLW	    B'00100100'  ; BRGH=1 (alta velocidad), TXEN=1 (habilitar TX)
    MOVWF	    TXSTA

    MOVLW	    .25          ; SPBRG = 25 para 9600 bauds (4MHz, BRGH=1)
    MOVWF	    SPBRG

    BANKSEL	    RCSTA
    MOVLW	    B'10010000'  ; puerto serial habilitado, 8-bit recepcion
    MOVWF	    RCSTA

    BANKSEL	    BAUDCTL
    BCF		    BAUDCTL, 3   ; BAUD Generator = 8 bits

    ENDM
	
;-------------------------
; Código principal
;-------------------------
		    ORG		    0X00
		    GOTO	    INICIO
		    
		    ORG		    0X05

INICIO		    ;INICIO PROGRAMA

		    CONF_COMUNICACION

		    MOVLW	    .1
		    MOVWF	    VAL_ADC_C
		    MOVWF	    VAL_ADC_U
		    MOVLW	    .4
		    MOVWF	    VAL_ADC_D	    ;VALOR A ENVIAR: 141D
		    
REFRESH		    CALL	    ENVIAR_INFO
		    GOTO	    REFRESH
		    
;------------------------------------
; Subrutinas de comunicacion serial
;------------------------------------

;--------------------------------------------------------------
;ENVIAR_INFO: Realiza el envio del valor almacenado en la 
;	variable VAL_ADC codificada en codigo ASCII, con salto
;	de linea y retorno de carro incluido entre valores
;--------------------------------------------------------------
ENVIAR_INFO	    ;CALL	    DESCOMP_VAL_ADC
		    MOVF	    VAL_ADC_C, W
		    ADDLW	    .48
		    CALL	    UART_TX	    ;ENVIA CENTENA
		    
		    MOVF	    VAL_ADC_D, W
		    ADDLW	    .48
		    CALL	    UART_TX	    ;ENVIA DECENA

		    MOVF	    VAL_ADC_U, W
		    ADDLW	    .48
		    CALL	    UART_TX	    ;ENVIA UNIDAD
		    
		    MOVLW	    .10
		    CALL	    UART_TX	    ;SALTO DE LINEA
		    
		    MOVLW	    .13
		    CALL	    UART_TX	    ;RETORNO DE CARRO
		    RETURN
		    
		    
UART_TX		    
		    BANKSEL	    TXSTA
		    BTFSS	    TXSTA, TRMT	    ;VERIFICA QUE EL TSR ESTE VACIO
		    GOTO	    UART_TX
		    BANKSEL	    TXREG
		    MOVWF	    TXREG	    ;LUEGO DE ESTO SE ENVIA SOLO... EN TEORIA
		    RETURN
	    
		    END