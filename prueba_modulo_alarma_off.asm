;------------------------------------------------------------
; PROGRAMA: Medidor de CO - Prueba del modulo alarma - Trabajo Práctico Final Electrónica Digital 2
; ARCHIVO:  TPFinal.asm
; AUTORES:  Jerez, Facundo - Krede, Julián - Méndez Quiroga, Celeste Evangelina
; FECHA:    19/06/2025
; MICROCONTROLADOR: PIC16F887
; CLOCK: Oscilador XT (cristal externo) 4[MHz]
;
;------------------------------------------------------------

;--------------------------------------------------------------------------------------
; PRUEBA: EL SISTEMA INICIA CON VALOR DE ADC = 50D, EL VALOR DE UMBRAL = 75D EL
;	REGISTRO OPCIONES INICIA CON ALARMA ENCENDIDA
; RESULTADO ESPERADO: NO DEBE ACTIVARSE LA ALARMA
;--------------------------------------------------------------------------------------
		    
		    LIST P=16F887
		    #INCLUDE "P16F887.INC"
	
		    __CONFIG _CONFIG1, _XT_OSC & _WDTE_OFF & _MCLRE_ON & _LVP_OFF
	
;-------------------------
; VARIABLES DEL SISTEMA
;-------------------------
		    
; Variables en banco 0 (0x20-0x6F)
TEMP_VAL_ADC  EQU   0x23    ; Valor temporal del ADC para realizar la descomposicion
  
; Variables en banco común (0x70-0x7F)
VAL_UMBRAL_U  EQU   0x71    ; Unidades del umbral (0-9)
VAL_UMBRAL_D  EQU   0x72    ; Decenas del umbral (0-9)
VAL_UMBRAL_C  EQU   0x73    ; Centenas del umbral (0-9)
OPCIONES      EQU   0x77    ; Registro de opciones:
			     ;   BIT 0: Modo display (0=ADC, 1=Teclado)
			     ;   BIT 1: Estado alarma (0=NO DISPARADA, 1=DISPARADA)
			     ;	 BIT 2: Activar/Desactivar Alarma (0=OFF, 1=ON)
VAL_ADC_U     EQU   0x7C    ; Unidades del valor ADC (0-9)
VAL_ADC_D     EQU   0x7D    ; Decenas del valor ADC (0-9)
VAL_ADC_C     EQU   0x7E    ; Centenas del valor ADC (0-9)
VAL_ADC_M     EQU   0x7F    ; Unidades de mil del valor ADC
   
   
;-------------------------
; MACROS DE CONFIGURACIÓN
;-------------------------

;----------------------------------------------------------------
; CONFIGURACIÓN DE ALARMA
;----------------------------------------------------------------
; Configura RC0 como salida para controlar la alarma:
;   - Puede ser un LED, buzzer o relay
;   - Inicialmente apagada
;----------------------------------------------------------------
CONF_ALARMA		    MACRO
    BANKSEL	    TRISC
    BCF		    TRISC, 0     ; RC0 como salida

    BANKSEL	    PORTC
    BCF		    PORTC, 0     ; Apaga la alarma inicialmente

    ENDM

;-------------------------
; Código principal
;-------------------------
		    ORG		    0X00
		    GOTO	    INICIO
		    

		    
		    ORG		    0X05

INICIO		    ;INICIO PROGRAMA
		    CONF_TECLADO
		    CONF_DISPLAY
		    CONF_ALARMA
		    CONF_INTERRUPCION
		    
		    MOVLW	    .0		   
		    MOVWF	    VAL_ADC_C
		    MOVLW	    .5
		    MOVWF	    VAL_ADC_D
		    MOVLW	    .0
		    MOVWF	    VAL_ADC_U		; Carga el valor del adc con 50D
		    MOVLW	    .0	           
		    MOVWF	    VAL_UMBRAL_C
		    MOVLW	    .7	           
		    MOVWF	    VAL_UMBRAL_D
		    MOVLW	    .5	           
		    MOVWF	    VAL_UMBRAL_U	; Carga el valor del umbral con 75D
		    CLRF	    OPCIONES
		    BSF		    OPCIONES, 2		; Alarma activada
		    
REFRESH		    ;Polling de alarma		
		    CALL	    CHECK_ALARMA
		    CALL	    TEST_ALARMA
		    GOTO	    REFRESH
		    
;--------------------------
; Subrutinas de la alarma
;--------------------------
;----------------------------------------------------------
; CHECK_ALARMA: Verifica que el valor del adc sea menor
;		   que el del valor umbral, y en base a eso
;		   edita el bit 1 del registro opciones
;----------------------------------------------------------
CHECK_ALARMA
		    MOVF	    VAL_ADC_C, W
		    SUBWF	    VAL_UMBRAL_C, W	; VAL_ADC_C - VAL_UMBRAL_C
		    BTFSS	    STATUS, C		; Si C=0 (VAL_ADC_C < VAL_UMBRAL_C)
		    GOTO	    ALARMA_ON		; Centena ADC menor que umbral -> Activar

		    BTFSS	    STATUS, Z		; Si Z=1 (VAL_ADC_C = VAL_UMBRAL_C)
		    GOTO	    ALARMA_OFF		; Centena ADC mayor que umbral -> Desactivar

		    ; Si centenas iguales, comparar DECENAS
		    MOVF	    VAL_ADC_D, W
		    SUBWF	    VAL_UMBRAL_D, W	; VAL_ADC_D - VAL_UMBRAL_D
		    BTFSS	    STATUS, C		; Si C=0 (VAL_ADC_D < VAL_UMBRAL_D)
		    GOTO	    ALARMA_ON		; Decena ADC menor que umbral -> Activar

		    BTFSS	    STATUS, Z		; Si Z=1 (VAL_ADC_D = VAL_UMBRAL_D)
		    GOTO	    ALARMA_OFF		; Decena ADC mayor que umbral -> Desactivar

		    ; Si decenas iguales, comparar UNIDADES
		    MOVF	    VAL_ADC_U, W
		    SUBWF	    VAL_UMBRAL_U, W	; VAL_ADC_U - VAL_UMBRAL_U
		    BTFSS	    STATUS, C		; Si C=0 (VAL_ADC_U < VAL_UMBRAL_U)
		    GOTO	    ALARMA_ON		; Unidad ADC menor que umbral -> Activar
		    GOTO	    ALARMA_OFF		; Unidad ADC mayor que umbral -> Desactivar

ALARMA_ON	    BSF		    OPCIONES, 1		; Setear bit de estado de alarma
		    RETURN

ALARMA_OFF	    BCF		    OPCIONES, 1		; Limpiar bit de estado de alarma		
		    RETURN
		    
; Togglea entre alarma activada y desactivada
TEST_ALARMA	    
		    BANKSEL	    PORTC
		    BTFSC	    OPCIONES, 2		;Verifica que la alarma este activada
		    CALL   	    VERIFICAR_ALARMA	;Si esta activada entonces verifica el valor del estado de la alarma
		    BTFSS	    OPCIONES, 2		
		    BCF		    PORTC, 0		;Si no esta activada entonces ignora todo y desactiva la alarma
		    RETURN
		    
;Verifica el estado de la alarma y en base al estado activa o desactiva la alarma
VERIFICAR_ALARMA    
		    BTFSC	    OPCIONES, 1	
		    BSF		    PORTC, 0
		    BTFSS	    OPCIONES, 1
		    BCF		    PORTC, 0
		    RETURN
		    
		    END





