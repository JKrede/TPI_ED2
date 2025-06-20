;------------------------------------------------------------
; PROGRAMA: Medidor de CO - Prueba del modulo conversor analogico digital - Trabajo Práctico Final Electrónica Digital 2
; ARCHIVO:  TPFinal.asm
; AUTORES:  Jerez, Facundo - Krede, Julián - Méndez Quiroga, Celeste Evangelina
; FECHA:    19/06/2025
; MICROCONTROLADOR: PIC16F887
; CLOCK: Oscilador XT (cristal externo) 4[MHz]
;
;------------------------------------------------------------

;-------------------------------------------------------------------------------------------
; PRUEBA: INICIAR EL SISTEMA Y VERIFICAR EL CORRECTO FUNCIONAMIENTO DEL ADC Y EL SENSOR
;	MEDIANTE EL SUBSISTEMA DE VISUALIZACION DE DATOS (DISPLAYS 7 SEGMENTOS)
; RESULTADO ESPERADO: AL VARIAR LOS VALORES EN LOS TIEMPOS CORRECTOS, DEBEN VARIAR
;	LAS CONVERSIONES REALIZADAS
;-------------------------------------------------------------------------------------------
		    
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

DELAY1TMR0    EQU   0x78    ; Contador delay para ADC (primer nivel)
DELAY2TMR0    EQU   0x79    ; Contador delay para ADC (segundo nivel)
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

;----------------------------------------------------------------
; CONFIGURACIÓN DEL MÓDULO ADC
;----------------------------------------------------------------
; Configura el ADC para leer el sensor de CO:
;   - Canal AN7 (RE2) como entrada analógica
;   - Voltaje de referencia interno (VDD y VSS)
;   - Justificación a la izquierda (8 bits MSB en ADRESH)
;   - Frecuencia de reloj Fosc/8
;   - Inicializa contadores de delay para primera lectura
;----------------------------------------------------------------
CONF_ADC		    MACRO
    BANKSEL	    TRISE
    BSF		    TRISE, RE2   ; RE2 como entrada (AN7)

    BANKSEL	    ADCON1
    CLRF	    ADCON1       ; Voltaje referencia VDD/VSS, justificación izquierda

    BANKSEL	    ADCON0
    MOVLW	    B'01011101'  ; Fosc/8, canal AN7, ADC encendido
    MOVWF	    ADCON0

    ;Contadores inicializados en 0 para que se realice una conversion inmediata
    ;al encender el sistema
    CLRF	    DELAY1TMR0   
    CLRF	    DELAY2TMR0  

    ENDM

;----------------------------------------------------------------
; CONFIGURACIÓN DEL TIMER0
;----------------------------------------------------------------
; Configura Timer0 para generar interrupciones periódicas:
;   - Reloj interno (Fosc/4)
;   - Prescaler 1:256
;   - Valor inicial 61 para interrupciones cada ~50ms
;   (Considerando oscilador de 4MHz: 256*(256-61)*1?s ? 50ms)
;----------------------------------------------------------------
CONF_TIMER0		     MACRO
    BANKSEL	    OPTION_REG
    MOVLW	    B'01010111'  ; Internal clock, prescaler 1:256 asignado a TMR0
    MOVWF	    OPTION_REG

    BANKSEL	    TMR0
    MOVLW	    .61          ; Valor inicial para ~50ms (ajustar según Fosc)
    MOVWF	    TMR0

    ENDM
;----------------------------------------------------------------
; CONFIGURACIÓN DE INTERRUPCIONES
;----------------------------------------------------------------
; Habilita las interrupciones globales y específicas:
;   - Interrupciones por cambio en PORTB (teclado)
;   - Interrupciones por desborde del Timer0
;   - Configura IOCB para interrupciones solo en RB4-RB6
;----------------------------------------------------------------
CONF_INTERRUPCION	    MACRO
    MOVLW	    B'10101000'  ; Habilita: INTCON.GIE, INTCON.T0IE, INTCON.RBIE
    MOVWF	    INTCON

    BANKSEL	    IOCB
    MOVLW	    B'01110000'  ; Habilita interrupciones solo en RB4-RB6
    MOVWF	    IOCB

    ENDM
	
;-------------------------
; Código principal
;-------------------------
		    ORG		    0X00
		    GOTO	    INICIO
		    
		    ORG		    0X04
		    GOTO	    ISR
		    
		    ORG		    0X05

INICIO		    ;INICIO PROGRAMA
		    CONF_DISPLAY
		    CONF_ADC
		    CONF_TIMER0
		    CONF_INTERRUPCION
		    CLRF	    VAL_ADC
REFRESH		    
		    CALL	    SHOW_ADC_DISPLAY

		    GOTO	    REFRESH
		    

;-------------------------- 
; Subrutinas del display
;--------------------------
		    
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
		    BCF		    STATUS, RP1	    ;BANCO DE LOS VALORES DE TEMP_ADC 
		    MOVF	    VAL_ADC, W
		    MOVWF	    TEMP_VAL_ADC    ;GUARDA UNA COMPIA DE VAL_ADC
		    CLRF	    VAL_ADC_U	    ;UNIDAD DEL VALOR A MOSTRAR
		    CLRF	    VAL_ADC_D	    ;UNIDAD DE DECENA DEL VALOR A MOSTRAR
		    CLRF	    VAL_ADC_C	    ;UNIDAD DE CENTENA DEL VALOR A MOSTRAR
		    CLRF	    VAL_ADC_M	    ;UNIDAD DE MIL DEL VALOR A MOSTRAR (NO IMPLEMENTADO)
		    
TEST_C		    MOVLW	    .100	    ;CALCULA LA CENTENA
		    SUBWF	    TEMP_VAL_ADC, F
		    BTFSC	    STATUS, C	    ;VERIFICA LA RESTA NO PRODUZCA QUE EL NUMERO SE VUELVA NEGATIVO (SOLO SE DA CUANDO LA CENTENA ES 0)
		    GOTO	    ADD_C
		    MOVLW	    .100          
		    ADDWF	    TEMP_VAL_ADC, F ;SI ES NEGATIVO RECUPERA EL VALOR
		    GOTO	    TEST_D 
		    
TEST_D		    MOVLW	    .10		    ;CALCULA LA DECENA
		    SUBWF	    TEMP_VAL_ADC, F
		    BTFSC	    STATUS, C	    ;VERIFICA LA RESTA NO PRODUZCA QUE EL NUMERO SE VUELVA NEGATIVO (SOLO SE DA CUANDO LA DECENA ES 0)
		    GOTO	    ADD_D
		    MOVLW	    .10	    	        
		    ADDWF	    TEMP_VAL_ADC, F ;SI ES NEGATIVO RECUPERA EL VALOR 
		    GOTO	    TEST_U
		    
TEST_U		    MOVLW	    .1		    ;CALCULA LA UNIDAD
		    SUBWF	    TEMP_VAL_ADC, F
		    BTFSC	    STATUS, C	    ;VERIFICA LA RESTA NO PRODUZCA QUE EL NUMERO SE VUELVA NEGATIVO (SOLO SE DA CUANDO LA UNIDAD ES 0)
		    GOTO	    ADD_U
		    MOVLW	    .1		            
		    ADDWF	    TEMP_VAL_ADC, F ;SI ES NEGATIVO RECUPERA EL VALOR 
		    
		    RETURN	

ADD_C		    INCF	    VAL_ADC_C, F
		    GOTO	    TEST_C

ADD_D		    INCF	    VAL_ADC_D, F
		    GOTO	    TEST_D	   

ADD_U		    INCF	    VAL_ADC_U, F
		    GOTO	    TEST_U
		    
;-----------------------------------------------------------------
; DELAY5ms: retardo por software de 5ms usado para multiplexacion
;	de displays 7 segmentos
; Usado en: SHOW_ADC_DISPLAY
;-----------------------------------------------------------------
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

		    

;-------------------------
; Subrutinas del ADC
;-------------------------	    

;------------------------------------------------------------------------
; ISR_ADC: Realiza adquision y conversion de la señal analogica de 
;	manera periodica utilizando el modulo Timer 0 configurado para
;	realizar interrupcion cada 50ms, periodo que se extiende
;	63,75s utilizando dos variables auxiliares DELAY1TMR0 y 
;	DELAY2TMR0.
;   Formula: tmuestreo = 50ms * DELAY1TMR0 * DELAY2TMR0
;	!IMPORTANTE! Para realizar esta prueba DELAY1TMR0 = 10D, 
;	DELAY2TMR0 = 2D -> tmuestreo = 1s
;-------------------------------------------------------------------------
ISR_ADC		    
		    BANKSEL	    ADCON0
		    MOVLW	    .61
		    MOVWF	    TMR0	    ;RECARGA EL VALOR INICIAL DE TMR0 CON 61
		    DECFSZ	    DELAY1TMR0, F
		    GOTO	    FIN_ISR_ADC	    
		    GOTO	    TEST_DELAY2	    ;SI DELAY1TMR0 LLEGO A 0 VA A TESTEAR EL VALOR DE DELAY2TMR0 
		    
		    ;SUBRUTINAS COMPLEMENTARIAS DE ISR_ADC
TEST_DELAY2	    
		    DECFSZ	    DELAY2TMR0, F
		    GOTO	    RECARGA_D1TMR0  ;SI DELAY2TMR0 NO LLEGO A 0 RECARGA DELAY1TMR0 Y SE VA A FIN_ISR_ADC
		    CALL	    READ_ADC	    ;SI DELAY1TMR0 LLEGO A 0 VA A LA SUBRUTINA QUE HACE LA CONVERSION
		    GOTO	    FIN_ISR_ADC	    ;LUEGO DE HACER LA CONVERSION SE VA A FIN_ISR_ADC

RECARGA_D1TMR0	    
		    MOVLW	    .10
		    MOVWF	    DELAY1TMR0
		    GOTO	    FIN_ISR_ADC

;--------------------------------------------------------------------
; READ_ADC: Realiza el proceso de adquision mediante el ADC del PIC
;	Esta subrutina es disparada en ISR_ADC.
;	Aqui se recarga los valores de DELAY1TMR0 y DELAY2TMR0 
;--------------------------------------------------------------------
READ_ADC	    
		    BANKSEL	    ADCON0
		    BSF		    ADCON0, 1	    ;INICIALIZA LA CONVERSION
WAIT_ADC	    BTFSC	    ADCON0, 1
		    GOTO	    WAIT_ADC	    ;ESPERA A LA FINALIZACION DE LA CONVERSION
		    MOVF	    ADRESH, W
		    MOVWF	    VAL_ADC	    ;ALMACENA EL VALOR OBTENIDO POR EL ADC EN VAL_ADC
		    
		    ;RECARGA LOS VALORES PARA DELAY_ADC
		    MOVLW	    .10
		    MOVWF	    DELAY1TMR0
		    MOVLW	    .2
		    MOVWF	    DELAY2TMR0
		    
		    RETURN
		    		    
FIN_ISR_ADC	   
		    BCF		    INTCON, T0IF    ;LIMPIA LA BANDERA
		    RETURN  

		    
;-------------------------
; Servicio de interrupcion
;-------------------------
ISR		    ;SALVADO DE CONTEXTO
		    MOVWF	    W_TEMP
		    SWAPF	    STATUS, W
		    MOVWF	    STATUS_TEMP
		    ;FIN DE SALVADO DE CONTEXTO
		    ;INICIO ISR
		    BTFSC	    INTCON, T0IF
		    CALL	    ISR_ADC
		    ;RECUPERACION DE CONTEXTO
		    SWAPF	    STATUS_TEMP, W
		    MOVWF	    STATUS
		    SWAPF	    W_TEMP, F
		    SWAPF	    W_TEMP, W
		    ;FIN DE RECUPERACION DE CONTEXTO
		    RETFIE
		    
		    
		    END


