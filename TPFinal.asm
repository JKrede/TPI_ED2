;------------------------------------------------------------
; PROGRAMA: Medidor de CO - Trabajo Pr�ctico Final Electr�nica Digital 2
; ARCHIVO:  TPFinal.asm
; AUTORES:  Jerez, Facundo - Krede, Juli�n - M�ndez Quiroga, Celeste Evangelina
; FECHA:    19/06/2025
; MICROCONTROLADOR: PIC16F887
; CLOCK: Oscilador XT (cristal externo) 4[MHz]
;
; DESCRIPCI�N:
;   - Lee se�al anal�gica de sensor de CO (canal AN7)
;   - Muestra valor en 4 displays de 7 segmentos:
;     * Modo normal: Muestra valor actual del ADC
;     * Modo teclado: Muestra umbral configurado
;   - Control mediante teclado matricial 4x3:
;     * Tecla '*' alterna entre modos de visualizaci�n
;     * Tecla '#' activa/desactiva alarma
;     * Teclas 0-9 configuran umbral de alarma
;   - Transmisi�n serial de valores (9600 baudios)
;   - Alarma visual/sonora cuando se supera umbral
;
; CONFIGURACI�N:
;   - Oscilador XT
;   - WDT desactivado
;   - Master Clear habilitado
;   - Low Voltage Programming desactivado
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

; Variables en banco com�n (0x70-0x7F)
VAL_ADC       EQU   0x70    ; Valor crudo del ADC (0-255)
VAL_UMBRAL_U  EQU   0x71    ; Unidades del umbral (0-9)
VAL_UMBRAL_D  EQU   0x72    ; Decenas del umbral (0-9)
VAL_UMBRAL_C  EQU   0x73    ; Centenas del umbral (0-9)
POS_TECLA     EQU   0x74    ; Posici�n de tecla presionada (1-12)
N_TECLA       EQU   0x75    ; Contador de teclas presionadas (1-3)
TECLA_ACTIVA  EQU   0x76    ; Tecla actualmente presionada
OPCIONES      EQU   0x77    ; Registro de opciones:
			    ;   BIT 0: Modo display (0=ADC, 1=Teclado)
			     ;   BIT 1: Estado alarma (0=NO DISPARADA, 1=DISPARADA)
			     ;	 BIT 2: Activar/Desactivar Alarma (0=OFF, 1=ON)
DELAY1TMR0    EQU   0x78    ; Contador delay para ADC (primer nivel)
DELAY2TMR0    EQU   0x79    ; Contador delay para ADC (segundo nivel)
W_TEMP        EQU   0x7A    ; Backup de W durante ISR
STATUS_TEMP   EQU   0x7B    ; Backup de STATUS durante ISR
VAL_ADC_U     EQU   0x7C    ; Unidades del valor ADC (0-9)
VAL_ADC_D     EQU   0x7D    ; Decenas del valor ADC (0-9)
VAL_ADC_C     EQU   0x7E    ; Centenas del valor ADC (0-9)
VAL_ADC_M     EQU   0x7F    ; Unidades de mil del valor ADC
   
   
;-------------------------
; MACROS DE CONFIGURACI�N
;-------------------------

;----------------------------------------------------------------
; CONFIGURACI�N DE DISPLAYS 7 SEGMENTOS
;----------------------------------------------------------------
; Configura los puertos para controlar 4 displays de 7 segmentos:
;   - PORTD como salida (segmentos a-g)
;   - RA0-RA3 como salidas (control de displays)
;   - Configura AN7 como entrada anal�gica para el sensor
;----------------------------------------------------------------
CONF_DISPLAY	    MACRO
	    
    BANKSEL	    ANSEL
    MOVLW	    B'10000000'  ; AN7 como anal�gico, dem�s pines como digital
    MOVWF	    ANSEL

    BANKSEL	    TRISD
    CLRF	    TRISD        ; PORTD completo como salida (segmentos)

    MOVLW	    B'11110000'  ; RA0-RA3 como salidas (control displays)
    MOVWF	    TRISA        ; RA4-RA7 como entradas (no usadas)
    ENDM

;----------------------------------------------------------------
; CONFIGURACI�N DEL TECLADO
;----------------------------------------------------------------
; CONFIG_TECLADO: Configura el puerto B para teclado matricial
;   - RB0-RB3 como salidas (filas)
;   - RB4-RB6 como entradas con pull-up (columnas)
;   - Habilita interrupciones por cambio en puerto B
;----------------------------------------------------------------
CONF_TECLADO	    MACRO

    BANKSEL	    ANSELH
    CLRF	    ANSELH	    ; PUERTO B COMO DIGITAL
    MOVLW	    B'01110000'  
    MOVWF	    TRISB	    ; <RB0-RB3> SALIDAS 
				    ; <RB4-RB6> ENTRADAS
    BANKSEL	    WPUB	    
    BCF		    OPTION_REG,7
    MOVLW	    B'01110000'
    MOVWF	    WPUB	    ; RESISTENCIAS PULL-UPS ACTIVADAS
    BANKSEL	    PORTB
    MOVLW	    B'01110000'	    
    MOVWF	    PORTB	    ;TODAS LAS ENTRADAS EN ALTO POR DEFECTO
    ENDM
 
;----------------------------------------------------------------
; CONFIGURACI�N DE ALARMA
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

;----------------------------------------------------------------
; CONFIGURACI�N DE COMUNICACI�N SERIAL (UART)
;----------------------------------------------------------------
; Configura el m�dulo EUSART para transmisi�n serial:
;   - Baud rate 9600 (para 4MHz y BRGH=1)
;   - Solo transmisi�n (no recepci�n)
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
    
;----------------------------------------------------------------
; CONFIGURACI�N DEL M�DULO ADC
;----------------------------------------------------------------
; Configura el ADC para leer el sensor de CO:
;   - Canal AN7 (RE2) como entrada anal�gica
;   - Voltaje de referencia interno (VDD y VSS)
;   - Justificaci�n a la izquierda (8 bits MSB en ADRESH)
;   - Frecuencia de reloj Fosc/8
;   - Inicializa contadores de delay para primera lectura
;----------------------------------------------------------------
CONF_ADC		    MACRO
		    
    BANKSEL	    TRISE
    BSF		    TRISE, RE2   ; RE2 como entrada (AN7)

    BANKSEL	    ADCON1
    CLRF	    ADCON1       ; Voltaje referencia VDD/VSS, justificaci�n izquierda

    BANKSEL	    ADCON0
    MOVLW	    B'01011101'  ; Fosc/8, canal AN7, ADC encendido
    MOVWF	    ADCON0

    ;Contadores inicializados en 0 para que se realice una conversion inmediata
    ;al encender el sistema
    CLRF	    DELAY1TMR0   
    CLRF	    DELAY2TMR0  
    ENDM

;----------------------------------------------------------------
; CONFIGURACI�N DEL TIMER0
;----------------------------------------------------------------
; Configura Timer0 para generar interrupciones peri�dicas:
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
    MOVLW	    .61          ; Valor inicial para ~50ms (ajustar seg�n Fosc)
    MOVWF	    TMR0
    ENDM
    
;----------------------------------------------------------------
; CONFIGURACI�N DE INTERRUPCIONES
;----------------------------------------------------------------
; Habilita las interrupciones globales y espec�ficas:
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
; C�digo principal
;-------------------------
		    ORG		    0X00
		    GOTO	    INICIO
		    
		    ORG		    0X04
		    GOTO	    ISR
		    
		    ORG		    0X05

INICIO		    ;INICIO PROGRAMA
		    CONF_TECLADO
		    CONF_DISPLAY
		    CONF_ALARMA
		    CONF_ADC
		    CONF_TIMER0
		    CONF_COMUNICACION
		    CONF_INTERRUPCION
		    CLRF	    OPCIONES       ; Inicializar todas las opciones a 0
		    MOVLW	    .1             ; Valor por defecto para umbral (100)
		    MOVWF	    VAL_UMBRAL_C
		    MOVLW	    .0
		    MOVWF	    VAL_UMBRAL_D
		    MOVWF	    VAL_UMBRAL_U
		    
REFRESH		    BTFSC	    OPCIONES, 0
		    CALL	    MOSTRAR_TECLADO
		    BTFSS	    OPCIONES, 0
		    CALL	    SHOW_ADC_DISPLAY
		    CALL	    CHECK_ALARMA
		    CALL	    TEST_ALARMA
		    GOTO	    REFRESH
		    
;--------------------------
; Subrutinas de la alarma
;--------------------------
;----------------------------------------------------------
; CHECK_ALARMA: Verifica que el valor del adc sea menor
;		   que el del valor umbral, y en base a eso
;		   edita el bit 1 del registro opciones:
;	Si VAL_ADC > VAL_UMBRAL -> BIT 1 = '1'
;	Si VAL_ADC < VAL_UMBRAL -> BIT 1 = '0'
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
; DESCOMP_VAL_ADC: Descompone valor ADC (0-255) en d�gitos
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
    
		    
;------------------------------------
; Subrutinas de comunicacion serial
;------------------------------------
		    
;--------------------------------------------------------------
;ENVIAR_INFO: Realiza el envio del valor almacenado en la 
;	variable VAL_ADC codificada en codigo ASCII, con salto
;	de linea y retorno de carro incluido entre valores
;--------------------------------------------------------------
ENVIAR_INFO	    CALL	    DESCOMP_VAL_ADC
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
	    
;-------------------------
; Subrutinas del teclado
;-------------------------
;LAYOUT TECLADO:
; 1  2  3
; 4  5  6
; 7  8  9
; *  0  #
;
;------------------------------------------------
; ISR_TECLADO: Maneja interrupciones del teclado
;   Detecta tecla presionada mediante barrido
;   Maneja teclas especiales (* y #)
;   Almacena valores num�ricos para umbral
;   L�gica antirrebote incorporada
;------------------------------------------------
ISR_TECLADO	    CLRF	    POS_TECLA
		    
		    CLRF	    TECLA_ACTIVA
		    
		    MOVLW	    .3		    ;VERIFICA QUE N_TECLA NO SUPERE 3
		    SUBWF	    N_TECLA, W
		    BTFSC	    STATUS,Z
		    CLRF	    N_TECLA
		    
		    BANKSEL	    PORTB
		    MOVLW	    B'11111110'
		    MOVWF	    PORTB
		    NOP
		    GOTO	    TEST_COL
		    
	
TEST_COL	    INCF	    POS_TECLA, F
		    BTFSS	    PORTB, RB4	    ;TESTEA COLUMNA1
		    GOTO	    TECLA_PRES
		    
		    INCF	    POS_TECLA, F
		    BTFSS	    PORTB, RB5	    ;TESTEA COLUMNA2
		    GOTO	    TECLA_PRES
		    
		    INCF	    POS_TECLA, F
		    BTFSS	    PORTB, RB6	    ;TESTEA COLUMNA3
		    GOTO	    TECLA_PRES
		    GOTO	    CAMBIAR_FILA

CAMBIAR_FILA	    MOVLW	    .12
		    SUBWF	    POS_TECLA, W
		    BTFSC	    STATUS, Z
		    GOTO	    FIN_ISR_TECLADO ;NO DETECTO TECLA, FINALIZA 
		    BSF		    STATUS, C
		    RLF		    PORTB, F	    ;MUEVE EL CERO A LA IZQUIERDA
		    GOTO	    TEST_COL
		    
		    
TECLA_PRES	    BTFSS	    PORTB, RB4	    ;ANTIREBOTE
		    GOTO	    TECLA_PRES	    ;----------
		    BTFSS	    PORTB, RB5	    ;----------
		    GOTO	    TECLA_PRES	    ;----------
		    BTFSS	    PORTB, RB6	    ;----------
		    GOTO	    TECLA_PRES	    ;ANTIREBOTE
		    
		    ; VERIFICA SI ES UNA TECLA ESPECIAL
		    MOVF	    POS_TECLA, W
		    MOVWF	    TECLA_ACTIVA
                
		    ; TECLA '*' (POSICION 10): CAMBIA DISPLAY
		    MOVLW	    .10
		    SUBWF	    TECLA_ACTIVA, W
		    BTFSC	    STATUS, Z
		    GOTO	    CAMBIAR_DSPL
                
		    ; TECLA '#' (POSICION 12): ACTIVA/DESACTIVA ALARMA
		    MOVLW	    .12
		    SUBWF	    TECLA_ACTIVA, W
		    BTFSC	    STATUS, Z
		    GOTO	    CAMBIAR_ALARMA
		
		    ; SI NO ERA TECLA ESPECIAL ENTONCES SIGUE Y CARGA VALORES
		    INCF	    N_TECLA, F
		    MOVLW	    .1
		    SUBWF	    N_TECLA, W
		    BTFSC	    STATUS,Z
		    GOTO	    CARGAR_UMBRAL_C
		    
		    MOVLW	    .2
		    SUBWF	    N_TECLA, W
		    BTFSC	    STATUS,Z
		    GOTO	    CARGAR_UMBRAL_D
		    
		    MOVLW	    .3
		    SUBWF	    N_TECLA, W
		    BTFSC	    STATUS,Z
		    GOTO	    CARGAR_UMBRAL_U
		          
		    GOTO	    FIN_ISR_TECLADO
		    
		    
CARGAR_UMBRAL_C	   
		    MOVF	    POS_TECLA, W
		    CALL	    TABLA_TECLADO
		    MOVWF	    VAL_UMBRAL_C
		    GOTO	    FIN_ISR_TECLADO

CARGAR_UMBRAL_D	   
		    MOVF	    POS_TECLA, W
		    CALL	    TABLA_TECLADO
		    MOVWF	    VAL_UMBRAL_D
		    GOTO	    FIN_ISR_TECLADO
		    
CARGAR_UMBRAL_U	    
		    MOVF	    POS_TECLA, W
		    CALL	    TABLA_TECLADO
		    MOVWF	    VAL_UMBRAL_U
		    GOTO	    FIN_ISR_TECLADO

;----------------------------------------------
; CAMBIAR_DSPL: Alterna modo de visualizaci�n
;   Alterna BIT 0 de OPCIONES:
;     0 = Muestra valores del ADC
;     1 = Muestra valores del teclado (umbral)
;----------------------------------------------
CAMBIAR_DSPL	    
		    MOVLW	    B'00000001'
		    XORWF	    OPCIONES, F 		   
		    GOTO	    FIN_ISR_TECLADO

;-----------------------------------------
; CAMBIAR_ALARMA: Activa/desactiva alarma
;   Alterna BIT 1 de OPCIONES:
;     0 = Alarma desactivada
;     1 = Alarma activada
;   Control f�sico en RC0
;-----------------------------------------
CAMBIAR_ALARMA	    
		    MOVLW	    B'00000100'	    
		    XORWF	    OPCIONES, F    
		    GOTO	    FIN_ISR_TECLADO
		    		    		        
MOSTRAR_TECLADO	   
		    BANKSEL	    PORTA	    
		    MOVLW	    B'00000001'
		    MOVWF	    PORTA	    ;ACTIVA DISPLAY 1 Y DESACTIVA LOS DEMAS
		    MOVF	    VAL_UMBRAL_U, W
		    CALL	    TABLA_DSPL	    
		    MOVWF	    PORTD	    ;CARGA EL VALOR DE LA UNIDAD EN EL DISPLAY CON SU CORRESPONDIENTE FORMATO
		    CALL	    DELAY5ms	    
		    
		    MOVLW	    B'00000010'
		    MOVWF	    PORTA	    ;ACTIVA DISPLAY 2 Y DESACTIVA LOS DEMAS
		    MOVF	    VAL_UMBRAL_D, W
		    CALL	    TABLA_DSPL
		    MOVWF	    PORTD	    ;CARGA EL VALOR DE LA DECENA EN EL DISPLAY CON SU CORRESPONDIENTE FORMATO
		    CALL	    DELAY5ms

		    MOVLW	    B'00000100'
		    MOVWF	    PORTA	    ;ACTIVA DISPLAY 3 Y DESACTIVA LOS DEMAS
		    MOVF	    VAL_UMBRAL_C, W
		    CALL	    TABLA_DSPL
		    MOVWF	    PORTD	    ;CARGA EL VALOR DE LA CENTENA EN EL DISPLAY CON SU CORRESPONDIENTE FORMATO
		    CALL	    DELAY5ms
		    
		    MOVLW	    B'00001000'
		    MOVWF	    PORTA	    ;ACTIVA DISPLAY 4 Y DESACTIVA LOS DEMAS
		    MOVLW	    .0
		    CALL	    TABLA_DSPL
		    MOVWF	    PORTD	    
		    CALL	    DELAY5ms
		    
		    RETURN
		    
FIN_ISR_TECLADO	    
		    MOVLW	    B'01110000'
		    MOVWF	    PORTB
		    BCF		    INTCON, RBIF
		    RETURN  
		    
TABLA_TECLADO	    
		    ADDWF	    PCL, F
		    NOP				    ;   NO DEBERIA LLEGAR A ESTA LINEA
		    RETLW	    .1		    ;1
		    RETLW	    .2		    ;2
		    RETLW	    .3		    ;3
		    RETLW	    .4		    ;4
		    RETLW	    .5		    ;5
		    RETLW	    .6		    ;6
		    RETLW	    .7		    ;7
		    RETLW	    .8		    ;8
		    RETLW	    .9		    ;9
		    RETLW	    0XFF	    ;*	NO DEBERIA LLEGAR A ESTA LINEA
		    RETLW	    .0		    ;0
		    RETLW	    0XFE	    ;#	NO DEBERIA LLEGAR A ESTA LINEA
		    
		    
;-------------------------
; Subrutinas del ADC
;-------------------------	    

;------------------------------------------------------------------------
; ISR_ADC: Realiza adquision y conversion de la se�al analogica de 
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
		    CALL	    ENVIAR_INFO	    ;ENVIA EL VALOR OBTENIDO MEDIANTE COM. SERIAL
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
		    BTFSC	    INTCON, RBIF
		    CALL	    ISR_TECLADO
		    ;RECUPERACION DE CONTEXTO
		    SWAPF	    STATUS_TEMP, W
		    MOVWF	    STATUS
		    SWAPF	    W_TEMP, F
		    SWAPF	    W_TEMP, W
		    ;FIN DE RECUPERACION DE CONTEXTO
		    RETFIE
		    
		    
		    END