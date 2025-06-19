;------------------------------------------------------------
; PROGRAMA: Medidor de CO - Trabajo Práctico Final Electrónica Digital 2
; ARCHIVO:  TPFinal.asm
; AUTORES:  Jerez, Facundo - Krede, Julián - Méndez Quiroga, Celeste Evangelina
; FECHA:    19/06/2025
; MICROCONTROLADOR: PIC16F887
; CLOCK: Oscilador XT (cristal externo) 4[MHz]
;
; DESCRIPCIÓN:
;   - Lee señal analógica de sensor de CO (canal AN7)
;   - Muestra valor en 4 displays de 7 segmentos:
;     * Modo normal: Muestra valor actual del ADC
;     * Modo teclado: Muestra umbral configurado
;   - Control mediante teclado matricial 4x3:
;     * Tecla '*' alterna entre modos de visualización
;     * Tecla '#' activa/desactiva alarma
;     * Teclas 0-9 configuran umbral de alarma
;   - Transmisión serial de valores (9600 baudios)
;   - Alarma visual/sonora cuando se supera umbral
;
; CONFIGURACIÓN:
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

; Variables en banco común (0x70-0x7F)
VAL_ADC       EQU   0x70    ; Valor crudo del ADC (0-255)
VAL_UMBRAL_U  EQU   0x71    ; Unidades del umbral (0-9)
VAL_UMBRAL_D  EQU   0x72    ; Decenas del umbral (0-9)
VAL_UMBRAL_C  EQU   0x73    ; Centenas del umbral (0-9)
POS_TECLA     EQU   0x74    ; Posición de tecla presionada (1-12)
N_TECLA       EQU   0x75    ; Contador de teclas presionadas (1-3)
TECLA_ACTIVA  EQU   0x76    ; Tecla actualmente presionada
OPCIONES      EQU   0x77    ; Registro de opciones:
			     ;   BIT 0: Modo display (0=ADC, 1=Teclado)
			     ;   BIT 1: Estado alarma (0=OFF, 1=ON)
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
; CONFIGURACIÓN DEL TECLADO
;----------------------------------------------------------------
; CONFIG_TECLADO: Configura el puerto B para teclado matricial
;   - RB0-RB3 como salidas (filas)
;   - RB4-RB6 como entradas con pull-up (columnas)
;   - Habilita interrupciones por cambio en RB4-RB6
CONF_TECLADO	    MACRO

    BANKSEL	    ANSELH
    CLRF	    ANSELH	    ; PUERTO B COMO DIGITAL
    MOVLW	    B'01110000'  
    MOVWF	    TRISB	    ; <RB0-RB3> SALIDAS 
				    ; <RB4-RB6> ENTRADAS
    BANKSEL	    WPUB	    
    BCF		    OPTION_REG,7
    MOVLW	    0XFF
    MOVWF	    WPUB	    ; RESISTENCIAS PULL-UPS ACTIVADAS EN ENTRADAS
    BANKSEL	    PORTB
    MOVLW	    B'01110000'	    
    MOVWF	    PORTB	    ;TODAS LAS ENTRADAS EN ALTO POR DEFECTO

    ENDM

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
; CONFIGURACIÓN DE SALIDA DE ALARMA
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

    CLRF	    DELAY1TMR0   ; Inicializa contadores para
    CLRF	    DELAY2TMR0   ; primera lectura del ADC

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
		    BTFSC	    OPCIONES, 1
		    CALL	    CHECK_ALARMA
		    CALL	    TEST_ALARMA
		    GOTO	    REFRESH
		    
;--------------------------
; Subrutinas de la alarma
;--------------------------
;----------------------------------------------------------
; DESCOMP_VAL_ADC: Verifica que el valor del adc sea menor
;		   que el del valor umbral, y en base a eso
;		   edita el registro
;----------------------------------------------------------
CHECK_ALARMA
		    MOVF	    VAL_UMBRAL_C, W
		    SUBWF	    VAL_ADC_C, W      ; VAL_ADC_C - VAL_UMBRAL_C
		    BTFSS	    STATUS, C         ; Si C=0 (VAL_ADC_C < VAL_UMBRAL_C)
		    GOTO	    ACTIVAR_ALARMA    ; Centena ADC menor que umbral -> Activar

		    BTFSS	    STATUS, Z         ; Si Z=1 (VAL_ADC_C = VAL_UMBRAL_C)
		    GOTO	    DESACTIVAR_ALARMA ; Centena ADC mayor que umbral -> Desactivar

		    ; Si centenas iguales, comparar DECENAS
		    MOVF	    VAL_UMBRAL_D, W
		    SUBWF	    VAL_ADC_D, W      ; VAL_ADC_D - VAL_UMBRAL_D
		    BTFSS	    STATUS, C         ; Si C=0 (VAL_ADC_D < VAL_UMBRAL_D)
		    GOTO	    ACTIVAR_ALARMA    ; Decena ADC menor que umbral -> Activar

		    BTFSS	    STATUS, Z         ; Si Z=1 (VAL_ADC_D = VAL_UMBRAL_D)
		    GOTO	    DESACTIVAR_ALARMA ; Decena ADC mayor que umbral -> Desactivar

		    ; Si decenas iguales, comparar UNIDADES
		    MOVF	    VAL_UMBRAL_U, W
		    SUBWF	    VAL_ADC_U, W      ; VAL_ADC_U - VAL_UMBRAL_U
		    BTFSS	    STATUS, C         ; Si C=0 (VAL_ADC_U < VAL_UMBRAL_U)
		    GOTO	    ACTIVAR_ALARMA    ; Unidad ADC menor que umbral -> Activar

		    ; Si llegamos aquí, VAL_ADC >= VAL_UMBRAL
DESACTIVAR_ALARMA   BCF		    OPCIONES, 1       ; Limpiar bit de estado de alarma
		    RETURN

ACTIVAR_ALARMA	    BSF		    OPCIONES, 1       ; Setear bit de estado de alarma
		    RETURN

TEST_ALARMA	    
		    BANKSEL	    PORTC
		    BTFSC	    OPCIONES, 1
		    BSF		    PORTC, 0
		    BTFSS	    OPCIONES, 1
		    BCF		    PORTC, 0

		    RETURN
		    
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
		    CLRF	    VAL_ADC_U	    ;UNIDAD DEL VALOR A MOSTRAR
		    CLRF	    VAL_ADC_D	    ;UNIDAD DE DECENA DEL VALOR A MOSTRAR
		    CLRF	    VAL_ADC_C	    ;UNIDAD DE CENTENA DEL VALOR A MOSTRAR
		    CLRF	    VAL_ADC_M	    ;NUNCA VALE DISTINTO DE CERO
		    
TEST_C		    MOVLW	    .100	    ;CALCULA LA CENTENA
		    SUBWF	    VAL_ADC, F
		    BTFSC	    STATUS,C
		    GOTO	    ADD_C
		    
TEST_D		    MOVLW	    .10		    ;CALCULA LA DECENA
		    SUBWF	    VAL_ADC, F
		    BTFSC	    STATUS,C
		    GOTO	    ADD_D
		    
TEST_U		    MOVLW	    .1		    ;CALCULA LA UNIDAD
		    SUBWF	    VAL_ADC, F
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

		    
		    
		    
		    
;------------------------------------
; Subrutinas de comunicacion serial
;------------------------------------
		    ;REALIZA EL ENVIO DEL VALOR OBTENIDO DEL ADC CODIFICADO EN CODIGO ASCII. ESTA FUNCION DEBE SER LLAMADA UNICAMENTE EN GET_ADC
ENVIAR_INFO	    MOVF	    VAL_ADC_C, W
		    ADDLW	    .48
		    CALL	    UART_TX	    ;ENVIA CENTENA
		    MOVF	    VAL_ADC_D, W
		    
		    ADDLW	    .48
		    CALL	    UART_TX	    ;ENVIA DECENA
		    BCF		    STATUS, RP0
		    BCF		    STATUS, RP1
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
;   Almacena valores numéricos para umbral
;   Lógica antirrebote incorporada
;------------------------------------------------
ISR_TECLADO	    CLRF	    POS_TECLA
		    CLRF	    N_TECLA
		    CLRF	    TECLA_ACTIVA
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
		    BTFSC	    STATUS,Z
		    GOTO	    FIN_ISR_TECLADO ;FINALIZA 
		    BSF		    STATUS, C
		    RLF		    PORTB, F	    ;MUEVE EL CERO A LA IZQUIERDA
		    GOTO	    TEST_COL
		    
		    
TECLA_PRES	    BTFSS	    PORTB, RB4	    ;----------
		    GOTO	    TECLA_PRES	    ;
		    BTFSS	    PORTB, RB5	    ;ANTIREBOTE
		    GOTO	    TECLA_PRES	    ;ANTIREBOTE
		    BTFSS	    PORTB, RB6	    ;
		    GOTO	    TECLA_PRES	    ;----------
		    
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
		    INCF	    N_TECLA
		    MOVLW	    .1
		    SUBWF	    N_TECLA,W
		    BTFSC	    STATUS,Z
		    GOTO	    CARGAR_UMBRAL_C
		    
		    MOVLW	    .2
		    SUBWF	    N_TECLA,W
		    BTFSC	    STATUS,Z
		    GOTO	    CARGAR_UMBRAL_D
		    
		    MOVLW	    .3
		    SUBWF	    N_TECLA,W
		    BTFSC	    STATUS,Z
		    GOTO	    CARGAR_UMBRAL_U
		    
		    CLRF	    N_TECLA         
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
; CAMBIAR_DSPL: Alterna modo de visualización
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
;   Control físico en RC0
;-----------------------------------------
CAMBIAR_ALARMA	    
		    MOVLW	    B'00000010'	    
		    XORWF	    OPCIONES, F    
		    GOTO	    FIN_ISR_TECLADO
		    		    
FIN_ISR_TECLADO	    
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
		 
		    
;-------------------------
; Subrutinas del ADC
;-------------------------	    
		    ;REALIZA LA CONVERSION DE LA SEÑAL ANALOGICA A DIGITAL Y GUARDA EL VALOR EN UNA VARIABLE TEMPORAL VAL_ADC
READ_ADC	    
		    BANKSEL	    ADCON0
		    BSF		    ADCON0, 1	    ;INICIALIZA LA CONVERSION
WAIT_ADC	    BTFSC	    ADCON0, 1
		    GOTO	    WAIT_ADC	    ;ESPERA A LA FINALIZACION DE LA CONVERSION
		    MOVF	    ADRESH, W
		    MOVWF	    VAL_ADC	    ;MUEVO EL VALOR OBTENIDO POR EL ADC ALMACENADO EN VAL_ADC
		    CALL	    DESCOMP_VAL_ADC ;DESCOMPONE EL VALOR DEL VAL_ADC EN CENTENA, DECENA Y UNIDAD
		    CALL	    ENVIAR_INFO	    ;ENVIA EL DATO OBTENIDO DEL ADC MEDIANTE TRANSMISION SERIE
		    
		    ;RECARGA LOS VALORES PARA DELAY_ADC
		    MOVLW	    .255
		    MOVWF	    DELAY1TMR0
		    MOVLW	    .5
		    MOVWF	    DELAY2TMR0
		    
		    RETURN
		    
	    
		    ;JUNTO CON EL TMR0, REALIZA UN DELAY DE 255*5*50ms = 63.75s
ISR_ADC		    
		    BANKSEL	    ADCON0
		    MOVLW	    .61
		    MOVWF	    TMR0	    ;RECARGO EL TMR0 CON 61
		    DECFSZ	    DELAY1TMR0, F
		    GOTO	    FIN_ISR_ADC	    
		    GOTO	    TEST_DELAY2	    ;SI DELAY1TMR0 LLEGO A 0 VA A CHECKEA DELAY2TMR0 

FIN_ISR_ADC	   
		    BCF		    INTCON, T0IF    ;LIMPIA LA BANDERA
		    RETURN  
		    
		    ;SUBRUTINAS COMPLEMENTARIAS DE ISR_ADC
TEST_DELAY2	    
		    DECFSZ	    DELAY2TMR0, F
		    GOTO	    RECARGA_D1TMR0  ;SI DELAY2TMR0 NO LLEGO A 0 RECARGA DELAY1TMR0 Y SE VA A FIN_ISR_ADC
		    CALL	    READ_ADC	    ;SI DELAY1TMR0 LLEGO A 0 VA A LA SUBRUTINA QUE HACE LA CONVERSION, RECARGA DELAY1TMR0 Y DELAY2TMR0 
		    GOTO	    FIN_ISR_ADC	    ;LUEGO DE HACER LA CONVERSION SE VA A FIN_ISR_ADC

RECARGA_D1TMR0	    
		    MOVLW	    .255
		    MOVWF	    DELAY1TMR0
		    GOTO	    FIN_ISR_ADC
		    
		    

;-------------------------
; Servicio de interrupcion
;-------------------------
		    ;SALVADO DE CONTEXTO
ISR		    
		    MOVWF	    W_TEMP
		    SWAPF	    STATUS, W
		    MOVWF	    STATUS_TEMP
		    ;FIN DE SALVADO DE CONTEXTO
		    ;INICIO ISR			FUENTES DE INTERRUPCION: PUERTO B(TECLADO), TIMER0(ADC)
		    BTFSC	    INTCON, RBIF
		    CALL	    ISR_TECLADO
		    BTFSC	    INTCON, T0IF
		    CALL	    ISR_ADC
		    ;RECUPERACION DE CONTEXTO
		    SWAPF	    STATUS_TEMP, W
		    MOVWF	    STATUS
		    SWAPF	    W_TEMP, F
		    SWAPF	    W_TEMP, F
		    ;FIN DE RECUPERACION DE CONTEXTO
		    RETFIE
		    
		    
		    END