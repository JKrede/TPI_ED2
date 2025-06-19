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
; PRUEBA: INICIAR EL SISTEMA CON VAL_ADC = 50D, VALOR UMBRAL = 25D Y LA ALARMA ACTIVADA.
;	1. MEDIANTE EL TECLADO CAMBIAR EL VALOR UMBRAL A >50 PARA QUE SUENE LA ALARMA.
;	2. POSTERIORMENTE TOCAR EL BOTON # DEL TECLADO PARA PROBAR LA DESACTIVACION.
;--------------------------------------------------------------------------------------
		    
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
VAL_UMBRAL_U  EQU   0x71    ; Unidades del umbral (0-9)
VAL_UMBRAL_D  EQU   0x72    ; Decenas del umbral (0-9)
VAL_UMBRAL_C  EQU   0x73    ; Centenas del umbral (0-9)
POS_TECLA     EQU   0x74    ; Posición de tecla presionada (1-12)
N_TECLA       EQU   0x75    ; Contador de teclas presionadas (1-3)
TECLA_ACTIVA  EQU   0x76    ; Tecla actualmente presionada
OPCIONES      EQU   0x77    ; Registro de opciones:
			     ;   BIT 0: Modo display (0=ADC, 1=Teclado)
			     ;   BIT 1: Estado alarma (0=OFF, 1=ON)
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
    MOVWF	    WPUB	    ; RESISTENCIAS PULL-UPS ACTIVADAS
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
		    CONF_TECLADO
		    CONF_DISPLAY
		    CONF_ALARMA
		    CONF_INTERRUPCION
		    
		    MOVLW	    .50		   ; VALOR A SUPERAR PARA QUE SE ACTIVE LA ALARMA
		    MOVWF	    VAL_ADC	   ;
		    BSF	    	    OPCIONES, 2    ; INICIALIZA LA ALARMA ACTIVADA
		    MOVLW	    .0	           ; VALOR MENOR AL VAL ADC PARA QUE NO SUENE
		    MOVWF	    VAL_UMBRAL_C
		    MOVLW	    .7	           
		    MOVWF	    VAL_UMBRAL_D
		    MOVLW	    .5	           
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
; DESCOMP_VAL_ADC: Verifica que el valor del adc sea menor
;		   que el del valor umbral, y en base a eso
;		   edita el registro
;----------------------------------------------------------
CHECK_ALARMA
		    MOVF	    VAL_UMBRAL_C, W
		    SUBWF	    VAL_ADC_C, W      ; VAL_ADC_C - VAL_UMBRAL_C
		    BTFSS	    STATUS, C         ; Si C=0 (VAL_ADC_C < VAL_UMBRAL_C)
		    GOTO	    ALARMA_ON	      ; Centena ADC menor que umbral -> Activar

		    BTFSS	    STATUS, Z         ; Si Z=1 (VAL_ADC_C = VAL_UMBRAL_C)
		    GOTO	    ALARMA_OFF	      ; Centena ADC mayor que umbral -> Desactivar

		    ; Si centenas iguales, comparar DECENAS
		    MOVF	    VAL_UMBRAL_D, W
		    SUBWF	    VAL_ADC_D, W      ; VAL_ADC_D - VAL_UMBRAL_D
		    BTFSS	    STATUS, C         ; Si C=0 (VAL_ADC_D < VAL_UMBRAL_D)
		    GOTO	    ALARMA_ON	      ; Decena ADC menor que umbral -> Activar

		    BTFSS	    STATUS, Z         ; Si Z=1 (VAL_ADC_D = VAL_UMBRAL_D)
		    GOTO	    ALARMA_OFF	      ; Decena ADC mayor que umbral -> Desactivar

		    ; Si decenas iguales, comparar UNIDADES
		    MOVF	    VAL_UMBRAL_U, W
		    SUBWF	    VAL_ADC_U, W      ; VAL_ADC_U - VAL_UMBRAL_U
		    BTFSS	    STATUS, C         ; Si C=0 (VAL_ADC_U < VAL_UMBRAL_U)
		    GOTO	    ALARMA_ON	      ; Unidad ADC menor que umbral -> Activar

		    ; Si llegamos aquí, VAL_ADC >= VAL_UMBRAL
ALARMA_ON	    BSF		    OPCIONES, 1       ; Setear bit de estado de alarma
		    RETURN

ALARMA_OFF	    BCF		    OPCIONES, 1       ; Limpiar bit de estado de alarma
							
		    RETURN
		    
; Togglea entre alarma activada y desactivada
TEST_ALARMA	    
		    BANKSEL	    PORTC
		    BTFSC	    OPCIONES, 2
		    CALL   	    VERIFICAR_ALARMA
		    BTFSS	    OPCIONES, 2
		    BCF		    PORTC, 0
		    RETURN
		    
VERIFICAR_ALARMA    BTFSC	    OPCIONES, 1
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
		    BCF		    STATUS, RP1	    ;BANCO DE LOS VALORES DE ADC 
		    MOVF	    VAL_ADC, W
		    MOVWF	    TEMP_VAL_ADC    ;GUARDA UNA COMPIA DE VAL_ADC
		    CLRF	    VAL_ADC_U	    ;UNIDAD DEL VALOR A MOSTRAR
		    CLRF	    VAL_ADC_D	    ;UNIDAD DE DECENA DEL VALOR A MOSTRAR
		    CLRF	    VAL_ADC_C	    ;UNIDAD DE CENTENA DEL VALOR A MOSTRAR
		    CLRF	    VAL_ADC_M	    ;NUNCA VALE DISTINTO DE CERO
		    
TEST_C		    MOVLW	    .100	    ;CALCULA LA CENTENA
		    SUBWF	    TEMP_VAL_ADC, F
		    BTFSC	    STATUS, C
		    GOTO	    ADD_C
		    MOVLW	    .100	    ;SI ES NEGATIVO RECUPERA EL VALOR ORIGINAL (SOLO SE DA CUANDO ES 0)          
		    ADDWF	    TEMP_VAL_ADC, F 
		    GOTO	    TEST_D 
		    
TEST_D		    MOVLW	    .10		    ;CALCULA LA DECENA
		    SUBWF	    TEMP_VAL_ADC, F
		    BTFSC	    STATUS, C
		    GOTO	    ADD_D
		    MOVLW	    .10	    	    ;SI ES NEGATIVO RECUPERA EL VALOR ORIGINAL (SOLO SE DA CUANDO ES 0)          
		    ADDWF	    TEMP_VAL_ADC, F 
		    GOTO	    TEST_U
		    
TEST_U		    MOVLW	    .1		    ;CALCULA LA UNIDAD
		    SUBWF	    TEMP_VAL_ADC, F
		    BTFSC	    STATUS, C
		    GOTO	    ADD_U
		    MOVLW	    .1		    ;SI ES NEGATIVO RECUPERA EL VALOR ORIGINAL (SOLO SE DA CUANDO ES 0)          
		    ADDWF	    TEMP_VAL_ADC, F 
		    
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
		    INCF	    N_TECLA, F
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
		    MOVLW	    B'00000100'	    
		    XORWF	    OPCIONES, F    
		    GOTO	    FIN_ISR_TECLADO
		    		    
FIN_ISR_TECLADO	    
		    MOVLW	    B'00001111'
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
		    ;RECUPERACION DE CONTEXTO
		    SWAPF	    STATUS_TEMP, W
		    MOVWF	    STATUS
		    SWAPF	    W_TEMP, F
		    SWAPF	    W_TEMP, F
		    ;FIN DE RECUPERACION DE CONTEXTO
		    RETFIE
		    
		    
		    END


