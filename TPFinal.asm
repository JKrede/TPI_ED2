;------------------------------------------------------------
; Programa : Medidor de CO - Trabajo Practico Final Electronica Digital 2
; Archivo  : TPFinal.asm
; Autores: Jerez, Facundo - Krede, Julian - Mendez Quiroga, Celeste Evangelina
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
	
		    
		    CBLOCK	    0X20
			    DELAY1DSPL
			    DELAY2DSPL
			    DELAY3DSPL
			    VAL_ADC_U
			    VAL_ADC_D
		   	    VAL_ADC_C
		    	    VAL_ADC_M
		    ENDC
		    
		    CBLOCK	    0x70
			    
			    VAL_ADC
			    
			    VAL_UMBRAL
			    
			    VAL_UMBRAL_U
			    VAL_UMBRAL_D
		   	    VAL_UMBRAL_C
			    
			    POS_TECLA
			    N_TECLA
			    
			    COUNT_DISPLAY

			    DELAY1TMR0
			    DELAY2TMR0
			    
			    W_TEMP
			    STATUS_TEMP
		    ENDC

		;MACROS DE CONFIGURACION
CONF_TECLADO	    MACRO
	    
		    
		    BANKSEL	    ANSELH
		    CLRF	    ANSELH	    ; PUERTO B COMO DIGITAL
		    MOVLW	    B'01110000'  
		    MOVWF	    TRISB	    ; <RB0-RB3> SALIDAS 
		    BANKSEL	    WPUB	    ; <RB4-RB6> ENTRADAS
		    BCF		    OPTION_REG,7
		    MOVWF	    WPUB	    ; RESISTENCIAS PULL-UPS ACTIVADAS EN ENTRADAS
		    BANKSEL	    PORTB
		    MOVLW	    B'01110000'	    
		    MOVWF	    PORTB	    ;TODAS LAS ENTRADAS EN ALTO POR DEFECTO
		    
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
		    
		    ;(PIN 10 (RE2/AN7) PREVIAMENTE CONFIGURADO COMO ANALOGICO EN (CONF_DISPLAY)
CONF_ADC	    MACRO
		    
		    BANKSEL	    TRISE
		    BSF		    TRISE, RE2	    ;RE2 COMO ENTRADA
		    BANKSEL	    ADCON1
		    CLRF	    ADCON1	    ;UTILIZA LAS TENSIONES DE REFERENCIAS INTERNAS DEL PIC Y JUSTIFICA HACIA LA IZQUIERDA (VALOR DE ADC EN ADRESH)
		    BANKSEL	    ADCON0	    
		    MOVLW	    B'01011101'	    
		    MOVWF	    ADCON0	    ;CLOCK DE CONVERSION FCLK/8 - CANAL ANALOGICO SELECCIONADO: AN7 - ADC ACTIVADO
		    CLRF	    DELAY1TMR0	    
		    CLRF	    DELAY2TMR0	    ;ARRANCAN EN 0 PARA QUE LA PRIMERA ADQUICISION SE REALICE CUANDO SE ENCIENDA EL DISPOSITIVO
		    
		    ENDM
		    
		    ;EL MODULO TIMER0 FUNCIONA DE MANERA AUXILIAR AL ADC, POR LO TANTO DEBEN SER CARGADAS SIMULTANEAMENTE
CONF_TIMER0	    MACRO
		    BSF		    STATUS,RP0
		    BSF		    STATUS,RP1
		    MOVLW	    B'01010111'
		    MOVWF	    OPTION_REG	    ;TMR0 CON CLOCK INTERNO Y PS = 1:256
		    BANKSEL	    TMR0
		    MOVLW	    .61
		    MOVWF	    TMR0	    ;PRECARGAR CON 61D PARA TENER INTERRUPCION CADA 50ms
		    
		    ENDM

CONF_INTERRUPCION   MACRO
		    MOVLW	    B'10101000'	    
		    MOVWF	    INTCON	    ;INTERRUPCION POR TMR0 Y PUERTO RB ACTIVADO
		    BANKSEL	    IOCB
		    MOVLW	    B'01110000'	    
		    MOVWF	    IOCB	    ;CONFIGURACION DE IOCB SOLO PARA LAS ENTRADAS
		    
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

		    
		    
		    
		    
		    
		    
;-------------------------
; Subrutinas del display
;-------------------------
		    ;DESCOMPONE LA VARIABLE VAL_ADC EN UNIDAD DE MIL, CENTENA, DECENA Y UNIDAD
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
		    
		    ;SUBRUTINAS COMPLEMENTARIAS DE DESCOMP_VAL_ADC
ADD_C		    INCF	    VAL_ADC_C
		    GOTO	    TEST_C

ADD_D		    INCF	    VAL_ADC_D
		    GOTO	    TEST_D	   
		    
ADD_U		    INCF	    VAL_ADC_U
		    GOTO    	    TEST_U
		    
		  
		  
		  
		  
		  
		    ;MUESTRA EL VALOR DEL ADC EN LOS DISPLAYS
SHOW_ADC_DISPLAY    CALL	    DESCOMP_VAL_ADC
		    BANKSEL	    PORTA	    ;BANCO 0
		    MOVLW	    B'00000001'
		    MOVWF	    PORTA	    ;ACTIVA DISPLAY 1 Y DESACTIVA LOS DEMAS
		    MOVF	    VAL_ADC_U
		    CALL	    TABLA_DSPL	    
		    MOVWF	    PORTD	    ;CARGA EL VALOR DE LA UNIDAD EN EL DISPLAY CON SU CORRESPONDIENTE FORMATO
		    CALL	    DELAY5ms	    
		    
		    MOVLW	    B'00000010'
		    MOVWF	    PORTA	    ;ACTIVA DISPLAY 2 Y DESACTIVA LOS DEMAS
		    MOVF	    VAL_ADC_D
		    CALL	    TABLA_DSPL
		    MOVWF	    PORTD	    ;CARGA EL VALOR DE LA DECENA EN EL DISPLAY CON SU CORRESPONDIENTE FORMATO
		    CALL	    DELAY5ms

		    MOVLW	    B'00000100'
		    MOVWF	    PORTA	    ;ACTIVA DISPLAY 3 Y DESACTIVA LOS DEMAS
		    MOVF	    VAL_ADC_C
		    CALL	    TABLA_DSPL
		    MOVWF	    PORTD	    ;CARGA EL VALOR DE LA CENTENA EN EL DISPLAY CON SU CORRESPONDIENTE FORMATO
		    CALL	    DELAY5ms
		    
		    MOVLW	    B'00001000'
		    MOVWF	    PORTA	    ;ACTIVA DISPLAY 4 Y DESACTIVA LOS DEMAS
		    MOVF	    VAL_ADC_M
		    CALL	    TABLA_DSPL
		    MOVWF	    PORTD	    ;CARGA EL VALOR DE LA UNIDAD DE MIL EN EL DISPLAY CON SU CORRESPONDIENTE FORMATO
		    CALL	    DELAY5ms
		    GOTO	    SHOW_ADC_DISPLAY	;LOOP INFINITO
		    
		    
		    
		    
		    
		    
		    ;DELAY DE 5ms USADO PARA MOSTRAR VALORES EN DISPLAY (SHOW_DISPLAY)
DELAY5ms	    BCF		    STATUS, RP0
		    BCF		    STATUS, RP1	    ;BANCO 0
		    MOVLW	    .5		    ;m=5
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
		    ;REALIZA EL ENVIO DEL VALOR OBTENIDO DEL ADC CODIFICADO EN CODIGO ASCII
ENVIAR_INFO	    CALL	    DESCOMP_VAL_ADC ;OBTIENE EL VALOR DE VAL_ADC DESCOMPUESTO
		    BCF		    STATUS, RP0
		    BCF		    STATUS, RP1
		    MOVF	    VAL_ADC_C, W
		    ADDLW	    .48
		    CALL	    UART_TX	    ;ENVIA CENTENA
		    
		    BCF		    STATUS, RP0
		    BCF		    STATUS, RP1
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
		    
		    
UART_TX		    BANKSEL	    TXSTA
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
		    ;CARGA EN POS_TECLA EL VALOR DE LA POSICION DE LA TECLA PRESIONADA QUE VA DESDE 1 HASTA 12
ISR_TECLADO	    CLRF	    POS_TECLA
		    CLRF	    N_TECLA
		    BANKSEL	    PORTB
		    MOVLW	    B'00001110'
		    MOVWF	    PORTB
		    NOP
		    GOTO	    TEST_COL
		    
	
TEST_COL	    INCF	    POS_TECLA	    
		    BTFSS	    PORTB, RB4	    ;TESTEA COLUMNA1
		    GOTO	    TECLA_PRES
		    
		    INCF	    POS_TECLA	
		    BTFSS	    PORTB, RB5	    ;TESTEA COLUMNA2
		    GOTO	    TECLA_PRES
		    
		    INCF	    POS_TECLA
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
		    
		    
TECLA_PRES	    BTFSS	    PORTB, RB4	    ;ANTIREBOTE
		    GOTO	    TECLA_PRES	    ;-----------
		    BTFSS	    PORTB, RB5	    ;-----------
		    GOTO	    TECLA_PRES	    ;-----------
		    BTFSS	    PORTB, RB6	    ;-----------
		    GOTO	    TECLA_PRES	    ;ANTIREBOTE
		    
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
		    
		    
CARGAR_UMBRAL_C	    MOVF	    POS_TECLA, W
		    CALL	    TABLA_TECLADO
		    MOVWF	    VAL_UMBRAL_C
		    ;SHOW_TECLADO_DISPLAY
		    GOTO	    FIN_ISR_TECLADO

CARGAR_UMBRAL_D	    MOVF	    POS_TECLA, W
		    CALL	    TABLA_TECLADO
		    MOVWF	    VAL_UMBRAL_D
		    ;SHOW_TECLADO_DISPLAY
		    GOTO	    FIN_ISR_TECLADO
		    
CARGAR_UMBRAL_U	    MOVF	    POS_TECLA, W
		    CALL	    TABLA_TECLADO
		    MOVWF	    VAL_UMBRAL_U
		    ;SHOW_TECLADO_DISPLAY
		    GOTO	    FIN_ISR_TECLADO
		    
		    		    
FIN_ISR_TECLADO	    BCF		    INTCON, RBIF    ;LIMPIA LA BANDERA
		    RETURN  
		 
;IMPLEMENTAR SUBRUTINAS SHOW_TECLADO_DISPLAY
		    
		    
		    
		    
;-------------------------
; Subrutinas del ADC
;-------------------------	    
		    ;REALIZA LA CONVERSION DE LA SEÑAL ANALOGICA A DIGITAL Y GUARDA EL VALOR EN UNA VARIABLE TEMPORAL VAL_ADC
GET_ADC		    BANKSEL	    ADCON0
		    BSF		    ADCON0, 1	    ;INICIALIZA LA CONVERSION
WAIT_ADC	    BTFSC	    ADCON0, 1
		    GOTO	    WAIT_ADC	    ;ESPERA A LA FINALIZACION DE LA CONVERSION
		    MOVF	    ADRESH, W
		    MOVWF	    VAL_ADC	    ;MUEVO EL VALOR OBTENIDO A VAL_ADC
		    CALL	    DESCOMP_VAL_ADC ;DESCOMPONE EL VALOR DEL VAL_ADC EN CENTENA, DECENA Y UNIDAD
		    CALL	    ENVIAR_INFO	    ;ENVIA EL DATO OBTENIDO DEL ADC MEDIANTE TRANSMISION SERIE
		    ;RECARGA LOS VALORES PARA DELAY_ADC
		    MOVLW	    .255
		    MOVWF	    DELAY1TMR0
		    MOVLW	    .5
		    MOVWF	    DELAY2TMR0
		    
		    RETURN
		    
		    
		    
		    
		    
		    
		    ;JUNTO CON EL TMR0, REALIZA UN DELAY DE APROX 64s
ISR_ADC		    BANKSEL	    ADCON0
		    MOVLW	    .61
		    MOVWF	    TMR0	    ;RECARGO EL TMR0 CON 61
		    DECFSZ	    DELAY1TMR0, F
		    GOTO	    FIN_ISR_ADC	    
		    GOTO	    TEST_DELAY2	    ;SI DELAY1TMR0 LLEGO A 0 VA A CHECKEA DELAY2TMR0 

FIN_ISR_ADC	    BCF		    INTCON, T0IF    ;LIMPIA LA BANDERA
		    RETURN  
		    
		    ;SUBRUTINAS COMPLEMENTARIAS DE ISR_ADC
TEST_DELAY2	    DECFSZ	    DELAY2TMR0, F
		    GOTO	    RECARGA_D1TMR0  ;SI DELAY2TMR0 NO LLEGO A 0 RECARGA DELAY1TMR0 Y SE VA A FIN_ISR_ADC
		    CALL	    GET_ADC	    ;SI DELAY1TMR0 LLEGO A 0 VA A LA SUBRUTINA QUE HACE LA CONVERSION, RECARGA DELAY1TMR0 Y DELAY2TMR0 
		    GOTO	    FIN_ISR_ADC	    ;LUEGO DE HACER LA CONVERSION SE VA A FIN_ISR_ADC

RECARGA_D1TMR0	    MOVLW	    .255
		    MOVWF	    DELAY1TMR0
		    GOTO	    FIN_ISR_ADC
		    
		    

;-------------------------
; Servicio de interrupcion
;-------------------------
		    ;SALVADO DE CONTEXTO
ISR		    MOVWF	    W_TEMP
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