; Projeto 2: Programa��o com PIC - Laborat�rio de Microcontroladores
; Escrever um programa que leia 3 d�gitos BCD da porta A, convertendo-os para
; um �nico n�mero bin�rio de 8 bits. Como sa�da o programa deve apresentar na
; porta B, em sequ�ncia, 1 d�gito BCD (para centena), seguido de 2 d�gitos BCD
; (dezena e unidade), correspondendo ao maior n�mero primo que seja menor ou
; igual ao valor de entrada. O programa deve ser desenvolvido e executado na
; plataforma MPlab (vers�o IDE X), usando o PIC16F873A.
;
; Autores: Alex Fabiano Garcia e Thales Marcel
; Data: 28/02/2019
; 
; Freq.: 1 MHz (simulador)
;
; A implementa��o utiliza interrup��o atrav�s do Timer 1 do PIC16F873A. A
; interrup��o ocorre com o overflow de TMR1H, sendo esta tratada somente quando
; o registrador 'timerCount', decrementado a cada interrup��o, chega a zero.
; Esta estrat�gia foi utilizada para ler a porta A de tempo em tempo sem
; utilizar o pino da porta B como entrada, pois este � utilizado como sa�da.
; Neste intervalo de interrup��o, um est�mulo pode ser causado para alterar
; os valores contidos na porta A que representam o BCD.

    list   P=PIC16F873A	    ; Microcontrolador utilizado: PIC16F873A

; Arquivos inclu�dos no c�digo
    #include <p16f873a.inc>

; FUSE bits: Cristal, sem watchdog timer, com Power Up timer (72 ms) e 
; sem Code protection
	    __config _XT_OSC & _WDT_OFF & _PWRTE_OFF & _CP_OFF

; Macros
; Selecionar o banco 0 da mem�ria
bank0	    macro
	    bcf		    STATUS, RP0
	    bcf		    STATUS, RP1
	    endm

; Selecionar o banco 1 da mem�ria
bank1	    macro
	    bsf		    STATUS, RP0
	    bcf		    STATUS, RP1
	    endm

; Skip the next instruction if W was less than or equal to the value
; it was subtracted from.  This assumes that the carry flag has been
; preserved from the last SUBWF or SUBLW instruction.
skip_wle    macro
	    btfss	    STATUS, C;skip if no borrow occurred
	    endm

; Registradores de uso geral
	    cblock	    H'20'
	    bcdCounter		; Registrador para contar BCDs informados
	    bcdAccumulator	; Registrador para acumular BCDs
	    timerCount		; Registrador auxiliar para tratar temporiza��o
				; juntamente com interrup��o do Timer0
	    bcdAux		; Auxilia na busca por BCDs na tabela look-up
	    iteracao		; Controle de itera��o

	    ; Registradores para troca de contexto com interrup��o
	    W_TEMP
	    STATUS_TEMP
	    PCLATH_TEMP
	    endc

; Vetor de RESET
	    org		    H'0000'	; Origem no endere�o de mem�ria 0000h
	    goto	    inicio	; V� para label que indica inicio

; Vetor de Interrup��o
	    org		    H'0004'	    ; Endere�o do vetor de interrup��o
	    call	    salvarContexto  ; Salva contexto para tratar ISR
	    
	    btfss	    PIR1,TMR1IF	    ; Ocorreu overflow no Timer1?
	    goto	    exitISR	    ; N�o, saia da interrup��o
	    bcf		    PIR1,TMR1IF	    ; Limpa flag de interrup��o TMR1IF

	    decfsz	    timerCount, F   ; Decrementa timerCount. � zero?
	    goto	    exitISR	    ; N�o, sai da interrup��o

	    ; Trata ISR (Interrupt Service Routine)
	    ;movfw	    PORTA	    ; L� o conte�do da porta A
	    movlw	    B'00110010'	    ; Tempor�rio, o correto � a linha acima
	    andlw	    H'0F'	    ; S� interessam os �ltimos 4 d�gitos
	    movwf	    bcdAux	    ; Armazena o conte�do lido em bcdAux
	    sublw	    B'00001001'	    ; Subtrai 9 do valor lido
	    skip_wle			    ; Verifica se W � menor ou igual a 9
	    goto	    beforeExitISR   ; BCD inv�lido. Sai da interrup��o
	    goto	    tratarBCD	    ; Realiza o tratamento do BCD

beforeExitISR:
	    call	    clearTimerCount

exitISR:
	    call	    recuperarContexto	; Retorna contexto antes de ISR
	    retfie			; Retorna da interrup��o

; Programa principal
inicio:
	    bank1
	    ; Definir entrada e sa�da via TRISA e TRISB
	    movlw	    H'FF'	    ; W = B'11111111', pois 1 = input
	    movwf	    TRISA	    ; Porta A servir� como input	    
	    movlw	    H'00'	    ; W = B'00000000', pois 0 = output
	    movwf	    TRISB	    ; Porta B servir� como outinput
	    bsf		    PIE1,TMR1IE	    ; Habilita interrup��o via
					    ;	overflow de TMR1IE (Timer1)
	    bank0
	    bsf		    INTCON,GIE	    ; Habilita interrup��o geral
	    bsf		    INTCON,PEIE	    ; Habilita inter. de perif�ricos
	    movlw	    B'00110001'	    ; T1CON para interrup��o via Timer1
	    movwf	    T1CON	    ; com internal clock e prescaler 1:8
	    
	    call	    clearBCDCount	    
	    call	    clearTimerCount

loop:
	    goto	    $	    ; Loop infinito (vai para posi��o atual $)

clearBCDCount:
	    movlw	    D'2'	    ; Define W = 2 pois ser�o lidos 3
	    movwf	    bcdCounter	    ; n�meros BCD: unidade, dez. e cent.
	    return

clearTimerCount:
	    movlw	    D'10'
	    movwf	    timerCount
	    return

salvarContexto:
	    ; Salvar contexto (copiado do DataSheet do PIC)
	    MOVWF    W_TEMP           ;Copy W to TEMP register
	    SWAPF    STATUS,W         ;Swap status to be saved into W
	    CLRF     STATUS           ;bank 0, regardless of current bank,
				      ;Clears IRP,RP1,RP0
	    MOVWF    STATUS_TEMP      ;Save status to bank zero STATUS_TEMP
				      ;register
	    MOVF     PCLATH, W        ;Only required if using pages 1,2 and/or 3
	    MOVWF    PCLATH_TEMP      ;Save PCLATH into W
	    CLRF     PCLATH           ;Page zero, regardless of current page
	    return

recuperarContexto:
	    ; Recupera contexto (copiado do DataSheet do PIC)
	    MOVF     PCLATH_TEMP, W   ;Restore PCLATH
	    MOVWF    PCLATH           ;Move W into PCLATH
	    SWAPF    STATUS_TEMP,W    ;Swap STATUS_TEMP register into W
				      ;(sets bank to original state)
	    MOVWF    STATUS           ;Move W into STATUS register
	    SWAPF    W_TEMP,F         ;Swap W_TEMP
	    SWAPF    W_TEMP,W         ;Swap W_TEMP into W
	    return


tratarBCD:
	    movfw	    bcdCounter	    ; Armazena contador de BCD
	    sublw	    D'2'	    ; Substrai 2 de W
	    btfsc	    STATUS,Z	    ; Resultado igual a zero?
	    goto	    unidadeBCD	    ; Tratar unidade
	    movfw	    bcdCounter
	    sublw	    D'1'	    ; Substrai 1 de W
	    btfsc	    STATUS,Z	    ; Resultado igual a zero?
	    goto	    dezenaBCD	    ; Tratar dezena
	    goto	    centenaBCD	    ; Tratar centena

unidadeBCD:
	    movfw	    bcdAux	    ; Retorna BCD para W
	    movwf	    bcdAccumulator  ; Inicia BCD com unidade informada
	    goto	    sairTratamentoBCD	; unidade BCD tratada, saindo...

dezenaBCD:
	    movfw	    bcdAux
	    btfsc	    STATUS,Z		; Resultado igual a zero?
	    goto	    sairTratamentoBCD	; dezena BCD tratada, saindo...
	    movfw	    bcdAccumulator
	    addlw	    D'10'
	    movwf	    bcdAccumulator
	    decf	    bcdAux,F
	    goto	    dezenaBCD

centenaBCD:
	    movfw	    bcdAux
	    btfsc	    STATUS,Z		; Resultado igual a zero?
	    goto	    sairCentenaBCD	; dezena BCD tratada, saindo...
	    movfw	    bcdAccumulator
	    addlw	    D'100'
	    btfsc	    STATUS,C
	    goto	    overflowBCD		; Overflow centena. BCD > 255
	    movwf	    bcdAccumulator
	    decf	    bcdAux,F
	    goto	    centenaBCD

sairCentenaBCD:
	    call	    clearBCDCount
	    goto	    beforeExitISR

overflowBCD:
	    call	    clearBCDCount
	    goto	    beforeExitISR   ; Prosseguir execu��o

sairTratamentoBCD:
	    decf	    bcdCounter,F    ; Decrementa o contador BCD
	    goto	    beforeExitISR   ; Prosseguir execu��o

buscarPrimo:
	    ; Procurando n�mero primo
	    call	    lookupPrimos
	    subwf	    bcdAux,w
	    btfsc	    STATUS,Z
	    goto	    mostrarPrimo    ; Valor � igual
	    btfsc	    STATUS,C
	    nop				    ; bcdAux � maior que W
	    ; ...
	    INCFSZ	    W
	    goto	    beforeExitISR
	    goto	    buscarPrimo	    ; W � maior

mostrarPrimo:
	    movfw	    bcdAux
	    movwf	    PORTB

lookupPrimos:
	    addwf	PCL,F
	    retlw       D'2'
	    retlw       D'3'
	    retlw       D'5'
	    retlw       D'7'
	    retlw       D'11'
	    retlw       D'13'
	    retlw       D'17'
	    retlw       D'19'
	    retlw       D'23'
	    retlw       D'29'
	    retlw       D'31'
	    retlw       D'37'
	    retlw       D'41'
	    retlw       D'43'
	    retlw       D'47'
	    retlw       D'53'
	    retlw       D'59'
	    retlw       D'61'
	    retlw       D'67'
	    retlw       D'71'
	    retlw       D'73'
	    retlw       D'79'
	    retlw       D'83'
	    retlw       D'89'
	    retlw       D'97'
	    retlw       D'101'
	    retlw       D'103'
	    retlw       D'107'
	    retlw       D'109'
	    retlw       D'113'
	    retlw       D'127'
	    retlw       D'131'
	    retlw       D'137'
	    retlw       D'139'
	    retlw       D'149'
	    retlw       D'151'
	    retlw       D'157'
	    retlw       D'163'
	    retlw       D'167'
	    retlw       D'173'
	    retlw       D'179'
	    retlw       D'181'
	    retlw       D'191'
	    retlw       D'193'
	    retlw       D'197'
	    retlw       D'199'
	    retlw       D'211'
	    retlw       D'223'
	    retlw       D'227'
	    retlw       D'229'
	    retlw       D'233'
	    retlw       D'239'
	    retlw       D'241'
	    retlw       D'251'

	    end