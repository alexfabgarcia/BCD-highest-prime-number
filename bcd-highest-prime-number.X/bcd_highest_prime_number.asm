; Projeto 2: Programação com PIC - Laboratório de Microcontroladores
; Escrever um programa que leia 3 dígitos BCD da porta A, convertendo-os para
; um único número binário de 8 bits. Como saída o programa deve apresentar na
; porta B, em sequência, 1 dígito BCD (para centena), seguido de 2 dígitos BCD
; (dezena e unidade), correspondendo ao maior número primo que seja menor ou
; igual ao valor de entrada. O programa deve ser desenvolvido e executado na
; plataforma MPlab (versão IDE X), usando o PIC16F873A.
;
; Autores: Alex Fabiano Garcia e Thales Marcel
; Data: 28/02/2019
; 
; Freq.: 1 MHz (simulador)
;
; A implementação utiliza interrupção através do Timer 1 do PIC16F873A. A
; interrupção ocorre com o overflow de TMR1H, sendo esta tratada somente quando
; o registrador 'timerCount', decrementado a cada interrupção, chega a zero.
; Esta estratégia foi utilizada para ler a porta A de tempo em tempo sem
; utilizar o pino da porta B como entrada, pois este é utilizado como saída.
; Neste intervalo de interrupção, um estímulo pode ser causado para alterar
; os valores contidos na porta A que representam o BCD.

    list   P=PIC16F873A	    ; Microcontrolador utilizado: PIC16F873A

; Arquivos incluídos no código
    #include <p16f873a.inc>

; FUSE bits: Cristal, sem watchdog timer, com Power Up timer (72 ms) e 
; sem Code protection
	    __config _XT_OSC & _WDT_OFF & _PWRTE_OFF & _CP_OFF

; Macros
; Selecionar o banco 0 da memória
bank0	    macro
	    bcf		    STATUS, RP0
	    bcf		    STATUS, RP1
	    endm

; Selecionar o banco 1 da memória
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
	    timerCount		; Registrador auxiliar para tratar temporização
				; juntamente com interrupção do Timer0
	    bcdAux		; Auxilia na busca por BCDs na tabela look-up
	    iteracao		; Controle de iteração

	    ; Registradores para troca de contexto com interrupção
	    W_TEMP
	    STATUS_TEMP
	    PCLATH_TEMP
	    endc

; Vetor de RESET
	    org		    H'0000'	; Origem no endereço de memória 0000h
	    goto	    inicio	; Vá para label que indica inicio

; Vetor de Interrupção
	    org		    H'0004'	    ; Endereço do vetor de interrupção
	    call	    salvarContexto  ; Salva contexto para tratar ISR
	    
	    btfss	    PIR1,TMR1IF	    ; Ocorreu overflow no Timer1?
	    goto	    exitISR	    ; Não, saia da interrupção
	    bcf		    PIR1,TMR1IF	    ; Limpa flag de interrupção TMR1IF

	    decfsz	    timerCount, F   ; Decrementa timerCount. É zero?
	    goto	    exitISR	    ; Não, sai da interrupção

	    ; Trata ISR (Interrupt Service Routine)
	    ;movfw	    PORTA	    ; Lê o conteúdo da porta A
	    movlw	    B'00110010'	    ; Temporário, o correto é a linha acima
	    andlw	    H'0F'	    ; Só interessam os últimos 4 dígitos
	    movwf	    bcdAux	    ; Armazena o conteúdo lido em bcdAux
	    sublw	    B'00001001'	    ; Subtrai 9 do valor lido
	    skip_wle			    ; Verifica se W é menor ou igual a 9
	    goto	    beforeExitISR   ; BCD inválido. Sai da interrupção
	    goto	    tratarBCD	    ; Realiza o tratamento do BCD

beforeExitISR:
	    call	    clearTimerCount

exitISR:
	    call	    recuperarContexto	; Retorna contexto antes de ISR
	    retfie			; Retorna da interrupção

; Programa principal
inicio:
	    bank1
	    ; Definir entrada e saída via TRISA e TRISB
	    movlw	    H'FF'	    ; W = B'11111111', pois 1 = input
	    movwf	    TRISA	    ; Porta A servirá como input	    
	    movlw	    H'00'	    ; W = B'00000000', pois 0 = output
	    movwf	    TRISB	    ; Porta B servirá como outinput
	    bsf		    PIE1,TMR1IE	    ; Habilita interrupção via
					    ;	overflow de TMR1IE (Timer1)
	    bank0
	    bsf		    INTCON,GIE	    ; Habilita interrupção geral
	    bsf		    INTCON,PEIE	    ; Habilita inter. de periféricos
	    movlw	    B'00110001'	    ; T1CON para interrupção via Timer1
	    movwf	    T1CON	    ; com internal clock e prescaler 1:8
	    
	    call	    clearBCDCount	    
	    call	    clearTimerCount

loop:
	    goto	    $	    ; Loop infinito (vai para posição atual $)

clearBCDCount:
	    movlw	    D'2'	    ; Define W = 2 pois serão lidos 3
	    movwf	    bcdCounter	    ; números BCD: unidade, dez. e cent.
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
	    goto	    beforeExitISR   ; Prosseguir execução

sairTratamentoBCD:
	    decf	    bcdCounter,F    ; Decrementa o contador BCD
	    goto	    beforeExitISR   ; Prosseguir execução

buscarPrimo:
	    ; Procurando número primo
	    call	    lookupPrimos
	    subwf	    bcdAux,w
	    btfsc	    STATUS,Z
	    goto	    mostrarPrimo    ; Valor é igual
	    btfsc	    STATUS,C
	    nop				    ; bcdAux é maior que W
	    ; ...
	    INCFSZ	    W
	    goto	    beforeExitISR
	    goto	    buscarPrimo	    ; W é maior

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