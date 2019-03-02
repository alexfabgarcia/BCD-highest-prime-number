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

; Pula próxima instrução se W é menor ou igual ao valor do qual foi subtraído.
skip_wle    macro
	    btfss	    STATUS, C ; Skip if no borrow occurred
	    endm

; Registradores de uso geral
	    cblock	    H'20'
	    bcdCounter		; Registrador para contar BCDs informados
	    bcdAccumulator	; Registrador para acumular BCDs
	    timerCount		; Registrador auxiliar para tratar temporização
				; juntamente com interrupção do Timer0
	    bcdAux		; Auxilia na leitura de BCDs e primos (look-up)
	    iteracao		; Controle de iteração
	    primo		; Número primo encontrado

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
tratarISR:
	    movfw	    PORTA	    ; Lê o conteúdo da porta A
	    movwf	    bcdAux	    ; Armazena o conteúdo em bcdAux
	    sublw	    B'00001001'	    ; Subtrai 9 do valor lido
	    skip_wle			    ; Verifica se W é menor ou igual a 9
	    goto	    beforeExitISR   ; BCD inválido. Sai da interrupção
	    goto	    leituraBCD	    ; Realiza o tratamento do BCD

beforeExitISR:
	    call	    clearTimerCount ; Reinicia contador de tempo

exitISR:
	    call	    recuperarContexto	; Retorna contexto antes de ISR
	    retfie				; Retorna da interrupção

; Programa principal
inicio:
	    clrf	    PORTA
	    bank1
	    ; Definir entrada e saída via TRISA e TRISB
	    movlw	    H'06'
	    movwf	    ADCON1
	    movlw	    H'CF'	    ; W = B'11111111', pois 1 = input
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

; Subrotinas

clearBCDCount:
	    movlw	    D'2'	    ; Define W = 2 pois serão lidos 3
	    movwf	    bcdCounter	    ; números BCD: unidade, dez. e cent.
	    return

clearTimerCount:
	    movlw	    D'10'	    ; W = 10. Pode ser alterado
	    movwf	    timerCount	    ; para controle de tempo de leitura
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

reiniciarBCD:
	    call	    clearBCDCount   ; Reinicia contador de leitura BCD
	    goto	    beforeExitISR   ; Prosseguir execução

sairTratamentoBCD:
	    decf	    bcdCounter,F    ; Decrementa o contador BCD
	    goto	    beforeExitISR   ; Prosseguir execução

leituraBCD:
	    movfw	    bcdCounter		; Armazena contador de BCD
	    sublw	    D'2'		; Substrai 2 de W
	    btfsc	    STATUS,Z		; Resultado igual a zero?
	    goto	    leituraUnidadeBCD	; Tratar unidade
	    movfw	    bcdCounter		; Armazena contador de BCD
	    sublw	    D'1'		; Substrai 1 de W
	    btfsc	    STATUS,Z		; Resultado igual a zero?
	    goto	    leituraDezenaBCD	; Tratar dezena
	    goto	    leituraCentenaBCD	; Tratar centena

leituraUnidadeBCD:
	    movfw	    bcdAux		; Move dígito BCD para W
	    movwf	    bcdAccumulator	; Inicia BCD com unidade informada
	    goto	    sairTratamentoBCD	; unidade BCD tratada, saindo...

leituraDezenaBCD:
	    movfw	    bcdAux		; Move dígito BCD para W
	    btfsc	    STATUS,Z		; Resultado igual a zero?
	    goto	    sairTratamentoBCD	; dezena BCD tratada, saindo...
	    movfw	    bcdAccumulator	; Move acumulador para W
	    addlw	    D'10'		; Soma 10 em W
	    movwf	    bcdAccumulator	; Move W para acumulador
	    decf	    bcdAux,F		; Dec. número lido (iteração)
	    goto	    leituraDezenaBCD	; Continua leitura de dezena

leituraCentenaBCD:
	    movfw	    bcdAux		; Move BCD para W
	    btfsc	    STATUS,Z		; Resultado igual a zero?
	    goto	    posLeituraBCD	; centena BCD tratada, saindo...
	    movfw	    bcdAccumulator	; Move acumulador para W
	    addlw	    D'100'		; Soma 100 a W
	    btfsc	    STATUS,C		; Ocorreu overflow? BCD > 255
	    goto	    reiniciarBCD	; Sim, reinicia a leitura
	    movwf	    bcdAccumulator	; Move W para acumulador
	    decf	    bcdAux,F		; Dec. número lido (iteração)
	    goto	    leituraCentenaBCD	; Continua leitura de centena

posLeituraBCD:
	    ; Verifica se o número lido é 0 ou 1, invalidando prosseguimento
	    movfw	    bcdAccumulator  ; Coloca o número lido em W
	    sublw	    D'1'	    ; Subtrai W de 1
	    btfsc	    STATUS,C	    ; O valor lido é 0 ou 1?
	    goto	    reiniciarBCD    ; Sim, sem número primo. Reinicia

	    ; Busca primo
	    call	    buscarPrimo	    ; Busca primo <= bcdAccumulator
	    movfw	    primo
	    movwf	    bcdAux
	    
	    ; Acumula dígito de centena em bcdAccumulator e envia para PORTB
	    movlw	    H'00'
	    movwf	    bcdAccumulator
	    call	    encontraCentenaBCD
	    movfw	    bcdAccumulator
	    movwf	    PORTB
	    
	    ; Acumula dígito de dezena em bcdAccumulator e envia para PORTB
	    movlw	    H'00'
	    movwf	    bcdAccumulator
	    call	    encontraDezenaBCD
	    movfw	    bcdAccumulator
	    movwf	    PORTB

	    ; Escreve na porta B a(s) unidade(s) restante(s) em bcdAux
	    movfw	    bcdAux		; Copia unidade(s) para W
	    movwf	    PORTB		; Move unidade(s) para porta B
	    
	    call	    clearBCDCount
	    goto	    beforeExitISR

; Armanazena em bcdAccumulator o dígito de centena BCD
encontraCentenaBCD:
	    movlw	    D'100'		; Define literal 100 em W
	    subwf	    bcdAux,W		; W = bcdAux - 100
	    btfss	    STATUS,C		; bcdAux é menor que 100?
	    return				; Sim, para acúmulo
	    movwf	    bcdAux		; bcdAux = bcdAux - 100
	    incf	    bcdAccumulator,F	; Incrementa acumulador
	    goto	    encontraCentenaBCD	; Continua busca

; Armanazena em bcdAccumulator o dígito de dezena BCD
encontraDezenaBCD:
	    movlw	    D'10'		; Define literal 100 em W
	    subwf	    bcdAux,W		; W = bcdAux - 10
	    btfss	    STATUS,C		; bcdAux é menor que 10?
	    return				; Sim, para acúmulo
	    movwf	    bcdAux		; bcdAux = bcdAux - 10
	    incf	    bcdAccumulator,F	; Incrementa acumulador
	    goto	    encontraDezenaBCD	; Continua busca

; Busca número primo na tabela de look-up e armazena no registrador 'primo'
buscarPrimo:
	    ; Procurando número primo
	    movlw	    D'53'	    ; 54: qnt. de números primos até 255
loopBuscaPrimo:
	    movwf	    iteracao	    ; Controla iteração
	    call	    lookupPrimos    ; Busca primo na posição 'iteracao'
	    movwf	    primo	    ; Armazena W em primo
	    subwf	    bcdAccumulator,W; Subtrai o W do número primo
	    btfsc	    STATUS,C	    ; W é menor ou igual ao n. primo?
	    return			    ; Sim, encontrado. Retorna.
	    decf	    iteracao,W	    ; Decrementa iteração.
	    goto	    loopBuscaPrimo  ; Continua busca

; Tabela look-up números primos
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