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
; Freq: 1 MHz
; Ciclo de máquina: 1/(Freq. Cristal / 4) = 
;

    list   P=PIC16F873A	    ; Microcontrolador utilizado: PIC16F873A

; Arquivos incluídos no código
    #include <p16f873a.inc>

; FUSE bits: Cristal, sem watchdog timer, com Power Up timer (72 ms) e 
; sem Code protection
	    __config _XT_OSC & _WDT_OFF & _PWRTE_OFF & _CP_OFF

; Troca de banco de memória
bank0	    macro ; Selecionar o banco 0 da memória
	    bcf		    STATUS, RP0
	    bcf		    STATUS, RP1
	    endm

bank1	    macro ; Selecionar o banco 1 da memória
	    bsf		    STATUS, RP0
	    bcf		    STATUS, RP1
	    endm

; Registradores de uso geral
	    ;bcd_count	    equ	    H'20'
	    cblock	    H'20'
	    bcdCounter		; H'20' Registrador para contar BCDs informados
	    digitoUnidade	; H'21' Registrador para dígito de unidade
	    digitoDezena	; H'22' Registrador para dígito de dezena
	    digitoCentena	; H'23' Registrador para dígito de centena
	    timerCount		; Registrador auxiliar para tratar temporização
				; juntamente com interrupção do Timer0
	    
	    ; Registradores para troca de contexto com interrupção
	    W_TEMP
	    STATUS_TEMP
	    PCLATH_TEMP
	    endc

; Vetor de RESET
	    org		    H'0000'	; Origem no endereço de memória 0000h
	    goto	    inicio	; Vá para label que indica inicio

; Vetor de Interrupção
	    org		    H'0004'	; Endereço do vetor de interrupção
	    
	    call	    salvarContexto  ; Salva contexto para tratar ISR
	    
	    btfss	    INTCON,TMR0IF   ;Ocorreu overflow no Timer0?
	    goto	    exitISR	    ;Não, saia da interrupção
	    
	    bcf		    INTCON,TMR0IF   ; Limpa flag de interrupção TRM0
	    decfsz	    timerCount, F   ; Decrementa timerCount. É zero?
	    goto	    exitISR	    ; Não, saisa da interrupção
	    
	    ; Trata ISR (Interrupt Service Routine)
	    nop
	    
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
	    ; Definir OPTION_REG para interrupção via Timer0
	    movlw	    B'11000111'	    ; Options: internal clock, timer0
	    movwf	    OPTION_REG	    ; prescaler 1:256 (111 menos sig.)
	    bsf		    INTCON,GIE	    ; Habilita interrupção do Timer0
	    bsf		    INTCON,TMR0IE   ; Habilita interrupção do Timer0
	    bank0
	    
	    movlw	    D'250'
	    movwf	    timerCount
	    clrf	    TMR0

loop:
	    goto	    loop
	    ;goto	    $	    ; Loop infinito (vai para posição atual $)

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

	    end