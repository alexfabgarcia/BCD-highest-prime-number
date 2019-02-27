; Projeto 2: Programação com PIC - Laboratório de Microcontroladores
; Escrever um programa que leia 3 dígitos BCD da porta A, convertendo-os para
; um único número binário de 8 bits. Como saída o programa deve apresentar na
; porta B, em sequência, 1 dígito BCD (para centena), seguido de 2 dígitos BCD
; (dezenae unidade), correspondendo ao maior número primo que seja menor ou
; igual ao valor de entrada. O programa deve ser desenvolvido e executado na
; plataforma MPlab (versão IDE X), usando o PIC16F873A.
;
; Autores: Alex Fabiano Garcia e Thales Marcel
; Data: 28/02/2019

    list   P=PIC16F873A	    ; Microcontrolador utilizado: PIC16F873A

; Arquivos incluídos no código
    #include <p16f873a.inc>

; FUSE bits: Cristal, sem watchdog timer, com Power Up timer (72 ms) e 
; sem Code protection
	    __config _XT_OSC & _WDT_OFF & _PWRTE_OFF & _CP_OFF

; Troca de banco de memória
bank0	    macro ; Selecionar o banco 0 da memória
	    bcf		STATUS, RP0
	    bcf		STATUS, RP1
	    endm

bank1	    macro ; Selecionar o banco 1 da memória
	    bsf		STATUS, RP0
	    bcf		STATUS, RP1
	    endm

; Entradas
;    #define	

; Vetor de RESET
	    org		H'0000'	; Origem no endereço de memória 0000h
	    goto	inicio	; Vá para label que indica inicio

; Vetor de Interrupção
	    org		H'0004'	; Endereço do vetor de interrupção
	    retfie		; Retorna da interrupção

; Programa principal
inicio:
	    ; Definir entrada e saída via TRISA e TRISB
	    bank1
	    movlw	H'FF'	    ; W = B'11111111', pois 1 = input
	    movwf	TRISA	    ; Porta A servirá como input	    
	    movlw	H'00'	    ; W = B'00000000', pois 0 = output
	    movwf	TRISB	    ; Porta B servirá como outinput
	    bank0
	    
	    movlw	H'FF'
	    movwf	PORTA

loop:
	    
	    ;goto	$	    ; Loop infinito (vai para posição atual $)

	    end