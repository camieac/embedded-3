;Cameron A. Craig cac30@hw.ac.uk H00131274

title     "Assignment3: ADC and PWM interfacing."
list      p=16f877a
include   "p16f877a.inc"
;         Cameron A. Craig cac30@hw.ac.uk H00131274
;**************************************************************
; '__CONFIG' directive is used to embed configuration
; data within .asm file. The labels following the directive
; are located in the respective .inc file. See respective
; data sheet for additional information on configuration word.
; Remember there are TWO underscore characters before the
; word CONFIG.
;**************************************************************

__CONFIG _CP_OFF & _WDT_OFF & _BODEN_OFF & _PWRTE_ON & _HS_OSC & _WRT_OFF & _LVP_OFF & _CPD_OFF

;**************************************************************
; Title
;    es3.asm
;    Generate LED bar and speaker frequency based on LDR value.
;
; Description
;    - The higher the light level, the higher the speaker frequency.
;    - The higher the light level, the more LEDs are on.
;
; Method
;    1. Read ADC value
;    2. Compare result with literals to turn on LEDs based
;       on light level detected by LDR.
;    3. If button is pressed, set PWM frequency depending
;       on the number of LEDs turned on.
;    4. Goto 1
;
; Version
;    Cameron Craig   V2.0    December 2015
;
; History
;    V0.1 26/11/2015
;    V0.9 01/12/2015
;    V1.0 03/12/2015
;    V2.0 04/12/2015
;**************************************************************

;**************************************************************
; Variable declarations
; User RAM area starts at location h'20' for PIC16F877a
;**************************************************************
        ; Variables used for context saving
        w_temp          equ     h'7D'
        status_temp     equ     h'7E'
        pclath_temp     equ     h'7F'

        pwm_counter     equ     h'22'
        counter10us     equ     h'20'

;**************************************************************
; Initial system vector
;**************************************************************

        org     h'00'                   ; initialise system restart vector
        clrf    STATUS
        clrf    PCLATH                  ; needed for TinyBootloader functionality
        goto    start

;**************************************************************
; Start of program space
;**************************************************************

        org     h'05'

;**************************************************************
; Initialize program space and variables
;**************************************************************

Init
        bsf     STATUS, RP0        ; enable page 1 register set
        bcf     STATUS, RP1

        movlw   b'00001111'
        movwf   ADCON1

        movlw   b'111111'
        movwf   TRISA
        movlw   b'00000000'
        movwf   TRISB
        movlw   b'00000000'
        movwf   TRISC
        movlw   b'11111111'
        movwf   TRISD


        bcf     STATUS, RP0        ; back to page 0 register set

        movlw   b'01000001'
        movwf   ADCON0

        movlw   b'00000000'
        movwf   PORTB

        return

;**************************************************************
; Wait for 20 microseconds, this is the acquisition time for ADC
; holding capacitor. This ensures the capacitor has enough time
; to charge in proportion to the input voltage.
;**************************************************************
delay20us:
        movlw D'6'
        movwf counter10us
        loop20us:
            decfsz  counter10us, 1
            goto    loop20us
        return

;**************************************************************
; Poll ADCON0 until it has been set to zero, indicating the ADC
; module is complete.
;**************************************************************
poll_adc
        btfsc ADCON0,   2
        goto  poll_adc
return

;**************************************************************
; Use the 8 most significant bits of the 10 bit ADC value to
; work out what LEDs to turn on.
; For each LED that is set to on, the pwm value counter is
; incremented.
; The PWM counter is then set to a value between 0 and 7, which
; is later used to work out the speaker frequency.
; *************************************************************
; Outputs: pwm_counter
;**************************************************************

set_leds_prepare_speaker:
        ;Clear the pwm value counter
        clrf    pwm_counter

        movf    ADRESH,      w
        addlw   d'227'
        btfss   STATUS,      0
        bcf     PORTB,       0
        btfsc   STATUS,      0
        bsf     PORTB,       0
        btfsc   STATUS,      0
        incf    pwm_counter, 1


        movf    ADRESH,      w
        addlw   d'199'
        btfss   STATUS,      0
        bcf     PORTB,       1
        btfsc   STATUS,      0
        bsf     PORTB,       1
        btfsc   STATUS,      0
        incf    pwm_counter, 1


        movf    ADRESH,      w
        addlw   d'171'
        btfss   STATUS,      0
        bcf     PORTB,       2
        btfsc   STATUS,      0
        bsf     PORTB,       2
        btfsc   STATUS,      0
        incf    pwm_counter, 1

        movf    ADRESH,      w
        addlw   d'143'
        btfss   STATUS,      0
        bcf     PORTB,       3
        btfsc   STATUS,      0
        bsf     PORTB,       3
        btfsc   STATUS,      0
        incf    pwm_counter, 1


        movf    ADRESH,      w
        addlw   d'115'
        btfss   STATUS,      0
        bcf     PORTB,       4
        btfsc   STATUS,      0
        bsf     PORTB,       4
        btfsc   STATUS,      0
        incf    pwm_counter, 1

        movf    ADRESH,      w
        addlw   d'87'
        btfss   STATUS,      0
        bcf     PORTB,       5
        btfsc   STATUS,      0
        bsf     PORTB,       5
        btfsc   STATUS,      0
        incf    pwm_counter, 1

        movf    ADRESH,      w
        addlw   d'59'
        btfss   STATUS,      0
        bcf     PORTB,       6
        btfsc   STATUS,      0
        bsf     PORTB,       6
        btfsc   STATUS,      0
        incf    pwm_counter, 1

        movf    ADRESH,      w
        addlw   d'31'
        btfss   STATUS,      0
        bcf     PORTB,       7
        btfsc   STATUS,      0
        bsf     PORTB,       7
        btfsc   STATUS,      0
        incf    pwm_counter, 1


        return

;**************************************************************
; Select the appropriate speaker PWM frequency based on the
; counter previously set by set_leds_prepare_speaker.
; The counter value is XOR'd with 0 - 7 to check it's value.
; *************************************************************
; Inputs: pwm_counter
;**************************************************************

set_pwm_frequency:

        movlw   d'0'
        xorwf   pwm_counter,  0
        btfsc   STATUS,       2
        call    pwm1

        movlw   d'1'
        xorwf   pwm_counter,  0
        btfsc   STATUS,       2
        call    pwm2

        movlw   d'2'
        xorwf   pwm_counter,  0
        btfsc   STATUS,       2
        call    pwm3

        movlw   d'3'
        xorwf   pwm_counter,  0
        btfsc   STATUS,       2
        call    pwm4

        movlw   d'4'
        xorwf   pwm_counter,  0
        btfsc   STATUS,       2
        call    pwm5

        movlw   d'5'
        xorwf   pwm_counter,  0
        btfsc   STATUS,       2
        call    pwm6

        movlw   d'6'
        xorwf   pwm_counter,  0
        btfsc   STATUS,       2
        call    pwm7

        movlw   d'7'
        xorwf   pwm_counter,  0
        btfsc   STATUS,       2
        call    pwm8

        movlw   d'8'
        xorwf   pwm_counter,  0
        btfsc   STATUS,       2
        call    pwm9

        return

;**************************************************************
; Routines to set the PWM frequency of the PWM submodule.
; Values are calculated using:
; http://www.micro-examples.com/public/microex-navig/doc/097-pwm-calculator.html
; 9 different frequencies are used, representing 0 to 8 LEDs being on.
;**************************************************************

; 290Hz
pwm1:
        bsf      STATUS,       RP0
        movlw    b'11001000'
        movwf    PR2
        bcf      STATUS,       RP0
        movlw    b'00000111'
        movwf    T2CON
        movlw    b'01100100'
        movwf    CCPR1L
        movlw    b'00011100'
        movwf    CCP1CON
return

; 590Hz
pwm2:
        bsf      STATUS,       RP0
        movlw    b'01101001'
        movwf    PR2
        bcf      STATUS,       RP0
        movlw    b'00000111'
        movwf    T2CON
        movlw    b'00110100'
        movwf    CCPR1L
        movlw    b'00011100'
        movwf    CCP1CON
return
; 890Hz
pwm3:
        bsf      STATUS,       RP0
        movlw    b'01000101'
        movwf    PR2
        bcf      STATUS,       RP0
        movlw    b'00000111'
        movwf    T2CON
        movlw    b'00100010'
        movwf    CCPR1L
        movlw    b'00111100'
        movwf    CCP1CON
return
; 1190Hz
pwm4:
        bsf      STATUS,       RP0
        movlw    b'00110100'
        movwf    PR2
        bcf      STATUS,       RP0
        movlw    b'00000111'
        movwf    T2CON
        movlw    b'00011010'
        movwf    CCPR1L
        movlw    b'00011100'
        movwf    CCP1CON
return
; 1490Hz
pwm5:
        bsf      STATUS,       RP0
        movlw    b'00101001'
        movwf    PR2
        bcf      STATUS,       RP0
        movlw    b'00000111'
        movwf    T2CON
        movlw    b'00010100'
        movwf    CCPR1L
        movlw    b'00111100'
        movwf    CCP1CON
return
; 1790Hz
pwm6:
        bsf      STATUS,       RP0
        movlw    b'00100010'
        movwf    PR2
        bcf      STATUS,       RP0
        movlw    b'00000111'
        movwf    T2CON
        movlw    b'00010001'
        movwf    CCPR1L
        movlw    b'00011100'
        movwf    CCP1CON
return

; 2090Hz
pwm7:
        bsf      STATUS,       RP0
        movlw    b'00011101'
        movwf    PR2
        bcf      STATUS,       RP0
        movlw    b'00000111'
        movwf    T2CON
        movlw    b'00001110'
        movwf    CCPR1L
        movlw    b'00111100'
        movwf    CCP1CON
return

; 2200Hz
pwm8:
        bsf      STATUS,       RP0
        movlw    b'01110001'
        movwf    PR2
        bcf      STATUS,       RP0
        movlw    b'00000101'
        movwf    T2CON
        movlw    b'00111000'
        movwf    CCPR1L
        movlw    b'00111100'
        movwf    CCP1CON
return

; 2400Hz
pwm9:
        bsf      STATUS,       RP0
        movlw    b'00011001'
        movwf    PR2
        bcf      STATUS,       RP0
        movlw    b'00000111'
        movwf    T2CON
        movlw    b'00001100'
        movwf    CCPR1L
        movlw    b'00111100'
        movwf    CCP1CON
return


;**************************************************************
; Main routine
;**************************************************************

start
    call    Init

ldr_loop:
    ;Delay to allow holding capacitor enough time to charge.
    call delay20us

    ;Start the ADC read
    bsf  ADCON0,2

    ;Wait until the ADC read is complete
    call poll_adc

    ;Set the leds and prepare set frequency counter.
    call set_leds_prepare_speaker

    ;If the switch is pressed, set speaker PWM frequency.
    btfsc   PORTA,1
    call    set_pwm_frequency
    btfss   PORTA,1
    clrf    CCP1CON
    ;Repeat forever
    goto ldr_loop

    END
