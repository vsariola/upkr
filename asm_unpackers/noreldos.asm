; This is the 16-bit DOS x86 decompression stub for upkr, which decompresses the
; code starting at address 0x3FFE (or whatever is defined by the entrypoint
; below). Thus, the packed code needs to be assembled with org 0x3FFE to work.
;
; How to use:
;   1) Put POPA as the first instruction of your compiled code and use org
;      0x3FFE
;   2) Pack your intro using upkr into data.bin with the --x86 command line
;      argument:
;           $ upkr --x86 intro.com data.bin
;   2) Compile this .asm file using n asm (or any compatible assembler) e.g.
;           $ nasm upkr_dos.asm -fbin -o intropck.com

; In specific cases, the unpacker stub can be further optimized to save a byte
; or two:
;   1) If your stub+compressed code is 2k or smaller, you can save 1 byte by
;      putting probs at 0x900 and initializing DI with SALC; XCHG AX, DI instead
;      of MOV DI, probs
;   2) If you remove the PUSHA (and POPA in the compressed code), then you can
;      assume the registers as follows: AX = 0x00XX, BX = probs + 0x1XX, CX = 0
;      DX = (trash), SI = DI = right after your program, SP = as it was when the
;      program started, flags = carry set
;
; Note that even with the PUSHA / POPA, carry will be set (!) unlike normal dos
; program.

entry     equ 3FFEh
probs     equ entry - 0x1FE     ; must be aligned to 256

org 100h

upkr_unpack:
    pusha
    xchg ax, bp                             ; position in bitstream = 0
    cwd                                     ; upkr_state = 0;
    mov  di, probs
    mov  ax, 0x8080                         ; for(int i = 0; i < sizeof(upkr_probs); ++i) upkr_probs[i] = 128;
    rep  stosw
    push di
.mainloop:
    mov  bx, probs
    call upkr_decode_bit
    jc   .else                  ; if(upkr_decode_bit(0)) {
    mov  bh, (probs+256)/256
    jcxz   .skip_call           ; if(prev_was_match || upkr_decode_bit(257)) {
    call upkr_decode_bit
    jc   .skipoffset
.skip_call:
    stc
    call upkr_decode_length     ;  offset = upkr_decode_length(258) - 1;
    mov  si, di
    loop .sub                   ; if(offset == 0)
    ret
.sub:
    dec si
    loop .sub
.skipoffset:
    mov  bl, 128                  ; int length = upkr_decode_length(384);
    call upkr_decode_length
    rep  movsb                    ; *write_ptr = write_ptr[-offset];
    jmp  .mainloop
.byteloop:
    call upkr_decode_bit     ; int bit = upkr_decode_bit(byte);
.else:
    adc  bl, bl              ; byte = (byte << 1) + bit;
    jnc  .byteloop
    xchg ax, bx
    stosb
    inc   si
    mov  cl, 1
    jmp  .mainloop               ;  prev_was_match = 0;


; parameters:
;    bx = context_index
;    dx = state
;    si = bit position in input stream
; returns:
;    bx = context_index+1
;    dx = new state
;    si = new bit position in input stream
;    carry = bit
upkr_load_bit:
     bt   [compressed_data], bp
     inc  bp
     adc  dx, dx
upkr_decode_bit:
     inc  dx
     dec  dx ; or whatever other test for the top bit there is
     jns  upkr_load_bit
     movzx ax, byte [bx]                     ; int prob = upkr_probs[context_index]
     push ax                                 ; save prob
     cmp  dl, al                             ; int bit = (upkr_state & 255) < prob ? 1 : 0; (carry = bit)
     pushf                                   ; save bit flags
     jc   .bit                               ; (skip if bit)
          neg  al                            ;   tmp = 256 - tmp;
     .bit:
     mov  [bx], al                            ; tmp_new = tmp + (256 - tmp + 8) >> 4;
     neg  byte [bx]
     shr  byte [bx], 4
     adc  [bx], al
     mul  dh                                 ; upkr_state = tmp * (upkr_state >> 8) + (upkr_state & 255);
     mov  dh, 0
     add  dx, ax
     popf
     pop  ax
     jc   .bit2                              ; (skip if bit)
          neg  byte [bx]                     ;    tmp = 256 - tmp;
          sub  dx, ax                        ;    upkr_state -= prob; note that this will also leave carry always unset, which is what we want
     .bit2:
     ret                                     ; flags = bit

; parameters:
;    bx = context_index
;    si = bit position in input stream
; returns:
;    cx = length
; trashes bl, ax
upkr_decode_length_loop:
          inc  bx
          call upkr_decode_bit
upkr_decode_length:
          rcr  cx, 1
          inc  bx
          call  upkr_decode_bit
          jnc  upkr_decode_length_loop ; while(upkr_decode_bit(context_index)) {
     .loop2:
          rcr  cx, 1
          jnc  .loop2
     ret

compressed_data:
    incbin "data.bin"