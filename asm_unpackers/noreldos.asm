; pack your intro using upkr (add --x86 command line argument)
; put the packed intro into data.bin
; the code is not relocated, so the intro must be assembled at entry point i.e. org 3FFEh
; the first instruction of the packed intro should be popa, to get the register defaults back
entry     equ 3FFEh
probs     equ entry - 0x1FE;  must be aligned to 256

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
          jc   .else                         ; if(upkr_decode_bit(0)) {
               mov  bh, (probs+256)/256
               jcxz   .skip_call             ; if(prev_was_match || upkr_decode_bit(257)) {
               call upkr_decode_bit
               jnc  .skipoffset
                    .skip_call:
                    call upkr_decode_length  ;  offset = upkr_decode_length(258) - 1;
                    loop .notdone            ; if(offset == 0)
                         ret
                    .notdone:
                    mov  si, di
                    sub  si, cx
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
               loop  .mainloop               ;  prev_was_match = 0;


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
     mov  [bx],al
     mul  dh                                 ; upkr_state = tmp * (upkr_state >> 8) + (upkr_state & 255);
     mov  dh, 0
     add  dx, ax
     mov  al, [bx]                           ; tmp += (256 - tmp + 8) >> 4;
     neg  al
     shr  al, 4
     adc  [bx], al                           ; upkr_probs[context_index] = tmp;
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
upkr_decode_length:
     mov  cx, 0x8000
     .loop:
          inc  bx
          call upkr_decode_bit
          jc   .end                          ; while(upkr_decode_bit(context_index)) {
          inc  bx
          call upkr_decode_bit
          rcr  cx, 1
          jmp  .loop
     .end:
     .loop2:
          rcr  cx, 1
          jnc  .loop2
     ret

compressed_data:
    incbin "data.bin"