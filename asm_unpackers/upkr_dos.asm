; supports only the bitstream data (add -b to command line when packing)
; put the packed intro into data.bin
prog_start     equ 0xC000
prog_len       equ 0x3100 ; must be divisible by 8
probs          equ prog_start+prog_len ; must be divisible by 256

org prog_start

entrypoint:    ; this is will be loaded at 0x100, but relocates the code and data to prog_start
     push si   ; si points to 0x100 = start of code, save it and return to it later with ret
     pusha
     push si
     mov  di, prog_start
     mov  cx, prog_len
     rep  movsb
     push upkr_unpack
     ret

upkr_unpack:
     xchg ax, dx                             ; upkr_state = 0;
     mov  al, 128                            ; for(int i = 0; i < sizeof(upkr_probs); ++i) upkr_probs[i] = 128;
     mov  ch, 2                              ; cx = 0x0200
     rep  stosb
     pop  di                                 ; u8* write_ptr = (u8*)destination;
     xor  bp, bp
     .mainloop:
          mov  bx, probs
          call upkr_decode_bit
          jnc  .else                         ; if(upkr_decode_bit(0)) {
               mov  bh, (probs+256)/256
               jcxz .skip_call
               call upkr_decode_bit
               jnc  .skipoffset
                    .skip_call:
                    call upkr_decode_length  ;  offset = upkr_decode_length(258) - 1;
                    loop .notdone            ; if(offset == 0)
                         popa
                         ret
                    .notdone:
                    mov  si, di
                    sub  si, cx
               .skipoffset:
               mov  bl, 128                  ; int length = upkr_decode_length(384);
               call upkr_decode_length
               rep  movsb                    ; *write_ptr = write_ptr[-offset];
               jmp  .mainloop
          .else:
               inc bx
               .byteloop:
                    call upkr_decode_bit     ; int bit = upkr_decode_bit(byte);
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
upkr_decode_bit_bxplus1:
     inc  bx
upkr_decode_bit:
     push ax
     push cx
     shr  dx, 1                              ; for the first round, the shr cancels the adc dx, dx and we just check the sign of dx
     jmp  .looptest
     .bitloop:                               ; while(upkr_state < 32768)
          bt   [compressed_data], bp
          inc  bp
          .looptest:
          adc  dx, dx
          jns  .bitloop
     movzx ax, byte [bx]                     ; int prob = upkr_probs[context_index]
     push ax                                 ; save prob
     cmp  dl, al                             ; int bit = (upkr_state & 255) < prob ? 1 : 0; (carry = bit)
     pushf                                   ; save bit flags
     jc   .bit                               ; (skip if bit)
          neg  al                            ;   tmp = 256 - tmp;
     .bit:
     push ax
     mul  dh                                 ; upkr_state = tmp * (upkr_state >> 8) + (upkr_state & 255);
     mov  dh, 0
     add  dx, ax
     pop  cx
     mov  ax, cx                             ; tmp += (256 - tmp + 8) >> 4;
     neg  al
     shr  ax, 4
     adc  ax, cx
     popf
     pop  cx
     jc   .bit2                              ; (skip if bit)
          neg  al                            ;    tmp = 256 - tmp;
          sub  dx, cx                        ;    upkr_state -= prob; note that this will also leave carry always unset, which is what we want
     .bit2:
     mov  [bx], al                           ; upkr_probs[context_index] = tmp;
     pop  cx
     pop  ax
     ret                                     ; flags = bit

; parameters:
;    bx = context_index
;    si = bit position in input stream
; returns:
;    cx = length
; trashes bl, ax
upkr_decode_length:
     xor  ax, ax                             ; int length = 0;
     xor  cx, cx
     .loop:
          call upkr_decode_bit_bxplus1
          jnc  .end                          ; while(upkr_decode_bit(context_index)) {
          call upkr_decode_bit_bxplus1
          rcr  ax, 1
          inc  cx
          jmp  .loop
     .end:
     inc  ax                                 ; length |= highest bit
     rol  ax, cl
     xchg  cx, ax
     ret

compressed_data:
    incbin "data.bin"