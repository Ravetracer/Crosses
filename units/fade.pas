{$R-} {Range checking off (Helps the fade speed)}
{$G+} {286 instructions must be enabled}
unit Fade;

interface
uses x320x200;

 procedure BlackenPalette;
   {Blackens all colors from First to Last.   Make sure you have the}
   {palette saved in a variable.}
 procedure FadeIn(Pal: dac_block);
   {AProc is called each palette step}
 procedure FadeOut(Pal: dac_block);
   {AProc is called each palette step}

    implementation

    const first = 1;
          last  = 255;
          speed = 32;

 procedure SetPalette(Pal: dac_block; First, Last: word); assembler;
     asm
  MOV   DX, 03DAh
       @Rt:
  IN    AL, DX       { wait for no retrace                 }
  TEST  AL, 8        { this bit is high during a retrace   }
  JZ    @Rt          { so loop until it goes high          }

  MOV   CX, [Last]   { CX = last colour to set             }
  MOV   AX, [First]  { AX = first colour to set            }
  SUB   CX, AX
  INC   CX           { CX = number of colours to set       }
  MOV   DX, 03C8h    { Palette Address register            }
  {CLI}
  OUT   DX, AL       { set starting register               }
  INC   DX           { Palette Data register               }
  PUSH  DS
  LDS   SI, [Pal]    { DS:SI -> palette                    }
  ADD   SI, AX
  ADD   SI, AX
  ADD   SI, AX       { DS:SI -> first entry to set         }
  MOV   AX, CX       { triple the value in CX              }
  ADD   CX, AX
  ADD   CX, AX       { CX = total number of bytes to write }
  REP   OUTSB        { write palette                       }
  {STI}
  POP   DS
     end;

 procedure BlackenPalette;
     var
  Pal: dac_block;
  i: word;
     begin
  for i := First to Last do
      begin
   Pal[i,1] := 0; Pal[i,2] := 0; Pal[i,3] := 0;
      end;
  SetPalette(Pal, First, Last);
     end;

 procedure FadeIn(Pal: dac_block);
     var
  i, j    : Byte;
  TempPal : dac_block;
     begin
  for i := 0 to Speed do
      begin
   for j := First to Last do
       begin
    TempPal[j,1] := Pal[j,1] * i div Speed;
    TempPal[j,2] := Pal[j,2] * i div Speed;
    TempPal[j,3] := Pal[j,3] * i div Speed;
       end;
       vret;
   Setpalette(TempPal, First, Last);
      end;
     end;

 procedure FadeOut(Pal: dac_block);
     var
  i, j    : Byte;
  TempPal : dac_block;
     begin
  TempPal := Pal;
  for i := Speed downto 0 do
      begin
   for j := First to Last do
       begin
    TempPal[j,1] := Pal[j,1] * i div Speed;
    TempPal[j,2] := Pal[j,2] * i div Speed;
    TempPal[j,3] := Pal[j,3] * i div Speed;
       end;
       vret;
   Setpalette(TempPal, First, Last);
      end;
     end;
    end.
