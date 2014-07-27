uses crt, dos;

type levels = array[1..99] of record
                               pos      : array[1..11, 1..11] of Boolean;
                               til      : Byte;
                               time     : LongInt;
                               password : String[8];
                              end;

type olevels = array[1..99] of record
                               pos  : array[1..11, 1..11] of Boolean;
                               til  : Byte;
                               time : LongInt;
                               password : String[8];
                              end;

var NewLevels    : Levels;
    NewLevelFile : File Of Levels;
    OldLevels    : OLevels;
    OldLevelFile : File Of OLevels;
    x            : Byte;
    x2, y2       : Byte;
    passencrypted: String;
    p_enc	 : Byte;
    p_enc_char   : Char;

begin
     assign(OldLevelFile,'levels.crs');
     reset(OldLevelFile);
     read(OldLevelFile,OldLevels);
     close(OldLevelFile);
     for x:=1 to 99 do
     begin
          passencrypted:='';
          for x2:=1 to 11 do
              for y2:=1 to 11 do
                  NewLevels[x].pos[x2,y2]:=OldLevels[x].pos[x2,y2];
          NewLevels[x].Til:=OldLevels[x].til;
          NewLevels[x].time:=OldLevels[x].time;
          for p_enc:=1 to length(oldlevels[x].password) do
          begin
               p_enc_char:=chr(ord(oldlevels[x].password[p_enc])+p_enc);
               passencrypted:=passencrypted+p_enc_char;
          end;
          NewLevels[x].password:=passencrypted;
     end;
{     for x:=1 to 99 do
     begin
         for x2:=1 to 11 do
             for y2:=1 to 11 do
                 NewLevels[x].pos[x2,y2]:=false;
         NewLevels[x].til:=1;
         NewLevels[x].time:=0; 
         NewLevels[x].password:='';
     end; }
     assign(NewLevelFile,'levels2.crs');
     rewrite(NewLevelFile);
     write(NewLevelFile,NewLevels);
     close(NewLevelFile);
end.