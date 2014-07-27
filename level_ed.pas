uses x320x200, crt, dos, fpack;

type levels = array[1..99] of record
                               pos      : array[1..11, 1..11] of Boolean;
                               til      : Byte;
                               time     : LongInt;
                               password : String[8];
                              end;

var x, y, x2, y2   : Integer;
    cx, cy         : Integer;
    ch             : Char;
    a              : Byte;
    actual_tile    : Byte;
    spf            : array[1..11, 1..11] of Boolean;
    levelfile      : file of levels;
    lev            : Byte;
    levels2        : levels;
    slev           : String;
    t_s            : String;
    SpritePointers : array[0..10] of Pointer;
    SpriteZaehler  : Byte;
    ZeigerP        : Pointer;
    SpriteFile     : File;
    showpass       : Boolean;
    pwlev          : Byte;

procedure copy_area(xs, ys, xl, yl, xd, yd : Integer; fromwhere, towhere : Word);
{ Kopiert einen Bereich von einer Seite auf eine andere oder auf die gleiche
  Seite an einer beliebigen Stelle.

  XS, YS     : Quellkoordinaten (<S>ource)
  XL, YL     : Breite und LÑnge des Bereiches
  XD, YD     : Zielkoordinaten (<D>estination)
  FROMWHERE,
  TOWHERE    : Seitenadressen
}
var  y      : Word;
begin
     for y:=0 to yl do
         move(mem[fromwhere:(ys+y)*320+xs],mem[towhere:(yd+y)*320+xd],xl);
end;

 procedure load_sprites;                { Sprites laden }
 begin
      for SpriteZaehler:=0 to 8 do                    { Tiles, 8 St. }
          getmem(SpritePointers[SpriteZaehler],200);  { Speicher reservieren }
     getmem(zeigerp,70);                              { Cursor im Spiel }

     assign(spritefile,'sprites.crs');               { Spritefile zuweisen }
     reset(spritefile,1);                            { Spritefile îffnen }
     for SpriteZaehler:=0 to 8 do                    { wieder einmal zÑhlen }
         blockread(spritefile,spritepointers[Spritezaehler]^,200);
                                                     { Tiles lesen }
     blockread(spritefile,zeigerp^,70);              { Cursor lesen }
     close(spritefile);                              { Spritefile schlie·en }
 end;

 procedure put_sprite(nr : Byte; x, y : Integer; topage : Word);
 { Sprite setzen:
                 NR     = Spritenummer
                 X,Y    = Bildschirmkoordinaten
                 TOPAGE = Bildschirmadresse }
 begin
      put_image(spritepointers[nr],x,y,topage); { Sprites aus SpritePointers }
 end;

 procedure free_sprites;        { Reservierten Speicher wieder freigeben }
 begin
     for SpriteZaehler:=8 downto 0 do
        freemem(SpritePointers[SpriteZaehler],196);
     freemem(zeigerp,65);
 end;

 procedure set_pointer(x, y : Integer; Page : Word);
 { Cursor Setzen }
 begin
     puttransparent(x,y,zeigerp,0,page);
 end;


 procedure write_array(level : Byte);
 var crossx, crossy : Integer;          { Koordinaten des Spielfeldes }
     x1, y1         : Integer;          { Koordinaten fÅr Spriteausgabe }
     w              : Byte;             { Tile-Outfit }
     w2             : Byte;             { Wegen BOOLEAN-Abfrage }

 begin
      w:=levels2[level].til;            { Aktuelles Tile-Outfit }
      for crossx:=1 to 11 do
       for crossy:=1 to 11 do
       begin
        if levels2[level].pos[crossx,crossy]=true then w2:=w-1 else w2:=8;
        x1:=crossx*17-8;
        y1:=crossy*17-8;
        put_sprite(w2,x1,y1,hiddenpage);
       end;
     set_pointer(cx*17-5,cy*17-4,hiddenpage);
 end;

procedure aufbau;
var passenc : String;
    passcnt : Byte;
    passchr : Char;

begin
   str(lev,slev);
   str(levels2[lev].time,t_s);
   bereich_loeschen(250,7,319,17,hiddenpage);
   text_ausgeben(200,7,'LEVEL: '+slev,15,hiddenpage);
   text_ausgeben(200, 20,'S ˙ Levels',15,hiddenpage);
   text_ausgeben(200, 30,'    speichern',15,hiddenpage);
   text_ausgeben(200, 40,'L ˙ Levels',15,hiddenpage);
   text_ausgeben(200, 50,'    laden',15,hiddenpage);
   text_ausgeben(200, 60,'+|- Level',15,hiddenpage);
   text_ausgeben(200, 70,'    wÑhlen',15,hiddenpage);
   text_ausgeben(200, 80,'*|/ Spielsteine',15,hiddenpage);
   text_ausgeben(200, 90,'    wÑhlen',15,hiddenpage);
   text_ausgeben(200,100,',|. Zeit ein-',15,hiddenpage);
   text_ausgeben(200,110,'    stellen',15,hiddenpage);
   text_ausgeben(200,120,'ESC beenden',15,hiddenpage);
   text_ausgeben(200,130,'C   Level',15,hiddenpage);
   text_ausgeben(200,140,'    lîschen',15,hiddenpage);
   bereich_loeschen(240,150,319,160,hiddenpage);
   text_ausgeben(200,150,'ZEIT: '+t_s,15,hiddenpage);
   text_ausgeben(200,160,'TILE: ',15,hiddenpage);
   if showpass=true then
   begin
        passenc:='';
	for passcnt:=1 to length(levels2[lev].password) do
        begin
             passchr:=chr(ord(levels2[lev].password[passcnt])-passcnt);
             passenc:=passenc+passchr;
        end;     
        text_ausgeben(200,175,'Pass<w>ort:',15,hiddenpage);
        bereich_loeschen(200,185,319,195,hiddenpage);
        text_ausgeben(200,185,passenc,15,hiddenpage);
   end
   else bereich_loeschen(200,175,319,199,hiddenpage);

   put_sprite(levels2[lev].til-1,250,160,hiddenpage);
   set_pointer(cx*17-4,cy*17-3,hiddenpage);
   write_array(lev);
   flip;
end;

 procedure put_cross(cx, cy : Integer); { Kreuze setzen }
 begin
     levels2[lev].pos[cx,cy]:=not levels2[lev].pos[cx,cy];

     if cx-1<1 then else
     levels2[lev].pos[cx-1,cy]:=not levels2[lev].pos[cx-1,cy];

     if cx+1>11 then else
     levels2[lev].pos[cx+1,cy]:=not levels2[lev].pos[cx+1,cy];

     if cy-1<1 then else
     levels2[lev].pos[cx,cy-1]:=not levels2[lev].pos[cx,cy-1];

     if cy+1>11 then else
     levels2[lev].pos[cx,cy+1]:=not levels2[lev].pos[cx,cy+1];
 end;

Function File_Exists(name : String) : Boolean;
Var
  DirInfo : SearchRec;
begin
  FindFirst(Name, AnyFile, DirInfo);
  if (DosError = 0) then
    File_Exists := True
  else
    File_Exists := False;
end;

function get_string(x, y : Integer) : String;
var ch   : Char;
    t, z : Byte;
    s2   : String;

begin
     t:=0;
     s2:='';
     repeat
           ch:=readkey;
           if ch=#13 then else
           begin
                if ch=#8 then
                begin
                     if t=0 then else
                     begin
                          delete(s2,length(s2),1);
                          t:=t-8;
                          text_ausgeben(x+t,y,'€',0,hiddenpage);
                          flip;
                     end;
                end
                else
                begin
                     if ch in ['a'..'z','A'..'Z','0'..'9','#','*','+','-'] then
                     begin
                          s2:=s2+ch;
                          text_ausgeben(x+t,y,ch,1,hiddenpage);
                          t:=t+8;
                          flip;
                     end;
                end;
           end;
     until ch=#13;
     if length(s2)>8 then delete(s2,9,length(s2));
     get_string:=s2;
end;

procedure load_levels;
var fname : String;
begin
     loesche_page(hiddenpage);
     text_ausgeben(0, 0,'LEVELS LADEN',15,hiddenpage);
     text_ausgeben(0,10,'------------',15,hiddenpage);
     text_ausgeben(0,30,'BITTE DATEINAMEN EINGEBEN:',15,hiddenpage);
     flip;
     fname:=get_string(0,40);
     if file_exists(fname+'.crs')=false then
     begin
          text_ausgeben(50,100,'DAS FILE EXISTIERT NICHT!',15,hiddenpage);
          text_ausgeben(50,120,'BITTE EINE TASTE DRöCKEN!',15,hiddenpage);
          flip;
          readkey;
     end
     else
     begin
          assign(levelfile,fname+'.crs');
          reset(levelfile);
          read(levelfile,levels2);
          close(levelfile);
     end;
     loesche_page(hiddenpage);
     copy_area(5,5,188,188,7,7,page_adr[2],hiddenpage);
     aufbau;
end;

procedure save_levels;
var fname : String;
    ch    : Char;
begin
     loesche_page(hiddenpage);
     text_ausgeben(0, 0,'LEVELS SPEICHERN',15,hiddenpage);
     text_ausgeben(0,10,'----------------',15,hiddenpage);
     text_ausgeben(0,30,'BITTE DATEINAMEN EINGEBEN:',15,hiddenpage);
     flip;
     fname:=get_string(0,40);
     if fname='' then fname:='NONAME';
     if file_exists(fname+'.crs')=true then
     begin
          text_ausgeben(50,100,'DAS FILE EXISTIERT SCHON!',15,hiddenpage);
          text_ausgeben(50,120,'öBERSCHREIBEN (J/N) ?',15,hiddenpage);
          flip;
          repeat ch:=readkey until ch in ['j','J','n','N'];
          case ch of
               'j','J' : begin
                            assign(levelfile,fname+'.crs');
                            rewrite(levelfile);
                            write(levelfile,levels2);
                            close(levelfile);
                         end;
          end;
     end
     else
     begin
          assign(levelfile,fname+'.crs');
          rewrite(levelfile);
          write(levelfile,levels2);
          close(levelfile);
     end;
     loesche_page(hiddenpage);
     copy_area(5,5,188,188,7,7,page_adr[2],hiddenpage);
     aufbau;
end;

procedure init;
begin
     lev:=1;
     randomize;
     cx:=1;
     cy:=1;

     for a:=1 to 99 do
         for x:=1 to 11 do
             for y:=1 to 11 do
             begin
                 levels2[a].pos[x,y]:=false;
                 levels2[a].til:=1;
                 levels2[a].time:=0;
             end;

     total:=1;
     infodat[1]:='field.pcx';
     actual_tile:=1;
     load_sprites;
     showpass:=false;
     pwlev:=1;
end;

procedure get_password;
var slev : String;
    passenc : String;
    passcnt : Byte;
    passchr : Char;
begin
     loesche_page(hiddenpage);
     text_ausgeben(20, 0,'PASSWORT FESTLEGEN',15,hiddenpage);
     text_ausgeben(20,10,'------------------',15,hiddenpage);
     str(lev,slev);
     text_ausgeben(20,30,'FöR LEVEL      : '+slev,15,hiddenpage);
     text_ausgeben(20,40,'ALTES PASSWORT : '+levels2[lev].password,15,hiddenpage);
     text_ausgeben(20,50,'NEUES PASSWORT : ',15,hiddenpage);
     flip;
     levels2[lev].password:=get_string(156,50);
     passenc:='';
     for passcnt:=1 to length(levels2[lev].password) do
     begin
	passchr:=chr(ord(levels2[lev].password[passcnt])+passcnt);
        passenc:=passenc+passchr;
     end;
     levels2[lev].password:=passenc;

     loesche_page(hiddenpage);
     copy_area(5,5,188,188,7,7,page_adr[2],hiddenpage);
     aufbau;
end;

begin
     init;
     initvga(2);
     loadpcx(1,page_adr[2],true); 

     copy_area(5,5,188,188,7,7,page_adr[2],hiddenpage);

     aufbau;

     repeat
           repeat
                 ch:=readkey;
                 if ord(ch)=0 then ch:=readkey;
           until ch in [#77,#75,#72,#80,#27,#32,#43,#45,#47,#42,#115,#108,#83,#76,#44,#46,#99,#67,#87,#119];

           case ch of
                #77 : begin
                           inc(cx);
                           if cx>11 then cx:=11;
                      end;
                #75 : begin
                           dec(cx);
                           if cx<1 then cx:=1;
                      end;
                #80 : begin
                           inc(cy);
                           if cy>11 then cy:=11;
                      end;
                #72 : begin
                           dec(cy);
                           if cy<1 then cy:=1;
                      end;
                #32 : put_cross(cx,cy);
                #43 : begin
                           inc(lev);
                           showpass:=false;
                           case lev of
                                5,10,15,20,25,30,35,40,
                                45,50,55,60,65,70,75,80,
                                85,90,95 : showpass:=true;
                           end;
                           if lev>99 then lev:=99;
                           write_array(lev);
                           flip;
                      end;
                #45 : begin
                           dec(lev);
                           showpass:=false;
                           case lev of
                                5,10,15,20,25,30,35,40,45,
                                50,55,60,65,70,75,80,85,
                                90,95 : showpass:=true;
                           end;
                           if lev<1 then lev:=1;
                           write_array(lev);
                           flip;
                      end;

                #42 : begin
                           inc(actual_tile);
                           if actual_tile>8 then actual_tile:=8;
                           levels2[lev].til:=actual_tile;
                           aufbau;
                      end;
                #47 : begin
                           dec(actual_tile);
                           if actual_tile<1 then actual_tile:=1;
                           levels2[lev].til:=actual_tile;
                           aufbau;
                      end;
                #44 : begin
                           dec(levels2[lev].time,5);
                           if levels2[lev].time<0 then levels2[lev].time:=95;
                           aufbau;
                      end;
                #46 : begin
                           inc(levels2[lev].time,5);
                           if levels2[lev].time>95 then levels2[lev].time:=0;
                           aufbau;
                      end;
            #99,#67 : begin
                           for x2:=1 to 11 do
                               for y2:=1 to 11 do
                                   levels2[lev].pos[x2,y2]:=false;
                           write_array(lev);
                           flip;
                      end;


          #115, #83 : save_levels;
          #108, #76 : load_levels;
          #119, #87 : get_password;
           end;

           aufbau;

     until ch=#27;

     closevga;
end.