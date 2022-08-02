unit grafix;


interface

const
    header=128;     {Tamany de la cap‡alera dels PCX}
    tampaleta=768;  {Tamany de la paleta de 256 colors}

type
    t_paleta=array[1..768] of byte;  {tipus per guardar paletes}

    t_pcx=record           {Tipo PCX}
        flag:byte;         {si Flag =10 --> PCX}
        version:byte;
        encoded:byte;      {si =1 --> RLE Encoded}
        xmin:word;
        ymin:word;
        xmax:word;
        ymax:word;
    end;

    t_pantalla   = array[1..64000] of byte;   {pantalla del tamany de la VGA}
    ptr_pantalla = ^t_pantalla;

var
   pcx:t_pcx;
   tamany_de_dades:word;           {tamany de dades del fitxer}
   total:longint;                  {tamany del fitxer}
   paleta1:t_paleta;

procedure putpixel(x,y:word;color:byte;zona_mem:word);
{Entrada: x,y     -> coordenades on possar el pixel;
          color   -> color del pixel;
          zona_mem-> Zona de memoria on possar el pixel;
 Funcio : Possar un pixel en una zona de memoria}
procedure espera_vga;
{Funcio : Esperar el reta‡ vertical de la VGA}
procedure get_paleta(var paleta:t_paleta);
{Funcio : Guardar la paleta activa}
procedure set_paleta(paleta:t_paleta);
{Funcio : Possar una paleta en la VGA}
procedure setmode(mode:word);
{Funcio : Canviar el mode actual}
procedure putpixel_trans(x,y:word;color,trans:byte;zona_mem:word);
{Entrada: x,y     -> coordenades on possar el pixel;
          color   -> color del pixel;
          trans   -> color transparent;
          zona_mem-> Zona de memoria on possar el pixel;
 Funcio : Possar un pixel en una zona de memoria}
procedure setup_virtual(var screen:ptr_pantalla;var virtual_addr:word);
{Entrada: screen       -> punter a un vector de 64000 posicions;
 Eixida : virtual_addr -> adre‡a de memoria reservada;
 Funcio : Reservar una zona de memoria de 64000 posicions}
procedure volcar_pantalla(mem_orig,mem_dest:word);
{Entrada: mem_orig -> memoria des d'on copiar;
          mem_dest -> memoria on copiar;
 Funcio : copiar una zona de memoria a un altra}
procedure tancar_virtual(var screen:ptr_pantalla);
{Entrada: screen       -> punter a un vector de 64000 posicions;
 Funcio : Alliberar una zona de memoria previament reservada}
procedure load_pcx(nom_arxiu:string;adresa_mem:word);
{Entrada: nom_arxiu  -> nom del arxiu que volem carregar;
          adresa_mem -> adre‡a de memoria on possar el dibuix;
 Funcio : Carregar un PCX}
procedure get_color(color:byte;var red,green,blue:byte);
{Entrada : color -> numero del color del qual volem aconseguir RGB;
 Eixida  : red   -> intensitat de Roig del color;
           green -> intensitat de Green del color;
           blue  -> intensitat de Blue del color;
 Funcio  : Aconseguir els valors RGB d'un color}
procedure set_color(color,red,green,blue:byte);
{Entrada : color -> numero del color al qual volem posar el RGB;
           red   -> intensitat de Roig del color;
           green -> intensitat de Green del color;
           blue  -> intensitat de Blue del color;
 Funcio  : Possar els valors RGB a un color}
procedure cls(color:byte;zona_mem:word);
{Entrada : color    -> color del qual volem omplir una zona de memoria;
           zona_mem -> Zona de memoria que volem "netejar";
 Funcio  : Omplir una zona de memoria d'un color}
procedure load_pcx2(nom_arxiu:string;adresa_mem:word);
{Entrada: nom_arxiu  -> nom del arxiu que volem carregar;
          adresa_mem -> adre‡a de memoria on possar el dibuix;
 Funcio : Carregar un PCX
 Nota   : Gastar este procediment si load_pcx no funciona b‚}
procedure Fadeup(var paleta_e:t_paleta;espera:byte);
{Entrada: paleta_e  -> paleta a la que volem arribar;
          espera    -> temps de retard;
 Funcio : Fade Up}
procedure FadeDown(red,green,blue,espera:byte);
{Entrada: red     -> valor de roig del color al que volem arribar
          green   -> valor de verd del color al que volem arribar
          blue    -> valor de blau del color al que volem arribar
          espera  -> temps de retard;
 Funcio : Fade Down}
Procedure FadeDown_C(color,red,green,blue,espera:byte);
{Entrada: color   -> color del que volem fer fade
          red     -> valor de roig del color al que volem arribar
          green   -> valor de verd del color al que volem arribar
          blue    -> valor de blau del color al que volem arribar
          espera  -> temps de retard;
 Funcio : Fade Down}

procedure Line(x1, y1, x2, y2 : integer; color : byte;z_mem:word);

procedure blackout;
procedure parallax_scroll(mem_orig,mem_dest,desplasament:word);
procedure put_sprite(mem_orig,mem_dest,m_offset,ample,alt,posx,posy:word);
procedure scroll_v(mem_orig,mem_dest,desplasament:word);
implementation

{------------------------------------------------------------------}
procedure cls(color:byte;zona_mem:word);assembler;
asm
   mov ax,zona_mem;
   mov es,ax;
   xor di,di;
   mov al,color;
   mov ah,al;
   mov cx,32000;
   rep stosw;
end;

{------------------------------------------------------------------}
procedure espera_vga;assembler;
label
     l1,l2;
asm
   mov dx,3dah;
   l1:
    in al,dx;
    test al,8;
    jne l1;
   l2:
    in al,dx;
    test al,8;
    je l2;
   end;

{------------------------------------------------------------------}
procedure get_paleta(var paleta:t_paleta);
var count:word;
begin
     port[$3C7]:=0;
     for count:=1 to 768 do
           paleta[count]:=port[$3C9];
end;

{------------------------------------------------------------------}
procedure set_paleta(paleta:t_paleta);
var count:word;
begin
     port[$3C8]:=0;
     for count:=1 to 768 do
         port[$3C9]:=paleta[count];
end;


{------------------------------------------------------------------}
procedure putpixel(x,y:word;color:byte;zona_mem:word);assembler;
asm
   mov   ax,zona_mem
   mov   es,ax
   mov   di,x
   mov   dx,y
   shl   dx,8
   add   di,dx
   shr   dx,2
   add   di,dx
   mov   al,color
   mov   es:[di],al
end;

{------------------------------------------------------------------}
procedure setmode(mode:word);assembler;
{Canviar a un cert mode}
asm
   mov ax,mode;
   int 10h;
end;
{------------------------------------------------------------------}
procedure putpixel_trans(x,y:word;color,trans:byte;zona_mem:word);assembler;
asm
   {Guardar en ES la zona de memoria}
   mov   ax,zona_mem;
   mov   es,ax;
   {Calcular el offset}
   mov   di,x;      {DI = X}
   mov   dx,y;      {DX = Y}
   shl   dx,8;      {DX = 256*Y}
   add   di,dx;     {DI = 256*Y+BX}
   shr   dx,2;      {DX = 64*Y}
   add   di,dx;     {DI = 320*Y+X}
   mov   al,color;  {Possem el color en AL}
   xor   al,trans;  {si color=trans...}
   jnz   @paint;    {... saltem a @paint}

   inc(di);          {Avancem el punter de pantalla}
   jmp @exit;        {anem a l'eixida}

@Paint:    mov es:[di],al;  {posem al segment de memoria el color}

@exit:
end;
{------------------------------------------------------------------}
procedure volcar_pantalla(mem_orig,mem_dest:word);assembler;
asm
         push ds;
         mov ax,mem_orig;
         mov ds,ax;
         xor di,di;
         mov ax,mem_dest;
         mov es,ax;
         xor si,si;
         mov cx,32000;
         rep movsw;
         pop ds;
end;

{------------------------------------------------------------------}
procedure setup_virtual(var screen:ptr_pantalla;var virtual_addr:word);
begin
     getmem(screen,64000);
     virtual_addr:=seg(screen^);
end;

{------------------------------------------------------------------}
procedure tancar_virtual(var screen:ptr_pantalla);
begin
     freemem(screen,64000);
end;
{------------------------------------------------------------------}
procedure get_color(color:byte;var red,green,blue:byte);
begin
     port[$3C7]:=color;
     red:= port[$3C9];
     green:= port[$3C9];
     blue:= port[$3C9];
end;

{------------------------------------------------------------------}
procedure set_color(color,red,green,blue:byte);
begin
    port[$3C8]:=color;
    port[$3C9]:=red;
    port[$3C9]:=green;
    port[$3C9]:=blue;
end;

{------------------------------------------------------------------}
Procedure Fadeup(var paleta_e:t_paleta;espera:byte);
VAR loop1,i:integer;
    color:word;
    n_color:byte;
    Tmp : Array [1..3] of byte;
      { Per a guardar temporalment un color }
BEGIN
  For loop1:=1 to 64 do BEGIN
      { A color value for Red, green or blue is 0 to 63, so this loop only
        need be executed a maximum of 64 times }
    for i:=0 to espera do Espera_Vga;
    color:=0;
    n_color:=0;
    repeat
      Get_color(n_color,Tmp[1],Tmp[2],Tmp[3]);
      If Tmp[1]<Paleta_e[color+1] then inc (Tmp[1]);
      If Tmp[2]<Paleta_e[color+2] then inc (Tmp[2]);
      If Tmp[3]<Paleta_e[color+3] then inc (Tmp[3]);
        { If the Red, Green or Blue values of color loop2 are less then they
          should be, increase them by one. }
      Set_color(n_color,Tmp[1],Tmp[2],Tmp[3]);
        { Set the new, altered pallette color. }
      inc(color,3);
      inc(n_color);
    until color=768;
  End;
END;

{------------------------------------------------------------------}
Procedure FadeDown(red,green,blue,espera:byte);
  { This procedure fades the screen out to black. }
VAR loop1,loop2,i:integer;
    Tmp : Array [1..3] of byte;
      { This is temporary storage for the values of a color }
BEGIN
  For loop1:=1 to 64 do BEGIN
    for i:=0 to espera do Espera_vga;
    For loop2:=0 to 255 do BEGIN
      Get_color (loop2,Tmp[1],Tmp[2],Tmp[3]);
      If Tmp[1]>red then dec (Tmp[1]);
      If Tmp[2]>green then dec (Tmp[2]);
      If Tmp[3]>blue then dec (Tmp[3]);
        { If the Red, Green or Blue values of color loop2 are not yet zero,
          then, decrease them by one. }
      set_color (loop2,Tmp[1],Tmp[2],Tmp[3]);
        { Set the new, altered pallette color. }
    END;
  END;
END;

{------------------------------------------------------------------}
Procedure FadeDown_C(color,red,green,blue,espera:byte);
  { This procedure fades one color to another }
VAR loop1,loop2,i:integer;
    Tmp : Array [1..3] of byte;
      { This is temporary storage for the values of a color }
BEGIN
  For loop1:=1 to 64 do
  BEGIN
    for i:=0 to espera do Espera_vga;

      Get_color (color,Tmp[1],Tmp[2],Tmp[3]);
      If Tmp[1]>red then dec (Tmp[1]);
      If Tmp[2]>green then dec (Tmp[2]);
      If Tmp[3]>blue then dec (Tmp[3]);
        { If the Red, Green or Blue values of color loop2 are not yet zero,
          then, decrease them by one. }
      set_color (color,Tmp[1],Tmp[2],Tmp[3]);
        { Set the new, altered pallette color. }
  END;
END;


{------------------------------------------------------------------}
procedure Line(x1, y1, x2, y2 : integer; color : byte;z_mem:word);
var i, deltax, deltay, numpixels,
    d, dinc1, dinc2,
    x, xinc1, xinc2,
    y, yinc1, yinc2 : integer;
begin

  { Calculate deltax and deltay for initialisation }
  deltax := abs(x2 - x1);
  deltay := abs(y2 - y1);

  { Initialize all vars based on which is the independent variable }
  if deltax >= deltay then
    begin

      { x is independent variable }
      numpixels := deltax + 1;
      d := (2 * deltay) - deltax;
      dinc1 := deltay Shl 1;
      dinc2 := (deltay - deltax) shl 1;
      xinc1 := 1;
      xinc2 := 1;
      yinc1 := 0;
      yinc2 := 1;
    end
  else
    begin

      { y is independent variable }
      numpixels := deltay + 1;
      d := (2 * deltax) - deltay;
      dinc1 := deltax Shl 1;
      dinc2 := (deltax - deltay) shl 1;
      xinc1 := 0;
      xinc2 := 1;
      yinc1 := 1;
      yinc2 := 1;
    end;

  { Make sure x and y move in the right directions }
  if x1 > x2 then
    begin
      xinc1 := - xinc1;
      xinc2 := - xinc2;
    end;
  if y1 > y2 then
    begin
      yinc1 := - yinc1;
      yinc2 := - yinc2;
    end;

  { Start drawing at <x1, y1> }
  x := x1;
  y := y1;

  { Draw the pixels }
  for i := 1 to numpixels do
    begin
      PutPixel(x, y, color,z_mem);
      if d < 0 then
        begin
          d := d + dinc1;
          x := x + xinc1;
          y := y + yinc1;
        end
      else
        begin
          d := d + dinc2;
          x := x + xinc2;
          y := y + yinc2;
        end;
    end;
end;
{------------------------------------------------------------------}
procedure blackout;
var loop:byte;
begin
     for loop:=0 to 255 do set_color(loop,0,0,0);
end;

{------------------------------------------------------------------}
procedure parallax_scroll(mem_orig,mem_dest,desplasament:word);assembler;
 asm
       push ds;
       {inicialitzar l'acces a les memories orige i desti}
       mov ax,mem_orig;
       mov ds,ax;
       xor si,si;

       mov ax,mem_dest;
       mov es,ax;
       mov di,desplasament;

       {comen‡ar a copiar primer tros}
       mov cx,64000;            {bytes en una pantalla}
       sub cx,desplasament;     {bytes a copiar}
       shr cx,1;                {words a copiar}
       jz @part2;

@pintar1:
       mov ax,ds:[si];
       or al,00h;
       jz @next1;
       mov es:[di],al;
@next1:
       inc di;
       or ah,00h;
       jz @next2;
       mov es:[di],ah;
@next2:
       inc si;
       inc di;
       inc si;
       loop @pintar1;

@part2:
       {comen‡ar a copiar segon tros}
       mov cx,desplasament;
       shr cx,1;
       jz @fi;
       xor di,di;

@pintar2:
       mov ax,ds:[si];
       or al,00h;
       jz @next3;
       mov es:[di],al;
@next3:
       inc di;
       or ah,00h;
       jz @next4;
       mov es:[di],ah;
@next4:
       inc si;
       inc di;
       inc si;
       loop @pintar2;

@fi:   pop ds;
 end;
{------------------------------------------------------------------}
procedure put_sprite(mem_orig,mem_dest,m_offset,ample,alt,posx,posy:word);
   begin
    asm
       push ds;

       mov si,m_offset;

       mov ax,mem_orig;
       mov ds,ax;        {memoria orige}
       mov ax,mem_dest;
       mov es,ax;        {memoria desti}

       mov   di,posx;      {DI = X}
       mov   dx,posy;      {DX = Y}
       shl   dx,8;      {DX = 256*Y}
       add   di,dx;     {DI = 256*Y+BX}
       shr   dx,2;      {DX = 64*Y}
       add   di,dx;     {DI = 320*Y+X}

       mov cx,alt;     {guarde el alt}

   @1: push cx;        {guarde el alt}
       push di;        {guarde el offset desti}
       push si;        {guarde el offset orige}

       mov cx,ample;   {carregue el ample}

   @nou_pixel:
       mov al,ds:[si]       {color del pixel orige en al}
       or al,00h;           {AL=0?}
       jnz @paint;
       jmp @new;            {altre pixel}

   @paint:
       mov es:[di],al;      {pintar pixel}

   @new:
       inc di;              {augmentar punter pantalla}
       inc si;              {augmentar punter font}
       loop @nou_pixel;     {mentres no siga l'ample continuar}
       pop si;              {recuperem l'offset orige}
       pop di;              {recuperem l'offset desti}
       add si,320;          {segent linia orige}
       add di,320;          {segent linia desti}
       pop cx;              {recuperem l'alt}
       dec cx;              {una linia menys}
       cmp cx,0;            {Queden linies?}
       jnz @1;              {Si.Anar a @1}
       pop ds;
    end;
end;
{------------------------------------------------------------------}
procedure scroll_v(mem_orig,mem_dest,desplasament:word);assembler;
 asm
       push ds;
       {inicialitzar l'acces a les memories orige i desti}
       mov ax,mem_orig;
       mov ds,ax;
       xor si,si;

       mov ax,mem_dest;
       mov es,ax;
       mov di,desplasament;

       mov cx,64000;            {bytes en una pantalla}
       sub cx,desplasament;     {bytes a copiar}
       shr cx,1;                {words a copiar}
       {comen‡ar a copiar primer tros}
       rep movsw;

       mov cx,desplasament;
       shr cx,1;
       xor di,di;
       {comen‡ar a copiar segon tros}
       rep movsw;

       pop ds;
 end;

{------------------------------------------------------------------}
{------------------------------------------------------------------}
procedure llegir_capsalera(var file_in:file);
{llegir les dades de la cap‡alera}
{llegim directament els 128 bytes de la capsalera per a optimitzar}
var capsalera:array [1..128] of byte;
begin
     blockread(file_in,capsalera,128);
     pcx.flag:=capsalera[1];
     pcx.version:=capsalera[2];
     pcx.encoded:=capsalera[3];
     pcx.xmin:=capsalera[5]+(capsalera[6] shl 8);
     pcx.ymin:=capsalera[7]+(capsalera[8] shl 8);
     pcx.xmax:=capsalera[9]+(capsalera[10] shl 8);
     pcx.ymax:=capsalera[11]+(capsalera[12] shl 8);
end;

{------------------------------------------------------------------}
{Llegir paleta del fitxer}
procedure llegir_paleta(var file_in:file);
var pal:array [1..768] of byte;
    dada:byte;i:word;
begin
      blockread(file_in,dada,1); {Esta dada es un 12 que indica la presencia}
                                 {que indica la presencia de la paleta, pero}
                                 {no pertany a la paleta}
      port[$3c8]:=0;             {Comencem al color 0}
      blockread(file_in,pal,sizeof(pal));   {llegim totes les dades de la }
                                            {paleta}
      i:=0;
      while i<(256*3) do
      {pasem les dades al port de la VGA}
         begin
            port[$3C9]:=pal[i+1] shr 2;
            port[$3C9]:=pal[i+2] shr 2;
            port[$3C9]:=pal[i+3] shr 2;
            i:=i+3;
         end;
end;

{------------------------------------------------------------------}
procedure _pintar_dibuix(var file_in:file;zona_mem:word);
{Dibuixar les dades a la pantalla virtual}
var dada:byte;
    conta_bytes,compte,y,x,offset,i,pos:word;
begin
     conta_bytes:=0;    {Contador de Bytes}
     pos:=0;            {pixels escrits en pantalla}
     repeat
           blockread(file_in,dada,1);
           conta_bytes:=conta_bytes+1;
           if ((dada and $C0)=$C0) then        {Descompresio del PCX}
              begin
                   compte:=(dada and $3F);
                   blockread(file_in,dada,1);
                   inc(conta_bytes);
              end
           else compte:=1;
           for i:=1 to compte do            {pintar en pantalla en pantalla}
                                            {virtual}
            begin
             x:=pos mod ((pcx.xmax-pcx.xmin)+1);  {calcular la posicio}
             y:=pos div ((pcx.xmax-pcx.xmin)+1);  {del pixel}
             offset:=(y shl 8)+(y shl 6)+x;
                mem[zona_mem:offset]:=dada;    {pintar en memoria}
                inc(pos);
            end;
            compte:=1;
     until tamany_de_dades=conta_bytes;
end;

{------------------------------------------------------------------}
procedure pintar_dibuix2(var file_in:file;zona_mem:word);
{Dibuixar les dades a la pantalla virtual}
var dada:byte;
    conta_bytes,compte,y,x,offset,i,pos:word;
begin
     conta_bytes:=0;    {Contador de Bytes}
     pos:=0;            {pixels escrits en pantalla}
     repeat
           blockread(file_in,dada,1);
           conta_bytes:=conta_bytes+1;
           if ((dada and $C0)=$C0) then        {Descompresio del PCX}
              begin
                   compte:=(dada and $3F);
                   blockread(file_in,dada,1);
                   inc(conta_bytes);
              end
           else compte:=1;
           for i:=1 to compte do            {pintar en pantalla en pantalla}
                                            {virtual}
            begin
             x:=pos mod ((pcx.xmax-pcx.xmin)+2);  {calcular la posicio}
             y:=pos div ((pcx.xmax-pcx.xmin)+2);  {del pixel}
             offset:=(y shl 8)+(y shl 6)+x;
                mem[zona_mem:offset]:=dada;    {pintar en memoria}
                inc(pos);
            end;
            compte:=1;
     until tamany_de_dades=conta_bytes;
end;

{------------------------------------------------------------------}
procedure pintar_dibuix(var file_in:file;zona_mem:word);
{Dibuixar les dades a la pantalla virtual}
const max_dades=8192;
var
    compte,y,x,offset,i,k,pos:word;
    ptr_dada:array [1..max_dades] of byte;
    dades_disponibles:longint;
    flag_compte:byte;

procedure pintar_pixel;
begin
    x:=pos mod ((pcx.xmax-pcx.xmin)+1);  {calcular la posicio}
    y:=pos div ((pcx.xmax-pcx.xmin)+1);  {del pixel}
    offset:=(y shl 8)+(y shl 6)+x;
    mem[zona_mem:offset]:=ptr_dada[i];    {pintar en memoria}
    flag_compte:=0;
    inc(pos)
end;

begin
    pos:=0;
    flag_compte:=0;
    dades_disponibles:=tamany_de_dades;
    repeat
      {si dades disponibles>=15000 bytes ...}
      if dades_disponibles>=max_dades then
      begin
          dades_disponibles:=dades_disponibles-max_dades;
          {llegir un bloc de dades}
          blockread(file_in,ptr_dada,max_dades);
          i:=1;{posicio de dada}
          {repetir fins completar el bloc}
          repeat
           {la dada es comptador?}
           if ((ptr_dada[i] and $C0)=$C0) then
            begin
             compte:=ptr_dada[i] and $3F;
             {segent dada}
             inc(i);
             if i>max_dades then
             begin
              i:=max_dades;
              blockread(file_in,ptr_dada[i],1);
              dec (dades_disponibles);
             end;
             {repetir el pixel compte vegades}
             if i<=max_dades then
              for k:=1 to compte do
                pintar_pixel;
            end
           else pintar_pixel;
            {segent dada}
            inc(i);
          until max_dades+1=i;
          {inc(pos);}
      end
      {...sino (dades disponibles<max_dades,ultim bloc de dades)}
      else begin
            i:=1;
            {llegir un bloc de dades disponibles}
            blockread(file_in,ptr_dada,dades_disponibles);
            {repetir dades_disponibles}
            repeat
            begin
             {comptador?}
             if (ptr_dada[i] and $C0)=$C0 then
             begin
              compte:=ptr_dada[i] and $3F;
              {segent dada}
              inc(i);
              dec(dades_disponibles);
              for k:=1 to compte do
              begin
               x:=pos mod ((pcx.xmax-pcx.xmin)+1);  {calcular la posicio}
               y:=pos div ((pcx.xmax-pcx.xmin)+1);  {del pixel}
               offset:=(y shl 8)+(y shl 6)+x;
               mem[zona_mem:offset]:=ptr_dada[i];    {pintar en memoria}
               inc(pos);
               {segent dada}

              end;
              inc(i);
              dec(dades_disponibles);
              compte:=1;
             end
             else begin
               x:=pos mod ((pcx.xmax-pcx.xmin)+1);  {calcular la posicio}
               y:=pos div ((pcx.xmax-pcx.xmin)+1);  {del pixel}
               offset:=(y shl 8)+(y shl 6)+x;
               mem[zona_mem:offset]:=ptr_dada[i];    {pintar en memoria}
               inc(pos);
               {segent dada}
               inc(i);
               dec(dades_disponibles);
             end;
            end;
            until dades_disponibles<=0;
     end;
    until dades_disponibles<=0;
end;


{------------------------------------------------------------------}
procedure _load_pcx(nom_arxiu:string;adresa_mem:word);
var file_in:file;
begin
     assign(file_in,nom_arxiu);
     reset(file_in,1);
     total:=filesize(file_in);
     tamany_de_dades:=total-tampaleta-1-header;
     llegir_capsalera(file_in);
     setmode($13);
     espera_vga;
     pintar_dibuix(file_in,adresa_mem);
     llegir_paleta(file_in);
     close(file_in);
end;

{------------------------------------------------------------------}
procedure load_pcx2(nom_arxiu:string;adresa_mem:word);
var file_in:file;
begin
     assign(file_in,nom_arxiu);
     reset(file_in,1);
     total:=filesize(file_in);
     tamany_de_dades:=total-tampaleta-1-header;
     llegir_capsalera(file_in);
     {setmode($13);
     espera_vga;}
     pintar_dibuix2(file_in,adresa_mem);
     llegir_paleta(file_in);
     close(file_in);
end;
{------------------------------------------------------------------}
procedure load_pcx(nom_arxiu:string;adresa_mem:word);
var file_in:file;
begin
     assign(file_in,nom_arxiu);
     reset(file_in,1);
     total:=filesize(file_in);
     tamany_de_dades:=total-tampaleta-1-header;
     llegir_capsalera(file_in);
     {setmode($13);
     espera_vga;}
     pintar_dibuix(file_in,adresa_mem);
     llegir_paleta(file_in);
     close(file_in);
end;

end.