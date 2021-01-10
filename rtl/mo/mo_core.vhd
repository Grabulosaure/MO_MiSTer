--------------------------------------------------------------------------------
-- Thomson MO5 / MO6
--------------------------------------------------------------------------------
-- DO 12/2019
--------------------------------------------------------------------------------

-- - Touches spéciales clavier. Revoir mapping clavier
-- - Crayon optique. Compteur & curseur
-- - Joysticks
-- - Souris
-- - Cassette
-- - LED clavier

--------------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.std_logic_1164.all;
USE IEEE.numeric_std.all;

LIBRARY work;
USE work.base_pack.ALL;
USE work.rom_pack.ALL;

USE std.textio.ALL;

ENTITY mo_core IS
  PORT (
    -- Master input clock
    sysclk            : IN    std_logic;
    
    -- Async reset from top-level module. Can be used as initial reset.
    reset             : IN    std_logic;
    
    mo5               : IN std_logic;
    azerty            : IN std_logic;
    dcmoto            : IN std_logic;
    rewind            : IN std_logic;
    fast              : IN std_logic;
    ovo_ena           : IN std_logic;
    capslock          : OUT std_logic;
    
    -- Base video clock. Usually equals to CLK_SYS.
    clk_video         : OUT   std_logic;
    
    -- Multiple resolutions are supported using different CE_PIXEL rates.
    -- Must be based on CLK_VIDEO
    ce_pixel          : OUT   std_logic;
    
    -- VGA
    vga_r             : OUT   std_logic_vector(7 DOWNTO 0);
    vga_g             : OUT   std_logic_vector(7 DOWNTO 0);
    vga_b             : OUT   std_logic_vector(7 DOWNTO 0);
    vga_hs            : OUT   std_logic; -- positive pulse!
    vga_vs            : OUT   std_logic; -- positive pulse!
    vga_de            : OUT   std_logic; -- = not (VBlank or HBlank)
    
    -- AUDIO
    audio_l           : OUT std_logic_vector(15 DOWNTO 0);
    audio_r           : OUT std_logic_vector(15 DOWNTO 0);
    
    ps2_key           : IN  std_logic_vector(10 DOWNTO 0);
    ps2_mouse         : IN  std_logic_vector(24 DOWNTO 0);
    joystick_0        : IN  std_logic_vector(31 DOWNTO 0);
    joystick_1        : IN  std_logic_vector(31 DOWNTO 0);
    joystick_analog_0 : IN  std_logic_vector(15 DOWNTO 0);
    joystick_analog_1 : IN  std_logic_vector(15 DOWNTO 0);
    
    ddram_clk         : OUT std_logic;
    ddram_busy        : IN  std_logic;
    ddram_burstcnt    : OUT std_logic_vector(7 DOWNTO 0);
    ddram_addr        : OUT std_logic_vector(28 DOWNTO 0);
    ddram_dout        : IN  std_logic_vector(63 DOWNTO 0);
    ddram_dout_ready  : IN  std_logic;
    ddram_rd          : OUT std_logic;
    ddram_din         : OUT std_logic_vector(63 DOWNTO 0);
    ddram_be          : OUT std_logic_vector(7 DOWNTO 0);
    ddram_we          : OUT std_logic;
    
    ioctl_download    : IN  std_logic;
    ioctl_index       : IN  std_logic_vector(7 DOWNTO 0);
    ioctl_wr          : IN  std_logic;
    ioctl_addr        : IN  std_logic_vector(24 DOWNTO 0);
    ioctl_dout        : IN  std_logic_vector(7 DOWNTO 0);
    ioctl_wait        : OUT std_logic
    );
END mo_core;

ARCHITECTURE struct OF mo_core IS
  
  CONSTANT CPU_ALTERNATE : boolean :=false;
  SIGNAL ioctl_download2 : std_logic;
  SIGNAL adrs : uv17;
  SIGNAL ps2_key_delay : std_logic_vector(10 DOWNTO 0);
  SIGNAL ps2_mouse_delay : std_logic_vector(24 DOWNTO 0);
  
  ----------------------------------------
  SIGNAL mo6_keys : unsigned(71 DOWNTO 0) :=(OTHERS =>'0');
  SIGNAL shift,altgr : std_logic;
  SIGNAL mxpos : integer RANGE -1024*8 TO 1024*8-1 :=    500    ;
  SIGNAL mypos : integer RANGE -1024*8 TO 1024*8-1 :=    300    ;
  SIGNAL xpos : integer RANGE -1024 TO 1024-1;
  SIGNAL ypos : integer RANGE -1024 TO 1024-1;
  SIGNAL xposu,yposu : signed(11 DOWNTO 0);
  
  ----------------------------------------
  SIGNAL reset_na : std_logic;
    
  SIGNAL divclk : uint5;
  SIGNAL tick_cpu : std_logic;
  SIGNAL cpu_reset,cpu_wake : std_logic;
  SIGNAL reset_cpt : uint3;
  
  ----------------------------------------
  SIGNAL cpu_a,cpu_a2 : uv16;
  SIGNAL cpu_dw,cpu_dr : uv8;
  SIGNAL cpu_req,cpu_ack,cpu_wr : std_logic;
  SIGNAL cpu_irq,cpu_firq,cpu_nmi : std_logic;
  SIGNAL cpu_fast : std_logic;

  SIGNAL lpen_irq,lpen_irq2,lpen_clr : std_logic;
  SIGNAL ram_dr,rom_dr,pia1_dr,pia2_dr,reg_dr,reg_drp : uv8;
  SIGNAL romdisk_dr : uv8;
  SIGNAL cpu_aram : uv17;
  SIGNAL cpu_arom : uv16;

  SIGNAL adpia : uv2;
  SIGNAL pia1_pa_i,pia1_pa_o,pia1_pb_i,pia1_pb_o,pia1_pa_d,pia1_pb_d : uv8;
  SIGNAL pia2_pa_i,pia2_pa_o,pia2_pb_i,pia2_pb_o,pia2_pa_d,pia2_pb_d : uv8;
  SIGNAL pia1_irqa,pia1_irqb,pia2_irqa,pia2_irqb : std_logic;
  SIGNAL pia1_req,pia1_ack,pia2_req,pia2_ack : std_logic;
  SIGNAL pal_dr,pal_dw : uv8;
  SIGNAL pal_wr,pal_rd : std_logic;
  SIGNAL pal_a : uv5;
  
  SIGNAL vram_dr : uv8;
  SIGNAL vram_a  : uv14;
  
  ----------------------------------------
  SIGNAL rambank : uv5;
  SIGNAL acc_a7dc,basic,cart,selreg,vtrame : std_logic;
  SIGNAL vmode : uv3;
  SIGNAL vfreq,vconf,vpage : uv2;
  SIGNAL ta,l_ta : uv13 :=(OTHERS =>'0');
  SIGNAL ta_h124,l_ta_h124 : uv3;
  SIGNAL pulse50hz : std_logic;
  SIGNAL lpen_button,ena_sys1,lt3 : std_logic;
  SIGNAL vin,hin,hlr,l_vin,l_hin,l_hlr,vin_n : std_logic;
  SIGNAL vborder : uv4;

  SIGNAL bankswitch : std_logic;
  
  SIGNAL tape_motor_n,rk7,wk7 : std_logic;
  SIGNAL tratio : uv8;
  SIGNAL tpos : uv32;
  
  SIGNAL vid_r,vid_g,vid_b : uv8;
  SIGNAL vga_r_u,vga_g_u,vga_b_u : uv8;
  SIGNAL vid_hs,vid_vs,vid_de,vid_vde,vid_ce : std_logic;
  
  CONSTANT Z15 : unsigned(14 DOWNTO 0):=(OTHERS =>'0');
  
  ----------------------------------------
  SIGNAL cpu2_E,cpu2_Q,cpu2_irq_n,cpu2_firq_n,cpu2_nmi_n : std_logic;
  SIGNAL cpu2_dw,cpu2_dr : uv8;
  SIGNAL cpu2_a : uv16;
  SIGNAL cpu2_lic,cpu2_halt_n,cpu2_rnw : std_logic;
  SIGNAL cpu2_avma,cpu2_busy : std_logic;
  SIGNAL cpu2_ndmabreq : std_logic;
  SIGNAL cpu2_tick2,cpu2_reset_n : std_logic;
  
  COMPONENT mc6809i
    PORT (
      D : IN uv8;
      DOut : OUT uv8;
      ADDR : OUT uv16;
      RnW : OUT std_logic;
      E : IN std_logic;
      Q : IN std_logic;
      BS : OUT std_logic;
      BA : OUT std_logic;
      nIRQ : IN std_logic;
      nFIRQ : IN std_logic;
      nNMI : IN std_logic;
      AVMA : OUT std_logic;
      BUSY : OUT std_logic;
      LIC : OUT std_logic;
      nHALT : IN std_logic;
      nRESET : IN std_logic;
      nDMABREQ : IN std_logic;
      RegData : OUT unsigned(111 DOWNTO 0)
      );
  END COMPONENT mc6809i;
  
  ----------------------------------------
  SHARED VARIABLE ROM : arr8(0 TO 65536-1) :=
    ROM_BASIC6_0 & ROM_MO6_0 & ROM_BASIC6_1 & ROM_MO6_1 &
    ROM_BASIC6_2 & ROM_BASIC6_3; -- 64k ROM
  CONSTANT PAD : arr8(0 TO 63):=(OTHERS =>x"FF");
  SHARED VARIABLE ROM_DISK : arr8(0 TO 2047); -- :=ROM_DISK & PAD; -- 2k ROM
  
  SHARED VARIABLE RAM : arr8(0 TO 65536*2-1); -- 128k RAM
  ATTRIBUTE ramstyle : string;
  ATTRIBUTE ramstyle OF ROM : VARIABLE IS "no_rw_check";
  ATTRIBUTE ramstyle OF RAM : VARIABLE IS "no_rw_check";
  
  SIGNAL ovo_in0  : unsigned(0 TO 32*5-1) :=(OTHERS =>'0');
  SIGNAL ovo_in1  : unsigned(0 TO 32*5-1) :=(OTHERS =>'0');
  
  SIGNAL xxx_kr : uv8;
  SIGNAL vid_hpos,vid_vpos : uint11;
  SIGNAL xx_lock : std_logic :='0';
  FILE fil : text OPEN write_mode IS "mem.log";
BEGIN
  
  ----------------------------------------------------------
  
  reset_na<='0' WHEN reset='1' ELSE
            '1' WHEN rising_edge(sysclk);

  PROCESS(sysclk,reset_na) IS
    CONSTANT C_TICK : uv32 :="00000000000000000000000000000100";
    CONSTANT C_E    : uv32 :="00000000000000001111111111111111";
    CONSTANT C_Q    : uv32 :="00000000111111111111111100000000";
    
  BEGIN
    IF reset_na='0' THEN
      reset_cpt<=0;
      cpu_reset<='0';
    ELSIF rising_edge(sysclk) THEN
      
      tick_cpu<=C_TICK(divclk);
      cpu2_E  <=C_E(divclk);
      cpu2_Q  <=C_Q(divclk);

      cpu2_tick2<=tick_cpu;
      IF cpu2_tick2='1' THEN
        cpu2_dr<=cpu_dr;
      END IF;
      
      divclk<=(divclk+1) MOD 32;
      tick_cpu<=to_std_logic(divclk=0);
      IF fast='1' THEN
        tick_cpu<=to_std_logic(divclk MOD 2 = 0);
      END IF;
      
      IF tick_cpu='1' THEN
        IF reset_cpt<5 THEN
          reset_cpt<=reset_cpt+1;
          cpu_reset<='1';
        ELSE
          cpu_reset<='0';
        END IF;
      END IF;
      
    END IF;
  END PROCESS;
  
  ----------------------------------------------------------
  -- Compare
  No:IF CPU_ALTERNATE GENERATE
  i_ccpu: mc6809i
    PORT MAP (
      D      => cpu2_dr,
      DOut   => cpu2_dw,
      ADDR   => cpu2_a,
      RnW    => cpu2_rnw,
      E      => cpu2_E,
      Q      => cpu2_Q,
      BS     => OPEN,
      BA     => OPEN,
      nIRQ   => cpu2_irq_n,
      nFIRQ  => cpu2_firq_n,
      nNMI   => cpu2_nmi_n,
      AVMA   => cpu2_avma, -- OUT
      BUSY   => cpu2_busy, -- OUT
      LIC    => cpu2_lic, -- OUT
      nHALT  => cpu2_halt_n,
      nRESET => cpu2_reset_n,
      nDMABREQ => cpu2_ndmabreq
      );

  cpu2_halt_n<='1';
  cpu2_ndmabreq<='1';
  cpu2_irq_n <=NOT cpu_irq;
  cpu2_firq_n<=NOT cpu_firq;
  cpu2_nmi_n <=NOT cpu_nmi;
  cpu2_reset_n<=NOT cpu_reset AND reset_na;
  cpu_a<=cpu2_a;
  cpu_wr<=NOT cpu2_rnw;
  cpu_dw<=cpu2_dw;
  cpu_req<='1';

  END GENERATE;

--pragma synthesis_off
  PROCESS IS
    VARIABLE lout : line;
  BEGIN
    wure(sysclk);
    
    IF cpu_ack='1' THEN
      IF cpu_wr='1' THEN
        write(lout,string'("WR: "));
        write(lout,to_hstring(cpu_a) & " ");
        write(lout,to_hstring(cpu_dw));
        writeline(fil,lout);
      ELSE
        write(lout,string'("RD: "));
        write(lout,to_hstring(cpu_a) & " ");
        wure(sysclk);
        write(lout,to_hstring(cpu_dr));
        writeline(fil,lout);
      END IF;

    END IF;
    
  END PROCESS;
--pragma synthesis_on
  
  ----------------------------------------------------------
  -- CPU

  Oui:IF NOT CPU_ALTERNATE GENERATE
    
    i_cpu: ENTITY work.mc6809
      PORT MAP (
        ad       => cpu_a,
        dw       => cpu_dw,
        dr       => cpu_dr,
        req      => cpu_req,
        ack      => cpu_ack,
        wr       => cpu_wr,
        ph       => OPEN,
        irq      => cpu_irq,
        firq     => cpu_firq,
        nmi      => cpu_nmi,
        reset    => cpu_reset,
        wake     => cpu_wake,
        fast     => cpu_fast,
        xpc      => OPEN,
        clk      => sysclk,
        reset_na => reset_na);
  END GENERATE;
  
  cpu_wake<='1';
  
  cpu_fast<='0';
  cpu_irq <=pia1_irqb OR pia2_irqa OR pia2_irqb;
  cpu_firq<=pia1_irqa OR lpen_irq;
  cpu_nmi <='0';

  cpu_ack<=cpu_req AND tick_cpu;
  
  ----------------------------------------------------------
  -- 0000 : 1FFF : VIDEO RAM
  -- 2000 : 9FFF : USER RAM
  -- A000 : A7BF : DISK ROM
  -- A7C0 : A7C3 : PIA 6821 SYSTEM
  -- A7C4 : A7CA : <unused>
  -- A7CB :      : <reserved>
  -- A7CC : A7CF : PIA 6821 EXTENSION
  -- A7D0 : A7D8 : DISK CTRL
  -- A7D9 :      : <unused>
  -- A7DA : A7DB : VIDEO PALETTE
  -- A7DC : A7DD : VIDEO REGISTERS
  -- A7DE : A7E3 : <unused>
  -- A7E4 : A7E7 : VIDEO REGISTERS
  -- A7E8 : A7EB : RF 90932
  -- A7EC : A7EF : <unused>
  -- A7F0 : A7F7 : IEEE Extension
  -- A7F8 : AFFF : <unused>
  -- B000 : EFFF : BASIC ROM
  -- F000 : FFFF : MONITOR ROM
  
  cpu_dr<=ram_dr     WHEN cpu_a2<x"A000" ELSE
          pia1_dr    WHEN cpu_a2>=x"A7C0" AND cpu_a2<=x"A7C3" ELSE
          pia2_dr    WHEN cpu_a2>=x"A7CC" AND cpu_a2<=x"A7CF" ELSE
          reg_dr     WHEN cpu_a2>=x"A7DA" AND cpu_a2<=x"A7E7" ELSE
          romdisk_dr WHEN cpu_a2>=x"A000" AND cpu_a2<=x"A7BF" ELSE
          rom_dr     WHEN cpu_a2>=x"B000" ELSE
          x"00";
  
  cpu_a2<=cpu_a WHEN rising_edge(sysclk);
  
  ----------------------------------------------------------
  -- VIDEO
  i_video: ENTITY work.movideo
    PORT MAP (
      vram_a    => vram_a,
      vram_dr   => vram_dr,
      pal_a     => pal_a,
      pal_dw    => pal_dw,
      pal_dr    => pal_dr,
      pal_wr    => pal_wr,
      mo5       => mo5,
      vmode     => vmode,
      vborder   => vborder,
      vtrame    => vtrame,
      pulse50hz => pulse50hz,
      vid_r     => vid_r,
      vid_g     => vid_g,
      vid_b     => vid_b,
      vid_hs    => vid_hs,
      vid_vs    => vid_vs,
      vid_de    => vid_de,
      vid_vde   => vid_vde,
      vid_hpos  => vid_hpos,
      vid_vpos  => vid_vpos,
      vid_ce    => vid_ce,
      clk       => sysclk,
      reset_na  => reset_na
      );
  
  ----------------------------------------------------------
  i_ovo: ENTITY work.ovo
    GENERIC MAP (
      RGB => x"7F7F7F")
    PORT MAP (
      i_r     => vid_r,
      i_g     => vid_g,
      i_b     => vid_b,
      i_hs    => vid_hs,
      i_vs    => vid_vs,
      i_de    => vid_de,
      i_en    => vid_ce,
      i_clk   => sysclk,
      o_r     => vga_r_u,
      o_g     => vga_g_u,
      o_b     => vga_b_u,
      o_hs    => vga_hs,
      o_vs    => vga_vs,
      o_de    => vga_de,
      ena     => ovo_ena,
      in0     => ovo_in0,
      in1     => ovo_in1
      );

  ovo_in0 <=
    "0000" & ps2_key(8) &
    '0' & unsigned(ps2_key(7 DOWNTO 4)) &
    '0' & unsigned(ps2_key(3 DOWNTO 0)) &
    "10000" &
    '0' & pia1_pb_o(7 DOWNTO 4) &
    '0' & pia1_pb_o(3 DOWNTO 0) &
    "10000" &
    "0000" & pia1_pa_o(3) &
    "10000" &
    '0' & cpu_a(15 DOWNTO 12) &
    '0' & cpu_a(11 DOWNTO 8) &
    '0' & cpu_a(7 DOWNTO 4) &
    '0' & cpu_a(3 DOWNTO 0) &
    "10000" &
    "000" & vpage &
    "000" & vconf &
    "000" & vfreq &
    "00" & vmode &
    "10000" &
    "000" & cpu_irq & cpu_firq &
    "10000" &
    
    "10000" &
    "0000" & acc_a7dc &

    "10000" &
    '0' & unsigned(ioctl_addr(11 DOWNTO 8)) &
    '0' & unsigned(ioctl_addr(7 DOWNTO 4)) &
    '0' & unsigned(ioctl_addr(3 DOWNTO 0)) &
    '0' & tratio(7 DOWNTO 4) &
    '0' & tratio(3 DOWNTO 0) &
    "10000" &
    --"0000" & rk7 &
    --"00" & tape_motor_n & '0' & ioctl_wr
    "0000" & pia1_pa_o(4) &
    "0000" & pia1_pa_o(3);
  
  ovo_in1 <=
    '0' & tpos(31 DOWNTO 28) &
    "10000" &
    '0' & "000" & ta(12) &
    '0' & ta(11 DOWNTO 8) &
    '0' & ta(7 DOWNTO 4) &
    '0' & ta(3 DOWNTO 0) &
    "10000" &
    '0' & "000" & l_ta(12) &
    '0' & l_ta(11 DOWNTO 8) &
    '0' & l_ta(7 DOWNTO 4) &
    '0' & l_ta(3 DOWNTO 0) &
    "10000" &
    "10000" &
    '0' & unsigned(xposu(11 DOWNTO 8)) &
    '0' & unsigned(xposu(7 DOWNTO 4)) &
    '0' & unsigned(xposu(3 DOWNTO 0)) &
    "10000" &
    '0' & unsigned(yposu(11 DOWNTO 8)) &
    '0' & unsigned(yposu(7 DOWNTO 4)) &
    '0' & unsigned(yposu(3 DOWNTO 0)) &
  
    "10000" &
    "0000" & lpen_irq &
    "0000" & lpen_button &
    "10000" & 
    "10000" & "10000" & "10000" & "10000" &
    "10000" & "10000" & "10000" & "10000";


  xposu<=to_signed(xpos,12);
  yposu<=to_signed(ypos,12);
  
  vga_r<=std_logic_vector(vga_r_u);
  vga_g<=std_logic_vector(vga_g_u);
  vga_b<=std_logic_vector(vga_b_u);
  --vga_hs<=vid_hs;
  --vga_vs<=vid_vs;
  --vga_de<=vid_de;

  ce_pixel<=vid_ce;
  clk_video<=sysclk;
  
  ----------------------------------------------------------
  -- PIA SYSTEM
  i_pia1 : ENTITY work.mc6821
    PORT MAP (
      ad => adpia,
      dw => cpu_dw,
      dr => pia1_dr,
      req => pia1_req,
      ack => OPEN, -- pia1_ack,
      wr  => cpu_wr,
      pa_i  => pia1_pa_i,
      pa_o  => pia1_pa_o,
      pa_d  => pia1_pa_d,
      pb_i  => pia1_pb_i,
      pb_o  => pia1_pb_o,
      pb_d  => pia1_pb_d,
      ca1   => '0', -- Barcode ?
      ca2_i => '0',
      ca2_o => tape_motor_n,
      cb1   => vin_n,
      cb2_i => '0',
      cb2_o => OPEN, -- Péritel commutation
      irqa  => pia1_irqa,
      irqb  => pia1_irqb,
      clk   => sysclk,
      reset_na => reset_na
      );

  pia1_req<=cpu_req AND to_std_logic(cpu_a>=x"A7C0" AND cpu_a<=x"A7C3");
--  pia1_pa_i<=pia1_pa_o;
  pia1_pa_i(1)<=lpen_button;
  pia1_pa_i(6 DOWNTO 2)<="00000";

  pia1_pa_i(7)<=rk7;
  wk7<=pia1_pa_o(6);
  
  pia1_pa_i(0)<='0';
  pia1_pb_i(6 DOWNTO 0)<=pia1_pb_o(6 DOWNTO 0);

  adpia<=cpu_a(0) & cpu_a(1);

  vin_n <=NOT vin;
  
  -- A7C0 : PIA0 : PRA / DDRA
  -- A7C1 : PIA0 : PRB / DDRB
  -- A7C2 : PIA0 : CRA
  -- A7C3 : PIA0 : CRB
  -- A7CC : PIA1 : PRA / DDRA
  -- A7CD : PIA1 : PRB / DDRB
  -- A7CE : PIA1 : CRA
  -- A7CF : PIA1 : CRB
  
  ----------------------------------------------------------
  -- PIA JOYSTICK / SOUND
  i_pia2 : ENTITY work.mc6821
    PORT MAP (
      ad => adpia,
      dw => cpu_dw,
      dr => pia2_dr,
      req => pia2_req,
      ack => OPEN, --pia2_ack,
      wr  => cpu_wr,
      pa_i  => pia2_pa_i,
      pa_o  => pia2_pa_o,
      pa_d  => pia2_pa_d,
      pb_i  => pia2_pb_i,
      pb_o  => pia2_pb_o,
      pb_d  => pia2_pb_d,
      ca1   => '0',-- = PB2 Joystick XA
      ca2_i => '0',-- = PB6 Joystick YA
      ca2_o => OPEN,
      cb1   => '0', -- BUSY PRINTER
      cb2_i => '0',
      cb2_o => OPEN, -- STROBE
      irqa  => pia2_irqa,
      irqb  => pia2_irqb,
      clk   => sysclk,
      reset_na => reset_na
      ); -- Péritel commutation

  pia2_req<=cpu_req AND to_std_logic(cpu_a>=x"A7CC" AND cpu_a<=x"A7CF");
  
  pia2_pb_i(0)<='0'; --pia2_pb_o(0);
  pia2_pb_i(1)<='0'; --pia2_pb_o(1);
  pia2_pb_i(4)<=pia2_pb_o(4);
  pia2_pb_i(5)<=pia2_pb_o(5);
  
  ----------------------------------------------------------
  -- RAM/ROM
  -- CPU                  RAM
  -- 0000:1FFF : 8kB  : 00000 : COLOUR(FORM=0) 02000 : PIX(FORM=1)
  -- 2000:5FFF : 16kB : 04000
  -- 6000:9FFF : 16kB : 6 banques : 08000,0C000,10000,14000,18000,1C000
  
  cpu_aram(12 DOWNTO 0)<=cpu_a(12 DOWNTO 0);
  cpu_aram(16 DOWNTO 13)<=
    "0000" WHEN cpu_a(15 DOWNTO 13)="000" AND pia1_pa_o(0)='0' ELSE -- 0000:1FFF
    "0001" WHEN cpu_a(15 DOWNTO 13)="000" AND pia1_pa_o(0)='1' ELSE -- 0000:1FFF
    "0010" WHEN cpu_a(15 DOWNTO 13)="001" ELSE -- 2000:3FFF
    "0011" WHEN cpu_a(15 DOWNTO 13)="010" ELSE -- 4000:5FFF

    "0100" WHEN cpu_a(15 DOWNTO 13)="011" AND rambank="00000" ELSE -- 6000:7FFF
    "0101" WHEN cpu_a(15 DOWNTO 13)="100" AND rambank="00000" ELSE -- 8000:9FFF
    "0110" WHEN cpu_a(15 DOWNTO 13)="011" AND rambank="00001" ELSE -- 6000:7FFF
    "0111" WHEN cpu_a(15 DOWNTO 13)="100" AND rambank="00001" ELSE -- 8000:9FFF
    "1000" WHEN cpu_a(15 DOWNTO 13)="011" AND rambank="00010" ELSE -- 6000:7FFF
    "1001" WHEN cpu_a(15 DOWNTO 13)="100" AND rambank="00010" ELSE -- 8000:9FFF

    "1010" WHEN cpu_a(15 DOWNTO 13)="011" AND rambank="00011" ELSE -- 6000:7FFF
    "1011" WHEN cpu_a(15 DOWNTO 13)="100" AND rambank="00011" ELSE -- 8000:9FFF
    "1100" WHEN cpu_a(15 DOWNTO 13)="011" AND rambank="00100" ELSE -- 6000:7FFF
    "1101" WHEN cpu_a(15 DOWNTO 13)="100" AND rambank="00100" ELSE -- 8000:9FFF
    "1110" WHEN cpu_a(15 DOWNTO 13)="011" AND rambank="00101" ELSE -- 6000:7FFF
    "1111" WHEN cpu_a(15 DOWNTO 13)="100" AND rambank="00101" ELSE -- 8000:9FFF
    "1110";
  
  mem:PROCESS(sysclk) IS
  BEGIN
    IF rising_edge(sysclk) THEN
      ram_dr<=ram(to_integer(cpu_aram));
      IF cpu_wr='1' AND cpu_ack='1' AND cpu_a<=x"9FFF" THEN
        ram(to_integer(cpu_aram)):= cpu_dw;
      END IF;

      vram_dr<=ram(to_integer(vpage & vram_a));
    END IF;
  END PROCESS;

  ----------------------------------------------------------
  -- ROM
  -- C000:FFFF : 16kB

  -- BASIC=0 : BASIC1 BASIC=1 =BASIC 128
  -- PA5=CBR
  
  cpu_arom(11 DOWNTO 0)<=cpu_a(11 DOWNTO 0);
  cpu_arom(15 DOWNTO 12)<=
    '0' & pia1_pa_o(5)  & "00" WHEN cpu_a(15 DOWNTO 12)=x"C" AND basic='0' ELSE -- 
    '0' & pia1_pa_o(5)  & "01" WHEN cpu_a(15 DOWNTO 12)=x"D" AND basic='0' ELSE -- 
    '0' & pia1_pa_o(5)  & "10" WHEN cpu_a(15 DOWNTO 12)=x"E" AND basic='0' ELSE -- 
    '0' & pia1_pa_o(5)  & "11" WHEN cpu_a(15 DOWNTO 12)=x"F" ELSE               -- 
    '1' & pia1_pa_o(5)  & "00" WHEN cpu_a(15 DOWNTO 12)=x"B" AND basic='1' ELSE -- 
    '1' & pia1_pa_o(5)  & "01" WHEN cpu_a(15 DOWNTO 12)=x"C" AND basic='1' ELSE -- 
    '1' & pia1_pa_o(5)  & "10" WHEN cpu_a(15 DOWNTO 12)=x"D" AND basic='1' ELSE -- 
    '1' & pia1_pa_o(5)  & "11" WHEN cpu_a(15 DOWNTO 12)=x"E" AND basic='1' ELSE -- 
    '0' & pia1_pa_o(5)  & "11";
  
  rom_dr<=ROM(to_integer(cpu_arom)) WHEN rising_edge(sysclk);
  
  romdisk_dr<=x"FF";
  
  ----------------------------------------------------------
  -- REGS
  -- 16 couleurs => 32 entrées palette
  --1FFF [AD+1][AD]

-- REGS :
--             7     6     5     4     3     2     1     0

-- A7DA     : [              PALETTE DATA                 ]

-- A7DB     :                    [    PALETTE ADRESS      ]

-- A7DC   W :  <res>  <VIDMEM>    <VIDFREQ>  < VID MODE   >

-- A7DD   W : [ DISP PAGE  ] CART BASIC  < BORDER COLOUR  >
  
-- A7E4 : W :  0     0     0     0     0     0     0   RSEL
--        R :  <DISP PAGE> CART  BASIC 0     0     0     0   <= RSEL=0
--        R :  TA12  TA11  TA10  TA9   TA8   TA7   TA6   TA5 <= RSEL=1

-- A7E5 : W :  WEDC   0     0    [      RAM BANK SELECT    ]
--        R :  0      0     0    [      RAM BANK SELECT    ]  <= RSEL=0
--        R :  TA4    TA3   TA2   TA1  TA0   H1    H2    H4   <= RSEL=1

-- A7E6   W :  <reserved>
--        R :  ??? <= RSEL=0
--        R :  LT3latch   INILlatch
--             horizontal
-- A7E7   W :  EN_SYS <res> M525  PIAS  <MODEL  >  <RAMTYPE >
--        R :  INITinst  INITlatch  INILinst  0     0    0   LPEN  RSEL
--             vertical
--

  reg_drp<=pal_dr
             WHEN cpu_a(7 DOWNTO 0)=x"DA" ELSE
           "000" & pal_a
             WHEN cpu_a(7 DOWNTO 0)=x"DB" ELSE
           '0' & vconf & vfreq & vmode
             WHEN cpu_a(7 DOWNTO 0)=x"DC" ELSE
           vpage & cart & basic & vborder
             WHEN cpu_a(7 DOWNTO 0)=x"DD" ELSE
           mux(selreg,l_ta(12 DOWNTO 5),vpage & cart & basic & "0000")
             WHEN cpu_a(7 DOWNTO 0)=x"E4" ELSE
           mux(selreg,l_ta(4 DOWNTO 0) & l_ta_h124,"000" & rambank)
             WHEN cpu_a(7 DOWNTO 0)=x"E5" ELSE
           mux(selreg,l_hlr & l_hin & "000000",x"00")
             WHEN cpu_a(7 DOWNTO 0)=x"E6" ELSE
           vin & l_vin & hin & "000" & lpen_irq & selreg
             WHEN cpu_a(7 DOWNTO 0)=x"E7" ELSE
           x"00";

  -- VIN   :  (INITN)                     : 0=Top or bottom of screen 1=In screen vertical
  -- HIN   :  (INILN)                     : 0=Left or right border 1=In screen horizontal
  -- L_HLR : Latched on lightpen position : 0=Left of screen 1=Right of screen
  -- L_HIN : Latched on lightpen position : 0=Left or right border 1=In screen horizontal
  -- L_VIN : Latched on lightpen position : 0=Top or bottom of screen 1=In screen vertical
  
--  res = (v.init << 7) | (init << 6) | (v.inil << 5) | (m_to8_lightpen_intr << 1) | m_to7_lightpen;

 reg_dr<=reg_drp WHEN rising_edge(sysclk);
  
  PROCESS (sysclk,reset_na) IS
  BEGIN
    IF reset_na='0' THEN
      rambank<="00000";
      acc_a7dc<='0';
      vmode<="000";
      vfreq<="00";
      vconf<="00";
      vpage<="00";
      selreg<='0';
      basic<='0';
      cart<='0';
      vtrame<='0';
      ena_sys1<='0';
      
    ELSIF rising_edge(sysclk) THEN
      vin<=to_std_logic(vid_vpos< 200);
      hin<=to_std_logic(vid_hpos< 320*2);
      hlr<=to_std_logic(vid_hpos>=320*2);
      
      lpen_irq2<=lpen_irq;
      IF lpen_irq='1' AND lpen_irq2='0' THEN
        l_ta<=ta;
        l_ta_h124<=ta_h124;
        l_hin<=hin;
        l_vin<=vin;
        l_hlr<=hlr;
      END IF;
      
      lpen_clr<=to_std_logic(cpu_a=x"A7E5" AND cpu_ack='1' AND cpu_wr='0'
                             AND selreg='1');
      
      IF cpu_a=x"A7DA" AND cpu_wr='1' AND cpu_ack='1' THEN
        -- Palette Data
        pal_dw<=cpu_dw;
      END IF;
      
      pal_wr<=to_std_logic(cpu_a=x"A7DA" AND cpu_wr='1' AND cpu_ack='1');
      pal_rd<=to_std_logic(cpu_a=x"A7DA" AND cpu_wr='0' AND cpu_ack='1');
      
      IF pal_wr='1' OR pal_rd='1' THEN
        pal_a<=pal_a+1;
      END IF;
      
      IF cpu_a=x"A7DB" AND cpu_wr='1' AND cpu_ack='1' THEN
        -- Palette Address
        pal_a<=cpu_dw(4 DOWNTO 0);
      END IF;

      IF cpu_a=x"A7DC" AND cpu_wr='1' AND cpu_ack='1' AND
        (acc_a7dc='0' OR ena_sys1='0') THEN
        --  Video modes
        vmode<=cpu_dw(2 DOWNTO 0);
        vfreq<=cpu_dw(4 DOWNTO 3);
        vconf<=cpu_dw(6 DOWNTO 5);
      END IF;
      
      IF cpu_a=x"A7DD" AND cpu_wr='1' AND cpu_ack='1' THEN
        vborder<=cpu_dw(3 DOWNTO 0); -- Border colour
        basic<=cpu_dw(4); -- 0=Basic 1 1=Basic 128
        cart <=cpu_dw(5); -- Cartrdige mapping
        vpage<=cpu_dw(7 DOWNTO 6); -- Video page 0...3
      END IF;
      
      IF cpu_a=x"A7E4" AND cpu_wr='1' AND cpu_ack='1' THEN
        selreg<=cpu_dw(0); -- Reg. read select / LPEN interrupt enable
      END IF;

      IF cpu_a=x"A7E5" AND cpu_wr='1' AND cpu_ack='1' THEN
        rambank<=cpu_dw(4 DOWNTO 0); -- Ram bank 0..5 on MO6
        acc_a7dc<=cpu_dw(7); -- Enable access to A7DC regs.
      END IF;

      IF cpu_a=x"A7E7" AND cpu_wr='1' AND cpu_ack='1' THEN
        vtrame<=cpu_dw(5);
        bankswitch<=cpu_dw(4);
        ena_sys1<=cpu_dw(7);
        --model<=cpu_dw(3 DOWNTO 2);
        --ramtype<=cpu_dw(1 DOWNTO 0);
      END IF;
      
    END IF;
  END PROCESS;
  
  ----------------------------------------------------------
  audio_l<=std_logic_vector((pia2_pb_o(5 DOWNTO 0) & "0000000000") XOR
                            (pia1_pb_o(0) & Z15) XOR (rk7 & Z15));
  audio_r<=std_logic_vector((pia2_pb_o(5 DOWNTO 0) & "0000000000") XOR
                            (pia1_pb_o(0) & Z15) XOR (rk7 & Z15));
  
  -- KBD MO5
  --        7       6       5       4       3       2       1       0
  -- 0                                                    BASIC   SHIFT  
  -- 1    STOP     ACC     CNT    ENTER    CLR      C       ^^      W
  -- 2      1       +       A       *       Q       V       <<      X
  -- 3      2       -       Z       /       S       B       vv    SPACE
  -- 4      3       0       E       P       D       M       >>      @
  -- 5      4       9       R       O       F       L       ^<      .
  -- 6      5       8       T       I       G       K       INS     ,
  -- 7      6       7       Y       U       H       J       DEL     N
  
  --------------------------------------------------------
  --    KBD MO5          QWERTY               AZERTY

  --    STOP             ESCAPE               ESCAPE
  --    1   !            1         shift-1!   shift-1&      !§ [/?]
  --    2   "            2         shift-'"   shift-2é      3"
  --    3   #            3         shift-3#   shift-3"      altgr-3"
  --    4   $            4         shift-4$   shift-4'      $£ []}]
  --    5   %            5         shift-5%   shift-5(      shift-ù% ['"]  ???
  --    6   &            6         shift-7&   shift-6-      1& [1!]
  --    7   '            7         '"         shift-7è      4' [4$]
  --    8   (            8         shift-9(   shift-8_      5( [5%]
  --    9   )            9         shift-0)   shift-9ç      )° [-_]
  --    0   `            0         `~         shift-0à      altgr-7é
  --    -   =            -_        =+         6-            =+ [\|]
  --    +   ;            shift-=+  ;:         shift-=+ [\|] ;. [,<]
  --    CNT              LCTRL + RCTRL        LCTRL + RCTRL
  --    A                A                    A [Q]
  --    Z                Z                    Z [W]
  --    E                E                    E
  --    R                R                    R
  --    T                T                    T
  --    Y                Y                    Y
  --    U                U                    U
  --    I                I                    I
  --    O                O                    O
  --    P                P                    P
  --    /   ?            /?        shift-/?   
  --    *   :            shift-8*  shift-;:   *µ [???]      :/ [.>]
  --    RAZ CLS          BACKSPACE            BACKSPACE
  --    HOME             HOME                 HOME
  --    Q                Q                    Q [A]
  --    S                S                    S
  --    D                D                    D
  --    F                F                    F
  --    G                G                    G
  --    H                H                    H
  --    J                J                    J
  --    K                K                    K
  --    L                L                    L
  --    M                M                    M [;:]
  --    ENTREE           ENTER                ENTER     
  --    SHIFT            LSHIFT               LSHIFT
  --    W                W                    W [Z]
  --    X                X                    X
  --    C                C                    C
  --    V                V                    V
  --    B                B                    B
  --    N                N                    N
  --    ,   <            ,<        shift-,<   ,? [M]     <> [??]
  --    .   >            .>        shift-.>   shift-;.   shift-<> [??]
  --    @   ^            shift-2@  shift-6^   altgr-0à   ^¨ [[{]
  --    BASIC            RCONTROL             RCONTROL
  --    INS              INS                  INS
  --    EFF              DEL                  DEL
  --    UP               UP                   UP
  --    DOWN             DOWN                 DOWN
  --    LEFT             LEFT                 LEFT
  --    RIGHT            RIGHT                RIGHT
  --    SPACE            SPACE                SPACE
  
  --------------------------------------------------------
  --    KBD MO6          QWERTY               AZERTY                DCMOTO
  -- 0  N                N                    N                     N
  -- 1  ,     ?          ,#        /?         ,? [M]    shift-,?[M] ,?
  -- 2  ;     .          ;:        .>         ;.        shift-;.    ;.
  -- 3  #     @          shift-3#  shift-2@   altgr-3"  altgr-0à    ??? => ALT
  -- 4  SPACE            SPACE                SPACE                 SPACE
  -- 5  X                X                    X                     X
  -- 6  W                W                    W                     W
  -- 7  SHIFT            LSHIFT               LSHIFT                LSHIFT
  -- 8  EFF   DEL        DEL                  DEL                   DEL
  -- 9  INS              INS                  INS                   INS
  -- 10 >     <          shift-.>  shift-,<   shift-<>   <>         !§
  -- 11 RIGHT >          RIGHT                RIGHT                 RIGHT
  -- 12 DOWN  v          DOWN                 DOWN                  DOWN
  -- 13 LEFT  <          LEFT                 LEFT                  LEFT
  -- 14 UP    ^          UP                   UP                    UP
  -- 15 BASIC            RSHIFT               RSHIFT                RSHIFT
  -- 16 J                J                    J                     J
  -- 17 K                K                    K                     K
  -- 18 L                L                    L                     L
  -- 19 M                M                    M                     M
  -- 20 B                B                    B                     B
  -- 21 V                V                    V                     V
  -- 22 C                C                    C                     C
  -- 23 SHIFTLOCK        SHIFTLOCK            SHIFTLOCK             SHIFTLOCK
  -- 24 H                H                    H                     H
  -- 25 G                G                    G                     G
  -- 26 F                F                    F                     F
  -- 27 D                D                    D                     D
  -- 28 S                S                    S                     S
  -- 29 Q                Q                    Q [A]                 Q [A]
  -- 30 HOME  RAZ        HOME                 HOME                  HOME
  -- 31 F1    F6         F1        F6         F1        F6          F1
  -- 32 U                U                    U                     U
  -- 33 I                I                    I                     I
  -- 34 O                O                    O                     O
  -- 35 P                P                    P                     P
  -- 36 :     /          shift-;:  /?         :/ [.>]   shift-:/    :/
  -- 37 $     &          shift-4$  shift-7&   $£ []}]   1&          $£
  -- 38 ENTER            ENTER                ENTER                 ENTER
  -- 39 F2    F7         F2        F7         F2        F7          F2
  -- 40 Y                Y                    Y                     Y
  -- 41 T                T                    T                     T
  -- 42 R                R                    R                     R
  -- 43 E                E                    E                     E
  -- 44 Z                Z                    Z [W]                 Z [W]
  -- 45 A                A                    A [Q]                 A [Q]
  -- 46 CNT              LCTRL                LCTRL                 LCTRL
  -- 47 F3    F8         F3        F8         F3        F8          F3
  -- 48 è     7          <??>      7&         7è        shift-7è    7è
  -- 49 !     8          shift-1!  8*         !§ [/?]   shift-8_    8
  -- 50 ç     9          <??>      9(         9ç        shift-9ç    9
  -- 51 à     0          <??>      0)         0à        shift-0à    0
  -- 52 -     \          -_        \|         6-        altgr-8_    BACKSPACE
  -- 53 =     +          =+        shift-=+   =+        shift-=+    =+
  -- 54 ACCENT           RCTRL                RCTRL                 RCTRL
  -- 55 F4    F9         F4        F9         F4        F9          F4
  -- 56 _     6          shift--_  6^         8_        shift-6-    6
  -- 57 (     5          shift-9(  5%         5(        shift-5(    5
  -- 58 '     4          '"        4$         4'        shift-4'    4
  -- 59 "     3          shift-'"  3#         3"        shift-3"    3
  -- 60 é     2          <??>      2@         2é        shift-2é    2 
  -- 61 *     1          shift-8*  1!         *µ        shift-1&    1
  -- 62 STOP             TAB                  TAB                   TAB
  -- 63 F5    F10        F5        F10        F5        F10         F5
  -- 64 [     {          {[        shift-{[   altgr-4'  altgr-5[    <>
  -- 65 ]     }          ]}        shift-]}   altgr-)°  altgr-=+ [\|] *µ
  -- 66 )     °          shift-0)  <??>       )° [-_]   shift-)°    )°
  -- 67 ^     ¨          shift-6  <??>        ^" [{[]   shift-^"    ^"
  -- 68 ù     %          <??>      shift-5%   ù%        shift-ù%    ² `~
  
  
  --        7       6       5       4       3       2       1       0
  -- 0    SHIFT     W       X     SPACE    # @     ; .     , ?      N
  -- 1    BASIC     UP     LEFT    DOWN    RIGHT   > <     INS     DEL
  -- 2    CAPS      C       V       B       M       L       K       J
  -- 3    F1 F6    HOME     Q       S       D       F       G       H  <<30
  
  -- 4    F2 F7    ENTER   $ &     : /      P       O       I       U
  -- 5    F3 F8   CONTROL   A       Z       E       R       T       Y
  -- 6    F4 F9   ACCENT   = +     - \     0 à     9 ç     8 !     7 è
  -- 7    F5 F10   STOP    1 *     2 é     3 "     4 '     5 (     6 _
  -- 8      ___    ___     ___     ù %     ^ "     ) °     ] }     [ {
  
  PROCESS(mo6_keys,pia1_pa_o,pia1_pb_o) IS
    VARIABLE kr_v : uv8;
  BEGIN
    IF pia1_pa_o(3)='0' AND pia1_pa_d(3)='1' THEN -- Pullup
      -- LINE 8
      kr_v:=mo6_keys(71 DOWNTO 64);
    ELSE
      kr_v:=mo6_keys (8*to_integer(pia1_pb_o(3 DOWNTO 1)) + 7 DOWNTO
                      8*to_integer(pia1_pb_o(3 DOWNTO 1)));
    END IF;
    xxx_kr<=kr_v;
    pia1_pb_i(7)<=NOT kr_v(to_integer(pia1_pb_o(6 DOWNTO 4)));
  END PROCESS;
  
  capslock<=pia1_pa_o(4);
  
  ----------------------------------------------------------
  KeyCodes:PROCESS (sysclk,reset_na) IS
  BEGIN
    IF rising_edge(sysclk) THEN
      IF reset_na='0' THEN
        mo6_keys <=(OTHERS =>'0');
        shift<='0';
        altgr<='0';
      END IF;
      
      ps2_key_delay<=ps2_key;
      
      IF ps2_key_delay(10)/=ps2_key(10) THEN
        IF ps2_key(8)='0' THEN
          CASE ps2_key(7 DOWNTO 0) IS
            WHEN x"00" => --
            WHEN X"01" => -- F9
            WHEN x"03" => -- F5
              mo6_keys(63)<=ps2_key(9);
            WHEN x"04" => -- F3
              mo6_keys(47)<=ps2_key(9);
            WHEN x"05" => -- F1
              mo6_keys(31)<=ps2_key(9);
            WHEN x"06" => -- F2
              mo6_keys(39)<=ps2_key(9);
            WHEN x"07" => -- F12
            WHEN x"09" => -- F10
            WHEN x"0A" => -- F8
            WHEN x"0B" => -- F6
            WHEN x"0C" => -- F4
              mo6_keys(55)<=ps2_key(9);
            WHEN x"0D" => -- TAB > STOP
              mo6_keys(62)<=ps2_key(9);
            WHEN X"0E" => -- `~  ²  => ù%
              IF dcmoto='1' THEN
                mo6_keys(68)<=ps2_key(9);
              END IF;
            WHEN x"11" => -- LEFT ALT => #@
              IF dcmoto='1' THEN
                mo6_keys(3)<=ps2_key(9);
              END IF;
            WHEN x"12" => -- LEFT SHIFT
              mo6_keys(7)<=ps2_key(9); shift<=ps2_key(9);
            WHEN X"14" => -- LEFT CTRL
              mo6_keys(46)<=ps2_key(9);
            WHEN x"15" => -- Q  | A
              IF    dcmoto='1' THEN mo6_keys(45)<=ps2_key(9);
              ELSIF azerty='0' THEN mo6_keys(29)<=ps2_key(9);
                               ELSE mo6_keys(45)<=ps2_key(9);
              END IF;
            WHEN x"16" => -- 1! | 1&
              IF dcmoto='1' THEN mo6_keys(61)<=ps2_key(9);
              ELSIF azerty='0' THEN
                IF shift='0' THEN mo6_keys(61)<=ps2_key(9); mo6_keys(7)<='1';
                             ELSE mo6_keys(49)<=ps2_key(9); mo6_keys(7)<='0';
                END IF;
              ELSE
                IF shift='0' THEN mo6_keys(37)<=ps2_key(9); mo6_keys(7)<='1';
                             ELSE mo6_keys(61)<=ps2_key(9); mo6_keys(7)<='1';
                END IF;
              END IF;
            WHEN x"1A" => -- Z  | W
              IF dcmoto='1' THEN mo6_keys(6)<=ps2_key(9);
              ELSIF azerty='0' THEN mo6_keys(44)<=ps2_key(9);
                            ELSE mo6_keys(6) <=ps2_key(9);
              END IF;
            WHEN x"1B" => -- S
              mo6_keys(28)<=ps2_key(9);
            WHEN x"1C" => -- A  | Q
              IF dcmoto='1' THEN mo6_keys(29)<=ps2_key(9);
              ELSIF azerty='0' THEN mo6_keys(45)<=ps2_key(9);
                            ELSE mo6_keys(29)<=ps2_key(9);
              END IF;
            WHEN x"1D" => -- W  | Z
              IF dcmoto='1' THEN mo6_keys(44)<=ps2_key(9);
              ELSIF azerty='0' THEN mo6_keys(6) <=ps2_key(9);
                            ELSE mo6_keys(44)<=ps2_key(9);
              END IF;
            WHEN x"1E" => -- 2@ | 2é
              IF dcmoto='1' THEN mo6_keys(60)<=ps2_key(9);
              ELSIF azerty='0' THEN
                IF shift='0' THEN mo6_keys(60)<=ps2_key(9); mo6_keys(7)<='1';
                             ELSE mo6_keys(3) <=ps2_key(9); mo6_keys(7)<='1';
                END IF;
              ELSE
                IF shift='0' THEN mo6_keys(60)<=ps2_key(9); mo6_keys(7)<='0';
                             ELSE mo6_keys(60)<=ps2_key(9); mo6_keys(7)<='1';
                END IF;
              END IF;
            WHEN x"21" => -- C
              mo6_keys(22)<=ps2_key(9);
            WHEN x"22" => -- X
              mo6_keys(5)<=ps2_key(9);
            WHEN x"23" => -- D
              mo6_keys(27)<=ps2_key(9);
            WHEN x"24" => -- E
              mo6_keys(43)<=ps2_key(9);
            WHEN x"25" => -- 4$ | 4' {
              IF dcmoto='1' THEN mo6_keys(58)<=ps2_key(9);
              ELSIF azerty='0' THEN
                IF shift='0' THEN mo6_keys(58)<=ps2_key(9); mo6_keys(7)<='1';
                             ELSE mo6_keys(37)<=ps2_key(9); mo6_keys(7)<='0';
                END IF;
              ELSIF altgr='0' THEN
                IF shift='0' THEN mo6_keys(58)<=ps2_key(9); mo6_keys(7)<='0';
                             ELSE mo6_keys(58)<=ps2_key(9); mo6_keys(7)<='1';
                END IF;
              ELSE
                mo6_keys(64)<=ps2_key(9); mo6_keys(7)<='1';
              END IF;
            WHEN x"26" => -- 3# | 3" #
              IF dcmoto='1' THEN mo6_keys(59)<=ps2_key(9);
              ELSIF azerty='0' THEN
                IF shift='0' THEN mo6_keys(59)<=ps2_key(9); mo6_keys(7)<='1';
                             ELSE mo6_keys(3) <=ps2_key(9); mo6_keys(7)<='0';
                END IF;
              ELSIF altgr='0' THEN
                IF shift='0' THEN mo6_keys(59)<=ps2_key(9); mo6_keys(7)<='0';
                             ELSE mo6_keys(59)<=ps2_key(9); mo6_keys(7)<='1';
                END IF;
              ELSE
                mo6_keys(3)<=ps2_key(9); mo6_keys(7)<='0';
              END IF;
            WHEN x"29" => -- SPACE
              mo6_keys(4)<=ps2_key(9);
            WHEN x"2A" => -- V
              mo6_keys(21)<=ps2_key(9);
            WHEN x"2B" => -- F
              mo6_keys(26)<=ps2_key(9);
            WHEN x"2C" => -- T
              mo6_keys(41)<=ps2_key(9);
            WHEN x"2D" => -- R
              mo6_keys(42)<=ps2_key(9);
            WHEN x"2E" => -- 5%  | 5( [
              IF dcmoto='1' THEN mo6_keys(57)<=ps2_key(9);
              ELSIF azerty='0' THEN
                IF shift='0' THEN mo6_keys(57)<=ps2_key(9); mo6_keys(7)<='1';
                             ELSE mo6_keys(68)<=ps2_key(9); mo6_keys(7)<='1';
                END IF;
              ELSIF altgr='0' THEN
                IF shift='0' THEN mo6_keys(57)<=ps2_key(9); mo6_keys(7)<='0';
                             ELSE mo6_keys(57)<=ps2_key(9); mo6_keys(7)<='1';
                END IF;
              ELSE
                mo6_keys(64)<=ps2_key(9); mo6_keys(7)<='0';
              END IF;
            WHEN x"31" => -- N
              mo6_keys(0)<=ps2_key(9);
            WHEN x"32" => -- B
              mo6_keys(20)<=ps2_key(9);
            WHEN x"33" => -- H
              mo6_keys(24)<=ps2_key(9);
            WHEN x"34" => -- G
              mo6_keys(25)<=ps2_key(9);
            WHEN x"35" => -- Y
              mo6_keys(40)<=ps2_key(9);
            WHEN x"36" => -- 6^  | 6-
              IF dcmoto='1' THEN mo6_keys(56)<=ps2_key(9);
              ELSIF azerty='0' THEN
                IF shift='0' THEN mo6_keys(56)<=ps2_key(9); mo6_keys(7)<='1';
                             ELSE mo6_keys(67)<=ps2_key(9); mo6_keys(7)<='0';
                END IF;
              ELSE
                IF shift='0' THEN mo6_keys(52)<=ps2_key(9); mo6_keys(7)<='0';
                             ELSE mo6_keys(56)<=ps2_key(9); mo6_keys(7)<='1';
                END IF;
              END IF;
            WHEN x"3A" => -- M   | ,?
              IF dcmoto='1' THEN mo6_keys(1)<=ps2_key(9);
              ELSIF azerty='0'   THEN mo6_keys(19)<=ps2_key(9);
                              ELSE mo6_keys(1) <=ps2_key(9); END IF;
            WHEN x"3B" => -- J
              mo6_keys(16)<=ps2_key(9);
            WHEN x"3C" => -- U
              mo6_keys(32)<=ps2_key(9);
            WHEN x"3D" => -- 7&  | 7è
              IF dcmoto='1' THEN mo6_keys(48)<=ps2_key(9);
              ELSIF azerty='0' THEN
                IF shift='0' THEN mo6_keys(48)<=ps2_key(9); mo6_keys(7)<='1';
                             ELSE mo6_keys(37)<=ps2_key(9); mo6_keys(7)<='1';
                END IF;
              ELSE
                IF shift='0' THEN mo6_keys(48)<=ps2_key(9); mo6_keys(7)<='0';
                             ELSE mo6_keys(48)<=ps2_key(9); mo6_keys(7)<='1';
                END IF;
              END IF;
            WHEN x"3E" => -- 8*  | 8_ \
              IF dcmoto='1' THEN mo6_keys(49)<=ps2_key(9);
              ELSIF azerty='0' THEN
                IF shift='0' THEN mo6_keys(49)<=ps2_key(9); mo6_keys(7)<='1';
                             ELSE mo6_keys(61)<=ps2_key(9); mo6_keys(7)<='0';
                END IF;
            ELSIF altgr='0' THEN
                IF shift='0' THEN mo6_keys(56)<=ps2_key(9); mo6_keys(7)<='0';
                             ELSE mo6_keys(49)<=ps2_key(9); mo6_keys(7)<='1';
                END IF;
              ELSE
                mo6_keys(52)<=ps2_key(9); mo6_keys(7)<='1';
              END IF;
            WHEN x"41" => -- ,<  | ;.
              IF dcmoto='1' THEN mo6_keys(2)<=ps2_key(9);
              ELSIF azerty='0' THEN
                IF shift='0' THEN mo6_keys(1) <=ps2_key(9); mo6_keys(7)<='0';
                             ELSE mo6_keys(10)<=ps2_key(9); mo6_keys(7)<='1';
                END IF;
              ELSE
                IF shift='0' THEN mo6_keys(2)<=ps2_key(9); mo6_keys(7)<='0';
                             ELSE mo6_keys(2)<=ps2_key(9); mo6_keys(7)<='1';
                END IF;
              END IF;
            WHEN x"42" => -- K
              mo6_keys(17)<=ps2_key(9);
            WHEN x"43" => -- I
              mo6_keys(33)<=ps2_key(9);
              xx_lock<=xx_lock XOR ps2_key(9);
            WHEN x"44" => -- O
              mo6_keys(34)<=ps2_key(9);
            WHEN x"45" => -- 0)  | 0à @
              IF dcmoto='1' THEN mo6_keys(51)<=ps2_key(9);
              ELSIF azerty='0' THEN
                IF shift='0' THEN mo6_keys(51)<=ps2_key(9); mo6_keys(7)<='1';
                             ELSE mo6_keys(66)<=ps2_key(9); mo6_keys(7)<='0';
                END IF;
            ELSIF altgr='0' THEN
                IF shift='0' THEN mo6_keys(51)<=ps2_key(9); mo6_keys(7)<='0';
                             ELSE mo6_keys(51)<=ps2_key(9); mo6_keys(7)<='1';
                END IF;
              ELSE
                mo6_keys(3)<=ps2_key(9); mo6_keys(7)<='0';
              END IF;
            WHEN x"46" => -- 9(  | 9ç
              IF dcmoto='1' THEN mo6_keys(50)<=ps2_key(9);
              ELSIF azerty='0' THEN
                IF shift='0' THEN mo6_keys(50)<=ps2_key(9); mo6_keys(7)<='1';
                             ELSE mo6_keys(57)<=ps2_key(9); mo6_keys(7)<='0';
                END IF;
  	           ELSE
                IF shift='0' THEN mo6_keys(50)<=ps2_key(9); mo6_keys(7)<='0';
                             ELSE mo6_keys(50)<=ps2_key(9); mo6_keys(7)<='1';
                END IF;
              END IF;
            WHEN X"49" => -- .>  | :/
              IF dcmoto='1' THEN mo6_keys(36)<=ps2_key(9);
              ELSIF azerty='0' THEN
                IF shift='0' THEN mo6_keys(2) <=ps2_key(9); mo6_keys(7)<='1';
                             ELSE mo6_keys(10)<=ps2_key(9); mo6_keys(7)<='0';
                END IF;
              ELSE
                IF shift='0' THEN mo6_keys(36)<=ps2_key(9); mo6_keys(7)<='0';
                             ELSE mo6_keys(36)<=ps2_key(9); mo6_keys(7)<='1';
                END IF;
              END IF;
                                
            WHEN X"4A" => -- /?  | !§
              IF azerty='0' THEN
                IF shift='0' THEN mo6_keys(36)<=ps2_key(9); mo6_keys(7)<='1';
                             ELSE mo6_keys(1) <=ps2_key(9); mo6_keys(7)<='1';
                END IF;
              ELSE
                IF shift='0' THEN mo6_keys(49)<=ps2_key(9); mo6_keys(7)<='0';
                             ELSE NULL;
                END IF;
              END IF;
            WHEN x"4B" => -- L
              mo6_keys(18)<=ps2_key(9);
            WHEN x"4C" => -- ;:  | M
              IF azerty='0' THEN
                IF shift='0' THEN mo6_keys(2) <=ps2_key(9); mo6_keys(7)<='0';
                             ELSE mo6_keys(36)<=ps2_key(9); mo6_keys(7)<='0';
                END IF;
              ELSE
                mo6_keys(19)<=ps2_key(9);
              END IF;
              WHEN x"4D" => -- P
                mo6_keys(35)<=ps2_key(9);
            WHEN x"4E" => -- -_  | )° ]
              IF dcmoto='1' THEN mo6_keys(66)<=ps2_key(9);
              ELSIF azerty='0' THEN
                IF shift='0' THEN mo6_keys(52)<=ps2_key(9); mo6_keys(7)<='0';
                             ELSE mo6_keys(56)<=ps2_key(9); mo6_keys(7)<='0';
                END IF;
            ELSIF altgr='0' THEN
                IF shift='0' THEN mo6_keys(66)<=ps2_key(9); mo6_keys(7)<='0';
                             ELSE mo6_keys(66)<=ps2_key(9); mo6_keys(7)<='1';
                END IF;
              ELSE
                mo6_keys(65)<=ps2_key(9); mo6_keys(7)<='0';
              END IF;
            WHEN x"52" => -- '"  | ù%
              IF azerty='0' THEN
                IF shift='0' THEN mo6_keys(58)<=ps2_key(9); mo6_keys(7)<='0';
                             ELSE mo6_keys(59)<=ps2_key(9); mo6_keys(7)<='0';
                END IF;
              ELSE
                IF shift='0' THEN mo6_keys(68)<=ps2_key(9); mo6_keys(7)<='0';
                             ELSE mo6_keys(68)<=ps2_key(9); mo6_keys(7)<='1';
                END IF;
              END IF;
            WHEN x"54" => -- [{  | ^"
              IF dcmoto='1' THEN mo6_keys(67)<=ps2_key(9);
              ELSIF azerty='0' THEN
                IF shift='0' THEN mo6_keys(64)<=ps2_key(9); mo6_keys(7)<='0';
                             ELSE mo6_keys(64)<=ps2_key(9); mo6_keys(7)<='1';
                END IF;
              ELSE
                IF shift='0' THEN mo6_keys(67)<=ps2_key(9); mo6_keys(7)<='0';
                             ELSE mo6_keys(67)<=ps2_key(9); mo6_keys(7)<='1';
                END IF;
              END IF;
             WHEN x"55" => -- =+  | =+ }
              IF dcmoto='1' THEN mo6_keys(53)<=ps2_key(9);
              ELSIF azerty='0' OR altgr='0' THEN
                mo6_keys(53)<=ps2_key(9);
              ELSE
                mo6_keys(65)<=ps2_key(9); mo6_keys(7)<='1';
              END IF;
            WHEN x"58" => -- CAPS LOCK
              mo6_keys(23)<=ps2_key(9);
            WHEN x"59" => -- RIGHT SHIFT > BASIC
              mo6_keys(15)<=ps2_key(9);
            WHEN x"5A" => -- ENTER
              mo6_keys(38)<=ps2_key(9);
            WHEN x"5B" => -- ]}  | $£
              IF dcmoto='1' THEN mo6_keys(37)<=ps2_key(9);
              ELSIF azerty='0' THEN
                IF shift='0' THEN mo6_keys(65)<=ps2_key(9); mo6_keys(7)<='0';
                             ELSE mo6_keys(65)<=ps2_key(9); mo6_keys(7)<='1';
                END IF;
              ELSE
                IF shift='0' THEN mo6_keys(37)<=ps2_key(9); mo6_keys(7)<='0';
                             ELSE NULL;
                END IF;
              END IF;
            WHEN x"5D" => -- \|  | *µ
              IF dcmoto='1' THEN mo6_keys(65)<=ps2_key(9);
              ELSIF azerty='0' THEN
                IF shift='0' THEN mo6_keys(52)<=ps2_key(9); mo6_keys(7)<='1';
                             ELSE NULL;
                END IF;
              ELSE
                IF shift='0' THEN mo6_keys(61)<=ps2_key(9); mo6_keys(7)<='0';
                             ELSE NULL;
                END IF;
              END IF;
            WHEN x"61" => --     | <>
              IF dcmoto='1' THEN mo6_keys(64)<=ps2_key(9);
              ELSIF azerty='1' THEN
                mo6_keys(10)<=ps2_key(9);
              END IF;
            WHEN x"66" => -- BACKSPACE
            WHEN x"69" => -- Keypad 1 end
            WHEN x"6B" => -- Keypad 4 left
              mo6_keys(13)<=ps2_key(9);
            WHEN x"6C" => -- Keypad 7 home
            WHEN x"70" => -- Keypad 0 ins
            WHEN x"71" => -- Keypad . del
            WHEN x"72" => -- Keypad 2 down
              mo6_keys(12)<=ps2_key(9);
            WHEN x"73" => -- Keypad 5
            WHEN x"74" => -- Keypad 6 right
              mo6_keys(11)<=ps2_key(9);
            WHEN x"75" => -- Keypad 8 up
              mo6_keys(14)<=ps2_key(9);
            WHEN x"76" => -- ESCAPE
            WHEN x"77" => -- Num Lock
            WHEN x"78" => -- F11
            WHEN x"79" => -- Keypad +
            WHEN x"7A" => -- Keypad 3 pgdown
            WHEN x"7B" => -- Keypad -
            WHEN x"7C" => -- Keypad *
            WHEN x"7D" => -- Keypad 9 pgup
            WHEN x"7E" => -- Scroll Lock
            WHEN x"83" => -- F7
            WHEN OTHERS => NULL;
          END CASE;
        ELSE
          CASE ps2_key(7 DOWNTO 0) IS -- E0 prefix
            WHEN x"11"   => -- R ALT   | ALTGR
              altgr<=ps2_key(9);
            WHEN x"12"   => -- PRTSCR
            WHEN x"14"   => -- RIGHT CTRL > ACCENT
              mo6_keys(54)<=ps2_key(9);
            WHEN x"1F"   => -- Left Win
            WHEN x"27"   => -- Right Win
            WHEN x"2F"   => -- Win menu
            WHEN x"4A"   => -- Keypad /
            WHEN x"5A"   => -- Keypad Enter
            WHEN x"69"   => -- End
            WHEN x"6B"   => -- Left
              mo6_keys(13)<=ps2_key(9);
            WHEN x"6C"   => -- Home
            WHEN x"70"   => -- Insert
            WHEN x"71"   => -- Del
            WHEN x"72"   => -- Down
              mo6_keys(12)<=ps2_key(9);
            WHEN x"74"   => -- Right
              mo6_keys(11)<=ps2_key(9);
            WHEN x"75"   => -- Up
              mo6_keys(14)<=ps2_key(9);
            WHEN x"7A"   => -- PageDown
            WHEN x"7C"   => -- PrintScreen
            WHEN x"7D"   => -- PageUp
            WHEN OTHERS  =>
              NULL;
          END CASE;
        END IF;
      END IF;
    END IF;
  END PROCESS KeyCodes;
  
  ----------------------------------------------------------
  -- Light Pen
  -- PS2_MOUSE(0)     : LEFT
  -- PS2_MOUSE(1)     : RIGHT
  -- PS2_MOUSE(2)     : MIDDLE
  -- PS2_MOUSE(4)     : X sign
  -- PS2_MOUSE(5)     : Y sign
  -- PS2_MOUSE(15:8)  : X diff
  -- PS2_MOUSE(23:16) : Y diff
  -- PS2_MOUSE(24)    : Toggle
  
  PROCESS(sysclk) IS
    VARIABLE tmp : integer RANGE -1024*8 TO 1024*8-1;
  BEGIN
    IF rising_edge(sysclk) THEN
      ps2_mouse_delay<=ps2_mouse;
      
      IF ps2_mouse(24)/=ps2_mouse_delay(24) THEN
        tmp:=mxpos+to_integer(signed(ps2_mouse(15 DOWNTO 8)));
        IF tmp<-32*4 THEN tmp:=-32*4; END IF;
        IF tmp>=(320+32)*4 THEN tmp:=(320+32)*4; END IF;
        mxpos<=tmp;
        
        tmp:=mypos-to_integer(signed(ps2_mouse(23 DOWNTO 16)));
        IF tmp<=-32*4 THEN tmp:=-32*4; END IF;
        IF tmp>=(200+32)*4 THEN tmp:=(200+32)*4; END IF;
        mypos<=tmp;
      END IF;
      
      xpos<=mxpos/4;
      ypos<=mypos/4;
      
      -- 320 pix/ligne 
      ta<=to_unsigned(vid_vpos * 40 + vid_hpos/2/8,13);
      ta_h124<= to_unsigned((vid_hpos/2) MOD 8,3) +  "110";
      
      IF azerty='1' THEN ta_h124<="000"; END IF;
      
      -- 0x280   => 0x50
      lpen_irq<=(lpen_irq OR
                 to_std_logic(xpos=vid_hpos/2 AND
                              (ypos  =vid_vpos OR ypos+1=vid_vpos OR
                               ypos+2=vid_vpos OR ypos+3=vid_vpos))) AND
                 to_std_logic(xpos>=0 AND ypos>=0 AND xpos<=639 AND ypos<=199)
                 AND selreg AND NOT lpen_clr;
      
      lpen_button<=ps2_mouse(0);
      
    END IF;
  END PROCESS;
  -- ----------------------------------------------------------
  -- Joysticks
  pia2_pa_i(3)<=NOT joystick_0(0); -- UP
  pia2_pa_i(2)<=NOT joystick_0(1); -- DOWN
  pia2_pa_i(1)<=NOT joystick_0(2); -- LEFT
  pia2_pa_i(0)<=NOT joystick_0(3); -- RIGHT
  pia2_pb_i(6)<=NOT joystick_0(4); -- Button 
  pia2_pb_i(2)<=NOT joystick_0(5); -- Button

  pia2_pa_i(7)<=NOT joystick_1(0); -- UP
  pia2_pa_i(6)<=NOT joystick_1(1); -- DOWN
  pia2_pa_i(5)<=NOT joystick_1(2); -- LEFT
  pia2_pa_i(4)<=NOT joystick_1(3); -- RIGHT
  pia2_pb_i(7)<=NOT joystick_1(4); -- Button 
  pia2_pb_i(3)<=NOT joystick_1(5); -- Button
  
  --  pia1_pa_o(2) : MUTE SOURIS
  
  ----------------------------------------------------------
  -- Tape interface
  i_motape: ENTITY work.motape
    GENERIC MAP (SYSFREQ => 32000000)
    PORT MAP (
      motor_n        => tape_motor_n,
      rk7            => rk7,
      wk7            => wk7,
      rewind         => rewind,
      tratio         => tratio,
      tpos           => tpos,
      ioctl_download => ioctl_download,
      ioctl_index    => ioctl_index,
      ioctl_wr       => ioctl_wr,
      ioctl_addr     => ioctl_addr,
      ioctl_data     => ioctl_dout,
      ioctl_wait     => ioctl_wait,
      
      ddram_clk        => ddram_clk,
      ddram_busy       => ddram_busy,
      ddram_burstcnt   => ddram_burstcnt,
      ddram_addr       => ddram_addr,
      ddram_dout       => ddram_dout,
      ddram_dout_ready => ddram_dout_ready,
      ddram_rd         => ddram_rd,
      ddram_din        => ddram_din,
      ddram_be         => ddram_be,
      ddram_we         => ddram_we,
      
      fast           => fast,
      sysclk         => sysclk,
      reset_na       => reset_na);
  
END ARCHITECTURE struct;
