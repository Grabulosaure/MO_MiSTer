--------------------------------------------------------------------------------
-- Thomson MO5 / MO6
--------------------------------------------------------------------------------
-- DO 12/2019
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.std_logic_1164.all;
USE IEEE.numeric_std.all;

LIBRARY work;
USE work.base_pack.ALL;

ENTITY motape IS
  GENERIC (
    SYSFREQ : natural);
  PORT (
    motor_n        : IN std_logic;
    rk7            : OUT std_logic;
    wk7            : IN  std_logic;
    
    rewind         : IN std_logic;
    
    tpos           : OUT uv32;
    tlen           : OUT uv32;
    tratio         : OUT uv8;
    
    ioctl_download : IN  std_logic;
    ioctl_index    : IN  std_logic_vector(7 DOWNTO 0);
    ioctl_wr       : IN  std_logic;
    ioctl_addr     : IN  std_logic_vector(24 DOWNTO 0);
    ioctl_data     : IN  std_logic_vector(7 DOWNTO 0);
    ioctl_wait     : OUT std_logic;
    
    ddram_clk        : OUT   std_logic;
    ddram_busy       : IN    std_logic;
    ddram_burstcnt   : OUT   std_logic_vector(7 DOWNTO 0);
    ddram_addr       : OUT   std_logic_vector(28 DOWNTO 0);
    ddram_dout       : IN    std_logic_vector(63 DOWNTO 0);
    ddram_dout_ready : IN    std_logic;
    ddram_rd         : OUT   std_logic;
    ddram_din        : OUT   std_logic_vector(63 DOWNTO 0);
    ddram_be         : OUT   std_logic_vector(7 DOWNTO 0);
    ddram_we         : OUT   std_logic;

    fast           : IN std_logic;
    sysclk         : IN std_logic;
    reset_na       : IN std_logic);
    
END motape;

ARCHITECTURE rtl OF motape IS
  SIGNAL head : string(1 TO 4);
  SIGNAL size,filesize  : uv32;
  SIGNAL rpos,wpos : uv32;
  SIGNAL fmt : uv128;
  ALIAS  fmt_audio    : uv16 IS fmt(15 DOWNTO 0);    -- Audio Format 1=PCM
  ALIAS  fmt_chan     : uv16 IS fmt(31 DOWNTO 16);   -- Number of channels
  ALIAS  fmt_freq     : uv32 IS fmt(63 DOWNTO 32);   -- Sampling freq
  ALIAS  fmt_bpsec    : uv32 IS fmt(95 DOWNTO 64);   -- Bytes Per Sample
  ALIAS  fmt_bpbloc   : uv16 IS fmt(111 DOWNTO 96);  -- Bytes Per Bloc
  ALIAS  fmt_bpsample : uv16 IS fmt(127 DOWNTO 112); -- Bits Per Sample
  SIGNAL alt : uint8;

  TYPE enum_state IS (sIDLE,sHEAD,
                      sBLOCK_RIFF,sBLOCK_FMT,sBLOCK_DATA,sBLOCK_SKIP);
  SIGNAL state : enum_state;

  SIGNAL ioctl_wait_l,motor_pre_n : std_logic;
  SIGNAL sample : std_logic;
  SIGNAL tick : std_logic;
  SIGNAL acc   : integer RANGE -32768*32768 TO 32768*32768-1;
  SIGNAL freq : uint24;
  
  SIGNAL divrun : std_logic;
  SIGNAL vdivr : unsigned(64 DOWNTO 0);
  SIGNAL vdivi : uv32;
  SIGNAL divcpt : uint8;

  SIGNAL ddram_we_l,ddram_rd_l : std_logic;
  SIGNAL wshift,rshift : uv64;
  SIGNAL pushsample,pushsample2,pushlast : std_logic;
  SIGNAL rcpt,wcpt : uint6;
  SIGNAL waddr,raddr : uv29;
  SIGNAL loaded,rsample : std_logic;
BEGIN


-- 00  52 49 46 46 e4 c3 44 01  57 41 56 45 66 6d 74 20  |RIFF....WAVEfmt |
-- 10  10 00 00 00 01 00 01 00  44 ac 00 00 44 ac 00 00  |................|
-- 20  01 00 08 00 64 61 74 61  bf c3 44 01 80 80 80 80  |....data........|

-- 52 49 46 46 "RIFF"
-- E4 C3 44 01 File Length
-- 57 41 56 45 "WAVE"

-- 66 6d 74 20 "fmt "
-- 10 00 00 00 Block size
-- 01 00       Audioformat
-- 01 00       Channels
-- 44 AC 00 00 Freq
-- 44 AC 00 00 Bytes per second
-- 01 00       Bytes per Bloc
-- 08 00       Bits per sample


-- 64 61 74 61 "data"
-- BF C3 44 01 Block size
  
  rk7<=rsample WHEN motor_n='0' ELSE '1';
  
  PROCESS(sysclk,reset_na) IS
    VARIABLE t : uint8;
  BEGIN
    IF reset_na='0' THEN
      ioctl_wait_l<='0';
      state<=sIDLE;
      
    ELSIF rising_edge(sysclk) THEN

      pushsample<='0';
      pushlast<='0';
      --------------------------------------------
      CASE state IS
        WHEN sIDLE =>
          IF ioctl_download='1' THEN
            state<=sHEAD;
          END IF;
          wpos<=x"00000000";
          ioctl_wait_l<='0';
          ddram_we_l<='0';
          
        WHEN sHEAD =>
          loaded<='0';
          -- Block header
          IF ioctl_wr='1' AND ioctl_wait_l='0' THEN
            t:=to_integer(unsigned(ioctl_data));
            IF wpos=0 THEN head(1)<=character'val(t); END IF;
            IF wpos=1 THEN head(2)<=character'val(t); END IF;
            IF wpos=2 THEN head(3)<=character'val(t); END IF;
            IF wpos=3 THEN head(4)<=character'val(t); END IF;
            IF wpos=4 THEN size( 7 DOWNTO 0)<=unsigned(ioctl_data); END IF;
            IF wpos=5 THEN size(15 DOWNTO 8)<=unsigned(ioctl_data); END IF;
            IF wpos=6 THEN size(23 DOWNTO 16)<=unsigned(ioctl_data); END IF;
            IF wpos=7 THEN size(31 DOWNTO 24)<=unsigned(ioctl_data); END IF;
            
            wpos<=wpos+1;
            IF wpos=7 THEN
              wpos<=x"00000000";
              IF head="RIFF" THEN
                state<=sBLOCK_RIFF;
              ELSIF head="fmt " THEN
                state<=sBLOCK_FMT;
              ELSIF head="data" THEN
                state<=sBLOCK_DATA;
              ELSE
                state<=sBLOCK_SKIP;
              END IF;
            END IF;
          END IF;
          
        WHEN sBLOCK_RIFF =>
          filesize<=size;
          IF ioctl_wr='1' AND ioctl_wait_l='0' THEN
            wpos<=wpos+1;
            IF wpos=3 THEN
              wpos<=x"00000000";
              state<=sHEAD;
            END IF;
          END IF;
          
        WHEN sBLOCK_FMT =>
          IF ioctl_wr='1' AND ioctl_wait_l='0' THEN
            fmt(to_integer(wpos)*8+7 DOWNTO
                to_integer(wpos)*8)<=unsigned(ioctl_data);
            wpos<=wpos+1;
            IF wpos=size-1 THEN
              wpos<=x"00000000";
              state<=sHEAD;
            END IF;
          END IF;
          
        WHEN sBLOCK_SKIP =>
          IF ioctl_wr='1' AND ioctl_wait_l='0' THEN
            wpos<=wpos+1;
            IF wpos=size-1 THEN
              wpos<=x"00000000";
              state<=sHEAD;
            END IF;
          END IF;
          
        WHEN sBLOCK_DATA =>
          IF ioctl_wr='1' AND ioctl_wait_l='0' THEN
            wpos<=wpos+1;
            IF alt=0 AND fmt_bpsample=8 THEN
              sample<=ioctl_data(7);
              pushsample<='1';
            END IF;
            IF alt=1 AND fmt_bpsample=16 THEN
              sample<=NOT ioctl_data(7);
              pushsample<='1';
            END IF;
            
            IF alt=fmt_bpbloc-1 THEN
              alt<=0;
            ELSE
              alt<=alt+1;
            END IF;
            
          END IF;
          IF ioctl_download='0' THEN
            pushlast<='1';
            loaded<='1';
          END IF;
          
      END CASE;
      
      --------------------------------------------
      IF ioctl_download='0' THEN
        state<=sIDLE;
      END IF;
      
      --------------------------------------------
      IF state=sIDLE THEN
        wcpt<=0;
        waddr<=(OTHERS =>'0');
      ELSIF pushsample='1' THEN
        wshift(wcpt)<=sample;
        wcpt<=(wcpt+1) MOD 64;
      END IF;

      pushsample2<=pushsample;
      IF (pushsample2='1' AND wcpt=0) OR pushlast='1' THEN
        ddram_we_l<='1';
        ddram_din<=std_logic_vector(wshift);
        ddram_addr<=std_logic_vector(waddr);
        waddr<=waddr+1;
      END IF;
      
      --------------------------------------------

	--if ( (cassstate & CASSETTE_MASK_MOTOR) == CASSETTE_MOTOR_DISABLED &&  !state && pos > 0.3 )
	--{
	--	/* rewind a little before starting the motor */
	--	m_cassette->seek(-0.3, SEEK_CUR );
	--}
      motor_pre_n<=motor_n;
      
      IF loaded='0' OR rewind='1' THEN
        raddr<=(OTHERS =>'0');
        rcpt<=0;
        ddram_rd_l<='0';
        rpos<=x"00000000";
        
      ELSIF motor_n='0' AND motor_pre_n='1' THEN
        IF rpos>206 THEN
          rpos<=rpos - 206;
        END IF;
        
      ELSIF tick='1' AND motor_n='0' THEN
        IF rcpt=0 THEN
          ddram_rd_l<='1';
          ddram_addr<=std_logic_vector(raddr);
          raddr<=raddr+1;
        END IF;
        rcpt<=(rcpt + 1 ) MOD 64;
        rsample<=rshift((rcpt+63) MOD 64);
        rpos<=rpos+1;
        
      END IF;
      
      IF ddram_dout_ready='1' THEN
        rshift<=unsigned(ddram_dout);
      END IF;
      
      --------------------------------------------
      IF ddram_we_l='1' AND ddram_busy='0' THEN
        ddram_we_l<='0';
      END IF;
      IF ddram_rd_l='1' AND ddram_busy='0' THEN
        ddram_rd_l<='0';
      END IF;
      
      --------------------------------------------
      ddram_addr(28 DOWNTO 25)<="0011";
      
    END IF;
  END PROCESS;
    
  -------------------------------------------------
  ClockGen:PROCESS (sysclk, reset_na)
  BEGIN
    IF reset_na = '0' THEN
      tick<='0';
    ELSIF rising_edge(sysclk) THEN
      IF fast='0' THEN
        freq<=to_integer(fmt_freq);
      ELSE
        freq<=to_integer(fmt_freq)*16;
      END IF;
      IF acc>0 THEN
        acc<=acc - freq;
        tick<='0';
      ELSE
        acc<=acc+SYSFREQ - freq;
        tick<='1';
      END IF;
    END IF;
  END PROCESS ClockGen;
  
  -------------------------------------------------
  tpos<=rpos;
  tlen<=size;

  ioctl_wait<=ioctl_wait_l;

  ddram_clk<=sysclk;
  ddram_burstcnt<=x"01";
  ddram_rd<=ddram_rd_l;
  ddram_we<=ddram_we_l;
  ddram_be<=x"FF";
  
  -----------------------------------------------------------------------------
  -- 32 / 32 --> 32
  Dividers:PROCESS (sysclk) IS
  BEGIN
    IF rising_edge(sysclk) THEN
      vdivi<=size(30 DOWNTO 0) & '0';
      vdivr<=rpos(31 DOWNTO 0) & '0' & x"00000000";
      
      ------------------------------------------------------
      IF tick='1' THEN
        divcpt<=0;
        divrun<='1';
        
      ELSIF divrun='1' THEN
        ----------------------------------------------------
        IF divcpt=8 THEN
          divrun<='0';
          tratio<=vdivr(6 DOWNTO 0) & NOT vdivr(64);
        ELSE
          divcpt<=divcpt+1;
        END IF;
        
        IF vdivr(64)='0' THEN
          vdivr(64 DOWNTO 32)<=vdivr(63 DOWNTO 31) - vdivi;
        ELSE
          vdivr(64 DOWNTO 32)<=vdivr(63 DOWNTO 31) + vdivi;
        END IF;
        vdivr(15 DOWNTO 0)<=vdivr(14 DOWNTO 0) & NOT vdivr(64);
        
        ----------------------------------------------------
      END IF;
    END IF;
  END PROCESS Dividers;
  
  -------------------------------------------------
  
END ARCHITECTURE rtl;


-- MGT :
--  Lancer : run "

-- Le moteur démarre/arrête une première fois avec "FOUND MGT.BAS",
--   puis une seconde fois avec  "Erreur 53"
