package Console_Management is

   subtype Rows is Natural range 0 .. 24;

   subtype Columns is Natural range 0 .. 79;

   type Position is
      record
         Row    : Rows;
         Column : Columns;
      end record;
   pragma Convention (C, Position);

   -- Upper left corner of the screen
   UpLeft_Position    : constant Position := (Row => 0, Column => 0);
   -- Lower right corner of the screen
   DownRight_Position : constant Position := (Row => Rows'Last,
                                              Column => Columns'Last);
   -- Text (foreground) colors
   type Colors is
     (Black,
      Blue,
      Green,
      Cyan,
      Red,
      Magenta,
      Brown,
      LightGray,
      DarkGray,
      LightBlue,
      LightGreen,
      LightCyan,
      LightRed,
      LightMagenta,
      Yellow,
      White);

   -- Background colors
   type BkColors is
     (Black,
      Blue,
      Green,
      Cyan,
      Red,
      Magenta,
      Brown,
      LightGray);

   -----------------------------------------------------------------------------
   -- Change the Text Attributes -----------------------------------------------
   -----------------------------------------------------------------------------

   procedure Set_Text_Background_Color (BkC : in BkColors);
   pragma Export (C, Set_Text_Background_Color, "set_text_background_color");

   procedure Set_Text_Color (C : in Colors);
   pragma Export (C, Set_Text_Color, "set_text_color");

   procedure Set_HighVideo;
   pragma Export (C, Set_HighVideo, "set_highvideo");

   procedure Set_LowVideo;
   pragma Export (C, Set_LowVideo, "set_lowvideo");

   procedure Set_Blink;
   pragma Export (C, Set_Blink, "set_blink");

   procedure Cancel_Blink;
   pragma Export (C, Cancel_Blink, "cancel_blink");

   procedure Activate_Scroll;
   pragma Export (C, Activate_Scroll, "activate_scroll");

   procedure Deactivate_Scroll;
   pragma Export (C, Deactivate_Scroll, "deactivate_scroll");

   procedure Set_Cursor (To : in Position);
   pragma Export (C, Set_Cursor, "set_cursor");
   pragma Export_Procedure (Internal => Set_Cursor, External => "set_cursor",
                            Mechanism => References);

   procedure Set_Cursor (Row : in Rows; Column : in Columns);

   procedure Get_Cursor (To : out Position);
   pragma Export (C, Get_Cursor, "get_cursor");
   pragma Export_Procedure (Internal => Get_Cursor, External => "get_cursor",
                            Mechanism => References);

   -----------------------------------------------------------------------------
   -- Clear the screen ---------------------------------------------------------
   -----------------------------------------------------------------------------

   procedure Clear_Screen;
   pragma Export (C, Clear_Screen, "clear");

   -----------------------------------------------------------------------------
   -- Console input configuration ----------------------------------------------
   -----------------------------------------------------------------------------

   procedure Set_Cooked_Mode;
   pragma Export (C, Set_Cooked_Mode, "set_cooked_mode");

   procedure Set_Raw_Mode;
   pragma Export (C, Set_Raw_Mode, "set_raw_mode");

   procedure Enable_Echo;
   pragma Export (C, Enable_Echo, "enable_echo");

   procedure Disable_Echo;
   pragma Export (C, Disable_Echo, "disable_echo");

   procedure Set_Blocking_Mode;
   pragma Export (C, Set_Blocking_Mode, "set_blocking_mode");

   procedure Reset_Blocking_Mode;
   pragma Export (C, Reset_Blocking_Mode, "reset_blocking_mode");

private
   -- Foreground colors
   for Colors use
     (Black        => 16#00#,
      Blue         => 16#01#,
      Green        => 16#02#,
      Cyan         => 16#03#,
      Red          => 16#04#,
      Magenta      => 16#05#,
      Brown        => 16#06#,
      LightGray    => 16#07#,
      DarkGray     => 16#08#,
      LightBlue    => 16#09#,
      LightGreen   => 16#0a#,
      LightCyan    => 16#0b#,
      LightRed     => 16#0c#,
      LightMagenta => 16#0d#,
      Yellow       => 16#0e#,
      White        => 16#0f#);
   for Colors'Size use 8;

   -- Background colors
   for BkColors use
     (Black     => 16#00#,
      Blue      => 16#01#,
      Green     => 16#02#,
      Cyan      => 16#03#,
      Red       => 16#04#,
      Magenta   => 16#05#,
      Brown     => 16#06#,
      LightGray => 16#07#);
   for BkColors'Size use 8;
end Console_Management;
