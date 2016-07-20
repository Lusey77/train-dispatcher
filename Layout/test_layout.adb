--------------------------------------------------------------------------------
------------------------------- Reading Railroad -------------------------------
------------------------------ The Ada Dream Team ------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Written By : Kaleb Luse -----------------------------------------------------
-- Tests      : Layout ---------------------------------------------------------
-- Updated    : 17 Feb 15 ------------------------------------------------------
--------------------------------------------------------------------------------
with Ada.Integer_Text_IO;
with Ada.Text_IO;
with Layout;
use type Layout.Terminator_Type;
use type Layout.Block_ID;
use type Layout.Turnout_ID;
use type Layout.Sensor_ID;
use type Layout.Block_Polarity;
use type Layout.Turn_Choice;
use type Layout.Turnout_Limb;

procedure Test_Layout is

   -- Packages for IO of the test objects
   package Terminator_IO is new Ada.Text_IO.Enumeration_IO
     (Enum => Layout.Terminator_Type);
   package Polarity_IO   is new Ada.Text_IO.Enumeration_IO
     (Enum => Layout.Block_Polarity);
   package Limb_IO       is new Ada.Text_IO.Enumeration_IO
     (Enum => Layout.Turnout_Limb);
   package Turn_IO       is new Ada.Text_IO.Enumeration_IO
     (Enum => Layout.Turn_Choice);
   package Boolean_IO    is new Ada.Text_IO.Enumeration_IO
     (Enum => Boolean);
   package Block_IO      is new Ada.Text_IO.Integer_IO
     (Num => Layout.Block_ID);
   package Turnout_IO    is new Ada.Text_IO.Integer_IO
     (Num => Layout.Turnout_ID);
   package Sensor_IO     is new Ada.Text_IO.Integer_IO
     (Num => Layout.Sensor_ID);

   -----------------------
   -- Testing Next_Term --
   -----------------------

   procedure Test_Next_Term is

      --------------------
      -- Test Variables --
      --------------------

      Test_File  : Ada.Text_IO.File_Type;
      Test_Num   : Integer;
      Block_Num  : Layout.Block_ID;
      Polarity   : Layout.Block_Polarity;
      Terminator : Layout.Terminator_Type;
      Test       : Boolean := True;

   begin
      -- Open File
      Ada.Text_IO.Open (File => Test_File,
                        Mode => Ada.Text_IO.In_File,
                        Name => "Next_Term.tsv");

      -- Output starting message
      Ada.Text_IO.Put_Line
        ("Starting Testing of the package Layout.End_Block:");
      -- This loop goes through and tests the proper funtion
      -- Each iteration, tests one test case and displays if failed
      loop
         exit when Ada.Text_IO.End_Of_File (Test_File);

         -- Get test information

         Ada.Integer_Text_IO.Get (File => Test_File,
                                  Item => Test_Num);

         Block_IO.Get (File => Test_File,
                       Item => Block_Num);

         Polarity_IO.Get (File => Test_File,
                          Item => Polarity);

         Terminator_IO.Get (File => Test_File,
                            Item => Terminator);

         if not (Layout.Next_Term (Block_Num, Polarity) = Terminator) then

            Ada.Text_IO.Put_Line
               ("Case " & Integer'Image (Test_Num) & ":");
            Ada.Text_IO.Put_Line ("Test failed");
            Test := False;

         end if;
      end loop;
      -- Close File
      Ada.Text_IO.Close (Test_File);

      -- Output results of the test
      if Test then
         Ada.Text_IO.Put_Line ("Full test successful");
      else
         Ada.Text_IO.Put_Line ("Full test failed");
      end if;
   end Test_Next_Term;
   ----------------------------------------------------------------------------
   procedure Test_End_Turnout is

      --------------------
      -- Test Variables --
      --------------------

      Test_File   : Ada.Text_IO.File_Type;
      Test_Num    : Integer;
      Turnout_Num : Layout.Turnout_ID;
      Direction   : Layout.Turnout_Limb;
      E_Output    : Layout.Block_ID;
      Test        : Boolean := True;

   begin
      -- Open File
      Ada.Text_IO.Open (File => Test_File,
                        Mode => Ada.Text_IO.In_File,
                        Name => "End_Turnout.tsv");

      -- Output starting message
      Ada.Text_IO.Put_Line
        ("Starting Testing of the package Layout.End_Turnout:");
      -- This loop goes through and tests the proper funtion
      -- Each iteration, tests one test case and displays if failed
      loop
         exit when Ada.Text_IO.End_Of_File (Test_File);

         -- Get test information

         Ada.Integer_Text_IO.Get (File => Test_File,
                                  Item => Test_Num);

         Turnout_IO.Get (File => Test_File,
                         Item => Turnout_Num);

         Limb_IO.Get (File => Test_File,
                      Item => Direction);

         Block_IO.Get (File => Test_File,
                       Item => E_Output);

         -- Output cases that failed
         if not (Layout.End_Turnout (Turnout_Num, Direction) = E_Output) then

            Ada.Text_IO.Put_Line
               ("Case " & Integer'Image (Test_Num) & ":");
            Ada.Text_IO.Put_Line ("Test failed");
            Test := False;

         end if;
      end loop;
      -- Close File
      Ada.Text_IO.Close (Test_File);

      -- Output results of the test
      if Test then
         Ada.Text_IO.Put_Line ("Full test successful");
      else
         Ada.Text_IO.Put_Line ("Full test failed");
      end if;
   end Test_End_Turnout;
   ----------------------------------------------------------------------------
   procedure Test_End_Block_1 is

      --------------------
      -- Test Variables --
      --------------------

      Test_File : Ada.Text_IO.File_Type;
      Test_Num  : Integer;
      Block     : Layout.Block_ID;
      Polarity  : Layout.Block_Polarity;
      E_Output  : Layout.Block_ID;
      Test      : Boolean := True;

   begin
      -- Open File
      Ada.Text_IO.Open (File => Test_File,
                        Mode => Ada.Text_IO.In_File,
                        Name => "End_Block_1.tsv");

      -- Output starting message
      Ada.Text_IO.Put_Line
        ("Starting Testing of the package Layout.End_Block :");
      -- This loop goes through and tests the proper funtion
      -- Each iteration, tests one test case and displays if failed
      loop
         exit when Ada.Text_IO.End_Of_File (Test_File);

         -- Get test information

         Ada.Integer_Text_IO.Get (File => Test_File,
                                  Item => Test_Num);

         Block_IO.Get (File => Test_File,
                       Item => Block);

         Polarity_IO.Get (File => Test_File,
                          Item => Polarity);

         Block_IO.Get (File => Test_File,
                       Item => E_Output);

         -- Output cases that failed
         if not (Layout.End_Block (Block, Polarity) = E_Output) then

            Ada.Text_IO.Put_Line
               ("Case " & Integer'Image (Test_Num) & ":");
            Ada.Text_IO.Put_Line ("Test failed");
            Test := False;

         end if;
      end loop;
      -- Close File
      Ada.Text_IO.Close (Test_File);

      -- Output results of the test
      if Test then
         Ada.Text_IO.Put_Line ("Full test successful");
      else
         Ada.Text_IO.Put_Line ("Full test failed");
      end if;
   end Test_End_Block_1;
   -----------------------------------------------------------------------------
   procedure Test_End_Block_2 is

      --------------------
      -- Test Variables --
      --------------------

      Test_File : Ada.Text_IO.File_Type;
      Test_Num  : Integer;
      Block     : Layout.Block_ID;
      Polarity  : Layout.Block_Polarity;
      E_Output  : Layout.Turnout_ID;
      Test      : Boolean := True;

   begin
      -- Open File
      Ada.Text_IO.Open (File => Test_File,
                       Mode  => Ada.Text_IO.In_File,
                       Name  => "End_Block_2.tsv");

      -- Output starting message
      Ada.Text_IO.Put_Line
        ("Starting Testing of the package Layout.End_Block :");
      -- This loop goes through and tests the proper funtion
      -- Each iteration, tests one test case and displays if failed
      loop
         exit when Ada.Text_IO.End_Of_File (Test_File);

         -- Get test information

         Ada.Integer_Text_IO.Get (File => Test_File,
                                  Item => Test_Num);

         Block_IO.Get (File => Test_File,
                       Item => Block);

         Polarity_IO.Get (File => Test_File,
                          Item => Polarity);

         Turnout_IO.Get (File => Test_File,
                         Item => E_Output);

         -- Output cases that failed
         if not (Layout.End_Block (Block, Polarity) = E_Output) then

            Ada.Text_IO.Put_Line
               ("Case " & Integer'Image (Test_Num) & ":");
            Ada.Text_IO.Put_Line ("Test failed");
            Test := False;

         end if;
      end loop;
      -- Close File
      Ada.Text_IO.Close (Test_File);

      -- Output results of the test
      if Test then
         Ada.Text_IO.Put_Line ("Full test successful");
      else
         Ada.Text_IO.Put_Line ("Full test failed");
      end if;
   end Test_End_Block_2;
   -----------------------------------------------------------------------------
   procedure Test_Sensor_Number is

      --------------------
      -- Test Variables --
      --------------------

      Test_File : Ada.Text_IO.File_Type;
      Test_Num  : Integer;
      Block_1   : Layout.Block_ID;
      Block_2   : Layout.Block_ID;
      E_Output  : Layout.Sensor_ID;
      Test      : Boolean := True;

   begin
      -- Open File
      Ada.Text_IO.Open (File => Test_File,
                       Mode => Ada.Text_IO.In_File,
                       Name => "Sensor_Number.tsv");

      -- Output starting message
      Ada.Text_IO.Put_Line
        ("Starting Testing of the package Layout.Sensor_Number :");
      -- This loop goes through and tests the proper funtion
      -- Each iteration, tests one test case and displays if failed
      loop
         exit when Ada.Text_IO.End_Of_File (Test_File);

         -- Get test information

         Ada.Integer_Text_IO.Get (File => Test_File,
                                  Item => Test_Num);

         Block_IO.Get (File => Test_File,
                       Item => Block_1);

         Block_IO.Get (File => Test_File,
                       Item => Block_2);

         Sensor_IO.Get (File => Test_File,
                        Item => E_Output);

         -- Output cases that failed
         if not (Layout.Sensor_Number (Block_1, Block_2) = E_Output) then

            Ada.Text_IO.Put_Line
               ("Case " & Integer'Image (Test_Num) & ":");
            Ada.Text_IO.Put_Line ("Test failed");
            Test := False;

         end if;
      end loop;
      -- Close File
      Ada.Text_IO.Close (Test_File);

      -- Output results of the test
      if Test then
         Ada.Text_IO.Put_Line ("Full test successful");
      else
         Ada.Text_IO.Put_Line ("Full test failed");
      end if;
   end Test_Sensor_Number;
   -----------------------------------------------------------------------------
   procedure Test_Is_Reverse is

      --------------------
      -- Test Variables --
      --------------------

      Test_File : Ada.Text_IO.File_Type;
      Test_Num  : Integer;
      Sensor    : Layout.Sensor_ID;
      E_Output  : Boolean;
      Test      : Boolean := True;

   begin
      -- Open File
      Ada.Text_IO.Open (File => Test_File,
                        Mode => Ada.Text_IO.In_File,
                        Name => "Is_Reverse.tsv");

      -- Output starting message
      Ada.Text_IO.Put_Line
        ("Starting Testing of the package Layout.Is_Reverse :");
      -- This loop goes through and tests the proper funtion
      -- Each iteration, tests one test case and displays if failed
      loop
         exit when Ada.Text_IO.End_Of_File (Test_File);

         -- Get test information

         Ada.Integer_Text_IO.Get (File => Test_File,
                                  Item => Test_Num);

         Sensor_IO.Get (File => Test_File,
                        Item => Sensor);

         Boolean_IO.Get (File => Test_File,
                         Item => E_Output);

         -- Output cases that failed
         if not (Layout.Is_Reverse (Sensor) = E_Output) then

            Ada.Text_IO.Put_Line
               ("Case " & Integer'Image (Test_Num) & ":");
            Ada.Text_IO.Put_Line ("Test failed");
            Test := False;

         end if;
      end loop;
      -- Close File
      Ada.Text_IO.Close (Test_File);

      -- Output results of the test
      if Test then
         Ada.Text_IO.Put_Line ("Full test successful");
      else
         Ada.Text_IO.Put_Line ("Full test failed");
      end if;
   end Test_Is_Reverse;
   -----------------------------------------------------------------------------
   procedure Test_Opposite_1 is

      --------------------
      -- Test Variables --
      --------------------

      Test_File : Ada.Text_IO.File_Type;
      Test_Num  : Integer;
      Polarity  : Layout.Block_Polarity;
      E_Output  : Layout.Block_Polarity;
      Test      : Boolean := True;

   begin
      -- Open File
      Ada.Text_IO.Open (File => Test_File,
                       Mode => Ada.Text_IO.In_File,
                       Name => "Opposite_1.tsv");

      -- Output starting message
      Ada.Text_IO.Put_Line
        ("Starting Testing of the package Layout.Opposite :");
      -- This loop goes through and tests the proper funtion
      -- Each iteration, tests one test case and displays if failed
      loop
         exit when Ada.Text_IO.End_Of_File (Test_File);

         -- Get test information

         Ada.Integer_Text_IO.Get (File => Test_File,
                                  Item => Test_Num);

         Polarity_IO.Get (File => Test_File,
                          Item => Polarity);

         Polarity_IO.Get (File => Test_File,
                          Item => E_Output);

         -- Output cases that failed
         if not (Layout.Opposite (Polarity) = E_Output) then

            Ada.Text_IO.Put_Line
               ("Case " & Integer'Image (Test_Num) & ":");
            Ada.Text_IO.Put_Line ("Test failed");
            Test := False;

         end if;
      end loop;
      -- Close File
      Ada.Text_IO.Close (Test_File);

      -- Output results of the test
      if Test then
         Ada.Text_IO.Put_Line ("Full test successful");
      else
         Ada.Text_IO.Put_Line ("Full test failed");
      end if;
   end Test_Opposite_1;
   -----------------------------------------------------------------------------
   procedure Test_Opposite_2 is

      --------------------
      -- Test Variables --
      --------------------

      Test_File : Ada.Text_IO.File_Type;
      Test_Num  : Integer;
      Direction : Layout.Turn_Choice;
      E_Output  : Layout.Turn_Choice;
      Test      : Boolean := True;

   begin
      -- Open File
      Ada.Text_IO.Open (File => Test_File,
                       Mode => Ada.Text_IO.In_File,
                       Name => "Opposite_2.tsv");

      -- Output starting message
      Ada.Text_IO.Put_Line
        ("Starting Testing of the package Layout.Opposite :");
      -- This loop goes through and tests the proper funtion
      -- Each iteration, tests one test case and displays if failed
      loop
         exit when Ada.Text_IO.End_Of_File (Test_File);

         -- Get test information

         Ada.Integer_Text_IO.Get (File => Test_File,
                                  Item => Test_Num);

         Turn_IO.Get (File => Test_File,
                      Item => Direction);

         Turn_IO.Get (File => Test_File,
                      Item => E_Output);

         -- Output cases that failed
         if not (Layout.Opposite (Direction) = E_Output) then

            Ada.Text_IO.Put_Line
               ("Case " & Integer'Image (Test_Num) & ":");
            Ada.Text_IO.Put_Line ("Test failed");
            Test := False;

         end if;
      end loop;
      -- Close File
      Ada.Text_IO.Close (Test_File);

      -- Output results of the test
      if Test then
         Ada.Text_IO.Put_Line ("Full test successful");
      else
         Ada.Text_IO.Put_Line ("Full test failed");
      end if;
   end Test_Opposite_2;
   -----------------------------------------------------------------------------
   procedure Test_Seperates is

      --------------------
      -- Test Variables --
      --------------------

      Test_File  : Ada.Text_IO.File_Type;
      Test_Num   : Integer;
      Sensor     : Layout.Sensor_ID;
      E_Output_1 : Layout.Block_ID;
      E_Output_2 : Layout.Block_ID;
      A_Output_1 : Layout.Block_ID;
      A_Output_2 : Layout.Block_ID;
      Test       : Boolean := True;

   begin
      -- Open File
      Ada.Text_IO.Open (File => Test_File,
                        Mode => Ada.Text_IO.In_File,
                        Name => "Seperates.tsv");

      -- Output starting message
      Ada.Text_IO.Put_Line
        ("Starting Testing of the package Layout.Seperates:");
      -- This loop goes through and tests the proper funtion
      -- Each iteration, tests one test case and displays if failed
      loop
         exit when Ada.Text_IO.End_Of_File (Test_File);

         -- Get test information

         Ada.Integer_Text_IO.Get (File => Test_File,
                                  Item => Test_Num);

         Sensor_IO.Get (File => Test_File,
                        Item => Sensor);

         Block_IO.Get (File => Test_File,
                       Item => E_Output_1);

         Block_IO.Get (File => Test_File,
                       Item => E_Output_2);

         Layout.Seperates (Sensor, A_Output_1, A_Output_2);

         -- Output cases that failed
         if not ((A_Output_1 = E_Output_1 and A_Output_2 = E_Output_2) or
               (A_Output_2 = E_Output_1 and A_Output_1 = E_Output_2)) then

            Ada.Text_IO.Put_Line
               ("Case " & Integer'Image (Test_Num) & ":");
            Ada.Text_IO.Put_Line ("Test failed");
            Test := False;

         end if;
      end loop;
      -- Close File
      Ada.Text_IO.Close (Test_File);

      -- Output results of the test
      if Test then
         Ada.Text_IO.Put_Line ("Full test successful");
      else
         Ada.Text_IO.Put_Line ("Full test failed");
      end if;
   end Test_Seperates;
   -----------------------------------------------------------------------------
   procedure Test_Is_FTurnout is

      --------------------
      -- Test Variables --
      --------------------

      Test_File : Ada.Text_IO.File_Type;
      Test_Num  : Integer;
      Block     : Layout.Block_ID;
      Polarity  : Layout.Block_Polarity;
      E_Output  : Boolean;
      Test      : Boolean := True;

   begin
      -- Open File
      Ada.Text_IO.Open (File => Test_File,
                       Mode => Ada.Text_IO.In_File,
                       Name => "Is_FTurnout.tsv");

      -- Output starting message
      Ada.Text_IO.Put_Line
        ("Starting Testing of the package Layout.Is_FTurnout :");
      -- This loop goes through and tests the proper funtion
      -- Each iteration, tests one test case and displays if failed
      loop
         exit when Ada.Text_IO.End_Of_File (Test_File);

         -- Get test information

         Ada.Integer_Text_IO.Get (File => Test_File,
                                  Item => Test_Num);

         Block_IO.Get (File => Test_File,
                       Item => Block);

         Polarity_IO.Get (File => Test_File,
                          Item => Polarity);

         Boolean_IO.Get (File => Test_File,
                         Item => E_Output);

         -- Output cases that failed
         if not (Layout.Is_Force_Turnout (Block, Polarity) = E_Output) then

            Ada.Text_IO.Put_Line
               ("Case " & Integer'Image (Test_Num) & ":");
            Ada.Text_IO.Put_Line ("Test failed");
            Test := False;

         end if;
      end loop;
      -- Close File
      Ada.Text_IO.Close (Test_File);

      -- Output results of the test
      if Test then
         Ada.Text_IO.Put_Line ("Full test successful");
      else
         Ada.Text_IO.Put_Line ("Full test failed");
      end if;
   end Test_Is_FTurnout;
   -----------------------------------------------------------------------------
   procedure Test_F_Turnout is

      --------------------
      -- Test Variables --
      --------------------

      Test_File     : Ada.Text_IO.File_Type;
      Test_Num      : Integer;
      Block         : Layout.Block_ID;
      Polarity      : Layout.Block_Polarity;
      A_Output      : Layout.Turnout_Rec;
      E_Output_ID   : Layout.Turnout_ID;
      E_Output_Turn : Layout.Turn_Choice;
      Test          : Boolean := True;

   begin
      -- Open File
      Ada.Text_IO.Open (File => Test_File,
                       Mode => Ada.Text_IO.In_File,
                       Name => "F_Turnout.tsv");

      -- Output starting message
      Ada.Text_IO.Put_Line
        ("Starting Testing of the package Layout.F_Turnout :");
      -- This loop goes through and tests the proper funtion
      -- Each iteration, tests one test case and displays if failed
      loop
         exit when Ada.Text_IO.End_Of_File (Test_File);

         -- Get test information

         Ada.Integer_Text_IO.Get (File => Test_File,
                                  Item => Test_Num);

         Block_IO.Get (File => Test_File,
                       Item => Block);

         Polarity_IO.Get (File => Test_File,
                          Item => Polarity);


         Turnout_IO.Get (File => Test_File,
                         Item => E_Output_ID);

         Turn_IO.Get (File => Test_File,
                      Item => E_Output_Turn);

         A_Output := Layout.Force_Turnout (Block, Polarity);

         -- Output cases that failed
         if not (A_Output.Turnout = E_Output_ID and
            A_Output.Direction = E_Output_Turn) then

            Ada.Text_IO.Put_Line
               ("Case " & Integer'Image (Test_Num) & ":");
            Ada.Text_IO.Put_Line ("Test failed");
            Test := False;

         end if;
      end loop;
      -- Close File
      Ada.Text_IO.Close (Test_File);

      -- Output results of the test
      if Test then
         Ada.Text_IO.Put_Line ("Full test successful");
      else
         Ada.Text_IO.Put_Line ("Full test failed");
      end if;
   end Test_F_Turnout;
   -----------------------------------------------------------------------------
   procedure Test_Is_JTurnout is

      --------------------
      -- Test Variables --
      --------------------

      Test_File : Ada.Text_IO.File_Type;
      Test_Num  : Integer;
      Turnout   : Layout.Turnout_ID;
      Direction : Layout.Turn_Choice;
      E_Output  : Boolean;
      Test      : Boolean := True;

   begin
      -- Open File
      Ada.Text_IO.Open (File => Test_File,
                       Mode => Ada.Text_IO.In_File,
                       Name => "Is_JTurnout.tsv");

      -- Output starting message
      Ada.Text_IO.Put_Line
        ("Starting Testing of the package Layout.Is_JTurnout :");
      -- This loop goes through and tests the proper funtion
      -- Each iteration, tests one test case and displays if failed
      loop
         exit when Ada.Text_IO.End_Of_File (Test_File);

         -- Get test information

         Ada.Integer_Text_IO.Get (File => Test_File,
                                  Item => Test_Num);

         Turnout_IO.Get (File => Test_File,
                        Item => Turnout);

         Turn_IO.Get (File => Test_File,
                      Item => Direction);

         Boolean_IO.Get (File => Test_File,
                         Item => E_Output);

         -- Output cases that failed
         if not (Layout.Is_Joint_Turnout (Turnout, Direction) = E_Output) then

            Ada.Text_IO.Put_Line
               ("Case " & Integer'Image (Test_Num) & ":");
            Ada.Text_IO.Put_Line ("Test failed");
            Test := False;

         end if;
      end loop;
      -- Close File
      Ada.Text_IO.Close (Test_File);

      -- Output results of the test
      if Test then
         Ada.Text_IO.Put_Line ("Full test successful");
      else
         Ada.Text_IO.Put_Line ("Full test failed");
      end if;
   end Test_Is_JTurnout;
   -----------------------------------------------------------------------------
   procedure Test_J_Turnout is

      --------------------
      -- Test Variables --
      --------------------

      Test_File : Ada.Text_IO.File_Type;
      Test_Num  : Integer;
      Turnout   : Layout.Turnout_ID;
      E_Output  : Layout.Turnout_ID;
      Test      : Boolean := True;

   begin
      -- Open File
      Ada.Text_IO.Open (File => Test_File,
                       Mode => Ada.Text_IO.In_File,
                       Name => "J_Turnout.tsv");

      -- Output starting message
      Ada.Text_IO.Put_Line
        ("Starting Testing of the package Layout.JTurnout :");
      -- This loop goes through and tests the proper funtion
      -- Each iteration, tests one test case and displays if failed
      loop
         exit when Ada.Text_IO.End_Of_File (Test_File);

         -- Get test information

         Ada.Integer_Text_IO.Get (File => Test_File,
                                  Item => Test_Num);

         Turnout_IO.Get (File => Test_File,
                         Item => Turnout);

         Turnout_IO.Get (File => Test_File,
                         Item => E_Output);

         -- Output cases that failed
         if not (Layout.Joint_Turnout (Turnout) = E_Output) then

            Ada.Text_IO.Put_Line
               ("Case " & Integer'Image (Test_Num) & ":");
            Ada.Text_IO.Put_Line ("Test failed");
            Test := False;

         end if;
      end loop;
      -- Close File
      Ada.Text_IO.Close (Test_File);

      -- Output results of the test
      if Test then
         Ada.Text_IO.Put_Line ("Full test successful");
      else
         Ada.Text_IO.Put_Line ("Full test failed");
      end if;
   end Test_J_Turnout;
   -----------------------------------------------------------------------------
   procedure Test_Next_CTurnout is

      --------------------
      -- Test Variables --
      --------------------

      Test_File : Ada.Text_IO.File_Type;
      Test_Num  : Integer;
      Block     : Layout.Block_ID;
      Polarity  : Layout.Block_Polarity;
      E_Output  : Layout.Turnout_ID;
      Test      : Boolean := True;

   begin
      -- Open File
      Ada.Text_IO.Open (File => Test_File,
                       Mode => Ada.Text_IO.In_File,
                       Name => "Next_CTurnout.tsv");

      -- Output starting message
      Ada.Text_IO.Put_Line
        ("Starting Testing of the package Layout.Next_CTurnout :");
      -- This loop goes through and tests the proper funtion
      -- Each iteration, tests one test case and displays if failed
      loop
         exit when Ada.Text_IO.End_Of_File (Test_File);

         -- Get test information

         Ada.Integer_Text_IO.Get (File => Test_File,
                                  Item => Test_Num);

         Block_IO.Get (File => Test_File,
                       Item => Block);

         Polarity_IO.Get (File => Test_File,
                          Item => Polarity);

         Turnout_IO.Get (File => Test_File,
                         Item => E_Output);

         -- Output cases that failed
         if not (Layout.Next_Choice_Turnout (Block, Polarity) = E_Output) then

            Ada.Text_IO.Put_Line
               ("Case " & Integer'Image (Test_Num) & ":");
            Ada.Text_IO.Put_Line ("Test failed");
            Test := False;

         end if;
      end loop;
      -- Close File
      Ada.Text_IO.Close (Test_File);

      -- Output results of the test
      if Test then
         Ada.Text_IO.Put_Line ("Full test successful");
      else
         Ada.Text_IO.Put_Line ("Full test failed");
      end if;
   end Test_Next_CTurnout;
   -----------------------------------------------------------------------------
begin  -- Test_Layout

   Ada.Text_IO.Put_Line ("Please press enter to continue test.");
   Ada.Text_IO.Skip_Line;
   Test_Next_Term;
   Ada.Text_IO.Put_Line ("Please press enter to continue test.");
   Ada.Text_IO.Skip_Line;
   Test_End_Turnout;
   Ada.Text_IO.Put_Line ("Please press enter to continue test.");
   Ada.Text_IO.Skip_Line;
   Test_End_Block_1;
   Ada.Text_IO.Put_Line ("Please press enter to continue test.");
   Ada.Text_IO.Skip_Line;
   Test_End_Block_2;
   Ada.Text_IO.Put_Line ("Please press enter to continue test.");
   Ada.Text_IO.Skip_Line;
   Test_Sensor_Number;
   Ada.Text_IO.Put_Line ("Please press enter to continue test.");
   Ada.Text_IO.Skip_Line;
   Test_Is_Reverse;
   Ada.Text_IO.Put_Line ("Please press enter to continue test.");
   Ada.Text_IO.Skip_Line;
   Test_Opposite_1;
   Ada.Text_IO.Put_Line ("Please press enter to continue test.");
   Ada.Text_IO.Skip_Line;
   Test_Opposite_2;
   Ada.Text_IO.Put_Line ("Please press enter to continue test.");
   Ada.Text_IO.Skip_Line;
   Test_Seperates;
   Ada.Text_IO.Put_Line ("Please press enter to continue test.");
   Ada.Text_IO.Skip_Line;
   Test_Is_FTurnout;
   Ada.Text_IO.Put_Line ("Please press enter to continue test.");
   Ada.Text_IO.Skip_Line;
   Test_F_Turnout;
   Ada.Text_IO.Put_Line ("Please press enter to continue test.");
   Ada.Text_IO.Skip_Line;
   Test_Is_JTurnout;
   Ada.Text_IO.Put_Line ("Please press enter to continue test.");
   Ada.Text_IO.Skip_Line;
   Test_J_Turnout;
   Ada.Text_IO.Put_Line ("Please press enter to continue test.");
   Ada.Text_IO.Skip_Line;
   Test_Next_CTurnout;

end Test_Layout;
