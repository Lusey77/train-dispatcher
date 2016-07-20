--------------------------------------------------------------------------------
------------------------------- Reading Railroad -------------------------------
------------------------------ The Ada Dream Team ------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Written By : Kaleb Luse -----------------------------------------------------
-- Tested By  : Travis Sullivan ------------------------------------------------
-- Updated    : 10 Mar 15 ------------------------------------------------------
--------------------------------------------------------------------------------
with Layout;
with Trains;
with Engineers;
package Commands is

   -----------------------------------------------------------------------------
   -- Represents all the pssible commands that could be entered               --
   -----------------------------------------------------------------------------
   type Command_Type is (Stop_All, Stop, Go, Left, Right,
                         Free, Skill, Restart, Quit, Error);

   -----------------------------------------------------------------------------
   -- Represents a record of a command and its properties                     --
   -----------------------------------------------------------------------------
   type Command_Rec (Which : Command_Type := Stop_All) is
      record
         case Which is
            when Stop_All | Restart | Quit | Error =>
               null;
            when Stop | Go =>
               Train : Trains.Train_ID;
            when Left | Right =>
               Turnout  : Layout.Turnout_ID;
            when Free =>
               Block    : Layout.Block_ID;
            when Skill =>
               Engineer : Engineers.Engineer_ID;
         end case;
      end record;

   -- Purpose        :Returns a dispatcher command entered at the keyboard
   --
   -- Preconditions  : None
   --
   -- Postconditions : Returns one of the nine possible keyboard commands
   --                  along with the associated data
   --                      or
   --                  returns Error when the command is invalid.
   procedure Get (Command : out Command_Rec);

end Commands;
