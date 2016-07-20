--------------------------------------------------------------------------------
------------------------------- Reading Railroad -------------------------------
------------------------------ The Ada Dream Team ------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Written By : Kaleb Luse  ----------------------------------------------------
-- Tests      : Engineers ------------------------------------------------------
-- Updated    : 22 Mar 15 ------------------------------------------------------
--------------------------------------------------------------------------------
with Ada.Text_IO;
with Engineers;
with MaRTE_OS;
procedure Test_Engineers is

   package Engineer_IO is new
     Ada.Text_IO.Integer_IO (Num => Engineers.Engineer_ID);

   Char     : Character;
   Engineer : Engineers.Engineer_ID;

begin

   loop
      Ada.Text_IO.Put_Line ("What would you like to do");
      Ada.Text_IO.Put_Line ("(E)nable the engineer");
      Ada.Text_IO.Put_Line ("(D)isable the engineer");
      Ada.Text_IO.Put_Line ("(C)hange the skill");
      Ada.Text_IO.Put_Line ("(G)et the skill");
      Ada.Text_IO.Get (Char);
      Ada.Text_IO.Put_Line ("What engineer would you like to perform this " &
                              "operation on?");
      Engineer_IO.Get (Engineer);
      case Char is
         when 'E' =>
            Engineers.Enable (Engineer => Engineer);
         when 'D' =>
            Engineers.Disable (Engineer => Engineer);
         when 'C' =>
            Engineers.Change_Skill (Engineer => Engineer);
         when 'G' =>
            Ada.Text_IO.Put_Line ("Skill: " &
            Engineers.Skill'Image (Engineers.Get_Skill (Engineer => Engineer)));
         when others =>
            null;
      end case;
   end loop;

end Test_Engineers;
