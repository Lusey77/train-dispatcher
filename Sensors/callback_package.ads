with Layout;
with Trains;
package Callback_Package is

   procedure Moniter_Halls (My_Hall : in Layout.Hall_ID);

   procedure Put_Failure (Requestor : in Trains.Request_ID;
                          Turnout   : in Layout.Turnout_ID);
   procedure Put_Recovery (Turnout  : in Layout.Turnout_ID);

   procedure Put_Change (Turnout   : in Layout.Turnout_ID;
                         Direction : in Layout.Turn_Choice;
                         Moving    : in Boolean);

end Callback_Package;
