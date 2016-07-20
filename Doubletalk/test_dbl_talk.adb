with DoubleTalk_K;
with MaRTE_OS;
with Ada.Text_IO;
procedure Test_DBL_Talk is

begin



   Ada.Text_IO.Put ("Speaking");
   DoubleTalk_K.Speak
     (Phrase => DoubleTalk_K.Phrase_Strings.To_Bounded_String ("Hello I am Paul"),
      Voice  => DoubleTalk_K.Paul);
   --delay 1.5;
   Ada.Text_IO.Put ("Speaking");
   DoubleTalk_K.Speak
     (Phrase => DoubleTalk_K.Phrase_Strings.To_Bounded_String ("Hello I am darth vader"),
      Voice  => DoubleTalk_K.Vader);
   --delay 1.5;
   Ada.Text_IO.Put ("Speaking");
   DoubleTalk_K.Speak
     (Phrase => DoubleTalk_K.Phrase_Strings.To_Bounded_String ("Hello I am Bob"),
      Voice  => DoubleTalk_K.Bob);
   --delay 1.5;
   Ada.Text_IO.Put ("Speaking");
   DoubleTalk_K.Speak
     (Phrase => DoubleTalk_K.Phrase_Strings.To_Bounded_String ("Hello I am Pete"),
      Voice  => DoubleTalk_K.Pete);
   --delay 1.5;
   Ada.Text_IO.Put ("Speaking");
   DoubleTalk_K.Speak
     (Phrase => DoubleTalk_K.Phrase_Strings.To_Bounded_String ("Hello I am Randy"),
      Voice  => DoubleTalk_K.Randy);
   --delay 1.5;
   Ada.Text_IO.Put ("Speaking");
   DoubleTalk_K.Speak
     (Phrase => DoubleTalk_K.Phrase_Strings.To_Bounded_String ("Hello I am Biff"),
      Voice  => DoubleTalk_K.Biff);
   --delay 1.5;
   Ada.Text_IO.Put ("Speaking");
   DoubleTalk_K.Speak
     (Phrase => DoubleTalk_K.Phrase_Strings.To_Bounded_String ("Hello I am Skip"),
      Voice  => DoubleTalk_K.Skip);
   --delay 1.5;
   Ada.Text_IO.Put ("Speaking");
   DoubleTalk_K.Speak
     (Phrase => DoubleTalk_K.Phrase_Strings.To_Bounded_String ("Hello I am Robo"),
      Voice  => DoubleTalk_K.Robo);

   Ada.Text_IO.Put ("Done Speaking");
end Test_DBL_Talk;
