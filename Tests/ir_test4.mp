program Program1;
  var Apples: Integer;
  begin
     Read(Apples);
     if Apples > 0
       then begin
         Apples := Apples * 5;
         Write(Apples)
       end
       else begin
         Apples := Apples * 7;
         Write(Apples)
       end
   end.