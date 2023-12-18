with Ada.Characters.Handling;  use Ada.Characters.Handling;
with Ada.Strings;              use Ada.Strings;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;

procedure Main is
  type Sliding_Window_Cursor is record
    Container    : Unbounded_String;
    Value        : Unbounded_String;
    High         : Positive;
    Low          : Positive;
    Maximum_Size : Positive;
    Finished     : Boolean;
  end record;

  --  type Written_Number is
  --   (One, Two, Three, Four, Five, Six, Seven, Eight, Nine);

  --  for Written_Number use
  --   (One => 1, Two => 2, Three => 3, Four => 4, Five => 5, Six => 6, Seven => 7,
  --    Eight => 8, Nine => 9);

  -- --------------------------
  -- Get_Line_Calibration_Value
  -- --------------------------

  function Get_Line_Calibration_Value (Line : Unbounded_String) return Natural
  is
    Calibration_Value : Natural := 0;
  begin
    for Index in 1 .. Length (Line) loop
      if Is_Decimal_Digit (Element (Line, Index)) then
        Calibration_Value :=
         Calibration_Value + (Character'Pos (Element (Line, Index)) - 48) * 10;
        exit;
      end if;
    end loop;

    for Index in reverse 1 .. Length (Line) loop
      if Is_Decimal_Digit (Element (Line, Index)) then
        Calibration_Value :=
         Calibration_Value + (Character'Pos (Element (Line, Index)) - 48);
        exit;
      end if;
    end loop;

    return Calibration_Value;
  end Get_Line_Calibration_Value;

  -- -----------------------------------------------
  -- Get_Sliding_Window_Calibration_Value
  -- -----------------------------------------------

  function Get_Sliding_Window_Calibration_Value
   (Window_Cursor : Sliding_Window_Cursor) return Integer
  is
  begin
    if Is_Decimal_Digit (Element (Window_Cursor.Value, 1)) then
      return Character'Pos (Element (Window_Cursor.Value, 1)) - 48;
    end if;

    --  for Index in 1 .. Length (Window_Cursor.Value) loop
    --    declare
    --      A : Unbounded_String :=
    --       Head (Window_Cursor.Value, Length (Written_Number'Val (Index)));
    --    begin
    --      if A = Written_Number'Val (Index) then
    --        return Index;
    --      end if;
    --    end;
    --  end loop;

    if Head (Window_Cursor.Value, 3) = "one" then
      return 1;
    end if;

    if Head (Window_Cursor.Value, 3) = "two" then
      return 2;
    end if;

    if Head (Window_Cursor.Value, 5) = "three" then
      return 3;
    end if;

    if Head (Window_Cursor.Value, 4) = "four" then
      return 4;
    end if;

    if Head (Window_Cursor.Value, 4) = "five" then
      return 5;
    end if;

    if Head (Window_Cursor.Value, 3) = "six" then
      return 6;
    end if;

    if Head (Window_Cursor.Value, 5) = "seven" then
      return 7;
    end if;

    if Head (Window_Cursor.Value, 5) = "eight" then
      return 8;
    end if;

    if Head (Window_Cursor.Value, 4) = "nine" then
      return 9;
    end if;

    return -1;
  end Get_Sliding_Window_Calibration_Value;

  -- -----------------------------------------------
  -- Next_Sliding_Window
  -- -----------------------------------------------

  function Next_Sliding_Window
   (Window_Cursor : Sliding_Window_Cursor) return Sliding_Window_Cursor
  is
    New_Window_Cursor : Sliding_Window_Cursor;
  begin
    New_Window_Cursor := Window_Cursor;

    if Window_Cursor.High = Length (Window_Cursor.Container) and
     Window_Cursor.Low = Length (Window_Cursor.Container)
    then
      New_Window_Cursor.Finished := True;
      return New_Window_Cursor;
    end if;

    New_Window_Cursor.High :=
     Natural'Min (Window_Cursor.High + 1, Length (Window_Cursor.Container));

    if New_Window_Cursor.High > Window_Cursor.Maximum_Size or
     New_Window_Cursor.High = Length (Window_Cursor.Container)
    then
      New_Window_Cursor.Low := Natural'Max (Window_Cursor.Low + 1, 1);
    end if;

    New_Window_Cursor.Value :=
     Tail
      (Head (Window_Cursor.Container, New_Window_Cursor.High),
       New_Window_Cursor.High - New_Window_Cursor.Low + 1);

    return New_Window_Cursor;
  end Next_Sliding_Window;

  -- -----------------------------------------------
  -- Previous_Sliding_Window
  -- -----------------------------------------------

  function Previous_Sliding_Window
   (Window_Cursor : Sliding_Window_Cursor) return Sliding_Window_Cursor
  is
    New_Window_Cursor : Sliding_Window_Cursor;
  begin
    New_Window_Cursor := Window_Cursor;

    if New_Window_Cursor.Low = 1 and New_Window_Cursor.High = 1 then
      New_Window_Cursor.Finished := True;
      return New_Window_Cursor;
    end if;

    New_Window_Cursor.Low := Natural'Max (Window_Cursor.Low - 1, 1);

    if New_Window_Cursor.Low <
     Length (Window_Cursor.Container) - Window_Cursor.Maximum_Size or
     New_Window_Cursor.Low = 1
    then
      New_Window_Cursor.High :=
       Natural'Min (Window_Cursor.High - 1, Length (Window_Cursor.Container));
    end if;

    New_Window_Cursor.Value :=
     Head
      (Tail
        (Window_Cursor.Container,
         Length (Window_Cursor.Container) - New_Window_Cursor.Low + 1),
       New_Window_Cursor.High - New_Window_Cursor.Low + 1);

    return New_Window_Cursor;
  end Previous_Sliding_Window;

  -- -----------------------------------------------
  -- Get_First_Sliding_Window
  -- -----------------------------------------------

  function Get_First_Sliding_Window
   (Line : Unbounded_String) return Sliding_Window_Cursor
  is
  begin
    return
     (Container => Line, High => 1, Low => 1, Value => Head (Line, 1),
      Finished  => False, Maximum_Size => 5);
  end Get_First_Sliding_Window;

  -- -----------------------------------------------
  -- Get_Last_Sliding_Window
  -- -----------------------------------------------

  function Get_Last_Sliding_Window
   (Line : Unbounded_String) return Sliding_Window_Cursor
  is
    Line_Length : Positive := Length (Line);
  begin
    return
     (Container => Line, High => Line_Length, Low => Line_Length,
      Value     => Tail (Line, 1), Finished => False, Maximum_Size => 5);
  end Get_Last_Sliding_Window;

  -- -----------------------------------------------
  -- Is_Last_Sliding_Window
  -- -----------------------------------------------

  function Is_Last_Sliding_Window
   (Window_Cursor : Sliding_Window_Cursor) return Boolean
  is
  begin
    return
     Window_Cursor.High = Length (Window_Cursor.Container) and
     Window_Cursor.Low = Window_Cursor.High;
  end Is_Last_Sliding_Window;

  -- -----------------------------------------------
  -- Get_Line_Calibration_Value_With_Written_Numbers
  -- -----------------------------------------------

  function Get_Line_Calibration_Value_With_Written_Numbers
   (Line : Unbounded_String) return Natural
  is
    Window                      : Unbounded_String;
    Window_Cursor               : Sliding_Window_Cursor;
    Calibration_Value           : Natural := 0;
    Calibration_Value_Candidate : Integer := -1;
  begin
    Window_Cursor := Get_First_Sliding_Window (Line);

    while not Window_Cursor.Finished loop
      Calibration_Value_Candidate :=
       Get_Sliding_Window_Calibration_Value (Window_Cursor);

      if Calibration_Value_Candidate > -1 then
        Calibration_Value :=
         Calibration_Value + Calibration_Value_Candidate * 10;
        exit;
      end if;

      Window_Cursor := Next_Sliding_Window (Window_Cursor);
    end loop;

    Window_Cursor := Get_Last_Sliding_Window (Line);

    while not Window_Cursor.Finished loop
      Calibration_Value_Candidate :=
       Get_Sliding_Window_Calibration_Value (Window_Cursor);

      if Calibration_Value_Candidate > -1 then
        Calibration_Value := Calibration_Value + Calibration_Value_Candidate;
        exit;
      end if;

      Window_Cursor := Previous_Sliding_Window (Window_Cursor);
    end loop;

    return Calibration_Value;
  end Get_Line_Calibration_Value_With_Written_Numbers;

  -- -----------------------------------------------
  -- Execute
  -- -----------------------------------------------

  procedure Execute is
    Input_File                                  : File_Type;
    Input_File_Name : constant String := "input.txt";
    Current_Line                                : Unbounded_String;
    Calibration_Values_Sum                      : Natural         := 0;
    Calibration_Values_Sum_With_Written_Numbers : Natural         := 0;
  begin
    Open (Input_File, In_File, Input_File_Name);

    while not End_Of_File (Input_File) loop
      Current_Line := Get_Line (Input_File);

      Calibration_Values_Sum :=
       Calibration_Values_Sum + Get_Line_Calibration_Value (Current_Line);

      Calibration_Values_Sum_With_Written_Numbers :=
       Calibration_Values_Sum_With_Written_Numbers +
       Get_Line_Calibration_Value_With_Written_Numbers (Current_Line);
    end loop;

    Close (Input_File);

    Put_Line
     ("Sum of all calibration values" &
      Natural'Image (Calibration_Values_Sum));

    Put_Line
     ("Sum of all calibration values considering written numbers" &
      Natural'Image (Calibration_Values_Sum_With_Written_Numbers));
  end Execute;

begin
  Execute;
end Main;
