// Calling GetConsoleScreenBufferInfo that much just to
// make sure we will not miss anything from the console
// TODO: Handle GetConsoleScreenBufferInfo fails

procedure SetForegroundColor(const Color: TConsoleColor);
begin
  GetConsoleScreenBufferInfo(ConsoleHandle, ConsoleInfo);
  SetConsoleTextAttribute(ConsoleHandle, (Info.wAttributes and $F0) or Ord(Color));
end;

procedure SetBackgroundColor(const Color: TConsoleColor);
begin
  GetConsoleScreenBufferInfo(ConsoleHandle, ConsoleInfo);
  SetConsoleTextAttribute(ConsoleHandle, (Info.wAttributes and $0F) or (Ord(Color) shl 4));
end;

procedure ResetColors;
begin
  GetConsoleScreenBufferInfo(ConsoleHandle, ConsoleInfo);
  SetConsoleTextAttribute(ConsoleHandle, FDefaultAttr);
end;
