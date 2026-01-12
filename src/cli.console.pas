unit CLI.Console;

{$H+}{$J-}

{ This unit provides console output functionality with color support.
  It handles both Windows and ANSI terminal color codes, cursor movement,
  and text formatting. }

interface

type
  { Console colors enumeration
    Supports both standard and bright colors
    Note: Not all terminals support bright colors }
  TConsoleColor = (
    ccBlack, ccBlue, ccGreen, ccCyan, 
    ccRed, ccMagenta, ccYellow, ccWhite,
    ccBrightBlack, ccBrightBlue, ccBrightGreen, ccBrightCyan,
    ccBrightRed, ccBrightMagenta, ccBrightYellow, ccBrightWhite
  );

{ Sets text foreground color
  @param Color The color to set for subsequent text output }
procedure SetForegroundColor(const Color: TConsoleColor);
    
{ Sets text background color
  @param Color The color to set for text background }
procedure SetBackgroundColor(const Color: TConsoleColor);
    
{ Resets colors to console defaults
  Note: Uses stored default attributes on Windows }
procedure ResetColors;
    
{ Clears the current line
  Note: Moves cursor to start of line }
procedure ClearLine;
    
{ Moves cursor up specified number of lines
  @param Lines Number of lines to move up (default 1) }
procedure MoveCursorUp(const Lines: Integer = 1);
    
{ Moves cursor down specified number of lines
  @param Lines Number of lines to move down (default 1) }
procedure MoveCursorDown(const Lines: Integer = 1);
    
{ Moves cursor left specified number of columns
  @param Columns Number of columns to move left (default 1) }
procedure MoveCursorLeft(const Columns: Integer = 1);
    
{ Moves cursor right specified number of columns
  @param Columns Number of columns to move right (default 1) }
procedure MoveCursorRight(const Columns: Integer = 1);

{ Moves the cursor to the specified location }
procedure MoveCursorTo(const X, Y: integer);

{ Saves current cursor position
  Note: Can be restored with RestoreCursorPosition }
procedure SaveCursorPosition;
    
{ Restores previously saved cursor position
  Note: Must be preceded by SaveCursorPosition }
procedure RestoreCursorPosition;
    
{ Writes colored text without line ending
  @param Text The text to write
  @param FgColor The color to use for the text }
procedure WriteColored(const Text: string; const FgColor: TConsoleColor);
    
{ Writes colored text with line ending
  @param Text The text to write
  @param FgColor The color to use for the text }
procedure WriteColoredLn(const Text: string; const FgColor: TConsoleColor);

implementation

// Windows 10 1607+ support ANSI escape sequences.
// But for the best compabitility, stick to the traditional
// Windows Console API. (one more reason: some functions used
// to get Windows version are deprecated, returning 6.2 (aka
// Windows 8.x) without doing stuff like creating a .manifest.

// The best bet is to use a macro. And here, introducing:
// WIN_CAN_USE_ESC_SEQ

{$IF DEFINED(WINDOWS) AND NOT DEFINED(WIN_CAN_USE_ESC_SEQ)}

var
  ConsoleHandle: THandle;
  ConsoleInfo: TConsoleScreenBufferInfo;
  FDefaultAttr: Word;
{$I inc/console.win32.inc}

{$ELSE}

{$I inc/console.unix.inc}

{$ENDIF}

procedure ClearLine;
begin
  System.Write(#27'[2K');
  System.Write(#13);
end;

procedure MoveCursorUp(const Lines: Integer);
begin
  System.Write(#27'[', Lines, 'A');
end;

procedure MoveCursorDown(const Lines: Integer);
begin
  System.Write(#27'[', Lines, 'B');
end;

procedure MoveCursorLeft(const Columns: Integer);
begin
  System.Write(#27'[', Columns, 'D');
end;

procedure MoveCursorRight(const Columns: Integer);
begin
  System.Write(#27'[', Columns, 'C');
end;

procedure MoveCursorTo(const X, Y: integer);
begin
{$WARNING This is not confirmed to work. The cursor will jump to 0,0 first.}
	System.Write(#13);
  MoveCursorDown(Y);
  MoveCursorRight(X);
end;

procedure SaveCursorPosition;
begin
  System.Write(#27'7');
end;

procedure RestoreCursorPosition;
begin
  System.Write(#27'8');
end;

procedure WriteColored(const Text: string; const FgColor: TConsoleColor);
begin
  SetForegroundColor(FgColor);
  System.Write(Text);
  ResetColors;
end;

procedure WriteColoredLn(const Text: string; const FgColor: TConsoleColor);
begin
  SetForegroundColor(FgColor);
  System.WriteLn(Text);
  ResetColors;
end;

{$IF DEFINED(WINDOWS) AND NOT DEFINED(WIN_CAN_USE_ESC_SEQ)}
initialization
  ConsoleHandle := GetStdHandle(STD_OUTPUT_HANDLE);
  if GetConsoleScreenBufferInfo(ConsoleHandle, ConsoleInfo) then
    FDefaultAttr := Info.wAttributes
  else
    FDefaultAttr := $07;  // Default to light gray on black
{$ENDIF}

finalization
  ResetColors;
end.
