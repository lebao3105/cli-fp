// While this (escape sequences) is simple enough,
// there is a FCL package named fcl-fpterm.
// Put inside FPC source (version? Unchecked.), it
// provides terminal management functions:
// * Can get/set cursor position
// * Get/set terminal size
// * Blink / deblink cursor
// * Loggers
// * Keyboard
// See https://www.freepascal.org/daily/packages/fcl-fpterm/index.html.
// *undocumented*

{$IFDEF UNIX}
// Do we really have to do this?
// uses BaseUnix, termio;
{$ENDIF}

procedure SetForegroundColor(const Color: TConsoleColor);
begin
    case Color of
        ccBlack: System.Write(#27'[30m');
        ccBlue: System.Write(#27'[34m');
        ccGreen: System.Write(#27'[32m');
        ccCyan: System.Write(#27'[36m');
        ccRed: System.Write(#27'[31m');
        ccMagenta: System.Write(#27'[35m');
        ccYellow: System.Write(#27'[33m');
        ccWhite: System.Write(#27'[37m');
        ccBrightBlack: System.Write(#27'[90m');
        ccBrightBlue: System.Write(#27'[94m');
        ccBrightGreen: System.Write(#27'[92m');
        ccBrightCyan: System.Write(#27'[96m');
        ccBrightRed: System.Write(#27'[91m');
        ccBrightMagenta: System.Write(#27'[95m');
        ccBrightYellow: System.Write(#27'[93m');
        ccBrightWhite: System.Write(#27'[97m');
    end;
end;

procedure SetBackgroundColor(const Color: TConsoleColor);
begin
    case Color of
        ccBlack: System.Write(#27'[40m');
        ccBlue: System.Write(#27'[44m');
        ccGreen: System.Write(#27'[42m');
        ccCyan: System.Write(#27'[46m');
        ccRed: System.Write(#27'[41m');
        ccMagenta: System.Write(#27'[45m');
        ccYellow: System.Write(#27'[43m');
        ccWhite: System.Write(#27'[47m');
        ccBrightBlack: System.Write(#27'[100m');
        ccBrightBlue: System.Write(#27'[104m');
        ccBrightGreen: System.Write(#27'[102m');
        ccBrightCyan: System.Write(#27'[106m');
        ccBrightRed: System.Write(#27'[101m');
        ccBrightMagenta: System.Write(#27'[105m');
        ccBrightYellow: System.Write(#27'[103m');
        ccBrightWhite: System.Write(#27'[107m');
    end;
end;

procedure ResetColors; inline;
begin
    System.Write(#27'[0m');
end;

procedure ClearScreen; inline;
begin
    System.Write(#27'[2J');
    System.Write(#13);
end;
