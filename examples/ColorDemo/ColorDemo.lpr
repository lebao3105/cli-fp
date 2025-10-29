{
  Professional Color Demo

  This example demonstrates how to create a professional-looking CLI application
  using the CLI framework's color capabilities. It showcases:

  1. Professional color combinations
  2. Structured output formatting
  3. Basic command parameters
  4. Progress indication
  5. Clean error handling

  Usage examples:
  $ ColorDemo.exe greet              # Default greeting
  $ ColorDemo.exe greet --name John  # Personalized greeting
}
program ColorDemo;

{$mode objfpc}{$H+}{$J-}  // Enable Object Pascal mode, long strings, disable writeable constants

uses
  SysUtils,         // For exception handling and string functions
  Classes,          // For TStringList and other basic classes
  CLI.Interfaces,   // Core interfaces for CLI framework
  CLI.Application,  // Application creation and management
  CLI.Command,      // Base command functionality
  CLI.Parameter,    // Parameter handling
  CLI.Progress,     // Progress indicators like spinners
  CLI.Console;      // Colored console output

type
  { TGreetCommand - Professional greeting with colored output
    This command demonstrates how to create a visually appealing CLI
    with headers, footers, progress indicators and error handling }
  TGreetCommand = class(TBaseCommand)
  private
    { Shows a decorative header with the application name }
    procedure ShowHeader;
    
    { Displays a personalized greeting with animation
      @param Name The name to include in the greeting }
    procedure ShowGreeting(const AName: string);
    
    { Shows helpful information at the bottom }
    procedure ShowFooter;
  public
    { Main execution method that orchestrates the greeting display
      @return 0 for success, 1 for errors }
    function Execute: Integer; override;
  end;

procedure TGreetCommand.ShowHeader;
begin
  // Add spacing before header
  WriteColoredLn('', ccWhite);  // Empty line for spacing
  
  // Draw a decorative box using Unicode characters
  WriteColoredLn('╔══════════════════════════════════════╗', ccCyan);
  WriteColoredLn('║        Welcome to ColorDemo          ║', ccCyan);
  WriteColoredLn('╚══════════════════════════════════════╝', ccCyan);
  WriteColoredLn('', ccWhite);  // Empty line for spacing
end;

procedure TGreetCommand.ShowGreeting(const AName: string);
var
  Spinner: IProgressIndicator;
  i: Integer;
begin
  // Show a progress message with an arrow indicator
  WriteColored('► ', ccGreen);
  WriteColoredLn('Preparing greeting...', ccWhite);
  
  // Create and run an animated spinner to show activity
  Spinner := CreateSpinner(ssDots);
  Spinner.Start;
  try
    // Simulate some work being done
    for i := 1 to 5 do
    begin
      Spinner.Update(0);
      Sleep(100);  // Short delay for visual effect
    end;
  finally
    Spinner.Stop;  // Always stop the spinner
  end;

  // Display the greeting with mixed colors for visual appeal
  WriteColoredLn('', ccWhite);  // Spacing
  WriteColored('  ❯ ', ccGreen);  // Modern arrow indicator
  WriteColored('Hello, ', ccWhite);
  WriteColored(AName, ccYellow);  // Highlight the AName
  WriteColoredLn('!', ccWhite);
  WriteColoredLn('  Welcome to our professional CLI application.', ccWhite);
  WriteColoredLn('', ccWhite);  // Spacing
end;

procedure TGreetCommand.ShowFooter;
begin
  // Show help hint with info symbol
  WriteColored('  ℹ ', ccBlue);
  WriteColoredLn('Type --help for more options', ccWhite);
  WriteColoredLn('', ccWhite);  // Final spacing
end;

function TGreetCommand.Execute: Integer;
var
  UserName: string;
begin
  Result := 0;
  try
    ShowHeader;

    // Get the UserName parameter or fall back to default
    if not GetParameterValue('--name', UserName) then
      UserName := 'World';  // Default if no UserName provided

    ShowGreeting(UserName);
    ShowFooter;
  except
    // Handle any errors with a red X symbol
    on E: Exception do
    begin
      WriteColored('  ✘ ', ccRed);
      WriteColoredLn('Error: ' + E.Message, ccRed);
      Result := 1;  // Return error code
    end;
  end;
end;

var
  App: ICLIApplication;
  Cmd: TGreetCommand;
begin
  try
    // Create the main application with name and version
    App := CreateCLIApplication('ColorDemo', '1.0.0');
    
    // Create the greet command and configure its parameters
    Cmd := TGreetCommand.Create('greet', 'Display a colorful greeting');
    Cmd.AddStringParameter(
      '-n',            // Short form
      '--name',        // Long form
      'Name to greet', // Description
      False,           // Optional parameter
      'World'          // Default value if not provided
    );
    
    // Register command and run the application
    App.RegisterCommand(Cmd);
    ExitCode := App.Execute;
  except
    // Handle any fatal errors during setup
    on E: Exception do
    begin
      WriteColoredLn('Fatal Error: ' + E.Message, ccRed);
      ExitCode := 1;
    end;
  end;
end. 
