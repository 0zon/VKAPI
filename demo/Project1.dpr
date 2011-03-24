program Project1;

uses
  Forms,
  VKAPI in '..\src\VKAPI.pas',
  md5hash in '..\src\md5hash.pas',
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
