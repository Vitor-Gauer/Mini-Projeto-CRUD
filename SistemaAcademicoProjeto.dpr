program SistemaAcademicoProjeto;

uses
  Vcl.Forms,
  SistemaAcademico in 'SistemaAcademico.pas' {TFSistemaAcademico},
  uLogin in 'uLogin.pas' {FLocalLogin};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFSistemaAcademico, FormPrincipal);
  Application.Run;
end.
