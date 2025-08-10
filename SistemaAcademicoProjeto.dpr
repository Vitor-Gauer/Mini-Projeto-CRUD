program SistemaAcademicoProjeto;

uses
  Vcl.Forms,
  SistemaAcademico in 'SistemaAcademico.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFSistemaAcademico, FormPrincipal);
  Application.Run;
end.
