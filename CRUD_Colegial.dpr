program CRUD_Colegial;

uses
  Vcl.Forms,
  CRUDColegial in 'CRUDColegial.pas' {FColegio_CRUD},
  estudantes in 'estudantes.pas',
  professores in 'professores.pas',
  disciplinas in 'disciplinas.pas',
  turmas in 'turmas.pas',
  matriculas in 'matriculas.pas',
  UChamarArquivoJSON in 'UChamarArquivoJSON.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFColegio_CRUD, FColegio_CRUD);
  Application.Run;
end.
