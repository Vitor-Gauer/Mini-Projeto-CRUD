program Crudando;

uses
  Vcl.Forms,
  CRUDColegial in 'CRUDColegial.pas' {,
  UChamarArquivoJSON in 'UChamarArquivoJSON.pas'};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFColegio_CRUD, FColegio_CRUD);
  Application.Run;
end.
