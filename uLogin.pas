unit uLogin;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.Buttons;

type
  // Formul�rio de login para autentica��o de usu�rios no sistema acad�mico.
  TFLocalLogin = class(TForm)
    lblUsuario: TLabel;
    lblSenha: TLabel;
    edtUsuario: TEdit;
    edtSenha: TEdit;
    btnEntrar: TButton;
    btnCancelar: TButton;
    lblTitulo: TLabel;
    procedure btnEntrarClick(Sender: TObject);
    procedure btnCancelarClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { M�todos privados }
    function Autenticar: Boolean;
  public
    UsuarioLogado: string;
    NivelAcesso: string;
  end;

var
  FLocalLogin: TFLocalLogin;

implementation

{$R *.dfm}

// --------------------------------------------------------------------------------------------------
// Evento ao exibir o formul�rio
// --------------------------------------------------------------------------------------------------
procedure TFLocalLogin.FormShow(Sender: TObject);
begin
  // Limpa os campos e posiciona o foco no campo de usu�rio.
  edtUsuario.Clear;
  edtSenha.Clear;
  edtUsuario.SetFocus;
end;

// --------------------------------------------------------------------------------------------------
// Bot�o: Entrar
// --------------------------------------------------------------------------------------------------
procedure TFLocalLogin.btnEntrarClick(Sender: TObject);
begin
  if Autenticar then
  begin
    ModalResult := mrOk; // Fecha o formul�rio com sucesso
  end
  else
  begin
    ShowMessage('Usu�rio ou senha incorretos!');
    edtSenha.Clear;
    edtUsuario.SetFocus;
  end;
end;

// --------------------------------------------------------------------------------------------------
// Bot�o: Cancelar
// --------------------------------------------------------------------------------------------------
procedure TFLocalLogin.btnCancelarClick(Sender: TObject);
begin
  ModalResult := mrCancel; // Fecha o formul�rio com cancelamento
end;

// --------------------------------------------------------------------------------------------------
// Fun��o: Autenticar
// --------------------------------------------------------------------------------------------------
function TFLocalLogin.Autenticar: Boolean;
var
  usuario, senha: string;
begin
  Result := False;
  usuario := Trim(edtUsuario.Text);
  senha := Trim(edtSenha.Text);

  // Valida��o simples de campos
  if (usuario = '') or (senha = '') then
  begin
    ShowMessage('Preencha todos os campos!');
    Exit;
  end;

  // Simula��o de autentica��o com base em credenciais fixas
  // Em um sistema real, isso viria de um banco de dados.
  if (usuario = 'admin') and (senha = '123') then
  begin
    UsuarioLogado := usuario;
    NivelAcesso := 'admin';
    Result := True;
  end
  else if (usuario = 'professor') and (senha = '123') then
  begin
    UsuarioLogado := usuario;
    NivelAcesso := 'professor';
    Result := True;
  end
  else if (usuario = 'estudante') and (senha = '123') then
  begin
    UsuarioLogado := usuario;
    NivelAcesso := 'estudante';
    Result := True;
  end;
end;

end.
