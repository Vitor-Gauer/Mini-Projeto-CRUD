unit uLogin;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.Buttons;

type
  // Formulário de login para autenticação de usuários no sistema acadêmico.
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
    { Métodos privados }
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
// Evento ao exibir o formulário
// --------------------------------------------------------------------------------------------------
procedure TFLocalLogin.FormShow(Sender: TObject);
begin
  // Limpa os campos e posiciona o foco no campo de usuário.
  edtUsuario.Clear;
  edtSenha.Clear;
  edtUsuario.SetFocus;
end;

// --------------------------------------------------------------------------------------------------
// Botão: Entrar
// --------------------------------------------------------------------------------------------------
procedure TFLocalLogin.btnEntrarClick(Sender: TObject);
begin
  if Autenticar then
  begin
    ModalResult := mrOk; // Fecha o formulário com sucesso
  end
  else
  begin
    ShowMessage('Usuário ou senha incorretos!');
    edtSenha.Clear;
    edtUsuario.SetFocus;
  end;
end;

// --------------------------------------------------------------------------------------------------
// Botão: Cancelar
// --------------------------------------------------------------------------------------------------
procedure TFLocalLogin.btnCancelarClick(Sender: TObject);
begin
  ModalResult := mrCancel; // Fecha o formulário com cancelamento
end;

// --------------------------------------------------------------------------------------------------
// Função: Autenticar
// --------------------------------------------------------------------------------------------------
function TFLocalLogin.Autenticar: Boolean;
var
  usuario, senha: string;
begin
  Result := False;
  usuario := Trim(edtUsuario.Text);
  senha := Trim(edtSenha.Text);

  // Validação simples de campos
  if (usuario = '') or (senha = '') then
  begin
    ShowMessage('Preencha todos os campos!');
    Exit;
  end;

  // Simulação de autenticação com base em credenciais fixas
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
