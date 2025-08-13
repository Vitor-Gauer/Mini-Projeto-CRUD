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
    // --- Componente adicionado ---
    btnCadastroUsuarios: TButton;
    // ----------------------------
    procedure btnEntrarClick(Sender: TObject);
    procedure btnCancelarClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnCadastroUsuariosClick(Sender: TObject);
  private
    { M�todos privados }
    FAdminAutenticado: Boolean; // Flag para controlar o estado do login admin
    function Autenticar: Boolean;
  public
    UsuarioLogado: string;
    NivelAcesso: string;
  end;

var
  FLocalLogin: TFLocalLogin;

implementation

{$R *.dfm}

// --- Adicionado Uses para o formul�rio de cadastro ---
uses
  uCadastroUsuarios;
// -----------------------------------------------------

// --------------------------------------------------------------------------------------------------
// Evento ao exibir o formul�rio
// --------------------------------------------------------------------------------------------------
procedure TFLocalLogin.FormShow(Sender: TObject);
begin
  // Limpa os campos e posiciona o foco no campo de usu�rio.
  edtUsuario.Clear;
  edtSenha.Clear;
  edtUsuario.SetFocus;
  // Esconde o bot�o de gerenciamento de usu�rios e reseta a flag
  btnCadastroUsuarios.Visible := False;
  FAdminAutenticado := False; // Inicializa a flag
end;

// --------------------------------------------------------------------------------------------------
// Bot�o: Entrar
// --------------------------------------------------------------------------------------------------
procedure TFLocalLogin.btnEntrarClick(Sender: TObject);
begin
  // --- Verifica se � o segundo clique de um admin autenticado ---
  if FAdminAutenticado then
  begin
    // Se o admin j� foi autenticado e o bot�o estava vis�vel,
    // este clique significa "prosseguir para o sistema principal".
    ModalResult := mrOk; // Fecha o formul�rio com sucesso
    Exit; // Sai do procedimento
  end;
  // --- Fim da verifica��o ---

  if Autenticar then
  begin
    // --- FLUXO MODIFICADO PARA O NOVO REQUISITO ---
    // Torna o bot�o de gerenciamento de usu�rios vis�vel se for admin
    if NivelAcesso = 'admin' then
    begin
      btnCadastroUsuarios.Visible := True;
      FAdminAutenticado := True; // Marca que o admin foi autenticado
      // N�O fecha o login imediatamente.
      // Permite clicar no novo bot�o ou clicar em "Entrar" novamente.
      btnCadastroUsuarios.SetFocus; // Opcional: foca o bot�o
      // Mostra uma dica ou mensagem opcional
      // ShowMessage('Bem-vindo, administrador! Clique em "Gerenciar Usu�rios" ou "Entrar" novamente para continuar.');
    end
    else
    begin
      // Se n�o for admin, fecha o login normalmente
      btnCadastroUsuarios.Visible := False;
      ModalResult := mrOk; // Fecha o formul�rio com sucesso
    end;
    // -----------------------------
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

// --------------------------------------------------------------------------------------------------
// Bot�o: Gerenciar Usu�rios (Cadastro de Usu�rios)
// --------------------------------------------------------------------------------------------------
procedure TFLocalLogin.btnCadastroUsuariosClick(Sender: TObject);
var
  FormCadastroUsuarios: TFCadastroUsuarios;
begin
  // Verifica o n�vel de acesso (deve ser 'admin')
  if (NivelAcesso = 'admin') and FAdminAutenticado then
  begin
    // Cria e mostra o formul�rio de cadastro de usu�rios
    FormCadastroUsuarios := TFCadastroUsuarios.Create(nil);
    try
      FormCadastroUsuarios.ShowModal;
    finally
      FormCadastroUsuarios.Free;
    end;
    // Ap�s fechar o formul�rio de cadastro, fecha o login tamb�m.
    ModalResult := mrOk;
  end
  else
  begin
    ShowMessage('Acesso negado. Apenas administradores podem gerenciar usu�rios.');
  end;
end;


end.
