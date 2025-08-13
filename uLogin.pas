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
    // --- Componente adicionado ---
    btnCadastroUsuarios: TButton;
    // ----------------------------
    procedure btnEntrarClick(Sender: TObject);
    procedure btnCancelarClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnCadastroUsuariosClick(Sender: TObject);
  private
    { Métodos privados }
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

// --- Adicionado Uses para o formulário de cadastro ---
uses
  uCadastroUsuarios;
// -----------------------------------------------------

// --------------------------------------------------------------------------------------------------
// Evento ao exibir o formulário
// --------------------------------------------------------------------------------------------------
procedure TFLocalLogin.FormShow(Sender: TObject);
begin
  // Limpa os campos e posiciona o foco no campo de usuário.
  edtUsuario.Clear;
  edtSenha.Clear;
  edtUsuario.SetFocus;
  // Esconde o botão de gerenciamento de usuários e reseta a flag
  btnCadastroUsuarios.Visible := False;
  FAdminAutenticado := False; // Inicializa a flag
end;

// --------------------------------------------------------------------------------------------------
// Botão: Entrar
// --------------------------------------------------------------------------------------------------
procedure TFLocalLogin.btnEntrarClick(Sender: TObject);
begin
  // --- Verifica se é o segundo clique de um admin autenticado ---
  if FAdminAutenticado then
  begin
    // Se o admin já foi autenticado e o botão estava visível,
    // este clique significa "prosseguir para o sistema principal".
    ModalResult := mrOk; // Fecha o formulário com sucesso
    Exit; // Sai do procedimento
  end;
  // --- Fim da verificação ---

  if Autenticar then
  begin
    // --- FLUXO MODIFICADO PARA O NOVO REQUISITO ---
    // Torna o botão de gerenciamento de usuários visível se for admin
    if NivelAcesso = 'admin' then
    begin
      btnCadastroUsuarios.Visible := True;
      FAdminAutenticado := True; // Marca que o admin foi autenticado
      // NÃO fecha o login imediatamente.
      // Permite clicar no novo botão ou clicar em "Entrar" novamente.
      btnCadastroUsuarios.SetFocus; // Opcional: foca o botão
      // Mostra uma dica ou mensagem opcional
      // ShowMessage('Bem-vindo, administrador! Clique em "Gerenciar Usuários" ou "Entrar" novamente para continuar.');
    end
    else
    begin
      // Se não for admin, fecha o login normalmente
      btnCadastroUsuarios.Visible := False;
      ModalResult := mrOk; // Fecha o formulário com sucesso
    end;
    // -----------------------------
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
// Botão: Gerenciar Usuários (Cadastro de Usuários)
// --------------------------------------------------------------------------------------------------
procedure TFLocalLogin.btnCadastroUsuariosClick(Sender: TObject);
var
  FormCadastroUsuarios: TFCadastroUsuarios;
begin
  // Verifica o nível de acesso (deve ser 'admin')
  if (NivelAcesso = 'admin') and FAdminAutenticado then
  begin
    // Cria e mostra o formulário de cadastro de usuários
    FormCadastroUsuarios := TFCadastroUsuarios.Create(nil);
    try
      FormCadastroUsuarios.ShowModal;
    finally
      FormCadastroUsuarios.Free;
    end;
    // Após fechar o formulário de cadastro, fecha o login também.
    ModalResult := mrOk;
  end
  else
  begin
    ShowMessage('Acesso negado. Apenas administradores podem gerenciar usuários.');
  end;
end;


end.
