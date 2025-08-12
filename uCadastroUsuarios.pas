unit uCadastroUsuarios;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls;

type
  // Formulário para cadastro e gerenciamento de usuários do sistema.
  TFCadastroUsuarios = class(TForm)
    pnlDadosUsuario: TPanel;
    lblCodigo: TLabel;
    lblUsuario: TLabel;
    lblSenha: TLabel;
    lblNivel: TLabel;
    edtCodigo: TEdit;
    edtUsuario: TEdit;
    edtSenha: TEdit;
    cmbNivel: TComboBox;
    btnIncluir: TButton;
    btnAtualizar: TButton;
    btnExcluir: TButton;
    btnBuscar: TButton;
    btnLimpar: TButton;
    lbxUsuarios: TListBox;
    btnFechar: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnIncluirClick(Sender: TObject);
    procedure btnAtualizarClick(Sender: TObject);
    procedure btnExcluirClick(Sender: TObject);
    procedure btnBuscarClick(Sender: TObject);
    procedure btnLimparClick(Sender: TObject);
    procedure lbxUsuariosClick(Sender: TObject);
    procedure btnFecharClick(Sender: TObject);
  private
    { Private declarations }
    FUsuarios: TStringList; // Armazena os dados dos usuários em memória.

    // Métodos auxiliares para manipulação dos dados e interface.
    procedure LimparCampos;
    procedure CarregarDadosUsuario(const LinhaUsuario: string);
    procedure AtualizarListaUsuarios;
    function ObterProximoCodigo: Integer;
    function ValidarCampos: Boolean; // Verifica se os campos obrigatórios estão preenchidos.
    function UsuarioExiste(const Usuario: string; CodigoIgnorar: Integer = -1): Boolean; // Verifica se o nome de usuário já existe.
  public
    { Public declarations }
  end;

var
  FCadastroUsuarios: TFCadastroUsuarios;

implementation

{$R *.dfm}

// --------------------------------------------------------------------------------------------------
// Eventos do Formulário
// --------------------------------------------------------------------------------------------------

// --------------------------------------------------------------------------------------------------
// Evento ao criar o formulário.
// --------------------------------------------------------------------------------------------------
procedure TFCadastroUsuarios.FormCreate(Sender: TObject);
begin
  // Inicializa a lista de usuários.
  FUsuarios := TStringList.Create;

  // Verificação defensiva rigorosa (embora improvável após Create)
  if not Assigned(FUsuarios) then
  begin
    // Mostra mensagem de erro
    ShowMessage('Falha crítica ao criar FUsuarios! O formulário será fechado.');
    // Invés de Application.Terminate ou Self.Close, que podem ser arriscados aqui,
    // desabilita os componentes e força o fechamento.
    // Ou simplesmente não inicializa e deixa o resto do código tratar FUsuarios = nil.
    // Uma abordagem mais robusta é desabilitar os controles de interação.
    btnIncluir.Enabled := False;
    btnAtualizar.Enabled := False;
    btnExcluir.Enabled := False;
    btnBuscar.Enabled := False;
    // AtualizarListaUsuarios também tem verificações, então pode ser chamado.
    // Mas como não há usuários, a lista estará vazia.
    lbxUsuarios.Items.Add('Erro: Sistema de usuários não inicializado.');
    Exit; // Sai do FormCreate, mas o form continua existindo (embora inutilizável)
  end;

  // Carrega dados iniciais de exemplo (opcional).
  // FUsuarios.Add('1|admin|123|admin');
  // FUsuarios.Add('2|professor|123|professor');
  // FUsuarios.Add('3|estudante|123|estudante');

  // Atualiza a lista na interface.
  // AtualizarListaUsuarios também tem verificação interna para FUsuarios.
  AtualizarListaUsuarios;
end;

// --------------------------------------------------------------------------------------------------
// Evento ao destruir o formulário.
// --------------------------------------------------------------------------------------------------
procedure TFCadastroUsuarios.FormDestroy(Sender: TObject);
begin
  // Libera a memória alocada para a lista de usuários.
  FUsuarios.Free;
end;

// --------------------------------------------------------------------------------------------------
// Eventos dos Botões
// --------------------------------------------------------------------------------------------------

// --------------------------------------------------------------------------------------------------
// Botão: Incluir Usuário
// --------------------------------------------------------------------------------------------------
procedure TFCadastroUsuarios.btnIncluirClick(Sender: TObject);
var
  NovoCodigo: Integer;
  NovaLinha: string;
begin
  // --- Verificação rigorosa e defensiva ---
  if (FUsuarios = nil) then // Teste mais explícito
  begin
    ShowMessage('Erro Crítico: FUsuarios é NIL! Verifique o FormCreate e FormDestroy.');
    Exit;
  end;
  try
    // Tenta acessar Count para ver se o objeto está íntegro
    // Se isso falhar, indica problema no objeto TStringList em si.
    var TestCount := FUsuarios.Count;
  except
    on E: Exception do
    begin
      ShowMessage('Erro Crítico: FUsuarios parece corrompido. Count falhou: ' + E.Message);
      Exit;
    end;
  end;
  // ----------------------------------------

  // Valida os campos de entrada.
  if not ValidarCampos then
    Exit;

  // Verifica se o nome de usuário já existe.
  if UsuarioExiste(edtUsuario.Text) then
  begin
    ShowMessage('Nome de usuário já existe!');
    edtUsuario.SetFocus;
    Exit;
  end;

  // Obtém o próximo código disponível.
  NovoCodigo := ObterProximoCodigo;
  // Formata a linha de dados do novo usuário.
  NovaLinha := Format('%d|%s|%s|%s', [NovoCodigo, edtUsuario.Text, edtSenha.Text, cmbNivel.Text]);

  // Adiciona o novo usuário à lista.
  try
    FUsuarios.Add(NovaLinha); // Agora é mais seguro (com as verificações acima)
    ShowMessage('Usuário incluído com sucesso!');
  except
    on E: Exception do
    begin
      ShowMessage('Erro ao adicionar usuário à lista: ' + E.Message);
      Exit; // Sai em caso de erro na adição
    end;
  end;

  // Atualiza a interface.
  LimparCampos;
  AtualizarListaUsuarios; // Certifique-se de que este método também tenha verificações
end;

// --------------------------------------------------------------------------------------------------
// Botão: Atualizar Usuário
// --------------------------------------------------------------------------------------------------
procedure TFCadastroUsuarios.btnAtualizarClick(Sender: TObject);
var
  Codigo: Integer;
  LinhaIndex: Integer;
  LinhaExistente, LinhaAtualizada: string;
  Partes: TArray<string>;
begin
  // Verifica se um usuário foi selecionado (código preenchido e válido).
  if not TryStrToInt(edtCodigo.Text, Codigo) or (Codigo <= 0) then
  begin
    ShowMessage('Selecione um usuário para atualizar!');
    Exit;
  end;

  // Valida os campos de entrada.
  if not ValidarCampos then
    Exit;

  // Verifica se o nome de usuário já existe (excluindo o próprio usuário sendo editado).
  if UsuarioExiste(edtUsuario.Text, Codigo) then
  begin
    ShowMessage('Nome de usuário já existe!');
    edtUsuario.SetFocus;
    Exit;
  end;

  // Procura o índice da linha correspondente ao código na lista.
  LinhaIndex := -1;
  for var i := 0 to FUsuarios.Count - 1 do
  begin
    Partes := FUsuarios[i].Split(['|']);
    if (Length(Partes) >= 1) and (Partes[0].ToInteger = Codigo) then
    begin
      LinhaIndex := i;
      Break;
    end;
  end;

  if LinhaIndex = -1 then
  begin
    ShowMessage('Erro: Usuário não encontrado na lista interna!');
    Exit;
  end;

  // Obtém a linha existente.
  LinhaExistente := FUsuarios[LinhaIndex];
  // Formata a linha de dados atualizada.
  LinhaAtualizada := Format('%d|%s|%s|%s', [Codigo, edtUsuario.Text, edtSenha.Text, cmbNivel.Text]);

  // Atualiza a linha na lista.
  FUsuarios[LinhaIndex] := LinhaAtualizada;
  ShowMessage('Usuário atualizado com sucesso!');

  // Atualiza a interface.
  LimparCampos;
  AtualizarListaUsuarios;
end;

// --------------------------------------------------------------------------------------------------
// Botão: Excluir Usuário
// --------------------------------------------------------------------------------------------------
procedure TFCadastroUsuarios.btnExcluirClick(Sender: TObject);
var
  Codigo: Integer;
  LinhaIndex: Integer;
  Partes: TArray<string>;
begin
  // Verifica se um usuário foi selecionado.
  if not TryStrToInt(edtCodigo.Text, Codigo) or (Codigo <= 0) then
  begin
    ShowMessage('Selecione um usuário para excluir!');
    Exit;
  end;

  // Procura o índice da linha correspondente ao código na lista.
  LinhaIndex := -1;
  for var i := 0 to FUsuarios.Count - 1 do
  begin
    Partes := FUsuarios[i].Split(['|']);
    if (Length(Partes) >= 1) and (Partes[0].ToInteger = Codigo) then
    begin
      LinhaIndex := i;
      Break;
    end;
  end;

  if LinhaIndex = -1 then
  begin
    ShowMessage('Erro: Usuário não encontrado na lista interna!');
    Exit;
  end;

  // Pede confirmação ao usuário antes de excluir.
  if MessageDlg(Format('Confirma a exclusão do usuário "%s"?', [edtUsuario.Text]),
    mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    // Remove o usuário da lista.
    FUsuarios.Delete(LinhaIndex);
    ShowMessage('Usuário excluído com sucesso!');

    // Atualiza a interface.
    LimparCampos;
    AtualizarListaUsuarios;
  end;
end;

// --------------------------------------------------------------------------------------------------
// Botão: Buscar Usuário
// --------------------------------------------------------------------------------------------------
procedure TFCadastroUsuarios.btnBuscarClick(Sender: TObject);
var
  CodigoStr, LinhaUsuario: string;
  Codigo: Integer;
  Partes: TArray<string>;
  Encontrado: Boolean;
begin
  // Valida se um código foi digitado para a busca.
  CodigoStr := Trim(edtCodigo.Text);
  if CodigoStr = '' then
  begin
    ShowMessage('Digite um código para buscar!');
    edtCodigo.SetFocus;
    Exit;
  end;

  if not TryStrToInt(CodigoStr, Codigo) or (Codigo <= 0) then
  begin
    ShowMessage('Código inválido!');
    edtCodigo.SetFocus;
    Exit;
  end;

  // Procura o usuário na lista pelo código.
  Encontrado := False;
  for var i := 0 to FUsuarios.Count - 1 do
  begin
    LinhaUsuario := FUsuarios[i];
    Partes := LinhaUsuario.Split(['|']);
    if (Length(Partes) >= 1) and (Partes[0].ToInteger = Codigo) then
    begin
      // Carrega os dados do usuário encontrado nos campos.
      CarregarDadosUsuario(LinhaUsuario);
      Encontrado := True;
      Break;
    end;
  end;

  if not Encontrado then
    ShowMessage('Usuário não encontrado!');
end;

// --------------------------------------------------------------------------------------------------
// Botão: Limpar Campos
// --------------------------------------------------------------------------------------------------
procedure TFCadastroUsuarios.btnLimparClick(Sender: TObject);
begin
  LimparCampos;
end;

// --------------------------------------------------------------------------------------------------
// Botão: Fechar Formulário
// --------------------------------------------------------------------------------------------------
procedure TFCadastroUsuarios.btnFecharClick(Sender: TObject);
begin
  Close; // Fecha o formulário.
end;

// --------------------------------------------------------------------------------------------------
// Evento ao Clicar na ListBox
// --------------------------------------------------------------------------------------------------
procedure TFCadastroUsuarios.lbxUsuariosClick(Sender: TObject);
var
  LinhaSelecionada: string;
begin
  // Se um item da ListBox for clicado, carrega os dados nos campos.
  if lbxUsuarios.ItemIndex >= 0 then
  begin
    LinhaSelecionada := lbxUsuarios.Items[lbxUsuarios.ItemIndex];
    CarregarDadosUsuario(LinhaSelecionada);
  end;
end;

// --------------------------------------------------------------------------------------------------
// Métodos Auxiliares
// --------------------------------------------------------------------------------------------------

// --------------------------------------------------------------------------------------------------
// Limpa os campos de entrada e reabilita o campo de código.
// --------------------------------------------------------------------------------------------------
procedure TFCadastroUsuarios.LimparCampos;
begin
  edtCodigo.Clear;
  edtUsuario.Clear;
  edtSenha.Clear;
  cmbNivel.ItemIndex := 0; // Seleciona 'admin' por padrão.
  edtCodigo.Enabled := True; // Reabilita para nova inclusão.
  btnAtualizar.Enabled := False; // Desabilita botão de atualizar.
  btnExcluir.Enabled := False; // Desabilita botão de excluir.
  edtUsuario.SetFocus; // Coloca o foco no campo de usuário.
end;

// --------------------------------------------------------------------------------------------------
// Carrega os dados de uma linha formatada nos campos de entrada e desabilita o campo de código.
// --------------------------------------------------------------------------------------------------
procedure TFCadastroUsuarios.CarregarDadosUsuario(const LinhaUsuario: string);
var
  Partes: TArray<string>;
begin
  // Divide a linha em partes usando o caractere '|'.
  Partes := LinhaUsuario.Split(['|']);
  // Verifica se a linha tem o número esperado de partes.
  if Length(Partes) >= 4 then
  begin
    edtCodigo.Text := Partes[0];
    edtUsuario.Text := Partes[1];
    edtSenha.Text := Partes[2];
    // Seleciona o nível de acesso no ComboBox.
    cmbNivel.ItemIndex := cmbNivel.Items.IndexOf(Partes[3]);
    if cmbNivel.ItemIndex = -1 then
      cmbNivel.ItemIndex := 0; // Fallback para 'admin' se não encontrar.

    edtCodigo.Enabled := False; // Desabilita o campo de código após carregar.
    btnAtualizar.Enabled := True; // Habilita botão de atualizar.
    btnExcluir.Enabled := True; // Habilita botão de excluir.
  end
  else
    ShowMessage('Erro ao carregar dados do usuário: Formato inválido.');
end;

// --------------------------------------------------------------------------------------------------
// Atualiza a ListBox com a lista de usuários formatada.
// --------------------------------------------------------------------------------------------------
procedure TFCadastroUsuarios.AtualizarListaUsuarios;
var
  Linha: string;
  Partes: TArray<string>;
begin
  lbxUsuarios.Items.Clear;
  // Itera sobre a lista de usuários e adiciona uma representação formatada à ListBox.
  for var i := 0 to FUsuarios.Count - 1 do
  begin
    Linha := FUsuarios[i];
    Partes := Linha.Split(['|']);
    if Length(Partes) >= 4 then
      lbxUsuarios.Items.Add(Format('Cód: %s | Usuário: %s | Nível: %s', [Partes[0], Partes[1], Partes[3]]))
    else
      lbxUsuarios.Items.Add('Formato inválido: ' + Linha);
  end;
end;

// --------------------------------------------------------------------------------------------------
// Encontra e retorna o próximo código sequencial disponível.
// --------------------------------------------------------------------------------------------------
function TFCadastroUsuarios.ObterProximoCodigo: Integer;
var
  MaiorCodigo: Integer;
  Partes: TArray<string>;
begin
  MaiorCodigo := 0;
  // Itera sobre a lista para encontrar o maior código existente.
  for var i := 0 to FUsuarios.Count - 1 do
  begin
    Partes := FUsuarios[i].Split(['|']);
    if (Length(Partes) >= 1) and TryStrToInt(Partes[0], Result) and (Result > MaiorCodigo) then
      MaiorCodigo := Result;
  end;
  Result := MaiorCodigo + 1; // O próximo código é o maior encontrado mais 1.
end;

// --------------------------------------------------------------------------------------------------
// Valida se os campos obrigatórios estão preenchidos.
// --------------------------------------------------------------------------------------------------
function TFCadastroUsuarios.ValidarCampos: Boolean;
begin
  Result := True;
  if Trim(edtUsuario.Text) = '' then
  begin
    ShowMessage('Nome de usuário é obrigatório!');
    edtUsuario.SetFocus;
    Result := False;
    Exit;
  end;
  if Trim(edtSenha.Text) = '' then
  begin
    ShowMessage('Senha é obrigatória!');
    edtSenha.SetFocus;
    Result := False;
    Exit;
  end;
  if cmbNivel.ItemIndex = -1 then
  begin
    ShowMessage('Selecione um nível de acesso!');
    cmbNivel.SetFocus;
    Result := False;
    Exit;
  end;
end;

// --------------------------------------------------------------------------------------------------
// Verifica se um nome de usuário já existe.
// --------------------------------------------------------------------------------------------------
function TFCadastroUsuarios.UsuarioExiste(const Usuario: string; CodigoIgnorar: Integer = -1): Boolean;
var
  Linha: string;
  Partes: TArray<string>;
  CodigoExistente: Integer;
begin
  Result := False;

  // --- Verificação rigorosa ---
  if (FUsuarios = nil) then
  begin
    ShowMessage('Erro Interno em UsuarioExiste: FUsuarios é NIL!');
    Exit; // Retorna False
  end;
  // ---------------------------

  // Itera sobre a lista para verificar se o nome de usuário já existe.
  try
    for var i := 0 to FUsuarios.Count - 1 do // <-- Erro ocorria aqui antes
    begin
      Linha := FUsuarios[i];
      Partes := Linha.Split(['|']);
      if (Length(Partes) >= 2) and (SameText(Partes[1], Usuario)) then
      begin
        // Se um código para ignorar foi fornecido, verifica se é o mesmo usuário.
        if (CodigoIgnorar >= 0) and TryStrToInt(Partes[0], CodigoExistente) and (CodigoExistente = CodigoIgnorar) then
          Continue // É o próprio usuário sendo editado, ignora.
        else
        begin
          Result := True; // Nome de usuário já existe para outro usuário.
          Exit;
        end;
      end;
    end;
  except
    on E: Exception do
    begin
      ShowMessage('Erro ao verificar existência do usuário: ' + E.Message);
      Result := False; // Assume que não existe em caso de erro
    end;
  end;
end;

end.
