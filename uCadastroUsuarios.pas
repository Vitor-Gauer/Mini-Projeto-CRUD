unit uCadastroUsuarios;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls;

type
  // Formul�rio para cadastro e gerenciamento de usu�rios do sistema.
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
    FUsuarios: TStringList; // Armazena os dados dos usu�rios em mem�ria.

    // M�todos auxiliares para manipula��o dos dados e interface.
    procedure LimparCampos;
    procedure CarregarDadosUsuario(const LinhaUsuario: string);
    procedure AtualizarListaUsuarios;
    function ObterProximoCodigo: Integer;
    function ValidarCampos: Boolean; // Verifica se os campos obrigat�rios est�o preenchidos.
    function UsuarioExiste(const Usuario: string; CodigoIgnorar: Integer = -1): Boolean; // Verifica se o nome de usu�rio j� existe.
  public
    { Public declarations }
  end;

var
  FCadastroUsuarios: TFCadastroUsuarios;

implementation

{$R *.dfm}

// --------------------------------------------------------------------------------------------------
// Eventos do Formul�rio
// --------------------------------------------------------------------------------------------------

// --------------------------------------------------------------------------------------------------
// Evento ao criar o formul�rio.
// --------------------------------------------------------------------------------------------------
procedure TFCadastroUsuarios.FormCreate(Sender: TObject);
begin
  // Inicializa a lista de usu�rios.
  FUsuarios := TStringList.Create;

  // Verifica��o defensiva rigorosa (embora improv�vel ap�s Create)
  if not Assigned(FUsuarios) then
  begin
    // Mostra mensagem de erro
    ShowMessage('Falha cr�tica ao criar FUsuarios! O formul�rio ser� fechado.');
    // Inv�s de Application.Terminate ou Self.Close, que podem ser arriscados aqui,
    // desabilita os componentes e for�a o fechamento.
    // Ou simplesmente n�o inicializa e deixa o resto do c�digo tratar FUsuarios = nil.
    // Uma abordagem mais robusta � desabilitar os controles de intera��o.
    btnIncluir.Enabled := False;
    btnAtualizar.Enabled := False;
    btnExcluir.Enabled := False;
    btnBuscar.Enabled := False;
    // AtualizarListaUsuarios tamb�m tem verifica��es, ent�o pode ser chamado.
    // Mas como n�o h� usu�rios, a lista estar� vazia.
    lbxUsuarios.Items.Add('Erro: Sistema de usu�rios n�o inicializado.');
    Exit; // Sai do FormCreate, mas o form continua existindo (embora inutiliz�vel)
  end;

  // Carrega dados iniciais de exemplo (opcional).
  // FUsuarios.Add('1|admin|123|admin');
  // FUsuarios.Add('2|professor|123|professor');
  // FUsuarios.Add('3|estudante|123|estudante');

  // Atualiza a lista na interface.
  // AtualizarListaUsuarios tamb�m tem verifica��o interna para FUsuarios.
  AtualizarListaUsuarios;
end;

// --------------------------------------------------------------------------------------------------
// Evento ao destruir o formul�rio.
// --------------------------------------------------------------------------------------------------
procedure TFCadastroUsuarios.FormDestroy(Sender: TObject);
begin
  // Libera a mem�ria alocada para a lista de usu�rios.
  FUsuarios.Free;
end;

// --------------------------------------------------------------------------------------------------
// Eventos dos Bot�es
// --------------------------------------------------------------------------------------------------

// --------------------------------------------------------------------------------------------------
// Bot�o: Incluir Usu�rio
// --------------------------------------------------------------------------------------------------
procedure TFCadastroUsuarios.btnIncluirClick(Sender: TObject);
var
  NovoCodigo: Integer;
  NovaLinha: string;
begin
  // --- Verifica��o rigorosa e defensiva ---
  if (FUsuarios = nil) then // Teste mais expl�cito
  begin
    ShowMessage('Erro Cr�tico: FUsuarios � NIL! Verifique o FormCreate e FormDestroy.');
    Exit;
  end;
  try
    // Tenta acessar Count para ver se o objeto est� �ntegro
    // Se isso falhar, indica problema no objeto TStringList em si.
    var TestCount := FUsuarios.Count;
  except
    on E: Exception do
    begin
      ShowMessage('Erro Cr�tico: FUsuarios parece corrompido. Count falhou: ' + E.Message);
      Exit;
    end;
  end;
  // ----------------------------------------

  // Valida os campos de entrada.
  if not ValidarCampos then
    Exit;

  // Verifica se o nome de usu�rio j� existe.
  if UsuarioExiste(edtUsuario.Text) then
  begin
    ShowMessage('Nome de usu�rio j� existe!');
    edtUsuario.SetFocus;
    Exit;
  end;

  // Obt�m o pr�ximo c�digo dispon�vel.
  NovoCodigo := ObterProximoCodigo;
  // Formata a linha de dados do novo usu�rio.
  NovaLinha := Format('%d|%s|%s|%s', [NovoCodigo, edtUsuario.Text, edtSenha.Text, cmbNivel.Text]);

  // Adiciona o novo usu�rio � lista.
  try
    FUsuarios.Add(NovaLinha); // Agora � mais seguro (com as verifica��es acima)
    ShowMessage('Usu�rio inclu�do com sucesso!');
  except
    on E: Exception do
    begin
      ShowMessage('Erro ao adicionar usu�rio � lista: ' + E.Message);
      Exit; // Sai em caso de erro na adi��o
    end;
  end;

  // Atualiza a interface.
  LimparCampos;
  AtualizarListaUsuarios; // Certifique-se de que este m�todo tamb�m tenha verifica��es
end;

// --------------------------------------------------------------------------------------------------
// Bot�o: Atualizar Usu�rio
// --------------------------------------------------------------------------------------------------
procedure TFCadastroUsuarios.btnAtualizarClick(Sender: TObject);
var
  Codigo: Integer;
  LinhaIndex: Integer;
  LinhaExistente, LinhaAtualizada: string;
  Partes: TArray<string>;
begin
  // Verifica se um usu�rio foi selecionado (c�digo preenchido e v�lido).
  if not TryStrToInt(edtCodigo.Text, Codigo) or (Codigo <= 0) then
  begin
    ShowMessage('Selecione um usu�rio para atualizar!');
    Exit;
  end;

  // Valida os campos de entrada.
  if not ValidarCampos then
    Exit;

  // Verifica se o nome de usu�rio j� existe (excluindo o pr�prio usu�rio sendo editado).
  if UsuarioExiste(edtUsuario.Text, Codigo) then
  begin
    ShowMessage('Nome de usu�rio j� existe!');
    edtUsuario.SetFocus;
    Exit;
  end;

  // Procura o �ndice da linha correspondente ao c�digo na lista.
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
    ShowMessage('Erro: Usu�rio n�o encontrado na lista interna!');
    Exit;
  end;

  // Obt�m a linha existente.
  LinhaExistente := FUsuarios[LinhaIndex];
  // Formata a linha de dados atualizada.
  LinhaAtualizada := Format('%d|%s|%s|%s', [Codigo, edtUsuario.Text, edtSenha.Text, cmbNivel.Text]);

  // Atualiza a linha na lista.
  FUsuarios[LinhaIndex] := LinhaAtualizada;
  ShowMessage('Usu�rio atualizado com sucesso!');

  // Atualiza a interface.
  LimparCampos;
  AtualizarListaUsuarios;
end;

// --------------------------------------------------------------------------------------------------
// Bot�o: Excluir Usu�rio
// --------------------------------------------------------------------------------------------------
procedure TFCadastroUsuarios.btnExcluirClick(Sender: TObject);
var
  Codigo: Integer;
  LinhaIndex: Integer;
  Partes: TArray<string>;
begin
  // Verifica se um usu�rio foi selecionado.
  if not TryStrToInt(edtCodigo.Text, Codigo) or (Codigo <= 0) then
  begin
    ShowMessage('Selecione um usu�rio para excluir!');
    Exit;
  end;

  // Procura o �ndice da linha correspondente ao c�digo na lista.
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
    ShowMessage('Erro: Usu�rio n�o encontrado na lista interna!');
    Exit;
  end;

  // Pede confirma��o ao usu�rio antes de excluir.
  if MessageDlg(Format('Confirma a exclus�o do usu�rio "%s"?', [edtUsuario.Text]),
    mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    // Remove o usu�rio da lista.
    FUsuarios.Delete(LinhaIndex);
    ShowMessage('Usu�rio exclu�do com sucesso!');

    // Atualiza a interface.
    LimparCampos;
    AtualizarListaUsuarios;
  end;
end;

// --------------------------------------------------------------------------------------------------
// Bot�o: Buscar Usu�rio
// --------------------------------------------------------------------------------------------------
procedure TFCadastroUsuarios.btnBuscarClick(Sender: TObject);
var
  CodigoStr, LinhaUsuario: string;
  Codigo: Integer;
  Partes: TArray<string>;
  Encontrado: Boolean;
begin
  // Valida se um c�digo foi digitado para a busca.
  CodigoStr := Trim(edtCodigo.Text);
  if CodigoStr = '' then
  begin
    ShowMessage('Digite um c�digo para buscar!');
    edtCodigo.SetFocus;
    Exit;
  end;

  if not TryStrToInt(CodigoStr, Codigo) or (Codigo <= 0) then
  begin
    ShowMessage('C�digo inv�lido!');
    edtCodigo.SetFocus;
    Exit;
  end;

  // Procura o usu�rio na lista pelo c�digo.
  Encontrado := False;
  for var i := 0 to FUsuarios.Count - 1 do
  begin
    LinhaUsuario := FUsuarios[i];
    Partes := LinhaUsuario.Split(['|']);
    if (Length(Partes) >= 1) and (Partes[0].ToInteger = Codigo) then
    begin
      // Carrega os dados do usu�rio encontrado nos campos.
      CarregarDadosUsuario(LinhaUsuario);
      Encontrado := True;
      Break;
    end;
  end;

  if not Encontrado then
    ShowMessage('Usu�rio n�o encontrado!');
end;

// --------------------------------------------------------------------------------------------------
// Bot�o: Limpar Campos
// --------------------------------------------------------------------------------------------------
procedure TFCadastroUsuarios.btnLimparClick(Sender: TObject);
begin
  LimparCampos;
end;

// --------------------------------------------------------------------------------------------------
// Bot�o: Fechar Formul�rio
// --------------------------------------------------------------------------------------------------
procedure TFCadastroUsuarios.btnFecharClick(Sender: TObject);
begin
  Close; // Fecha o formul�rio.
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
// M�todos Auxiliares
// --------------------------------------------------------------------------------------------------

// --------------------------------------------------------------------------------------------------
// Limpa os campos de entrada e reabilita o campo de c�digo.
// --------------------------------------------------------------------------------------------------
procedure TFCadastroUsuarios.LimparCampos;
begin
  edtCodigo.Clear;
  edtUsuario.Clear;
  edtSenha.Clear;
  cmbNivel.ItemIndex := 0; // Seleciona 'admin' por padr�o.
  edtCodigo.Enabled := True; // Reabilita para nova inclus�o.
  btnAtualizar.Enabled := False; // Desabilita bot�o de atualizar.
  btnExcluir.Enabled := False; // Desabilita bot�o de excluir.
  edtUsuario.SetFocus; // Coloca o foco no campo de usu�rio.
end;

// --------------------------------------------------------------------------------------------------
// Carrega os dados de uma linha formatada nos campos de entrada e desabilita o campo de c�digo.
// --------------------------------------------------------------------------------------------------
procedure TFCadastroUsuarios.CarregarDadosUsuario(const LinhaUsuario: string);
var
  Partes: TArray<string>;
begin
  // Divide a linha em partes usando o caractere '|'.
  Partes := LinhaUsuario.Split(['|']);
  // Verifica se a linha tem o n�mero esperado de partes.
  if Length(Partes) >= 4 then
  begin
    edtCodigo.Text := Partes[0];
    edtUsuario.Text := Partes[1];
    edtSenha.Text := Partes[2];
    // Seleciona o n�vel de acesso no ComboBox.
    cmbNivel.ItemIndex := cmbNivel.Items.IndexOf(Partes[3]);
    if cmbNivel.ItemIndex = -1 then
      cmbNivel.ItemIndex := 0; // Fallback para 'admin' se n�o encontrar.

    edtCodigo.Enabled := False; // Desabilita o campo de c�digo ap�s carregar.
    btnAtualizar.Enabled := True; // Habilita bot�o de atualizar.
    btnExcluir.Enabled := True; // Habilita bot�o de excluir.
  end
  else
    ShowMessage('Erro ao carregar dados do usu�rio: Formato inv�lido.');
end;

// --------------------------------------------------------------------------------------------------
// Atualiza a ListBox com a lista de usu�rios formatada.
// --------------------------------------------------------------------------------------------------
procedure TFCadastroUsuarios.AtualizarListaUsuarios;
var
  Linha: string;
  Partes: TArray<string>;
begin
  lbxUsuarios.Items.Clear;
  // Itera sobre a lista de usu�rios e adiciona uma representa��o formatada � ListBox.
  for var i := 0 to FUsuarios.Count - 1 do
  begin
    Linha := FUsuarios[i];
    Partes := Linha.Split(['|']);
    if Length(Partes) >= 4 then
      lbxUsuarios.Items.Add(Format('C�d: %s | Usu�rio: %s | N�vel: %s', [Partes[0], Partes[1], Partes[3]]))
    else
      lbxUsuarios.Items.Add('Formato inv�lido: ' + Linha);
  end;
end;

// --------------------------------------------------------------------------------------------------
// Encontra e retorna o pr�ximo c�digo sequencial dispon�vel.
// --------------------------------------------------------------------------------------------------
function TFCadastroUsuarios.ObterProximoCodigo: Integer;
var
  MaiorCodigo: Integer;
  Partes: TArray<string>;
begin
  MaiorCodigo := 0;
  // Itera sobre a lista para encontrar o maior c�digo existente.
  for var i := 0 to FUsuarios.Count - 1 do
  begin
    Partes := FUsuarios[i].Split(['|']);
    if (Length(Partes) >= 1) and TryStrToInt(Partes[0], Result) and (Result > MaiorCodigo) then
      MaiorCodigo := Result;
  end;
  Result := MaiorCodigo + 1; // O pr�ximo c�digo � o maior encontrado mais 1.
end;

// --------------------------------------------------------------------------------------------------
// Valida se os campos obrigat�rios est�o preenchidos.
// --------------------------------------------------------------------------------------------------
function TFCadastroUsuarios.ValidarCampos: Boolean;
begin
  Result := True;
  if Trim(edtUsuario.Text) = '' then
  begin
    ShowMessage('Nome de usu�rio � obrigat�rio!');
    edtUsuario.SetFocus;
    Result := False;
    Exit;
  end;
  if Trim(edtSenha.Text) = '' then
  begin
    ShowMessage('Senha � obrigat�ria!');
    edtSenha.SetFocus;
    Result := False;
    Exit;
  end;
  if cmbNivel.ItemIndex = -1 then
  begin
    ShowMessage('Selecione um n�vel de acesso!');
    cmbNivel.SetFocus;
    Result := False;
    Exit;
  end;
end;

// --------------------------------------------------------------------------------------------------
// Verifica se um nome de usu�rio j� existe.
// --------------------------------------------------------------------------------------------------
function TFCadastroUsuarios.UsuarioExiste(const Usuario: string; CodigoIgnorar: Integer = -1): Boolean;
var
  Linha: string;
  Partes: TArray<string>;
  CodigoExistente: Integer;
begin
  Result := False;

  // --- Verifica��o rigorosa ---
  if (FUsuarios = nil) then
  begin
    ShowMessage('Erro Interno em UsuarioExiste: FUsuarios � NIL!');
    Exit; // Retorna False
  end;
  // ---------------------------

  // Itera sobre a lista para verificar se o nome de usu�rio j� existe.
  try
    for var i := 0 to FUsuarios.Count - 1 do // <-- Erro ocorria aqui antes
    begin
      Linha := FUsuarios[i];
      Partes := Linha.Split(['|']);
      if (Length(Partes) >= 2) and (SameText(Partes[1], Usuario)) then
      begin
        // Se um c�digo para ignorar foi fornecido, verifica se � o mesmo usu�rio.
        if (CodigoIgnorar >= 0) and TryStrToInt(Partes[0], CodigoExistente) and (CodigoExistente = CodigoIgnorar) then
          Continue // � o pr�prio usu�rio sendo editado, ignora.
        else
        begin
          Result := True; // Nome de usu�rio j� existe para outro usu�rio.
          Exit;
        end;
      end;
    end;
  except
    on E: Exception do
    begin
      ShowMessage('Erro ao verificar exist�ncia do usu�rio: ' + E.Message);
      Result := False; // Assume que n�o existe em caso de erro
    end;
  end;
end;

end.
