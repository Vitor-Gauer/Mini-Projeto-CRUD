unit CRUDColegial;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Mask, Vcl.ExtCtrls,
  System.JSON, System.IOUtils, System.DateUtils,
  System.JSON.Writers, System.JSON.Readers;

type
  TFColegio_CRUD = class(TForm)
    btnConfirmarLogin: TButton;
    btnConfirmarNomeEstudante: TButton;
    btnDisciplinaFullStack: TButton;
    btnDisciplinaInformaticaBasica: TButton;
    btnDisciplinaSeguranca: TButton;
    btnDisciplinaUXDesign: TButton;
    btnDisciplinaAnaliseData: TButton;
    btnDisciplinaEtica: TButton;
    btnMostrarDados: TButton;
    btnProfessorFausto: TButton;
    btnProfessorFlavio: TButton;
    edtNomeEstudante: TEdit;
    edtSenha: TEdit;
    edtUsuario: TEdit;
    lblEscolhaDisciplina: TLabel;
    lblEscolhaProfessor: TLabel;
    pnlEscolhaDisciplina: TPanel;
    pnlEscolhaProfessor: TPanel;
    pnlLogin: TPanel;
    pnlCaminhoEscolha: TPanel;
    btnExibirLista: TButton;
    lblExibirLista: TLabel;
    lblAdicionarPessoasNaLista: TLabel;
    btnAdicionarPessoasNaLista: TButton;
    lblEscolhaCaminho: TLabel;
    lblLogin: TLabel;
    pnlVerLista: TPanel;
    lblOQVaiEditarNoListBox: TLabel;
    btnEditarNoListboxProfessores: TButton;
    lbxVisualizarDados: TListBox;
    btnEditarNoListboxEstudantes: TButton;
    btnEditarNoListboxTurmas: TButton;
    btnEditarNoListboxDisciplinas: TButton;
    btnEditarNoListboxMatriculas: TButton;
    pnlListBoxComEscolhaFeita: TPanel;

    procedure FormCreate(Sender: TObject);
    procedure btnConfirmarNomeEstudanteClick(Sender: TObject);
    procedure btnDisciplinaClick(Sender: TObject);
    procedure btnProfessorClick(Sender: TObject);
    procedure btnMostrarDadosClick(Sender: TObject);
    procedure btnConfirmarLoginClick(Sender: TObject);
    procedure btnExibirListaClick(Sender: TObject);
    procedure btnAdicionarPessoasNaListaClick(Sender: TObject);
    procedure btnEditarNoListbox(Sender: TObject);
  private
    EstudanteNome: string;
    CodigoAluno: string;
    MatriculaAluno: string;
    DisciplinaSelecionada: string;
    ProfessorSelecionado: string;
    ProximoCodigoAluno: Integer;
    ProximoCodigoTurma: Integer;
    ProximoCodigoMatricula: Integer;
    AbrirNovoItemNaLista: Boolean;
    AbrirListaAtual:Boolean;

    function ObterCaminhoArquivoDados: string;
    procedure GerarCodigoMatricula;
    procedure SalvarDadosJSON;
    function CarregarDadosJSON: TJSONObject;
    procedure ResetForm;
    function ObterCodigoDisciplina(NomeDisciplina: string): string;
    function ObterCodigoProfessor(NomeProfessor: string): string;
    function ObterCPFProfessor(NomeProfessor: string): string;
    function CriarOuObterTurma(CodigoProfessor, CodigoDisciplina: string): string;
    procedure InicializarDisciplinas(DisciplinasObj: TJSONObject);
    procedure CarregarProximosCodigos(DadosObj: TJSONObject);
    function CriarEstruturaJSONVazia: TJSONObject;
  public
    { Public declarations }
  end;

var
  FColegio_CRUD: TFColegio_CRUD;

implementation

{$R *.dfm}

// --------------------------------------------------------------------------------------------------
// Inicialização do Formulário
// --------------------------------------------------------------------------------------------------
procedure TFColegio_CRUD.FormCreate(Sender: TObject);
begin
  ProximoCodigoAluno := 1;
  ProximoCodigoTurma := 1;
  ProximoCodigoMatricula := 1;
  ResetForm;
end;
procedure TFColegio_CRUD.btnEditarNoListbox(Sender: TObject);
begin
  //vcl
    pnlListBoxComEscolhaFeita.visible:= true;
    btnEditarNoListboxDisciplinas.Visible := false;
    btnEditarNoListboxEstudantes.Visible := false;
    btnEditarNoListboxMatriculas.Visible := false;
    btnEditarNoListboxProfessores.Visible := false;
    btnEditarNoListboxTurmas.Visible := false;
    lblOQVaiEditarNoListBox.Visible := false;
    pnlVerLista.bevelouter:=bvnone;

  // if valor(string) = caption botao, then mostrar dentro do escopo {caption.text} no JSON
end;

procedure TFColegio_CRUD.btnExibirListaClick(Sender: TObject);
begin
  //? vcl
    pnlCaminhoEscolha.visible := false;
    btnExibirLista.visible := false;
    btnAdicionarPessoasNaLista.visible := false;
    lblExibirLista.visible := false;
    lblEscolhaCaminho.visible := false;
    lblAdicionarPessoasNaLista.visible := false;

  //? booleanas
    AbrirNovoItemNaLista := false;
    AbrirListaAtual := true;
end;
procedure TFColegio_CRUD.btnAdicionarPessoasNaListaClick(Sender: TObject);
begin
  pnlCaminhoEscolha.visible := false;
  AbrirNovoItemNaLista := true;
  AbrirListaAtual := false;
end;

// --------------------------------------------------------------------------------------------------
// Lógica de Login
// --------------------------------------------------------------------------------------------------


procedure TFColegio_CRUD.btnConfirmarLoginClick(Sender: TObject);
begin
  if (edtUsuario.text='admin') and (edtSenha.text='1234') then
  begin
   if AbrirNovoItemNaLista then
    begin
    pnlEscolhaDisciplina.visible := false;
    edtNomeEstudante.visible := true;
    btnConfirmarNomeEstudante.visible := true;
    edtUsuario.visible := false;
    edtSenha.visible := false;
    btnConfirmarLogin.visible := false;
    pnlLogin.visible := false;
    btnMostrarDados.visible := false;
    end
   else if AbrirListaAtual then
    begin
    pnlCaminhoEscolha.visible:= true;
    pnlVerLista.visible := true;
    end;
  end;
end;

// --------------------------------------------------------------------------------------------------
// Lógica de Confirmação do Nome do Estudante
// --------------------------------------------------------------------------------------------------
procedure TFColegio_CRUD.btnConfirmarNomeEstudanteClick(Sender: TObject);
begin
  EstudanteNome := Trim(edtNomeEstudante.Text);
  if EstudanteNome = '' then
  begin
    ShowMessage('Por favor, digite o nome do estudante.');
    Exit;
  end;

  GerarCodigoMatricula; // Gera CodigoAluno e MatriculaAluno

  // Desabilita a entrada do nome e o botão de confirmação
  edtNomeEstudante.Enabled := False;
  btnConfirmarNomeEstudante.Enabled := False;

  // Habilita a visibilidade do painel de seleção de disciplina
    pnlEscolhaDisciplina.visible := true;
    btnDisciplinaFullStack.visible := true;
    btnDisciplinaInformaticaBasica.visible := true;
    btnDisciplinaSeguranca.visible := true;
    btnDisciplinaUXDesign.visible := true;
    btnDisciplinaAnaliseData.visible := true;
    btnDisciplinaEtica.visible := true;
    lblEscolhaDisciplina.visible := true;
end;

// --------------------------------------------------------------------------------------------------
// Lógica do Botão de Disciplina
// --------------------------------------------------------------------------------------------------
procedure TFColegio_CRUD.btnDisciplinaClick(Sender: TObject);
begin
  // A propriedade Caption do botão clicado contém o nome da disciplina
  DisciplinaSelecionada := (Sender as TButton).Caption;

  // Habilita a visibilidade do painel de seleção de professor
  lblEscolhaProfessor.Visible := True;
  pnlEscolhaProfessor.Visible := True;
  btnProfessorFlavio.Visible := True;
  btnProfessorFausto.Visible := True;

  // Reabilita todos os botões de professor antes de aplicar as regras específicas
  btnProfessorFlavio.Enabled := True;
  btnProfessorFausto.Enabled := True;

  // Aplica as regras de desabilitação de professor com base na disciplina selecionada
  if (DisciplinaSelecionada = 'Informática básica') or
    (DisciplinaSelecionada = 'FullStack') or
    (DisciplinaSelecionada = 'UX Design') or
    (DisciplinaSelecionada = 'Segurança da informação') then
  begin
    // Se estas disciplinas forem escolhidas, Fausto é desabilitado
    btnProfessorFausto.Enabled := False;
  end
  else if (DisciplinaSelecionada = 'Análise de Dados') or
    (DisciplinaSelecionada = 'Ética de Dados') then
  begin
    // Se estas disciplinas forem escolhidas, Flavio é desabilitado
    btnProfessorFlavio.Enabled := False;
  end;
  // Para outras disciplinas (se houver), ambos os professores permanecem habilitados
end;

// --------------------------------------------------------------------------------------------------
// Lógica do Botão de Professor (completa o cadastro)
// --------------------------------------------------------------------------------------------------
procedure TFColegio_CRUD.btnProfessorClick(Sender: TObject);
var
  TurmaNome: string;
  Mensagem: string;
  LetraTurma: string;
begin
  ProfessorSelecionado := (Sender as TButton).Caption;

  // Extrai a letra do prefixo da matrícula (ex: MA# -> 'A')
  if (Length(MatriculaAluno) >= 3) then
    LetraTurma := MatriculaAluno[2]
  else
    LetraTurma := 'A';

  // TurmaNome no formato "1A" (1º período, turma A)
  TurmaNome := '1' + LetraTurma;

  // Salva as informações em JSON usando a nova estrutura normalizada
  SalvarDadosJSON;

  // Constrói e exibe a mensagem de confirmação após salvar
  Mensagem :=
    Format('%s foi cadastrado com sucesso! Segue a frente as informacoes especificas: '
    + 'Matricula: %s, Disciplina: %s, Professor: %s, Turma: %s',
    [EstudanteNome, MatriculaAluno, DisciplinaSelecionada, ProfessorSelecionado,
    TurmaNome]);
  ShowMessage(Mensagem);

  // Reseta o formulário para um novo cadastro
  ResetForm;
end;

// --------------------------------------------------------------------------------------------------
// Lógica do Botão Mostrar Dados - Atualizado com melhor tratamento de erros
// --------------------------------------------------------------------------------------------------
procedure TFColegio_CRUD.btnMostrarDadosClick(Sender: TObject);
var
  DadosGerais: TJSONObject;
  EstudantesObj, ProfessoresObj, DisciplinasObj, TurmasObj, MatriculasObj: TJSONObject;
  Pair: TJSONPair;
  ItemObj: TJSONObject;
  Mensagem: string;
begin
  // Carrega todos os dados com estrutura garantida
  DadosGerais := CarregarDadosJSON;

  try
    Mensagem := 'Dados Carregados (Estrutura Normalizada):' + sLineBreak;

    // Mostra os Estudantes - garantido de existir agora
    EstudantesObj := DadosGerais.GetValue<TJSONObject>('estudantes');
    Mensagem := Mensagem + sLineBreak + '--- ESTUDANTES ---' + sLineBreak;
    for Pair in EstudantesObj do
    begin
      ItemObj := Pair.JsonValue as TJSONObject;
      if Assigned(ItemObj) then
      begin
        Mensagem := Mensagem +
          Format('Codigo: %s, Nome: %s',
          [Pair.JsonString.Value, ItemObj.GetValue<string>('nome')]) + sLineBreak;
      end;
    end;

    // Mostra os Professores - garantido de existir agora
    ProfessoresObj := DadosGerais.GetValue<TJSONObject>('professores');
    Mensagem := Mensagem + sLineBreak + '--- PROFESSORES ---' + sLineBreak;
    for Pair in ProfessoresObj do
    begin
      ItemObj := Pair.JsonValue as TJSONObject;
      if Assigned(ItemObj) then
      begin
        Mensagem := Mensagem + Format('Codigo: %s, Nome: %s, CPF: %s',
          [Pair.JsonString.Value,
          ItemObj.GetValue<string>('nome'),
          ItemObj.GetValue<string>('cpf')]) + sLineBreak;
      end;
    end;

    // Mostra as Disciplinas - garantido de existir agora
    DisciplinasObj := DadosGerais.GetValue<TJSONObject>('disciplinas');
    Mensagem := Mensagem + sLineBreak + '--- DISCIPLINAS ---' + sLineBreak;
    for Pair in DisciplinasObj do
    begin
      ItemObj := Pair.JsonValue as TJSONObject;
      if Assigned(ItemObj) then
      begin
        Mensagem := Mensagem + Format('Codigo: %s, Nome: %s',
          [Pair.JsonString.Value,
          ItemObj.GetValue<string>('nome')]) + sLineBreak;
      end;
    end;

    // Mostra as Turmas - garantido de existir agora
    TurmasObj := DadosGerais.GetValue<TJSONObject>('turmas');
    Mensagem := Mensagem + sLineBreak + '--- TURMAS ---' + sLineBreak;
    for Pair in TurmasObj do
    begin
      ItemObj := Pair.JsonValue as TJSONObject;
      if Assigned(ItemObj) then
      begin
        Mensagem := Mensagem + Format('Codigo: %s, Professor: %s, Disciplina: %s',
          [Pair.JsonString.Value,
          ItemObj.GetValue<string>('codigo_professor'),
          ItemObj.GetValue<string>('codigo_disciplina')]) + sLineBreak;
      end;
    end;

    // Mostra as Matrículas - garantido de existir agora
    MatriculasObj := DadosGerais.GetValue<TJSONObject>('matriculas');
    Mensagem := Mensagem + sLineBreak + '--- MATRICULAS ---' + sLineBreak;
    for Pair in MatriculasObj do
    begin
      ItemObj := Pair.JsonValue as TJSONObject;
      if Assigned(ItemObj) then
      begin
        Mensagem := Mensagem + Format('Codigo: %s, Turma: %s, Estudante: %s',
          [Pair.JsonString.Value,
          ItemObj.GetValue<string>('codigo_turma'),
          ItemObj.GetValue<string>('codigo_estudante')]) + sLineBreak;
      end;
    end;

    ShowMessage(Mensagem);
  finally
    DadosGerais.Free; // Libera a memória do objeto JSON
  end;
end;

// --------------------------------------------------------------------------------------------------
// NOVO: Criar Estrutura JSON Vazia - Esta é a correção principal
// --------------------------------------------------------------------------------------------------
function TFColegio_CRUD.CriarEstruturaJSONVazia: TJSONObject;
var
  DisciplinasObj: TJSONObject;
begin
  // Cria o objeto JSON raiz com todas as seções necessárias
  Result := TJSONObject.Create;

  // Cria todas as seções principais como objetos vazios
  Result.AddPair('estudantes', TJSONObject.Create);
  Result.AddPair('professores', TJSONObject.Create);

  // Cria a seção de disciplinas e a inicializa com todas as disciplinas disponíveis
  DisciplinasObj := TJSONObject.Create;
  InicializarDisciplinas(DisciplinasObj);
  Result.AddPair('disciplinas', DisciplinasObj);

  Result.AddPair('turmas', TJSONObject.Create);
  Result.AddPair('matriculas', TJSONObject.Create);
end;

// --------------------------------------------------------------------------------------------------
// Funções de Ajuda
// --------------------------------------------------------------------------------------------------
function TFColegio_CRUD.ObterCaminhoArquivoDados: string;
begin
  Result := TPath.Combine(TPath.GetHomePath, 'JSONCRUD', 'data.json');
end;

// Mapeia nomes de disciplinas para códigos padronizados
function TFColegio_CRUD.ObterCodigoDisciplina(NomeDisciplina: string): string;
begin
  if NomeDisciplina = 'Informatica basica' then
    Result := 'DISC01'
  else if NomeDisciplina = 'FullStack' then
    Result := 'DISC02'
  else if NomeDisciplina = 'UX Design' then
    Result := 'DISC03'
  else if NomeDisciplina = 'Seguranca da informacao' then
    Result := 'DISC04'
  else if NomeDisciplina = 'Analise de Dados' then
    Result := 'DISC05'
  else if NomeDisciplina = 'Etica de Dados' then
    Result := 'DISC06'
  else
    Result := 'DISC99'; // Disciplina desconhecida
end;

// Mapeia nomes de professores para códigos padronizados
function TFColegio_CRUD.ObterCodigoProfessor(NomeProfessor: string): string;
begin
  if NomeProfessor = 'Flavio' then
    Result := 'PROF01'
  else if NomeProfessor = 'Fausto' then
    Result := 'PROF02'
  else
    Result := 'PROF99'; // Professor desconhecido
end;

// Mapeia nomes de professores para seus CPFs
function TFColegio_CRUD.ObterCPFProfessor(NomeProfessor: string): string;
begin
  if NomeProfessor = 'Flavio' then
    Result := '123.456.789-00'
  else if NomeProfessor = 'Fausto' then
    Result := '987.654.321-00'
  else
    Result := '000.000.000-00'; // Professor desconhecido
end;

procedure TFColegio_CRUD.GerarCodigoMatricula;
var
  AnoAtual: Integer;
  Semestre: string;
  Prefixos: array [0 .. 3] of string;
  PrefixoEscolhido: string;
begin
  AnoAtual := YearOf(Date);
  if MonthOf(Date) < 7 then
    Semestre := '01'
  else
    Semestre := '02';

  // Array de prefixos possíveis
  Prefixos[0] := 'MA#';
  Prefixos[1] := 'MB#';
  Prefixos[2] := 'MC#';
  Prefixos[3] := 'MD#';

  // Inicializa o gerador de números aleatórios
  Randomize;

  // Escolhe um prefixo aleatório
  PrefixoEscolhido := Prefixos[Random(4)];

  CodigoAluno := Format('%d%s%.4d', [AnoAtual, Semestre, ProximoCodigoAluno]);
  MatriculaAluno := PrefixoEscolhido + CodigoAluno;

  Inc(ProximoCodigoAluno);
end;

// Cria ou encontra uma turma existente para a combinação professor+disciplina
function TFColegio_CRUD.CriarOuObterTurma(CodigoProfessor, CodigoDisciplina: string): string;
var
  DadosObj: TJSONObject;
  TurmasObj: TJSONObject;
  TurmaPair: TJSONPair;
  TurmaObj: TJSONObject;
begin
  Result := '';
  DadosObj := CarregarDadosJSON; // Isso agora garante uma estrutura válida

  try
    // Obtém a seção de turmas - garantido de existir agora
    TurmasObj := DadosObj.GetValue<TJSONObject>('turmas');

    // Procura por uma turma existente com esta combinação professor+disciplina
    for TurmaPair in TurmasObj do
    begin
      TurmaObj := TurmaPair.JsonValue as TJSONObject;
      if Assigned(TurmaObj) and
        (TurmaObj.GetValue<string>('codigo_professor') = CodigoProfessor) and
        (TurmaObj.GetValue<string>('codigo_disciplina') = CodigoDisciplina) then
      begin
        Result := TurmaPair.JsonString.Value;
        Exit; // Encontrou uma turma existente
      end;
    end;

    // Se nenhuma turma existente for encontrada, cria um novo código
    CarregarProximosCodigos(DadosObj);
    Result := Format('TURMA%.3d', [ProximoCodigoTurma]);
    Inc(ProximoCodigoTurma);
  finally
    DadosObj.Free;
  end;
end;

// Inicializa as disciplinas na estrutura JSON
procedure TFColegio_CRUD.InicializarDisciplinas(DisciplinasObj: TJSONObject);
var
  DisciplinaObj: TJSONObject;
begin
  // Apenas adiciona disciplinas se elas ainda não existirem
  if DisciplinasObj.FindValue('DISC01') = nil then
  begin
    DisciplinaObj := TJSONObject.Create;
    DisciplinaObj.AddPair('nome', 'Informatica basica');
    DisciplinasObj.AddPair('DISC01', DisciplinaObj);
  end;

  if DisciplinasObj.FindValue('DISC02') = nil then
  begin
    DisciplinaObj := TJSONObject.Create;
    DisciplinaObj.AddPair('nome', 'FullStack');
    DisciplinasObj.AddPair('DISC02', DisciplinaObj);
  end;

  if DisciplinasObj.FindValue('DISC03') = nil then
  begin
    DisciplinaObj := TJSONObject.Create;
    DisciplinaObj.AddPair('nome', 'UX Design');
    DisciplinasObj.AddPair('DISC03', DisciplinaObj);
  end;

  if DisciplinasObj.FindValue('DISC04') = nil then
  begin
    DisciplinaObj := TJSONObject.Create;
    DisciplinaObj.AddPair('nome', 'Seguranca da informacao');
    DisciplinasObj.AddPair('DISC04', DisciplinaObj);
  end;

  if DisciplinasObj.FindValue('DISC05') = nil then
  begin
    DisciplinaObj := TJSONObject.Create;
    DisciplinaObj.AddPair('nome', 'Analise de Dados');
    DisciplinasObj.AddPair('DISC05', DisciplinaObj);
  end;

  if DisciplinasObj.FindValue('DISC06') = nil then
  begin
    DisciplinaObj := TJSONObject.Create;
    DisciplinaObj.AddPair('nome', 'Etica de Dados');
    DisciplinasObj.AddPair('DISC06', DisciplinaObj);
  end;
end;

// Carrega os próximos códigos disponíveis a partir dos dados existentes para manter a sequência
procedure TFColegio_CRUD.CarregarProximosCodigos(DadosObj: TJSONObject);
var
  EstudantesObj, TurmasObj, MatriculasObj: TJSONObject;
  Pair: TJSONPair;
  MaxEstudante, MaxTurma, MaxMatricula: Integer;
  CodNumStr: string;
  CodNum: Integer;
begin
  MaxEstudante := 0;
  MaxTurma := 0;
  MaxMatricula := 0;

  // Obtém a seção de estudantes - garantido de existir agora
  EstudantesObj := DadosObj.GetValue<TJSONObject>('estudantes');
  for Pair in EstudantesObj do
  begin
    if TryStrToInt(Pair.JsonString.Value, CodNum) then
      if CodNum > MaxEstudante then
        MaxEstudante := CodNum;
  end;

  // Obtém a seção de turmas - garantido de existir agora
  TurmasObj := DadosObj.GetValue<TJSONObject>('turmas');
  for Pair in TurmasObj do
  begin
    CodNumStr := StringReplace(Pair.JsonString.Value, 'TURMA', '', []);
    if TryStrToInt(CodNumStr, CodNum) then
      if CodNum > MaxTurma then
        MaxTurma := CodNum;
  end;

  // Obtém a seção de matrículas - garantido de existir agora
  MatriculasObj := DadosObj.GetValue<TJSONObject>('matriculas');
  for Pair in MatriculasObj do
  begin
    CodNumStr := StringReplace(Pair.JsonString.Value, 'MAT', '', []);
    if TryStrToInt(CodNumStr, CodNum) then
      if CodNum > MaxMatricula then
        MaxMatricula := CodNum;
  end;

  // Define os próximos códigos para serem um a mais que o máximo encontrado
  if MaxEstudante > 0 then
  begin
    // Extrai apenas a parte sequencial dos códigos de estudante como "2025010001"
    CodNumStr := Copy(IntToStr(MaxEstudante), 8, 4); // Obtém os últimos 4 dígitos
    if TryStrToInt(CodNumStr, CodNum) then
      ProximoCodigoAluno := CodNum + 1;
  end;

  ProximoCodigoTurma := MaxTurma + 1;
  ProximoCodigoMatricula := MaxMatricula + 1;
end;

// --------------------------------------------------------------------------------------------------
// Função Principal de Salvamento - Simplificada com estrutura garantida
// --------------------------------------------------------------------------------------------------
procedure TFColegio_CRUD.SalvarDadosJSON;
var
  CaminhoDados: string;
  FileName: string;
  RootObj: TJSONObject;
  EstudantesObj, ProfessoresObj, TurmasObj, MatriculasObj: TJSONObject;
  NovoEstudanteObj, NovoProfessorObj, NovaTurmaObj, NovaMatriculaObj: TJSONObject;
  CodigoProfessor, CodigoDisciplina, CodigoTurma, CodigoMatriculaFinal: string;
  ProfessorCPF: string;
  JsonString: string;
begin
  // Obtém códigos padronizados para professor e disciplina
  CodigoProfessor := ObterCodigoProfessor(ProfessorSelecionado);
  CodigoDisciplina := ObterCodigoDisciplina(DisciplinaSelecionada);
  ProfessorCPF := ObterCPFProfessor(ProfessorSelecionado);

  // Garante que o diretório 'JSONCRUD' exista
  CaminhoDados := TPath.Combine(TPath.GetHomePath, 'JSONCRUD');
  if not TDirectory.Exists(CaminhoDados) then
  begin
    try
      TDirectory.CreateDirectory(CaminhoDados);
    except
      on E: Exception do
      begin
        ShowMessage('Erro ao criar diretorio: ' + E.Message);
        Exit;
      end;
    end;
  end;

  FileName := ObterCaminhoArquivoDados;

  // Carrega os dados existentes - agora garantido de ter a estrutura correta
  RootObj := CarregarDadosJSON;

  try
    // Obtém os objetos existentes - garantido de existirem agora
    EstudantesObj := RootObj.GetValue<TJSONObject>('estudantes');
    ProfessoresObj := RootObj.GetValue<TJSONObject>('professores');
    TurmasObj := RootObj.GetValue<TJSONObject>('turmas');
    MatriculasObj := RootObj.GetValue<TJSONObject>('matriculas');

    // Carrega os próximos códigos disponíveis
    CarregarProximosCodigos(RootObj);

    // 1. Cria/Atualiza Estudante
    NovoEstudanteObj := TJSONObject.Create;
    NovoEstudanteObj.AddPair('nome', EstudanteNome);
    EstudantesObj.AddPair(CodigoAluno, NovoEstudanteObj);

    // 2. Cria/Atualiza Professor (se não existir)
    if ProfessoresObj.FindValue(CodigoProfessor) = nil then
    begin
      NovoProfessorObj := TJSONObject.Create;
      NovoProfessorObj.AddPair('nome', ProfessorSelecionado);
      NovoProfessorObj.AddPair('cpf', ProfessorCPF);
      ProfessoresObj.AddPair(CodigoProfessor, NovoProfessorObj);
    end;

    // 3. Cria/Obtém Turma - vincula professor à disciplina
    CodigoTurma := CriarOuObterTurma(CodigoProfessor, CodigoDisciplina);
    if TurmasObj.FindValue(CodigoTurma) = nil then
    begin
      NovaTurmaObj := TJSONObject.Create;
      NovaTurmaObj.AddPair('codigo_professor', CodigoProfessor);
      NovaTurmaObj.AddPair('codigo_disciplina', CodigoDisciplina);
      TurmasObj.AddPair(CodigoTurma, NovaTurmaObj);
    end;

    // 4. Cria Matrícula - vincula estudante à turma
    CodigoMatriculaFinal := Format('MAT%.3d', [ProximoCodigoMatricula]);
    NovaMatriculaObj := TJSONObject.Create;
    NovaMatriculaObj.AddPair('codigo_turma', CodigoTurma);
    NovaMatriculaObj.AddPair('codigo_estudante', CodigoAluno);
    MatriculasObj.AddPair(CodigoMatriculaFinal, NovaMatriculaObj);

    Inc(ProximoCodigoMatricula);

    // Converte o objeto JSON para uma string formatada
    JsonString := RootObj.Format;

    // Salva o JSON formatado no arquivo
    TFile.WriteAllText(FileName, JsonString, TEncoding.UTF8);

  finally
    RootObj.Free; // Libera a memória do objeto JSON
  end;
end;

// --------------------------------------------------------------------------------------------------
// ATUALIZADA Função de Carregamento de Dados JSON - Esta é a correção principal
// --------------------------------------------------------------------------------------------------
function TFColegio_CRUD.CarregarDadosJSON: TJSONObject;
var
  FileName: string;
  JsonString: string;
begin
  FileName := ObterCaminhoArquivoDados;

  if TFile.Exists(FileName) then
  begin
    try
      // Tenta carregar o arquivo JSON existente
      JsonString := TFile.ReadAllText(FileName, TEncoding.UTF8);
      Result := TJSONObject.ParseJSONValue(JsonString) as TJSONObject;

      // Verifica se o JSON carregado tem a estrutura necessária
      if not Assigned(Result) then
      begin
        // Se a análise falhou, cria uma estrutura vazia
        Result := CriarEstruturaJSONVazia;
      end
      else
      begin
        // Verifica se todas as seções necessárias existem, cria se estiverem faltando
        if not Assigned(Result.GetValue('estudantes')) then
          Result.AddPair('estudantes', TJSONObject.Create);
        if not Assigned(Result.GetValue('professores')) then
          Result.AddPair('professores', TJSONObject.Create);
        if not Assigned(Result.GetValue('disciplinas')) then
        begin
          var DisciplinasObj := TJSONObject.Create;
          InicializarDisciplinas(DisciplinasObj);
          Result.AddPair('disciplinas', DisciplinasObj);
        end;
        if not Assigned(Result.GetValue('turmas')) then
          Result.AddPair('turmas', TJSONObject.Create);
        if not Assigned(Result.GetValue('matriculas')) then
          Result.AddPair('matriculas', TJSONObject.Create);
      end;
    except
      on E: Exception do
      begin
        ShowMessage('Erro ao carregar dados do JSON: ' + E.Message + '. Criando estrutura nova.');
        // Se houver qualquer erro no carregamento, cria uma nova estrutura
        Result := CriarEstruturaJSONVazia;
      end;
    end;
  end
  else
  begin
    // Se o arquivo não existir, cria a estrutura vazia com todas as seções necessárias
    Result := CriarEstruturaJSONVazia;
  end;
end;

// --------------------------------------------------------------------------------------------------
// Função de Reset do Formulário
// --------------------------------------------------------------------------------------------------
procedure TFColegio_CRUD.ResetForm;
begin
  // Limpa os campos e esconde os painéis de seleção
  edtNomeEstudante.Text := '';
  EstudanteNome := ''; // Limpa o estado interno do nome do estudante
  CodigoAluno := '';
  MatriculaAluno := '';
  DisciplinaSelecionada := '';
  ProfessorSelecionado := '';

  edtNomeEstudante.Enabled := True;
  btnConfirmarNomeEstudante.Enabled := True; // Habilita para novo cadastro

  // Esconde os painéis e rótulos de seleção
  pnlEscolhaDisciplina.Visible := False;
  pnlEscolhaProfessor.Visible := False;
  lblEscolhaDisciplina.Visible := False;
  lblEscolhaProfessor.Visible := False;

  // Garante que todos os botões de disciplina e professor estejam habilitados para um novo ciclo
  btnDisciplinaInformaticaBasica.Enabled := True;
  btnDisciplinaFullStack.Enabled := True;
  btnDisciplinaUXDesign.Enabled := True;
  btnDisciplinaSeguranca.Enabled := True;
  btnDisciplinaAnaliseData.Enabled := True;
  btnDisciplinaEtica.Enabled := True;

  btnProfessorFlavio.Enabled := True;
  btnProfessorFausto.Enabled := True;

  edtNomeEstudante.SetFocus; // Define o foco para o campo de nome do estudante
end;

procedure CarregarDadosDaLista;
var
  JsonFileContent: TStringList;
  JsonValue: TJSONValue;
  JsonArray: TJSONArray;
  JsonObject: TJSONObject;
  I: Integer;
  Nome, Email: string;
begin
  // Limpar a lista, se necessário
//  ListBox1.Clear;

  // 1. Ler o conteúdo do arquivo JSON
  try
    JsonFileContent := TStringList.Create;
    try
      // Lê o arquivo para um TStringList
      JsonFileContent.LoadFromFile('C:\caminho\para\seu\dados.json');

      // 2. Fazer o parsing do JSON
      // Converte o conteúdo para um TJSONValue
      JsonValue := TJSONObject.ParseJSONValue(JsonFileContent.Text);

      // 3. Acessar o array JSON
      if (JsonValue <> nil) and (JsonValue is TJSONArray) then
      begin
        JsonArray := JsonValue as TJSONArray;

        // 4. Iterar sobre os objetos do array
        for I := 0 to JsonArray.Count - 1 do
        begin
          JsonObject := JsonArray.Items[I] as TJSONObject;

          if JsonObject <> nil then
          begin
            // 5. Obter os valores de cada objeto
            // Tenta obter o valor da chave 'nome'
            if JsonObject.TryGetValue<string>('nome', Nome) then
            begin
              // Tenta obter o valor da chave 'email'
              if JsonObject.TryGetValue<string>('email', Email) then
              begin
                // 6. Adicionar os dados na lista (ListBox)
//                ListBox1.Items.Add(Format('Nome: %s, Email: %s', [Nome, Email]));
              end;
            end;
          end;
        end;
      end;
    finally
      // Liberar a memória do JsonFileContent
      JsonFileContent.Free;
      // Liberar a memória do JsonValue
      // O TJSONObject.ParseJSONValue retorna uma instância que precisa ser liberada
      JsonValue.Free;
    end;
  except
    on E: Exception do
    begin
      // Tratar erros, como arquivo não encontrado ou JSON inválido
      ShowMessage('Ocorreu um erro: ' + E.Message);
    end;
  end;
end;

end.

