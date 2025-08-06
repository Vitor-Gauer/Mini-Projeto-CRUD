unit CRUDColegial;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Mask, Vcl.ExtCtrls,
  System.JSON, System.IOUtils, System.DateUtils,
  System.JSON.Writers, System.JSON.Readers; // Added missing JSON units

type
  TFColegio_CRUD = class(TForm)
    pnlEscolhaDisciplina: TPanel;
    pnlEscolhaProfessor: TPanel;
    lblEscolhaDisciplina: TLabel;
    lblEscolhaProfessor: TLabel;
    edtNomeEstudante: TEdit;
    btnConfirmarNomeEstudante: TButton;
    btnDisciplinaFullStack: TButton;
    btnDisciplinaUXDesign: TButton;
    btnDisciplinaSeguranca: TButton;
    btnProfessorFlavio: TButton;
    btnProfessorFausto: TButton;
    btnMostrarDados: TButton;
    btnDisciplinaInformaticaBasica: TButton; // Novo botao para demonstrar o resgate de dados

    procedure FormCreate(Sender: TObject);
    procedure btnConfirmarNomeEstudanteClick(Sender: TObject);
    procedure btnDisciplinaClick(Sender: TObject);
    procedure btnProfessorClick(Sender: TObject);
    procedure btnMostrarDadosClick(Sender: TObject); // Evento para o novo botao
  private
    EstudanteNome: string;
    CodigoAluno: string;
    MatriculaAluno: string;
    DisciplinaSelecionada: string;
    ProfessorSelecionado: string;
    ProximoCodigoAluno: Integer;

    function ObterCaminhoArquivoDados: string;
    // Funcao para obter o caminho do arquivo JSON
    procedure GerarCodigoMatricula;
    procedure SalvarDadosJSON; // Renomeado para refletir o novo proposito
    function CarregarDadosJSON: TJSONObject;
    // Nova funcao para carregar todos os dados
    procedure ResetForm;
  public
    { Public declarations }
  end;

var
  FColegio_CRUD: TFColegio_CRUD;

implementation

{$R *.dfm}

// --------------------------------------------------------------------------------------------------
// Inicializacao do Formulario
// --------------------------------------------------------------------------------------------------
procedure TFColegio_CRUD.FormCreate(Sender: TObject);
begin
  ProximoCodigoAluno := 1; // Inicializa o contador de codigo do aluno
  ResetForm;
end;

// --------------------------------------------------------------------------------------------------
// Logica para o botao de confirmacao do nome do estudante
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

  // Desabilita a entrada de nome e o botao de confirmacao
  edtNomeEstudante.Enabled := False;
  btnConfirmarNomeEstudante.Enabled := False;

  // Habilita a visibilidade do painel de escolha de disciplina
  lblEscolhaDisciplina.Visible := True;
  pnlEscolhaDisciplina.Visible := True;
end;

// --------------------------------------------------------------------------------------------------
// Logica para os botoes de disciplina
// --------------------------------------------------------------------------------------------------
procedure TFColegio_CRUD.btnDisciplinaClick(Sender: TObject);
begin
  // A propriedade Caption do botao clicado contem o nome da disciplina
  DisciplinaSelecionada := (Sender as TButton).Caption;

  // Habilita a visibilidade do painel e do label de professor
  lblEscolhaProfessor.Visible := True;
  pnlEscolhaProfessor.Visible := True;
  btnProfessorFlavio.Visible := True;
  btnProfessorFausto.Visible := True;

  // Reabilita todos os botoes de professor antes de aplicar as regras especificas
  btnProfessorFlavio.Enabled := True;
  btnProfessorFausto.Enabled := True;

  // Aplica a regra de desabilitacao de professor com base na disciplina selecionada
  if (DisciplinaSelecionada = 'Informatica basica') or
    (DisciplinaSelecionada = 'FullStack') or
    (DisciplinaSelecionada = 'UX Design') or
    (DisciplinaSelecionada = 'Seguranca da informacao') then
  begin
    // Se estas disciplinas forem escolhidas, Fausto e desabilitado
    btnProfessorFausto.Enabled := False;
  end
  else if (DisciplinaSelecionada = 'Analise de Dados') or
    (DisciplinaSelecionada = 'Etica de Dados') then
  begin
    // Se estas disciplinas forem escolhidas, Flavio e desabilitado
    btnProfessorFlavio.Enabled := False;
  end;
  // Para outras disciplinas (se houver), ambos os professores permanecerao habilitados
end;

// --------------------------------------------------------------------------------------------------
// Logica para os botoes de professor (finaliza o cadastro)
// --------------------------------------------------------------------------------------------------
procedure TFColegio_CRUD.btnProfessorClick(Sender: TObject);
var
  TurmaNome: string;
  Mensagem: string;
  LetraTurma: string;
begin
  ProfessorSelecionado := (Sender as TButton).Caption;

  // Extrai a letra do prefixo da matricula (ex: MA# -> 'A')
  if (Length(MatriculaAluno) >= 3) then
    LetraTurma := MatriculaAluno[2]
  else
    LetraTurma := 'A';

  // TurmaNome no formato "1A" (1o periodo, turma A)
  TurmaNome := '1' + LetraTurma;

  // Salva as informacoes em JSON usando a nova estrutura
  SalvarDadosJSON;

  // Constroi e exibe a mensagem de confirmacao apos o salvamento
  Mensagem :=
    Format('%s foi cadastrado com sucesso! Segue a frente as informacoes especificas: '
    + 'Matricula: %s, Disciplina: %s, Professor: %s, Turma: %s',
    [EstudanteNome, MatriculaAluno, DisciplinaSelecionada, ProfessorSelecionado,
    TurmaNome]);
  ShowMessage(Mensagem);

  // Reseta o formulario para um novo cadastro
  ResetForm;
end;

// --------------------------------------------------------------------------------------------------
// Logica para o novo botao de mostrar dados
// --------------------------------------------------------------------------------------------------
procedure TFColegio_CRUD.btnMostrarDadosClick(Sender: TObject);
var
  DadosGerais: TJSONObject;
  AlunosObj: TJSONObject;
  ProfessoresObj: TJSONObject;
  AlunoPair: TJSONPair;
  ProfessorPair: TJSONPair;
  AlunoObj: TJSONObject;
  ProfessorObj: TJSONObject;
  Mensagem: string;
begin
  DadosGerais := CarregarDadosJSON; // Carrega todos os dados

  if Assigned(DadosGerais) then
  begin
    try
      Mensagem := 'Dados Carregados:' + sLineBreak;
      AlunosObj := DadosGerais.GetValue<TJSONObject>('alunos');
      ProfessoresObj := DadosGerais.GetValue<TJSONObject>('professores');

      if Assigned(AlunosObj) then
      begin
        Mensagem := Mensagem + sLineBreak + '--- ALUNOS ---' + sLineBreak;
        for AlunoPair in AlunosObj do
        begin
          AlunoObj := AlunoPair.JsonValue as TJSONObject;
          if Assigned(AlunoObj) then
          begin
            Mensagem := Mensagem +
              Format('Codigo: %s, Nome: %s, Matricula: %s, Disciplina: %s, Professor Codigo: %s',
              [AlunoPair.JsonString.Value, AlunoObj.GetValue<string>('nome'),
              AlunoObj.GetValue<string>('matricula'),
              AlunoObj.GetValue<string>('disciplina'),
              AlunoObj.GetValue<string>('professor_codigo')]) + sLineBreak;
          end;
        end;
      end;

      if Assigned(ProfessoresObj) then
      begin
        Mensagem := Mensagem + sLineBreak + '--- PROFESSORES ---' + sLineBreak;
        for ProfessorPair in ProfessoresObj do
        begin
          ProfessorObj := ProfessorPair.JsonValue as TJSONObject;
          if Assigned(ProfessorObj) then
          begin
            Mensagem := Mensagem + Format('Codigo: %s, Nome: %s, CPF: %s',
              [ProfessorPair.JsonString.Value,
              ProfessorObj.GetValue<string>('nome'),
              ProfessorObj.GetValue<string>('cpf')]) + sLineBreak;
          end;
        end;
      end;

      ShowMessage(Mensagem);
    finally
      DadosGerais.Free; // Libera a memoria do objeto JSON
    end;
  end
  else
  begin
    ShowMessage('Nenhum dado encontrado ou erro ao carregar o arquivo JSON.');
  end;
end;

// --------------------------------------------------------------------------------------------------
// Procedures auxiliares
// --------------------------------------------------------------------------------------------------
function TFColegio_CRUD.ObterCaminhoArquivoDados: string;
begin
  Result := TPath.Combine(TPath.GetHomePath, 'JSONCRUD', 'data.json');
end;

procedure TFColegio_CRUD.GerarCodigoMatricula;
var
  AnoAtual: Integer;
  Semestre: string;
  Prefixos: array [0 .. 3] of string;
  // Array pra fazer o prefixo aleatorio entre MA#, MB#, MC# e MD#
  PrefixoEscolhido: string;
begin
  AnoAtual := YearOf(Date);
  if MonthOf(Date) < 7 then
    Semestre := '01'
  else
    Semestre := '02';

  // Array de prefixos possiveis
  Prefixos[0] := 'MA#';
  Prefixos[1] := 'MB#';
  Prefixos[2] := 'MC#';
  Prefixos[3] := 'MD#';

  // Inicializa o gerador de numeros aleatorios (faca isso uma vez no inicio do programa, se preferir)
  Randomize;

  // Escolhe um prefixo aleatorio
  PrefixoEscolhido := Prefixos[Random(4)];

  CodigoAluno := Format('%d%s%.4d', [AnoAtual, Semestre, ProximoCodigoAluno]);
  MatriculaAluno := PrefixoEscolhido + CodigoAluno;

  Inc(ProximoCodigoAluno);
end;

procedure TFColegio_CRUD.SalvarDadosJSON;
var
  CaminhoDados: string;
  FileName: string;
  RootObj: TJSONObject;
  AlunosObj: TJSONObject;
  ProfessoresObj: TJSONObject;
  NovoAlunoObj: TJSONObject;
  NovoProfessorObj: TJSONObject;
  ProfessorCodigo: string;
  ProfessorCPF: string;
  JsonString: string;
begin
  // Determina o codigo e CPF do professor selecionado
  ProfessorCPF := '';
  ProfessorCodigo := '';

  if ProfessorSelecionado = 'Flavio' then
  begin
    ProfessorCPF := '123.456.789-00';
    ProfessorCodigo := 'PROF01';
  end
  else if ProfessorSelecionado = 'Fausto' then
  begin
    ProfessorCPF := '987.654.321-00';
    ProfessorCodigo := 'PROF02';
  end;

  // Garante que o diretorio 'JSONCRUD' exista
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
  // Obtem o caminho completo do arquivo data.json

  // Carrega os dados existentes ou cria um novo objeto JSON
  RootObj := CarregarDadosJSON;
  if not Assigned(RootObj) then
  begin
    RootObj := TJSONObject.Create;
    RootObj.AddPair('alunos', TJSONObject.Create);
    RootObj.AddPair('professores', TJSONObject.Create);
  end;

  try
    // Obtem os objetos de alunos e professores existentes
    AlunosObj := RootObj.GetValue<TJSONObject>('alunos');
    ProfessoresObj := RootObj.GetValue<TJSONObject>('professores');

    // Cria o objeto JSON para o novo aluno
    NovoAlunoObj := TJSONObject.Create;
    NovoAlunoObj.AddPair('nome', EstudanteNome);
    NovoAlunoObj.AddPair('turma', '1' + MatriculaAluno[2]);
    // Assume que a turma e 1 + a letra da matricula
    NovoAlunoObj.AddPair('matricula', MatriculaAluno);
    NovoAlunoObj.AddPair('disciplina', DisciplinaSelecionada);
    NovoAlunoObj.AddPair('professor_codigo', ProfessorCodigo);
    // Referencia ao professor

    // Adiciona o novo aluno a lista de alunos, usando CodigoAluno como chave
    AlunosObj.AddPair(CodigoAluno, NovoAlunoObj);

    // Cria o objeto JSON para o professor (se ainda nao existir)
    if ProfessoresObj.FindValue(ProfessorCodigo) = nil then
    begin
      NovoProfessorObj := TJSONObject.Create;
      NovoProfessorObj.AddPair('nome', ProfessorSelecionado);
      NovoProfessorObj.AddPair('cpf', ProfessorCPF);
      NovoProfessorObj.AddPair('codigo', ProfessorCodigo);
      ProfessoresObj.AddPair(ProfessorCodigo, NovoProfessorObj);
    end;

    // Converte o objeto JSON para string com formatacao
    JsonString := RootObj.Format;

    // Salva o JSON formatado no arquivo
    TFile.WriteAllText(FileName, JsonString, TEncoding.UTF8);
  finally
    RootObj.Free; // Libera a memoria do objeto JSON
  end;
end;

function TFColegio_CRUD.CarregarDadosJSON: TJSONObject;
var
  FileName: string;
  JsonString: string;
begin
  Result := nil; // Inicializa o resultado como nulo
  FileName := ObterCaminhoArquivoDados;

  if TFile.Exists(FileName) then
  begin
    try
      JsonString := TFile.ReadAllText(FileName, TEncoding.UTF8);
      Result := TJSONObject.ParseJSONValue(JsonString) as TJSONObject;
    except
      on E: Exception do
      begin
        ShowMessage('Erro ao carregar dados do JSON: ' + E.Message);
      end;
    end;
  end;
end;

procedure TFColegio_CRUD.ResetForm;
begin
  // Limpa os campos e esconde os paineis de selecao
  edtNomeEstudante.Text := '';
  EstudanteNome := ''; // Limpa o estado interno do nome do estudante
  CodigoAluno := '';
  MatriculaAluno := '';
  DisciplinaSelecionada := '';
  ProfessorSelecionado := '';

  edtNomeEstudante.Enabled := True;
  btnConfirmarNomeEstudante.Enabled := True; // Habilita para um novo cadastro

  // Oculta os paineis e labels de selecao
  pnlEscolhaDisciplina.Visible := False;
  pnlEscolhaProfessor.Visible := False;
  lblEscolhaDisciplina.Visible := False;
  lblEscolhaProfessor.Visible := False;

  // Garante que todos os botoes de disciplina e professor estejam habilitados para um novo ciclo
  btnDisciplinaInformaticaBasica.Enabled := True;
  btnDisciplinaFullStack.Enabled := True;
  btnDisciplinaUXDesign.Enabled := True;
  btnDisciplinaSeguranca.Enabled := True;
  // Adicione aqui outros botoes de disciplina se houver

  btnProfessorFlavio.Enabled := True;
  btnProfessorFausto.Enabled := True;

  edtNomeEstudante.SetFocus; // Coloca o foco no campo de nome do estudante
end;

end.
