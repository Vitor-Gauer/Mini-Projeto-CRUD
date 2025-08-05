unit CRUDColegial;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Mask, Vcl.ExtCtrls,
  System.JSON, System.IOUtils, System.DateUtils;

type
  TFColegio_CRUD = class(TForm)
    pnlEscolhaDisciplina: TPanel;
    pnlEscolhaProfessor: TPanel;
    lblEscolhaDisciplina: TLabel;
    lblEscolhaProfessor: TLabel;
    edtNomeEstudante: TEdit;
    btnConfirmarNomeEstudante: TButton;
    btnDisciplinaInformaticaBasica: TButton;
    btnDisciplinaFullStack: TButton;
    btnDisciplinaUXDesign: TButton;
    btnDisciplinaSeguranca: TButton;
    btnProfessorFlavio: TButton;
    btnProfessorFausto: TButton;

    procedure FormCreate(Sender: TObject);
    procedure btnConfirmarNomeEstudanteClick(Sender: TObject);
    procedure btnDisciplinaClick(Sender: TObject);
    procedure btnProfessorClick(Sender: TObject);
  private
    EstudanteNome: string;
    CodigoAluno: string;
    MatriculaAluno: string;
    DisciplinaSelecionada: string;
    ProfessorSelecionado: string;
    ProximoCodigoAluno: Integer; // Movido para a seção private da classe

    procedure GerarCodigoMatricula;
    procedure SalvarAlunoJSON;
    procedure ResetForm;
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
  ProximoCodigoAluno := 1; // Inicializa o contador de código do aluno
  ResetForm;
end;

// --------------------------------------------------------------------------------------------------
// Lógica para o botão de confirmação do nome do estudante
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

  // Desabilita a entrada de nome e o botão de confirmação
  edtNomeEstudante.Enabled := False;
  btnConfirmarNomeEstudante.Enabled := False;

  // Habilita a visibilidade do painel de escolha de disciplina
  lblEscolhaDisciplina.Visible := True;
  pnlEscolhaDisciplina.Visible := True;
end;

// --------------------------------------------------------------------------------------------------
// Lógica para os botões de disciplina
// --------------------------------------------------------------------------------------------------
procedure TFColegio_CRUD.btnDisciplinaClick(Sender: TObject);
begin
  // A propriedade Caption do botão clicado contém o nome da disciplina
  DisciplinaSelecionada := (Sender as TButton).Caption;

  // Habilita a visibilidade do painel e do label de professor
  lblEscolhaProfessor.Visible := True;
  pnlEscolhaProfessor.Visible := True;
  btnProfessorFlavio.Visible := True;
  btnProfessorFausto.Visible := True;

  // Reabilita todos os botões de professor antes de aplicar as regras específicas
  btnProfessorFlavio.Enabled := True;
  btnProfessorFausto.Enabled := True;

  // Aplica a regra de desabilitação de professor com base na disciplina selecionada
  if (DisciplinaSelecionada = 'Informatica basica') or
     (DisciplinaSelecionada = 'FullStack') or
     (DisciplinaSelecionada = 'UX Design') or
     (DisciplinaSelecionada = 'Segurança da informação') then
  begin
    // Se estas disciplinas forem escolhidas, Fausto é desabilitado
    btnProfessorFausto.Enabled := False;
  end
  else if (DisciplinaSelecionada = 'Analise de Dados') or
          (DisciplinaSelecionada = 'Ética de Dados') then
  begin
    // Se estas disciplinas forem escolhidas, Flavio é desabilitado
    btnProfessorFlavio.Enabled := False;
  end;
  // Para outras disciplinas (se houver), ambos os professores permanecerão habilitados
end;

// --------------------------------------------------------------------------------------------------
// Lógica para os botões de professor (finaliza o cadastro)
// --------------------------------------------------------------------------------------------------
procedure TFColegio_CRUD.btnProfessorClick(Sender: TObject);
var
  TurmaNome: string;
  Mensagem: string;
begin
  ProfessorSelecionado := (Sender as TButton).Caption;

  // Lógica de turma simplificada (pode ser expandida se necessário)
  TurmaNome := 'T001';

  // Salva as informações em JSON
  SalvarAlunoJSON;

  // Constrói e exibe a mensagem de confirmação após o salvamento
  Mensagem :=
    Format('%s foi cadastrado com sucesso! Segue a frente as informações especificas: '
    + 'Matricula: %s, Disciplina: %s, Professor: %s, Turma: %s',
    [EstudanteNome, MatriculaAluno, DisciplinaSelecionada, ProfessorSelecionado,
    TurmaNome]);
  ShowMessage(Mensagem);

  // Reseta o formulário para um novo cadastro
  ResetForm;
end;

// --------------------------------------------------------------------------------------------------
// Procedures auxiliares
// --------------------------------------------------------------------------------------------------
procedure TFColegio_CRUD.GerarCodigoMatricula;
var
  AnoAtual: Integer; // Declarado localmente
  Semestre: string;  // Declarado localmente
begin
  AnoAtual := YearOf(Date);
  if MonthOf(Date) < 7 then
    Semestre := '01'
  else
    Semestre := '02';

  // O formato %.4d garante que o número tenha 4 dígitos, preenchendo com zeros à esquerda se necessário
  CodigoAluno := Format('%d%s%.4d', [AnoAtual, Semestre, ProximoCodigoAluno]);
  MatriculaAluno := 'MA#' + CodigoAluno;

  Inc(ProximoCodigoAluno); // Incrementa para o próximo aluno
end;

procedure TFColegio_CRUD.SalvarAlunoJSON;
var
  JSONObj: TJSONObject;
  ProfessorCPF: string;
  ProfessorCodigo: string;
  FileName: string;
  ProfessorJSON: TJSONObject;
  CaminhoDados: string;
begin
  ShowMessage('Entering SalvarAlunoJSON procedure.');

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

  // Create objects
  JSONObj := TJSONObject.Create;

  try
    // Add pairs
    JSONObj.AddPair('nome', EstudanteNome);
    JSONObj.AddPair('codigo_aluno', CodigoAluno);
    JSONObj.AddPair('matricula', MatriculaAluno);
    JSONObj.AddPair('disciplina', DisciplinaSelecionada);

    ProfessorJSON := TJSONObject.Create;
    ProfessorJSON.AddPair('nome', ProfessorSelecionado);
    ProfessorJSON.AddPair('cpf', ProfessorCPF);
    ProfessorJSON.AddPair('codigo', ProfessorCodigo);

    JSONObj.AddPair('professor', ProfessorJSON);

    // File path logic
    CaminhoDados := TPath.Combine(TPath.GetHomePath, 'JSONCRUD');
    ShowMessage('Attempting to create directory: ' + CaminhoDados);

    if not TDirectory.Exists(CaminhoDados) then
    begin
      try
        TDirectory.CreateDirectory(CaminhoDados);
        ShowMessage('Directory created successfully: ' + CaminhoDados);
      except
        on E: Exception do
        begin
          ShowMessage('Error creating directory: ' + E.Message);
          Exit; // Exit if directory creation fails
        end;
      end;
    end
    else
    begin
      ShowMessage('Directory already exists: ' + CaminhoDados);
    end;

    FileName := TPath.Combine(CaminhoDados, 'alunos.json');
    ShowMessage('Attempting to save file to: ' + FileName);
    ShowMessage('JSON content to save: ' + JSONObj.ToString);

    // Write to file
    TFile.WriteAllText(FileName, JSONObj.ToString);
    ShowMessage('File save operation completed (no immediate error).');

  finally
    JSONObj.Free;
    ShowMessage('JSON object freed.');
  end;
end;

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
  btnConfirmarNomeEstudante.Enabled := True; // Habilita para um novo cadastro

  // Oculta os painéis e labels de seleção
  pnlEscolhaDisciplina.Visible := False;
  pnlEscolhaProfessor.Visible := False;
  lblEscolhaDisciplina.Visible := False;
  lblEscolhaProfessor.Visible := False;

  // Garante que todos os botões de disciplina e professor estejam habilitados para um novo ciclo
  btnDisciplinaInformaticaBasica.Enabled := True;
  btnDisciplinaFullStack.Enabled := True;
  btnDisciplinaUXDesign.Enabled := True;
  btnDisciplinaSeguranca.Enabled := True;
  // Adicione aqui outros botões de disciplina se houver

  btnProfessorFlavio.Enabled := True;
  btnProfessorFausto.Enabled := True;

  edtNomeEstudante.SetFocus; // Coloca o foco no campo de nome do estudante
end;

end.
