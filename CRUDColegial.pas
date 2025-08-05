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
    ProximoCodigoAluno: Integer; // Movido para a se��o private da classe

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
// Inicializa��o do Formul�rio
// --------------------------------------------------------------------------------------------------
procedure TFColegio_CRUD.FormCreate(Sender: TObject);
begin
  ProximoCodigoAluno := 1; // Inicializa o contador de c�digo do aluno
  ResetForm;
end;

// --------------------------------------------------------------------------------------------------
// L�gica para o bot�o de confirma��o do nome do estudante
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

  // Desabilita a entrada de nome e o bot�o de confirma��o
  edtNomeEstudante.Enabled := False;
  btnConfirmarNomeEstudante.Enabled := False;

  // Habilita a visibilidade do painel de escolha de disciplina
  lblEscolhaDisciplina.Visible := True;
  pnlEscolhaDisciplina.Visible := True;
end;

// --------------------------------------------------------------------------------------------------
// L�gica para os bot�es de disciplina
// --------------------------------------------------------------------------------------------------
procedure TFColegio_CRUD.btnDisciplinaClick(Sender: TObject);
begin
  // A propriedade Caption do bot�o clicado cont�m o nome da disciplina
  DisciplinaSelecionada := (Sender as TButton).Caption;

  // Habilita a visibilidade do painel e do label de professor
  lblEscolhaProfessor.Visible := True;
  pnlEscolhaProfessor.Visible := True;
  btnProfessorFlavio.Visible := True;
  btnProfessorFausto.Visible := True;

  // Reabilita todos os bot�es de professor antes de aplicar as regras espec�ficas
  btnProfessorFlavio.Enabled := True;
  btnProfessorFausto.Enabled := True;

  // Aplica a regra de desabilita��o de professor com base na disciplina selecionada
  if (DisciplinaSelecionada = 'Informatica basica') or
     (DisciplinaSelecionada = 'FullStack') or
     (DisciplinaSelecionada = 'UX Design') or
     (DisciplinaSelecionada = 'Seguran�a da informa��o') then
  begin
    // Se estas disciplinas forem escolhidas, Fausto � desabilitado
    btnProfessorFausto.Enabled := False;
  end
  else if (DisciplinaSelecionada = 'Analise de Dados') or
          (DisciplinaSelecionada = '�tica de Dados') then
  begin
    // Se estas disciplinas forem escolhidas, Flavio � desabilitado
    btnProfessorFlavio.Enabled := False;
  end;
  // Para outras disciplinas (se houver), ambos os professores permanecer�o habilitados
end;

// --------------------------------------------------------------------------------------------------
// L�gica para os bot�es de professor (finaliza o cadastro)
// --------------------------------------------------------------------------------------------------
procedure TFColegio_CRUD.btnProfessorClick(Sender: TObject);
var
  TurmaNome: string;
  Mensagem: string;
begin
  ProfessorSelecionado := (Sender as TButton).Caption;

  // L�gica de turma simplificada (pode ser expandida se necess�rio)
  TurmaNome := 'T001';

  // Salva as informa��es em JSON
  SalvarAlunoJSON;

  // Constr�i e exibe a mensagem de confirma��o ap�s o salvamento
  Mensagem :=
    Format('%s foi cadastrado com sucesso! Segue a frente as informa��es especificas: '
    + 'Matricula: %s, Disciplina: %s, Professor: %s, Turma: %s',
    [EstudanteNome, MatriculaAluno, DisciplinaSelecionada, ProfessorSelecionado,
    TurmaNome]);
  ShowMessage(Mensagem);

  // Reseta o formul�rio para um novo cadastro
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

  // O formato %.4d garante que o n�mero tenha 4 d�gitos, preenchendo com zeros � esquerda se necess�rio
  CodigoAluno := Format('%d%s%.4d', [AnoAtual, Semestre, ProximoCodigoAluno]);
  MatriculaAluno := 'MA#' + CodigoAluno;

  Inc(ProximoCodigoAluno); // Incrementa para o pr�ximo aluno
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
  // Limpa os campos e esconde os pain�is de sele��o
  edtNomeEstudante.Text := '';
  EstudanteNome := ''; // Limpa o estado interno do nome do estudante
  CodigoAluno := '';
  MatriculaAluno := '';
  DisciplinaSelecionada := '';
  ProfessorSelecionado := '';

  edtNomeEstudante.Enabled := True;
  btnConfirmarNomeEstudante.Enabled := True; // Habilita para um novo cadastro

  // Oculta os pain�is e labels de sele��o
  pnlEscolhaDisciplina.Visible := False;
  pnlEscolhaProfessor.Visible := False;
  lblEscolhaDisciplina.Visible := False;
  lblEscolhaProfessor.Visible := False;

  // Garante que todos os bot�es de disciplina e professor estejam habilitados para um novo ciclo
  btnDisciplinaInformaticaBasica.Enabled := True;
  btnDisciplinaFullStack.Enabled := True;
  btnDisciplinaUXDesign.Enabled := True;
  btnDisciplinaSeguranca.Enabled := True;
  // Adicione aqui outros bot�es de disciplina se houver

  btnProfessorFlavio.Enabled := True;
  btnProfessorFausto.Enabled := True;

  edtNomeEstudante.SetFocus; // Coloca o foco no campo de nome do estudante
end;

end.
