unit CRUDColegial;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Mask, Vcl.ExtCtrls, System.JSON, System.IOUtils, System.DateUtils;

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
    btnDisicplinaAnaliseData: TButton;
    btnDisciplinaEtica: TButton;
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

// Variável global para manter o número sequencial do aluno.
// Em um sistema real, isso seria persistido em um banco de dados ou arquivo.
var
  ProximoCodigoAluno: Integer = 1;

//--------------------------------------------------------------------------------------------------
// Inicialização do Formulário
//--------------------------------------------------------------------------------------------------
procedure TFColegio_CRUD.FormCreate(Sender: TObject);
begin
  ResetForm;
end;

//--------------------------------------------------------------------------------------------------
// Lógica para o botão de confirmação do nome do estudante
//--------------------------------------------------------------------------------------------------
procedure TFColegio_CRUD.btnConfirmarNomeEstudanteClick(Sender: TObject);
begin
  EstudanteNome := Trim(edtNomeEstudante.Text);
  if EstudanteNome = '' then
  begin
    ShowMessage('Por favor, digite o nome do estudante.');
    Exit;
  end;

  GerarCodigoMatricula;

  // Oculta os controles do nome e mostra os das disciplinas
  edtNomeEstudante.Enabled := False;
  btnConfirmarNomeEstudante.Enabled := False;
  pnlEscolhaDisciplina.Visible := True;
end;

//--------------------------------------------------------------------------------------------------
// Lógica para os botões de disciplina
//--------------------------------------------------------------------------------------------------
procedure TFColegio_CRUD.btnDisciplinaClick(Sender: TObject);
begin
  // A propriedade Caption do botão clicado contém o nome da disciplina
  DisciplinaSelecionada := (Sender as TButton).Caption;

  // Torna o painel de professores visível
  pnlEscolhaProfessor.Visible := True;

  // Habilita os botões de professor para reavaliar
  btnProfessorFlavio.Enabled := True;
  btnProfessorFausto.Enabled := True;

  // Aplica a regra de desabilitação de professor
  if DisciplinaSelecionada in ['Informatica basica', 'FullStack', 'UX Design', 'Seguranca da informacao'] then
  begin
    btnProfessorFausto.Enabled := False
  end
  else if DisciplinaSelecionada = 'Analise de Dados', 'Ética de Dados' then
  begin
    btnProfessorFlavio.Enabled := False;
  end;
end;

//--------------------------------------------------------------------------------------------------
// Lógica para os botões de professor
//--------------------------------------------------------------------------------------------------
procedure TFColegio_CRUD.btnProfessorClick(Sender: TObject);
var
  TurmaNome: string;
  Mensagem: string;
begin
  ProfessorSelecionado := (Sender as TButton).Caption;

  // Lógica de turma simplificada
  TurmaNome := 'T001';

  // Constrói e exibe a mensagem de confirmação
  Mensagem := Format(
    '%s foi cadastrado com sucesso! Segue a frente as informações especificas: ' +
    'Matricula: %s, Disciplina: %s, Professor: %s, Turma: %s',
    [EstudanteNome, MatriculaAluno, DisciplinaSelecionada, ProfessorSelecionado, TurmaNome]
  );
  ShowMessage(Mensagem);

  // Salva as informações em JSON e reseta o formulário
  SalvarAlunoJSON;
  ResetForm;
end;

//--------------------------------------------------------------------------------------------------
// Procedures auxiliares
//--------------------------------------------------------------------------------------------------
procedure TFColegio_CRUD.GerarCodigoMatricula;
var
  AnoAtual: Integer;
  Semestre: string;
begin
  AnoAtual := YearOf(Date);
  if MonthOf(Date) < 7 then
    Semestre := '01'
  else
    Semestre := '02';

  CodigoAluno := Format('%d%s%.4d', [AnoAtual, Semestre, ProximoCodigoAluno]);
  MatriculaAluno := 'MA#' + CodigoAluno;

  Inc(ProximoCodigoAluno);
end;

procedure TFColegio_CRUD.SalvarAlunoJSON;
var
  JSONObj: TJSONObject;
  ProfessorCPF: string;
  ProfessorCodigo: string;
  FileName: string;
begin
  // Simulação de busca de dados do professor
  if ProfessorSelecionado = 'Flavio' then
  begin
    ProfessorCPF := '123.456.789-00';
    ProfessorCodigo := 'PROF01';
  end
  else if ProfessorSelecionado = 'Fausto Silva' then
  begin
    ProfessorCPF := '987.654.321-00';
    ProfessorCodigo := 'PROF02';
  end;

  JSONObj := TJSONObject.Create;
  try
    JSONObj.AddPair('nome', EstudanteNome);
    JSONObj.AddPair('codigo_aluno', CodigoAluno);
    JSONObj.AddPair('matricula', MatriculaAluno);
    JSONObj.AddPair('disciplina', DisciplinaSelecionada);

    var ProfessorJSON := TJSONObject.Create;
    try
      ProfessorJSON.AddPair('nome', ProfessorSelecionado);
      ProfessorJSON.AddPair('cpf', ProfessorCPF);
      ProfessorJSON.AddPair('codigo', ProfessorCodigo);
    except
      ProfessorJSON.Free;
      raise;
    end;
    JSONObj.AddPair('professor', ProfessorJSON);

    FileName := TPath.Combine(TPath.GetHomePath, 'alunos.json');
    TFile.WriteAllText(FileName, JSONObj.ToString);

  finally
    JSONObj.Free;
  end;
end;

procedure TFColegio_CRUD.ResetForm;
begin
  // Limpa os campos e esconde os painéis de seleção
  edtNomeEstudante.Text := '';
  edtNomeEstudante.Enabled := True;
  btnConfirmarNomeEstudante.Enabled := True;
  pnlEscolhaDisciplina.Visible := False;
  pnlEscolhaProfessor.Visible := False;
  edtNomeEstudante.SetFocus;
end;

end.
