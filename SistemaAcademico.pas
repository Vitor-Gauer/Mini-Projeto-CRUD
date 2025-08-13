unit SistemaAcademico;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.Grids, Vcl.ExtCtrls, Vcl.ComCtrls,
  uEstudante, uProfessor, uDisciplina, uTurma, uMatricula, uLogin,
  System.Generics.Collections; // Necessário para ObterTodos

type

  TFSistemaAcademico = class(TForm)

// --------------------------------------------------------------------------------------------------
// Controle de Páginas
// --------------------------------------------------------------------------------------------------

    pgcAbasDoForm: TPageControl;

// --------------------------------------------------------------------------------------------------
// ABA: Estudantes
// --------------------------------------------------------------------------------------------------

    tabEstudantes: TTabSheet;
    pnlEstudantes: TPanel;
    lblCodigoEst: TLabel;
    lblNomeEst: TLabel;
    edtCodigoEst: TEdit;
    edtNomeEst: TEdit;
    btnIncluirEst: TButton;
    btnAtualizarEst: TButton;
    btnExcluirEst: TButton;
    btnBuscarEst: TButton;
    lbxEstudantes: TListBox;

// --------------------------------------------------------------------------------------------------
// ABA: Professores
// --------------------------------------------------------------------------------------------------

    tabProfessores: TTabSheet;
    pnlProfessores: TPanel;
    lblCodigoProf: TLabel;
    lblNomeProf: TLabel;
    lblCPFProf: TLabel;
    edtCodigoProf: TEdit;
    edtNomeProf: TEdit;
    edtCPFProf: TEdit;
    btnIncluirProf: TButton;
    btnAtualizarProf: TButton;
    btnExcluirProf: TButton;
    btnBuscarProf: TButton;
    lbxProfessores: TListBox;

// --------------------------------------------------------------------------------------------------
// ABA: Disicplinas
// --------------------------------------------------------------------------------------------------

    tabDisciplinas: TTabSheet;
    pnlDisciplinas: TPanel;
    lblCodigoDisc: TLabel;
    lblNomeDisc: TLabel;
    edtCodigoDisc: TEdit;
    edtNomeDisc: TEdit;
    btnIncluirDisc: TButton;
    btnAtualizarDisc: TButton;
    btnExcluirDisc: TButton;
    btnBuscarDisc: TButton;
    lbxDisciplinas: TListBox;

// --------------------------------------------------------------------------------------------------
// ABA: Turmas
// --------------------------------------------------------------------------------------------------

    tabTurmas: TTabSheet;
    pnlTurmas: TPanel;
    lblCodigoTurma: TLabel;
    lblCodigoProfTurma: TLabel;
    lblCodigoDiscTurma: TLabel;
    edtCodigoTurma: TEdit;
    edtCodigoProfTurma: TEdit;
    edtCodigoDiscTurma: TEdit;
    btnIncluirTurma: TButton;
    btnAtualizarTurma: TButton;
    btnExcluirTurma: TButton;
    btnBuscarTurma: TButton;
    lbxTurmas: TListBox;

// --------------------------------------------------------------------------------------------------
// ABA: Matrículas
// --------------------------------------------------------------------------------------------------
    tabMatriculas: TTabSheet;
    pnlMatriculas: TPanel;
    lblCodigoMat: TLabel;
    lblCodigoTurmaMat: TLabel;
    lblCodigoEstMat: TLabel;
    edtCodigoMat: TEdit;
    edtCodigoTurmaMat: TEdit;
    edtCodigoEstMat: TEdit;
    btnIncluirMat: TButton;
    btnAtualizarMat: TButton;
    btnExcluirMat: TButton;
    btnBuscarMat: TButton;
    lbxMatriculas: TListBox;

// --------------------------------------------------------------------------------------------------
// Botões: Relatório
// --------------------------------------------------------------------------------------------------

    btnRelatorioEstudantes: TButton;
    btnRelatorioProfessores: TButton;
    btnRelatorioDisciplinas: TButton;
    btnRelatorioMatriculas: TButton;

// --------------------------------------------------------------------------------------------------
// Eventos: Formulários
// --------------------------------------------------------------------------------------------------

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

// --------------------------------------------------------------------------------------------------
// Eventos: Estudantes
// --------------------------------------------------------------------------------------------------

    procedure btnIncluirEstClick(Sender: TObject);
    procedure btnAtualizarEstClick(Sender: TObject);
    procedure btnExcluirEstClick(Sender: TObject);
    procedure btnBuscarEstClick(Sender: TObject);
    procedure lbxEstudantesClick(Sender: TObject);

// --------------------------------------------------------------------------------------------------
// Eventos: Professores
// --------------------------------------------------------------------------------------------------

    procedure btnIncluirProfClick(Sender: TObject);
    procedure btnAtualizarProfClick(Sender: TObject);
    procedure btnExcluirProfClick(Sender: TObject);
    procedure btnBuscarProfClick(Sender: TObject);
    procedure lbxProfessoresClick(Sender: TObject);

// --------------------------------------------------------------------------------------------------
// Eventos: Disciplinas
// --------------------------------------------------------------------------------------------------

    procedure btnIncluirDiscClick(Sender: TObject);
    procedure btnAtualizarDiscClick(Sender: TObject);
    procedure btnExcluirDiscClick(Sender: TObject);
    procedure btnBuscarDiscClick(Sender: TObject);
    procedure lbxDisciplinasClick(Sender: TObject);

// --------------------------------------------------------------------------------------------------
// Eventos: Turma
// --------------------------------------------------------------------------------------------------

    procedure btnIncluirTurmaClick(Sender: TObject);
    procedure btnAtualizarTurmaClick(Sender: TObject);
    procedure btnExcluirTurmaClick(Sender: TObject);
    procedure btnBuscarTurmaClick(Sender: TObject);
    procedure lbxTurmasClick(Sender: TObject);

// --------------------------------------------------------------------------------------------------
// Eventos: Matrículas
// --------------------------------------------------------------------------------------------------

    procedure btnIncluirMatClick(Sender: TObject);
    procedure btnAtualizarMatClick(Sender: TObject);
    procedure btnExcluirMatClick(Sender: TObject);
    procedure btnBuscarMatClick(Sender: TObject);
    procedure lbxMatriculasClick(Sender: TObject);

// --------------------------------------------------------------------------------------------------
// Eventos: Relatórios
// --------------------------------------------------------------------------------------------------

    procedure btnRelatorioEstudantesClick(Sender: TObject);
    procedure btnRelatorioProfessoresClick(Sender: TObject);
    procedure btnRelatorioDisciplinasClick(Sender: TObject);
    procedure btnRelatorioMatriculasClick(Sender: TObject);

  private
    { Private declarations }
    // Variáveis e métodos privados
    FEstudanteControlador: TEstudanteControlador;
    FProfessorControlador: TProfessorControlador;
    FDisciplinaControlador: TDisciplinaControlador;
    FTurmaControlador: TTurmaControlador;
    FMatriculaControlador: TMatriculaControlador;
    FNivelAcesso: Integer; // Armazena o nível de acesso como inteiro

    // Métodos auxiliares para limpar os campos de entrada
    procedure LimparCamposEstudante;
    procedure LimparCamposProfessor;
    procedure LimparCamposDisciplina;
    procedure LimparCamposTurma;
    procedure LimparCamposMatricula;

    // Métodos auxiliares para carregar dados das entidades nos campos de entrada
    procedure CarregarDadosEstudante(estudante: TEstudante);
    procedure CarregarDadosProfessor(professor: TProfessor);
    procedure CarregarDadosDisciplina(disciplina: TDisciplina);
    procedure CarregarDadosTurma(turma: TTurma);
    procedure CarregarDadosMatricula(matricula: TMatricula);

    // Métodos para Atualizar as Listas ---
    procedure AtualizarListaEstudantes;
    procedure AtualizarListaProfessores;
    procedure AtualizarListaDisciplinas;
    procedure AtualizarListaTurmas;
    procedure AtualizarListaMatriculas;
    // ---------------------------------------------

    // Controle de permissões
    procedure AtualizarPermissoes;

    // Geração de relatórios
    procedure GerarRelatorioEstudantesPorTurma;
    procedure GerarRelatorioProfessoresPorDisciplina;
    procedure GerarRelatorioDisciplinasOfertadas;
    procedure GerarRelatorioMatriculasPorEstudante;

  public
    { Public declarations }
  end;

var
  FormPrincipal: TFSistemaAcademico;

// Constantes para níveis de acesso (tipos ordinais para uso em case)
const
  NIVEL_ADMIN = 1;
  NIVEL_PROFESSOR = 2;
  NIVEL_ESTUDANTE = 3;

implementation

{$R *.dfm}

// --------------------------------------------------------------------------------------------------
// Eventos do Formulário Principal
// --------------------------------------------------------------------------------------------------
procedure TFSistemaAcademico.FormCreate(Sender: TObject);
var
  Login: TFLocalLogin;
begin
  try
    // Exibe tela de login antes de carregar o sistema
    Login := TFLocalLogin.Create(nil);
    try
      if Login.ShowModal = mrOk then
      begin
        // Converte o nível de acesso string para inteiro
        if Login.NivelAcesso = 'admin' then
          FNivelAcesso := NIVEL_ADMIN
        else if Login.NivelAcesso = 'professor' then
          FNivelAcesso := NIVEL_PROFESSOR
        else if Login.NivelAcesso = 'estudante' then
          FNivelAcesso := NIVEL_ESTUDANTE;

        Caption := Format('Sistema Acadêmico - Usuário: %s [%s]', [Login.UsuarioLogado, Login.NivelAcesso]);
      end
      else
      begin
        Application.Terminate;
        Exit;
      end;
    finally
      Login.Free;
    end;

    // Inicializa os controladores
    FEstudanteControlador := TEstudanteControlador.Create;
    FProfessorControlador := TProfessorControlador.Create;
    FDisciplinaControlador := TDisciplinaControlador.Create;
    FTurmaControlador := TTurmaControlador.Create;
    FMatriculaControlador := TMatriculaControlador.Create;

    FEstudanteControlador.CarregarDados;
    FProfessorControlador.CarregarDados;
    FDisciplinaControlador.CarregarDados;
    FTurmaControlador.CarregarDados;
    FMatriculaControlador.CarregarDados;

    // Atualiza as listas INICIALMENTE
    AtualizarListaEstudantes;
    AtualizarListaProfessores;
    AtualizarListaDisciplinas;
    AtualizarListaTurmas;
    AtualizarListaMatriculas;

    // Configura permissões com base no nível de acesso
    AtualizarPermissoes;
  except
    on E: Exception do
      ShowMessage('Erro ao inicializar sistema: ' + E.Message);
  end;
end;

    // Quando o formulário for fechado, se tiver algo para salvar, salve e tire da memória
procedure TFSistemaAcademico.FormDestroy(Sender: TObject);
begin
  try
    if Assigned(FEstudanteControlador) then
    begin
      FEstudanteControlador.SalvarDados;
      FEstudanteControlador.Free;
    end;
    if Assigned(FProfessorControlador) then
    begin
      FProfessorControlador.SalvarDados;
      FProfessorControlador.Free;
    end;
    if Assigned(FDisciplinaControlador) then
    begin
      FDisciplinaControlador.SalvarDados;
      FDisciplinaControlador.Free;
    end;
    if Assigned(FTurmaControlador) then
    begin
      FTurmaControlador.SalvarDados;
      FTurmaControlador.Free;
    end;
    if Assigned(FMatriculaControlador) then
    begin
      FMatriculaControlador.SalvarDados;
      FMatriculaControlador.Free;
    end;
  except
    on E: Exception do
      ShowMessage('Erro ao finalizar sistema: ' + E.Message);
  end;
end;


// --------------------------------------------------------------------------------------------------
// Métodos para Atualizar Listas
//
// Atualizar: Estudantes
// --------------------------------------------------------------------------------------------------


procedure TFSistemaAcademico.AtualizarListaEstudantes;
begin
  try
    FEstudanteControlador.Listar(lbxEstudantes.Items);
  except
    on E: Exception do
      ShowMessage('Erro ao atualizar lista de estudantes: ' + E.Message);
  end;
end;

// --------------------------------------------------------------------------------------------------
// Atualizar: Professores
// --------------------------------------------------------------------------------------------------

procedure TFSistemaAcademico.AtualizarListaProfessores;
begin
  try
    FProfessorControlador.Listar(lbxProfessores.Items);
  except
    on E: Exception do
      ShowMessage('Erro ao atualizar lista de professores: ' + E.Message);
  end;
end;

// --------------------------------------------------------------------------------------------------
// Atualizar: Dsiciplinas
// --------------------------------------------------------------------------------------------------

procedure TFSistemaAcademico.AtualizarListaDisciplinas;
begin
  try
    FDisciplinaControlador.Listar(lbxDisciplinas.Items);
  except
    on E: Exception do
      ShowMessage('Erro ao atualizar lista de disciplinas: ' + E.Message);
  end;
end;

// --------------------------------------------------------------------------------------------------
// Atualizar: Turmas
// --------------------------------------------------------------------------------------------------

procedure TFSistemaAcademico.AtualizarListaTurmas;
begin
  try
    // Passa os controladores de Professor e Disciplina para obter os nomes na listbox
    FTurmaControlador.Listar(lbxTurmas.Items, FProfessorControlador, FDisciplinaControlador);
  except
    on E: Exception do
      ShowMessage('Erro ao atualizar lista de turmas: ' + E.Message);
  end;
end;

// --------------------------------------------------------------------------------------------------
// Atualizar: Mátriculas
// --------------------------------------------------------------------------------------------------

procedure TFSistemaAcademico.AtualizarListaMatriculas;
begin
  try
    // Passa todos os controladores necessários para obter os nomes na listbox
    FMatriculaControlador.Listar(lbxMatriculas.Items, FTurmaControlador, FEstudanteControlador, FProfessorControlador, FDisciplinaControlador);
  except
    on E: Exception do
      ShowMessage('Erro ao atualizar lista de matrículas: ' + E.Message);
  end;
end;


// --------------------------------------------------------------------------------------------------
// Controle de Permissões por Nível de Acesso
// --------------------------------------------------------------------------------------------------


procedure TFSistemaAcademico.AtualizarPermissoes;
begin
  case FNivelAcesso of
    NIVEL_ADMIN:
      begin
        pgcAbasDoForm.Enabled := True;
        tabEstudantes.TabVisible := True;
        tabProfessores.TabVisible := True;
        tabDisciplinas.TabVisible := True;
        tabTurmas.TabVisible := True;
        tabMatriculas.TabVisible := True;
      end;
    NIVEL_PROFESSOR:
      begin
        tabEstudantes.TabVisible := False;
        tabProfessores.TabVisible := False;
        btnIncluirProf.Enabled := False;
        btnAtualizarProf.Enabled := False;
        btnExcluirProf.Enabled := False;
      end;
    NIVEL_ESTUDANTE:
      begin
        tabEstudantes.TabVisible := True;
        tabProfessores.TabVisible := False;
        tabDisciplinas.TabVisible := False;
        tabTurmas.TabVisible := False;
        tabMatriculas.TabVisible := True;

        btnIncluirEst.Enabled := False;
        btnAtualizarEst.Enabled := False;
        btnExcluirEst.Enabled := False;
        btnBuscarEst.Enabled := False;

        btnIncluirMat.Enabled := False;
        btnAtualizarMat.Enabled := False;
        btnExcluirMat.Enabled := False;
        btnBuscarMat.Enabled := False;
      end;
  end;
end;


// --------------------------------------------------------------------------------------------------
// Indexação para a Geração de Relatórios
//
// Relatório: Estudante/Turma
// --------------------------------------------------------------------------------------------------


procedure TFSistemaAcademico.btnRelatorioEstudantesClick(Sender: TObject);
begin
  GerarRelatorioEstudantesPorTurma;
end;

// --------------------------------------------------------------------------------------------------
// Relatório: Professores/Disciplina
// --------------------------------------------------------------------------------------------------

procedure TFSistemaAcademico.btnRelatorioProfessoresClick(Sender: TObject);
begin
  GerarRelatorioProfessoresPorDisciplina;
end;

// --------------------------------------------------------------------------------------------------
// Relatório: Disciplinas
// --------------------------------------------------------------------------------------------------

procedure TFSistemaAcademico.btnRelatorioDisciplinasClick(Sender: TObject);
begin
  GerarRelatorioDisciplinasOfertadas;
end;

// --------------------------------------------------------------------------------------------------
// Relatório: Matrículas/Estudante
// --------------------------------------------------------------------------------------------------

procedure TFSistemaAcademico.btnRelatorioMatriculasClick(Sender: TObject);
begin
  GerarRelatorioMatriculasPorEstudante;
end;


// --------------------------------------------------------------------------------------------------
// Procedimentos para Geração de Relatórios
//
// Relatório: Estudantes/Turma
// --------------------------------------------------------------------------------------------------


procedure TFSistemaAcademico.GerarRelatorioEstudantesPorTurma;
var
  Stream: TStringList;
  turma: TTurma;
  matricula: TMatricula;
  estudante: TEstudante;
begin
  Stream := TStringList.Create;
  try
    Stream.Add('RELATÓRIO: ESTUDANTES POR TURMA');
    Stream.Add('');
    for turma in FTurmaControlador.ObterTodos do
    begin
      Stream.Add(Format('Turma Cód: %d | Disciplina: %s | Professor: %s',
        [turma.Codigo,
         FDisciplinaControlador.BuscarPorCodigo(turma.CodigoDisciplina).Nome,
         FProfessorControlador.BuscarPorCodigo(turma.CodigoProfessor).Nome]));
      Stream.Add('--- Estudantes:');
      for matricula in FMatriculaControlador.ObterTodos do
      begin
        if matricula.CodigoTurma = turma.Codigo then
        begin
          estudante := FEstudanteControlador.BuscarPorCodigo(matricula.CodigoEstudante);
          if Assigned(estudante) then
            Stream.Add(Format('  Cód: %d | Nome: %s', [estudante.Codigo, estudante.Nome]));
        end;
      end;
      Stream.Add('');
    end;

    Stream.SaveToFile(ExtractFilePath(Application.ExeName) + 'relatorio_estudantes_turma.txt');
    ShowMessage('Relatório salvo como TXT.');
  finally
    Stream.Free;
  end;
end;

// --------------------------------------------------------------------------------------------------
// Relatório: Professores/Disicplina
// --------------------------------------------------------------------------------------------------

procedure TFSistemaAcademico.GerarRelatorioProfessoresPorDisciplina;
var
  Stream: TStringList;
  disciplina: TDisciplina;
  turma: TTurma;
  professor: TProfessor;
begin
  Stream := TStringList.Create;
  try
    Stream.Add('RELATÓRIO: PROFESSORES POR DISCIPLINA');
    Stream.Add('');
    for disciplina in FDisciplinaControlador.ObterTodos do
    begin
      Stream.Add(Format('Disciplina: %s', [disciplina.Nome]));
      for turma in FTurmaControlador.ObterTodos do
      begin
        if turma.CodigoDisciplina = disciplina.Codigo then
        begin
          professor := FProfessorControlador.BuscarPorCodigo(turma.CodigoProfessor);
          if Assigned(professor) then
            Stream.Add(Format('  Professor: %s (CPF: %s)', [professor.Nome, professor.CPF]));
        end;
      end;
      Stream.Add('');
    end;

    Stream.SaveToFile(ExtractFilePath(Application.ExeName) + 'relatorio_professores_disciplina.txt');
    ShowMessage('Relatório salvo como TXT.');
  finally
    Stream.Free;
  end;
end;

// Relatório: Disciplinas Ofertadas
procedure TFSistemaAcademico.GerarRelatorioDisciplinasOfertadas;
var
  Stream: TStringList;
  disciplina: TDisciplina;
  turma: TTurma;
  professor: TProfessor;
begin
  Stream := TStringList.Create;
  try
    Stream.Add('RELATÓRIO: DISCIPLINAS OFERTADAS');
    Stream.Add('');
    for disciplina in FDisciplinaControlador.ObterTodos do
    begin
      Stream.Add(Format('Disciplina: %s', [disciplina.Nome]));
      for turma in FTurmaControlador.ObterTodos do
      begin
        if turma.CodigoDisciplina = disciplina.Codigo then
        begin
          professor := FProfessorControlador.BuscarPorCodigo(turma.CodigoProfessor);
          Stream.Add(Format('  Turma: %d | Professor: %s', [turma.Codigo, professor.Nome]));
        end;
      end;
      Stream.Add('');
    end;

    Stream.SaveToFile(ExtractFilePath(Application.ExeName) + 'relatorio_disciplinas_ofertadas.txt');
    ShowMessage('Relatório salvo como TXT.');
  finally
    Stream.Free;
  end;
end;

// Relatório: Matrículas por Estudante
procedure TFSistemaAcademico.GerarRelatorioMatriculasPorEstudante;
var
  Stream: TStringList;
  estudante: TEstudante;
  matricula: TMatricula;
  turma: TTurma;
  disciplina: TDisciplina;
begin
  Stream := TStringList.Create;
  try
    Stream.Add('RELATÓRIO: MATRÍCULAS POR ESTUDANTE');
    Stream.Add('');
    for estudante in FEstudanteControlador.ObterTodos do
    begin
      Stream.Add(Format('Estudante: %s (Cód: %d)', [estudante.Nome, estudante.Codigo]));
      Stream.Add('  Matrículas:');
      for matricula in FMatriculaControlador.ObterTodos do
      begin
        if matricula.CodigoEstudante = estudante.Codigo then
        begin
          turma := FTurmaControlador.BuscarPorCodigo(matricula.CodigoTurma);
          if Assigned(turma) then
          begin
            disciplina := FDisciplinaControlador.BuscarPorCodigo(turma.CodigoDisciplina);
            Stream.Add(Format('    Turma: %d | Disciplina: %s', [turma.Codigo, disciplina.Nome]));
          end;
        end;
      end;
      Stream.Add('');
    end;

    Stream.SaveToFile(ExtractFilePath(Application.ExeName) + 'relatorio_matriculas_estudante.txt');
    ShowMessage('Relatório salvo como TXT.');
  finally
    Stream.Free;
  end;
end;


// --------------------------------------------------------------------------------------------------
// Métodos Auxiliares
// --------------------------------------------------------------------------------------------------
procedure TFSistemaAcademico.LimparCamposEstudante;
begin
  edtCodigoEst.Clear;
  edtNomeEst.Clear;
  edtCodigoEst.Enabled := True;
end;

procedure TFSistemaAcademico.LimparCamposProfessor;
begin
  edtCodigoProf.Clear;
  edtNomeProf.Clear;
  edtCPFProf.Clear;
  edtCodigoProf.Enabled := True;
end;

procedure TFSistemaAcademico.LimparCamposDisciplina;
begin
  edtCodigoDisc.Clear;
  edtNomeDisc.Clear;
  edtCodigoDisc.Enabled := True;
end;

procedure TFSistemaAcademico.LimparCamposTurma;
begin
  edtCodigoTurma.Clear;
  edtCodigoProfTurma.Clear;
  edtCodigoDiscTurma.Clear;
  edtCodigoTurma.Enabled := True;
end;

procedure TFSistemaAcademico.LimparCamposMatricula;
begin
  edtCodigoMat.Clear;
  edtCodigoTurmaMat.Clear;
  edtCodigoEstMat.Clear;
  edtCodigoMat.Enabled := True;
end;

procedure TFSistemaAcademico.CarregarDadosEstudante(estudante: TEstudante);
begin
  if Assigned(estudante) then
  begin
    edtCodigoEst.Text := IntToStr(estudante.Codigo);
    edtNomeEst.Text := estudante.Nome;
    edtCodigoEst.Enabled := False;
  end;
end;

procedure TFSistemaAcademico.CarregarDadosProfessor(professor: TProfessor);
begin
  if Assigned(professor) then
  begin
    edtCodigoProf.Text := IntToStr(professor.Codigo);
    edtNomeProf.Text := professor.Nome;
    edtCPFProf.Text := professor.CPF;
    edtCodigoProf.Enabled := False;
  end;
end;

procedure TFSistemaAcademico.CarregarDadosDisciplina(disciplina: TDisciplina);
begin
  if Assigned(disciplina) then
  begin
    edtCodigoDisc.Text := IntToStr(disciplina.Codigo);
    edtNomeDisc.Text := disciplina.Nome;
    edtCodigoDisc.Enabled := False;
  end;
end;

procedure TFSistemaAcademico.CarregarDadosTurma(turma: TTurma);
begin
  if Assigned(turma) then
  begin
    edtCodigoTurma.Text := IntToStr(turma.Codigo);
    edtCodigoProfTurma.Text := IntToStr(turma.CodigoProfessor);
    edtCodigoDiscTurma.Text := IntToStr(turma.CodigoDisciplina);
    edtCodigoTurma.Enabled := False;
  end;
end;

procedure TFSistemaAcademico.CarregarDadosMatricula(matricula: TMatricula);
begin
  if Assigned(matricula) then
  begin
    edtCodigoMat.Text := IntToStr(matricula.Codigo);
    edtCodigoTurmaMat.Text := IntToStr(matricula.CodigoTurma);
    edtCodigoEstMat.Text := IntToStr(matricula.CodigoEstudante);
    edtCodigoMat.Enabled := False;
  end;
end;


// --------------------------------------------------------------------------------------------------
// Eventos de Estudantes (com AtualizarListaEstudantes)
// --------------------------------------------------------------------------------------------------
procedure TFSistemaAcademico.btnIncluirEstClick(Sender: TObject);
var
  Codigo: Integer;
  Nome: string;
begin
  try
    if not TryStrToInt(edtCodigoEst.Text, Codigo) then
    begin
      ShowMessage('Código deve ser um número inteiro válido!');
      edtCodigoEst.SetFocus;
      Exit;
    end;
    Nome := Trim(edtNomeEst.Text);
    if Nome = '' then
    begin
      ShowMessage('Nome é obrigatório!');
      edtNomeEst.SetFocus;
      Exit;
    end;

    if FEstudanteControlador.Incluir(Codigo, Nome) then
    begin
      ShowMessage('Estudante incluído com sucesso!');
      LimparCamposEstudante;
      AtualizarListaEstudantes;
    end;
  except
    on E: Exception do
      ShowMessage('Erro ao incluir estudante: ' + E.Message);
  end;
end;

procedure TFSistemaAcademico.btnAtualizarEstClick(Sender: TObject);
var
  Codigo: Integer;
  Nome: string;
begin
  try
    if not TryStrToInt(edtCodigoEst.Text, Codigo) then
    begin
      ShowMessage('Selecione um estudante para atualizar!');
      Exit;
    end;
    Nome := Trim(edtNomeEst.Text);
    if Nome = '' then
    begin
      ShowMessage('Nome é obrigatório!');
      edtNomeEst.SetFocus;
      Exit;
    end;

    if FEstudanteControlador.Atualizar(Codigo, Nome) then
    begin
      ShowMessage('Estudante atualizado com sucesso!');
      LimparCamposEstudante;
      AtualizarListaEstudantes;
    end
    else
      ShowMessage('Erro: Estudante não encontrado!');
  except
    on E: Exception do
      ShowMessage('Erro ao atualizar estudante: ' + E.Message);
  end;
end;

procedure TFSistemaAcademico.btnExcluirEstClick(Sender: TObject);
var
  Codigo: Integer;
begin
  try
    if not TryStrToInt(edtCodigoEst.Text, Codigo) then
    begin
      ShowMessage('Selecione um estudante para excluir!');
      Exit;
    end;

    if MessageDlg('Confirma a exclusão do estudante?', mtConfirmation,
      [mbYes, mbNo], 0) = mrYes then
    begin
      if FEstudanteControlador.Excluir(Codigo) then
      begin
        ShowMessage('Estudante excluído com sucesso!');
        LimparCamposEstudante;
        AtualizarListaEstudantes;
      end
      else
        ShowMessage('Erro: Estudante não encontrado!');
    end;
  except
    on E: Exception do
      ShowMessage('Erro ao excluir estudante: ' + E.Message);
  end;
end;

procedure TFSistemaAcademico.btnBuscarEstClick(Sender: TObject);
var
  Codigo: Integer;
  estudante: TEstudante;
begin
  try
    if not TryStrToInt(edtCodigoEst.Text, Codigo) then
    begin
      ShowMessage('Digite um código válido para buscar!');
      edtCodigoEst.SetFocus;
      Exit;
    end;

    estudante := FEstudanteControlador.BuscarPorCodigo(Codigo);
    if Assigned(estudante) then
      CarregarDadosEstudante(estudante)
    else
      ShowMessage('Estudante não encontrado!');
  except
    on E: Exception do
      ShowMessage('Erro ao buscar estudante: ' + E.Message);
  end;
end;

procedure TFSistemaAcademico.lbxEstudantesClick(Sender: TObject);
var
  linha: string;
  Codigo: Integer;
  estudante: TEstudante;
begin
  try
    if lbxEstudantes.ItemIndex >= 0 then
    begin
      linha := lbxEstudantes.Items[lbxEstudantes.ItemIndex];
      if Pos('Código: ', linha) > 0 then
      begin
        linha := Copy(linha, Pos('Código: ', linha) + 8, Length(linha));
        linha := Copy(linha, 1, Pos(' -', linha) - 1);
        if TryStrToInt(linha, Codigo) then
        begin
          estudante := FEstudanteControlador.BuscarPorCodigo(Codigo);
          CarregarDadosEstudante(estudante);
        end;
      end;
    end;
  except
    on E: Exception do
      ShowMessage('Erro ao selecionar estudante: ' + E.Message);
  end;
end;

// --------------------------------------------------------------------------------------------------
// Eventos de Professores (com AtualizarListaProfessores)
// --------------------------------------------------------------------------------------------------
procedure TFSistemaAcademico.btnIncluirProfClick(Sender: TObject);
var
  Codigo: Integer;
  Nome, CPF: string;
begin
  try
    if not TryStrToInt(edtCodigoProf.Text, Codigo) then
    begin
      ShowMessage('Código deve ser um número inteiro válido!');
      edtCodigoProf.SetFocus;
      Exit;
    end;
    Nome := Trim(edtNomeProf.Text);
    if Nome = '' then
    begin
      ShowMessage('Nome é obrigatório!');
      edtNomeProf.SetFocus;
      Exit;
    end;
    CPF := Trim(edtCPFProf.Text);
    if CPF = '' then
    begin
      ShowMessage('CPF é obrigatório!');
      edtCPFProf.SetFocus;
      Exit;
    end;

    if FProfessorControlador.Incluir(Codigo, Nome, CPF) then
    begin
      ShowMessage('Professor incluído com sucesso!');
      LimparCamposProfessor;
      AtualizarListaProfessores;
    end;
  except
    on E: Exception do
      ShowMessage('Erro ao incluir professor: ' + E.Message);
  end;
end;

procedure TFSistemaAcademico.btnAtualizarProfClick(Sender: TObject);
var
  Codigo: Integer;
  Nome, CPF: string;
begin
  try
    if not TryStrToInt(edtCodigoProf.Text, Codigo) then
    begin
      ShowMessage('Selecione um professor para atualizar!');
      Exit;
    end;
    Nome := Trim(edtNomeProf.Text);
    if Nome = '' then
    begin
      ShowMessage('Nome é obrigatório!');
      edtNomeProf.SetFocus;
      Exit;
    end;
    CPF := Trim(edtCPFProf.Text);
    if CPF = '' then
    begin
      ShowMessage('CPF é obrigatório!');
      edtCPFProf.SetFocus;
      Exit;
    end;

    if FProfessorControlador.Atualizar(Codigo, Nome, CPF) then
    begin
      ShowMessage('Professor atualizado com sucesso!');
      LimparCamposProfessor;
      AtualizarListaProfessores;
    end
    else
      ShowMessage('Erro: Professor não encontrado!');
  except
    on E: Exception do
      ShowMessage('Erro ao atualizar professor: ' + E.Message);
  end;
end;

procedure TFSistemaAcademico.btnExcluirProfClick(Sender: TObject);
var
  Codigo: Integer;
begin
  try
    if not TryStrToInt(edtCodigoProf.Text, Codigo) then
    begin
      ShowMessage('Selecione um professor para excluir!');
      Exit;
    end;

    if MessageDlg('Confirma a exclusão do professor?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      if FProfessorControlador.Excluir(Codigo) then
      begin
        ShowMessage('Professor excluído com sucesso!');
        LimparCamposProfessor;
        AtualizarListaProfessores;
      end
      else
        ShowMessage('Erro: Professor não encontrado!');
    end;
  except
    on E: Exception do
      ShowMessage('Erro ao excluir professor: ' + E.Message);
  end;
end;

procedure TFSistemaAcademico.btnBuscarProfClick(Sender: TObject);
var
  Codigo: Integer;
  professor: TProfessor;
begin
  try
    if not TryStrToInt(edtCodigoProf.Text, Codigo) then
    begin
      ShowMessage('Digite um código válido para buscar!');
      edtCodigoProf.SetFocus;
      Exit;
    end;

    professor := FProfessorControlador.BuscarPorCodigo(Codigo);
    if Assigned(professor) then
      CarregarDadosProfessor(professor)
    else
      ShowMessage('Professor não encontrado!');
  except
    on E: Exception do
      ShowMessage('Erro ao buscar professor: ' + E.Message);
  end;
end;

procedure TFSistemaAcademico.lbxProfessoresClick(Sender: TObject);
var
  linha: string;
  Codigo: Integer;
  professor: TProfessor;
begin
  try
    if lbxProfessores.ItemIndex >= 0 then
    begin
      linha := lbxProfessores.Items[lbxProfessores.ItemIndex];
      if Pos('Código: ', linha) > 0 then
      begin
        linha := Copy(linha, Pos('Código: ', linha) + 8, Length(linha));
        linha := Copy(linha, 1, Pos(' -', linha) - 1);
        if TryStrToInt(linha, Codigo) then
        begin
          professor := FProfessorControlador.BuscarPorCodigo(Codigo);
          CarregarDadosProfessor(professor);
        end;
      end;
    end;
  except
    on E: Exception do
      ShowMessage('Erro ao selecionar professor: ' + E.Message);
  end;
end;

// --------------------------------------------------------------------------------------------------
// Eventos de Disciplinas (com AtualizarListaDisciplinas)
// --------------------------------------------------------------------------------------------------
procedure TFSistemaAcademico.btnIncluirDiscClick(Sender: TObject);
var
  Codigo: Integer;
  Nome: string;
begin
  try
    if not TryStrToInt(edtCodigoDisc.Text, Codigo) then
    begin
      ShowMessage('Código deve ser um número inteiro válido!');
      edtCodigoDisc.SetFocus;
      Exit;
    end;
    Nome := Trim(edtNomeDisc.Text);
    if Nome = '' then
    begin
      ShowMessage('Nome é obrigatório!');
      edtNomeDisc.SetFocus;
      Exit;
    end;

    if FDisciplinaControlador.Incluir(Codigo, Nome) then
    begin
      ShowMessage('Disciplina incluída com sucesso!');
      LimparCamposDisciplina;
      AtualizarListaDisciplinas;
    end;
  except
    on E: Exception do
      ShowMessage('Erro ao incluir disciplina: ' + E.Message);
  end;
end;

procedure TFSistemaAcademico.btnAtualizarDiscClick(Sender: TObject);
var
  Codigo: Integer;
  Nome: string;
begin
  try
    if not TryStrToInt(edtCodigoDisc.Text, Codigo) then
    begin
      ShowMessage('Selecione uma disciplina para atualizar!');
      Exit;
    end;
    Nome := Trim(edtNomeDisc.Text);
    if Nome = '' then
    begin
      ShowMessage('Nome é obrigatório!');
      edtNomeDisc.SetFocus;
      Exit;
    end;

    if FDisciplinaControlador.Atualizar(Codigo, Nome) then
    begin
      ShowMessage('Disciplina atualizada com sucesso!');
      LimparCamposDisciplina;
      AtualizarListaDisciplinas;
    end
    else
      ShowMessage('Erro: Disciplina não encontrada!');
  except
    on E: Exception do
      ShowMessage('Erro ao atualizar disciplina: ' + E.Message);
  end;
end;

procedure TFSistemaAcademico.btnExcluirDiscClick(Sender: TObject);
var
  Codigo: Integer;
begin
  try
    if not TryStrToInt(edtCodigoDisc.Text, Codigo) then
    begin
      ShowMessage('Selecione uma disciplina para excluir!');
      Exit;
    end;

    if MessageDlg('Confirma a exclusão da disciplina?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      if FDisciplinaControlador.Excluir(Codigo) then
      begin
        ShowMessage('Disciplina excluída com sucesso!');
        LimparCamposDisciplina;
        AtualizarListaDisciplinas;
      end
      else
        ShowMessage('Erro: Disciplina não encontrada!');
    end;
  except
    on E: Exception do
      ShowMessage('Erro ao excluir disciplina: ' + E.Message);
  end;
end;

procedure TFSistemaAcademico.btnBuscarDiscClick(Sender: TObject);
var
  Codigo: Integer;
  disciplina: TDisciplina;
begin
  try
    if not TryStrToInt(edtCodigoDisc.Text, Codigo) then
    begin
      ShowMessage('Digite um código válido para buscar!');
      edtCodigoDisc.SetFocus;
      Exit;
    end;

    disciplina := FDisciplinaControlador.BuscarPorCodigo(Codigo);
    if Assigned(disciplina) then
      CarregarDadosDisciplina(disciplina)
    else
      ShowMessage('Disciplina não encontrada!');
  except
    on E: Exception do
      ShowMessage('Erro ao buscar disciplina: ' + E.Message);
  end;
end;

procedure TFSistemaAcademico.lbxDisciplinasClick(Sender: TObject);
var
  linha: string;
  Codigo: Integer;
  disciplina: TDisciplina;
begin
  try
    if lbxDisciplinas.ItemIndex >= 0 then
    begin
      linha := lbxDisciplinas.Items[lbxDisciplinas.ItemIndex];
      if Pos('Código: ', linha) > 0 then
      begin
        linha := Copy(linha, Pos('Código: ', linha) + 8, Length(linha));
        linha := Copy(linha, 1, Pos(' -', linha) - 1);
        if TryStrToInt(linha, Codigo) then
        begin
          disciplina := FDisciplinaControlador.BuscarPorCodigo(Codigo);
          CarregarDadosDisciplina(disciplina);
        end;
      end;
    end;
  except
    on E: Exception do
      ShowMessage('Erro ao selecionar disciplina: ' + E.Message);
  end;
end;

// --------------------------------------------------------------------------------------------------
// Eventos de Turmas (com AtualizarListaTurmas)
// --------------------------------------------------------------------------------------------------
procedure TFSistemaAcademico.btnIncluirTurmaClick(Sender: TObject);
var
  Codigo, codigoProf, codigoDisc: Integer;
begin
  try
    if not TryStrToInt(edtCodigoTurma.Text, Codigo) then
    begin
      ShowMessage('Código deve ser um número inteiro válido!');
      edtCodigoTurma.SetFocus;
      Exit;
    end;
    if not TryStrToInt(edtCodigoProfTurma.Text, codigoProf) then
    begin
      ShowMessage('Código do Professor deve ser um número inteiro válido!');
      edtCodigoProfTurma.SetFocus;
      Exit;
    end;
    if not TryStrToInt(edtCodigoDiscTurma.Text, codigoDisc) then
    begin
      ShowMessage('Código da Disciplina deve ser um número inteiro válido!');
      edtCodigoDiscTurma.SetFocus;
      Exit;
    end;

    // Verifica a existência do professor e da disciplina antes de incluir a turma.
    if not Assigned(FProfessorControlador.BuscarPorCodigo(codigoProf)) then
    begin
      ShowMessage('Professor não encontrado!');
      edtCodigoProfTurma.SetFocus;
      Exit;
    end;
    if not Assigned(FDisciplinaControlador.BuscarPorCodigo(codigoDisc)) then
    begin
      ShowMessage('Disciplina não encontrada!');
      edtCodigoDiscTurma.SetFocus;
      Exit;
    end;

    if FTurmaControlador.Incluir(Codigo, codigoProf, codigoDisc) then
    begin
      ShowMessage('Turma incluída com sucesso!');
      LimparCamposTurma;
      AtualizarListaTurmas;
    end;
  except
    on E: Exception do
      ShowMessage('Erro ao incluir turma: ' + E.Message);
  end;
end;

procedure TFSistemaAcademico.btnAtualizarTurmaClick(Sender: TObject);
var
  Codigo, codigoProf, codigoDisc: Integer;
begin
  try
    if not TryStrToInt(edtCodigoTurma.Text, Codigo) then
    begin
      ShowMessage('Selecione uma turma para atualizar!');
      Exit;
    end;
    if not TryStrToInt(edtCodigoProfTurma.Text, codigoProf) then
    begin
      ShowMessage('Código do Professor deve ser um número inteiro válido!');
      edtCodigoProfTurma.SetFocus;
      Exit;
    end;
    if not TryStrToInt(edtCodigoDiscTurma.Text, codigoDisc) then
    begin
      ShowMessage('Código da Disciplina deve ser um número inteiro válido!');
      edtCodigoDiscTurma.SetFocus;
      Exit;
    end;

    // Verifica a existência do professor e da disciplina antes de atualizar a turma.
    if not Assigned(FProfessorControlador.BuscarPorCodigo(codigoProf)) then
    begin
      ShowMessage('Professor não encontrado!');
      edtCodigoProfTurma.SetFocus;
      Exit;
    end;
    if not Assigned(FDisciplinaControlador.BuscarPorCodigo(codigoDisc)) then
    begin
      ShowMessage('Disciplina não encontrada!');
      edtCodigoDiscTurma.SetFocus;
      Exit;
    end;

    if FTurmaControlador.Atualizar(Codigo, codigoProf, codigoDisc) then
    begin
      ShowMessage('Turma atualizada com sucesso!');
      LimparCamposTurma;
      AtualizarListaTurmas;
    end
    else
      ShowMessage('Erro: Turma não encontrada!');
  except
    on E: Exception do
      ShowMessage('Erro ao atualizar turma: ' + E.Message);
  end;
end;

procedure TFSistemaAcademico.btnExcluirTurmaClick(Sender: TObject);
var
  Codigo: Integer;
begin
  try
    if not TryStrToInt(edtCodigoTurma.Text, Codigo) then
    begin
      ShowMessage('Selecione uma turma para excluir!');
      Exit;
    end;

    if MessageDlg('Confirma a exclusão da turma?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      if FTurmaControlador.Excluir(Codigo) then
      begin
        ShowMessage('Turma excluída com sucesso!');
        LimparCamposTurma;
        AtualizarListaTurmas;
      end
      else
        ShowMessage('Erro: Turma não encontrada!');
    end;
  except
    on E: Exception do
      ShowMessage('Erro ao excluir turma: ' + E.Message);
  end;
end;

procedure TFSistemaAcademico.btnBuscarTurmaClick(Sender: TObject);
var
  Codigo: Integer;
  turma: TTurma;
begin
  try
    if not TryStrToInt(edtCodigoTurma.Text, Codigo) then
    begin
      ShowMessage('Digite um código válido para buscar!');
      edtCodigoTurma.SetFocus;
      Exit;
    end;

    turma := FTurmaControlador.BuscarPorCodigo(Codigo);
    if Assigned(turma) then
      CarregarDadosTurma(turma)
    else
      ShowMessage('Turma não encontrada!');
  except
    on E: Exception do
      ShowMessage('Erro ao buscar turma: ' + E.Message);
  end;
end;

procedure TFSistemaAcademico.lbxTurmasClick(Sender: TObject);
var
  linha: string;
  Codigo: Integer;
  turma: TTurma;
begin
  try
    if lbxTurmas.ItemIndex >= 0 then
    begin
      linha := lbxTurmas.Items[lbxTurmas.ItemIndex];
      if Pos('Código: ', linha) > 0 then
      begin
        linha := Copy(linha, Pos('Código: ', linha) + 8, Length(linha));
        linha := Copy(linha, 1, Pos(' -', linha) - 1);
        if TryStrToInt(linha, Codigo) then
        begin
          turma := FTurmaControlador.BuscarPorCodigo(Codigo);
          CarregarDadosTurma(turma);
        end;
      end;
    end;
  except
    on E: Exception do
      ShowMessage('Erro ao selecionar turma: ' + E.Message);
  end;
end;

// --------------------------------------------------------------------------------------------------
// Eventos de Matrículas (com AtualizarListaMatriculas)
// --------------------------------------------------------------------------------------------------
procedure TFSistemaAcademico.btnIncluirMatClick(Sender: TObject);
var
  Codigo, CodigoTurma, codigoEst: Integer;
begin
  try
    if not TryStrToInt(edtCodigoMat.Text, Codigo) then
    begin
      ShowMessage('Código deve ser um número inteiro válido!');
      edtCodigoMat.SetFocus;
      Exit;
    end;
    if not TryStrToInt(edtCodigoTurmaMat.Text, CodigoTurma) then
    begin
      ShowMessage('Código da Turma deve ser um número inteiro válido!');
      edtCodigoTurmaMat.SetFocus;
      Exit;
    end;
    if not TryStrToInt(edtCodigoEstMat.Text, codigoEst) then
    begin
      ShowMessage('Código do Estudante deve ser um número inteiro válido!');
      edtCodigoEstMat.SetFocus;
      Exit;
    end;

    // Verifica a existência da turma e do estudante antes de incluir a matrícula.
    if not Assigned(FTurmaControlador.BuscarPorCodigo(CodigoTurma)) then
    begin
      ShowMessage('Turma não encontrada!');
      edtCodigoTurmaMat.SetFocus;
      Exit;
    end;
    if not Assigned(FEstudanteControlador.BuscarPorCodigo(codigoEst)) then
    begin
      ShowMessage('Estudante não encontrado!');
      edtCodigoEstMat.SetFocus;
      Exit;
    end;

    if FMatriculaControlador.Incluir(Codigo, CodigoTurma, codigoEst) then
    begin
      ShowMessage('Matrícula incluída com sucesso!');
      LimparCamposMatricula;
      AtualizarListaMatriculas;
    end;
  except
    on E: Exception do
      ShowMessage('Erro ao incluir matrícula: ' + E.Message);
  end;
end;

procedure TFSistemaAcademico.btnAtualizarMatClick(Sender: TObject);
var
  Codigo, CodigoTurma, codigoEst: Integer;
begin
  try
    if not TryStrToInt(edtCodigoMat.Text, Codigo) then
    begin
      ShowMessage('Selecione uma matrícula para atualizar!');
      Exit;
    end;
    if not TryStrToInt(edtCodigoTurmaMat.Text, CodigoTurma) then
    begin
      ShowMessage('Código da Turma deve ser um número inteiro válido!');
      edtCodigoTurmaMat.SetFocus;
      Exit;
    end;
    if not TryStrToInt(edtCodigoEstMat.Text, codigoEst) then
    begin
      ShowMessage('Código do Estudante deve ser um número inteiro válido!');
      edtCodigoEstMat.SetFocus;
      Exit;
    end;

    // Verifica a existência da turma e do estudante antes de atualizar a matrícula.
    if not Assigned(FTurmaControlador.BuscarPorCodigo(CodigoTurma)) then
    begin
      ShowMessage('Turma não encontrada!');
      edtCodigoTurmaMat.SetFocus;
      Exit;
    end;
    if not Assigned(FEstudanteControlador.BuscarPorCodigo(codigoEst)) then
    begin
      ShowMessage('Estudante não encontrado!');
      edtCodigoEstMat.SetFocus;
      Exit;
    end;

    if FMatriculaControlador.Atualizar(Codigo, CodigoTurma, codigoEst) then
    begin
      ShowMessage('Matrícula atualizada com sucesso!');
      LimparCamposMatricula;
      AtualizarListaMatriculas;
    end
    else
      ShowMessage('Erro: Matrícula não encontrada!');
  except
    on E: Exception do
      ShowMessage('Erro ao atualizar matrícula: ' + E.Message);
  end;
end;

procedure TFSistemaAcademico.btnExcluirMatClick(Sender: TObject);
var
  Codigo: Integer;
begin
  try
    if not TryStrToInt(edtCodigoMat.Text, Codigo) then
    begin
      ShowMessage('Selecione uma matrícula para excluir!');
      Exit;
    end;

    if MessageDlg('Confirma a exclusão da matrícula?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      if FMatriculaControlador.Excluir(Codigo) then
      begin
        ShowMessage('Matrícula excluída com sucesso!');
        LimparCamposMatricula;
        AtualizarListaMatriculas;
      end
      else
        ShowMessage('Erro: Matrícula não encontrada!');
    end;
  except
    on E: Exception do
      ShowMessage('Erro ao excluir matrícula: ' + E.Message);
  end;
end;

procedure TFSistemaAcademico.btnBuscarMatClick(Sender: TObject);
var
  Codigo: Integer;
  matricula: TMatricula;
begin
  try
    if not TryStrToInt(edtCodigoMat.Text, Codigo) then
    begin
      ShowMessage('Digite um código válido para buscar!');
      edtCodigoMat.SetFocus;
      Exit;
    end;

    matricula := FMatriculaControlador.BuscarPorCodigo(Codigo);
    if Assigned(matricula) then
      CarregarDadosMatricula(matricula)
    else
      ShowMessage('Matrícula não encontrada!');
  except
    on E: Exception do
      ShowMessage('Erro ao buscar matrícula: ' + E.Message);
  end;
end;

procedure TFSistemaAcademico.lbxMatriculasClick(Sender: TObject);
var
  linha: string;
  Codigo: Integer;
  matricula: TMatricula;
begin
  try
    if lbxMatriculas.ItemIndex >= 0 then
    begin
      linha := lbxMatriculas.Items[lbxMatriculas.ItemIndex];
      if Pos('Código: ', linha) > 0 then
      begin
        linha := Copy(linha, Pos('Código: ', linha) + 8, Length(linha));
        linha := Copy(linha, 1, Pos(' -', linha) - 1);
        if TryStrToInt(linha, Codigo) then
        begin
          matricula := FMatriculaControlador.BuscarPorCodigo(Codigo);
          CarregarDadosMatricula(matricula);
        end;
      end;
    end;
  except
    on E: Exception do
      ShowMessage('Erro ao selecionar matrícula: ' + E.Message);
  end;
end;

// --- MÉTODOS btnListar...Click REMOVIDOS ---

end.
