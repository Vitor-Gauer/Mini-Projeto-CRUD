unit SistemaAcademico;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.Grids, Vcl.ExtCtrls, Vcl.ComCtrls,
  uEstudante, uProfessor, uDisciplina, uTurma, uMatricula;

type
  TFSistemaAcademico = class(TForm)
    pgcAbasDoForm: TPageControl;
    tabEstudantes: TTabSheet;
    tabProfessores: TTabSheet;
    tabDisciplinas: TTabSheet;
    tabTurmas: TTabSheet;
    tabMatriculas: TTabSheet;

    // Estudantes
    pnlEstudantes: TPanel;
    lblCodigoEst: TLabel;
    lblNomeEst: TLabel;
    edtCodigoEst: TEdit;
    edtNomeEst: TEdit;
    btnIncluirEst: TButton;
    btnListarEst: TButton;
    btnAtualizarEst: TButton;
    btnExcluirEst: TButton;
    btnBuscarEst: TButton;
    lbxEstudantes: TListBox;

    // Professores
    pnlProfessores: TPanel;
    lblCodigoProf: TLabel;
    lblNomeProf: TLabel;
    lblCPFProf: TLabel;
    edtCodigoProf: TEdit;
    edtNomeProf: TEdit;
    edtCPFProf: TEdit;
    btnIncluirProf: TButton;
    btnListarProf: TButton;
    btnAtualizarProf: TButton;
    btnExcluirProf: TButton;
    btnBuscarProf: TButton;
    lbxProfessores: TListBox;

    // Disciplinas
    pnlDisciplinas: TPanel;
    lblCodigoDisc: TLabel;
    lblNomeDisc: TLabel;
    edtCodigoDisc: TEdit;
    edtNomeDisc: TEdit;
    btnIncluirDisc: TButton;
    btnListarDisc: TButton;
    btnAtualizarDisc: TButton;
    btnExcluirDisc: TButton;
    btnBuscarDisc: TButton;
    lbxDisciplinas: TListBox;

    // Turmas
    pnlTurmas: TPanel;
    lblCodigoTurma: TLabel;
    lblCodigoProfTurma: TLabel;
    lblCodigoDiscTurma: TLabel;
    edtCodigoTurma: TEdit;
    edtCodigoProfTurma: TEdit;
    edtCodigoDiscTurma: TEdit;
    btnIncluirTurma: TButton;
    btnListarTurma: TButton;
    btnAtualizarTurma: TButton;
    btnExcluirTurma: TButton;
    btnBuscarTurma: TButton;
    lbxTurmas: TListBox;

    // Matriculas
    pnlMatriculas: TPanel;
    lblCodigoMat: TLabel;
    lblCodigoTurmaMat: TLabel;
    lblCodigoEstMat: TLabel;
    edtCodigoMat: TEdit;
    edtCodigoTurmaMat: TEdit;
    edtCodigoEstMat: TEdit;
    btnIncluirMat: TButton;
    btnListarMat: TButton;
    btnAtualizarMat: TButton;
    btnExcluirMat: TButton;
    btnBuscarMat: TButton;
    lbxMatriculas: TListBox;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

    // Eventos Estudantes
    procedure btnIncluirEstClick(Sender: TObject);
    procedure btnListarEstClick(Sender: TObject);
    procedure btnAtualizarEstClick(Sender: TObject);
    procedure btnExcluirEstClick(Sender: TObject);
    procedure btnBuscarEstClick(Sender: TObject);
    procedure lbxEstudantesClick(Sender: TObject);

    // Eventos Professores
    procedure btnIncluirProfClick(Sender: TObject);
    procedure btnListarProfClick(Sender: TObject);
    procedure btnAtualizarProfClick(Sender: TObject);
    procedure btnExcluirProfClick(Sender: TObject);
    procedure btnBuscarProfClick(Sender: TObject);
    procedure lbxProfessoresClick(Sender: TObject);

    // Eventos Disciplinas
    procedure btnIncluirDiscClick(Sender: TObject);
    procedure btnListarDiscClick(Sender: TObject);
    procedure btnAtualizarDiscClick(Sender: TObject);
    procedure btnExcluirDiscClick(Sender: TObject);
    procedure btnBuscarDiscClick(Sender: TObject);
    procedure lbxDisciplinasClick(Sender: TObject);

    // Eventos Turmas
    procedure btnIncluirTurmaClick(Sender: TObject);
    procedure btnListarTurmaClick(Sender: TObject);
    procedure btnAtualizarTurmaClick(Sender: TObject);
    procedure btnExcluirTurmaClick(Sender: TObject);
    procedure btnBuscarTurmaClick(Sender: TObject);
    procedure lbxTurmasClick(Sender: TObject);

    // Eventos Matriculas
    procedure btnIncluirMatClick(Sender: TObject);
    procedure btnListarMatClick(Sender: TObject);
    procedure btnAtualizarMatClick(Sender: TObject);
    procedure btnExcluirMatClick(Sender: TObject);
    procedure btnBuscarMatClick(Sender: TObject);
    procedure lbxMatriculasClick(Sender: TObject);

  private
    FEstudanteControlador: TEstudanteControlador;
    FProfessorControlador: TProfessorControlador;
    FDisciplinaControlador: TDisciplinaControlador;
    FTurmaControlador: TTurmaControlador;
    FMatriculaControlador: TMatriculaControlador;

    procedure LimparCamposEstudante;
    procedure LimparCamposProfessor;
    procedure LimparCamposDisciplina;
    procedure LimparCamposTurma;
    procedure LimparCamposMatricula;

    procedure CarregarDadosEstudante(estudante: TEstudante);
    procedure CarregarDadosProfessor(professor: TProfessor);
    procedure CarregarDadosDisciplina(disciplina: TDisciplina);
    procedure CarregarDadosTurma(turma: TTurma);
    procedure CarregarDadosMatricula(matricula: TMatricula);

  public
    { Public declarations }
  end;

var
  FormPrincipal: TFSistemaAcademico;

implementation

{$R *.dfm}

// --------------------------------------------------------------------------------------------------
// Eventos do Formulário Principal
// --------------------------------------------------------------------------------------------------

procedure TFSistemaAcademico.FormCreate(Sender: TObject);
begin
  try
    // Inicializa os Controladores
    FEstudanteControlador := TEstudanteControlador.Create;
    FProfessorControlador := TProfessorControlador.Create;
    FDisciplinaControlador := TDisciplinaControlador.Create;
    FTurmaControlador := TTurmaControlador.Create;
    FMatriculaControlador := TMatriculaControlador.Create;

    // Carrega todos os dados dos arquivos
    FEstudanteControlador.CarregarDados;
    FProfessorControlador.CarregarDados;
    FDisciplinaControlador.CarregarDados;
    FTurmaControlador.CarregarDados;
    FMatriculaControlador.CarregarDados;

    // Atualiza as listas
    btnListarEstClick(nil);
    btnListarProfClick(nil);
    btnListarDiscClick(nil);
    btnListarTurmaClick(nil);
    btnListarMatClick(nil);

  except
    on E: Exception do
      ShowMessage('Erro ao inicializar sistema: ' + E.Message);
  end;
end;

procedure TFSistemaAcademico.FormDestroy(Sender: TObject);
begin
  try
    // Salva todos os dados antes de fechar
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
// Eventos Estudantes
// --------------------------------------------------------------------------------------------------

procedure TFSistemaAcademico.btnIncluirEstClick(Sender: TObject);
var
  codigo: Integer;
  nome: string;
begin
  try
    // Validação de dados
    if not TryStrToInt(edtCodigoEst.Text, codigo) then
    begin
      ShowMessage('Código deve ser um número inteiro válido!');
      edtCodigoEst.SetFocus;
      Exit;
    end;

    nome := Trim(edtNomeEst.Text);
    if nome = '' then
    begin
      ShowMessage('Nome é obrigatório!');
      edtNomeEst.SetFocus;
      Exit;
    end;

    // Inclui o estudante
    if FEstudanteControlador.Incluir(codigo, nome) then
    begin
      ShowMessage('Estudante incluído com sucesso!');
      LimparCamposEstudante;
      btnListarEstClick(nil);
      FEstudanteControlador.SalvarDados;
    end
    else
      ShowMessage('Erro: Código já existe!');

  except
    on E: Exception do
      ShowMessage('Erro ao incluir estudante: ' + E.Message);
  end;
end;

procedure TFSistemaAcademico.btnListarEstClick(Sender: TObject);
begin
  try
    FEstudanteControlador.Listar(lbxEstudantes.Items);
  except
    on E: Exception do
      ShowMessage('Erro ao listar estudantes: ' + E.Message);
  end;
end;

procedure TFSistemaAcademico.btnAtualizarEstClick(Sender: TObject);
var
  codigo: Integer;
  nome: string;
begin
  try
    if not TryStrToInt(edtCodigoEst.Text, codigo) then
    begin
      ShowMessage('Selecione um estudante para atualizar!');
      Exit;
    end;

    nome := Trim(edtNomeEst.Text);
    if nome = '' then
    begin
      ShowMessage('Nome é obrigatório!');
      edtNomeEst.SetFocus;
      Exit;
    end;

    if FEstudanteControlador.Atualizar(codigo, nome) then
    begin
      ShowMessage('Estudante atualizado com sucesso!');
      LimparCamposEstudante;
      btnListarEstClick(nil);
      FEstudanteControlador.SalvarDados;
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
  codigo: Integer;
begin
  try
    if not TryStrToInt(edtCodigoEst.Text, codigo) then
    begin
      ShowMessage('Selecione um estudante para excluir!');
      Exit;
    end;

    if MessageDlg('Confirma a exclusão do estudante?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      if FEstudanteControlador.Excluir(codigo) then
      begin
        ShowMessage('Estudante excluído com sucesso!');
        LimparCamposEstudante;
        btnListarEstClick(nil);
        FEstudanteControlador.SalvarDados;
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
  codigo: Integer;
  estudante: TEstudante;
begin
  try
    if not TryStrToInt(edtCodigoEst.Text, codigo) then
    begin
      ShowMessage('Digite um código válido para buscar!');
      edtCodigoEst.SetFocus;
      Exit;
    end;

    estudante := FEstudanteControlador.BuscarPorCodigo(codigo);
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
  codigo: Integer;
  estudante: TEstudante;
begin
  try
    if lbxEstudantes.ItemIndex >= 0 then
    begin
      linha := lbxEstudantes.Items[lbxEstudantes.ItemIndex];
      // Extrai o código da linha (formato: "Código: X - Nome: Y")
      if Pos('Código: ', linha) > 0 then
      begin
        linha := Copy(linha, Pos('Código: ', linha) + 8, Length(linha));
        linha := Copy(linha, 1, Pos(' -', linha) - 1);
        if TryStrToInt(linha, codigo) then
        begin
          estudante := FEstudanteControlador.BuscarPorCodigo(codigo);
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
// Eventos Professores (Similar aos Estudantes, adaptado para Professor)
// --------------------------------------------------------------------------------------------------

procedure TFSistemaAcademico.btnIncluirProfClick(Sender: TObject);
var
  codigo: Integer;
  nome, cpf: string;
begin
  try
    if not TryStrToInt(edtCodigoProf.Text, codigo) then
    begin
      ShowMessage('Código deve ser um número inteiro válido!');
      edtCodigoProf.SetFocus;
      Exit;
    end;

    nome := Trim(edtNomeProf.Text);
    if nome = '' then
    begin
      ShowMessage('Nome é obrigatório!');
      edtNomeProf.SetFocus;
      Exit;
    end;

    cpf := Trim(edtCPFProf.Text);
    if cpf = '' then
    begin
      ShowMessage('CPF é obrigatório!');
      edtCPFProf.SetFocus;
      Exit;
    end;

    if FProfessorControlador.Incluir(codigo, nome, cpf) then
    begin
      ShowMessage('Professor incluído com sucesso!');
      LimparCamposProfessor;
      btnListarProfClick(nil);
      FProfessorControlador.SalvarDados;
    end
    else
      ShowMessage('Erro: Código já existe!');

  except
    on E: Exception do
      ShowMessage('Erro ao incluir professor: ' + E.Message);
  end;
end;

procedure TFSistemaAcademico.btnListarProfClick(Sender: TObject);
begin
  try
    FProfessorControlador.Listar(lbxProfessores.Items);
  except
    on E: Exception do
      ShowMessage('Erro ao listar professores: ' + E.Message);
  end;
end;

procedure TFSistemaAcademico.btnAtualizarProfClick(Sender: TObject);
var
  codigo: Integer;
  nome, cpf: string;
begin
  try
    if not TryStrToInt(edtCodigoProf.Text, codigo) then
    begin
      ShowMessage('Selecione um professor para atualizar!');
      Exit;
    end;

    nome := Trim(edtNomeProf.Text);
    if nome = '' then
    begin
      ShowMessage('Nome é obrigatório!');
      edtNomeProf.SetFocus;
      Exit;
    end;

    cpf := Trim(edtCPFProf.Text);
    if cpf = '' then
    begin
      ShowMessage('CPF é obrigatório!');
      edtCPFProf.SetFocus;
      Exit;
    end;

    if FProfessorControlador.Atualizar(codigo, nome, cpf) then
    begin
      ShowMessage('Professor atualizado com sucesso!');
      LimparCamposProfessor;
      btnListarProfClick(nil);
      FProfessorControlador.SalvarDados;
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
  codigo: Integer;
begin
  try
    if not TryStrToInt(edtCodigoProf.Text, codigo) then
    begin
      ShowMessage('Selecione um professor para excluir!');
      Exit;
    end;

    if MessageDlg('Confirma a exclusão do professor?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      if FProfessorControlador.Excluir(codigo) then
      begin
        ShowMessage('Professor excluído com sucesso!');
        LimparCamposProfessor;
        btnListarProfClick(nil);
        FProfessorControlador.SalvarDados;
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
  codigo: Integer;
  professor: TProfessor;
begin
  try
    if not TryStrToInt(edtCodigoProf.Text, codigo) then
    begin
      ShowMessage('Digite um código válido para buscar!');
      edtCodigoProf.SetFocus;
      Exit;
    end;

    professor := FProfessorControlador.BuscarPorCodigo(codigo);
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
  codigo: Integer;
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
        if TryStrToInt(linha, codigo) then
        begin
          professor := FProfessorControlador.BuscarPorCodigo(codigo);
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
// Eventos Disciplinas
// --------------------------------------------------------------------------------------------------

procedure TFSistemaAcademico.btnIncluirDiscClick(Sender: TObject);
var
  codigo: Integer;
  nome: string;
begin
  try
    if not TryStrToInt(edtCodigoDisc.Text, codigo) then
    begin
      ShowMessage('Código deve ser um número inteiro válido!');
      edtCodigoDisc.SetFocus;
      Exit;
    end;

    nome := Trim(edtNomeDisc.Text);
    if nome = '' then
    begin
      ShowMessage('Nome é obrigatório!');
      edtNomeDisc.SetFocus;
      Exit;
    end;

    if FDisciplinaControlador.Incluir(codigo, nome) then
    begin
      ShowMessage('Disciplina incluída com sucesso!');
      LimparCamposDisciplina;
      btnListarDiscClick(nil);
      FDisciplinaControlador.SalvarDados;
    end
    else
      ShowMessage('Erro: Código já existe!');

  except
    on E: Exception do
      ShowMessage('Erro ao incluir disciplina: ' + E.Message);
  end;
end;

procedure TFSistemaAcademico.btnListarDiscClick(Sender: TObject);
begin
  try
    FDisciplinaControlador.Listar(lbxDisciplinas.Items);
  except
    on E: Exception do
      ShowMessage('Erro ao listar disciplinas: ' + E.Message);
  end;
end;

procedure TFSistemaAcademico.btnAtualizarDiscClick(Sender: TObject);
var
  codigo: Integer;
  nome: string;
begin
  try
    if not TryStrToInt(edtCodigoDisc.Text, codigo) then
    begin
      ShowMessage('Selecione uma disciplina para atualizar!');
      Exit;
    end;

    nome := Trim(edtNomeDisc.Text);
    if nome = '' then
    begin
      ShowMessage('Nome é obrigatório!');
      edtNomeDisc.SetFocus;
      Exit;
    end;

    if FDisciplinaControlador.Atualizar(codigo, nome) then
    begin
      ShowMessage('Disciplina atualizada com sucesso!');
      LimparCamposDisciplina;
      btnListarDiscClick(nil);
      FDisciplinaControlador.SalvarDados;
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
  codigo: Integer;
begin
  try
    if not TryStrToInt(edtCodigoDisc.Text, codigo) then
    begin
      ShowMessage('Selecione uma disciplina para excluir!');
      Exit;
    end;

    if MessageDlg('Confirma a exclusão da disciplina?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      if FDisciplinaControlador.Excluir(codigo) then
      begin
        ShowMessage('Disciplina excluída com sucesso!');
        LimparCamposDisciplina;
        btnListarDiscClick(nil);
        FDisciplinaControlador.SalvarDados;
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
  codigo: Integer;
  disciplina: TDisciplina;
begin
  try
    if not TryStrToInt(edtCodigoDisc.Text, codigo) then
    begin
      ShowMessage('Digite um código válido para buscar!');
      edtCodigoDisc.SetFocus;
      Exit;
    end;

    disciplina := FDisciplinaControlador.BuscarPorCodigo(codigo);
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
  codigo: Integer;
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
        if TryStrToInt(linha, codigo) then
begin
          disciplina := FDisciplinaControlador.BuscarPorCodigo(codigo);
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
// Eventos Turmas
// --------------------------------------------------------------------------------------------------

procedure TFSistemaAcademico.btnIncluirTurmaClick(Sender: TObject);
var
  codigo, codigoProf, codigoDisc: Integer;
begin
  try
    if not TryStrToInt(edtCodigoTurma.Text, codigo) then
    begin
      ShowMessage('Código deve ser um número inteiro válido!');
      edtCodigoTurma.SetFocus;
      Exit;
    end;

    if not TryStrToInt(edtCodigoProfTurma.Text, codigoProf) then
    begin
      ShowMessage('Código do professor deve ser um número inteiro válido!');
      edtCodigoProfTurma.SetFocus;
      Exit;
    end;

    if not TryStrToInt(edtCodigoDiscTurma.Text, codigoDisc) then
    begin
      ShowMessage('Código da disciplina deve ser um número inteiro válido!');
      edtCodigoDiscTurma.SetFocus;
      Exit;
    end;

    // Verifica se o professor existe
    if not Assigned(FProfessorControlador.BuscarPorCodigo(codigoProf)) then
    begin
      ShowMessage('Professor não encontrado!');
      edtCodigoProfTurma.SetFocus;
      Exit;
    end;

    // Verifica se a disciplina existe
    if not Assigned(FDisciplinaControlador.BuscarPorCodigo(codigoDisc)) then
    begin
      ShowMessage('Disciplina não encontrada!');
      edtCodigoDiscTurma.SetFocus;
      Exit;
    end;

    if FTurmaControlador.Incluir(codigo, codigoProf, codigoDisc) then
    begin
      ShowMessage('Turma incluída com sucesso!');
      LimparCamposTurma;
      btnListarTurmaClick(nil);
      FTurmaControlador.SalvarDados;
    end
    else
      ShowMessage('Erro: Código já existe!');

  except
    on E: Exception do
      ShowMessage('Erro ao incluir turma: ' + E.Message);
  end;
end;

procedure TFSistemaAcademico.btnListarTurmaClick(Sender: TObject);
begin
  try
    FTurmaControlador.Listar(lbxTurmas.Items);
  except
    on E: Exception do
      ShowMessage('Erro ao listar turmas: ' + E.Message);
  end;
end;

procedure TFSistemaAcademico.btnAtualizarTurmaClick(Sender: TObject);
var
  codigo, codigoProf, codigoDisc: Integer;
begin
  try
    if not TryStrToInt(edtCodigoTurma.Text, codigo) then
    begin
      ShowMessage('Selecione uma turma para atualizar!');
      Exit;
    end;

    if not TryStrToInt(edtCodigoProfTurma.Text, codigoProf) then
    begin
      ShowMessage('Código do professor deve ser um número inteiro válido!');
      edtCodigoProfTurma.SetFocus;
      Exit;
    end;

    if not TryStrToInt(edtCodigoDiscTurma.Text, codigoDisc) then
    begin
      ShowMessage('Código da disciplina deve ser um número inteiro válido!');
      edtCodigoDiscTurma.SetFocus;
      Exit;
    end;

    // Verifica se o professor existe
    if not Assigned(FProfessorControlador.BuscarPorCodigo(codigoProf)) then
    begin
      ShowMessage('Professor não encontrado!');
      edtCodigoProfTurma.SetFocus;
      Exit;
    end;

    // Verifica se a disciplina existe
    if not Assigned(FDisciplinaControlador.BuscarPorCodigo(codigoDisc)) then
    begin
      ShowMessage('Disciplina não encontrada!');
      edtCodigoDiscTurma.SetFocus;
      Exit;
    end;

    if FTurmaControlador.Atualizar(codigo, codigoProf, codigoDisc) then
    begin
      ShowMessage('Turma atualizada com sucesso!');
      LimparCamposTurma;
      btnListarTurmaClick(nil);
      FTurmaControlador.SalvarDados;
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
  codigo: Integer;
begin
  try
    if not TryStrToInt(edtCodigoTurma.Text, codigo) then
    begin
      ShowMessage('Selecione uma turma para excluir!');
      Exit;
    end;

    if MessageDlg('Confirma a exclusão da turma?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      if FTurmaControlador.Excluir(codigo) then
      begin
        ShowMessage('Turma excluída com sucesso!');
        LimparCamposTurma;
        btnListarTurmaClick(nil);
        FTurmaControlador.SalvarDados;
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
  codigo: Integer;
  turma: TTurma;
begin
  try
    if not TryStrToInt(edtCodigoTurma.Text, codigo) then
    begin
      ShowMessage('Digite um código válido para buscar!');
      edtCodigoTurma.SetFocus;
      Exit;
    end;

    turma := FTurmaControlador.BuscarPorCodigo(codigo);
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
  codigo: Integer;
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
        if TryStrToInt(linha, codigo) then
        begin
          turma := FTurmaControlador.BuscarPorCodigo(codigo);
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
// Eventos Matrículas
// --------------------------------------------------------------------------------------------------

procedure TFSistemaAcademico.btnIncluirMatClick(Sender: TObject);
var
  codigo, codigoTurma, codigoEst: Integer;
begin
  try
    if not TryStrToInt(edtCodigoMat.Text, codigo) then
    begin
      ShowMessage('Código deve ser um número inteiro válido!');
      edtCodigoMat.SetFocus;
      Exit;
    end;

    if not TryStrToInt(edtCodigoTurmaMat.Text, codigoTurma) then
    begin
      ShowMessage('Código da turma deve ser um número inteiro válido!');
      edtCodigoTurmaMat.SetFocus;
      Exit;
    end;

    if not TryStrToInt(edtCodigoEstMat.Text, codigoEst) then
    begin
      ShowMessage('Código do estudante deve ser um número inteiro válido!');
      edtCodigoEstMat.SetFocus;
      Exit;
    end;

    // Verifica se a turma existe
    if not Assigned(FTurmaControlador.BuscarPorCodigo(codigoTurma)) then
    begin
      ShowMessage('Turma não encontrada!');
      edtCodigoTurmaMat.SetFocus;
      Exit;
    end;

    // Verifica se o estudante existe
    if not Assigned(FEstudanteControlador.BuscarPorCodigo(codigoEst)) then
    begin
      ShowMessage('Estudante não encontrado!');
      edtCodigoEstMat.SetFocus;
      Exit;
    end;

    if FMatriculaControlador.Incluir(codigo, codigoTurma, codigoEst) then
    begin
      ShowMessage('Matrícula incluída com sucesso!');
      LimparCamposMatricula;
      btnListarMatClick(nil);
      FMatriculaControlador.SalvarDados;
    end
    else
      ShowMessage('Erro: Código já existe!');

  except
    on E: Exception do
      ShowMessage('Erro ao incluir matrícula: ' + E.Message);
  end;
end;

procedure TFSistemaAcademico.btnListarMatClick(Sender: TObject);
begin
  try
    FMatriculaControlador.Listar(lbxMatriculas.Items);
  except
    on E: Exception do
      ShowMessage('Erro ao listar matrículas: ' + E.Message);
  end;
end;

procedure TFSistemaAcademico.btnAtualizarMatClick(Sender: TObject);
var
  codigo, codigoTurma, codigoEst: Integer;
begin
  try
    if not TryStrToInt(edtCodigoMat.Text, codigo) then
    begin
      ShowMessage('Selecione uma matrícula para atualizar!');
      Exit;
    end;

    if not TryStrToInt(edtCodigoTurmaMat.Text, codigoTurma) then
    begin
      ShowMessage('Código da turma deve ser um número inteiro válido!');
      edtCodigoTurmaMat.SetFocus;
      Exit;
    end;

    if not TryStrToInt(edtCodigoEstMat.Text, codigoEst) then
    begin
      ShowMessage('Código do estudante deve ser um número inteiro válido!');
      edtCodigoEstMat.SetFocus;
      Exit;
    end;

    // Verifica se a turma existe
    if not Assigned(FTurmaControlador.BuscarPorCodigo(codigoTurma)) then
    begin
      ShowMessage('Turma não encontrada!');
      edtCodigoTurmaMat.SetFocus;
      Exit;
    end;

    // Verifica se o estudante existe
    if not Assigned(FEstudanteControlador.BuscarPorCodigo(codigoEst)) then
    begin
      ShowMessage('Estudante não encontrado!');
      edtCodigoEstMat.SetFocus;
      Exit;
    end;

    if FMatriculaControlador.Atualizar(codigo, codigoTurma, codigoEst) then
    begin
      ShowMessage('Matrícula atualizada com sucesso!');
      LimparCamposMatricula;
      btnListarMatClick(nil);
      FMatriculaControlador.SalvarDados;
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
  codigo: Integer;
begin
  try
    if not TryStrToInt(edtCodigoMat.Text, codigo) then
    begin
      ShowMessage('Selecione uma matrícula para excluir!');
      Exit;
    end;

    if MessageDlg('Confirma a exclusão da matrícula?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      if FMatriculaControlador.Excluir(codigo) then
      begin
        ShowMessage('Matrícula excluída com sucesso!');
        LimparCamposMatricula;
        btnListarMatClick(nil);
        FMatriculaControlador.SalvarDados;
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
  codigo: Integer;
  matricula: TMatricula;
begin
  try
    if not TryStrToInt(edtCodigoMat.Text, codigo) then
    begin
      ShowMessage('Digite um código válido para buscar!');
      edtCodigoMat.SetFocus;
      Exit;
    end;

    matricula := FMatriculaControlador.BuscarPorCodigo(codigo);
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
  codigo: Integer;
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
        if TryStrToInt(linha, codigo) then
        begin
          matricula := FMatriculaControlador.BuscarPorCodigo(codigo);
          CarregarDadosMatricula(matricula);
        end;
      end;
    end;
  except
    on E: Exception do
      ShowMessage('Erro ao selecionar matrícula: ' + E.Message);
  end;
end;

end.
