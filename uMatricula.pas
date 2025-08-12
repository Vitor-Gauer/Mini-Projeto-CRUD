unit uMatricula;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections,
  uEstudante, uProfessor, uDisciplina, uTurma;

type
  // Classe modelo para a entidade Matrícula.
  TMatricula = class
  private
    FCodigo: Integer;
    FCodigoTurma: Integer;
    FCodigoEstudante: Integer;
  public
    constructor Create(aCodigo, aCodigoTurma, aCodigoEstudante: Integer); // Construtor para inicializar uma nova instância de TMatricula com código, código da turma e código do estudante.
    property Codigo: Integer read FCodigo write FCodigo; // Propriedade de acesso para o código da matrícula (leitura e escrita).
    property CodigoTurma: Integer read FCodigoTurma write FCodigoTurma; // Propriedade de acesso para o código da turma (leitura e escrita).
    property CodigoEstudante: Integer read FCodigoEstudante write FCodigoEstudante; // Propriedade de acesso para o código do estudante (leitura e escrita).
    function ToString: string; override; // Sobrescreve o método padrão para retornar uma representação em string do objeto.
  end;

  // Controlador para gerenciar as operações de CRUD da entidade Matrícula.
  TMatriculaControlador = class
  private
    FLista: TObjectList<TMatricula>; // Gerencia uma lista de objetos TMatricula, liberando a memória automaticamente.
    FArquivo: string; // Armazena o nome do arquivo onde os dados serão persistidos.
    function BuscarIndice(aCodigo: Integer): Integer; // Função auxiliar para encontrar o índice de uma matrícula na lista através do código.
    function ObterProximoCodigo: Integer; // Função auxiliar que encontra e retorna o próximo código sequencial disponível.
    function ExisteCombinacao(aCodigoTurma, aCodigoEstudante: Integer): Boolean; // Verifica se já existe uma matrícula para a combinação de turma e estudante fornecida.
  public
    constructor Create; // Construtor do controlador.
    destructor Destroy; override; // Destrutor, responsável por liberar a memória da lista.

    function Incluir(aCodigo: Integer; aCodigoTurma, aCodigoEstudante: Integer): Boolean; // Adiciona uma nova matrícula à lista e salva os dados no arquivo.
    function Atualizar(aCodigo, aCodigoTurma, aCodigoEstudante: Integer): Boolean; // Atualiza os códigos da turma e do estudante de uma matrícula existente e salva os dados.
    function Excluir(aCodigo: Integer): Boolean; // Remove uma matrícula da lista e salva os dados.
    function BuscarPorCodigo(aCodigo: Integer): TMatricula; // Busca e retorna uma matrícula específica pelo código.
    procedure Listar(AStringList: TStrings; ATurmaControlador: TTurmaControlador; AEstudanteControlador: TEstudanteControlador; AProfessorControlador: TProfessorControlador; ADisciplinaControlador: TDisciplinaControlador); // Preenche um TStrings (como de um TListBox) com a representação em string de cada matrícula.

    procedure CarregarDados; // Carrega os dados das matrículas do arquivo para a lista.
    procedure SalvarDados; // Salva os dados da lista de matrículas no arquivo.

    // Novo método: Retorna a lista completa para iteração em relatórios
    function ObterTodos: TObjectList<TMatricula>;
  end;

implementation

// --------------------------------------------------------------------------------------------------
// TMatricula
// --------------------------------------------------------------------------------------------------

constructor TMatricula.Create(aCodigo, aCodigoTurma, aCodigoEstudante: Integer);
begin
  inherited Create;
  FCodigo := aCodigo;
  FCodigoTurma := aCodigoTurma;
  FCodigoEstudante := aCodigoEstudante;
end;

function TMatricula.ToString: string;
begin
  Result := Format('Código: %d - Turma: %d - Estudante: %d',
    [FCodigo, FCodigoTurma, FCodigoEstudante]);
end;

// --------------------------------------------------------------------------------------------------
// TMatriculaControlador
// --------------------------------------------------------------------------------------------------

constructor TMatriculaControlador.Create;
begin
  inherited Create;
  FLista := TObjectList<TMatricula>.Create(True);
  FArquivo := 'matriculas.txt';
end;

destructor TMatriculaControlador.Destroy;
begin
  FLista.Free;
  inherited Destroy;
end;

function TMatriculaControlador.BuscarIndice(aCodigo: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FLista.Count - 1 do
  begin
    if FLista[i].Codigo = aCodigo then
    begin
      Result := i;
      Break;
    end;
  end;
end;

function TMatriculaControlador.ObterProximoCodigo: Integer;
var
  i: Integer;
  maiorCodigo: Integer;
begin
  maiorCodigo := 0;
  for i := 0 to FLista.Count - 1 do
  begin
    if FLista[i].Codigo > maiorCodigo then
      maiorCodigo := FLista[i].Codigo;
  end;
  Result := maiorCodigo + 1;
end;

function TMatriculaControlador.ExisteCombinacao(aCodigoTurma, aCodigoEstudante: Integer): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to FLista.Count - 1 do
  begin
    if (FLista[i].CodigoTurma = aCodigoTurma) and
       (FLista[i].CodigoEstudante = aCodigoEstudante) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TMatriculaControlador.Incluir(aCodigo: Integer; aCodigoTurma, aCodigoEstudante: Integer): Boolean;
var
  matricula: TMatricula;
begin
  Result := False;
  if BuscarIndice(aCodigo) <> -1 then
    raise Exception.Create('Código da matrícula já existe');
  if aCodigoTurma <= 0 then
    raise Exception.Create('Código da turma deve ser maior que zero');
  if aCodigoEstudante <= 0 then
    raise Exception.Create('Código do estudante deve ser maior que zero');
  if ExisteCombinacao(aCodigoTurma, aCodigoEstudante) then
    raise Exception.Create('Estudante já está matriculado nesta turma');

  try
    matricula := TMatricula.Create(aCodigo, aCodigoTurma, aCodigoEstudante);
    FLista.Add(matricula);
    SalvarDados;
    Result := True;
  except
    on E: Exception do
      raise Exception.Create('Erro ao incluir matrícula: ' + E.Message);
  end;
end;

function TMatriculaControlador.Atualizar(aCodigo, aCodigoTurma, aCodigoEstudante: Integer): Boolean;
var
  indice: Integer;
  i: Integer;
begin
  Result := False;
  indice := BuscarIndice(aCodigo);
  if indice < 0 then
    raise Exception.Create('Matrícula não encontrada');
  if aCodigoTurma <= 0 then
    raise Exception.Create('Código da turma deve ser maior que zero');
  if aCodigoEstudante <= 0 then
    raise Exception.Create('Código do estudante deve ser maior que zero');

  for i := 0 to FLista.Count - 1 do
  begin
    if (i <> indice) and (FLista[i].CodigoTurma = aCodigoTurma) and (FLista[i].CodigoEstudante = aCodigoEstudante) then
      raise Exception.Create('Estudante já está matriculado nesta turma');
  end;

  try
    FLista[indice].CodigoTurma := aCodigoTurma;
    FLista[indice].CodigoEstudante := aCodigoEstudante;
    SalvarDados;
    Result := True;
  except
    on E: Exception do
      raise Exception.Create('Erro ao atualizar matrícula: ' + E.Message);
  end;
end;

function TMatriculaControlador.Excluir(aCodigo: Integer): Boolean;
var
  indice: Integer;
begin
  Result := False;
  indice := BuscarIndice(aCodigo);
  if indice >= 0 then
  begin
    try
      FLista.Delete(indice);
      SalvarDados;
      Result := True;
    except
      on E: Exception do
        raise Exception.Create('Erro ao excluir matrícula: ' + E.Message);
    end;
  end;
end;

function TMatriculaControlador.BuscarPorCodigo(aCodigo: Integer): TMatricula;
var
  indice: Integer;
begin
  Result := nil;
  indice := BuscarIndice(aCodigo);
  if indice >= 0 then
    Result := FLista[indice];
end;

procedure TMatriculaControlador.Listar(AStringList: TStrings; ATurmaControlador: TTurmaControlador; AEstudanteControlador: TEstudanteControlador; AProfessorControlador: TProfessorControlador; ADisciplinaControlador: TDisciplinaControlador);
var
  matricula: TMatricula;
  NomeEstudante: string;
  NomeTurma: string; // Nome da disciplina da turma
  Estudante: TEstudante;
  Turma: TTurma;
  Disciplina: TDisciplina;
begin
  AStringList.Clear;
  for matricula in FLista do
  begin
    NomeEstudante := 'Estudante não encontrado'; // Valor padrão
    NomeTurma := 'Turma/Disciplina não encontrada'; // Valor padrão

    // Busca o Estudante
    Estudante := AEstudanteControlador.BuscarPorCodigo(matricula.CodigoEstudante);
    if Assigned(Estudante) then
      NomeEstudante := Estudante.Nome;

    // Busca a Turma para obter o código da disciplina
    Turma := ATurmaControlador.BuscarPorCodigo(matricula.CodigoTurma);
    if Assigned(Turma) then
    begin
      // Busca a Disciplina da Turma para obter o nome
      Disciplina := ADisciplinaControlador.BuscarPorCodigo(Turma.CodigoDisciplina);
      if Assigned(Disciplina) then
        NomeTurma := Disciplina.Nome
      else
        NomeTurma := Format('Disciplina Cód: %d', [Turma.CodigoDisciplina]); // Fallback
    end
    else
    begin
       NomeTurma := Format('Turma Cód: %d', [matricula.CodigoTurma]); // Fallback
    end;

    // Formata a string para exibição rica
    AStringList.Add(Format('Matrícula (Cód: %d) - Estudante: %s (Cód: %d) - Turma: %s (Cód: %d)',
      [matricula.Codigo, NomeEstudante, matricula.CodigoEstudante, NomeTurma, matricula.CodigoTurma]));
  end;
end;

procedure TMatriculaControlador.CarregarDados;
var
  F: TFileStream;
  matricula: TMatricula;
  codigo, codigoTurma, codigoEstudante: Integer;
begin
  if not FileExists(FArquivo) then Exit;
  F := TFileStream.Create(FArquivo, fmOpenRead);
  try
    FLista.Clear;
    while F.Position < F.Size do
    begin
      F.Read(codigo, SizeOf(Integer));
      F.Read(codigoTurma, SizeOf(Integer));
      F.Read(codigoEstudante, SizeOf(Integer));
      matricula := TMatricula.Create(codigo, codigoTurma, codigoEstudante);
      FLista.Add(matricula);
    end;
  finally
    F.Free;
  end;
end;

procedure TMatriculaControlador.SalvarDados;
var
  F: TFileStream;
  matricula: TMatricula;
begin
  F := TFileStream.Create(FArquivo, fmCreate);
  try
    for matricula in FLista do
    begin
      F.Write(matricula.Codigo, SizeOf(Integer));
      F.Write(matricula.CodigoTurma, SizeOf(Integer));
      F.Write(matricula.CodigoEstudante, SizeOf(Integer));
    end;
  finally
    F.Free;
  end;
end;

// --------------------------------------------------------------------------------------------------
// Novo método: ObterTodos
// --------------------------------------------------------------------------------------------------

function TMatriculaControlador.ObterTodos: TObjectList<TMatricula>;
begin
  Result := FLista;
end;

end.
