unit uProfessor;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections;

type
  // Classe modelo para a entidade Professor.
  TProfessor = class
  private
    FCodigo: Integer;
    FNome: string;
    FCPF: string;
  public
    constructor Create(aCodigo: Integer; aNome, aCPF: string); // Construtor para inicializar uma nova instância de TProfessor com código, nome e CPF.
    property Codigo: Integer read FCodigo write FCodigo; // Propriedade de acesso para o código do professor (leitura e escrita).
    property Nome: string read FNome write FNome; // Propriedade de acesso para o nome do professor (leitura e escrita).
    property CPF: string read FCPF write FCPF; // Propriedade de acesso para o CPF do professor (leitura e escrita).
    function ToString: string; override; // Sobrescreve o método padrão para retornar uma representação em string do objeto.
  end;

  // Controlador para gerenciar as operações de CRUD da entidade Professor.
  TProfessorControlador = class
  private
    FLista: TObjectList<TProfessor>; // Gerencia uma lista de objetos TProfessor, liberando a memória automaticamente.
    FArquivo: string; // Armazena o nome do arquivo onde os dados serão persistidos.
    function BuscarIndice(aCodigo: Integer): Integer; // Função auxiliar para encontrar o índice de um professor na lista através do código.
    function ObterProximoCodigo: Integer; // Função auxiliar que encontra e retorna o próximo código sequencial disponível.
    function ExisteNome(const aNome: string): Boolean; // Verifica se já existe um professor com o nome fornecido (não case-sensitive).
    function ExisteCPF(const aCPF: string): Boolean; // Verifica se já existe um professor com o CPF fornecido.
  public
    constructor Create; // Construtor do controlador.
    destructor Destroy; override; // Destrutor, responsável por liberar a memória da lista.

    function Incluir(aCodigo: Integer; const aNome, aCPF: string): Boolean; // Adiciona um novo professor à lista e salva os dados no arquivo.
    function Atualizar(aCodigo: Integer; const aNome, aCPF: string): Boolean; // Atualiza o nome e o CPF de um professor existente e salva os dados.
    function Excluir(aCodigo: Integer): Boolean; // Remove um professor da lista e salva os dados.
    function BuscarPorCodigo(aCodigo: Integer): TProfessor; // Busca e retorna um professor específico pelo código.
    procedure Listar(AStringList: TStrings); // Preenche um TStrings (como de um TListBox) com a representação em string de cada professor.

    procedure CarregarDados; // Carrega os dados dos professores do arquivo para a lista.
    procedure SalvarDados; // Salva os dados da lista de professores no arquivo.

    // Novo método: Retorna a lista completa para iteração em relatórios
    function ObterTodos: TObjectList<TProfessor>;
  end;

implementation

// --------------------------------------------------------------------------------------------------
// TProfessor
// --------------------------------------------------------------------------------------------------

constructor TProfessor.Create(aCodigo: Integer; aNome, aCPF: string);
begin
  inherited Create;
  FCodigo := aCodigo;
  FNome := aNome;
  FCPF := aCPF;
end;

function TProfessor.ToString: string;
begin
  Result := Format('Código: %d - Nome: %s - CPF: %s', [FCodigo, FNome, FCPF]);
end;

// --------------------------------------------------------------------------------------------------
// TProfessorControlador
// --------------------------------------------------------------------------------------------------

constructor TProfessorControlador.Create;
begin
  inherited Create;
  FLista := TObjectList<TProfessor>.Create(True);
  FArquivo := 'professores.txt';
end;

destructor TProfessorControlador.Destroy;
begin
  FLista.Free;
  inherited Destroy;
end;

function TProfessorControlador.BuscarIndice(aCodigo: Integer): Integer;
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

function TProfessorControlador.ObterProximoCodigo: Integer;
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

function TProfessorControlador.ExisteNome(const aNome: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to FLista.Count - 1 do
  begin
    if SameText(FLista[i].Nome, aNome) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TProfessorControlador.ExisteCPF(const aCPF: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to FLista.Count - 1 do
  begin
    if FLista[i].CPF = aCPF then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TProfessorControlador.Incluir(aCodigo: Integer; const aNome, aCPF: string): Boolean;
begin
  Result := False;
  if BuscarIndice(aCodigo) <> -1 then
    raise Exception.Create('Código de professor já existe');
  if Trim(aNome) = '' then
    raise Exception.Create('Nome do professor não pode estar vazio');
  if Trim(aCPF) = '' then
    raise Exception.Create('CPF do professor não pode estar vazio');
  if ExisteNome(aNome) then
    raise Exception.Create('Já existe um professor com este nome');
  if ExisteCPF(aCPF) then
    raise Exception.Create('Já existe um professor com este CPF');

  try
    FLista.Add(TProfessor.Create(aCodigo, aNome, aCPF));
    SalvarDados;
    Result := True;
  except
    on E: Exception do
      raise Exception.Create('Erro ao incluir professor: ' + E.Message);
  end;
end;

function TProfessorControlador.Atualizar(aCodigo: Integer; const aNome, aCPF: string): Boolean;
var
  indice: Integer;
  i: Integer;
begin
  Result := False;
  indice := BuscarIndice(aCodigo);
  if indice < 0 then
    raise Exception.Create('Professor não encontrado');
  if Trim(aNome) = '' then
    raise Exception.Create('Nome do professor não pode estar vazio');
  if Trim(aCPF) = '' then
    raise Exception.Create('CPF do professor não pode estar vazio');

  for i := 0 to FLista.Count - 1 do
  begin
    if (i <> indice) and SameText(FLista[i].Nome, aNome) then
      raise Exception.Create('Já existe outro professor com este nome');
  end;
  for i := 0 to FLista.Count - 1 do
  begin
    if (i <> indice) and (FLista[i].CPF = aCPF) then
      raise Exception.Create('Já existe outro professor com este CPF');
  end;

  try
    FLista[indice].Nome := aNome;
    FLista[indice].CPF := aCPF;
    SalvarDados;
    Result := True;
  except
    on E: Exception do
      raise Exception.Create('Erro ao atualizar professor: ' + E.Message);
  end;
end;

function TProfessorControlador.Excluir(aCodigo: Integer): Boolean;
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
        raise Exception.Create('Erro ao excluir professor: ' + E.Message);
    end;
  end;
end;

function TProfessorControlador.BuscarPorCodigo(aCodigo: Integer): TProfessor;
var
  indice: Integer;
begin
  Result := nil;
  indice := BuscarIndice(aCodigo);
  if indice >= 0 then
    Result := FLista[indice];
end;

procedure TProfessorControlador.Listar(AStringList: TStrings);
var
  professor: TProfessor;
begin
  AStringList.Clear;
  for professor in FLista do
    AStringList.Add(professor.ToString);
end;

procedure TProfessorControlador.SalvarDados;
var
  txt: TextFile;
  professor: TProfessor;
begin
  AssignFile(txt, FArquivo);
  try
    Rewrite(txt); // Open file for writing
    for professor in FLista do
    begin
      WriteLn(txt, Format('%d;%s;%s', [professor.Codigo, professor.Nome, professor.CPF]));
    end;
  finally
    CloseFile(txt);
  end;
end;

procedure TProfessorControlador.CarregarDados;
var
  txt: TextFile;
  linha: string;
  codigo: Integer;
  nome, cpf: string;
begin
  if not FileExists(FArquivo) then Exit;
  AssignFile(txt, FArquivo);
  Reset(txt); // Open file for reading
  try
    FLista.Clear;
    while not Eof(txt) do
    begin
      ReadLn(txt, linha);
      if Pos(';', linha) > 0 then
      begin
        codigo := StrToInt(Copy(linha, 1, Pos(';', linha) - 1));
        nome := Copy(linha, Pos(';', linha) + 1, Pos(';', linha, Pos(';', linha) + 1) - Pos(';', linha) - 1);
        cpf := Copy(linha, Pos(';', linha, Pos(';', linha) + 1) + 1, Length(linha));
        FLista.Add(TProfessor.Create(codigo, nome, cpf));
      end;
    end;
  finally
    CloseFile(txt);
  end;
end;

// --------------------------------------------------------------------------------------------------
// Novo método: ObterTodos
// --------------------------------------------------------------------------------------------------

function TProfessorControlador.ObterTodos: TObjectList<TProfessor>;
begin
  Result := FLista;
end;

end.
