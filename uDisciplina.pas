unit uDisciplina;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections;

type
  // Classe modelo para a entidade Disciplina.
  TDisciplina = class
  private
    FCodigo: Integer;
    FNome: string;
  public
    constructor Create(aCodigo: Integer; aNome: string); // Construtor para inicializar uma nova instância de TDisciplina com código e nome.
    property Codigo: Integer read FCodigo write FCodigo; // Propriedade de acesso para o código da disciplina (leitura e escrita).
    property Nome: string read FNome write FNome; // Propriedade de acesso para o nome da disciplina (leitura e escrita).
    function ToString: string; override; // Sobrescreve o método padrão para retornar uma representação em string do objeto.
  end;

  // Controlador para gerenciar as operações de CRUD da entidade Disciplina.
  TDisciplinaControlador = class
  private
    FLista: TObjectList<TDisciplina>; // Gerencia uma lista de objetos TDisciplina, liberando a memória automaticamente.
    FArquivo: string; // Armazena o nome do arquivo onde os dados serão persistidos.
    function BuscarIndice(aCodigo: Integer): Integer; // Função auxiliar para encontrar o índice de uma disciplina na lista através do código.
    function ObterProximoCodigo: Integer; // Função auxiliar que encontra e retorna o próximo código sequencial disponível.
    function ExisteNome(const aNome: string): Boolean; // Verifica se já existe uma disciplina com o nome fornecido (não case-sensitive).
  public
    constructor Create; // Construtor do controlador.
    destructor Destroy; override; // Destrutor, responsável por liberar a memória da lista.

    function Incluir(aCodigo: Integer; const aNome: string): Boolean; // Adiciona uma nova disciplina à lista e salva os dados no arquivo.
    function Atualizar(aCodigo: Integer; const aNome: string): Boolean; // Atualiza o nome de uma disciplina existente e salva os dados.
    function Excluir(aCodigo: Integer): Boolean; // Remove uma disciplina da lista e salva os dados.
    function BuscarPorCodigo(aCodigo: Integer): TDisciplina; // Busca e retorna uma disciplina específica pelo código.
    procedure Listar(AStringList: TStrings); // Preenche um TStrings (como de um TListBox) com a representação em string de cada disciplina.

    procedure CarregarDados; // Carrega os dados das disciplinas do arquivo para a lista.
    procedure SalvarDados; // Salva os dados da lista de disciplinas no arquivo.

    // Novo método: Retorna a lista completa para iteração em relatórios
    function ObterTodos: TObjectList<TDisciplina>;
  end;

implementation

// --------------------------------------------------------------------------------------------------
// TDisciplina
// --------------------------------------------------------------------------------------------------

constructor TDisciplina.Create(aCodigo: Integer; aNome: string);
begin
  inherited Create;
  FCodigo := aCodigo;
  FNome := aNome;
end;

function TDisciplina.ToString: string;
begin
  Result := Format('Código: %d - Nome: %s', [FCodigo, FNome]);
end;

// --------------------------------------------------------------------------------------------------
// TDisciplinaControlador
// --------------------------------------------------------------------------------------------------

constructor TDisciplinaControlador.Create;
begin
  inherited Create;
  FLista := TObjectList<TDisciplina>.Create(True);
  FArquivo := 'disciplinas.txt';
end;

destructor TDisciplinaControlador.Destroy;
begin
  FLista.Free;
  inherited Destroy;
end;

function TDisciplinaControlador.BuscarIndice(aCodigo: Integer): Integer;
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

function TDisciplinaControlador.ObterProximoCodigo: Integer;
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

function TDisciplinaControlador.ExisteNome(const aNome: string): Boolean;
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

function TDisciplinaControlador.Incluir(aCodigo: Integer; const aNome: string): Boolean;
begin
  Result := False;
  if BuscarIndice(aCodigo) <> -1 then
    raise Exception.Create('Código de disciplina já existe');
  if Trim(aNome) = '' then
    raise Exception.Create('Nome da disciplina não pode estar vazio');

  try
    FLista.Add(TDisciplina.Create(aCodigo, aNome));
    SalvarDados;
    Result := True;
  except
    on E: Exception do
      raise Exception.Create('Erro ao incluir disciplina: ' + E.Message);
  end;
end;

function TDisciplinaControlador.Atualizar(aCodigo: Integer; const aNome: string): Boolean;
var
  indice: Integer;
begin
  Result := False;
  indice := BuscarIndice(aCodigo);
  if indice < 0 then
    raise Exception.Create('Disciplina não encontrada');
  if Trim(aNome) = '' then
    raise Exception.Create('Nome da disciplina não pode estar vazio');

  try
    FLista[indice].Nome := aNome;
    SalvarDados;
    Result := True;
  except
    on E: Exception do
      raise Exception.Create('Erro ao atualizar disciplina: ' + E.Message);
  end;
end;

function TDisciplinaControlador.Excluir(aCodigo: Integer): Boolean;
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
        raise Exception.Create('Erro ao excluir disciplina: ' + E.Message);
    end;
  end;
end;

function TDisciplinaControlador.BuscarPorCodigo(aCodigo: Integer): TDisciplina;
var
  indice: Integer;
begin
  Result := nil;
  indice := BuscarIndice(aCodigo);
  if indice >= 0 then
    Result := FLista[indice];
end;

procedure TDisciplinaControlador.Listar(AStringList: TStrings);
var
  disciplina: TDisciplina;
begin
  AStringList.Clear;
  for disciplina in FLista do
    AStringList.Add(disciplina.ToString);
end;

procedure TDisciplinaControlador.CarregarDados;
var
  txt: TextFile;
  linha: string;
  partes: TArray<string>;
  codigo: Integer;
  nome: string;
begin
  if not FileExists(FArquivo) then Exit;
  AssignFile(txt, FArquivo);
  Reset(txt); // Abrir o arquivo para ler
  try
    FLista.Clear;
    while not Eof(txt) do
    begin
      ReadLn(txt, linha);
      partes := linha.Split([';']); // cada ; é um split de array, tipo ['1';'matematica']
      if Length(partes) >= 2 then
      begin
        if TryStrToInt(partes[0], codigo) then
        begin
          nome := partes[1];  // nome é partes[1], código é partes[0]
          FLista.Add(TDisciplina.Create(codigo, nome));
        end;
      end;
    end;
  finally
    CloseFile(txt);
  end;
end;

procedure TDisciplinaControlador.SalvarDados;
var
  txt: TextFile;
  disciplina: TDisciplina;
begin
  AssignFile(txt, FArquivo);
  Rewrite(txt); // Abrir o arquivo para escrever
  try
    for disciplina in FLista do
    begin
      WriteLn(txt, Format('%d;%s', [disciplina.Codigo, disciplina.Nome]));
    end;
  finally
    CloseFile(txt);
  end;
end;

// --------------------------------------------------------------------------------------------------
// Novo método: ObterTodos
// --------------------------------------------------------------------------------------------------

function TDisciplinaControlador.ObterTodos: TObjectList<TDisciplina>;
begin
  Result := FLista;
end;

end.
