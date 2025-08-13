unit uEstudante;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections;

type
  // Classe modelo para a entidade Estudante.
  TEstudante = class
  private
    FCodigo: Integer;
    FNome: string;
  public
    constructor Create(aCodigo: Integer; aNome: string); // Construtor para inicializar uma nova instância de TEstudante com código e nome.
    property Codigo: Integer read FCodigo write FCodigo; // Propriedade de acesso para o código do estudante (leitura e escrita).
    property Nome: string read FNome write FNome; // Propriedade de acesso para o nome do estudante (leitura e escrita).
    function ToString: string; override; // Sobrescreve o método padrão para retornar uma representação em string do objeto.
  end;

  // Controlador para gerenciar as operações de CRUD da entidade Estudante.
  TEstudanteControlador = class
  private
    FLista: TObjectList<TEstudante>; // Gerencia uma lista de objetos TEstudante, liberando a memória automaticamente.
    FArquivo: string; // Armazena o nome do arquivo onde os dados serão persistidos.
    function BuscarIndice(aCodigo: Integer): Integer; // Função auxiliar para encontrar o índice de um estudante na lista através do código.
    function ObterProximoCodigo: Integer; // Função auxiliar que encontra e retorna o próximo código sequencial disponível.
    function ExisteNome(const aNome: string): Boolean; // Verifica se já existe um estudante com o nome fornecido (não case-sensitive).
  public
    constructor Create; // Construtor do controlador.
    destructor Destroy; override; // Destrutor, responsável por liberar a memória da lista.

    function Incluir(aCodigo: Integer; const aNome: string): Boolean; // Adiciona um novo estudante à lista e salva os dados no arquivo.
    function Atualizar(aCodigo: Integer; const aNome: string): Boolean; // Atualiza o nome de um estudante existente e salva os dados.
    function Excluir(aCodigo: Integer): Boolean; // Remove um estudante da lista e salva os dados.
    function BuscarPorCodigo(aCodigo: Integer): TEstudante; // Busca e retorna um estudante específico pelo código.
    procedure Listar(AStringList: TStrings); // Preenche um TStrings (como de um TListBox) com a representação em string de cada estudante.

    procedure CarregarDados; // Carrega os dados dos estudantes do arquivo para a lista.
    procedure SalvarDados; // Salva os dados da lista de estudantes no arquivo.

    // Novo método: Retorna a lista completa para iteração em relatórios
    function ObterTodos: TObjectList<TEstudante>;
  end;

implementation

// --------------------------------------------------------------------------------------------------
// TEstudante
// --------------------------------------------------------------------------------------------------

constructor TEstudante.Create(aCodigo: Integer; aNome: string);
begin
  inherited Create;
  FCodigo := aCodigo;
  FNome := aNome;
end;

function TEstudante.ToString: string;
begin
  Result := Format('Código: %d - Nome: %s', [FCodigo, FNome]);
end;

// --------------------------------------------------------------------------------------------------
// TEstudanteControlador
// --------------------------------------------------------------------------------------------------

constructor TEstudanteControlador.Create;
begin
  inherited Create;
  FLista := TObjectList<TEstudante>.Create(True);
  FArquivo := 'estudantes.txt';
end;

destructor TEstudanteControlador.Destroy;
begin
  FLista.Free;
  inherited Destroy;
end;

function TEstudanteControlador.BuscarIndice(aCodigo: Integer): Integer;
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

function TEstudanteControlador.ObterProximoCodigo: Integer;
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

function TEstudanteControlador.ExisteNome(const aNome: string): Boolean;
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

function TEstudanteControlador.Incluir(aCodigo: Integer; const aNome: string): Boolean;
begin
  Result := False;
  if BuscarIndice(aCodigo) <> -1 then
    raise Exception.Create('Código de estudante já existe');
  if Trim(aNome) = '' then
    raise Exception.Create('Nome do estudante não pode estar vazio');

  try
    FLista.Add(TEstudante.Create(aCodigo, aNome));
    SalvarDados;
    Result := True;
  except
    on E: Exception do
      raise Exception.Create('Erro ao incluir estudante: ' + E.Message);
  end;
end;

function TEstudanteControlador.Atualizar(aCodigo: Integer; const aNome: string): Boolean;
var
  indice: Integer;
begin
  Result := False;
  indice := BuscarIndice(aCodigo);
  if indice < 0 then
    raise Exception.Create('Estudante não encontrado');
  if Trim(aNome) = '' then
    raise Exception.Create('Nome do estudante não pode estar vazio');

  try
    FLista[indice].Nome := aNome;
    SalvarDados;
    Result := True;
  except
    on E: Exception do
      raise Exception.Create('Erro ao atualizar estudante: ' + E.Message);
  end;
end;

function TEstudanteControlador.Excluir(aCodigo: Integer): Boolean;
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
        raise Exception.Create('Erro ao excluir estudante: ' + E.Message);
    end;
  end;
end;

function TEstudanteControlador.BuscarPorCodigo(aCodigo: Integer): TEstudante;
var
  indice: Integer;
begin
  Result := nil;
  indice := BuscarIndice(aCodigo);
  if indice >= 0 then
    Result := FLista[indice];
end;

procedure TEstudanteControlador.Listar(AStringList: TStrings);
var
  estudante: TEstudante;
begin
  AStringList.Clear;
  for estudante in FLista do
    AStringList.Add(estudante.ToString);
end;

procedure TEstudanteControlador.SalvarDados;
var
  txt: TextFile;
  estudante: TEstudante;
begin
  AssignFile(txt, FArquivo);
  try
    Rewrite(txt); // Abrir o arquivo para escrever
    for estudante in FLista do
    begin
      WriteLn(txt, Format('%d;%s', [estudante.Codigo, estudante.Nome]));
    end;
  finally
    CloseFile(txt);
  end;
end;

procedure TEstudanteControlador.CarregarDados;
var
  txt: TextFile;
  linha: string;
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
      if Pos(';', linha) > 0 then
      begin
        codigo := StrToInt(Copy(linha, 1, Pos(';', linha) - 1));
        nome := Copy(linha, Pos(';', linha) + 1, Length(linha));
        FLista.Add(TEstudante.Create(codigo, nome));
      end;
    end;
  finally
    CloseFile(txt);
  end;
end;

// --------------------------------------------------------------------------------------------------
// Novo método: ObterTodos
// --------------------------------------------------------------------------------------------------

function TEstudanteControlador.ObterTodos: TObjectList<TEstudante>;
begin
  Result := FLista;
end;

end.
