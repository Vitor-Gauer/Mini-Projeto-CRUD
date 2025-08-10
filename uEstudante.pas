unit uEstudante;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections;

type
  // Classe modelo para Estudante
  TEstudante = class
  private
    FCodigo: Integer;
    FNome: string;
  public
    constructor Create(ACodigo: Integer; ANome: string);
    property Codigo: Integer read FCodigo write FCodigo;
    property Nome: string read FNome write FNome;
    function ToString: string; override;
  end;

  // Controlador para gerenciar Estudantes
  TEstudanteControlador = class
  private
    FLista: TObjectList<TEstudante>;
    FArquivo: string;
    function BuscarIndice(ACodigo: Integer): Integer;
    function ObterProximoCodigo: Integer;
    function ExisteNome(const ANome: string): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function Incluir(const ANome: string): Integer;
    function Atualizar(ACodigo: Integer; const ANome: string): Boolean;
    function Excluir(ACodigo: Integer): Boolean;
    function BuscarPorCodigo(ACodigo: Integer): TEstudante;
    procedure Listar(AStringList: TStrings);

    procedure CarregarDados;
    procedure SalvarDados;
  end;

implementation

// --------------------------------------------------------------------------------------------------
// TEstudante
// --------------------------------------------------------------------------------------------------

constructor TEstudante.Create(ACodigo: Integer; ANome: string);
begin
  inherited Create;
  FCodigo := ACodigo;
  FNome := ANome;
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

function TEstudanteControlador.BuscarIndice(ACodigo: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FLista.Count - 1 do
  begin
    if FLista[i].Codigo = ACodigo then
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

function TEstudanteControlador.ExisteNome(const ANome: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to FLista.Count - 1 do
  begin
    if SameText(FLista[i].Nome, ANome) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TEstudanteControlador.Incluir(const ANome: string): Integer;
var
  estudante: TEstudante;
  novoCodigo: Integer;
begin
  Result := -1;

  // Verifica se já existe um estudante com este nome
  if ExisteNome(ANome) then
    raise Exception.Create('Já existe um estudante com este nome');

  if Trim(ANome) = '' then
    raise Exception.Create('Nome do estudante não pode estar vazio');

  try
    novoCodigo := ObterProximoCodigo;
    estudante := TEstudante.Create(novoCodigo, ANome);
    FLista.Add(estudante);
    SalvarDados;
    Result := novoCodigo;
  except
    on E: Exception do
      raise Exception.Create('Erro ao incluir estudante: ' + E.Message);
  end;
end;

function TEstudanteControlador.Atualizar(ACodigo: Integer; const ANome: string): Boolean;
var
  indice: Integer;
  i: Integer;
begin
  Result := False;
  indice := BuscarIndice(ACodigo);

  if indice < 0 then
    raise Exception.Create('Estudante não encontrado');

  if Trim(ANome) = '' then
    raise Exception.Create('Nome do estudante não pode estar vazio');

  // Verifica se já existe outro estudante com este nome
  for i := 0 to FLista.Count - 1 do
  begin
    if (i <> indice) and SameText(FLista[i].Nome, ANome) then
      raise Exception.Create('Já existe outro estudante com este nome');
  end;

  try
    FLista[indice].Nome := ANome;
    SalvarDados;
    Result := True;
  except
    on E: Exception do
      raise Exception.Create('Erro ao atualizar estudante: ' + E.Message);
  end;
end;

function TEstudanteControlador.Excluir(ACodigo: Integer): Boolean;
var
  indice: Integer;
begin
  Result := False;
  indice := BuscarIndice(ACodigo);

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

function TEstudanteControlador.BuscarPorCodigo(ACodigo: Integer): TEstudante;
var
  indice: Integer;
begin
  Result := nil;
  indice := BuscarIndice(ACodigo);

  if indice >= 0 then
    Result := FLista[indice];
end;

procedure TEstudanteControlador.Listar(AStringList: TStrings);
var
  i: Integer;
begin
  try
    AStringList.Clear;
    for i := 0 to FLista.Count - 1 do
      AStringList.Add(FLista[i].ToString);
  except
    on E: Exception do
      raise Exception.Create('Erro ao listar estudantes: ' + E.Message);
  end;
end;

procedure TEstudanteControlador.CarregarDados;
var
  arquivo: TStringList;
  i: Integer;
  linha: string;
  codigo: Integer;
  nome: string;
  posicao: Integer;
begin
  try
    if FileExists(FArquivo) then
    begin
      arquivo := TStringList.Create;
      try
        arquivo.LoadFromFile(FArquivo);
        FLista.Clear;

        for i := 0 to arquivo.Count - 1 do
        begin
          linha := arquivo[i];
          if Trim(linha) <> '' then
          begin
            // Formato: codigo|nome
            posicao := Pos('|', linha);
            if posicao > 0 then
            begin
              if TryStrToInt(Copy(linha, 1, posicao - 1), codigo) then
              begin
                nome := Copy(linha, posicao + 1, Length(linha));
                FLista.Add(TEstudante.Create(codigo, nome));
              end;
            end;
          end;
        end;
      finally
        arquivo.Free;
      end;
    end;
  except
    on E: Exception do
      raise Exception.Create('Erro ao carregar dados dos estudantes: ' + E.Message);
  end;
end;

procedure TEstudanteControlador.SalvarDados;
var
  arquivo: TStringList;
  i: Integer;
begin
  try
    arquivo := TStringList.Create;
    try
      for i := 0 to FLista.Count - 1 do
        arquivo.Add(Format('%d|%s', [FLista[i].Codigo, FLista[i].Nome]));

      arquivo.SaveToFile(FArquivo);
    finally
      arquivo.Free;
    end;
  except
    on E: Exception do
      raise Exception.Create('Erro ao salvar dados dos estudantes: ' + E.Message);
  end;
end;

end.