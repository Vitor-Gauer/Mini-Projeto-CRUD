unit uDisciplina;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections;

type
  // Classe modelo para Disciplina
  TDisciplina = class
  private
    FCodigo: Integer;
    FNome: string;
  public
    constructor Create(ACodigo: Integer; ANome: string);
    property Codigo: Integer read FCodigo write FCodigo;
    property Nome: string read FNome write FNome;
    function ToString: string; override;
  end;

  // Controlador para gerenciar Disciplinas
  TDisciplinaControlador = class
  private
    FLista: TObjectList<TDisciplina>;
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
    function BuscarPorCodigo(ACodigo: Integer): TDisciplina;
    procedure Listar(AStringList: TStrings);

    procedure CarregarDados;
    procedure SalvarDados;
  end;

implementation

// --------------------------------------------------------------------------------------------------
// TDisciplina
// --------------------------------------------------------------------------------------------------

constructor TDisciplina.Create(ACodigo: Integer; ANome: string);
begin
  inherited Create;
  FCodigo := ACodigo;
  FNome := ANome;
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

function TDisciplinaControlador.BuscarIndice(ACodigo: Integer): Integer;
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

function TDisciplinaControlador.ExisteNome(const ANome: string): Boolean;
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

function TDisciplinaControlador.Incluir(const ANome: string): Integer;
var
  disciplina: TDisciplina;
  novoCodigo: Integer;
begin
  Result := -1;

  // Verifica se já existe uma disciplina com este nome
  if ExisteNome(ANome) then
    raise Exception.Create('Já existe uma disciplina com este nome');

  if Trim(ANome) = '' then
    raise Exception.Create('Nome da disciplina não pode estar vazio');

  try
    novoCodigo := ObterProximoCodigo;
    disciplina := TDisciplina.Create(novoCodigo, ANome);
    FLista.Add(disciplina);
    SalvarDados;
    Result := novoCodigo;
  except
    on E: Exception do
      raise Exception.Create('Erro ao incluir disciplina: ' + E.Message);
  end;
end;

function TDisciplinaControlador.Atualizar(ACodigo: Integer; const ANome: string): Boolean;
var
  indice: Integer;
  i: Integer;
begin
  Result := False;
  indice := BuscarIndice(ACodigo);

  if indice < 0 then
    raise Exception.Create('Disciplina não encontrada');

  if Trim(ANome) = '' then
    raise Exception.Create('Nome da disciplina não pode estar vazio');

  // Verifica se já existe outra disciplina com este nome
  for i := 0 to FLista.Count - 1 do
  begin
    if (i <> indice) and SameText(FLista[i].Nome, ANome) then
      raise Exception.Create('Já existe outra disciplina com este nome');
  end;

  try
    FLista[indice].Nome := ANome;
    SalvarDados;
    Result := True;
  except
    on E: Exception do
      raise Exception.Create('Erro ao atualizar disciplina: ' + E.Message);
  end;
end;

function TDisciplinaControlador.Excluir(ACodigo: Integer): Boolean;
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
        raise Exception.Create('Erro ao excluir disciplina: ' + E.Message);
    end;
  end;
end;

function TDisciplinaControlador.BuscarPorCodigo(ACodigo: Integer): TDisciplina;
var
  indice: Integer;
begin
  Result := nil;
  indice := BuscarIndice(ACodigo);

  if indice >= 0 then
    Result := FLista[indice];
end;

procedure TDisciplinaControlador.Listar(AStringList: TStrings);
var
  i: Integer;
begin
  try
    AStringList.Clear;
    for i := 0 to FLista.Count - 1 do
      AStringList.Add(FLista[i].ToString);
  except
    on E: Exception do
      raise Exception.Create('Erro ao listar disciplinas: ' + E.Message);
  end;
end;

procedure TDisciplinaControlador.CarregarDados;
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
                FLista.Add(TDisciplina.Create(codigo, nome));
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
      raise Exception.Create('Erro ao carregar dados das disciplinas: ' + E.Message);
  end;
end;

procedure TDisciplinaControlador.SalvarDados;
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
      raise Exception.Create('Erro ao salvar dados das disciplinas: ' + E.Message);
  end;
end;

end.