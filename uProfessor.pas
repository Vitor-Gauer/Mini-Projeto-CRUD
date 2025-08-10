unit uProfessor;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections;

type
  // Classe modelo para Professor
  TProfessor = class
  private
    FCodigo: Integer;
    FNome: string;
    FCPF: string;
  public
    constructor Create(ACodigo: Integer; ANome, ACPF: string);
    property Codigo: Integer read FCodigo write FCodigo;
    property Nome: string read FNome write FNome;
    property CPF: string read FCPF write FCPF;
    function ToString: string; override;
  end;

  // Controlador para gerenciar Professores
  TProfessorControlador = class
  private
    FLista: TObjectList<TProfessor>;
    FArquivo: string;
    function BuscarIndice(ACodigo: Integer): Integer;
    function ObterProximoCodigo: Integer;
    function ExisteNome(const ANome: string): Boolean;
    function ExisteCPF(const ACPF: string): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function Incluir(const ANome, ACPF: string): Integer;
    function Atualizar(ACodigo: Integer; const ANome, ACPF: string): Boolean;
    function Excluir(ACodigo: Integer): Boolean;
    function BuscarPorCodigo(ACodigo: Integer): TProfessor;
    procedure Listar(AStringList: TStrings);

    procedure CarregarDados;
    procedure SalvarDados;
  end;

implementation

// --------------------------------------------------------------------------------------------------
// TProfessor
// --------------------------------------------------------------------------------------------------

constructor TProfessor.Create(ACodigo: Integer; ANome, ACPF: string);
begin
  inherited Create;
  FCodigo := ACodigo;
  FNome := ANome;
  FCPF := ACPF;
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

function TProfessorControlador.BuscarIndice(ACodigo: Integer): Integer;
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

function TProfessorControlador.ExisteNome(const ANome: string): Boolean;
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

function TProfessorControlador.ExisteCPF(const ACPF: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to FLista.Count - 1 do
  begin
    if FLista[i].CPF = ACPF then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TProfessorControlador.Incluir(const ANome, ACPF: string): Integer;
var
  professor: TProfessor;
  novoCodigo: Integer;
begin
  Result := -1;

  if Trim(ANome) = '' then
    raise Exception.Create('Nome do professor não pode estar vazio');

  if Trim(ACPF) = '' then
    raise Exception.Create('CPF do professor não pode estar vazio');

  // Verifica se já existe um professor com este nome
  if ExisteNome(ANome) then
    raise Exception.Create('Já existe um professor com este nome');

  // Verifica se já existe um professor com este CPF
  if ExisteCPF(ACPF) then
    raise Exception.Create('Já existe um professor com este CPF');

  try
    novoCodigo := ObterProximoCodigo;
    professor := TProfessor.Create(novoCodigo, ANome, ACPF);
    FLista.Add(professor);
    SalvarDados;
    Result := novoCodigo;
  except
    on E: Exception do
      raise Exception.Create('Erro ao incluir professor: ' + E.Message);
  end;
end;

function TProfessorControlador.Atualizar(ACodigo: Integer; const ANome, ACPF: string): Boolean;
var
  indice: Integer;
  i: Integer;
begin
  Result := False;
  indice := BuscarIndice(ACodigo);

  if indice < 0 then
    raise Exception.Create('Professor não encontrado');

  if Trim(ANome) = '' then
    raise Exception.Create('Nome do professor não pode estar vazio');

  if Trim(ACPF) = '' then
    raise Exception.Create('CPF do professor não pode estar vazio');

  // Verifica se já existe outro professor com este nome
  for i := 0 to FLista.Count - 1 do
  begin
    if (i <> indice) and SameText(FLista[i].Nome, ANome) then
      raise Exception.Create('Já existe outro professor com este nome');
  end;

  // Verifica se já existe outro professor com este CPF
  for i := 0 to FLista.Count - 1 do
  begin
    if (i <> indice) and (FLista[i].CPF = ACPF) then
      raise Exception.Create('Já existe outro professor com este CPF');
  end;

  try
    FLista[indice].Nome := ANome;
    FLista[indice].CPF := ACPF;
    SalvarDados;
    Result := True;
  except
    on E: Exception do
      raise Exception.Create('Erro ao atualizar professor: ' + E.Message);
  end;
end;

function TProfessorControlador.Excluir(ACodigo: Integer): Boolean;
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
        raise Exception.Create('Erro ao excluir professor: ' + E.Message);
    end;
  end;
end;

function TProfessorControlador.BuscarPorCodigo(ACodigo: Integer): TProfessor;
var
  indice: Integer;
begin
  Result := nil;
  indice := BuscarIndice(ACodigo);

  if indice >= 0 then
    Result := FLista[indice];
end;

procedure TProfessorControlador.Listar(AStringList: TStrings);
var
  i: Integer;
begin
  try
    AStringList.Clear;
    for i := 0 to FLista.Count - 1 do
      AStringList.Add(FLista[i].ToString);
  except
    on E: Exception do
      raise Exception.Create('Erro ao listar professores: ' + E.Message);
  end;
end;

procedure TProfessorControlador.CarregarDados;
var
  arquivo: TStringList;
  i: Integer;
  linha: string;
  codigo: Integer;
  nome, cpf: string;
  posicao1, posicao2: Integer;
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
            // Formato: codigo|nome|cpf
            posicao1 := Pos('|', linha);
            if posicao1 > 0 then
            begin
              if TryStrToInt(Copy(linha, 1, posicao1 - 1), codigo) then
              begin
                linha := Copy(linha, posicao1 + 1, Length(linha));
                posicao2 := Pos('|', linha);
                if posicao2 > 0 then
                begin
                  nome := Copy(linha, 1, posicao2 - 1);
                  cpf := Copy(linha, posicao2 + 1, Length(linha));
                  FLista.Add(TProfessor.Create(codigo, nome, cpf));
                end;
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
      raise Exception.Create('Erro ao carregar dados dos professores: ' + E.Message);
  end;
end;

procedure TProfessorControlador.SalvarDados;
var
  arquivo: TStringList;
  i: Integer;
begin
  try
    arquivo := TStringList.Create;
    try
      for i := 0 to FLista.Count - 1 do
        arquivo.Add(Format('%d|%s|%s', [FLista[i].Codigo, FLista[i].Nome, FLista[i].CPF]));

      arquivo.SaveToFile(FArquivo);
    finally
      arquivo.Free;
    end;
  except
    on E: Exception do
      raise Exception.Create('Erro ao salvar dados dos professores: ' + E.Message);
  end;
end;

end.