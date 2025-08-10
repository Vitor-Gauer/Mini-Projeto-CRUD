unit uTurma;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections;

type
  // Classe modelo para Turma
  TTurma = class
  private
    FCodigo: Integer;
    FCodigoProfessor: Integer;
    FCodigoDisciplina: Integer;
  public
    constructor Create(ACodigo, ACodigoProfessor, ACodigoDisciplina: Integer);
    property Codigo: Integer read FCodigo write FCodigo;
    property CodigoProfessor: Integer read FCodigoProfessor write FCodigoProfessor;
    property CodigoDisciplina: Integer read FCodigoDisciplina write FCodigoDisciplina;
    function ToString: string; override;
  end;

  // Controlador para gerenciar Turmas
  TTurmaControlador = class
  private
    FLista: TObjectList<TTurma>;
    FArquivo: string;
    function BuscarIndice(ACodigo: Integer): Integer;
    function ObterProximoCodigo: Integer;
    function ExisteCombinacao(ACodigoProfessor, ACodigoDisciplina: Integer): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function Incluir(ACodigoProfessor, ACodigoDisciplina: Integer): Integer;
    function Atualizar(ACodigo, ACodigoProfessor, ACodigoDisciplina: Integer): Boolean;
    function Excluir(ACodigo: Integer): Boolean;
    function BuscarPorCodigo(ACodigo: Integer): TTurma;
    procedure Listar(AStringList: TStrings);

    procedure CarregarDados;
    procedure SalvarDados;
  end;

implementation

// --------------------------------------------------------------------------------------------------
// TTurma
// --------------------------------------------------------------------------------------------------

constructor TTurma.Create(ACodigo, ACodigoProfessor, ACodigoDisciplina: Integer);
begin
  inherited Create;
  FCodigo := ACodigo;
  FCodigoProfessor := ACodigoProfessor;
  FCodigoDisciplina := ACodigoDisciplina;
end;

function TTurma.ToString: string;
begin
  Result := Format('Código: %d - Professor: %d - Disciplina: %d',
    [FCodigo, FCodigoProfessor, FCodigoDisciplina]);
end;

// --------------------------------------------------------------------------------------------------
// TTurmaControlador
// --------------------------------------------------------------------------------------------------

constructor TTurmaControlador.Create;
begin
  inherited Create;
  FLista := TObjectList<TTurma>.Create(True);
  FArquivo := 'turmas.txt';
end;

destructor TTurmaControlador.Destroy;
begin
  FLista.Free;
  inherited Destroy;
end;

function TTurmaControlador.BuscarIndice(ACodigo: Integer): Integer;
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

function TTurmaControlador.ObterProximoCodigo: Integer;
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

function TTurmaControlador.ExisteCombinacao(ACodigoProfessor, ACodigoDisciplina: Integer): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to FLista.Count - 1 do
  begin
    if (FLista[i].CodigoProfessor = ACodigoProfessor) and 
       (FLista[i].CodigoDisciplina = ACodigoDisciplina) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TTurmaControlador.Incluir(ACodigoProfessor, ACodigoDisciplina: Integer): Integer;
var
  turma: TTurma;
  novoCodigo: Integer;
begin
  Result := -1;

  // Verifica se já existe uma turma com esta combinação professor/disciplina
  if ExisteCombinacao(ACodigoProfessor, ACodigoDisciplina) then
    raise Exception.Create('Já existe uma turma com esta combinação de professor e disciplina');

  if ACodigoProfessor <= 0 then
    raise Exception.Create('Código do professor deve ser maior que zero');

  if ACodigoDisciplina <= 0 then
    raise Exception.Create('Código da disciplina deve ser maior que zero');

  try
    novoCodigo := ObterProximoCodigo;
    turma := TTurma.Create(novoCodigo, ACodigoProfessor, ACodigoDisciplina);
    FLista.Add(turma);
    SalvarDados;
    Result := novoCodigo;
  except
    on E: Exception do
      raise Exception.Create('Erro ao incluir turma: ' + E.Message);
  end;
end;

function TTurmaControlador.Atualizar(ACodigo, ACodigoProfessor, ACodigoDisciplina: Integer): Boolean;
var
  indice: Integer;
  i: Integer;
begin
  Result := False;
  indice := BuscarIndice(ACodigo);

  if indice < 0 then
    raise Exception.Create('Turma não encontrada');

  if ACodigoProfessor <= 0 then
    raise Exception.Create('Código do professor deve ser maior que zero');

  if ACodigoDisciplina <= 0 then
    raise Exception.Create('Código da disciplina deve ser maior que zero');

  // Verifica se já existe outra turma com esta combinação
  for i := 0 to FLista.Count - 1 do
  begin
    if (i <> indice) and 
       (FLista[i].CodigoProfessor = ACodigoProfessor) and 
       (FLista[i].CodigoDisciplina = ACodigoDisciplina) then
      raise Exception.Create('Já existe outra turma com esta combinação de professor e disciplina');
  end;

  try
    FLista[indice].CodigoProfessor := ACodigoProfessor;
    FLista[indice].CodigoDisciplina := ACodigoDisciplina;
    SalvarDados;
    Result := True;
  except
    on E: Exception do
      raise Exception.Create('Erro ao atualizar turma: ' + E.Message);
  end;
end;

function TTurmaControlador.Excluir(ACodigo: Integer): Boolean;
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
        raise Exception.Create('Erro ao excluir turma: ' + E.Message);
    end;
  end;
end;

function TTurmaControlador.BuscarPorCodigo(ACodigo: Integer): TTurma;
var
  indice: Integer;
begin
  Result := nil;
  indice := BuscarIndice(ACodigo);

  if indice >= 0 then
    Result := FLista[indice];
end;

procedure TTurmaControlador.Listar(AStringList: TStrings);
var
  i: Integer;
begin
  try
    AStringList.Clear;
    for i := 0 to FLista.Count - 1 do
      AStringList.Add(FLista[i].ToString);
  except
    on E: Exception do
      raise Exception.Create('Erro ao listar turmas: ' + E.Message);
  end;
end;

procedure TTurmaControlador.CarregarDados;
var
  arquivo: TStringList;
  i: Integer;
  linha: string;
  codigo, codigoProf, codigoDisc: Integer;
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
            // Formato: codigo|codigoProfessor|codigoDisciplina
            posicao1 := Pos('|', linha);
            if posicao1 > 0 then
            begin
              if TryStrToInt(Copy(linha, 1, posicao1 - 1), codigo) then
              begin
                linha := Copy(linha, posicao1 + 1, Length(linha));
                posicao2 := Pos('|', linha);
                if posicao2 > 0 then
                begin
                  if TryStrToInt(Copy(linha, 1, posicao2 - 1), codigoProf) then
                  begin
                    if TryStrToInt(Copy(linha, posicao2 + 1, Length(linha)), codigoDisc) then
                      FLista.Add(TTurma.Create(codigo, codigoProf, codigoDisc));
                  end;
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
      raise Exception.Create('Erro ao carregar dados das turmas: ' + E.Message);
  end;
end;

procedure TTurmaControlador.SalvarDados;
var
  arquivo: TStringList;
  i: Integer;
begin
  try
    arquivo := TStringList.Create;
    try
      for i := 0 to FLista.Count - 1 do
        arquivo.Add(Format('%d|%d|%d', [FLista[i].Codigo,
          FLista[i].CodigoProfessor, FLista[i].CodigoDisciplina]));

      arquivo.SaveToFile(FArquivo);
    finally
      arquivo.Free;
    end;
  except
    on E: Exception do
      raise Exception.Create('Erro ao salvar dados das turmas: ' + E.Message);
  end;
end;

end.