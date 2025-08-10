unit uMatricula;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections;

type
  // Classe modelo para Matrícula
  TMatricula = class
  private
    FCodigo: Integer;
    FCodigoTurma: Integer;
    FCodigoEstudante: Integer;
  public
    constructor Create(ACodigo, ACodigoTurma, ACodigoEstudante: Integer);
    property Codigo: Integer read FCodigo write FCodigo;
    property CodigoTurma: Integer read FCodigoTurma write FCodigoTurma;
    property CodigoEstudante: Integer read FCodigoEstudante write FCodigoEstudante;
    function ToString: string; override;
  end;

  // Controlador para gerenciar Matrículas
  TMatriculaControlador = class
  private
    FLista: TObjectList<TMatricula>;
    FArquivo: string;
    function BuscarIndice(ACodigo: Integer): Integer;
    function ObterProximoCodigo: Integer;
    function ExisteCombinacao(ACodigoTurma, ACodigoEstudante: Integer): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function Incluir(ACodigoTurma, ACodigoEstudante: Integer): Integer;
    function Atualizar(ACodigo, ACodigoTurma, ACodigoEstudante: Integer): Boolean;
    function Excluir(ACodigo: Integer): Boolean;
    function BuscarPorCodigo(ACodigo: Integer): TMatricula;
    procedure Listar(AStringList: TStrings);

    procedure CarregarDados;
    procedure SalvarDados;
  end;

implementation

// --------------------------------------------------------------------------------------------------
// TMatricula
// --------------------------------------------------------------------------------------------------

constructor TMatricula.Create(ACodigo, ACodigoTurma, ACodigoEstudante: Integer);
begin
  inherited Create;
  FCodigo := ACodigo;
  FCodigoTurma := ACodigoTurma;
  FCodigoEstudante := ACodigoEstudante;
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

function TMatriculaControlador.BuscarIndice(ACodigo: Integer): Integer;
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

function TMatriculaControlador.ExisteCombinacao(ACodigoTurma, ACodigoEstudante: Integer): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to FLista.Count - 1 do
  begin
    if (FLista[i].CodigoTurma = ACodigoTurma) and 
       (FLista[i].CodigoEstudante = ACodigoEstudante) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TMatriculaControlador.Incluir(ACodigoTurma, ACodigoEstudante: Integer): Integer;
var
  matricula: TMatricula;
  novoCodigo: Integer;
begin
  Result := -1;

  // Verifica se já existe uma matrícula com esta combinação turma/estudante
  if ExisteCombinacao(ACodigoTurma, ACodigoEstudante) then
    raise Exception.Create('Estudante já está matriculado nesta turma');

  if ACodigoTurma <= 0 then
    raise Exception.Create('Código da turma deve ser maior que zero');

  if ACodigoEstudante <= 0 then
    raise Exception.Create('Código do estudante deve ser maior que zero');

  try
    novoCodigo := ObterProximoCodigo;
    matricula := TMatricula.Create(novoCodigo, ACodigoTurma, ACodigoEstudante);
    FLista.Add(matricula);
    SalvarDados;
    Result := novoCodigo;
  except
    on E: Exception do
      raise Exception.Create('Erro ao incluir matrícula: ' + E.Message);
  end;
end;

function TMatriculaControlador.Atualizar(ACodigo, ACodigoTurma, ACodigoEstudante: Integer): Boolean;
var
  indice: Integer;
  i: Integer;
begin
  Result := False;
  indice := BuscarIndice(ACodigo);

  if indice < 0 then
    raise Exception.Create('Matrícula não encontrada');

  if ACodigoTurma <= 0 then
    raise Exception.Create('Código da turma deve ser maior que zero');

  if ACodigoEstudante <= 0 then
    raise Exception.Create('Código do estudante deve ser maior que zero');

  // Verifica se já existe outra matrícula com esta combinação
  for i := 0 to FLista.Count - 1 do
  begin
    if (i <> indice) and 
       (FLista[i].CodigoTurma = ACodigoTurma) and 
       (FLista[i].CodigoEstudante = ACodigoEstudante) then
      raise Exception.Create('Estudante já está matriculado nesta turma');
  end;

  try
    FLista[indice].CodigoTurma := ACodigoTurma;
    FLista[indice].CodigoEstudante := ACodigoEstudante;
    SalvarDados;
    Result := True;
  except
    on E: Exception do
      raise Exception.Create('Erro ao atualizar matrícula: ' + E.Message);
  end;
end;

function TMatriculaControlador.Excluir(ACodigo: Integer): Boolean;
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
        raise Exception.Create('Erro ao excluir matrícula: ' + E.Message);
    end;
  end;
end;

function TMatriculaControlador.BuscarPorCodigo(ACodigo: Integer): TMatricula;
var
  indice: Integer;
begin
  Result := nil;
  indice := BuscarIndice(ACodigo);

  if indice >= 0 then
    Result := FLista[indice];
end;

procedure TMatriculaControlador.Listar(AStringList: TStrings);
var
  i: Integer;
begin
  try
    AStringList.Clear;
    for i := 0 to FLista.Count - 1 do
      AStringList.Add(FLista[i].ToString);
  except
    on E: Exception do
      raise Exception.Create('Erro ao listar matrículas: ' + E.Message);
  end;
end;

procedure TMatriculaControlador.CarregarDados;
var
  arquivo: TStringList;
  i: Integer;
  linha: string;
  codigo, codigoTurma, codigoEst: Integer;
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
            // Formato: codigo|codigoTurma|codigoEstudante
            posicao1 := Pos('|', linha);
            if posicao1 > 0 then
            begin
              if TryStrToInt(Copy(linha, 1, posicao1 - 1), codigo) then
              begin
                linha := Copy(linha, posicao1 + 1, Length(linha));
                posicao2 := Pos('|', linha);
                if posicao2 > 0 then
                begin
                  if TryStrToInt(Copy(linha, 1, posicao2 - 1), codigoTurma) then
                  begin
                    if TryStrToInt(Copy(linha, posicao2 + 1, Length(linha)), codigoEst) then
                      FLista.Add(TMatricula.Create(codigo, codigoTurma, codigoEst));
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
      raise Exception.Create('Erro ao carregar dados das matrículas: ' + E.Message);
  end;
end;

procedure TMatriculaControlador.SalvarDados;
var
  arquivo: TStringList;
  i: Integer;
begin
  try
    arquivo := TStringList.Create;
    try
      for i := 0 to FLista.Count - 1 do
        arquivo.Add(Format('%d|%d|%d', [FLista[i].Codigo,
          FLista[i].CodigoTurma, FLista[i].CodigoEstudante]));

      arquivo.SaveToFile(FArquivo);
    finally
      arquivo.Free;
    end;
  except
    on E: Exception do
      raise Exception.Create('Erro ao salvar dados das matrículas: ' + E.Message);
  end;
end;

end.