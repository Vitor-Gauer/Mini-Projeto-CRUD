object FColegio_CRUD: TFColegio_CRUD
  Left = 0
  Top = 0
  Caption = 'TFColegio_CRUD'
  ClientHeight = 512
  ClientWidth = 657
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poDesigned
  TextHeight = 15
  object pnlEscolhaDisciplina: TPanel
    AlignWithMargins = True
    Left = 22
    Top = 78
    Width = 618
    Height = 350
    Margins.Top = 20
    Margins.Bottom = 20
    Caption = 'pnlEscolhaDisciplina'
    ShowCaption = False
    TabOrder = 0
    object lblEscolhaDisciplina: TLabel
      AlignWithMargins = True
      Left = 4
      Top = 17
      Width = 610
      Height = 15
      Margins.Top = 16
      Align = alTop
      Alignment = taCenter
      Caption = 'Escolha a disciplina a cursar'
      ExplicitWidth = 146
    end
    object btnDisciplinaFullStack: TButton
      AlignWithMargins = True
      Left = 242
      Top = 53
      Width = 165
      Height = 24
      Caption = 'FullStack'
      TabOrder = 1
      OnClick = btnDisciplinaClick
    end
    object btnDisciplinaUXDesign: TButton
      AlignWithMargins = True
      Left = 419
      Top = 53
      Width = 165
      Height = 24
      Caption = 'UX Design'
      TabOrder = 2
      OnClick = btnDisciplinaClick
    end
    object btnDisciplinaSeguranca: TButton
      Left = 57
      Top = 105
      Width = 165
      Height = 24
      Caption = 'Seguran'#231'a da informa'#231#227'o'
      TabOrder = 3
      OnClick = btnDisciplinaClick
    end
    object btnDisciplinaAnaliseData: TButton
      Left = 242
      Top = 105
      Width = 165
      Height = 24
      Caption = 'Analise de Dados'
      TabOrder = 4
      OnClick = btnDisciplinaClick
    end
    object btnDisciplinaEtica: TButton
      Left = 419
      Top = 105
      Width = 165
      Height = 24
      Caption = #201'tica de Dados'
      TabOrder = 5
      OnClick = btnDisciplinaClick
    end
    object pnlEscolhaProfessor: TPanel
      Left = 36
      Top = 146
      Width = 548
      Height = 135
      Caption = 'pnlEscolhaProfessor'
      ParentBackground = False
      ShowCaption = False
      TabOrder = 0
      Visible = False
      object lblEscolhaProfessor: TLabel
        Left = 175
        Top = 8
        Width = 186
        Height = 15
        Caption = 'Escolha o professor que ir'#225' lecionar'
        Visible = False
      end
      object btnProfessorFausto: TButton
        Left = 276
        Top = 63
        Width = 165
        Height = 24
        Caption = 'Fausto Silva'
        TabOrder = 0
        Visible = False
        OnClick = btnProfessorClick
      end
      object btnProfessorFlavio: TButton
        Left = 92
        Top = 63
        Width = 165
        Height = 24
        Caption = 'Flavio'
        TabOrder = 1
        Visible = False
        OnClick = btnProfessorClick
      end
    end
    object btnDisciplinaInformaticaBasica: TButton
      AlignWithMargins = True
      Left = 57
      Top = 53
      Width = 165
      Height = 24
      Margins.Bottom = 2
      Caption = 'Informatica basica'
      TabOrder = 6
      OnClick = btnDisciplinaClick
    end
  end
  object btnConfirmarNomeEstudante: TButton
    Left = 287
    Top = 51
    Width = 74
    Height = 25
    Caption = 'Confirmar'
    TabOrder = 1
    OnClick = btnConfirmarNomeEstudanteClick
  end
  object edtNomeEstudante: TEdit
    Left = 255
    Top = 22
    Width = 144
    Height = 23
    TabOrder = 2
    TextHint = 'Digite nome do estudante'
  end
  object btnMostrarDados: TButton
    Left = 270
    Top = 381
    Width = 106
    Height = 25
    Caption = 'Mostrar JSON'
    TabOrder = 3
    OnClick = btnMostrarDadosClick
  end
end
