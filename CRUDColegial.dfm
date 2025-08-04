object FColegio_CRUD: TFColegio_CRUD
  Left = 0
  Top = 0
  Caption = 'FColegio_CRUD'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object btnConfirmarNomeEstudante: TButton
    Left = 272
    Top = 64
    Width = 75
    Height = 25
    Caption = 'Confirmar'
    TabOrder = 0
  end
  object pnlEscolhaDisciplina: TPanel
    Left = 8
    Top = 111
    Width = 608
    Height = 322
    Caption = 'pnlEscolhaDisciplina'
    ShowCaption = False
    TabOrder = 1
    object lblEscolhaDisciplina: TLabel
      AlignWithMargins = True
      Left = 4
      Top = 17
      Width = 600
      Height = 15
      Margins.Top = 16
      Align = alTop
      Alignment = taCenter
      Caption = 'Escolha a disciplina a cursar'
      ExplicitLeft = 161
      ExplicitTop = 20
      ExplicitWidth = 146
    end
    object lblEscolhaProfessor: TLabel
      Left = 215
      Top = 161
      Width = 186
      Height = 15
      Caption = 'Escolha o professor que ir'#225' lecionar'
    end
    object btnDisciplinaInformaticaBasica: TButton
      Left = 44
      Top = 49
      Width = 165
      Height = 24
      Caption = 'Informatica basica'
      TabOrder = 1
    end
    object btnDisciplinaFullStack: TButton
      Left = 228
      Top = 49
      Width = 165
      Height = 24
      Caption = 'FullStack'
      TabOrder = 2
    end
    object btnDisciplinaUXDesign: TButton
      Left = 407
      Top = 49
      Width = 165
      Height = 24
      Caption = 'UX Design'
      TabOrder = 3
    end
    object btnDisciplinaSeguranca: TButton
      Left = 44
      Top = 105
      Width = 165
      Height = 24
      Caption = 'Seguran'#231'a da informa'#231#227'o'
      TabOrder = 4
    end
    object btnDisicplinaAnaliseData: TButton
      Left = 228
      Top = 105
      Width = 165
      Height = 24
      Caption = 'Analise de Dados'
      TabOrder = 5
    end
    object btnDisciplinaEtica: TButton
      Left = 407
      Top = 105
      Width = 165
      Height = 24
      Caption = #201'tica de Dados'
      TabOrder = 6
    end
    object btnProfessorFlavio: TButton
      Left = 132
      Top = 201
      Width = 165
      Height = 24
      Caption = 'Flavio'
      TabOrder = 7
    end
    object btnProfessorFausto: TButton
      Left = 316
      Top = 201
      Width = 165
      Height = 24
      Caption = 'Fausto Silva'
      TabOrder = 8
    end
    object pnlEscolhaProfessor: TPanel
      Left = 24
      Top = 154
      Width = 548
      Height = 135
      Caption = 'pnlEscolhaProfessor'
      ParentBackground = False
      ShowCaption = False
      TabOrder = 0
    end
  end
  object edtNomeEstudante: TEdit
    Left = 236
    Top = 35
    Width = 145
    Height = 23
    TabOrder = 2
    Text = 'Digite nome do estudante'
    TextHint = 'Digite nome do estudante'
  end
end
