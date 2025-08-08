object FColegio_CRUD: TFColegio_CRUD
  Left = 633
  Top = 228
  BorderStyle = bsSingle
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
    Left = 24
    Top = 82
    Width = 618
    Height = 350
    Margins.Top = 20
    Margins.Bottom = 20
    Caption = 'pnlEscolhaDisciplina'
    ShowCaption = False
    TabOrder = 0
    Visible = False
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
      Caption = 'An'#225'lise de Dados'
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
      Left = 44
      Top = 148
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
      Caption = 'Inform'#225'tica b'#225'sica'
      TabOrder = 6
      OnClick = btnDisciplinaClick
    end
    object btnMostrarDados: TButton
      Left = 242
      Top = 301
      Width = 106
      Height = 25
      Caption = 'Mostrar JSON'
      TabOrder = 7
      OnClick = btnMostrarDadosClick
    end
  end
  object btnConfirmarNomeEstudante: TButton
    Left = 287
    Top = 51
    Width = 74
    Height = 25
    Caption = 'Confirmar'
    TabOrder = 1
    Visible = False
    OnClick = btnConfirmarNomeEstudanteClick
  end
  object edtNomeEstudante: TEdit
    Left = 255
    Top = 22
    Width = 144
    Height = 23
    TabOrder = 2
    TextHint = 'Digite nome do estudante'
    Visible = False
  end
  object pnlLogin: TPanel
    Left = 48
    Top = 96
    Width = 577
    Height = 322
    Caption = 'pnlLogin'
    ShowCaption = False
    TabOrder = 3
    object lblLogin: TLabel
      Left = 64
      Top = 7
      Width = 434
      Height = 40
      Caption = 'Fa'#231'a o login para seguir em frente'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = 40
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object edtUsuario: TEdit
      Left = 204
      Top = 69
      Width = 144
      Height = 23
      TabOrder = 0
      TextHint = 'Usuario'
    end
    object btnConfirmarLogin: TButton
      Left = 241
      Top = 146
      Width = 74
      Height = 25
      Caption = 'Confirmar'
      TabOrder = 1
      OnClick = btnConfirmarLoginClick
    end
    object edtSenha: TEdit
      Left = 204
      Top = 113
      Width = 143
      Height = 23
      TabOrder = 2
      TextHint = 'Senha'
    end
  end
  object pnlCaminhoEscolha: TPanel
    Left = 8
    Top = 8
    Width = 649
    Height = 481
    Caption = 'pnlCaminhoEscolha'
    ShowCaption = False
    TabOrder = 4
    object lblExibirLista: TLabel
      Left = 82
      Top = 199
      Width = 146
      Height = 15
      Caption = 'Ver lista j'#225' existente por tipo'
    end
    object lblAdicionarPessoasNaLista: TLabel
      Left = 410
      Top = 201
      Width = 128
      Height = 15
      Caption = 'Adicionar outros na lista'
    end
    object lblEscolhaCaminho: TLabel
      Left = 191
      Top = 11
      Width = 263
      Height = 40
      Caption = 'Escolha seu caminho'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = 40
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object btnExibirLista: TButton
      Left = 120
      Top = 226
      Width = 75
      Height = 25
      Caption = 'Ver Lista'
      TabOrder = 0
      OnClick = btnExibirListaClick
    end
    object btnAdicionarPessoasNaLista: TButton
      Left = 427
      Top = 228
      Width = 94
      Height = 25
      Caption = 'Adicionar '#224' lista'
      TabOrder = 1
      OnClick = btnAdicionarPessoasNaListaClick
    end
    object pnlVerLista: TPanel
      Left = 40
      Top = 0
      Width = 577
      Height = 481
      Caption = 'pnlLogin'
      ShowCaption = False
      TabOrder = 2
      Visible = False
      object lblOQVaiEditarNoListBox: TLabel
        Left = 64
        Top = 11
        Width = 434
        Height = 40
        Caption = 'Escolha o que vai editar no listbox'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = 40
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object pnlListBoxComEscolhaFeita: TPanel
        Left = 0
        Top = 57
        Width = 577
        Height = 407
        Caption = 'pnlListBoxComEscolhaFeita'
        ShowCaption = False
        TabOrder = 5
        Visible = False
        object lbxVisualizarDados: TListBox
          Left = 95
          Top = 136
          Width = 403
          Height = 255
          ItemHeight = 15
          TabOrder = 0
        end
      end
      object btnEditarNoListboxProfessores: TButton
        Left = 144
        Top = 66
        Width = 74
        Height = 25
        Caption = 'Professores'
        TabOrder = 0
        OnClick = btnEditarNoListbox
      end
      object btnEditarNoListboxEstudantes: TButton
        Left = 251
        Top = 93
        Width = 74
        Height = 25
        Caption = 'Estudantes'
        TabOrder = 1
        OnClick = btnEditarNoListbox
      end
      object btnEditarNoListboxTurmas: TButton
        Left = 144
        Top = 119
        Width = 74
        Height = 25
        Caption = 'Turmas'
        TabOrder = 2
        OnClick = btnEditarNoListbox
      end
      object btnEditarNoListboxDisciplinas: TButton
        Left = 363
        Top = 66
        Width = 74
        Height = 25
        Caption = 'Disciplinas'
        TabOrder = 3
        OnClick = btnEditarNoListbox
      end
      object btnEditarNoListboxMatriculas: TButton
        Left = 363
        Top = 120
        Width = 74
        Height = 25
        Caption = 'Matriculas'
        TabOrder = 4
        OnClick = btnEditarNoListbox
      end
    end
  end
end
