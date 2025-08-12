object FCadastroUsuarios: TFCadastroUsuarios
  Left = 0
  Top = 0
  Caption = 'Cadastro de Usu'#225'rios'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  TextHeight = 15
  object pnlDadosUsuario: TPanel
    Left = 0
    Top = 0
    Width = 624
    Height = 150
    Align = alTop
    TabOrder = 0
    object lblCodigo: TLabel
      Left = 16
      Top = 16
      Width = 42
      Height = 15
      Caption = 'C'#243'digo:'
    end
    object lblUsuario: TLabel
      Left = 16
      Top = 46
      Width = 43
      Height = 15
      Caption = 'Usu'#225'rio:'
    end
    object lblSenha: TLabel
      Left = 16
      Top = 76
      Width = 35
      Height = 15
      Caption = 'Senha:'
    end
    object lblNivel: TLabel
      Left = 16
      Top = 106
      Width = 86
      Height = 15
      Caption = 'N'#237'vel de Acesso:'
    end
    object edtCodigo: TEdit
      Left = 112
      Top = 13
      Width = 121
      Height = 23
      Enabled = False
      TabOrder = 0
    end
    object edtUsuario: TEdit
      Left = 112
      Top = 43
      Width = 121
      Height = 23
      TabOrder = 1
    end
    object edtSenha: TEdit
      Left = 112
      Top = 73
      Width = 121
      Height = 23
      PasswordChar = '*'
      TabOrder = 2
    end
    object cmbNivel: TComboBox
      Left = 112
      Top = 103
      Width = 121
      Height = 23
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 3
      Text = 'admin'
      Items.Strings = (
        'admin'
        'professor'
        'estudante')
    end
    object btnIncluir: TButton
      Left = 280
      Top = 13
      Width = 75
      Height = 25
      Caption = 'Incluir'
      TabOrder = 4
      OnClick = btnIncluirClick
    end
    object btnAtualizar: TButton
      Left = 280
      Top = 44
      Width = 75
      Height = 25
      Caption = 'Atualizar'
      Enabled = False
      TabOrder = 5
      OnClick = btnAtualizarClick
    end
    object btnExcluir: TButton
      Left = 280
      Top = 75
      Width = 75
      Height = 25
      Caption = 'Excluir'
      Enabled = False
      TabOrder = 6
      OnClick = btnExcluirClick
    end
    object btnBuscar: TButton
      Left = 361
      Top = 13
      Width = 75
      Height = 25
      Caption = 'Buscar'
      TabOrder = 7
      OnClick = btnBuscarClick
    end
    object btnLimpar: TButton
      Left = 361
      Top = 44
      Width = 75
      Height = 25
      Caption = 'Limpar'
      TabOrder = 8
      OnClick = btnLimparClick
    end
  end
  object lbxUsuarios: TListBox
    Left = 0
    Top = 150
    Width = 624
    Height = 250
    Align = alClient
    ItemHeight = 15
    TabOrder = 1
    OnClick = lbxUsuariosClick
  end
  object btnFechar: TButton
    Left = 0
    Top = 400
    Width = 624
    Height = 41
    Align = alBottom
    Caption = 'Fechar'
    TabOrder = 2
    OnClick = btnFecharClick
  end
end
