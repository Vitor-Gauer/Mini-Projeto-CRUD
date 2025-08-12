object FLocalLogin: TFLocalLogin
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Login - Sistema Acad'#234'mico'
  ClientHeight = 180
  ClientWidth = 300
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnShow = FormShow
  TextHeight = 13
  object lblTitulo: TLabel
    Left = 90
    Top = 15
    Width = 151
    Height = 19
    Caption = 'Acesso ao Sistema'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblUsuario: TLabel
    Left = 30
    Top = 60
    Width = 40
    Height = 13
    Caption = 'Usu'#225'rio:'
  end
  object lblSenha: TLabel
    Left = 30
    Top = 90
    Width = 34
    Height = 13
    Caption = 'Senha:'
  end
  object edtUsuario: TEdit
    Left = 80
    Top = 57
    Width = 180
    Height = 21
    TabOrder = 0
  end
  object edtSenha: TEdit
    Left = 80
    Top = 87
    Width = 180
    Height = 21
    PasswordChar = '*'
    TabOrder = 1
  end
  object btnEntrar: TButton
    Left = 80
    Top = 125
    Width = 75
    Height = 25
    Caption = 'Entrar'
    TabOrder = 2
    OnClick = btnEntrarClick
  end
  object btnCancelar: TButton
    Left = 185
    Top = 125
    Width = 75
    Height = 25
    Caption = 'Cancelar'
    TabOrder = 3
    OnClick = btnCancelarClick
  end
end