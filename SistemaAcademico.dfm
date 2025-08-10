object FSistemaAcademico: TFSistemaAcademico
  Left = 0
  Top = 0
  Caption = 'Sistema Acad'#234'mico'
  ClientHeight = 600
  ClientWidth = 900
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    900
    600)
  TextHeight = 13
  object pgcAbasDoForm: TPageControl
    Left = 8
    Top = 8
    Width = 884
    Height = 584
    ActivePage = tabDisciplinas
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object tabEstudantes: TTabSheet
      Caption = 'Estudantes'
      object pnlEstudantes: TPanel
        Left = 0
        Top = 0
        Width = 876
        Height = 556
        Align = alClient
        TabOrder = 0
        object lblCodigoEst: TLabel
          Left = 16
          Top = 16
          Width = 33
          Height = 13
          Caption = 'C'#243'digo'
        end
        object lblNomeEst: TLabel
          Left = 16
          Top = 56
          Width = 31
          Height = 13
          Caption = 'Nome:'
        end
        object edtCodigoEst: TEdit
          Left = 80
          Top = 13
          Width = 121
          Height = 21
          TabOrder = 0
        end
        object edtNomeEst: TEdit
          Left = 80
          Top = 53
          Width = 300
          Height = 21
          TabOrder = 1
        end
        object btnIncluirEst: TButton
          Left = 16
          Top = 90
          Width = 75
          Height = 25
          Caption = 'Incluir'
          TabOrder = 2
          OnClick = btnIncluirEstClick
        end
        object btnListarEst: TButton
          Left = 97
          Top = 90
          Width = 75
          Height = 25
          Caption = 'Listar'
          TabOrder = 3
          OnClick = btnListarEstClick
        end
        object btnAtualizarEst: TButton
          Left = 178
          Top = 90
          Width = 75
          Height = 25
          Caption = 'Atualizar'
          TabOrder = 4
          OnClick = btnAtualizarEstClick
        end
        object btnExcluirEst: TButton
          Left = 259
          Top = 90
          Width = 75
          Height = 25
          Caption = 'Excluir'
          TabOrder = 5
          OnClick = btnExcluirEstClick
        end
        object btnBuscarEst: TButton
          Left = 340
          Top = 90
          Width = 75
          Height = 25
          Caption = 'Buscar'
          TabOrder = 6
          OnClick = btnBuscarEstClick
        end
        object lbxEstudantes: TListBox
          Left = 16
          Top = 130
          Width = 844
          Height = 410
          ItemHeight = 13
          TabOrder = 7
          OnClick = lbxEstudantesClick
        end
      end
    end
    object tabProfessores: TTabSheet
      Caption = 'Professores'
      ImageIndex = 1
      object pnlProfessores: TPanel
        Left = 0
        Top = 0
        Width = 876
        Height = 556
        Align = alClient
        TabOrder = 0
        object lblCodigoProf: TLabel
          Left = 16
          Top = 16
          Width = 33
          Height = 13
          Caption = 'C'#243'digo'
        end
        object lblNomeProf: TLabel
          Left = 16
          Top = 56
          Width = 31
          Height = 13
          Caption = 'Nome:'
        end
        object lblCPFProf: TLabel
          Left = 16
          Top = 96
          Width = 23
          Height = 13
          Caption = 'CPF:'
        end
        object edtCodigoProf: TEdit
          Left = 80
          Top = 13
          Width = 121
          Height = 21
          TabOrder = 0
        end
        object edtNomeProf: TEdit
          Left = 80
          Top = 53
          Width = 300
          Height = 21
          TabOrder = 1
        end
        object edtCPFProf: TEdit
          Left = 80
          Top = 93
          Width = 150
          Height = 21
          TabOrder = 2
        end
        object btnIncluirProf: TButton
          Left = 16
          Top = 130
          Width = 75
          Height = 25
          Caption = 'Incluir'
          TabOrder = 3
          OnClick = btnIncluirProfClick
        end
        object btnListarProf: TButton
          Left = 97
          Top = 130
          Width = 75
          Height = 25
          Caption = 'Listar'
          TabOrder = 4
          OnClick = btnListarProfClick
        end
        object btnAtualizarProf: TButton
          Left = 178
          Top = 130
          Width = 75
          Height = 25
          Caption = 'Atualizar'
          TabOrder = 5
          OnClick = btnAtualizarProfClick
        end
        object btnExcluirProf: TButton
          Left = 259
          Top = 130
          Width = 75
          Height = 25
          Caption = 'Excluir'
          TabOrder = 6
          OnClick = btnExcluirProfClick
        end
        object btnBuscarProf: TButton
          Left = 340
          Top = 130
          Width = 75
          Height = 25
          Caption = 'Buscar'
          TabOrder = 7
          OnClick = btnBuscarProfClick
        end
        object lbxProfessores: TListBox
          Left = 16
          Top = 170
          Width = 844
          Height = 370
          ItemHeight = 13
          TabOrder = 8
          OnClick = lbxProfessoresClick
        end
      end
    end
    object tabDisciplinas: TTabSheet
      Caption = 'Disciplinas'
      ImageIndex = 2
      object pnlDisciplinas: TPanel
        Left = 0
        Top = 0
        Width = 876
        Height = 556
        Align = alClient
        TabOrder = 0
        object lblCodigoDisc: TLabel
          Left = 16
          Top = 16
          Width = 33
          Height = 13
          Caption = 'C'#243'digo'
        end
        object lblNomeDisc: TLabel
          Left = 16
          Top = 56
          Width = 31
          Height = 13
          Caption = 'Nome:'
        end
        object edtCodigoDisc: TEdit
          Left = 80
          Top = 13
          Width = 121
          Height = 21
          TabOrder = 0
        end
        object edtNomeDisc: TEdit
          Left = 80
          Top = 53
          Width = 300
          Height = 21
          TabOrder = 1
        end
        object btnIncluirDisc: TButton
          Left = 16
          Top = 90
          Width = 75
          Height = 25
          Caption = 'Incluir'
          TabOrder = 2
          OnClick = btnIncluirDiscClick
        end
        object btnListarDisc: TButton
          Left = 97
          Top = 90
          Width = 75
          Height = 25
          Caption = 'Listar'
          TabOrder = 3
          OnClick = btnListarDiscClick
        end
        object btnAtualizarDisc: TButton
          Left = 178
          Top = 90
          Width = 75
          Height = 25
          Caption = 'Atualizar'
          TabOrder = 4
          OnClick = btnAtualizarDiscClick
        end
        object btnExcluirDisc: TButton
          Left = 259
          Top = 90
          Width = 75
          Height = 25
          Caption = 'Excluir'
          TabOrder = 5
          OnClick = btnExcluirDiscClick
        end
        object btnBuscarDisc: TButton
          Left = 340
          Top = 90
          Width = 75
          Height = 25
          Caption = 'Buscar'
          TabOrder = 6
          OnClick = btnBuscarDiscClick
        end
        object lbxDisciplinas: TListBox
          Left = 16
          Top = 130
          Width = 844
          Height = 410
          ItemHeight = 13
          TabOrder = 7
          OnClick = lbxDisciplinasClick
        end
      end
    end
    object tabTurmas: TTabSheet
      Caption = 'Turmas'
      ImageIndex = 3
      object pnlTurmas: TPanel
        Left = 0
        Top = 0
        Width = 876
        Height = 556
        Align = alClient
        TabOrder = 0
        object lblCodigoTurma: TLabel
          Left = 16
          Top = 16
          Width = 33
          Height = 13
          Caption = 'C'#243'digo'
        end
        object lblCodigoProfTurma: TLabel
          Left = 16
          Top = 56
          Width = 86
          Height = 13
          Caption = 'C'#243'digo Professor:'
        end
        object lblCodigoDiscTurma: TLabel
          Left = 16
          Top = 96
          Width = 83
          Height = 13
          Caption = 'C'#243'digo Disciplina:'
        end
        object edtCodigoTurma: TEdit
          Left = 120
          Top = 13
          Width = 121
          Height = 21
          TabOrder = 0
        end
        object edtCodigoProfTurma: TEdit
          Left = 120
          Top = 53
          Width = 121
          Height = 21
          TabOrder = 1
        end
        object edtCodigoDiscTurma: TEdit
          Left = 120
          Top = 93
          Width = 121
          Height = 21
          TabOrder = 2
        end
        object btnIncluirTurma: TButton
          Left = 16
          Top = 130
          Width = 75
          Height = 25
          Caption = 'Incluir'
          TabOrder = 3
          OnClick = btnIncluirTurmaClick
        end
        object btnListarTurma: TButton
          Left = 97
          Top = 130
          Width = 75
          Height = 25
          Caption = 'Listar'
          TabOrder = 4
          OnClick = btnListarTurmaClick
        end
        object btnAtualizarTurma: TButton
          Left = 178
          Top = 130
          Width = 75
          Height = 25
          Caption = 'Atualizar'
          TabOrder = 5
          OnClick = btnAtualizarTurmaClick
        end
        object btnExcluirTurma: TButton
          Left = 259
          Top = 130
          Width = 75
          Height = 25
          Caption = 'Excluir'
          TabOrder = 6
          OnClick = btnExcluirTurmaClick
        end
        object btnBuscarTurma: TButton
          Left = 340
          Top = 130
          Width = 75
          Height = 25
          Caption = 'Buscar'
          TabOrder = 7
          OnClick = btnBuscarTurmaClick
        end
        object lbxTurmas: TListBox
          Left = 16
          Top = 170
          Width = 844
          Height = 370
          ItemHeight = 13
          TabOrder = 8
          OnClick = lbxTurmasClick
        end
      end
    end
    object tabMatriculas: TTabSheet
      Caption = 'Matr'#237'culas'
      ImageIndex = 4
      object pnlMatriculas: TPanel
        Left = 0
        Top = 0
        Width = 876
        Height = 556
        Align = alClient
        TabOrder = 0
        object lblCodigoMat: TLabel
          Left = 16
          Top = 16
          Width = 33
          Height = 13
          Caption = 'C'#243'digo'
        end
        object lblCodigoTurmaMat: TLabel
          Left = 16
          Top = 56
          Width = 70
          Height = 13
          Caption = 'C'#243'digo Turma:'
        end
        object lblCodigoEstMat: TLabel
          Left = 16
          Top = 96
          Width = 89
          Height = 13
          Caption = 'C'#243'digo Estudante:'
        end
        object edtCodigoMat: TEdit
          Left = 120
          Top = 13
          Width = 121
          Height = 21
          TabOrder = 0
        end
        object edtCodigoTurmaMat: TEdit
          Left = 120
          Top = 53
          Width = 121
          Height = 21
          TabOrder = 1
        end
        object edtCodigoEstMat: TEdit
          Left = 120
          Top = 93
          Width = 121
          Height = 21
          TabOrder = 2
        end
        object btnIncluirMat: TButton
          Left = 16
          Top = 130
          Width = 75
          Height = 25
          Caption = 'Incluir'
          TabOrder = 3
          OnClick = btnIncluirMatClick
        end
        object btnListarMat: TButton
          Left = 97
          Top = 130
          Width = 75
          Height = 25
          Caption = 'Listar'
          TabOrder = 4
          OnClick = btnListarMatClick
        end
        object btnAtualizarMat: TButton
          Left = 178
          Top = 130
          Width = 75
          Height = 25
          Caption = 'Atualizar'
          TabOrder = 5
          OnClick = btnAtualizarMatClick
        end
        object btnExcluirMat: TButton
          Left = 259
          Top = 130
          Width = 75
          Height = 25
          Caption = 'Excluir'
          TabOrder = 6
          OnClick = btnExcluirMatClick
        end
        object btnBuscarMat: TButton
          Left = 340
          Top = 130
          Width = 75
          Height = 25
          Caption = 'Buscar'
          TabOrder = 7
          OnClick = btnBuscarMatClick
        end
        object lbxMatriculas: TListBox
          Left = 16
          Top = 170
          Width = 844
          Height = 370
          ItemHeight = 13
          TabOrder = 8
          OnClick = lbxMatriculasClick
        end
      end
    end
  end
end
