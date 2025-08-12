object FSistemaAcademico: TFSistemaAcademico
  Left = 0
  Top = 0
  Caption = 'Sistema Acad'#234'mico'
  ClientHeight = 611
  ClientWidth = 883
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object pgcAbasDoForm: TPageControl
    Left = 0
    Top = 0
    Width = 883
    Height = 611
    ActivePage = tabEstudantes
    Align = alClient
    TabOrder = 0
    object tabEstudantes: TTabSheet
      Caption = 'Estudantes'
      object pnlEstudantes: TPanel
        Left = 0
        Top = 0
        Width = 875
        Height = 583
        Align = alClient
        TabOrder = 0
        object lblCodigoEst: TLabel
          Left = 16
          Top = 16
          Width = 37
          Height = 13
          Caption = 'C'#243'digo:'
        end
        object lblNomeEst: TLabel
          Left = 16
          Top = 43
          Width = 31
          Height = 13
          Caption = 'Nome:'
        end
        object edtCodigoEst: TEdit
          Left = 59
          Top = 13
          Width = 121
          Height = 21
          TabOrder = 0
        end
        object edtNomeEst: TEdit
          Left = 59
          Top = 40
          Width = 285
          Height = 21
          TabOrder = 1
        end
        object btnIncluirEst: TButton
          Left = 16
          Top = 75
          Width = 75
          Height = 25
          Caption = 'Incluir'
          TabOrder = 2
          OnClick = btnIncluirEstClick
        end
        object btnAtualizarEst: TButton
          Left = 178
          Top = 75
          Width = 75
          Height = 25
          Caption = 'Atualizar'
          TabOrder = 3
          OnClick = btnAtualizarEstClick
        end
        object btnExcluirEst: TButton
          Left = 259
          Top = 75
          Width = 75
          Height = 25
          Caption = 'Excluir'
          TabOrder = 4
          OnClick = btnExcluirEstClick
        end
        object btnBuscarEst: TButton
          Left = 97
          Top = 75
          Width = 75
          Height = 25
          Caption = 'Buscar'
          TabOrder = 5
          OnClick = btnBuscarEstClick
        end
        object lbxEstudantes: TListBox
          Left = 16
          Top = 115
          Width = 841
          Height = 400
          ItemHeight = 13
          TabOrder = 6
          OnClick = lbxEstudantesClick
        end
        object btnRelatorioEstudantes: TButton
          Left = 16
          Top = 530
          Width = 150
          Height = 25
          Caption = 'Estudantes por Turma'
          TabOrder = 7
          OnClick = btnRelatorioEstudantesClick
        end
        object btnRelatorioProfessores: TButton
          Left = 170
          Top = 530
          Width = 150
          Height = 25
          Caption = 'Professores por Disciplina'
          TabOrder = 8
          OnClick = btnRelatorioProfessoresClick
        end
        object btnRelatorioDisciplinas: TButton
          Left = 325
          Top = 530
          Width = 150
          Height = 25
          Caption = 'Disciplinas Ofertadas'
          TabOrder = 9
          OnClick = btnRelatorioDisciplinasClick
        end
        object btnRelatorioMatriculas: TButton
          Left = 480
          Top = 530
          Width = 150
          Height = 25
          Caption = 'Matr'#237'culas por Estudante'
          TabOrder = 10
          OnClick = btnRelatorioMatriculasClick
        end
      end
    end
    object tabProfessores: TTabSheet
      Caption = 'Professores'
      ImageIndex = 1
      object pnlProfessores: TPanel
        Left = 0
        Top = 0
        Width = 875
        Height = 583
        Align = alClient
        TabOrder = 0
        object lblCodigoProf: TLabel
          Left = 16
          Top = 16
          Width = 37
          Height = 13
          Caption = 'C'#243'digo:'
        end
        object lblNomeProf: TLabel
          Left = 16
          Top = 43
          Width = 31
          Height = 13
          Caption = 'Nome:'
        end
        object lblCPFProf: TLabel
          Left = 16
          Top = 70
          Width = 23
          Height = 13
          Caption = 'CPF:'
        end
        object edtCodigoProf: TEdit
          Left = 59
          Top = 13
          Width = 121
          Height = 21
          TabOrder = 0
        end
        object edtNomeProf: TEdit
          Left = 59
          Top = 40
          Width = 285
          Height = 21
          TabOrder = 1
        end
        object edtCPFProf: TEdit
          Left = 59
          Top = 67
          Width = 121
          Height = 21
          TabOrder = 2
        end
        object btnIncluirProf: TButton
          Left = 16
          Top = 100
          Width = 75
          Height = 25
          Caption = 'Incluir'
          TabOrder = 3
          OnClick = btnIncluirProfClick
        end
        object btnAtualizarProf: TButton
          Left = 178
          Top = 100
          Width = 75
          Height = 25
          Caption = 'Atualizar'
          TabOrder = 4
          OnClick = btnAtualizarProfClick
        end
        object btnExcluirProf: TButton
          Left = 259
          Top = 100
          Width = 75
          Height = 25
          Caption = 'Excluir'
          TabOrder = 5
          OnClick = btnExcluirProfClick
        end
        object btnBuscarProf: TButton
          Left = 97
          Top = 100
          Width = 75
          Height = 25
          Caption = 'Buscar'
          TabOrder = 6
          OnClick = btnBuscarProfClick
        end
        object lbxProfessores: TListBox
          Left = 16
          Top = 140
          Width = 841
          Height = 421
          ItemHeight = 13
          TabOrder = 7
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
        Width = 875
        Height = 583
        Align = alClient
        TabOrder = 0
        object lblCodigoDisc: TLabel
          Left = 16
          Top = 16
          Width = 37
          Height = 13
          Caption = 'C'#243'digo:'
        end
        object lblNomeDisc: TLabel
          Left = 16
          Top = 43
          Width = 31
          Height = 13
          Caption = 'Nome:'
        end
        object edtCodigoDisc: TEdit
          Left = 59
          Top = 13
          Width = 121
          Height = 21
          TabOrder = 0
        end
        object edtNomeDisc: TEdit
          Left = 59
          Top = 40
          Width = 285
          Height = 21
          TabOrder = 1
        end
        object btnIncluirDisc: TButton
          Left = 16
          Top = 75
          Width = 75
          Height = 25
          Caption = 'Incluir'
          TabOrder = 2
          OnClick = btnIncluirDiscClick
        end
        object btnAtualizarDisc: TButton
          Left = 178
          Top = 75
          Width = 75
          Height = 25
          Caption = 'Atualizar'
          TabOrder = 3
          OnClick = btnAtualizarDiscClick
        end
        object btnExcluirDisc: TButton
          Left = 259
          Top = 75
          Width = 75
          Height = 25
          Caption = 'Excluir'
          TabOrder = 4
          OnClick = btnExcluirDiscClick
        end
        object btnBuscarDisc: TButton
          Left = 97
          Top = 75
          Width = 75
          Height = 25
          Caption = 'Buscar'
          TabOrder = 5
          OnClick = btnBuscarDiscClick
        end
        object lbxDisciplinas: TListBox
          Left = 16
          Top = 115
          Width = 841
          Height = 446
          ItemHeight = 13
          TabOrder = 6
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
        Width = 875
        Height = 583
        Align = alClient
        TabOrder = 0
        object lblCodigoTurma: TLabel
          Left = 16
          Top = 16
          Width = 37
          Height = 13
          Caption = 'C'#243'digo:'
        end
        object lblCodigoProfTurma: TLabel
          Left = 16
          Top = 43
          Width = 50
          Height = 13
          Caption = 'Professor:'
        end
        object lblCodigoDiscTurma: TLabel
          Left = 16
          Top = 70
          Width = 47
          Height = 13
          Caption = 'Disciplina:'
        end
        object edtCodigoTurma: TEdit
          Left = 74
          Top = 13
          Width = 121
          Height = 21
          TabOrder = 0
        end
        object edtCodigoProfTurma: TEdit
          Left = 74
          Top = 40
          Width = 121
          Height = 21
          TabOrder = 1
        end
        object edtCodigoDiscTurma: TEdit
          Left = 74
          Top = 67
          Width = 121
          Height = 21
          TabOrder = 2
        end
        object btnIncluirTurma: TButton
          Left = 16
          Top = 100
          Width = 75
          Height = 25
          Caption = 'Incluir'
          TabOrder = 3
          OnClick = btnIncluirTurmaClick
        end
        object btnAtualizarTurma: TButton
          Left = 178
          Top = 100
          Width = 75
          Height = 25
          Caption = 'Atualizar'
          TabOrder = 4
          OnClick = btnAtualizarTurmaClick
        end
        object btnExcluirTurma: TButton
          Left = 259
          Top = 100
          Width = 75
          Height = 25
          Caption = 'Excluir'
          TabOrder = 5
          OnClick = btnExcluirTurmaClick
        end
        object btnBuscarTurma: TButton
          Left = 97
          Top = 100
          Width = 75
          Height = 25
          Caption = 'Buscar'
          TabOrder = 6
          OnClick = btnBuscarTurmaClick
        end
        object lbxTurmas: TListBox
          Left = 16
          Top = 140
          Width = 841
          Height = 421
          ItemHeight = 13
          TabOrder = 7
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
        Width = 875
        Height = 583
        Align = alClient
        TabOrder = 0
        object lblCodigoMat: TLabel
          Left = 16
          Top = 16
          Width = 37
          Height = 13
          Caption = 'C'#243'digo:'
        end
        object lblCodigoTurmaMat: TLabel
          Left = 16
          Top = 43
          Width = 34
          Height = 13
          Caption = 'Turma:'
        end
        object lblCodigoEstMat: TLabel
          Left = 16
          Top = 70
          Width = 53
          Height = 13
          Caption = 'Estudante:'
        end
        object edtCodigoMat: TEdit
          Left = 74
          Top = 13
          Width = 121
          Height = 21
          TabOrder = 0
        end
        object edtCodigoTurmaMat: TEdit
          Left = 74
          Top = 40
          Width = 121
          Height = 21
          TabOrder = 1
        end
        object edtCodigoEstMat: TEdit
          Left = 74
          Top = 67
          Width = 121
          Height = 21
          TabOrder = 2
        end
        object btnIncluirMat: TButton
          Left = 16
          Top = 100
          Width = 75
          Height = 25
          Caption = 'Incluir'
          TabOrder = 3
          OnClick = btnIncluirMatClick
        end
        object btnAtualizarMat: TButton
          Left = 178
          Top = 100
          Width = 75
          Height = 25
          Caption = 'Atualizar'
          TabOrder = 4
          OnClick = btnAtualizarMatClick
        end
        object btnExcluirMat: TButton
          Left = 259
          Top = 100
          Width = 75
          Height = 25
          Caption = 'Excluir'
          TabOrder = 5
          OnClick = btnExcluirMatClick
        end
        object btnBuscarMat: TButton
          Left = 97
          Top = 100
          Width = 75
          Height = 25
          Caption = 'Buscar'
          TabOrder = 6
          OnClick = btnBuscarMatClick
        end
        object lbxMatriculas: TListBox
          Left = 16
          Top = 140
          Width = 841
          Height = 421
          ItemHeight = 13
          TabOrder = 7
          OnClick = lbxMatriculasClick
        end
      end
    end
  end
end
