# Histórico de Alterações do Projeto: Sistema Acadêmico (CRUD)

Este documento detalha as principais mudanças realizadas no projeto, desde a remoção do sistema antigo até a implementação de uma arquitetura robusta com persistência de dados em formato textual legível.

---

## 1. Remoção do Projeto Antigo (`Crudando`)
### Arquivos Removidos:
- `CRUDColegial.dfm`
- `CRUDColegial.pas`
- `Crudando.dpr`
- `Crudando.res`
- `database.json`

### Motivo:
O projeto anterior, que utilizava uma abordagem baseada em JSON e interface menos estruturada, foi totalmente removido porconta de não atender as requisições técnicas de presença e uso de classes no projeto. Devido a isso o projeto foi reconstruido com ajuda de IA codificadora para agilizar a reescrição do código para permitir mais tempo na minha organização dos elementos com os principios de UX

---

## 2. Criação do Novo Projeto: `SistemaAcademicoProjeto`
### Arquivos Adicionados:
- `SistemaAcademicoProjeto.dpr`: Arquivo principal do projeto Delphi.
- `SistemaAcademicoProjeto.dproj`: Arquivo de configuração do projeto.
- `SistemaAcademico.dfm`: Interface visual com abas para Estudantes, Professores, Disciplinas, Turmas e Matrículas.
- `SistemaAcademico.pas`: Código-fonte do formulário principal com lógica de CRUD e controle de permissões.

### Unidades de Modelo e Controle:
- `uEstudante.pas`
- `uProfessor.pas`
- `uDisciplina.pas`
- `uTurma.pas`
- `uMatricula.pas`

> ✅ **Arquitetura**: Implementação de MVC simplificado com classes modelo e controladoras para cada entidade.

## 3. Estrutura de Dados e Persistência de Arquivos
### Arquivos `.txt` para Persistência (criados na pasta do executável):
- `estudantes.txt`
- `professores.txt`
- `disciplinas.txt`
- `turmas.txt`
- `matriculas.txt`

### Problema Anterior ao commit do dia 13/08/2025:
Os métodos `SalvarDados` e `CarregarDados` usavam `TFileStream` para salvar dados binários diretamente, sem registro de tamanho de strings, resultando em dados corrompidos ou ilegíveis.

### Solução Implementada:
Os métodos foram **reescritos completamente** para usar `TextFile` (ou `TStringList`) e salvar os dados em formato **texto plano**, com campos separados por ponto e vírgula (`;`). Isso garantiu:

- ✅ Arquivos `.txt` legíveis em qualquer editor (Notepad, VS Code).
- ✅ Exibição correta dos dados no `TListBox`.
- ✅ Facilidade de depuração e manutenção.

---

### 📌 Exemplo de Arquivos com o formato corrigido =

#### `estudantes.txt`
1;João
2;Maria
#### `professores.txt`
1;Carlos;12345678901
2;Ana;98765432109
#### `disciplinas.txt`
1;Matemática
2;Português
3;Qualidade
#### `turmas.txt`
1;1;1
2;2;2
#### `matriculas.txt`
101;1;1
102;1;2
103;2;1
> ✅ Todos os registros são salvos em linhas separadas, com campos no formato `código;nome` ou `código;código2;código3`.