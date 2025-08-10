# Histórico de Alterações do Projeto (CRUD Sistema Acadêmico)
Este documento detalha as principais mudanças realizadas no projeto, incluindo remoções, adições e substituições de arquivos, bem como a evolução da estrutura do sistema.

## 1. Remoção do Projeto Antigo (Crudando)
### Arquivos Removidos:
#### CRUDColegial.dfm
#### CRUDColegial.pas
#### Crudando.dpr
#### Crudando.res
#### database.json
### Motivo:
O projeto anterior, que utilizava uma abordagem baseada em JSON e interface menos estruturada, foi totalmente removido porconta de não atender as requisições técnicas de presença e uso de classes no projeto. Devido a isso o projeto foi reconstruido com ajuda de IA codificadora para agilizar a reescrição do código para permitir mais tempo na minha organização dos elementos com os principios de UX

## 2. Criação do Novo Projeto (SistemaAcademicoProjeto)
### Arquivos Adicionados:
#### SistemaAcademicoProjeto.dpr
Novo arquivo principal do projeto Delphi, inicializando o formulário principal.
#### SistemaAcademicoProjeto.dproj
Arquivo de configuração do projeto Delphi.
#### SistemaAcademico.dfm
Interface visual do novo sistema acadêmico, com abas para Estudantes, Professores, Disciplinas, Turmas e Matrículas.
#### SistemaAcademico.pas
Código-fonte do formulário principal, implementando toda a lógica de CRUD para cada entidade.
### Unidades de Controle e Modelo:
uEstudante.pas
uProfessor.pas
uDisciplina.pas
uTurma.pas
uMatricula.pas
### Motivo:
O novo projeto segue uma arquitetura MVC simplificada, com classes modelo e controladoras para cada entidade, facilitando manutenção, testes e futuras expansões.

## 3. Estrutura de Dados
### Arquivos .txt para Persistência (dentro da pasta onde está o executavel do programa):
estudantes.txt
professores.txt
disciplinas.txt
turmas.txt
matriculas.txt
### Motivo:
Cada entidade possui seu próprio arquivo de dados, facilitando o CRUD e a separação de responsabilidades.

## 4. Implementação das Classes Controladoras
Para cada entidade (Estudante, Professor, Disciplina, Turma, Matrícula), foi criada uma unidade com:

#### Classe modelo (ex: TEstudante)
#### Classe controladora (ex: TEstudanteController)
#### Métodos para incluir, atualizar, excluir, buscar e listar registros
#### Métodos para carregar e salvar dados do respectivo arquivo .txt
## Exemplo de Formato dos Arquivos:

estudantes.txt:
professores.txt:
disciplinas.txt:
turmas.txt:
matriculas.txt:

## 5. Interface Gráfica
O novo formulário principal (SistemaAcademico.dfm) utiliza um TPageControl com abas para cada entidade, permitindo ao usuário navegar e realizar operações CRUD de forma intuitiva.
Cada aba possui campos de entrada, botões de ação e um TListBox para exibição dos registros.
## 6. Resumo das Mudanças
Remoção completa do projeto anterior e seus arquivos.
Criação de um novo projeto, com arquitetura modular e separação clara entre entidades.
Persistência de dados agora em arquivos .txt por entidade na pasta onde está o executavel do programa.
Interface gráfica aprimorada e centralizada.
Código mais organizado, facilitando manutenção e expansão futura.

## 7. Proximas Atividades:

### 1. Controle de Usuários e Permissões

Readicionar login e diferentes níveis de acesso (admin, professor, estudante).

#### Funcionalidades:

- **Tela de Login:**  
  Interface para autenticação dos usuários antes de acessar o sistema.

- **Níveis de Acesso:**  
  - **Administrador:**  
    Acesso total ao sistema, incluindo cadastro, edição e exclusão de todas as entidades, além de gerenciamento de usuários.
  - **Professor:**  
    Acesso restrito às funcionalidades relacionadas às disciplinas e turmas que ministra, podendo visualizar e editar apenas seus dados.
  - **Estudante:**  
    Acesso apenas às informações pessoais, matrículas e disciplinas em que está inscrito.

### 2. Relatórios

Implementar a funcionalidade de geração de relatórios para facilitar a visualização e análise dos dados do sistema acadêmico. Os relatórios podem ser gerados nos formatos PDF ou TXT, conforme a necessidade do usuário.

#### Exemplos de Relatórios:

- **Lista de Estudantes por Turma:**  
  Relatório que apresenta todos os estudantes matriculados em uma determinada turma, incluindo informações como nome, matrícula, CPF e status.

- **Professores por Disciplina:**  
  Relatório que exibe os professores responsáveis por cada disciplina, mostrando nome, área de atuação e disciplinas ministradas.

- **Disciplinas Ofertadas:**  
  Relatório com todas as disciplinas cadastradas, carga horária, professores responsáveis e turmas associadas.

- **Matrículas por Estudante:**  
  Relatório detalhando todas as matrículas de um estudante, incluindo turmas, disciplinas e situação.