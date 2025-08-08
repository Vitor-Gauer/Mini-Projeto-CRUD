# Mini-Projeto-CRUD

Este repositório contém um projeto de CRUD desenvolvido como parte de um mini-projeto para a disciplina de Programação Orientada a Objetos (POO). Este README destaca as diferenças entre o código presente na branch `main` e as alterações que serão commitadas nesta versão.

## Diferenças funcionais entre a branch `main` e esta versão

### 1. Arrumação do JSON para estar com formatação 

### 2. Adição do arquivo `database.json`
- **Descrição**: Esta versão inclui o arquivo `database.json` que servirá como exemplo de formatação do arquivo que sera feito para o banco de dados para armazenar informações de alunos e professores dentro da pasta principal.
- **Estrutura**:
  - **Alunos**:
    - Nome
    - Turma
    - Matrícula
    - Disciplina
    - Código do professor responsável
  - **Professores**:
    - Nome
    - CPF
    - Código do professor
- **Exemplo de conteúdo**:
  {
    "alunos": [
      {
        "nome": "Arthur",
        "turma": "1A",
        "matricula": "MA#2025020000",
        "disciplina": "Informatica basica",
        "professor_codigo": "PROF01"
      }
    ],
    "professores": [
      {
        "nome": "Flavio",
        "cpf": "123.456.789-00",
        "codigo": "PROF01"
      }
    ]
  }

## Proxima Atividade:

- ### Fazer o database.json funcionar para guardar as informações ao em vez de ter de ir ao AppData  (should) 
- ### Fazer o database realmente ser um database banco de dados (could)
- ### Fazer sistema de acesso a informação sensível/fora de escopo do usuario, como um estudante não precisando saber do cpf do professor (should)
- ### Estética de UX e UI (should)
