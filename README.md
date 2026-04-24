# Teoria da Computação

Este repositório contém um projeto desenvolvido para o curso de Computação Aplicada do IFES, com o professor Jefferson Andrade.

## Compilação

Para compilar os executáveis do projeto:

```bash
stack build
```

## Execução dos programas

### Parte 1: Conversão de Autômatos (NFAɛ → DFA): Programa lab01-part1

O programa **lab01-part1** converte um NFAe em NFA ou um NFA em DFA. Além de apresentar os passos da conversão em tela, o programa também grava o novo autômato em arquivo.

Uso geral:

```bash
stack run lab01-part1 -- <input-file.yaml>
```

Exemplo 1 (entrada NFAe, arquivo `nfae.yaml`):

```bash
stack run lab01-part1 -- nfae.yaml
```

Saída gerada:
* nfae-nfa.yaml

Exemplo 2 (entrada NFA, arquivo `test-nfa.yaml`):

```bash
stack run lab01-part1 -- test-nfa.yaml
```

Saída gerada:
* nfa-dfa.yaml


### Parte 2: Implementação de Expressões Regulares: Programa lab01-part2

O programa **lab01-part2** le uma expressao regular e gera um automato no formato NFAe usando a construcao de Thompson.

Operadores suportados:
* Concatenacao: `ab`
* Uniao: `a|b`
* Fecho de Kleene: `a*`
* Uma ou mais repeticoes: `a+`
* Opcional: `a?` (equivalente a `(a|epsilon)`)

Entrada YAML (exemplo):

```yaml
expression: "(a|b)*abb"
```

Execucao:

```bash
stack run lab01-part2 -- lab01-part2.yaml
```

Arquivo de saida:
* regex-nfae.yaml
* regex-nfae.puml

Para visualizar o automato usando PlantUML:

```bash
plantuml regex-nfae.puml
```

