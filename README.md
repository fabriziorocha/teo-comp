# Teoria da Computação

Este repositório contém um projeto desenvolvido para o curso de Computação Aplicada do IFES, com o professor Jefferson Andrade.

## Execução do programa

### Programa lab01-part1

O progrma **lab01-part1** converte um NFA-e em NFA ou um NFA em DFA. Além de apresentar os passos da conversão em tela, o programa também grava o novo autômato em arquivo.

* Arquivos de saída:
  * nfae-nfa.yaml
  * nfa-dfa.yaml

### Programa lab01-part2

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
cabal run lab01-part2 -- lab01-part2.yaml
```

Arquivo de saida:
* regex-nfae.yaml
* regex-nfae.puml

Para visualizar o automato usando PlantUML:

```bash
plantuml regex-nfae.puml
```
