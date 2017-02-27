# project-template

[![Build Status](https://travis-ci.org/cmc-haskell-2017/project-template.svg?branch=master)](https://travis-ci.org/cmc-haskell-2017/project-template)

Шаблон проекта для выполнения практического задания.

## Сборка и запуск

Соберите проект при помощи [утилиты Stack](https://www.haskellstack.org):

```
stack setup
stack build
```

Собрать и запустить проект можно при помощи команды

```
stack build && stack exec my-project
```

Запустить тесты можно при помощи команды

```
stack test
```

Чтобы запустить интепретатор GHCi и автоматически подгрузить все модули проекта, используйте команду

```
stack ghci
```

