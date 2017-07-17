# renju

Аналог игры Рейндзю. Спортивный вариант крестиков-ноликов по 5 в ряд.

[![Build Status](https://travis-ci.org/cmc-haskell-2016/renju.svg?branch=master)](https://travis-ci.org/cmc-haskell-2016/renju)

![Conway Life](screenshots/1.jpg)

## Установка и запуск

Для установки клонируйте репозиторий и соберите проект с помощью `stack`:

```
git clone https://github.com/cmc-haskell-2016/renju.git
cd renju
stack setup
stack build
```

После установки запуск осуществляется командой `stack exec`:

```
stack exec renju
```

Во время разработки инициировать повторную сборку проекта с последующим запуском рекомендуется
следующей командой:

```
stack build && stack exec renju
```
