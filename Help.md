# Workflow:
1. Ветка `master` - это основная ветка проекта. В ней все должно работать стабильно, без багов и проблем.
2. Ветка `develop` - это основная ветка разработки. Если возникает необходимость впилить новый функционал, или исправить старый от нее должна создаваться новая ветка типа `feature-<имя фичи>`, `release-<номер релиза>`.


# Впиливание нового функционала (feature)
```bash
    git checkout develop
    git checkout -b feature-GUI2.0 # это пример
    # do some staff in this branch
    git checkout develop
    git merge --no-ff feature-GUI2.0 # возвращаем ветку `feature-GUI2.0` в `develop`
```

То есть: cоздали новую ветку и перешли в нее. Наделали там коммитов с новым функционалом. Вмерджили все изменения обратно в `develop`. 


# Следущая стабильная версия (release)
```bash
    git checkout develop
    git checkout -b release-5.0

    git merge feature-newLogic # если этой ветки не было в `develop`
    # merges from other branches

    git checkout master
    git merge --no-ff release-5.0 # засунуть новый релиз в мастер
    git tag -a 5.0 # приписать комитам тег версии релиза 

    git checkout develop
    git merge --no-ff release-5.0 # обновить develop
```

То есть: если реализовано много различного функционала, но они разбросаны по веткам и не все вмерджены в develop, или пришла пора, наконец, объединить усилия команды -- мы создаем релизную ветку и вмердживаем в неё все feature-ветки. Причем, если с момента существования открытой релизной ветки создалась новая feature-ветка -- её нужно __мерджить в release-ветку!__ Когда все смерджилось -- необходимо закрыть релиз, то есть вмерджить все изменения в мастер и в develop.


# Исправления багов (fix)
* Если проблема в мастере:
```bash
    git checkout master
    git checkout -b fix-5.0.1 # последняя версия в мастере + .1

    # do some staff (исправления, коммиты)
    git checkout master
    git merge --no-ff fix-5.0.1
    git tag -a 5.0.1

    # обновить develop
    git checkout deveop
    git merge --no-ff fix-5.0.1
```

* Если проблема в открытом релизе:
```bash
    git checkout release-6.0
    # исправить проблему. закоммитить проблему
```

* Если одна и та же проблема в различных feature-ветках (отнаследована от develop):
```bash
    git checkout develop
    # исправить проблему в develop

    # во все проблемные feature-ветки вмерджить исправленный develop
    git checkout feature-Network
    git merge --no-ff develop
```

* Если проблема только в develop:
```bash
    git checkout develop
    # исправить проблему. закоммитить проблему
```


## Основная идея проста.
Если удается разбить весь проект на список задач, каждой из них выставляется приоритет, распределяется по членам команды и на каждую создается своя `feature`-ветка.

Как только решение отдельной задачи закончено, все изменения вмердживаются в `release`-ветку, если она есть, или в `develop`.

Если законченых фич больше одной -- следует создать новую `release`-ветку, чтобы ничего не потерять при merge-кофликтах.

Если какие-то странные баги появляются в вашей ветке -- желательно посмотреть, есть ли они в `develop`. Если да -- лучше исправить их там, и вмерджить изменения к себе. Иначе -- вы сами создали себе проблему, отныне она - ваша проблема.

Проблемы в `master` и `develop` -- самые приоритетные, потому что из-за них невозможно будет впиливать новый функционал.
