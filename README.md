# SGIT

Git-like CLI in scala

![travis test status](https://travis-ci.com/ThomasF34/sgit.svg?branch=master)

---

To use `sgit` please make sure you have `sbt` installed. If not, [see here](https://www.scala-sbt.org/)

## Installation

> You can find the binary file in the [Github release](https://github.com/ThomasF34/sgit/releases/tag/v1.0.0)

You can also execute the following command to build the binary file. You then have to add `target/scala-2.13` to your PATH.

```
sbt assembly
```

You're now able to use `sgit`

## Usage

- [x] help
- [x] init
- [x] status
- [x] add \<file\>
- [x] commit \[-m \<message\>\] \[-a \<author name\> \]
- [x] diff
- [x] log
- [x] log -p
- [x] log --stat
- [x] branch \[\<name\>\]
- [x] branch -av
- [x] tag \[\<name\>\]
- [x] checkout \[\<branch tag or commit\>\]
- [ ] merge :hammer: \[\<branch\>\]
- [ ] rebase
- [ ] rebase i

*NB: Merge algorithm is implemented but is not linked to Parser. Merge command will show an example on three given contents (I decided to focus on refactoring the architecture instead)*

## Test

You can run tests with the following command

```
sbt test
```

You can also see test with a coverage report with the following command (using [scoverage](https://github.com/scoverage/sbt-scoverage))

```
sbt coverage test coverageReport
```

