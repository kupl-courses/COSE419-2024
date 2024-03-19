## 빌드 및 실행법
- `hw1` 프로젝트를 빌드하기 위해 `make`, `make build`, 또는 `dune build` 명령어를 사용하고,
- 빌드된 프로젝트를 실행하기 위해 `dune exec hw1` 또는 `dune exec -- ./main.exe` 명령어를 사용할 수 있습니다.

현재 프로젝트 모습에서 아무것도 추가하지 않은 채로 이 프로젝트의 `main.ml` 을 실행했다면, 아래와 같은 결과를 얻을 수 있습니다.
```
> dune exec hw1
Fatal error: exception Dune__exe__Sat.Not_implemented
```

이는 `main.ml` 실행 중 호출한 `Sat.convert` 에서 `Not_implemented` 예외를 발생시켰기 때문에 발생한 오류입니다.

### 세부사항
- `dune-project` 파일은 이 폴더 (`hw1`) 가 dune 프로젝트임을 나타냅니다.
- dune 프로젝트에서, 빌드할 OCaml 소스코드를 가진 폴더는 항상 `dune` 파일을 가지고 있어야 합니다. 이 이유로 `hw1` 폴더는 dune 파일을 가지고 있습니다.
- `dune` 파일에 쓰인 `(executable (name main) ...)` 은 이 폴더의 빌드 목표가 `main.ml` 파일을 컴파일해서 `main.exe` 바이너리를 만드는 것임을 의미합니다.
- 빌드된 바이너리는 `_build/default` 폴더 밑에 저장됩니다. 따라서 `_build/default/main.exe` 를 직접 실행시킬 수도 있습니다.
