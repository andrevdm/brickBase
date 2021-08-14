package = brickBedrock
stack_yaml = STACK_YAML="stack.yaml"
stack = $(stack_yaml) stack

all: build test lint

setup:
	$(stack) setup
	$(stack) build --dependencies-only --test --no-run-tests
	$(stack) install hlint weeder

lint:
	hlint .
	weeder .

check-nightly:
	$(stack) setup --resolver nightly
	$(stack) build --resolver nightly --pedantic --test

build:
	$(stack) build $(package) --no-run-tests --ghc-options "-j6 +RTS -A128m -n2m -qg -RTS"

build-fast:
	$(stack) build $(package) --fast --no-run-tests --ghc-options "-j6 +RTS -A128m -n2m -qg -RTS"

build-watch:
	$(stack) build $(package) --fast --file-watch --no-run-tests --ghc-options "-j6 +RTS -A128m -n2m -qg -RTS"

test-watch:
	$(stack) test $(package) --fast --file-watch --ghc-options "-j6 +RTS -A128m -n2m -qg -RTS"

build-dirty:
	$(stack) build --ghc-options=-fforce-recomp $(package)

build-profile:
	$(stack) --work-dir .stack-work-profiling --profile build

run:
	$(stack) build $(package) --fast --no-run-tests --ghc-options "-j6 +RTS -A128m -n2m -qg -RTS" && $(stack) exec -- $(package)-exe

ghci:
	$(stack) ghci $(package):lib --ghci-options='-j8 +RTS -A128m -n2m -qg'

test:
	$(stack) test $(package) --fast --ghc-options "-j6 +RTS -A128m -n2m -qg -RTS"

test-ghci:
	$(stack) ghci $(package):test:$(package)-tests --ghci-options='-j8 +RTS -A128m -n2m -qg'

bench:
	$(stack) bench $(package)

ghcid:
	$(stack) exec -- ghcid --lint -c "stack ghci $(package):lib --ghci-options='-fobject-code -fno-warn-unused-do-bind -j6 +RTS -A128m -n2m -qg' --main-is $(package):exe:$(package)-exe"

ghcid-quiet:
	$(stack) exec -- ghcid -c "stack ghci $(package):lib --ghci-options='-fobject-code -fno-warn-unused-do-bind -fno-warn-unused-matches -fno-warn-unused-local-binds -fno-warn-unused-imports -j6 +RTS -A128m -n2m -qg' --main-is $(package):exe:$(package)-exe"

ghcid-run:
	$(stack) exec -- ghcid -c "stack ghci $(package):lib --ghci-options='-fobject-code -fno-warn-unused-do-bind -j6 +RTS -A128m -n2m -qg' --main-is $(package):exe:$(package)-exe" --test=":main debug" -W

ghcid-test:
	$(stack) exec -- ghcid -c "stack ghci $(package):lib --ghci-options='-fobject-code -fno-warn-unused-do-bind -j6 +RTS -A128m -n2m -qg' $(package):test:$(package)-test" --test="main" -W

dev-deps:
	stack install ghcid


.PHONY : build build-dirty run install ghci test test-ghci ghcid dev-deps lint check-nightly setup
