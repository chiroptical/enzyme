build:
	stack build

run:
	stack run

format:
	find src/ test/ app/ -name "*.hs" -exec ormolu -i {} \;

repl:
	stack repl

ghcid:
	ghcid -c stack repl

.PHONY: build run format repl ghcid
