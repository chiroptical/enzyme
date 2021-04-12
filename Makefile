build:
	stack build

format:
	find src/ test/ app/ -name "*.hs" -exec ormolu -i {} \;

.PHONY: build format
