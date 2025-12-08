clean:
	clear
	@Rscript -e 'devtools::clean_dll()'

test:
	clear
	@echo "Testing R code"
	@Rscript -e 'devtools::document()'
	@Rscript -e 'devtools::load_all(); devtools::test()'

check-local:
	clear
	@echo "Local"
	@Rscript -e 'devtools::check()'

check-ci:
	@echo "RHub"
	@Rscript -e 'devtools::check_rhub()'
	@echo "Win Builder"
	@Rscript -e 'devtools::check_win_release()'
	@Rscript -e 'devtools::check_win_devel()'

install:
	clear
	@Rscript -e 'devtools::install(upgrade = "never")'

clang_format=`which clang-format-18`

format: $(shell find . -name '*.h') $(shell find . -name '*.hpp') $(shell find . -name '*.cpp')
	@${clang_format} -i $?
