##
## EPITECH PROJECT, 2023
## B-FUN-400-PAR-4-1-wolfram-joseph.yu
## File description:
## Makefile
##

NAME = glados

all:
	 stack build
	 cp $$(stack path --local-install-root)/bin/$(NAME) .

clean:
	stack clean --full

artifact: all
	tar -czvf glados.tar.gz $$(stack path --local-install-root)

fclean: clean
	$(RM) -f $(NAME)
	stack purge
	$(RM) -rf .hpc
	$(RM) -rf $(NAME).cabal
	$(RM) -rf $(NAME)-test.tix

unit_tests: re
	stack test --coverage


coverage: unit_tests
  stack hpc report --all

re: fclean all

.PHONY: all clean fclean unit_tests coverage re
