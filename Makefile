##
## EPITECH PROJECT, 2023
## B-FUN-400-PAR-4-1-wolfram-joseph.yu
## File description:
## Makefile
##

NAME = Glados

all:
	 stack build
	 cp $$(stack path --local-install-root)/bin/$(NAME) .

clean:
	stack clean

fclean: clean
	rm -f $(NAME)
	stack purge

re: fclean all

.PHONY: all clean fclean re
