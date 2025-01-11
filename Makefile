##
## EPITECH PROJECT, 2025
## test_Makefile
## File description:
## Haskell test makefile
##

NAME = pushswap_checker
TEST_NAME = test

MAIN = Main
SRC = Main.hs
TEST_DIR = tests/
TEST_SRC = $(TEST_DIR)test.hs
COVERAGE_DIR = $(TEST_DIR)coverage/

GHC_FLAGS = -Wall -Wextra -main-is $(MAIN)

all: $(NAME)

$(NAME):
	@ghc $(SRC) -o $(NAME) $(GHC_FLAGS)

install: 
	stack update
	stack install HUnit 

$(TEST_NAME): install
	@ghc $(TEST_SRC) -o $(TEST_NAME) -package HUnit -fhpc

clean:
	rm -f *.hi *.o *.tix

fclean: clean
	rm -rf $(NAME) $(TEST_NAME) ./$(TEST_DIR)*.tix \
	./$(TEST_DIR)*.o ./$(TEST_DIR)*.hi \
	./$(COVERAGE_DIR)*.tix ./$(COVERAGE_DIR)*.html

re: fclean all

test: $(TEST_NAME)

tests_run: test
	./$(TEST_NAME)
	mkdir -p ./$(COVERAGE_DIR)
	mv ./$(TEST_NAME).tix ./$(COVERAGE_DIR)
	hpc report $(COVERAGE_DIR)$(TEST_NAME).tix
	hpc markup $(COVERAGE_DIR)$(TEST_NAME).tix --destdir ./$(COVERAGE_DIR)

tests_re: fclean tests_run

.PHONY: all clean fclean re test tests_run tests_re