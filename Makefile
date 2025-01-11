##
## EPITECH PROJECT, 2025
## test_Makefile
## File description:
## Haskell test makefile
##

NAME = pushswap_checker
TEST_NAME = test
BONUS_NAME = pushswap_checker_bonus

MAIN = Main
TEST_MAIN = Test

SRCS = 	Main.hs \
		CommandChecker.hs \
		Utilities.hs
BONUS_SRCS = bonus/Main.hs \
			bonus/CommandChecker.hs \
			bonus/Utilities.hs \
			bonus/My.hs

TEST_DIR = tests/
TEST_SRC = 	$(TEST_DIR)test.hs \
			$(TEST_DIR)test_CommandChecker.hs \
			$(TEST_DIR)test_Utilities.hs \
			$(TEST_DIR)test_Main.hs
BONUS_DIR = bonus/

COVERAGE_DIR = $(TEST_DIR)coverage/

GHC_FLAGS = -Wall -Wextra -main-is $(MAIN)
TEST_FLAGS = -package HUnit -fhpc -main-is $(TEST_MAIN)

all: $(NAME)

$(NAME):
	@ghc $(SRCS) -o $(NAME) $(GHC_FLAGS)

install:
	stack update
	stack install HUnit

$(TEST_NAME): install
	@ghc $(TEST_SRC) -o $(TEST_NAME) $(TEST_FLAGS)

clean:
	rm -f *.hi *.o *.tix

fclean: clean
	rm -rf $(NAME) $(TEST_NAME) ./$(TEST_DIR)*.tix \
	./$(TEST_DIR)*.o ./$(TEST_DIR)*.hi \
	./$(COVERAGE_DIR)*.tix ./$(COVERAGE_DIR)*.html \
	./$(BONUS_NAME) ./$(BONUS_DIR)*.tix \
	./$(BONUS_DIR)*.o ./$(BONUS_DIR)*.hi

re: fclean all

test: $(TEST_NAME)

tests_run: test
	./$(TEST_NAME)
	mkdir -p ./$(COVERAGE_DIR)
	mv ./$(TEST_NAME).tix ./$(COVERAGE_DIR)
	hpc report $(COVERAGE_DIR)$(TEST_NAME).tix
	hpc markup $(COVERAGE_DIR)$(TEST_NAME).tix --destdir ./$(COVERAGE_DIR)

tests_re: fclean tests_run

$(BONUS_NAME):
	@ghc $(BONUS_SRCS) -o $(BONUS_NAME) $(GHC_FLAGS)

Bonus: $(BONUS_NAME)

Bonus_re: fclean $(BONUS_NAME)

.PHONY: all clean fclean re test tests_run tests_re Bonus Bonus_re