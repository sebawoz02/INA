# Compiler
CC ?= gcc	# gcc by default
C_STD := -std=c11
C_OPT := -O3
C_WARNS :=

C_FLAGS :=

ifeq ($(CC),clang)
	C_WARNS += -Weverything -Wno-padded
	C_FLAGS += -g
else ifneq (, $(filter $(CC), cc gcc))
	C_WARNS += -Wall -Wextra -pedantic -Wcast-align \
			   -Winit-self -Wlogical-op -Wmissing-include-dirs \
			   -Wredundant-decls -Wshadow -Wstrict-overflow=5 -Wundef  \
			   -Wwrite-strings -Wpointer-arith -Wmissing-declarations \
			   -Wuninitialized -Wold-style-definition -Wstrict-prototypes \
			   -Wmissing-prototypes -Wswitch-default -Wbad-function-cast \
			   -Wnested-externs -Wunreachable-code -Wno-sign-compare -Wno-char-subscripts
	C_FLAGS += -rdynamic
endif

C_FLAGS += $(C_STD) $(C_OPT) $(C_WARNS)

# Verbose mode / Not quiet
ifeq ("$(origin V)", "command line")
	Q :=
else
	Q ?= @
endif

# Directories
SDIR := ./src
IDIR := ./inc
ADIR := ./app

# .c Files
SRC := $(wildcard $(SDIR)/*.c)
ASRC := $(wildcard $(ADIR)/*.c)

# .o Files
LOBJ := $(SRC:%.c=%.o)
AOBJ := $(ASRC:%.c=%.o)
OBJ := $(AOBJ) $(LOBJ)

# Binary Files
EXEC := FA

# Shell commands
RM := rm -rf

# Prints
define print_cc
   $(if $(Q), @echo "[CC]        $(1)")
endef

define print_bin
   $(if $(Q), @echo "[BIN]       $(1)")
endef

define print_rm
    $(if $(Q), @echo "[RM]        $(1)")
endef

all: fa

__FORCE:

fa: $(EXEC)

$(EXEC): $(OBJ)
	$(call print_bin,$@)
	$(Q)$(CC) $(C_FLAGS) -I$(IDIR) $(OBJ) -o $@

%.o:%.c
	$(call print_cc,$<)
	$(Q)$(CC) $(C_FLAGS) -I$(IDIR) -c $< -o $@

clean:
	$(call print_rm,EXEC)
	$(Q)$(RM) $(EXEC)
	$(call print_rm,OBJ)
	$(Q)$(RM) $(OBJ)
