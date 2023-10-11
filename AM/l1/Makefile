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
Z1_ADIR := ./app_z1
Z2_ADIR := ./app_z2

# .c Files
SRC := $(wildcard $(SDIR)/*.c)
Z1_ASRC := $(wildcard $(Z1_ADIR)/*.c)
Z2_ASRC := $(wildcard $(Z2_ADIR)/*.c)

# .o Files
LOBJ := $(SRC:%.c=%.o)
Z1_AOBJ := $(Z1_ASRC:%.c=%.o)
Z2_AOBJ := $(Z2_ASRC:%.c=%.o)
Z1_OBJ := $(Z1_AOBJ) $(LOBJ)
Z2_OBJ := $(Z2_AOBJ) $(LOBJ)

# Binary Files
Z1_EXEC := z1.out
Z2_EXEC := z2.out

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

all: z1 z2

__FORCE:

z2: $(Z2_EXEC)

z1: $(Z1_EXEC)

$(Z1_EXEC): $(Z1_OBJ)
	$(call print_bin,$@)
	$(Q)$(CC) $(C_FLAGS) -I$(IDIR) $(Z1_OBJ) -o $@

$(Z2_EXEC): $(Z2_OBJ)
	$(call print_bin,$@)
	$(Q)$(CC) $(C_FLAGS) -I$(IDIR) $(Z2_OBJ) -o $@

%.o:%.c
	$(call print_cc,$<)
	$(Q)$(CC) $(C_FLAGS) -I$(IDIR) -c $< -o $@

clean:
	$(call print_rm,Z1_EXEC)
	$(Q)$(RM) $(Z1_EXEC)
	$(call print_rm,Z2_EXEC)
	$(Q)$(RM) $(Z2_EXEC)
	$(call print_rm,Z1_OBJ)
	$(Q)$(RM) $(Z1_OBJ)
	$(call print_rm,Z2_OBJ)
	$(Q)$(RM) $(Z2_OBJ)
