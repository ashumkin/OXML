CC:=lazbuild
TARGET ?= $(shell fpc -iTO)
CPU ?= $(shell fpc -iTP)

OUTPUT_DIR ?= $(PWD)/bin
export OUTPUT_DIR

UNITS=units/$(CPU)-$(TARGET)
CFLAGS +=--build-all
CFLAGS +=--build-mode=$(BUILD_MODE)
CFLAGS +=--cpu=$(CPU)

PROJECT=demos/Lazarus/OXmlTest
MKDIR_P = mkdir -p

ifeq ($(TARGET),linux)
	SHARED_LIB_EXT=
else
	SHARED_LIB_EXT=.exe
endif
ifndef RPM_VERBOSE
	RPM_VERBOSE := --quiet
endif
ifdef BUILD_MODE
PROJECT_SUFFIX ?= .$(CPU)
endif
OUTPUT_NAME = $(PROJECT)$(PROJECT_SUFFIX)$(SHARED_LIB_EXT)

compile: $(OUTPUT_DIR) units $(OUTPUT_DIR)/$(OUTPUT_NAME)

$(OUTPUT_DIR)/$(OUTPUT_NAME): $(FILE_VERSION_INC)
	$(CC) $(CFLAGS) $(PROJECT).lpi

$(OUTPUT_DIR):
	$(MKDIR_P) $(OUTPUT_DIR)

units:
	$(MKDIR_P) $(UNITS)

test:
	make -C unittest
