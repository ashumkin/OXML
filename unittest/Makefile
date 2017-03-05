CC:=lazbuild
TARGET:=$(shell fpc -iTO)
CPU:=$(shell fpc -iTP)
OUTPUT_DIR ?= ../bin
export OUTPUT_DIR

UNITS=units/$(CPU)-$(TARGET)
CFLAGS +=--build-mode=$(BUILD_MODE)
CFLAGS +=--cpu=$(CPU)

PROJECT=Lazarus/OXmlUTest_Laz
MKDIR_P = mkdir -p

ifeq ($(TARGET),linux)
	SHARED_LIB_EXT=
else
	SHARED_LIB_EXT=.exe
endif
PROJECT_SUFFIX=$(CPU)
OUTPUT_NAME=$(PROJECT_PREFIX)$(PROJECT)$(SHARED_LIB_EXT)

all: compile

compile: directories
	$(CC) $(CFLAGS) $(PROJECT).lpi

clean:
	rm -f "$(OUTPUT_DIR)/$(OUTPUT_NAME)" "$(UNITS)"/*

.PHONY: directories

directories: $(OUTPUT_DIR) units

$(OUTPUT_DIR):
	$(MKDIR_P) $(OUTPUT_DIR)

units:
	$(MKDIR_P) $(UNITS)
