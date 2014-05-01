# setup the BUILD_DIR

init:
ifndef BUILD_DIR
	# BUILD_DIR is not defined
	exit 1
endif	

all: init $(BUILD_DIR)

$(BUILD_DIR):
	mkdir $(BUILD_DIR)
	mkdir $(BUILD_DIR)/bin #location for all executable binaries
	mkdir $(BUILD_DIR)/lib #location for all libraries
	
clean: init
	rm -fr $(BUILD_DIR)

.PHONY: init, all, clean