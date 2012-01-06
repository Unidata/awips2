# 
#  Licensed to the Apache Software Foundation (ASF) under one
#  or more contributor license agreements.  See the NOTICE file
#  distributed with this work for additional information
#  regarding copyright ownership.  The ASF licenses this file
#  to you under the Apache License, Version 2.0 (the
#  "License"); you may not use this file except in compliance
#  with the License.  You may obtain a copy of the License at
#  
#    http://www.apache.org/licenses/LICENSE-2.0
#  
#  Unless required by applicable law or agreed to in writing,
#  software distributed under the License is distributed on an
#  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
#  KIND, either express or implied.  See the License for the
#  specific language governing permissions and limitations
#  under the License.
# 

SRC_DIR     = .
QPID_DIR    = ../../..
SCHEMA_FILE = $(SRC_DIR)/schema.xml
GEN_DIR     = $(SRC_DIR)/gen
OUT_FILE    = $(SRC_DIR)/qmf-agent

CC           = gcc
LIB_DIR      = $(QPID_DIR)/cpp/src/.libs
CC_INCLUDES  = -I$(SRC_DIR) -I$(QPID_DIR)/cpp/include -I$(GEN_DIR)
CC_FLAGS     = -g -O3
LD_FLAGS     = -lqmf -L$(LIB_DIR)
SPEC_DIR     = $(QPID_DIR)/specs
MGEN_DIR     = $(QPID_DIR)/cpp/managementgen
MGEN         = $(MGEN_DIR)/qmf-gen

vpath %.cpp $(SRC_DIR):$(GEN_DIR)
vpath %.d   $(OBJ_DIR)
vpath %.o   $(OBJ_DIR)

cpps    = $(wildcard $(SRC_DIR)/*.cpp)
cpps   += $(wildcard $(GEN_DIR)/qmf/org/apache/qpid/agent/example/*.cpp)
deps    = $(addsuffix .d, $(basename $(cpps)))
objects = $(addsuffix .o, $(basename $(cpps)))

.PHONY: all clean gen

#==========================================================
# Pass 0: generate source files from schema
ifeq ($(MAKELEVEL), 0)

all: gen
	@$(MAKE)

gen:
	$(MGEN) -o $(GEN_DIR)/qmf $(SCHEMA_FILE)

clean:
	rm -rf $(GEN_DIR) $(OUT_FILE) *.d *.o


#==========================================================
# Pass 1: generate dependencies
else ifeq ($(MAKELEVEL), 1)

all: $(deps)
	@$(MAKE)

%.d : %.cpp
	$(CC) -M $(CC_FLAGS) $(CC_INCLUDES) $< > $@


#==========================================================
# Pass 2: build project
else ifeq ($(MAKELEVEL), 2)

$(OUT_FILE) : $(objects)
	$(CC) -o $(OUT_FILE) $(CC_FLAGS) $(LD_FLAGS) $(objects)

include $(deps)

%.o : %.cpp
	$(CC) -c $(CC_FLAGS) $(CC_INCLUDES) -o $@ $<

endif


