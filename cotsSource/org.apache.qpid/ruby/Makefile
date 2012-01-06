#
# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements.  See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership.  The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License.  You may obtain a copy of the License at
# 
#   http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.
#

SASL_DIR    = ext/sasl
SASL_MODULE = $(SASL_DIR)/sasl.so
RUBY_LIB    = lib
SPEC_CACHE_SCRIPT = sc.rb

.PHONY: spec_cache all clean distclean

all : build

$(SASL_MODULE) : $(SASL_DIR)/sasl.c
	cd $(SASL_DIR);	ruby extconf.rb
	$(MAKE) -C $(SASL_DIR)

spec_cache :
	echo "require 'qpid'" > $(SPEC_CACHE_SCRIPT)
	echo "Qpid::Spec010::load()" >> $(SPEC_CACHE_SCRIPT)
	ruby -I $(RUBY_LIB) -I $(SASL_DIR) $(SPEC_CACHE_SCRIPT)
	rm $(SPEC_CACHE_SCRIPT)

build: $(SASL_MODULE) spec_cache

clean:
	cd $(SASL_DIR); make clean

distclean:
	cd $(SASL_DIR); make distclean
	rm -rf $(RUBY_LIB)/qpid/spec_cache

