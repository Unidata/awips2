#
# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements.  See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership.  The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License.  You may obtain a copy of the License at
# 
#   http:#www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.
#
/*MGEN:commentPrefix=#*/
/*MGEN:Root.Disclaimer*/
/*MGEN:IF(CMakeLists.QpidBroker)*/
/*MGEN:mgenDir=${mgen_dir}*/
/*MGEN:specDir=${qpid-cpp_SOURCE_DIR}/../specs*/

set(mgen_generator /*MGEN:CMakeLists.GenSources*/)

set(mgen_broker_cpp /*MGEN:CMakeLists.GenCppFiles*/)

# Header file install rules.
#/*MGEN:CMakeLists.HeaderInstalls*/
#if GENERATE
#$(srcdir)/managementgen.mk: $(mgen_generator)
#	$(mgen_cmd)
#
#$(mgen_generator):
#endif
#/*MGEN:ENDIF*/

set(qmfgen_sources /*MGEN:CMakeLists.GeneratedFiles*/)
