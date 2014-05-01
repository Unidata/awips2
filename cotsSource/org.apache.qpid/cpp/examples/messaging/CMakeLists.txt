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

# drain and spout have explicit Boost.program_options usage in them, so be
# sure that lib is linked in.
add_example(messaging drain ${Boost_PROGRAM_OPTIONS_LIBRARY})
add_example(messaging spout ${Boost_PROGRAM_OPTIONS_LIBRARY})

add_example(messaging queue_receiver)
add_example(messaging queue_sender)

add_example(messaging topic_receiver)
add_example(messaging topic_sender)

add_example(messaging map_receiver)
add_example(messaging map_sender)

add_example(messaging client)
add_example(messaging server)
