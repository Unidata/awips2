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

from fedsim import Sim

sim = Sim()
b1 = sim.addBroker("B1")
b2 = sim.addBroker("B2")
b3 = sim.addBroker("B3")

sim.link(b1, b2, False)
sim.link(b2, b3, False)
sim.link(b3, b1, False)

c1 = sim.addClient("C1", b1)
c2 = sim.addClient("C2", b2)
c3 = sim.addClient("C3", b3)

sim.bind(c1, "A")
sim.bind(c2, "A")

sim.sendMessage("A", b1)
sim.sendMessage("A", b2)
sim.sendMessage("A", b3)

sim.unbind(c2, "A")

sim.sendMessage("A", b1)
sim.sendMessage("A", b2)
sim.sendMessage("A", b3)

sim.end()
