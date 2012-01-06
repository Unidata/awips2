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
b4 = sim.addBroker("B4")
b5 = sim.addBroker("B5")
b6 = sim.addBroker("B6")
b7 = sim.addBroker("B7")
b8 = sim.addBroker("B8")

c1 = sim.addClient("C1", b1)
c3 = sim.addClient("C3", b3)
c4 = sim.addClient("C4", b4)
c5 = sim.addClient("C5", b5)
c8 = sim.addClient("C8", b8)

sim.link(b1, b2)
sim.link(b3, b2)
sim.link(b4, b2)
sim.link(b5, b2)

sim.link(b6, b7)
sim.link(b6, b8)

sim.bind(c1, "A")
sim.bind(c3, "B")
sim.bind(c8, "A")

sim.link(b5, b6)

sim.bind(c4, "A")

sim.sendMessage("A", b1)
sim.sendMessage("A", b2)
sim.sendMessage("A", b3)
sim.sendMessage("A", b4)
sim.sendMessage("A", b5)
sim.sendMessage("A", b6)
sim.sendMessage("A", b7)
sim.sendMessage("A", b8)

sim.sendMessage("B", b1)
sim.sendMessage("B", b2)
sim.sendMessage("B", b3)
sim.sendMessage("B", b4)
sim.sendMessage("B", b5)
sim.sendMessage("B", b6)
sim.sendMessage("B", b7)
sim.sendMessage("B", b8)

sim.unbind(c1, "A")

sim.sendMessage("A", b1)
sim.sendMessage("A", b2)
sim.sendMessage("A", b3)
sim.sendMessage("A", b4)
sim.sendMessage("A", b5)
sim.sendMessage("A", b6)
sim.sendMessage("A", b7)
sim.sendMessage("A", b8)

sim.unbind(c4, "A")
sim.unbind(c3, "B")
sim.unbind(c8, "A")

sim.dumpState()
sim.end()
