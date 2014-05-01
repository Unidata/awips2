#!/usr/bin/ruby
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

require "qpid"

s = Qpid::Qmf::Session.new()
b = s.add_broker("amqp://localhost:5672")

while true:
    nodes = s.objects(:class => "node")
  nodes.each do |node|
    puts "node: #{node.hostname}"
    for (key, val) in node.properties
      puts "  property: #{key}, #{val}"
    end

    # Find any domains that on the current node.
    domains = s.objects(:class => "domain", 'node' => node.object_id)
    domains.each do |domain|
      r = domain.getXMLDesc()
      puts "status: #{r.status}"
      if r.status == 0
        puts "xml description: #{r.description}"
        puts "length: #{r.description.length}"
      end

      puts "  domain: #{domain.name}, state: #{domain.state}, id: #{domain.id}"
      for (key, val) in domain.properties
        puts "    property: #{key}, #{val}"
      end
    end

    pools = s.objects(:class => "pool", 'node' => node.object_id)
    pools.each do |pool|
      puts "  pool: #{pool.name}"
      for (key, val) in pool.properties
        puts "    property: #{key}, #{val}"
      end

      r = pool.getXMLDesc()
      puts "status: #{r.status}"
      puts "text: #{r.text}"
      if r.status == 0
        puts "xml description: #{r.description}"
        puts "length: #{r.description.length}"
      end

      # Find volumes that are part of the pool.
      volumes = s.objects(:class => "volume", 'pool' => pool.object_id)
      volumes.each do |volume|
        puts "    volume: #{volume.name}"
        for (key, val) in volume.properties
          puts "      property: #{key}, #{val}"
        end
      end
    end

  end

  puts '----------------------------'
  sleep(5)

end
