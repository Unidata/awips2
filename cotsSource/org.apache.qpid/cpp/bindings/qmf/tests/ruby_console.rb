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

require 'qmf'
require 'socket'

class App < Qmf::ConsoleHandler

  def agent_added(agent)
    puts "AgentAdded: label=#{agent.label} key=#{agent.key}"
  end

  def agent_deleted(agent)
    puts "AgentDeleted: #{agent.label}"
  end

  def new_package(package)
    puts "NewPackage: #{package}"
  end

  def new_class(class_key)
    puts "NewClass: #{class_key}"
  end

  def object_update(object, hasProps, hasStats)
    puts "ObjectUpdate: #{object.object_class.class_name} props=#{hasProps} stats=#{hasStats}"
    puts "      agent-key=#{object.object_id.agent_key}"
    puts "        package=#{object.object_class.package_name}"
  end

  def event_received(event); end

  def agent_heartbeat(agent, timestamp)
    puts "AgentHeartbeat: #{agent.label} time=#{timestamp/1000000000}"
  end

  def method_response(resp); end
  def broker_info(broker); end


  def dump_schema
    packages = @qmfc.packages
    puts "----- Packages -----"
    packages.each do |p|
      puts p
      puts "    ----- Object Classes -----"
      classes = @qmfc.classes(p)
      classes.each do |c|
        puts "    #{c.name}"

        puts "        ---- Properties ----"
        props = c.properties
        props.each do |prop|
          puts "        #{prop.name}"
        end

        puts "        ---- Statistics ----"
        stats = c.statistics
        stats.each do |stat|
          puts "        #{stat.name}"
        end

        puts "        ---- Methods ----"
        methods = c.methods
        methods.each do |method|
          puts "        #{method.name}"
          puts "            ---- Args ----"
          args = method.arguments
          args.each do |arg|
            puts "            #{arg.name}"
          end
        end
      end

      puts "    ----- Event Classes -----"
      classes = @qmfc.classes(p, Qmf::CLASS_EVENT)
      classes.each do |c|
        puts "    #{c.name}"
        puts "        ---- Args ----"
        args = c.arguments
        args.each do |arg|
          puts "        #{arg.name}"
        end
      end
    end
    puts "-----"
  end

  def main
    @settings = Qmf::ConnectionSettings.new
    @settings.host = ARGV[0] if ARGV.size > 0
    @settings.port = ARGV[1].to_i if ARGV.size > 1
    @connection = Qmf::Connection.new(@settings)
    @qmfc = Qmf::Console.new(self)

    @broker = @qmfc.add_connection(@connection)
    @broker.wait_for_stable

    ##dump_schema

    agents = @qmfc.agents()
    puts "---- Agents ----"
    agents.each do |a|
      puts "  => #{a.label}"
    end
    puts "----"

    for idx in 0...20
      blist = @qmfc.objects(Qmf::Query.new(:class => "broker"))
      puts "---- Brokers ----"
      blist.each do |b|
        puts "    ---- Broker ----"
        puts "    systemRef: #{b.systemRef}"
        puts "    port     : #{b.port}"
        puts "    uptime   : #{b.uptime / 1000000000}"
        puts "  properties : #{b.properties}"
        puts "  statistics : #{b.statistics}"

        for rep in 0...1
          puts "    Pinging..."
          ret = b.echo(45, 'text string')
          puts "        status=#{ret.status} text=#{ret.exception.asString} seq=#{ret.args.sequence} body=#{ret.args.body}"
        end
      end
      puts "----"

      elist = @qmfc.objects(:package => "org.apache.qpid.broker", :class => "exchange", 'durable' => true)
      puts "---- Durable Exchanges ----"
      elist.each do |e|
        puts "Exchange: #{e.name}"
      end
      puts "----"

      qlist = @qmfc.objects(Qmf::Query.new(:package => "org.apache.qpid.broker",
                                               :class => "queue"))
      puts "---- Queues ----"
      qlist.each do |q|
        puts "    ---- Queue ----"
        puts "    name     : #{q.name}"
      end
      puts "----"
      sleep(5)
    end

    sleep(5)
    puts "Deleting connection..."
    @qmfc.del_connection(@broker)
    puts "    done"
    sleep
  end
end

app = App.new
app.main


