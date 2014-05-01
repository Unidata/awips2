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

require 'test/unit'
require 'qpid'

class TestQueue < Test::Unit::TestCase

  # The qpid queue class just provides sime simple extensions to
  # python's standard queue data structure, so we don't need to test
  # all the queue functionality.

  def setup
    # Make sure errors in threads lead to a noisy death of the test
    Thread.abort_on_exception = true
  end

  def test_listen
    values = []
    heard = Qpid::Util::Event.new

    listener = Proc.new do |x|
      values << x
      heard.set
    end

    q = Qpid::Queue.new
    q.listen(&listener)

    heard.clear
    q.put(1)
    heard.wait
    assert_equal([1], values)
    heard.clear
    q.put(2)
    heard.wait
    assert_equal([1, 2], values)

    q.listen
    q.put(3)
    assert_equal(3, q.get)

    q.listen(&listener)
    heard.clear
    q.put(4)
    heard.wait
    assert_equal([1,2,4], values)
  end

  def test_close
    q = Qpid::Queue.new
    (1..3).each { |i| q.put(i) }
    q.close
    assert_equal(1, q.get)
    assert_equal(2, q.get)
    assert_equal(3, q.get)
    10.times do |i|
      assert_raises(Qpid::Closed) do
        q.get
      end
    end
  end

end
