/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *
 */
package org.apache.qpid;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.apache.qpid.commands.*;
import org.apache.qpid.commands.objects.*;
import org.apache.qpid.utils.TestCommandLineOption;
import org.apache.qpid.utils.TestCommandLineOptionParser;
import org.apache.qpid.utils.TestJMXConfiguration;

@RunWith(Suite.class)
@Suite.SuiteClasses( { TestCommand.class, TestCommandExecutionEngine.class, TestCommandLineOption.class,
        TestCommandLineOptionParser.class, TestConnector.class, TestJMXConfiguration.class, TestAllObject.class,
        TestConnectionObject.class, TestExchangeObject.class, TestQueueObject.class, TestVirtualHostObject.class,
        TestUserManagementObject.class, TestCommanddelete.class, TestCommandlist.class, TestCommandinfo.class,
        TestCommandmove.class, TestCommandview.class, TestCommandviewcontent.class, TestCommandLineInterpreter.class

})
public class AllTest
{
}
