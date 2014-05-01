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
package org.apache.qpid.server.logging.subjects;

import org.apache.qpid.server.AMQChannel;

/**
 * Validate ChannelLogSubjects are logged as expected
 */
public class ChannelLogSubjectTest extends ConnectionLogSubjectTest
{
    private final int _channelID = 1;

    @Override
    public void setUp() throws Exception
    {
        super.setUp();

        AMQChannel channel = new AMQChannel(_session, _channelID, _session.getVirtualHost().getMessageStore());

        _subject = new ChannelLogSubject(channel);
    }

    /**
     * MESSAGE [Blank][con:0(MockProtocolSessionUser@null/test)/ch:1] <Log Message>
     *
     * @param message the message whos format needs validation
     */
    protected void validateLogStatement(String message)
    {
        // Use the ConnectionLogSubjectTest to vaildate that the connection
        // section is ok
        super.validateLogStatement(message);

        // Finally check that the channel identifier is correctly added
        assertTrue("Channel 1 identifier not found as part of Subject",
                   message.contains(")/ch:1]"));
    }

}
