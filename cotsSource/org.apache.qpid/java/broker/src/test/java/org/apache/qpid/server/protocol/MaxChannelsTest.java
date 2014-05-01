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
package org.apache.qpid.server.protocol;

import junit.framework.TestCase;
import org.apache.qpid.AMQException;
import org.apache.qpid.protocol.AMQConstant;
import org.apache.qpid.server.AMQChannel;
import org.apache.qpid.server.virtualhost.VirtualHost;
import org.apache.qpid.server.registry.ApplicationRegistry;

/** Test class to test MBean operations for AMQMinaProtocolSession. */
public class MaxChannelsTest extends TestCase
{
	private AMQProtocolEngine _session;

    public void testChannels() throws Exception
    {
        VirtualHost vhost = ApplicationRegistry.getInstance().getVirtualHostRegistry().getVirtualHost("test");
        _session = new InternalTestProtocolSession(vhost);

        // check the channel count is correct
        int channelCount = _session.getChannels().size();
        assertEquals("Initial channel count wrong", 0, channelCount);

        long maxChannels = 10L;
        _session.setMaximumNumberOfChannels(maxChannels);
        assertEquals("Number of channels not correctly set.", new Long(maxChannels), _session.getMaximumNumberOfChannels());


        try
        {
            for (long currentChannel = 0L; currentChannel < maxChannels; currentChannel++)
            {
                _session.addChannel(new AMQChannel(_session, (int) currentChannel, null));
            }
        }
        catch (AMQException e)
        {
            assertEquals("Wrong exception recevied.", e.getErrorCode(), AMQConstant.NOT_ALLOWED);
        }
        assertEquals("Maximum number of channels not set.", new Long(maxChannels), new Long(_session.getChannels().size()));
    }

    @Override
    public void setUp()
    {
        //Highlight that this test will cause a new AR to be created
        ApplicationRegistry.getInstance();
    }

    @Override
    public void tearDown() throws Exception
    {
    	try {
			_session.closeSession();
		} catch (AMQException e) {
			// Yikes
			fail(e.getMessage());
		}
        finally
        {
            // Correctly Close the AR we created
            ApplicationRegistry.remove();
        }
    }

}
