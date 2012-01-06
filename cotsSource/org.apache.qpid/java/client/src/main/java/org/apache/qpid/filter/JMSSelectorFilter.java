/* Licensed to the Apache Software Foundation (ASF) under one
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
 */
package org.apache.qpid.filter;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.apache.qpid.QpidException;
import org.apache.qpid.filter.selector.SelectorParser;
import org.apache.qpid.client.message.AbstractJMSMessage;


public class JMSSelectorFilter implements MessageFilter
{
    /**
     * this JMSSelectorFilter's logger
     */
    private static final Logger _logger = LoggerFactory.getLogger(JMSSelectorFilter.class);

    private String _selector;
    private BooleanExpression _matcher;

    public JMSSelectorFilter(String selector) throws QpidException
    {
        _selector = selector;
        if (JMSSelectorFilter._logger.isDebugEnabled())
        {
            JMSSelectorFilter._logger.debug("Created JMSSelectorFilter with selector:" + _selector);
        }
        _matcher = new SelectorParser().parse(selector);
    }

    public boolean matches(AbstractJMSMessage message)
    {
        try
        {
            boolean match = _matcher.matches(message);
            if (JMSSelectorFilter._logger.isDebugEnabled())
            {
                JMSSelectorFilter._logger.debug(message + " match(" + match + ") selector(" + System
                        .identityHashCode(_selector) + "):" + _selector);
            }
            return match;
        }
        catch (QpidException e)
        {
            JMSSelectorFilter._logger.warn("Caght exception when evaluating message selector for message  " + message, e);
        }
        return false;
    }

    public String getSelector()
    {
        return _selector;
    }
}
