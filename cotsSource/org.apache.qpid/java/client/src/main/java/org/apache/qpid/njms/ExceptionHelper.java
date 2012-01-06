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
package org.apache.qpid.njms;

import org.apache.qpid.QpidException;

import javax.jms.JMSException;
import javax.transaction.xa.XAException;

/**
 * Helper class for handling exceptions
 */
public class ExceptionHelper
{
    static public JMSException convertQpidExceptionToJMSException(Exception exception)
    {
        JMSException jmsException = null;
        if (!(exception instanceof JMSException))
        {
            if (exception instanceof QpidException)
            {
                jmsException = new JMSException(exception.getMessage(), String.valueOf(((QpidException) exception).getErrorCode()));
            }
            else
            {
                jmsException = new JMSException(exception.getMessage());
            }
            jmsException.setLinkedException(exception);
            jmsException.initCause(exception);
        }
        else
        {
            jmsException = (JMSException) exception;
        }
        return jmsException;
    }

    static public XAException convertQpidExceptionToXAException(QpidException exception)
    {
        String qpidErrorCode = String.valueOf(exception.getErrorCode());
        // todo map this error to an XA code
        int xaCode = XAException.XAER_PROTO;
        return new XAException(xaCode);
    }
}
