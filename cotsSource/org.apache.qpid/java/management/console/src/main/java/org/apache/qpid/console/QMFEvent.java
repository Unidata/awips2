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
package org.apache.qpid.console;

import java.util.HashMap;

import org.apache.qpid.transport.codec.*;

public class QMFEvent
{
    private java.util.HashMap<String, Object> arguments;
    private ClassKey classKey;
    private Session session;
    private EventSeverity severity;
    // FIXME time?
    private long timestamp;

    public QMFEvent(Session session, Decoder dec)
    {
        setSession(session);
        setClassKey(new ClassKey(dec));
        setTimestamp(dec.readInt64());
        setSeverity(EventSeverity.values()[dec.readUint8()]);
        SchemaClass sClass = getSession().getSchema(getClassKey());
        setArguments(new java.util.HashMap<String, Object>());
        if (sClass != null)
        {
            for (SchemaArgument arg : sClass.arguments)
            {
                getArguments().put(arg.getName(),
                        getSession().decodeValue(dec, arg.getType()));
            }
        }
    }

    public final Object GetArgument(String argName)
    {
        return getArguments().get(argName);
    }

    public final HashMap<String, Object> getArguments()
    {
        return arguments;
    }

    public final ClassKey getClassKey()
    {
        return classKey;
    }

    public final Session getSession()
    {
        return session;
    }

    public final EventSeverity getSeverity()
    {
        return severity;
    }

    public final long getTimestamp()
    {
        return timestamp;
    }

    public final void setArguments(java.util.HashMap<String, Object> value)
    {
        arguments = value;
    }

    public final void setClassKey(ClassKey value)
    {
        classKey = value;
    }

    public final void setSession(Session value)
    {
        session = value;
    }

    public final void setSeverity(EventSeverity value)
    {
        severity = value;
    }

    public final void setTimestamp(long value)
    {
        timestamp = value;
    }
}