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
package org.apache.qpid.server.logging.actors;

import org.apache.qpid.server.logging.RootMessageLogger;
import org.apache.qpid.server.logging.subjects.QueueLogSubject;
import org.apache.qpid.server.queue.AMQQueue;

import java.text.MessageFormat;

/**
 * This Actor is used when while the queue is performing an asynchronous process
 * of its queue.
 */
public class QueueActor extends AbstractActor
{
    private QueueLogSubject _logSubject;

    /**
     * Create an QueueLogSubject that Logs in the following format.
     *
     * @param queue      The queue that this Actor is working for
     * @param rootLogger the Root logger to use.
     */
    public QueueActor(AMQQueue queue, RootMessageLogger rootLogger)
    {
        super(rootLogger);

        _logSubject = new QueueLogSubject(queue);
    }

    public String getLogMessage()
    {
        return _logSubject.toString();
    }
}
    
