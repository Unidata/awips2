/*
 *  Licensed to the Apache Software Foundation (ASF) under one
 *  or more contributor license agreements.  See the NOTICE file
 *  distributed with this work for additional information
 *  regarding copyright ownership.  The ASF licenses this file
 *  to you under the Apache License, Version 2.0 (the
 *  "License"); you may not use this file except in compliance
 *  with the License.  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing,
 *  software distributed under the License is distributed on an
 *  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 *  KIND, either express or implied.  See the License for the
 *  specific language governing permissions and limitations
 *  under the License.
 *
 *
 */
package org.apache.qpid.util;

import java.util.Queue;

/**
 * Defines a queue that has a push operation to add an element to the head of the queue.
 *
 * @todo Seems like this may be pointless, the implementation uses this method to increment the message count
 *       then calls offer. Why not simply override offer and drop this interface?
 */
public interface MessageQueue<E> extends Queue<E>
{
    /**
     * Inserts the specified element into this queue, if possible. When using queues that may impose insertion
     * restrictions (for example capacity bounds), method offer is generally preferable to method Collection.add(E),
     * which can fail to insert an element only by throwing an exception.
     *
     * @param o The element to insert.
     *
     * @return <tt>true</tt> if it was possible to add the element to this queue, else <tt>false</tt>
     */
    boolean pushHead(E o);
}
