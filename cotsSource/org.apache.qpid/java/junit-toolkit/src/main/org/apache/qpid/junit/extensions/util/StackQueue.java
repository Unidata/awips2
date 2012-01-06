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
package org.apache.qpid.junit.extensions.util;

import java.util.EmptyStackException;
import java.util.NoSuchElementException;
import java.util.Queue;
import java.util.Stack;

/**
 * The Stack class in java.util (most unhelpfully) does not implement the Queue interface. This is an adaption of that
 * class as a queue.
 *
 * <p><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Turn a stack into a queue.
 * </table>
 *
 * @todo Need to override the add method, and iterator and consider other methods too. They work like LIFO but
 *       really wany FIFO behaviour accross the whole data structure.
 *
 * @author Rupert Smith
 */
public class StackQueue<E> extends Stack<E> implements Queue<E>
{
    /**
     * Retrieves, but does not remove, the head of this queue.
     *
     * @return The element at the top of the stack.
     */
    public E element()
    {
        try
        {
            return super.peek();
        }
        catch (EmptyStackException e)
        {
            NoSuchElementException t = new NoSuchElementException();
            t.initCause(e);
            throw t;
        }
    }

    /**
     * Inserts the specified element into this queue, if possible.
     *
     * @param o The data element to push onto the stack.
     *
     * @return True if it was added to the stack, false otherwise (this implementation always returns true).
     */
    public boolean offer(E o)
    {
        push(o);

        return true;
    }

    /**
     * Retrieves, but does not remove, the head of this queue, returning null if this queue is empty.
     *
     * @return The top element from the stack, or null if the stack is empty.
     */
    public E peek()
    {
        try
        {
            return super.peek();
        }
        catch (EmptyStackException e)
        {
            return null;
        }
    }

    /**
     * Retrieves and removes the head of this queue, or null if this queue is empty.
     *
     * @return The top element from the stack, or null if the stack is empty.
     */
    public E poll()
    {
        try
        {
            return super.pop();
        }
        catch (EmptyStackException e)
        {
            return null;
        }
    }

    /**
     * Retrieves and removes the head of this queue.
     *
     * @return The top element from the stack, or null if the stack is empty.
     *
     * @throws NoSuchElementException If the stack is empty so no element can be removed from it.
     */
    public E remove()
    {
        try
        {
            return super.pop();
        }
        catch (EmptyStackException e)
        {
            NoSuchElementException t = new NoSuchElementException();
            t.initCause(e);
            throw t;
        }
    }
}
