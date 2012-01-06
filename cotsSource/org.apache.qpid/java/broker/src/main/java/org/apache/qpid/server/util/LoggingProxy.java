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
package org.apache.qpid.server.util;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.Arrays;

/**
 * Dynamic proxy that records invocations in a fixed size circular buffer,
 * dumping details on hitting an exception.
 * <p>
 * Useful in debugging.
 * <p>
 */
public class LoggingProxy implements InvocationHandler
{
    private final Object _target;
    private final CircularBuffer _log;

    public LoggingProxy(Object target, int size)
    {
        _target = target;
        _log = new CircularBuffer(size);
    }

    public Object invoke(Object proxy, Method method, Object[] args) throws Throwable
    {
        try
        {
            entered(method, args);
            Object result = method.invoke(_target, args);
            returned(method, result);
            return result;
        }
        catch(InvocationTargetException e)
        {
            dump();
            throw e.getTargetException();
        }
    }

    void dump()
    {
        _log.dump();
    }

    CircularBuffer getBuffer()
    {
        return _log;
    }

    private synchronized void entered(Method method, Object[] args)
    {
        if (args == null)
        {
            _log.add(Thread.currentThread() + ": " + method.getName() + "() entered");
        }
        else
        {
            _log.add(Thread.currentThread() + ": " + method.getName() + "(" + Arrays.toString(args) + ") entered");
        }
    }

    private synchronized void returned(Method method, Object result)
    {
        if (method.getReturnType() == Void.TYPE)
        {
            _log.add(Thread.currentThread() + ": " + method.getName() + "() returned");
        }
        else
        {
            _log.add(Thread.currentThread() + ": " + method.getName() + "() returned " + result);
        }
    }

    public Object getProxy(Class... c)
    {
        return Proxy.newProxyInstance(_target.getClass().getClassLoader(), c, this);
    }

    public int getBufferSize() {
        return _log.size();
    }
}
