package org.apache.qpid.thread;
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


import java.lang.reflect.Constructor;

public class RealtimeThreadFactory implements ThreadFactory
{
    private Class threadClass;
    private Constructor threadConstructor;
    private Constructor priorityParameterConstructor;
    private int defaultRTThreadPriority = 20;
    
    public RealtimeThreadFactory() throws Exception
    {
        defaultRTThreadPriority = Integer.getInteger("qpid.rt_thread_priority",20);
        threadClass = Class.forName("javax.realtime.RealtimeThread");
    
        Class schedulingParametersClass = Class.forName("javax.realtime.SchedulingParameters");
        Class releaseParametersClass = Class.forName("javax.realtime.ReleaseParameters");
        Class memoryParametersClass = Class.forName("javax.realtime.MemoryParameters");
        Class memoryAreaClass = Class.forName("javax.realtime.MemoryArea");
        Class processingGroupParametersClass = Class.forName("javax.realtime.ProcessingGroupParameters");
     
        Class[] paramTypes = new Class[]{schedulingParametersClass,
                                         releaseParametersClass, 
                                         memoryParametersClass,
                                         memoryAreaClass,
                                         processingGroupParametersClass,
                                         java.lang.Runnable.class};
        
        threadConstructor = threadClass.getConstructor(paramTypes);
        
        Class priorityParameterClass = Class.forName("javax.realtime.PriorityParameters");
        priorityParameterConstructor = priorityParameterClass.getConstructor(new Class[]{int.class});        
    }

    public Thread createThread(Runnable r) throws Exception
    {
        return createThread(r,defaultRTThreadPriority);
    }

    public Thread createThread(Runnable r, int priority) throws Exception
    {
        Object priorityParams = priorityParameterConstructor.newInstance(priority);
        return (Thread)threadConstructor.newInstance(priorityParams,null,null,null,null,r);
    }

}
