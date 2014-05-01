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
package org.apache.qpid.server.management;

import org.apache.qpid.server.logging.actors.ManagementActor;
import org.apache.qpid.server.logging.actors.CurrentActor;
import org.apache.qpid.server.logging.LogActor;

import javax.management.ListenerNotFoundException;
import javax.management.MBeanInfo;
import javax.management.MBeanNotificationInfo;
import javax.management.NotCompliantMBeanException;
import javax.management.NotificationBroadcaster;
import javax.management.NotificationBroadcasterSupport;
import javax.management.NotificationFilter;
import javax.management.NotificationListener;

/**
 * This class provides additinal feature of Notification Broadcaster to the
 * DefaultManagedObject.
 * @author  Bhupendra Bhardwaj
 * @version 0.1
 */
public abstract class AMQManagedObject extends DefaultManagedObject
                                       implements NotificationBroadcaster
{
    /**
     * broadcaster support class
     */
    protected NotificationBroadcasterSupport _broadcaster = new NotificationBroadcasterSupport();

    /**
     * sequence number for notifications
     */
    protected long _notificationSequenceNumber = 0;

    protected MBeanInfo _mbeanInfo;

    protected LogActor _logActor;

    protected AMQManagedObject(Class<?> managementInterface, String typeName, int version)
        throws NotCompliantMBeanException
    {
        super(managementInterface, typeName, version);
        // CurrentActor will be defined as these objects are created during
        // broker startup.
        _logActor = new ManagementActor(CurrentActor.get().getRootMessageLogger());
        buildMBeanInfo();
    }

    @Override
    public MBeanInfo getMBeanInfo()
    {
        return _mbeanInfo;
    }
    
    private void buildMBeanInfo() throws NotCompliantMBeanException
    {
        _mbeanInfo = new MBeanInfo(this.getClass().getName(),
                      MBeanIntrospector.getMBeanDescription(this.getClass()),
                      MBeanIntrospector.getMBeanAttributesInfo(getManagementInterface()),
                      MBeanIntrospector.getMBeanConstructorsInfo(this.getClass()),
                      MBeanIntrospector.getMBeanOperationsInfo(getManagementInterface()),
                      this.getNotificationInfo());
    }



    // notification broadcaster implementation

    public void addNotificationListener(NotificationListener listener,
                                        NotificationFilter filter,
                                        Object handback)
    {
        _broadcaster.addNotificationListener(listener, filter, handback);
    }

    public void removeNotificationListener(NotificationListener listener)
        throws ListenerNotFoundException
    {
        _broadcaster.removeNotificationListener(listener);
    }

    public MBeanNotificationInfo[] getNotificationInfo()
    {
        return null;
    } 
}
