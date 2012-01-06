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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map.Entry;

import org.apache.qpid.transport.codec.Decoder;
import org.apache.qpid.transport.codec.Encoder;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import edu.emory.mathcs.backport.java.util.Arrays;

public class QMFObject
{
    private static Logger log = LoggerFactory.getLogger(QMFObject.class);
    protected SchemaClass schema;
    private java.util.Date createTime;
    private java.util.Date currentTime;
    private java.util.Date deleteTime;
    private ObjectID objectID;
    private Session session;
    private boolean managed;
    public java.util.HashMap<String, Object> properties = new java.util.HashMap<String, Object>();
    public java.util.HashMap<String, Object> statistics = new java.util.HashMap<String, Object>();

    // This constructor is the "naked" constructor which creates
    // an object without a session or a schema. It is used by
    // subclasses which are auto generated
    public QMFObject()
    {
    }

    public QMFObject(QMFObject source)
    {
        this.setSession(source.getSession());
        this.setSchema(source.getSchema());
        this.managed = source.managed;
        this.setCurrentTime(source.getCurrentTime());
        this.setCreateTime(source.getCreateTime());
        this.setDeleteTime(source.getDeleteTime());
        this.setObjectID(source.getObjectID());
        this.properties = source.properties;
        this.statistics = source.statistics;
    }

    // This constructor is used by a session make object call to
    // create a blank object from a schema.
    public QMFObject(Session session, SchemaClass schema,
            boolean hasProperties, boolean hasStats, boolean isManaged)
    {
        setSession(session);
        setSchema(schema);
        managed = isManaged;
        if (hasProperties)
        {
            for (SchemaProperty prop : getSchema().getAllProperties())
            {
                Object propValue = null;
                if (!prop.getOptional())
                {
                    propValue = Util.defaultValue(prop.getType());
                }
                this.setProperty(prop.getName(), propValue);
            }
        }
        if (hasStats)
        {
            for (SchemaStatistic stat : getSchema().statistics)
            {
                setStatistic(stat.getName(), Util.defaultValue(stat.getType()));
            }
        }
    }

    // This constructor is used by the session to create an object based on a
    // data
    // stream by the agent.
    public QMFObject(Session session, SchemaClass schema, Decoder dec,
            boolean hasProperties, boolean hasStats, boolean isManaged)
    {
        setSession(session);
        setSchema(schema);
        managed = isManaged;
        if (managed)
        {
            // FIXME DateTime or Uint64??
            setCurrentTime(new java.util.Date(dec.readDatetime()));
            setCreateTime(new java.util.Date(dec.readDatetime()));
            setDeleteTime(new java.util.Date(dec.readDatetime()));
            setObjectID(new ObjectID(dec));
        }
        if (hasProperties)
        {
            java.util.ArrayList<String> excluded = processPresenceMasks(dec,
                    getSchema());
            for (SchemaProperty prop : getSchema().getAllProperties())
            {
                if (excluded.contains(prop.getName()))
                {
                    // log.Debug(String.Format("Setting Property Default {0}",
                    // prop.Name)) ;
                    safeAddProperty(prop.getName(), null);
                } else
                {
                    // log.Debug(String.Format("Setting Property {0}",
                    // prop.Name)) ;
                    safeAddProperty(prop.getName(), session.decodeValue(dec,
                            prop.getType()));
                }
            }
        }
        if (hasStats)
        {
            for (SchemaStatistic stat : getSchema().getAllStatistics())
            {
                // log.Debug(String.Format("Setting Statistic {0}", stat.Name))
                // ;
                statistics.put(stat.getName(), session.decodeValue(dec, stat
                        .getType()));
            }
        }
    }

    public final long agentBank()
    {
        return getObjectID().agentBank();
    }

    public final long brokerBank()
    {
        return getObjectID().brokerBank();
    }

    public final void encode(Encoder enc)
    {
        int mask = 0;
        int bit = 0;
        java.util.ArrayList<SchemaProperty> propsToEncode = new java.util.ArrayList<SchemaProperty>();
        log.debug(String.format("Encoding class %s:%s", getSchema()
                .getPackageName(), getSchema().getClassName()));
        enc.writeUint8((short) 20);
        getSchema().getKey().encode(enc);
        for (SchemaProperty prop : getSchema().getAllProperties())
        {
            if (prop.getOptional())
            {
                if (bit == 0)
                {
                    bit = 1;
                }
                if ((properties.containsKey(prop.getName()))
                        && (properties.get(prop.getName()) != null))
                {
                    mask |= bit;
                    propsToEncode.add(prop);
                } else
                {
                }
                bit = bit << 1;
                if (bit == 256)
                {
                    bit = 0;
                    enc.writeUint8((short) mask);
                    mask = 0;
                }
            } else
            {
                propsToEncode.add(prop);
            }
        }
        if (bit != 0)
        {
            enc.writeUint8((short) mask);
        }
        for (SchemaProperty prop : propsToEncode)
        {
            Object obj = properties.get(prop.getName());
            // log.Debug(String.Format("Encoding property {0}", prop.Name)) ;
            getSession().encodeValue(enc, prop.getType(), obj);
        }
        for (SchemaStatistic stat : getSchema().statistics)
        {
            Object obj = statistics.get(stat.getName());
            getSession().encodeValue(enc, stat.getType(), obj);
        }
        log.debug("Done");
    }

    public final Date getCreateTime()
    {
        return createTime;
    }

    public final Date getCurrentTime()
    {
        return currentTime;
    }

    public final Date getDeleteTime()
    {
        return deleteTime;
    }

    protected final ArrayList<SchemaMethod> getMethods()
    {
        return getSchema().getAllMethods();
    }

    public final ObjectID getObjectID()
    {
        return objectID;
    }

    public final Object getProperty(String attributeName)
    {
        return properties.get(attributeName);
    }

    public SchemaClass getSchema()
    {
        return schema;
    }

    public final Session getSession()
    {
        return session;
    }

    protected final MethodResult internalInvokeMethod(String name,
            List<Object> args, boolean synchronous, int timeToLive)
    {
        if (!managed)
        {
            throw new ConsoleException("Object is not Managed");
        }
        if (getSchema().getMethod(name) == null)
        {
            throw new ConsoleException(String.format(
                    "Method named '%s' does not exist", name));
        }
        return getSession().invokeMethod(this, name, args, synchronous,
                timeToLive);
    }

    public final MethodResult invokeMethod(String name, boolean synchronous,
            int timeToLive, Object... args)
    {
        return this.internalInvokeMethod(name, Arrays.asList(args),
                synchronous, timeToLive);
    }

    public final MethodResult invokeMethod(String name, boolean synchronous,
            Object... args)
    {
        return this.internalInvokeMethod(name, Arrays.asList(args),
                synchronous, Broker.SYNC_TIME);
    }

    public final MethodResult invokeMethod(String name, int timeToLive,
            Object... args)
    {
        return this.internalInvokeMethod(name, Arrays.asList(args), true,
                timeToLive);
    }

    public final MethodResult invokeMethod(String name, Object... args)
    {
        return this.internalInvokeMethod(name, Arrays.asList(args), true,
                Broker.SYNC_TIME);
    }

    public final boolean isDeleted()
    {
        return !getDeleteTime().equals(new java.util.Date(0));
    }

    protected final ArrayList<String> processPresenceMasks(Decoder dec,
            SchemaClass schema)
    {
        java.util.ArrayList<String> excludes = new java.util.ArrayList<String>();
        short bit = 0;
        short mask = 0;
        for (SchemaProperty prop : getSchema().getAllProperties())
        {
            if (prop.getOptional())
            {
                // log.Debug(String.Format("Property named {0} is optional",
                // prop.Name)) ;
                if (bit == 0)
                {
                    mask = dec.readUint8();
                    bit = 1;
                }
                if ((mask & bit) == 0)
                {
                    // log.Debug(String.Format("Property named {0} is not present",
                    // prop.Name)) ;
                    excludes.add(prop.getName());
                }
                bit *= 2;
                if (bit == 256)
                {
                    bit = 0;
                }
            }
        }
        return excludes;
    }

    public final String routingKey()
    {
        return getObjectID().routingCode();
    }

    protected final void safeAddProperty(String propName, Object value)
    {
        if (properties.containsKey(propName))
        {
            properties.put(propName, value);
        } else
        {
            properties.put(propName, value);
        }
    }

    public final void setCreateTime(java.util.Date value)
    {
        createTime = value;
    }

    public final void setCurrentTime(java.util.Date value)
    {
        currentTime = value;
    }

    public final void setDeleteTime(java.util.Date value)
    {
        deleteTime = value;
    }

    public final void setObjectID(ObjectID value)
    {
        objectID = value;
    }

    public final void setProperty(String attributeName, Object newValue)
    {
        properties.put(attributeName, newValue);
    }

    public void setSchema(SchemaClass value)
    {
        schema = value;
    }

    public final void setSession(Session value)
    {
        session = value;
    }

    protected final void setStatistic(String attributeName, Object newValue)
    {
        statistics.put(attributeName, newValue);
    }

    @Override
    public String toString()
    {
        String propertyString = "";
        for (Entry<String, Object> pair : properties.entrySet())
        {
            propertyString = propertyString
                    + String.format("(Name: '%0$s' Value: '%1$s')", pair
                            .getKey(), pair.getValue());
        }
        String statsString = "";
        for (Entry<String, Object> sPair : statistics.entrySet())
        {
            statsString = statsString
                    + String.format("(Name: '%0$s' Value: '%1$s')", sPair
                            .getKey(), sPair.getValue());
        }
        if (managed)
        {
            return String
                    .format(
                            "Managed QMFObject %0$s:%1$s(%2$s) Properties: [%3$s] Statistics: [%4$s])",
                            getSchema().getPackageName(), getSchema()
                                    .getClassName(), getObjectID(),
                            propertyString, statsString);
        } else
        {
            return String
                    .format(
                            "QMFObject %0$s:%1$s Properties: [%2$s] Statistics: [%3$s]",
                            getSchema().getPackageName(), getSchema()
                                    .getClassName(), propertyString,
                            statsString);
        }
    }
}