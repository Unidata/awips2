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
package org.apache.qpid.console;//

import java.lang.reflect.Constructor;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import javax.jms.Message;

import org.apache.qpid.transport.codec.BBDecoder;
import org.apache.qpid.transport.codec.BBEncoder;
import org.apache.qpid.transport.codec.Decoder;
import org.apache.qpid.transport.codec.Encoder;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Session
{
    private static Logger log = LoggerFactory.getLogger(Session.class);
    public static int CONTEXT_SYNC = 1;
    public static int CONTEXT_STARTUP = 2;
    public static int CONTEXT_MULTIGET = 3;
    public static int DEFAULT_GET_WAIT_TIME = 60000;
    public boolean recieveObjects = true;
    public boolean recieveEvents = true;
    public boolean recieveHeartbeat = true;
    public boolean userBindings = false;
    public Console console;
    protected HashMap<String, HashMap<String, SchemaClass>> packages = new HashMap<String, HashMap<String, SchemaClass>>();
    protected ArrayList<Broker> brokers = new ArrayList<Broker>();
    protected SequenceManager sequenceManager = new SequenceManager();
    protected Object lockObject = new Object();
    protected ArrayList<Long> syncSequenceList = new ArrayList<Long>();
    protected ArrayList<QMFObject> getResult;
    protected Object syncResult;

    public Session()
    {
    }

    public Session(Console console)
    {
        this.console = console;
    }

    public void addBroker(String url)
    {
        Broker broker = new Broker(this, url);
        brokers.add(broker);
        java.util.HashMap<String, Object> args = new java.util.HashMap<String, Object>();
        args.put("_class", "agent");
        args.put("_broker", broker);
        this.getObjects(args);
    }

    public ArrayList<String> bindingKeys()
    {
        ArrayList<String> bindings = new ArrayList<String>();
        bindings.add("schema.#");
        if (recieveObjects & recieveEvents & recieveHeartbeat & !userBindings)
        {
            bindings.add("console.#");
        } else
        {
            if (recieveObjects & !userBindings)
            {
                bindings.add("console.obj.#");
            } else
            {
                bindings.add("console.obj.*.*.org.apache.qpid.broker.agent");
            }
            if (recieveEvents)
            {
                bindings.add("console.event.#");
            }
            if (recieveHeartbeat)
            {
                bindings.add("console.heartbeat.#");
            }
        }
        return bindings;
    }

    public void close()
    {
        for (Broker broker : brokers.toArray(new Broker[0]))
        {
            this.removeBroker(broker);
        }
    }

    protected QMFObject createQMFObject(SchemaClass schema,
            boolean hasProperties, boolean hasStats, boolean isManaged)
    {
        Class realClass = QMFObject.class;
        if (console != null)
        {
            realClass = console.typeMapping(schema.getKey());
        }
        Class[] types = new Class[]
        { Session.class, SchemaClass.class, boolean.class, boolean.class,
                boolean.class };
        Object[] args = new Object[]
        { this, schema, hasProperties, hasStats, isManaged };
        try
        {
            Constructor ci = realClass.getConstructor(types);
            return (QMFObject) ci.newInstance(args);
        } catch (Exception e)
        {
            throw new ConsoleException(e);
        }
    }

    protected QMFObject createQMFObject(SchemaClass schema, Decoder dec,
            boolean hasProperties, boolean hasStats, boolean isManaged)
    {
        Class realClass = QMFObject.class;
        if (console != null)
        {
            realClass = console.typeMapping(schema.getKey());
        }
        Class[] types = new Class[]
        { Session.class, SchemaClass.class, Decoder.class, boolean.class,
                boolean.class, boolean.class };
        Object[] args = new Object[]
        { this, schema, dec, hasProperties, hasStats, isManaged };
        try
        {
            log.debug("" + realClass);
            Constructor ci = realClass.getConstructor(types);
            return (QMFObject) ci.newInstance(args);
        } catch (Exception e)
        {
            throw new ConsoleException(e);
        }
    }

    public Object decodeValue(Decoder dec, short type)
    {
        switch (type)
        {
        case 1: // U8
            return dec.readUint8();
        case 2: // U16
            return dec.readUint16();
        case 3: // U32
            return dec.readUint32();
        case 4: // U64
            return dec.readUint64();
        case 6: // SSTR
            return dec.readStr8();
        case 7: // LSTR
            return dec.readStr16();
        case 8: // ABSTIME
            return dec.readDatetime();
        case 9: // DELTATIME
            return dec.readUint32();
        case 10: // ref
            return new ObjectID(dec);
        case 11: // bool
            return dec.readUint8() != 0;
        case 12: // float
            return dec.readFloat();
        case 13: // double
            return dec.readDouble();
        case 14: // UUID
            return dec.readUuid();
        case 15: // Ftable
            return dec.readMap();
        case 16: // int8
            return dec.readInt8();
        case 17: // int16
            return dec.readInt16();
        case 18: // int32
            return dec.readInt32();
        case 19: // int64
            return dec.readInt64();
        case 20: // Object
            // Peek into the inner type code, make sure
            // it is actually an object
            Object returnValue = null;
            short innerTypeCode = dec.readUint8();
            if (innerTypeCode != 20)
            {
                returnValue = this.decodeValue(dec, innerTypeCode);
            } else
            {
                ClassKey classKey = new ClassKey(dec);
                synchronized (lockObject)
                {
                    SchemaClass sClass = getSchema(classKey);
                    if (sClass != null)
                    {
                        returnValue = this.createQMFObject(sClass, dec, true,
                                true, false);
                    }
                }
            }
            return returnValue;
        case 21: // List
            BBDecoder lDec = new BBDecoder();
            lDec.init(ByteBuffer.wrap(dec.readVbin32()));
            long count = lDec.readUint32();
            ArrayList<Object> newList = new ArrayList<Object>();
            while (count > 0)
            {
                short innerType = lDec.readUint8();
                newList.add(this.decodeValue(lDec, innerType));
                count -= 1;
            }
            return newList;
        case 22: // Array
            BBDecoder aDec = new BBDecoder();
            aDec.init(ByteBuffer.wrap(dec.readVbin32()));
            long cnt = aDec.readUint32();
            short innerType = aDec.readUint8();
            ArrayList<Object> aList = new ArrayList<Object>();
            while (cnt > 0)
            {
                aList.add(this.decodeValue(aDec, innerType));
                cnt -= 1;
            }
            return aList;
        default:
            throw new ConsoleException(String.format("Invalid Type Code: %s",
                    type));
        }
    }

    public void encodeValue(Encoder enc, short type, Object val)
    {
        try
        {
            switch (type)
            {
            case 1: // U8
                enc.writeUint8(((Short) val).shortValue());
                break;
            case 2: // U16
                enc.writeUint16(((Integer) val).intValue());
                break;
            case 3: // U32
                enc.writeUint32(((Integer) val).longValue());
                break;
            case 4: // U64
                enc.writeUint64(((Long) val).longValue());
                break;
            case 6: // SSTR
                enc.writeStr8((String) val);
                break;
            case 7: // LSTR
                enc.writeStr16((String) val);
                break;
            case 8: // ABSTIME
                enc.writeDatetime(((Long) val).longValue());
                break;
            case 9: // DELTATIME
                enc.writeUint32(((Long) val).longValue());
                break;
            case 10: // ref
                ((ObjectID) val).encode(enc);
                break;
            case 11:
                if (((Boolean) val).booleanValue())
                {
                    enc.writeUint8((short) 1);
                } else
                {
                    enc.writeUint8((short) 0);
                }
                break;
            case 12: // FLOAT
                enc.writeFloat(((Float) val).floatValue());
                break;
            case 13: // DOUBLE
                enc.writeDouble(((Double) val).doubleValue());
                break;
            case 14: // UUID
                enc.writeUuid((UUID) val);
                break;
            case 15: // Ftable
                enc.writeMap((HashMap) val);
                break;
            case 16: // int8
                enc.writeInt8((Byte) val);
                break;
            case 17: // int16
                enc.writeInt16((Short) val);
                break;
            case 18: // int32
                enc.writeInt32((Integer) val);
                break;
            case 19: // int64
                enc.writeInt64((Long) val);
                break;
            case 20: // Object
                // Check that the object has a session, if not
                // take ownership of it
                QMFObject qObj = (QMFObject) val;
                if (qObj.getSession() == null)
                {
                    qObj.setSession(this);
                }
                qObj.encode(enc);
                break;
            case 21: // List
                ArrayList<Object> items = (ArrayList<Object>) val;
                BBEncoder lEnc = new BBEncoder(1);
                lEnc.init();
                lEnc.writeUint32(items.size());
                for (Object obj : items)
                {
                    short innerType = Util.qmfType(obj);
                    lEnc.writeUint8(innerType);
                    this.encodeValue(lEnc, innerType, obj);
                }
                enc.writeVbin32(lEnc.segment().array());
                break;
            case 22: // Array
                ArrayList<Object> aItems = (ArrayList<Object>) val;
                BBEncoder aEnc = new BBEncoder(1);
                aEnc.init();
                long aCount = aItems.size();
                aEnc.writeUint32(aCount);
                if (aCount > 0)
                {
                    Object anObj = aItems.get(0);
                    short innerType = Util.qmfType(anObj);
                    aEnc.writeUint8(innerType);
                    for (Object obj : aItems)
                    {
                        this.encodeValue(aEnc, innerType, obj);
                    }
                }
                enc.writeVbin32(aEnc.segment().array());
                break;
            default:
                throw new ConsoleException(String.format(
                        "Invalid Type Code: %s", type));
            }
        } catch (ClassCastException e)
        {
            String msg = String.format(
                    "Class cast exception for typecode %s, type %s ", type, val
                            .getClass());
            log.error(msg);
            throw new ConsoleException(msg + type, e);
        }
    }

    public Broker getBroker(long BrokerBank)
    {
        Broker returnValue = null;
        for (Broker broker : brokers)
        {
            if (broker.brokerBank() == BrokerBank)
            {
                returnValue = broker;
                break;
            }
        }
        return returnValue;
    }

    public ArrayList<ClassKey> getClasses(String packageName)
    {
        ArrayList<ClassKey> returnValue = new ArrayList<ClassKey>();
        this.waitForStable();
        if (packages.containsKey(packageName))
        {
            for (SchemaClass sClass : packages.get(packageName).values())
            {
                returnValue.add(sClass.getKey());
            }
        }
        return returnValue;
    }

    public ArrayList<QMFObject> getObjects(
            java.util.HashMap<String, Object> args)
    {
        ArrayList<Broker> brokerList = null;
        ArrayList<Agent> agentList = new ArrayList<Agent>();
        if (args.containsKey("_broker"))
        {
            brokerList = new ArrayList<Broker>();
            brokerList.add((Broker) args.get("_broker"));
        } else
        {
            brokerList = this.brokers;
        }
        for (Broker broker : brokerList)
        {
            broker.waitForStable();
        }
        if (args.containsKey("_agent"))
        {
            Agent agent = (Agent) args.get("_agent");
            if (brokerList.contains(agent.getBroker()))
            {
                agentList.add(agent);
            } else
            {
                throw new ConsoleException(
                        "Agent is not managed by this console or the supplied broker");
            }
        } else
        {
            if (args.containsKey("_objectId"))
            {
                ObjectID oid = (ObjectID) args.get("_objectId");
                for (Broker broker : brokers)
                {
                    for (Agent agent : broker.Agents.values())
                    {
                        if ((agent.getAgentBank() == oid.agentBank())
                                && (agent.getBrokerBank() == oid.brokerBank()))
                        {
                            agentList.add(agent);
                        }
                    }
                }
            } else
            {
                for (Broker broker : brokerList)
                {
                    for (Agent agent : broker.Agents.values())
                    {
                        if (agent.getBroker().isConnected())
                        {
                            agentList.add(agent);
                        }
                    }
                }
            }
        }
        getResult = new ArrayList<QMFObject>();
        if (agentList.size() > 0)
        {
            // FIXME Add a bunch of other suff too
            for (Agent agent : agentList)
            {
                HashMap<String, Object> getParameters = new HashMap<String, Object>();
                Broker broker = agent.getBroker();
                long seq = -1;
                synchronized (lockObject)
                {
                    seq = sequenceManager.reserve(Session.CONTEXT_MULTIGET);
                    syncSequenceList.add(seq);
                }
                String packageName = (String) args.get("_package");
                String className = (String) args.get("_class");
                ClassKey key = (ClassKey) args.get("_key");
                Object sClass = args.get("_schema");
                Object oid = args.get("_objectID");
                long[] hash = (long[]) args.get("_hash");
                if ((className == null) && (oid == null) && (oid == null))
                {
                    throw new ConsoleException(
                            "No class supplied, use '_schema', '_key', '_class', or '_objectId' argument");
                }
                if (oid != null)
                {
                    getParameters.put("_objectID", oid);
                } else
                {
                    if (sClass != null)
                    {
                        key = (key != null) ? key : ((SchemaClass) sClass)
                                .getKey();
                    }
                    if (key != null)
                    {
                        className = (className != null) ? className : key
                                .getClassName();
                        packageName = (packageName != null) ? packageName : key
                                .getPackageName();
                        hash = (hash != null) ? hash : key.getHash();
                    }
                    if (packageName != null)
                    {
                        getParameters.put("_package", packageName);
                    }
                    if (className != null)
                    {
                        getParameters.put("_class", className);
                    }
                    if (hash != null)
                    {
                        getParameters.put("_hash", hash);
                    }
                    for (java.util.Map.Entry<String, Object> pair : args
                            .entrySet())
                    {
                        if (!pair.getKey().startsWith("_"))
                        {
                            getParameters.put(pair.getKey(), pair.getValue());
                        }
                    }
                }
                Encoder enc = broker.createEncoder('G', seq);
                enc.writeMap(getParameters);
                String routingKey = agent.routingCode();
                Message msg = broker.createMessage(enc);
                log.debug("Get Object Keys: ");
                for (String pKey : getParameters.keySet())
                {
                    log.debug(String.format("\tKey: '%s' Value: '%s'", pKey,
                            getParameters.get(pKey)));
                }
                broker.send(msg, routingKey);
            }
            int waittime = DEFAULT_GET_WAIT_TIME;
            boolean timeout = false;
            if (args.containsKey("_timeout"))
            {
                waittime = (Integer) args.get("_timeout");
            }
            long start = System.currentTimeMillis();
            synchronized (lockObject)
            {
                // FIXME ERROR
                while (syncSequenceList.size() > 0)
                {
                    try
                    {
                        lockObject.wait(waittime);
                    } catch (InterruptedException e)
                    {
                        throw new ConsoleException(e);
                    }
                    long duration = System.currentTimeMillis() - start;
                    if (duration > waittime)
                    {
                        for (long pendingSeq : syncSequenceList)
                        {
                            sequenceManager.release(pendingSeq);
                        }
                        syncSequenceList.clear();
                        timeout = true;
                    }
                }
            }
            // FIXME Add the error logic
            if ((getResult.isEmpty()) && timeout)
            {
                throw new ConsoleException("Get Request timed out");
            }
        }
        return getResult;
    }

    public ArrayList<String> getPackages()
    {
        this.waitForStable();
        ArrayList<String> returnValue = new ArrayList<String>();
        for (String name : packages.keySet())
        {
            returnValue.add(name);
        }
        return returnValue;
    }

    public SchemaClass getSchema(ClassKey key)
    {
        return getSchema(key, true);
    }

    protected SchemaClass getSchema(ClassKey key, boolean waitForStable)
    {
        if (waitForStable)
        {
            this.waitForStable();
        }
        SchemaClass returnValue = null;
        returnValue = packages.get(key.getPackageName())
                .get(key.getKeyString());
        return returnValue;
    }

    public void handleAgentRemoved(Agent agent)
    {
        if (console != null)
        {
            console.agentRemoved(agent);
        }
    }

    public void handleBrokerConnect(Broker broker)
    {
        if (console != null)
        {
            console.brokerConnected(broker);
        }
    }

    public void handleBrokerDisconnect(Broker broker)
    {
        if (console != null)
        {
            console.brokerDisconnected(broker);
        }
    }

    public void handleBrokerResponse(Broker broker, Decoder decoder,
            long sequence)
    {
        if (console != null)
        {
            console.brokerInformation(broker);
        }
        long seq = sequenceManager.reserve(CONTEXT_STARTUP);
        Encoder encoder = broker.createEncoder('P', seq);
        broker.send(encoder);
    }

    public void handleClassIndicator(Broker broker, Decoder decoder,
            long sequence)
    {
        short kind = decoder.readUint8();
        ClassKey classKey = new ClassKey(decoder);
        boolean unknown = false;
        synchronized (lockObject)
        {
            if (packages.containsKey(classKey.getPackageName()))
            {
                if (!packages.get(classKey.getPackageName()).containsKey(
                        classKey.getKeyString()))
                {
                    unknown = true;
                }
            }
        }
        if (unknown)
        {
            broker.incrementOutstanding();
            long seq = sequenceManager.reserve(Session.CONTEXT_STARTUP);
            Encoder enc = broker.createEncoder('S', seq);
            classKey.encode(enc);
            broker.send(enc);
        }
    }

    public void handleCommandComplete(Broker broker, Decoder decoder,
            long sequence)
    {
        long code = decoder.readUint32();
        String text = decoder.readStr8();
        Object context = this.sequenceManager.release(sequence);
        if (context.equals(CONTEXT_STARTUP))
        {
            broker.decrementOutstanding();
        } else
        {
            if ((context.equals(CONTEXT_SYNC)) & broker.getSyncInFlight())
            {
                broker.setSyncInFlight(false);
            } else
            {
                if (context.equals(CONTEXT_MULTIGET)
                        && syncSequenceList.contains(sequence))
                {
                    synchronized (lockObject)
                    {
                        syncSequenceList.remove(sequence);
                        if (syncSequenceList.isEmpty())
                        {
                            lockObject.notifyAll();
                        }
                    }
                }
            }
        }
    }

    public void handleContentIndicator(Broker broker, Decoder decoder,
            long sequence, boolean hasProperties, boolean hasStatistics)
    {
        ClassKey key = new ClassKey(decoder);
        SchemaClass sClass = null;
        ;
        synchronized (lockObject)
        {
            sClass = getSchema(key, false);
        }
        if (sClass != null)
        {
            QMFObject obj = this.createQMFObject(sClass, decoder,
                    hasProperties, hasStatistics, true);
            if (key.getPackageName().equals("org.apache.qpid.broker")
                    && key.getClassName().equals("agent") && hasProperties)
            {
                broker.updateAgent(obj);
            }
            synchronized (lockObject)
            {
                if (syncSequenceList.contains(sequence))
                {
                    if (!obj.isDeleted() && this.selectMatch(obj))
                    {
                        getResult.add(obj);
                    }
                }
            }
            if (console != null)
            {
                if (hasProperties)
                {
                    console.objectProperties(broker, obj);
                }
                if (hasStatistics)
                {
                    console.objectStatistics(broker, obj);
                }
            }
        }
    }

    public void handleEventIndicator(Broker broker, Decoder decoder,
            long sequence)
    {
        if (console != null)
        {
            QMFEvent newEvent = new QMFEvent(this, decoder);
            console.eventRecieved(broker, newEvent);
        }
    }

    public void handleHeartbeatIndicator(Broker broker, Decoder decoder,
            long sequence, Message msg)
    {
        if (console != null)
        {
            long brokerBank = 1;
            long agentBank = 0;
            try
            {
                // FIXME HOW DO WE GET THE ROUTING KEY
                // String routingKey = msg.DeliveryProperties.getRoutingKey();
                String routingKey = null;
                if (routingKey != null)
                {
                    agentBank = Agent.getBrokerBank(routingKey);
                    brokerBank = Agent.getBrokerBank(routingKey);
                }
            } catch (Throwable e)
            {
                log.warn("Internal QPID error", e);
            }
            String agentKey = Agent.AgentKey(agentBank, brokerBank);
            long timestamp = decoder.readUint64();
            if (broker.Agents.containsKey(agentKey))
            {
                Agent agent = broker.Agents.get(agentKey);
                console.hearbeatRecieved(agent, timestamp);
            }
        }
    }

    public void handleMethodResponse(Broker broker, Decoder decoder,
            long sequence)
    {
        long code = decoder.readUint32();
        String text = decoder.readStr16();
        java.util.HashMap<String, Object> outArgs = new java.util.HashMap<String, Object>();
        Object obj = sequenceManager.release(sequence);
        if (obj == null)
        {
            return;
        }
        Object[] pair = (Object[]) obj;
        if (code == 0)
        {
            for (SchemaArgument arg : ((SchemaMethod) pair[0]).Arguments)
            {
                if (arg.isOutput())
                {
                    outArgs.put(arg.getName(), this.decodeValue(decoder, arg
                            .getType()));
                }
            }
        }
        MethodResult result = new MethodResult(code, text, outArgs);
        if ((Boolean) pair[1])
        {
            this.syncResult = result;
            broker.setSyncInFlight(false);
        }
        if (console != null)
        {
            console.methodResponse(broker, sequence, result);
        }
    }

    // Callback Methods
    public void handleNewAgent(Agent agent)
    {
        if (console != null)
        {
            console.newAgent(agent);
        }
    }

    public void handlePackageIndicator(Broker broker, Decoder decoder,
            long sequence)
    {
        String packageName = decoder.readStr8();
        boolean notify = false;
        if (!packages.containsKey(packageName))
        {
            synchronized (lockObject)
            {
                packages.put(packageName,
                        new java.util.HashMap<String, SchemaClass>());
                notify = true;
            }
        }
        if (notify && console != null)
        {
            console.newPackage(packageName);
        }
        broker.incrementOutstanding();
        long seq = sequenceManager.reserve(Session.CONTEXT_STARTUP);
        Encoder enc = broker.createEncoder('Q', seq);
        enc.writeStr8(packageName);
        broker.send(enc);
    }

    public void handleSchemaResponse(Broker broker, Decoder decoder,
            long sequence)
    {
        short kind = decoder.readUint8();
        ClassKey classKey = new ClassKey(decoder);
        SchemaClass sClass = new SchemaClass(kind, classKey, decoder, this);
        synchronized (lockObject)
        {
            java.util.HashMap<String, SchemaClass> classMappings = packages
                    .get(sClass.getPackageName());
            classMappings.remove(sClass.getClassKeyString());
            classMappings.put(sClass.getClassKeyString(), sClass);
            log.debug(classKey.toString());
        }
        sequenceManager.release(sequence);
        broker.decrementOutstanding();
        if (console != null)
        {
            this.console.newClass(kind, classKey);
        }
    }

    public MethodResult invokeMethod(QMFObject obj, String name,
            List<Object> args, boolean synchronous, int timeToLive)
    {
        Broker aBroker = this.getBroker(obj.brokerBank());
        long seq = this.sendMethodRequest(obj, aBroker, name, args,
                synchronous, timeToLive);
        if (seq != 0)
        {
            if (!synchronous)
            {
                return null;
            }
            try
            {
                aBroker.waitForSync(timeToLive);
            } catch (Throwable e)
            {
                sequenceManager.release(seq);
                throw new ConsoleException(e);
            }
            // FIXME missing error logic in the broker
            return (MethodResult) syncResult;
        }
        return null;
    }

    public QMFObject makeObject(ClassKey key)
    {
        SchemaClass sClass = this.getSchema(key);
        if (sClass == null)
        {
            throw new ConsoleException("No schema found for class "
                    + key.toString());
        }
        return this.createQMFObject(sClass, true, true, false);
    }

    public QMFObject makeObject(String keyString)
    {
        return this.makeObject(new ClassKey(keyString));
    }

    public void removeBroker(Broker broker)
    {
        if (brokers.contains(broker))
        {
            brokers.remove(broker);
        }
        broker.shutdown();
    }

    public boolean selectMatch(QMFObject obj)
    {
        return true;
    }

    protected long sendMethodRequest(QMFObject obj, Broker aBroker,
            String name, List<Object> args, boolean synchronous, int timeToLive)
    {
        SchemaMethod method = obj.getSchema().getMethod(name);
        if (args == null)
        {
            args = new ArrayList<Object>();
        }
        long seq = 0;
        if (method != null)
        {
            Object[] pair =
            { method, synchronous };
            seq = sequenceManager.reserve(pair);
            Encoder enc = aBroker.createEncoder('M', seq);
            obj.getObjectID().encode(enc);
            obj.getSchema().getKey().encode(enc);
            enc.writeStr8(name);
            if (args.size() < method.getInputArgCount())
            {
                throw new ConsoleException(String.format(
                        "Incorrect number of arguments: expected %s, got %s",
                        method.getInputArgCount(), args.size()));
            }
            int argIndex = 0;
            for (SchemaArgument arg : method.Arguments)
            {
                if (arg.isInput())
                {
                    this.encodeValue(enc, arg.getType(), args.get(argIndex));
                    argIndex += 1;
                }
            }
            Message msg = aBroker.createMessage(enc);
            if (synchronous)
            {
                aBroker.setSyncInFlight(true);
            }
            aBroker.send(msg, obj.routingKey(), timeToLive);
        }
        return seq;
    }

    protected void waitForStable()
    {
        for (Broker broker : brokers)
        {
            broker.waitForStable();
        }
    }
}
