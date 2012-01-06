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
package org.apache.qpid.codec;

import org.apache.qpid.framing.*;
import org.apache.mina.common.*;
import org.apache.mina.common.support.BaseIoSession;
import org.apache.mina.filter.codec.ProtocolDecoderOutput;
import org.apache.mina.filter.codec.ProtocolEncoderOutput;

import java.net.SocketAddress;

/**
 */
public class BasicDeliverTest
{
    public static void main(String[] argv) throws Exception
    {
        BasicDeliverTest test = new BasicDeliverTest();

        //warm up:
        test.encode(512, 100000);

        //real tests:
        test.encode(16, 10000, 15);
        test.encode(32, 10000, 15);
        test.encode(64, 10000, 15);
        test.encode(128, 10000, 15);
        test.encode(256, 10000, 15);
        test.encode(512, 10000, 15);
        test.encode(1024, 10000, 15);
        test.encode(2048, 10000, 15);

        test.decode(16, 10000, 15);
        test.decode(32, 10000, 15);
        test.decode(64, 10000, 15);
        test.decode(128, 10000, 15);
        test.decode(256, 10000, 15);
        test.decode(512, 10000, 15);
        test.decode(1024, 10000, 15);
        test.decode(2048, 10000, 15);
    }

    void decode(int size, int count, int iterations) throws Exception
    {
        long min = Long.MAX_VALUE;
        long max = 0;
        long total = 0;
        for (int i = 0; i < iterations; i++)
        {
            long time = decode(size, count);
            total += time;
            if (time < min)
            {
                min = time;
            }
            if (time > max)
            {
                max = time;
            }
        }
        System.out.println("Decoded " + count + " messages of " + size +
                " bytes: avg=" + (total / iterations) + ", min=" + min + ", max=" + max);
    }


    long decode(int size, int count) throws Exception
    {
        AMQDataBlock block = getDataBlock(size);
        ByteBuffer data = ByteBuffer.allocate((int) block.getSize()); // XXX: Is cast a problem?
        block.writePayload(data);
        data.flip();
        AMQDecoder decoder = new AMQDecoder(false);
        long start = System.currentTimeMillis();
        for (int i = 0; i < count; i++)
        {
            decoder.decode(session, data, decoderOutput);
            data.rewind();
        }
        return System.currentTimeMillis() - start;
    }

    void encode(int size, int count, int iterations) throws Exception
    {
        long min = Long.MAX_VALUE;
        long max = 0;
        long total = 0;
        for (int i = 0; i < iterations; i++)
        {
            long time = encode(size, count);
            total += time;
            if (time < min)
            {
                min = time;
            }
            if (time > max)
            {
                max = time;
            }
        }
        System.out.println("Encoded " + count + " messages of " + size +
                " bytes: avg=" + (total / iterations) + ", min=" + min + ", max=" + max);
    }

    long encode(int size, int count) throws Exception
    {
        IoSession session = null;
        AMQDataBlock block = getDataBlock(size);
        AMQEncoder encoder = new AMQEncoder();
        long start = System.currentTimeMillis();
        for (int i = 0; i < count; i++)
        {
            encoder.encode(session, block, encoderOutput);
        }
        return System.currentTimeMillis() - start;
    }

    private final ProtocolEncoderOutput encoderOutput = new ProtocolEncoderOutput()
    {

        public void write(ByteBuffer byteBuffer)
        {
        }

        public void mergeAll()
        {
        }

        public WriteFuture flush()
        {
            return null;
        }
    };

    private final ProtocolDecoderOutput decoderOutput = new ProtocolDecoderOutput()
    {
        public void write(Object object)
        {
        }

        public void flush()
        {
        }
    };

    private final IoSession session = new BaseIoSession()
    {

        protected void updateTrafficMask()
        {
            //To change body of implemented methods use File | Settings | File Templates.
        }

        public IoService getService()
        {
            return null;  //To change body of implemented methods use File | Settings | File Templates.
        }

        public IoServiceConfig getServiceConfig()
        {
            return null;
        }

        public IoHandler getHandler()
        {
            return null;  //To change body of implemented methods use File | Settings | File Templates.
        }

        public IoSessionConfig getConfig()
        {
            return null;  //To change body of implemented methods use File | Settings | File Templates.
        }

        public IoFilterChain getFilterChain()
        {
            return null;  //To change body of implemented methods use File | Settings | File Templates.
        }

        public TransportType getTransportType()
        {
            return null;  //To change body of implemented methods use File | Settings | File Templates.
        }

        public SocketAddress getRemoteAddress()
        {
            return null;  //To change body of implemented methods use File | Settings | File Templates.
        }

        public SocketAddress getLocalAddress()
        {
            return null;  //To change body of implemented methods use File | Settings | File Templates.
        }

        public SocketAddress getServiceAddress()
        {
            return null;  //To change body of implemented methods use File | Settings | File Templates.
        }

        public int getScheduledWriteRequests()
        {
            return 0;  //To change body of implemented methods use File | Settings | File Templates.
        }

        public int getScheduledWriteBytes()
        {
            return 0;  //To change body of implemented methods use File | Settings | File Templates.
        }
    };

    private static final char[] DATA = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ".toCharArray();

    static CompositeAMQDataBlock getDataBlock(int size)
    {
        //create a frame representing message delivery
        AMQFrame[] frames = new AMQFrame[3];
        frames[0] = wrapBody(createBasicDeliverBody());
        frames[1] = wrapBody(createContentHeaderBody());
        frames[2] = wrapBody(createContentBody(size));

        return new CompositeAMQDataBlock(frames);
    }

    static AMQFrame wrapBody(AMQBody body)
    {
        AMQFrame frame = new AMQFrame(1, body);
        return frame;
    }

    static ContentBody createContentBody(int size)
    {
        ContentBody body = new ContentBody();
        body.payload = ByteBuffer.allocate(size);
        for (int i = 0; i < size; i++)
        {
            body.payload.put((byte) DATA[i % DATA.length]);
        }
        return body;
    }

    static ContentHeaderBody createContentHeaderBody()
    {
        ContentHeaderBody body = new ContentHeaderBody();
        body.properties = new BasicContentHeaderProperties();
        body.weight = 1;
        body.classId = 6;
        return body;
    }

    static BasicDeliverBody createBasicDeliverBody()
    {
        BasicDeliverBody body = new BasicDeliverBody((byte) 8, (byte) 0,
                                                     BasicDeliverBody.getClazz((byte) 8, (byte) 0),
                                                     BasicDeliverBody.getMethod((byte) 8, (byte) 0),                                                     
                                                     new AMQShortString("myConsumerTag"), 1,
                                                     new AMQShortString("myExchange"), false,
                                                     new AMQShortString("myRoutingKey"));
        return body;
    }
}
