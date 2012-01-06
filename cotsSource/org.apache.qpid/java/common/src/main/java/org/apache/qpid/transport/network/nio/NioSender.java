package org.apache.qpid.transport.network.nio;
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


import java.nio.ByteBuffer;
import java.nio.channels.SocketChannel;

import org.apache.qpid.transport.Sender;

public class NioSender implements Sender<java.nio.ByteBuffer>
{
    private final Object lock = new Object();
    private SocketChannel _ch;
    private boolean _batch =  false;
    private ByteBuffer _batcher;

    public NioSender(SocketChannel ch)
    {
        this._ch = ch;
    }

    public void send(java.nio.ByteBuffer buf)
    {
        if (_batch)
        {
            //System.out.println(_batcher.position() + " , " +  buf.remaining() + " , " + buf.position() + ","+_batcher.capacity());
            if (_batcher.position() + buf.remaining() >= _batcher.capacity())
            {
                _batcher.flip();
                write(_batcher);
                _batcher.clear();
                if (buf.remaining() > _batcher.capacity())
                {
                    write(buf);
                }
                else
                {
                    _batcher.put(buf);
                }
            }
            else
            {
                _batcher.put(buf);
            }
        }
        else
        {
            write(buf);
        }
    }

    public void flush()
    {
        // pass
    }

    private void write(java.nio.ByteBuffer buf)
    {
        synchronized (lock)
        {
            if( _ch.isConnected() && _ch.isOpen())
            {
                try
                {
                    _ch.write(buf);
                }
                catch(Exception e)
                {
                    e.fillInStackTrace();
                }
            }
            else
            {
                throw new RuntimeException("Trying to write on a closed socket");
            }

        }
    }

    public void setStartBatching()
    {
        _batch = true;
        _batcher = ByteBuffer.allocate(1024);
    }

    public void close()
    {
        // MINA will sometimes throw away in-progress writes when you
        // ask it to close
        synchronized (lock)
        {
            try
            {
                _ch.close();
            }
            catch(Exception e)
            {
                e.printStackTrace();
            }
        }
    }
    
    public void setIdleTimeout(long l)
    {
      //noop
    }
}
