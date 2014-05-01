package org.apache.mina.common;

import org.apache.mina.common.ByteBuffer;

import java.nio.*;

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
public class FixedSizeByteBufferAllocator  implements ByteBufferAllocator
{


        private static final int MINIMUM_CAPACITY = 1;

        public FixedSizeByteBufferAllocator  ()
        {
        }

        public ByteBuffer allocate( int capacity, boolean direct )
        {
            java.nio.ByteBuffer nioBuffer;
            if( direct )
            {
                nioBuffer = java.nio.ByteBuffer.allocateDirect( capacity );
            }
            else
            {
                nioBuffer = java.nio.ByteBuffer.allocate( capacity );
            }
            return new FixedSizeByteBuffer( nioBuffer );
        }

        public ByteBuffer wrap( java.nio.ByteBuffer nioBuffer )
        {
            return new FixedSizeByteBuffer( nioBuffer );
        }

        public void dispose()
        {
        }



        private static final class FixedSizeByteBuffer extends ByteBuffer
        {
            private java.nio.ByteBuffer buf;
            private int mark = -1;


            protected FixedSizeByteBuffer( java.nio.ByteBuffer buf )
            {
                this.buf = buf;
                buf.order( ByteOrder.BIG_ENDIAN );
            }

            public synchronized void acquire()
            {
            }

            public void release()
            {
            }

            public java.nio.ByteBuffer buf()
            {
                return buf;
            }

            public boolean isPooled()
            {
                return false;
            }

            public void setPooled( boolean pooled )
            {
            }

            public ByteBuffer duplicate() {
                return new FixedSizeByteBuffer( this.buf.duplicate() );
            }

            public ByteBuffer slice() {
                return new FixedSizeByteBuffer( this.buf.slice() );
            }

            public ByteBuffer asReadOnlyBuffer() {
                return new FixedSizeByteBuffer( this.buf.asReadOnlyBuffer() );
            }

            public byte[] array()
            {
                return buf.array();
            }

            public int arrayOffset()
            {
                return buf.arrayOffset();
            }

            public boolean isDirect()
            {
                return buf.isDirect();
            }

            public boolean isReadOnly()
            {
                return buf.isReadOnly();
            }

            public int capacity()
            {
                return buf.capacity();
            }

            public ByteBuffer capacity( int newCapacity )
                {
                    if( newCapacity > capacity() )
                    {
                        throw new IllegalArgumentException();
                    }

                    return this;
                }



            public boolean isAutoExpand()
            {
                return false;
            }

            public ByteBuffer setAutoExpand( boolean autoExpand )
            {
                if(autoExpand) throw new IllegalArgumentException();
                else return this;
            }

            public ByteBuffer expand( int pos, int expectedRemaining )
            {
                int end = pos + expectedRemaining;
                if( end > capacity() )
                {
                    // The buffer needs expansion.
                    capacity( end );
                }

                if( end > limit() )
                {
                    // We call limit() directly to prevent StackOverflowError
                    buf.limit( end );
                }
                return this;
            }

            public int position()
            {
                return buf.position();
            }

            public ByteBuffer position( int newPosition )
            {

                buf.position( newPosition );
                if( mark > newPosition )
                {
                    mark = -1;
                }
                return this;
            }

            public int limit()
            {
                return buf.limit();
            }

            public ByteBuffer limit( int newLimit )
            {
                buf.limit( newLimit );
                if( mark > newLimit )
                {
                    mark = -1;
                }
                return this;
            }

            public ByteBuffer mark()
            {
                buf.mark();
                mark = position();
                return this;
            }

            public int markValue()
            {
                return mark;
            }

            public ByteBuffer reset()
            {
                buf.reset();
                return this;
            }

            public ByteBuffer clear()
            {
                buf.clear();
                mark = -1;
                return this;
            }

            public ByteBuffer flip()
            {
                buf.flip();
                mark = -1;
                return this;
            }

            public ByteBuffer rewind()
            {
                buf.rewind();
                mark = -1;
                return this;
            }

            public byte get()
            {
                return buf.get();
            }

            public ByteBuffer put( byte b )
            {
                buf.put( b );
                return this;
            }

            public byte get( int index )
            {
                return buf.get( index );
            }

            public ByteBuffer put( int index, byte b )
            {
                buf.put( index, b );
                return this;
            }

            public ByteBuffer get( byte[] dst, int offset, int length )
            {
                buf.get( dst, offset, length );
                return this;
            }

            public ByteBuffer put( java.nio.ByteBuffer src )
            {
                buf.put( src );
                return this;
            }

            public ByteBuffer put( byte[] src, int offset, int length )
            {
                buf.put( src, offset, length );
                return this;
            }

            public ByteBuffer compact()
            {
                buf.compact();
                mark = -1;
                return this;
            }

            public ByteOrder order()
            {
                return buf.order();
            }

            public ByteBuffer order( ByteOrder bo )
            {
                buf.order( bo );
                return this;
            }

            public char getChar()
            {
                return buf.getChar();
            }

            public ByteBuffer putChar( char value )
            {
                buf.putChar( value );
                return this;
            }

            public char getChar( int index )
            {
                return buf.getChar( index );
            }

            public ByteBuffer putChar( int index, char value )
            {
                buf.putChar( index, value );
                return this;
            }

            public CharBuffer asCharBuffer()
            {
                return buf.asCharBuffer();
            }

            public short getShort()
            {
                return buf.getShort();
            }

            public ByteBuffer putShort( short value )
            {
                buf.putShort( value );
                return this;
            }

            public short getShort( int index )
            {
                return buf.getShort( index );
            }

            public ByteBuffer putShort( int index, short value )
            {
                buf.putShort( index, value );
                return this;
            }

            public ShortBuffer asShortBuffer()
            {
                return buf.asShortBuffer();
            }

            public int getInt()
            {
                return buf.getInt();
            }

            public ByteBuffer putInt( int value )
            {
                buf.putInt( value );
                return this;
            }

            public int getInt( int index )
            {
                return buf.getInt( index );
            }

            public ByteBuffer putInt( int index, int value )
            {
                buf.putInt( index, value );
                return this;
            }

            public IntBuffer asIntBuffer()
            {
                return buf.asIntBuffer();
            }

            public long getLong()
            {
                return buf.getLong();
            }

            public ByteBuffer putLong( long value )
            {
                buf.putLong( value );
                return this;
            }

            public long getLong( int index )
            {
                return buf.getLong( index );
            }

            public ByteBuffer putLong( int index, long value )
            {
                buf.putLong( index, value );
                return this;
            }

            public LongBuffer asLongBuffer()
            {
                return buf.asLongBuffer();
            }

            public float getFloat()
            {
                return buf.getFloat();
            }

            public ByteBuffer putFloat( float value )
            {
                buf.putFloat( value );
                return this;
            }

            public float getFloat( int index )
            {
                return buf.getFloat( index );
            }

            public ByteBuffer putFloat( int index, float value )
            {
                buf.putFloat( index, value );
                return this;
            }

            public FloatBuffer asFloatBuffer()
            {
                return buf.asFloatBuffer();
            }

            public double getDouble()
            {
                return buf.getDouble();
            }

            public ByteBuffer putDouble( double value )
            {
                buf.putDouble( value );
                return this;
            }

            public double getDouble( int index )
            {
                return buf.getDouble( index );
            }

            public ByteBuffer putDouble( int index, double value )
            {
                buf.putDouble( index, value );
                return this;
            }

            public DoubleBuffer asDoubleBuffer()
            {
                return buf.asDoubleBuffer();
            }


        }


}
