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

import java.util.ArrayList;

import org.apache.mina.common.ByteBuffer;
import org.apache.mina.common.IoSession;
import org.apache.mina.common.SimpleByteBufferAllocator;
import org.apache.mina.filter.codec.CumulativeProtocolDecoder;
import org.apache.mina.filter.codec.ProtocolDecoderOutput;

import org.apache.qpid.framing.AMQDataBlock;
import org.apache.qpid.framing.AMQDataBlockDecoder;
import org.apache.qpid.framing.AMQFrameDecodingException;
import org.apache.qpid.framing.AMQMethodBodyFactory;
import org.apache.qpid.framing.AMQProtocolVersionException;
import org.apache.qpid.framing.ProtocolInitiation;
import org.apache.qpid.protocol.AMQVersionAwareProtocolSession;

/**
 * AMQDecoder delegates the decoding of AMQP either to a data block decoder, or in the case of new connections, to a
 * protocol initiation decoder. It is a cumulative decoder, which means that it can accumulate data to decode in the
 * buffer until there is enough data to decode.
 *
 * <p/>One instance of this class is created per session, so any changes or configuration done at run time to the
 * decoder will only affect decoding of the protocol session data to which is it bound.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Delegate protocol initiation to its decoder. <td> {@link ProtocolInitiation.Decoder}
 * <tr><td> Delegate AMQP data to its decoder. <td> {@link AMQDataBlockDecoder}
 * <tr><td> Accept notification that protocol initiation has completed.
 * </table>
 *
 * @todo If protocol initiation decoder not needed, then don't create it. Probably not a big deal, but it adds to the
 *       per-session overhead.
 */
public class AMQDecoder extends CumulativeProtocolDecoder
{

    private static final String BUFFER = AMQDecoder.class.getName() + ".Buffer";

    /** Holds the 'normal' AMQP data decoder. */
    private AMQDataBlockDecoder _dataBlockDecoder = new AMQDataBlockDecoder();

    /** Holds the protocol initiation decoder. */
    private ProtocolInitiation.Decoder _piDecoder = new ProtocolInitiation.Decoder();

    /** Flag to indicate whether this decoder needs to handle protocol initiation. */
    private boolean _expectProtocolInitiation;
    private boolean firstDecode = true;

    private AMQMethodBodyFactory _bodyFactory;

    private ByteBuffer _remainingBuf;
    
    /**
     * Creates a new AMQP decoder.
     *
     * @param expectProtocolInitiation <tt>true</tt> if this decoder needs to handle protocol initiation.
     */
    public AMQDecoder(boolean expectProtocolInitiation, AMQVersionAwareProtocolSession session)
    {
        _expectProtocolInitiation = expectProtocolInitiation;
        _bodyFactory = new AMQMethodBodyFactory(session);
    }

    /**
     * Delegates decoding AMQP from the data buffer that Mina has retrieved from the wire, to the data or protocol
     * intiation decoders.
     *
     * @param session The Mina session.
     * @param in      The raw byte buffer.
     * @param out     The Mina object output gatherer to write decoded objects to.
     *
     * @return <tt>true</tt> if the data was decoded, <tt>false<tt> if more is needed and the data should accumulate.
     *
     * @throws Exception If the data cannot be decoded for any reason.
     */
    protected boolean doDecode(IoSession session, ByteBuffer in, ProtocolDecoderOutput out) throws Exception
    {

        boolean decoded;
        if (_expectProtocolInitiation  
            || (firstDecode
                && (in.remaining() > 0)
                && (in.get(in.position()) == (byte)'A')))
        {
            decoded = doDecodePI(session, in, out);
        }
        else
        {
            decoded = doDecodeDataBlock(session, in, out);
        }
        if(firstDecode && decoded)
        {
            firstDecode = false;
        }
        return decoded;
    }

    /**
     * Decodes AMQP data, delegating the decoding to an {@link AMQDataBlockDecoder}.
     *
     * @param session The Mina session.
     * @param in      The raw byte buffer.
     * @param out     The Mina object output gatherer to write decoded objects to.
     *
     * @return <tt>true</tt> if the data was decoded, <tt>false<tt> if more is needed and the data should accumulate.
     *
     * @throws Exception If the data cannot be decoded for any reason.
     */
    protected boolean doDecodeDataBlock(IoSession session, ByteBuffer in, ProtocolDecoderOutput out) throws Exception
    {
        int pos = in.position();
        boolean enoughData = _dataBlockDecoder.decodable(in.buf());
        in.position(pos);
        if (!enoughData)
        {
            // returning false means it will leave the contents in the buffer and
            // call us again when more data has been read
            return false;
        }
        else
        {
            _dataBlockDecoder.decode(session, in, out);

            return true;
        }
    }

    /**
     * Decodes an AMQP initiation, delegating the decoding to a {@link ProtocolInitiation.Decoder}.
     *
     * @param session The Mina session.
     * @param in      The raw byte buffer.
     * @param out     The Mina object output gatherer to write decoded objects to.
     *
     * @return <tt>true</tt> if the data was decoded, <tt>false<tt> if more is needed and the data should accumulate.
     *
     * @throws Exception If the data cannot be decoded for any reason.
     */
    private boolean doDecodePI(IoSession session, ByteBuffer in, ProtocolDecoderOutput out) throws Exception
    {
        boolean enoughData = _piDecoder.decodable(in.buf());
        if (!enoughData)
        {
            // returning false means it will leave the contents in the buffer and
            // call us again when more data has been read
            return false;
        }
        else
        {
            ProtocolInitiation pi = new ProtocolInitiation(in.buf());
            out.write(pi);

            return true;
        }
    }

    /**
     * Sets the protocol initation flag, that determines whether decoding is handled by the data decoder of the protocol
     * initation decoder. This method is expected to be called with <tt>false</tt> once protocol initation completes.
     *
     * @param expectProtocolInitiation <tt>true</tt> to use the protocol initiation decoder, <tt>false</tt> to use the
     *                                data decoder.
     */
    public void setExpectProtocolInitiation(boolean expectProtocolInitiation)
    {
        _expectProtocolInitiation = expectProtocolInitiation;
    }


    /**
     * Cumulates content of <tt>in</tt> into internal buffer and forwards
     * decoding request to {@link #doDecode(IoSession, ByteBuffer, ProtocolDecoderOutput)}.
     * <tt>doDecode()</tt> is invoked repeatedly until it returns <tt>false</tt>
     * and the cumulative buffer is compacted after decoding ends.
     *
     * @throws IllegalStateException if your <tt>doDecode()</tt> returned
     *                               <tt>true</tt> not consuming the cumulative buffer.
     */
    public void decode( IoSession session, ByteBuffer in,
                        ProtocolDecoderOutput out ) throws Exception
    {
        ByteBuffer buf = ( ByteBuffer ) session.getAttribute( BUFFER );
        // if we have a session buffer, append data to that otherwise
        // use the buffer read from the network directly
        if( buf != null )
        {
            buf.put( in );
            buf.flip();
        }
        else
        {
            buf = in;
        }

        for( ;; )
        {
            int oldPos = buf.position();
            boolean decoded = doDecode( session, buf, out );
            if( decoded )
            {
                if( buf.position() == oldPos )
                {
                    throw new IllegalStateException(
                            "doDecode() can't return true when buffer is not consumed." );
                }

                if( !buf.hasRemaining() )
                {
                    break;
                }
            }
            else
            {
                break;
            }
        }

        // if there is any data left that cannot be decoded, we store
        // it in a buffer in the session and next time this decoder is
        // invoked the session buffer gets appended to
        if ( buf.hasRemaining() )
        {
            storeRemainingInSession( buf, session );
        }
        else
        {
            removeSessionBuffer( session );
        }
    }

    /**
     * Releases the cumulative buffer used by the specified <tt>session</tt>.
     * Please don't forget to call <tt>super.dispose( session )</tt> when
     * you override this method.
     */
    public void dispose( IoSession session ) throws Exception
    {
        removeSessionBuffer( session );
    }

    private void removeSessionBuffer(IoSession session)
    {
        ByteBuffer buf = ( ByteBuffer ) session.getAttribute( BUFFER );
        if( buf != null )
        {
            buf.release();
            session.removeAttribute( BUFFER );
        }
    }

    private static final SimpleByteBufferAllocator SIMPLE_BYTE_BUFFER_ALLOCATOR = new SimpleByteBufferAllocator();

    private void storeRemainingInSession(ByteBuffer buf, IoSession session)
    {
        ByteBuffer remainingBuf = SIMPLE_BYTE_BUFFER_ALLOCATOR.allocate( buf.remaining(), false );
        remainingBuf.setAutoExpand( true );
        remainingBuf.put( buf );
        session.setAttribute( BUFFER, remainingBuf );
    }

    public ArrayList<AMQDataBlock> decodeBuffer(java.nio.ByteBuffer buf) throws AMQFrameDecodingException, AMQProtocolVersionException
    {

        // get prior remaining data from accumulator
        ArrayList<AMQDataBlock> dataBlocks = new ArrayList<AMQDataBlock>();
        ByteBuffer msg;
        // if we have a session buffer, append data to that otherwise
        // use the buffer read from the network directly
        if( _remainingBuf != null )
        {
            _remainingBuf.put(buf);
            _remainingBuf.flip();
            msg = _remainingBuf;
        }
        else
        {
            msg = ByteBuffer.wrap(buf);
        }
        
        if (_expectProtocolInitiation  
            || (firstDecode
                && (msg.remaining() > 0)
                && (msg.get(msg.position()) == (byte)'A')))
        {
            if (_piDecoder.decodable(msg.buf()))
            {
                dataBlocks.add(new ProtocolInitiation(msg.buf()));
            }
        }
        else
        {
            boolean enoughData = true;
            while (enoughData)
            {
                int pos = msg.position();

                enoughData = _dataBlockDecoder.decodable(msg);
                msg.position(pos);
                if (enoughData)
                {
                    dataBlocks.add(_dataBlockDecoder.createAndPopulateFrame(_bodyFactory, msg));
                }
                else
                {
                    _remainingBuf = SIMPLE_BYTE_BUFFER_ALLOCATOR.allocate(msg.remaining(), false);
                    _remainingBuf.setAutoExpand(true);
                    _remainingBuf.put(msg);
                }
            }
        }
        if(firstDecode && dataBlocks.size() > 0)
        {
            firstDecode = false;
        }
        return dataBlocks;
    }
}
