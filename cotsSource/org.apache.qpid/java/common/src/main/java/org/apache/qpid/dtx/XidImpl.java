/* Licensed to the Apache Software Foundation (ASF) under one
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
 */
package org.apache.qpid.dtx;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.apache.qpid.QpidException;

import javax.transaction.xa.Xid;

import java.io.*;

/**
 * Implements javax.transaction.dtx.Xid
 */
public class XidImpl implements Xid
{
    /**
     * this session's logger
     */
    private static final Logger _logger = LoggerFactory.getLogger(XidImpl.class);

    /**
     * the transaction branch identifier part of XID as an array of bytes
     */
    private byte[] _branchQualifier;

    /**
     * the format identifier part of the XID.
     */
    private int _formatID;

    /**
     * the global transaction identifier part of XID as an array of bytes.
     */
    private byte[] _globalTransactionID;

    //--- Constructors

    /**
     * Create new Xid.
     * this is an empty constructor.
     */
    public XidImpl()
    {

    }

    /**
     * Create a new XidImpl from an existing Xid.
     * <p> Usefull for casting external Xids
     *
     * @param xid Foreign Xid.
     */
    public XidImpl(Xid xid)
    {
        _branchQualifier = xid.getBranchQualifier();
        _formatID = xid.getFormatId();
        _globalTransactionID = xid.getGlobalTransactionId();
    }

    /**
     * Create a new Xid.
     *
     * @param branchQualifier     The transaction branch identifier part of XID as an array of bytes.
     * @param format              The format identifier part of the XID.
     * @param globalTransactionID The global transaction identifier part of XID as an array of bytes.
     */
    public XidImpl(byte[] branchQualifier, int format, byte[] globalTransactionID)
    {
        _branchQualifier = branchQualifier;
        _formatID = format;
        _globalTransactionID = globalTransactionID;
    }

    /**
     * Create a new Xid form its String form
     * 4         1   1        g            b
     * +---+---+---+---+---+---+---+-  -+---+---+-  -+---+
     * |   format_id   | g | b |   txn-id   |   br-id    |
     * +---+---+---+---+---+---+---+-  -+---+---+-  -+---+
     * 0               4   5   6           6+g         6+g+b
     * format_id: an implementation specific format identifier
     * <p/>
     * gtrid_length: how many bytes of this form the transaction id
     * <p/>
     * bqual_length: how many bytes of this form the branch id
     * <p/>
     * data: a sequence of octets of at most 128 bytes containing the txn id and the
     * branch id
     * <p/>
     * Note - The sum of the two lengths must equal the length of the data field.
     *
     * @param xid an XID STring Form
     * @throws QpidException If the string does not represent a valid Xid
     */
    public XidImpl(String xid) throws QpidException
    {
        if (_logger.isDebugEnabled())
        {
            _logger.debug("converting string " + xid + " into XidImpl");
        }
        try
        {
            DataInputStream input = new DataInputStream(new ByteArrayInputStream(xid.getBytes()));
            _formatID = (int) input.readLong();
            int g = input.readByte();
            int b = input.readByte();
            _globalTransactionID = new byte[g];
            _branchQualifier = new byte[b];
            if (input.read(_globalTransactionID, 0, g) != g)
            {
                throw new QpidException("Cannot convert the string " + xid + " into an Xid", null, null);
            }
            if (input.read(_branchQualifier, 0, b) != b)
            {
                throw new QpidException("Cannot convert the string " + xid + " into an Xid", null, null);
            }
        }
        catch (IOException e)
        {
            throw new QpidException("cannot convert the string " + xid + " into an Xid", null, e);
        }
    }

    //---  Xid interface implementation

    /**
     * Format identifier. O means the OSI CCR format.
     *
     * @return Global transaction identifier.
     */
    public byte[] getGlobalTransactionId()
    {
        return _globalTransactionID;
    }

    /**
     * Obtain the transaction branch identifier part of XID as an array of bytes.
     *
     * @return Branch identifier part of XID.
     */
    public byte[] getBranchQualifier()
    {
        return _branchQualifier;
    }

    /**
     * Obtain the format identifier part of the XID.
     *
     * @return Format identifier. O means the OSI CCR format.
     */
    public int getFormatId()
    {
        return _formatID;
    }

    //--- Object operations

    /**
     * Indicates whether some other Xid is "equal to" this one.
     * <p> Two Xids are equal if and only if their three elementary parts are equal
     *
     * @param o the object to compare this <code>XidImpl</code> against.
     * @return true if the <code>XidImpl</code> are equal, false otherwise.
     */
    public boolean equals(Object o)
    {
        if (this == o)
        {
            return true;
        }
        if (o instanceof XidImpl)
        {
            XidImpl other = (XidImpl) o;
            if (_formatID == other.getFormatId())
            {
                if (_branchQualifier.length == other.getBranchQualifier().length)
                {
                    for (int i = 0; i < _branchQualifier.length; i++)
                    {
                        if (_branchQualifier[i] != other.getBranchQualifier()[i])
                        {
                            return false;
                        }
                    }
                    if (_globalTransactionID.length == other.getGlobalTransactionId().length)
                    {
                        for (int i = 0; i < _globalTransactionID.length; i++)
                        {
                            if (_globalTransactionID[i] != other.getGlobalTransactionId()[i])
                            {
                                return false;
                            }
                        }
                        // everithing is equal
                        return true;
                    }
                }
            }
        }
        return false;
    }

    //-- Static helper method
    /**
     * Convert an Xid into the AMQP String format.
     * 
     * 4         1   1        g            b
     * +---+---+---+---+---+---+---+-  -+---+---+-  -+---+
     * |   format_id   | g | b |   txn-id   |   br-id    |
     * +---+---+---+---+---+---+---+-  -+---+---+-  -+---+
     * 0               4   5   6           6+g         6+g+b
     * format_id: an implementation specific format identifier
     * <p/>
     * gtrid_length: how many bytes of this form the transaction id
     * <p/>
     * bqual_length: how many bytes of this form the branch id
     * <p/>
     * data: a sequence of octets of at most 128 bytes containing the txn id and the
     * branch id
     * <p/>
     * Note - The sum of the two lengths must equal the length of the data field.
     *
     * @param xid an Xid to convert.
     * @return The String representation of this Xid
     * @throws QpidException In case of problem when converting this Xid into a string.
     */
    public static org.apache.qpid.transport.Xid convert(Xid xid) throws QpidException
    {
        return new org.apache.qpid.transport.Xid(xid.getFormatId(),
                                                    xid.getGlobalTransactionId(),
                                                    xid.getBranchQualifier());
    }
}
