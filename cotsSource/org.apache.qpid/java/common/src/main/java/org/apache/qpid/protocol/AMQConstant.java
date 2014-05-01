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
package org.apache.qpid.protocol;

import java.util.HashMap;
import java.util.Map;

import org.apache.qpid.framing.AMQShortString;

/**
 * Defines constants for AMQP codes and also acts as a factory for creating such constants from the raw codes. Each
 * constant also defines a short human readable description of the constant.
 *
 * @todo Why would a constant be defined that is not in the map? Seems more natural that getConstant should raise an
 *       exception for an unknown constant. Or else provide an explanation of why this is so. Also, there is no way for
 *       callers to determine the unknown status of a code except by comparing its name to "unknown code", which would
 *       seem to render this scheme a little bit pointless?
 *
 * @todo Java has a nice enum construct for doing this sort of thing. Maybe this is done in the old style for Java 1.4
 *       backward compatability? Now that is handled through retrotranslater it may be time to use enum.
 *
 * <p/><tabld id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Define the set of AMQP status codes.
 * <tr><td> Provide a factory to lookup constants by their code.
 * <tr><td>
 */
public final class AMQConstant
{
    /** Defines a map from codes to constants. */
    private static Map _codeMap = new HashMap();

    /** Indicates that the method completed successfully. */
    public static final AMQConstant REPLY_SUCCESS = new AMQConstant(200, "reply success", true);

    public static final AMQConstant FRAME_END = new AMQConstant(206, "frame end", true);

    /**
     * The client asked for a specific message that is no longer available. The message was delivered to another
     * client, or was purged from the queue for some other reason.
     */
    public static final AMQConstant NOT_DELIVERED = new AMQConstant(310, "not delivered", true);

    /**
     * The client attempted to transfer content larger than the server could accept at the present time.  The client
     * may retry at a later time.
     */
    public static final AMQConstant MESSAGE_TOO_LARGE = new AMQConstant(311, "message too large", true);

    /**
     * When the exchange cannot route the result of a .Publish, most likely due to an invalid routing key. Only when
     * the mandatory flag is set.
     */
    public static final AMQConstant NO_ROUTE = new AMQConstant(312, "no route", true);

    /**
     * When the exchange cannot deliver to a consumer when the immediate flag is set. As a result of pending data on
     * the queue or the absence of any consumers of the queue.
     */
    public static final AMQConstant NO_CONSUMERS = new AMQConstant(313, "no consumers", true);

    /**
     * An operator intervened to close the connection for some reason. The client may retry at some later date.
     */
    public static final AMQConstant CONTEXT_IN_USE = new AMQConstant(320, "context in use", true);

    /** The client tried to work with an unknown virtual host or cluster. */
    public static final AMQConstant INVALID_PATH = new AMQConstant(402, "invalid path", true);

    /** The client attempted to work with a server entity to which it has no access due to security settings. */
    public static final AMQConstant ACCESS_REFUSED = new AMQConstant(403, "access refused", true);

    /** The client attempted to work with a server entity that does not exist. */
    public static final AMQConstant NOT_FOUND = new AMQConstant(404, "not found", true);

    /**
     * The client attempted to work with a server entity to which it has no access because another client is
     * working with it.
     */
    public static final AMQConstant ALREADY_EXISTS = new AMQConstant(405, "Already exists", true);

    /** The client requested a method that was not allowed because some precondition failed. */
    public static final AMQConstant IN_USE = new AMQConstant(406, "In use", true);

    public static final AMQConstant INVALID_ROUTING_KEY = new AMQConstant(407, "routing key invalid", true);

    public static final AMQConstant REQUEST_TIMEOUT = new AMQConstant(408, "Request Timeout", true);

    public static final AMQConstant INVALID_ARGUMENT = new AMQConstant(409, "argument invalid", true);

    /**
     * The client sent a malformed frame that the server could not decode. This strongly implies a programming error
     * in the client.
     */
    public static final AMQConstant FRAME_ERROR = new AMQConstant(501, "frame error", true);

    /**
     * The client sent a frame that contained illegal values for one or more fields. This strongly implies a
     * programming error in the client.
     */
    public static final AMQConstant SYNTAX_ERROR = new AMQConstant(502, "syntax error", true);

    /**
     * The client sent an invalid sequence of frames, attempting to perform an operation that was considered invalid
     * by the server. This usually implies a programming error in the client.
     */
    public static final AMQConstant COMMAND_INVALID = new AMQConstant(503, "command invalid", true);

    /**
     * The client attempted to work with a channel that had not been correctly opened. This most likely indicates a
     * fault in the client layer.
     */
    public static final AMQConstant CHANNEL_ERROR = new AMQConstant(504, "channel error", true);

    /**
     * The server could not complete the method because it lacked sufficient resources. This may be due to the client
     * creating too many of some type of entity.
     */
    public static final AMQConstant RESOURCE_ERROR = new AMQConstant(506, "resource error", true);

    /**
     * The client tried to work with some entity in a manner that is prohibited by the server, due to security settings
     * or by some other criteria.
     */
    public static final AMQConstant NOT_ALLOWED = new AMQConstant(530, "not allowed", true);

    /** The client tried to use functionality that is not implemented in the server. */
    public static final AMQConstant NOT_IMPLEMENTED = new AMQConstant(540, "not implemented", true);

    /**
     * The server could not complete the method because of an internal error. The server may require intervention by
     * an operator in order to resume normal operations.
     */
    public static final AMQConstant INTERNAL_ERROR = new AMQConstant(541, "internal error", true);

    public static final AMQConstant FRAME_MIN_SIZE = new AMQConstant(4096, "frame min size", true);

    /**
     * The server does not support the protocol version
     */
    public static final AMQConstant UNSUPPORTED_BROKER_PROTOCOL_ERROR = new AMQConstant(542, "broker unsupported protocol", true);
    /**
     * The client imp does not support the protocol version
     */
    public static final AMQConstant UNSUPPORTED_CLIENT_PROTOCOL_ERROR = new AMQConstant(543, "client unsupported protocol", true);

    /** The AMQP status code. */
    private int _code;

    /** A short description of the status code. */
    private AMQShortString _name;

    /**
     * Creates a new AMQP status code.
     *
     * @param code The code.
     * @param name A short description of the code.
     * @param map  <tt>true</tt> to register the code as a known code, <tt>false</tt> otherwise.
     */
    private AMQConstant(int code, String name, boolean map)
    {
        _code = code;
        _name = new AMQShortString(name);
        if (map)
        {
            _codeMap.put(new Integer(code), this);
        }
    }

    /**
     * Creates a constant for a status code by looking up the code in the map of known codes. If the code is not known
     * a constant is still created for it, but it is marked as unknown.
     *
     * @param code The AMQP status code.
     *
     * @return The AMQP status code encapsulated as a constant.
     */
    public static AMQConstant getConstant(int code)
    {
        AMQConstant c = (AMQConstant) _codeMap.get(new Integer(code));
        if (c == null)
        {
            c = new AMQConstant(code, "unknown code", false);
        }

        return c;
    }

    /**
     * Gets the underlying AMQP status code.
     *
     * @return The AMQP status code.
     */
    public int getCode()
    {
        return _code;
    }

    /**
     * Gets a short description of the status code.
     *
     * @return A short description of the status code.
     */
    public AMQShortString getName()
    {
        return _name;
    }

    /**
     * Renders the constant as a string, mainly for debugging purposes.
     *
     * @return The status code and its description.
     */
    public String toString()
    {
        return _code + ": " + _name;
    }
}
