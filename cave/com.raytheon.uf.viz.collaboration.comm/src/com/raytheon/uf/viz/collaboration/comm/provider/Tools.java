/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.viz.collaboration.comm.provider;

import java.util.Map;
import java.util.Map.Entry;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.jivesoftware.smack.packet.Packet;
import org.jivesoftware.smack.util.Base64;

/**
 * Provides some utility methods for parsing and serializing/deserializing data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Mar 07, 2012           jkorman     Initial creation
 * Oct 31, 2013  2491     bsteffen    Use CollaborationXmlManager for xml
 *                                    serialization.
 * Dec  6, 2013  2561     bclement    removed ECF
 * Dec 16, 2013  2562     bclement    moved compression to smack, 
 *                                    moved marshalling to session payload
 * 
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public abstract class Tools {

    public static final String PROP_SESSION_ID = "sessionId";

    public static final String CMD_PREAMBLE = "[[COMMAND#";

    public static final String CONFIG_PREAMBLE = "[[CONFIG#";

    public static final String DIRECTIVE_SUFFIX = "]]";

    public static final String VENUE_SUBJECT_PROP = "subject";

    public static final String NAME_DELIM = "@";

    public static final String PORT_DELIM = ":";

    public static final String RESOURCE_DELIM = "/";

    public static final Pattern JID_RESERVED_CHARACTERS = Pattern
            .compile("[ \"&'/:<>@]");

    /**
     * 
     * @param id
     * @return
     */
    public static String parseName(String id) {
        String name = null;
        if (id != null) {
            int delimPos = id.indexOf(NAME_DELIM);
            if (delimPos >= 0) {
                name = id.substring(0, delimPos);
            }
        }
        return name;
    }

    /**
     * 
     * @param id
     * @return
     */
    public static String parseHost(String id) {
        String host = null;
        if (id != null) {
            int delimPos = id.indexOf(NAME_DELIM);
            if (delimPos >= 0) {
                // Ok we have the start of the host name
                int start = delimPos + 1;
                int stop = start;
                delimPos = id.indexOf(PORT_DELIM);
                if (delimPos > stop) {
                    // ok we have a port so grab anything in between
                    stop = delimPos;
                } else {
                    // no port delimiter, so check for the resource
                    delimPos = id.indexOf(RESOURCE_DELIM);
                    if (delimPos > stop) {
                        // we have the resource delimiter
                        stop = delimPos;
                    } else {
                        stop = id.length();
                    }
                }
                host = id.substring(start, stop);
            }
        }
        return host;
    }

    /**
     * 
     * @param id
     * @return
     */
    public static String parsePort(String id) {
        String host = null;
        if (id != null) {
            int delimPos = id.indexOf(NAME_DELIM);
            if (delimPos >= 0) {
                // Ok we have the start of the host name
                int start = delimPos + 1;
                int stop = start;
                delimPos = id.indexOf(PORT_DELIM);
                if (delimPos > stop) {
                    // ok we have a port so grab anything in between
                    start = delimPos + 1;
                    delimPos = id.indexOf(RESOURCE_DELIM);
                    if (delimPos > start) {
                        stop = delimPos;
                    } else {
                        stop = id.length();
                    }
                    host = id.substring(start, stop);
                }
            }
        }
        return host;
    }

    /**
     * 
     * @param id
     * @return
     */
    public static String parseResource(String id) {
        String resource = null;
        if (id != null) {
            int delimPos = id.indexOf(NAME_DELIM);
            // Ensure that the name delimiter is there first.
            if (delimPos >= 0) {
                int start = delimPos + 1;
                delimPos = id.indexOf(RESOURCE_DELIM);
                if (delimPos > start) {
                    delimPos++;
                    if (delimPos < id.length()) {
                        resource = id.substring(delimPos);
                    }
                }
            }
        }
        return resource;
    }

    /**
     * Decode Base64 encoded String data into a byte array.
     * 
     * @param message
     * @return The decoded byte array data.
     */
    public static byte[] decodeFromBase64(String message) {
        return Base64.decode(message);
    }

    /**
     * Encode byte array data into a Base64 String for transmission.
     * 
     * @param message
     *            The message to encode.
     * @return The encoded data as a String.
     */
    public static String encodeToBase64(byte[] message) {
        return Base64.encodeBytes(message);
    }

    /**
     * Add properties to packet
     * 
     * @param p
     * @param props
     */
    public static void setProperties(Packet p, Map<String, String> props) {
        for (Entry<String, String> e : props.entrySet()) {
            p.setProperty(e.getKey(), e.getValue());
        }
    }

    /**
     * Copy properties from packet
     * 
     * @param p
     * @param props
     */
    public static void copyProperties(Packet source, Packet dest) {
        for (String key : source.getPropertyNames()) {
            dest.setProperty(key, source.getProperty(key));
        }
    }

    /**
     * @param id
     * @return true if id doesn't contain any invalid characters
     */
    public static boolean isValidId(String id) {
        if (id == null) {
            return false;
        }
        Matcher matcher = JID_RESERVED_CHARACTERS.matcher(id);
        return !matcher.find();
    }

}
