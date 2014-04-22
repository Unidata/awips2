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
package com.raytheon.uf.common.xmpp;

import java.util.Collections;
import java.util.List;

/**
 * Simple XML string builder utility
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 26, 2014 2756       bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class XmlBuilder {

    private final StringBuilder builder = new StringBuilder();

    /**
     * Key value pair object
     */
    public static class Pair {
        public final String name;

        public final String value;

        public Pair(String name, String value) {
            this.name = name;
            this.value = value;
        }
    }

    /**
     * Append open tag to XML
     * 
     * @param name
     * @return
     */
    public XmlBuilder startTag(String name){
        return startTag(name, Collections.<Pair> emptyList());
    }

    /**
     * Append open tag to XML
     * 
     * @param name
     * @param namespace
     *            xmlns attribute to be included in tag
     * @return
     */
    public XmlBuilder startTag(String name, String namespace) {
        return startTag(name, namespace, Collections.<Pair> emptyList());
    }

    /**
     * Append open tag to XML
     * 
     * @param name
     * @param attributes
     * @return
     */
    public XmlBuilder startTag(String name, List<Pair> attributes) {
        return startTag(name, null, attributes);
    }

    /**
     * Append open tag to XML
     * 
     * @param name
     * @param namespace
     *            xmlns attribute to be included in tag
     * @param attributes
     * @return
     */
    public XmlBuilder startTag(String name, String namespace,
            List<Pair> attributes) {
        return appendTag(name, namespace, attributes, false);
    }

    /**
     * Append self closing tag to XML
     * 
     * @param name
     * @param attributes
     * @return
     */
    public XmlBuilder selfClosingTag(String name, List<Pair> attributes) {
        return appendTag(name, null, attributes, true);
    }

    /**
     * Append tag to XML
     * 
     * @param name
     * @param namespace
     *            xmlns attribute to be included in tag
     * @param attributes
     * @param selfClose
     *            if true, tag will self close
     * @return
     */
    public XmlBuilder appendTag(String name, String namespace,
            List<Pair> attributes, boolean selfClose) {
        builder.append("<").append(name);
        if (namespace != null) {
            builder.append(" ").append(PacketConstants.XMLNS_ATTRIBUTE)
                    .append("=\"");
            builder.append(namespace).append("\"");
        }
        for (Pair attrib : attributes) {
            builder.append(" ").append(attrib.name).append("=\"");
            builder.append(attrib.value).append("\"");
        }
        if (selfClose) {
            builder.append("/>");
        } else {
            builder.append(">");
        }
        return this;
    }

    /**
     * Append closing tag to XML
     * 
     * @param name
     * @return
     */
    public XmlBuilder endTag(String name) {
        builder.append("</").append(name).append(">");
        return this;
    }

    /**
     * Append text to XML
     * 
     * @param text
     * @return
     */
    public XmlBuilder appendText(String text) {
        builder.append(text);
        return this;
    }

    /**
     * @return a string containing the XML added to builder
     */
    public String toXml() {
        return builder.toString();
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return toXml();
    }

}
