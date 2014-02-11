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
package com.raytheon.uf.common.http;

import java.util.Iterator;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Parses Accept and Accept-Encoding headers for HTTP requests
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 8, 2013  2539       bclement     Initial creation
 * Feb 14, 2014 2756       bclement     moved to common http from ogc common
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class AcceptHeaderParser implements Iterable<AcceptHeaderValue> {

    private static final Pattern ENCODING_PATTERN = Pattern
            .compile("([^;, \\t]+)\\s*(;\\s*(q\\s*=\\s*([0-9.]+)))?");

    private final String input;

    /**
     * 
     */
    public AcceptHeaderParser(String input) {
        this.input = input;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Iterable#iterator()
     */
    @Override
    public Iterator<AcceptHeaderValue> iterator() {
        final Matcher matcher = ENCODING_PATTERN.matcher(input);
        return new Iterator<AcceptHeaderValue>() {

            @Override
            public boolean hasNext() {
                return matcher.find();
            }

            @Override
            public AcceptHeaderValue next() {
                String enc = matcher.group(1);
                String qvalStr = matcher.group(4);
                if (qvalStr == null) {
                    return new AcceptHeaderValue(enc);
                } else {
                    return new AcceptHeaderValue(enc,
                            Double.parseDouble(qvalStr));
                }
            }

            @Override
            public void remove() {
                throw new UnsupportedOperationException(
                        "Remove not supported for " + this.getClass());
            }
        };
    }

}
