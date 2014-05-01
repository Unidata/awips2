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
package com.raytheon.uf.edex.decodertools.core;

import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;

/**
 * Default parsing strategy that parses a string by spaces. All spaces are
 * removed from the list, all other white space is retained.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20071019            391 jkorman     Initial coding.
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class DefaultParserStrategy implements IParserStrategy {

    /**
     * Simple parser that parses a string on spaces. If the input data is null
     * an empty list is returned.
     * 
     * @param dataToParse
     *            The input string to parse.
     * @return A list of the parsed elements.
     * @see com.raytheon.uf.edex.decodertools.core.IParserStrategy#parse(java.lang.String)
     */
    @Override
    public List<String> parse(String dataToParse) {
        List<String> reportParts = new ArrayList<String>();

        if (dataToParse != null) {
            StringTokenizer st = new StringTokenizer(dataToParse, " ", true);
            while (st.hasMoreTokens()) {
                String s = st.nextToken();
                if (!" ".equals(s)) {
                    reportParts.add(s);
                }
            }
        }
        return reportParts;
    }
}
