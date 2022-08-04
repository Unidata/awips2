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
package com.raytheon.edex.plugin.sfcobs.decoder;

import java.util.List;

/**
 * This interface declares a single method parse(). The parse method takes a
 * string are returns a list of the parts of the input. The returned list of
 * string elements must always be not null. If an null or empty string is input
 * an empty list must be returned. In addition, the parts within the list must
 * remain ordered as they occur in the source string.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20071019            391 jkorman     Initial coding.
 * Sep 26, 2014       3629 mapeters    Moved from uf.edex.decodertools.core.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public interface IParserStrategy {

    /**
     * Use some algorithm to parse a string into individual elements. In the
     * event that the input data is null, this method must return an empty list.
     * 
     * @param dataToParse
     *            A string to be parsed.
     * @return A list of the parsed elements.
     */
    public List<String> parse(String dataToParse);
}
