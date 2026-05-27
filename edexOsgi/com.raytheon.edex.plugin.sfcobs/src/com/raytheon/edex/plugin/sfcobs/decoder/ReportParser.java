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
import java.util.regex.Matcher;
import java.util.regex.Pattern;


/**
 * A simple parser element factored from decoder report parsing. This class
 * wraps a list of elements parsed from a report.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03 Oct 2007         391 jkorman     Initial Development
 * 20071130            410 jkorman     Changed bad copyright symbol.
 * 20071203            410 jkorman     JavaDoc complaints.
 * Sep 18, 2014       3627 mapeters    Removed unused methods.
 * Sep 26, 2014       3629 mapeters    Moved from uf.edex.decodertools.core.
 * </pre>
 * 
 * @author jkorman
 * @version 1
 */
public class ReportParser {

    // List containing the parsed elements.
    private List<String> reportParts = null;

    private String report = null;

    // Current retrieval position.
    private int currentElement = -1;

    /**
     * Create a new parser from given report data.
     * 
     * @param reportData
     *            A message that should contain one report.
     * @param parser
     *            A parser strategy to be used when splitting the reportData. If
     *            a null reference is passed, the an instance of
     *            DefaultParserStrategy is used.
     * @see com.raytheon.edex.plugin.sfcobs.decoder.DefaultParserStrategy#parse(java.lang.String)
     */
    public ReportParser(String reportData, IParserStrategy parser) {

        if (parser == null) {
            parser = new DefaultParserStrategy();
        }
        report = reportData;
        reportParts = parser.parse(reportData);

        currentElement = 0;
    }

    /**
     * Get the report data for this parser.
     * 
     * @return The report data.
     */
    public String getReport() {
        return report;
    }

    /**
     * Get the current text element from the parser. This method will return the
     * same value until advance/previous is called.
     * 
     * @return The current text element.
     */
    public String getElement() {
        String element = null;
        if (currentElement < reportParts.size()) {
            element = reportParts.get(currentElement);
        }
        return element;
    }

    /**
     * Advance to the next text element.
     * 
     * @return Was the internal position advanced.
     */
    public boolean next() {
        boolean advanced = false;
        if (currentElement < reportParts.size()) {
            currentElement++;
            advanced = true;
        }
        return advanced;
    }

    /**
     * Move the current position to a given value. The element to position to
     * must equal the parsed element. If the match element is not found, the
     * internal position is not modified. If found the internal position is
     * modified to point to the found element.
     * 
     * @param element
     *            The element that must match a parsed element.
     * @return Was the element found.
     */
    public boolean positionTo(String element) {
        boolean positioned = false;
        if (currentElement < reportParts.size()) {
            // Attempt to locate the value.
            int pos = reportParts.indexOf(element);
            if (pos >= 0) {
                // Found it, so set currentElement.
                currentElement = pos;
                positioned = true;
            }
        }
        return positioned;
    }

    /**
     * Move the current position to a given pattern. The element to position to
     * must match the parsed element. If the match element is not found, the
     * internal position is not modified. If found the internal position is
     * modified to point to the found element.
     * 
     * @param element
     *            The element that must match a parsed element.
     * @return Was the element found.
     */
    public boolean positionTo(Pattern pattern) {
        boolean positioned = false;
        int pos = currentElement;
        while (pos < reportParts.size()) {
            Matcher m = pattern.matcher(reportParts.get(pos));
            if (m.matches()) {
                // Found it, so set currentElement.
                currentElement = pos;
                positioned = true;
                break;
            } else {
                pos++;
            }
        } // while()
        return positioned;
    }

}
