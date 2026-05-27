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
package com.raytheon.uf.common.grib.tables;

import java.nio.CharBuffer;
import java.util.ArrayList;
import java.util.List;

import javax.swing.text.MutableAttributeSet;
import javax.swing.text.html.HTML;
import javax.swing.text.html.HTML.Tag;
import javax.swing.text.html.HTMLEditorKit;

/**
 * 
 * HTML Parser callback for processing html documents that are linked from
 * http://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc.shtml and pulling the
 * tokens necessary to use in {@link GribTable}s. After processing an html
 * document use {@link #getTokenList()} to get the values to use in
 * {@link GribTable}s.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * May 06, 2016  5572     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 */
class GribTableHtmlParserCallback extends
        HTMLEditorKit.ParserCallback {

    /*
     * True if currently parsing an html table with a header that indicates
     * it is a grib table.
     */
    private boolean inTable = false;

    /* The list of all tokens processed in the table. */
    private List<String[]> tokenList = new ArrayList<>();

    /* The tokens for the current row. */
    private List<String> tokens = null;

    /*
     * While processing an individual table data this will contain the token
     * contents.
     */
    private StringBuilder token = null;

    public List<String[]> getTokenList() {
        return tokenList;
    }

    @Override
    public void handleText(char[] data, int pos) {
        if (token != null) {
            token.append(CharBuffer.wrap(data));
        }
    }

    @Override
    public void handleStartTag(Tag t, MutableAttributeSet a, int pos) {
        if (!inTable) {
            if (t.equals(HTML.Tag.TH)) {
                token = new StringBuilder();
            }
        } else if (t.equals(HTML.Tag.TR)) {
            tokens = new ArrayList<>();
        } else if (t.equals(HTML.Tag.TD)) {
            token = new StringBuilder();
        } else if (t.equals(HTML.Tag.SUP) && token != null) {
            /* Parameter units encode the exponent with sup tags. */
            token.append("^");
        }
    }

    @Override
    public void handleEndTag(Tag t, int pos) {
        if (!inTable) {
            if (t.equals(HTML.Tag.TH)) {
                String content = token.toString();
                /*
                 * Some documents contain more than one table, for example the
                 * notes might be organized in a table. In order to isolate a
                 * table that contains actual grib entries check the header for
                 * values indicating the table contains the grib numbers.
                 */
                if (content.startsWith("VALUE")
                        || content.startsWith("Number")
                        || content.startsWith("Code Figure")) {
                    inTable = true;
                }
                token = null;
            }
        } else if (t.equals(HTML.Tag.TABLE)) {
            inTable = false;
        } else if (t.equals(HTML.Tag.TR)) {
            if (tokens != null && !tokens.isEmpty()
                    && !tokens.get(2).startsWith("Reserved")) {
                tokenList.add(tokens.toArray(new String[0]));
            }
        } else if (t.equals(HTML.Tag.TD) && tokens != null) {
            String content = token.toString();
            if (tokens.isEmpty()) {
                /*
                 * The GribTableLookup expects the first two tokens to be the
                 * number, since the html only has the number once it is
                 * duplicated here.
                 */
                tokens.add(content);
            }
            tokens.add(content);
        } else if (t.equals(HTML.Tag.SUP) && token != null) {
            token.append("");
        }
    }

}