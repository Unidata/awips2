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
package com.raytheon.uf.viz.collaboration.ui;

import java.text.BreakIterator;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.ui.dialogs.PatternFilter;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 13, 2012            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class UsersTreeFilter extends PatternFilter {
    /**
 * 
 */
    public UsersTreeFilter() {
        // TODO Auto-generated constructor stub
        setIncludeLeadingWildcard(true);
    }

    @Override
    public void setPattern(String patternString) {
        super.setPattern(patternString);
        // String[] words = getWords(patternString);
        // StringBuilder builder = new StringBuilder();
        // for (String word : words) {
        // builder.append("(?=").append(word).append(")");
        // }
        // super.setPattern(builder.toString());
    }

    /**
     * Taken from PatternFilter
     * 
     * @param text
     * @return
     */
    private String[] getWords(String text) {
        List words = new ArrayList();
        // Break the text up into words, separating based on whitespace and
        // common punctuation.
        // Previously used String.split(..., "\\W"), where "\W" is a regular
        // expression (see the Javadoc for class Pattern).
        // Need to avoid both String.split and regular expressions, in order to
        // compile against JCL Foundation (bug 80053).
        // Also need to do this in an NL-sensitive way. The use of BreakIterator
        // was suggested in bug 90579.
        BreakIterator iter = BreakIterator.getWordInstance();
        iter.setText(text);
        int i = iter.first();
        while (i != java.text.BreakIterator.DONE && i < text.length()) {
            int j = iter.following(i);
            if (j == java.text.BreakIterator.DONE) {
                j = text.length();
            }
            // match the word
            if (Character.isLetterOrDigit(text.charAt(i))) {
                String word = text.substring(i, j);
                words.add(word);
            }
            i = j;
        }
        return (String[]) words.toArray(new String[words.size()]);
    }
}