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
 * 
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 19, 2011            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 *
 */
package com.raytheon.uf.viz.productbrowser.plugins;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import net.sf.swtaddons.autocomplete.AutocompleteContentProposalProvider;
import net.sf.swtaddons.autocomplete.text.AutocompleteTextSelector;

import org.eclipse.jface.fieldassist.IContentProposal;
import org.eclipse.swt.widgets.Text;

public class ProductBrowserTextSelector extends AutocompleteTextSelector {
    /**
     * @param text
     * @param selectionItems
     */
    public ProductBrowserTextSelector(Text text, String[] selectionItems) {
        super(text, selectionItems);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * net.sf.swtaddons.autocomplete.AutocompleteWidget#getAutoactivationChars()
     */
    @Override
    protected char[] getAutoactivationChars() {
        // List<Character> activationChars = new ArrayList<Character>();
        char[] chars = super.getAutoactivationChars();
        // for (int i = 0; i < chars.length; i++) {
        // activationChars.add(chars[i]);
        // }
        // activationChars.add('\b');
        // chars = new char[activationChars.size()];
        // for (int i = 0; i < chars.length; i++) {
        // chars[i] = activationChars.get(i);
        // }
        return chars;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * net.sf.swtaddons.autocomplete.AutocompleteWidget#getContentProposalProvider
     * (java.lang.String[])
     */
    @Override
    protected AutocompleteContentProposalProvider getContentProposalProvider(
            String[] proposals) {
        ProductBrowserHistoryProvider historyProvider = new ProductBrowserHistoryProvider(
                proposals);
        Arrays.sort(proposals);
        historyProvider.setProposals(proposals);
        return historyProvider;
    }

    class ProductBrowserHistoryProvider extends
            AutocompleteContentProposalProvider {

        /**
         * @param proposals
         */
        public ProductBrowserHistoryProvider(String[] proposals) {
            super(proposals);
        }

        /*
         * (non-Javadoc)
         * 
         * @see
         * net.sf.swtaddons.autocomplete.AutocompleteContentProposalProvider
         * #getMatchingProposals(java.lang.String[], java.lang.String)
         */
        @Override
        protected List<?> getMatchingProposals(String[] proposals,
                String contents) {
            List<IContentProposal> contentProposals = new ArrayList<IContentProposal>();
            String[] matchingProposals = matches(proposals, contents);

            for (int i = 0; i < matchingProposals.length; i++) {
                final String proposal = matchingProposals[i];
                contentProposals.add(new IContentProposal() {
                    public String getContent() {
                        return proposal;
                    }

                    public String getDescription() {
                        return null;
                    }

                    public String getLabel() {
                        return null;
                    }

                    public int getCursorPosition() {
                        return proposal.length();
                    }
                });
            }
            return contentProposals;
        }

        /**
         * Returns an array of Strings within the input array that match the
         * input test string
         * 
         * @param items
         *            the String array of possible completions
         * @param prefix
         *            the incomplete String to try and match
         * @return the array of possible completions to the input string
         */
        private String[] matches(String[] items, String prefix) {
            List<String> matches = new ArrayList<String>();
            for (int i = 0; i < items.length; ++i) {
                if (items[i].toLowerCase().contains(prefix.toLowerCase())) {
                    matches.add(items[i]);
                }
            }
            return (String[]) matches.toArray(new String[matches.size()]);
        }

    }
}
