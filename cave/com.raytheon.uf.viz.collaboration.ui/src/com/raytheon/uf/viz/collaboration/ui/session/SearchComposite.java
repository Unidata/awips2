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
package com.raytheon.uf.viz.collaboration.ui.session;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.regex.MatchResult;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyleRange;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Text;

/**
 * Simple search composite control for a text based composite..
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 31, 2012            bgonzale     Initial creation
 * Mar 31, 2014 2937       bgonzale     Fix error where text was continually appended
 *                                      during search.
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */

public class SearchComposite extends Composite {

    final Color HIGHLIGHT_COLOR = Display.getCurrent().getSystemColor(
            SWT.COLOR_YELLOW);

    private StringBuilder text;

    private Button caseSensitive;

    private String textToSearch;

    private List<MatchResult> results;

    private int index = -1;

    private ISearchText searchText;

    private KeyListener searchKeyListener;

    private Text searchBox;

    private Button bck;

    private Button fwd;

    private List<StyleRange> originalStyles;

    // need this to update matches even when the user is not currently on the
    // chat tab
    private boolean visible;

    public class DefaultSearchText implements ISearchText {
        private StyledText searchView;

        public DefaultSearchText(StyledText searchView) {
            this.searchView = searchView;
        }

        @Override
        public void select(int start, int end) {
            this.searchView.setSelectionRange(start, end - start);
        }

        @Override
        public void reachedLast() {
            // go to first
            if (index == results.size()) {
                index = 0;
            }
        }

        @Override
        public void reachedFirst() {
            // go to last
            while (index == 0) {
                index = results.size() - 1;
            }
        }

        @Override
        public StyledText getStyledText() {
            return searchView;
        }

    }

    private static final Comparator<? super StyleRange> startComparator = new Comparator<StyleRange>() {
        /*
         * return less than one; zero; and greater than one if o2 start is
         * before; equal to; or after o1 start.
         */
        @Override
        public int compare(StyleRange o1, StyleRange o2) {
            return o1.start - o2.start;
        }
    };

    /**
     * @param parent
     * @param style
     */
    public SearchComposite(Composite parent, int style) {
        super(parent, style);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, false);
        this.setLayout(new GridLayout(1, false));
        this.setLayoutData(gd);

        results = new ArrayList<MatchResult>();
        originalStyles = new ArrayList<StyleRange>();
        searchBox = new Text(this, SWT.RESIZE | SWT.BORDER);
        searchBox
                .setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false));
        gd = new GridData(SWT.FILL, SWT.FILL, true, false);
        searchBox.setLayoutData(gd);

        Composite searchControls = new Composite(this, SWT.NONE);
        searchControls.setLayout(new GridLayout(5, false));
        setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, false));

        Button first = new Button(searchControls, SWT.PUSH);
        first.setText("<<");
        first.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false));
        first.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                index = 0;
                doSearch();
            }
        });

        bck = new Button(searchControls, SWT.PUSH);
        bck.setText("<");
        bck.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false));
        bck.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                decrementIndex();
                doSearch();
            }
        });

        fwd = new Button(searchControls, SWT.PUSH);
        fwd.setText(">");
        fwd.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false));
        fwd.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                incrementIndex();
                doSearch();
            }
        });

        Button last = new Button(searchControls, SWT.PUSH);
        last.setText(">>");
        last.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false));
        last.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                index = results.size() - 1;
                doSearch();
            }
        });

        caseSensitive = new Button(searchControls, SWT.CHECK);
        caseSensitive.setText("Case");
        caseSensitive.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false,
                false));

        searchKeyListener = new KeyListener() {

            private boolean shiftPressed = false;

            /**
             * Check for search "text" inputs.
             * 
             * @see org.eclipse.swt.events.KeyListener#keyReleased(org.eclipse.swt.events.KeyEvent)
             */
            @Override
            public void keyReleased(KeyEvent e) {
                if (textToSearch == null
                        || textToSearch.equals(searchBox.getText()) == false) {
                    textToSearch = searchBox.getText();
                    updateMatches();
                }

                if (e.keyCode == SWT.SHIFT) {
                    shiftPressed = false;
                }
            }

            /**
             * Check for search "movement" commands.
             * 
             * @see org.eclipse.swt.events.KeyListener#keyPressed(org.eclipse.swt.events.KeyEvent)
             */
            @Override
            public void keyPressed(KeyEvent e) {
                if (e.keyCode == SWT.KEYPAD_CR || e.keyCode == SWT.CR) {
                    if (index < 0) {
                        index = 0;
                    } else {
                        incrementIndex();
                    }
                    nextSearchMatch();
                } else if (e.keyCode == SWT.SHIFT) {
                    shiftPressed = true;
                } else {
                    searchBox.setFocus();
                }
            }
        };

        this.addKeyListener(searchKeyListener);
        searchBox.addKeyListener(searchKeyListener);
        bck.addKeyListener(searchKeyListener);
        fwd.addKeyListener(searchKeyListener);
        caseSensitive.addKeyListener(searchKeyListener);

        caseSensitive.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                updateMatches();
            }
        });
    }

    private void incrementIndex() {
        index++;
        if (index >= results.size()) {
            index = 0;
        }
    }

    private void decrementIndex() {
        index--;
        if (index < 0) {
            index = results.size() - 1;
        }
    }

    private void doSearch() {
        if (textToSearch == null
                || textToSearch.equals(searchBox.getText()) == false) {
            textToSearch = searchBox.getText();
            updateMatches();
        }
        nextSearchMatch();
    }

    public boolean search(KeyEvent e) {
        boolean result = false;
        if (((e.stateMask & SWT.CTRL) == SWT.CTRL)) {
            if (e.keyCode == 'f') {
                hide(false);
                updateMatches();
                searchBox.setFocus();
            }
        } else if (e.keyCode == SWT.ESC) {
            hide(true);
            redraw();
            result = true;
        }
        return result;
    }

    public void hide(boolean isHidden) {
        ((GridData) getLayoutData()).exclude = isHidden;
        setVisible(!isHidden);
        visible = !isHidden;
        if (searchText != null) {
            removeHighlights();
        }
        getParent().layout();
    }

    private void removeHighlights() {
        searchText.getStyledText().setStyleRanges(
                originalStyles.toArray(new StyleRange[0]));
    }

    /**
     * increment/decrement to next match. if at end of matches, increment to
     * next/previous leaf.
     * 
     * @param tree2
     */
    protected void nextSearchMatch() {
        if (results == null || results.isEmpty()) {
            return;
        }
        MatchResult current = results.get(index);
        searchText.select(current.start(), current.end());
    }

    private void updateMatches() {
        int patternFlags = Pattern.LITERAL
                | (caseSensitive.getSelection() ? 0 : Pattern.CASE_INSENSITIVE);

        results.clear();

        if (textToSearch != null && textToSearch.isEmpty() == false) {
            Pattern pattern = Pattern.compile(textToSearch, patternFlags);
            Matcher matcher = pattern.matcher(text);

            while (matcher.find()) {
                results.add(matcher.toMatchResult());
            }
        }

        removeHighlights();
        if (results.size() > 0) {
            List<StyleRange> highlights = new ArrayList<StyleRange>();

            highlights.addAll(Arrays.asList(searchText.getStyledText()
                    .getStyleRanges()));
            StyledText text = searchText.getStyledText();
            // for each match
            for (MatchResult result : results) {
                // grab the style ranges within that match
                StyleRange[] ranges = text.getStyleRanges(result.start(),
                        result.end() - result.start());
                // create a single huge range for the entire match that is
                // highlighted
                StyleRange range = new StyleRange(result.start(), result.end()
                        - result.start(), null, HIGHLIGHT_COLOR);
                List<StyleRange> finalRanges = new ArrayList<StyleRange>();
                finalRanges.add(range);
                // loop through the style ranges
                for (StyleRange rng : ranges) {
                    // add a style range according to each one, that is
                    // highlighted
                    if (finalRanges.get(0).start == rng.start
                            && finalRanges.get(0).length == rng.length) {
                        finalRanges.remove(0);
                    }
                    finalRanges.add(new StyleRange(rng.start, rng.length,
                            rng.foreground, HIGHLIGHT_COLOR, rng.fontStyle));
                }
                text.replaceStyleRanges(result.start(),
                        result.end() - result.start(),
                        finalRanges.toArray(new StyleRange[0]));
            }
        }
    }

    private void setOriginalStyles(StyledText styledText) {
        originalStyles.clear();
        originalStyles.addAll(Arrays.asList(styledText.getStyleRanges()));
    }

    /**
     * @param searchText
     *            the SearchText to set
     */
    public void setSearchText(ISearchText searchText) {
        this.searchText = searchText;
        StyledText styledText = searchText.getStyledText();
        setOriginalStyles(styledText);
        if (isVisible()) {
            updateMatches();
        }
    }

    /**
     * @param styledText
     */
    public void setSearchText(StyledText styledText) {
        setText(styledText.getText());
        setSearchText(new DefaultSearchText(styledText));
    }

    /**
     * @return the text
     */
    public String getText() {
        return text.toString();
    }

    /**
     * @param text
     *            the text to set
     */
    private void setText(String text) {
        this.text = new StringBuilder(text);
    }

    /**
     * @return the searchKeyListener
     */
    public KeyListener getSearchKeyListener() {
        return searchKeyListener;
    }

    public void appendText(String newText) {
        originalStyles.addAll(Arrays.asList(searchText.getStyledText()
                .getStyleRanges(this.text.length(), newText.length())));
        this.text.append(newText);
        if (visible) {
            updateMatches();
        }
    }

    public void toggleVisibility() {
        hide(isVisible());
        if (isVisible()) {
            updateMatches();
        }
    }

}
