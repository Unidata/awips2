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
import java.util.List;
import java.util.ListIterator;
import java.util.regex.MatchResult;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
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
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */

public class SearchComposite extends Composite {

    // final Color HIGHLIGHT_COLOR = Display.getCurrent().getSystemColor(
    // SWT.COLOR_YELLOW);

    private StringBuilder text;

    private Button caseSensitive;

    private boolean searchFWD = true;

    private String searchText;

    private List<MatchResult> results = new ArrayList<MatchResult>();

    private ListIterator<MatchResult> resultIter;

    private SearchView searchView;

    private KeyListener searchKeyListener;

    private Text searchTextBox;

    private Button bck;

    private Button fwd;

    private Button first;

    private Button last;

    // public static class HighlightSelection {
    // public final int start;
    //
    // public final int length;
    //
    // public HighlightSelection(int start, int length) {
    // this.start = start;
    // this.length = length;
    // }
    // }

    public static interface SearchView {

        void select(int start, int end);

        void reachedLast();

        void reachedFirst();

        // void setHighlights(HighlightSelection[] selections);
        // void removeHighlights();

    }

    public class DefaultSearchView implements SearchView {
        private StyledText searchView;

        public DefaultSearchView(StyledText searchView) {
            this.searchView = searchView;
        }

        @Override
        public void select(int start, int end) {
            this.searchView.setSelectionRange(start, end - start);
        }

        @Override
        public void reachedLast() {
            // go to first
            while (resultIter.hasPrevious()) {
                resultIter.previous();
            }
        }

        @Override
        public void reachedFirst() {
            // go to last
            while (resultIter.hasNext()) {
                resultIter.next();
            }
        }

        // @Override
        // public void setHighlights(HighlightSelection[] selections) {
        // StyleRange[] ranges = new StyleRange[selections.length];
        // for (int i = 0; i < selections.length; ++i) {
        // HighlightSelection sel = selections[i];
        // ranges[i] = new StyleRange(sel.start, sel.length,
        // FOREGROUND_COLOR, HIGHLIGHT_COLOR);
        // }
        // searchView.setStyleRanges(ranges);
        // }
        //
        // @Override
        // public void removeHighlights() {
        // StyleRange[] ranges = searchView.getStyleRanges();
        // for (StyleRange style : ranges) {
        // if (style.background == HIGHLIGHT_COLOR) {
        // style.background = searchView.getBackground();
        // }
        // }
        // searchView.setStyleRanges(ranges);
        // }
    }

    /**
     * @param parent
     * @param style
     */
    public SearchComposite(Composite parent, int style) {
        super(parent, style);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, false);
        this.setLayout(new GridLayout(6, false));
        this.setLayoutData(gd);

        searchTextBox = new Text(this, SWT.RESIZE);
        searchTextBox.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false,
                false));
        gd = new GridData(SWT.FILL, SWT.FILL, true, false);
        searchTextBox.setLayoutData(gd);
        searchTextBox.addKeyListener(new KeyListener() {
            @Override
            public void keyReleased(KeyEvent e) {
            }

            @Override
            public void keyPressed(KeyEvent e) {
            }
        });

        first = new Button(this, SWT.PUSH);
        first.setText("<<");
        first.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false));
        first.addSelectionListener(new SelectionListener() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                searchFWD = false;
                MatchResult result = null;
                while (resultIter.hasPrevious()) {
                    result = resultIter.previous();
                }
                if (result != null) {
                    searchView.select(result.start(), result.end());
                }
                if (result == null && resultIter.hasNext()) {
                    result = resultIter.next();
                }
            }

            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
            }
        });

        bck = new Button(this, SWT.PUSH);
        bck.setText("<");
        bck.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false));
        bck.addSelectionListener(new SelectionListener() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                searchFWD = false;
                nextSearchMatch();
            }

            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
            }
        });

        fwd = new Button(this, SWT.PUSH);
        fwd.setText(">");
        fwd.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false));
        fwd.addSelectionListener(new SelectionListener() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                searchFWD = true;
                nextSearchMatch();
            }

            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
            }
        });

        last = new Button(this, SWT.PUSH);
        last.setText(">>");
        last.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false));
        last.addSelectionListener(new SelectionListener() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                searchFWD = true;
                MatchResult result = null;
                while (resultIter.hasNext()) {
                    result = resultIter.next();
                }
                if (result == null && resultIter.hasPrevious()) {
                    result = resultIter.previous();
                }
                if (result != null) {
                    searchView.select(result.start(), result.end());
                }
            }

            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
            }
        });

        caseSensitive = new Button(this, SWT.CHECK);
        caseSensitive.setText("Case");
        caseSensitive.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false,
                false));

        searchKeyListener = new KeyListener() {
            @Override
            public void keyReleased(KeyEvent e) {
            }

            @Override
            public void keyPressed(KeyEvent e) {
                if (search(e)) {
                } else if (e.keyCode == SWT.KEYPAD_CR || e.keyCode == SWT.CR) {
                    if (searchText == null
                            || !searchText.equals(searchTextBox.getText())) {
                        searchText = searchTextBox.getText();
                        updateMatches();
                    }
                    if (searchFWD) {
                        fwd.setFocus();
                    } else {
                        bck.setFocus();
                    }
                    nextSearchMatch();
                } else {
                    searchTextBox.setFocus();
                }
            }
        };

        this.addKeyListener(searchKeyListener);
        searchTextBox.addKeyListener(searchKeyListener);
        bck.addKeyListener(searchKeyListener);
        fwd.addKeyListener(searchKeyListener);
        caseSensitive.addKeyListener(searchKeyListener);

        caseSensitive.addSelectionListener(new SelectionListener() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                updateMatches();
            }

            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
                updateMatches();
            }
        });
    }

    public boolean search(KeyEvent e) {
        boolean result = false;
        if (((e.stateMask & SWT.CTRL) == SWT.CTRL)) {
            if (e.keyCode == 'f' || e.keyCode == 'b' || e.keyCode == 'B') {
                searchFWD = e.keyCode == 'f' ? true : false;
                hide(false);
                // redraw();
                searchTextBox.setFocus();
                if (searchFWD) {
                    fwd.setFocus();
                } else {
                    bck.setFocus();
                }
                if (text == null) {
                    reachedLimit();
                }
                if (searchText == null
                        || !searchText.equals(searchTextBox.getText())) {
                    searchText = searchTextBox.getText();
                    updateMatches();
                }
                nextSearchMatch();
                result = true;
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
        // if (searchView != null) {
        // searchView.removeHighlights();
        // }
        getParent().layout();
    }

    /**
     * increment/decrement to next match. if at end of matches, increment to
     * next/previous leaf.
     * 
     * @param tree2
     */
    protected void nextSearchMatch() {
        boolean hasNextInSequence;

        if (resultIter == null) {
            hasNextInSequence = false;
        } else {
            hasNextInSequence = searchFWD ? resultIter.hasNext() : resultIter
                    .hasPrevious();
        }
        if (hasNextInSequence) {
            MatchResult current = searchFWD ? resultIter.next() : resultIter
                    .previous();
            searchView.select(current.start(), current.end());
        } else {
            reachedLimit();
        }
    }

    private void updateMatches() {
        int patternFlags = caseSensitive.getSelection() ? 0
                : Pattern.CASE_INSENSITIVE;

        results.clear();

        if (text == null) {
            reachedLimit();
        }

        if (searchText != null) {
            Pattern pattern = Pattern.compile(searchText, patternFlags);
            Matcher matcher = pattern.matcher(text);

            while (matcher.find()) {
                results.add(matcher.toMatchResult());
            }

            // HighlightSelection[] highlights = new HighlightSelection[results
            // .size()];
            // for (ListIterator<MatchResult> iter = results.listIterator();
            // iter
            // .hasNext();) {
            // int index = iter.nextIndex();
            // MatchResult mr = iter.next();
            // highlights[index] = new HighlightSelection(mr.start(), mr.end()
            // - mr.start());
            // }
            // searchView.setHighlights(highlights);

            int index = searchFWD ? 0 : results.size();
            resultIter = results.listIterator(index);
        }
    }

    private void reachedLimit() {
        if (searchFWD) {
            searchView.reachedLast();
        } else {
            searchView.reachedFirst();
        }
    }

    /**
     * @param searchView
     *            the SearchView to set
     */
    public void setSearchView(SearchView searchView) {
        this.searchView = searchView;
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
    public void setText(String text) {
        if (this.text == null) {
            this.text = new StringBuilder();
        }
        this.text.setLength(0);
        this.text.append(text);
        updateMatches();
    }

    /**
     * @return the searchKeyListener
     */
    public KeyListener getSearchKeyListener() {
        return searchKeyListener;
    }

    public void setDefaultSearchView(StyledText styledText) {
        setText(styledText.getText());
        setSearchView(new DefaultSearchView(styledText));
    }

    public void appendText(String newText) {
        this.text.append(newText);
        updateMatches();
    }

}
