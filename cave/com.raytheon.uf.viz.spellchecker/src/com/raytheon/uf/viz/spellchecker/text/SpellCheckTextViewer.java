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
package com.raytheon.uf.viz.spellchecker.text;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.jdt.internal.ui.JavaPluginImages;
import org.eclipse.jdt.internal.ui.JavaUIMessages;
import org.eclipse.jdt.internal.ui.text.spelling.AddWordProposal;
import org.eclipse.jdt.internal.ui.text.spelling.DisableSpellCheckingProposal;
import org.eclipse.jdt.ui.PreferenceConstants;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.ITextListener;
import org.eclipse.jface.text.Region;
import org.eclipse.jface.text.TextEvent;
import org.eclipse.jface.text.TextPresentation;
import org.eclipse.jface.text.TextViewer;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyleRange;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.MenuAdapter;
import org.eclipse.swt.events.MenuEvent;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.ui.texteditor.spelling.ISpellingProblemCollector;
import org.eclipse.ui.texteditor.spelling.SpellingProblem;
import org.eclipse.ui.texteditor.spelling.SpellingService;

import com.google.common.collect.BoundType;
import com.google.common.collect.Range;
import com.google.common.collect.Ranges;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.spellchecker.jobs.EnhancedSpellCheckJob;

/**
 * A {@link TextViewer} that displays lines where text is misspelled.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 18, 2014 3453       rblum     Initial creation
 * Aug 06, 2014 3453       rblum     Refreshing all viewers on enable/
 *                                   disable of spell checking.
 * Aug 18, 2014 3453       rblum     Added the spell check dictionary
 *                                   to site level localization.
 * Oct 01, 2014 3453       rblum     Allow MB3 click anywhere in the textbox
 *                                   to enable/disable spellcheck.
 * 
 * </pre>
 * 
 * @author rblum
 * @version 1.0
 */

@SuppressWarnings("restriction")
public class SpellCheckTextViewer extends TextViewer implements
        ISpellingProblemCollector {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SpellCheckTextViewer.class);

    private final String enableSpellCheckText = "Enable spell checking";

    private List<SpellingProblem> problems;

    private IPreferenceStore store;

    private volatile boolean checkingSpelling = false;

    private static List<SpellCheckTextViewer> textViewers = new ArrayList<SpellCheckTextViewer>();

    private Map<Range<Integer>, SpellingProblem> ranges;

    private final String siteDictionary = "spellcheck/dictionary.txt";

    private LocalizationFile lf;

    /**
     * @param parent
     * @param styles
     */
    public SpellCheckTextViewer(Composite parent, int styles) {
        super(parent, styles);
        problems = new ArrayList<SpellingProblem>();
        ranges = new HashMap<Range<Integer>, SpellingProblem>();
        store = PreferenceConstants.getPreferenceStore();

        IPathManager mgr = PathManagerFactory.getPathManager();
        lf = mgr.getLocalizationFile(mgr.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.SITE),
                siteDictionary);

        // Get the localized dictionary and store it in the Preference store
        File dictionary = lf.getFile();
        String filePath = dictionary.getPath();
        store.setValue(PreferenceConstants.SPELLING_USER_DICTIONARY, filePath);

        setDocument(new Document());
        textViewers.add(this);
        ITextListener listener = new ITextListener() {
            @Override
            public void textChanged(TextEvent event) {
                if (checkingSpelling == false) {
                    scheduleSpellJob(true);
                }
            }
        };
        addTextListener(listener);
        getTextWidget().addMouseListener(new MouseAdapter() {
            @Override
            public void mouseDown(MouseEvent e) {
                int offset = -1;
                if (e.button == 3) {
                    try {
                        offset = getTextWidget().getOffsetAtLocation(
                                new Point(e.x, e.y));
                    } catch (Exception exception) {
                        /*
                         * Do Nothing - Did not click on text.
                         */
                    }
                    SpellingProblem problem = getProblemInRange(offset);
                    final Menu menu = new Menu(getTextWidget());
                    if (store
                            .getBoolean(SpellingService.PREFERENCE_SPELLING_ENABLED) == false) {
                        MenuItem item = new MenuItem(menu, SWT.PUSH);
                        item.setText(enableSpellCheckText);
                        item.setImage(JavaPluginImages
                                .get(JavaPluginImages.IMG_CORRECTION_ADD));
                        item.addSelectionListener(new SelectionAdapter() {
                            @Override
                            public void widgetSelected(SelectionEvent e) {
                                store.setValue(
                                        SpellingService.PREFERENCE_SPELLING_ENABLED,
                                        true);
                                refreshAllTextViewers(true);
                            }
                        });
                    } else if (problem != null) {
                        for (ICompletionProposal prop : problem.getProposals()) {
                            MenuItem item = new MenuItem(menu, SWT.PUSH);
                            item.setText(prop.getDisplayString());
                            item.setData(prop);
                            item.setImage(prop.getImage());
                            addSelectionAction(item);
                        }

                    } else {
                        /*
                         * Spell check is enabled but did not click on
                         * mis-spelled word - Allow disabling
                         */
                        MenuItem item = new MenuItem(menu, SWT.PUSH);
                        item.setText(JavaUIMessages.Spelling_disable_label);
                        item.setImage(JavaPluginImages
                                .get(JavaPluginImages.IMG_OBJS_NLS_NEVER_TRANSLATE));
                        item.addSelectionListener(new SelectionAdapter() {
                            @Override
                            public void widgetSelected(SelectionEvent e) {
                                store.setValue(
                                        SpellingService.PREFERENCE_SPELLING_ENABLED,
                                        false);
                                refreshAllTextViewers(false);
                            }
                        });
                    }

                    menu.addMenuListener(new MenuAdapter() {
                        @Override
                        public void menuHidden(MenuEvent e) {
                            // TODO, we might be leaking menus
                            // menu.dispose();
                        }
                    });
                    menu.setVisible(true);
                }
            }
        });
        getTextWidget().addDisposeListener(new DisposeListener() {

            @Override
            public void widgetDisposed(DisposeEvent e) {
                if (textViewers.contains(SpellCheckTextViewer.this)) {
                    textViewers.remove(SpellCheckTextViewer.this);
                }
            }
        });
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.texteditor.spelling.ISpellingProblemCollector#accept(org
     * .eclipse.ui.texteditor.spelling.SpellingProblem)
     */
    @Override
    public void accept(SpellingProblem problem) {
        problems.add(problem);
        Range<Integer> range = Ranges.range(problem.getOffset(),
                BoundType.CLOSED, problem.getOffset() + problem.getLength(),
                BoundType.CLOSED);
        ranges.put(range, problem);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.texteditor.spelling.ISpellingProblemCollector#beginCollecting
     * ()
     */
    @Override
    public void beginCollecting() {
        invalidateTextPresentation();
        problems.clear();
        ranges.clear();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.texteditor.spelling.ISpellingProblemCollector#endCollecting
     * ()
     */
    @Override
    public void endCollecting() {
        VizApp.runAsync(new Runnable() {
            public void run() {
                TextPresentation presentation = new TextPresentation();
                for (SpellingProblem problem : problems) {
                    StyleRange range = new StyleRange(problem.getOffset(),
                            problem.getLength(), Display.getCurrent()
                                    .getSystemColor(SWT.COLOR_LIST_FOREGROUND),
                            Display.getCurrent().getSystemColor(
                                    SWT.COLOR_LIST_BACKGROUND));
                    range.underline = true;
                    range.underlineColor = Display.getCurrent().getSystemColor(
                            SWT.COLOR_RED);
                    range.underlineStyle = SWT.UNDERLINE_SQUIGGLE;
                    presentation.addStyleRange(range);
                    changeTextPresentation(presentation, true);
                }
                checkingSpelling = false;
            };
        });
    }

    private SpellingProblem getProblemInRange(int offset) {
        SpellingProblem problem = null;
        for (Range<Integer> range : ranges.keySet()) {
            if (range.contains(offset)) {
                problem = ranges.get(range);
                break;
            }
        }
        return problem;
    }

    private void addSelectionAction(final MenuItem item) {
        item.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                ICompletionProposal prop = (ICompletionProposal) item.getData();
                if (prop instanceof DisableSpellCheckingProposal) {
                    store.setValue(SpellingService.PREFERENCE_SPELLING_ENABLED,
                            false);
                } else {
                    prop.apply(getDocument());

                    if (prop instanceof AddWordProposal) {
                        try {
                            if (lf != null) {
                                lf.save();
                            }
                        } catch (LocalizationOpFailedException exception) {
                            statusHandler
                                    .error("Unable to save dictionary into localization",
                                            exception);
                        }
                    }
                }
                scheduleSpellJob(true);
                refreshAllTextViewers(false);
            }
        });
    }

    /**
     * Cycles through all the SpellCheckTextViewers and issues a refresh or
     * schedules a EnhancedSpellCheckJob for each one.
     * 
     * @param issueSpellCheck
     *            Determines to schedule a spell check or to refresh.
     */
    private void refreshAllTextViewers(boolean issueSpellCheck) {
        for (SpellCheckTextViewer viewer : textViewers) {
            if (viewer.getDocument() != null) {
                if (issueSpellCheck) {
                    viewer.scheduleSpellJob(true);
                } else {
                    viewer.refresh();
                }
            }
        }
    }

    /**
     * Schedules a EnhancedSpellCheckJob for the SpellCheckTextViewer if spell
     * checking is enabled.
     * 
     * @param entireDocument
     *            Spell check the entire document or the current line.
     */
    private void scheduleSpellJob(boolean entireDocument) {
        if (store.getBoolean(SpellingService.PREFERENCE_SPELLING_ENABLED)) {
            EnhancedSpellCheckJob job = new EnhancedSpellCheckJob(
                    "Checking spelling...");
            job.setDocument(getDocument());
            if (entireDocument) {
                job.setRegion(new Region(0, getDocument().getLength()));
            } else {
                int lineIndex = getTextWidget().getLineAtOffset(
                        getTextWidget().getCaretOffset());
                int lineOffset = getTextWidget().getOffsetAtLine(lineIndex);
                int lineLength = getTextWidget().getLine(lineIndex).length();

                job.setRegion(new Region(lineOffset, lineLength));
            }

            job.setCollector(SpellCheckTextViewer.this);
            checkingSpelling = true;
            job.schedule();
            refresh();
        }
    }
}
