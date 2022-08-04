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
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.jdt.internal.ui.JavaPluginImages;
import org.eclipse.jdt.internal.ui.JavaUIMessages;
import org.eclipse.jdt.internal.ui.text.spelling.AddWordProposal;
import org.eclipse.jdt.internal.ui.text.spelling.DisableSpellCheckingProposal;
import org.eclipse.jdt.internal.ui.text.spelling.JavaSpellingProblem;
import org.eclipse.jdt.internal.ui.text.spelling.SpellCheckEngine;
import org.eclipse.jdt.internal.ui.text.spelling.engine.ISpellEvent;
import org.eclipse.jdt.internal.ui.text.spelling.engine.SpellEvent;
import org.eclipse.jdt.ui.PreferenceConstants;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.resource.ImageDescriptor;
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
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.ui.texteditor.spelling.ISpellingProblemCollector;
import org.eclipse.ui.texteditor.spelling.SpellingProblem;
import org.eclipse.ui.texteditor.spelling.SpellingService;

import com.google.common.collect.BoundType;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Range;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.spellchecker.jobs.EnhancedSpellCheckJob;
import com.raytheon.uf.viz.spellchecker.util.BlacklistedWordsUtil;
import com.raytheon.uf.viz.spellchecker.util.SpellCheckUtil;

/**
 * A {@link TextViewer} that displays lines where text is misspelled.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 18, 2014 3453       rblum       Initial creation
 * Aug 06, 2014 3453       rblum       Refreshing all viewers on enable/
 *                                     disable of spell checking.
 * Aug 18, 2014 3453       rblum       Added the spell check dictionary
 *                                     to site level localization.
 * Oct 01, 2014 3453       rblum       Allow MB3 click anywhere in the textbox
 *                                     to enable/disable spellcheck.
 * Mar 27, 2015 4138       dhladky     Guava name change.
 * Apr 14, 2015 4362       mapeters    Allow external contribution of menu items.
 * Nov 12, 2015 4834       njensen     Changed LocalizationOpFailedException to LocalizationException
 * Mar 25, 2019 7748       mapeters    Use replaceStyleRange instead of addStyleRange to prevent
 *                                     errors when doing suggested fixes
 * May 10, 2019 7747       mapeters    Flag/filter inappropriate words (copied from Hazard Services,
 *                                     with some constants extracted out to {@link SpellCheckUtil})
 * Feb 04, 2020 7998       drogalla    Use the same spellcheck dictionary as GFE and text workstation
 *
 * </pre>
 *
 * @author rblum
 */
@SuppressWarnings("restriction")
public class SpellCheckTextViewer extends TextViewer
        implements ISpellingProblemCollector {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SpellCheckTextViewer.class);

    protected static final String ENABLE_SPELL_CHECK_TEXT = "Enable spell checking";

    protected static final String SITE_DICTIONARY = "spellchecker"
            + IPathManager.SEPARATOR + "spelldict.txt";

    /**
     * Patterns to match proposals' display strings against and capture the word
     * that they are for, so that we can remove proposals for blacklisted words.
     */
    protected static final Set<Pattern> PROPOSAL_PATTERNS = ImmutableSet.of(
            SpellCheckUtil.IGNORE_PATTERN, SpellCheckUtil.ADD_TO_PATTERN,
            SpellCheckUtil.CHANGE_TO_PATTERN);

    protected static List<SpellCheckTextViewer> textViewers = new ArrayList<>();

    protected List<SpellingProblem> problems;

    protected IPreferenceStore store;

    protected volatile boolean checkingSpelling = false;

    protected Map<Range<Integer>, SpellingProblem> ranges;

    protected LocalizationFile lf;

    protected IAction[] menuItems;

    /**
     * Create a text viewer that spell checks.
     *
     * @param parent
     *            the parent of the viewer's control
     * @param styles
     *            the SWT style bits for the viewer's control
     */
    public SpellCheckTextViewer(Composite parent, int styles) {
        super(parent, styles);
        problems = new ArrayList<>();
        ranges = new HashMap<>();
        store = PreferenceConstants.getPreferenceStore();

        IPathManager mgr = PathManagerFactory.getPathManager();
        lf = mgr.getLocalizationFile(
                mgr.getContext(LocalizationType.CAVE_STATIC,
                        LocalizationLevel.SITE),
                SITE_DICTIONARY);

        // Get the localized dictionary and store it in the Preference store
        File dictionary = lf.getFile();
        String filePath = dictionary.getPath();
        store.setValue(PreferenceConstants.SPELLING_USER_DICTIONARY, filePath);

        setDocument(new Document());
        textViewers.add(this);
        ITextListener listener = new ITextListener() {
            @Override
            public void textChanged(TextEvent event) {
                if (!checkingSpelling) {
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
                        offset = getTextWidget()
                                .getOffsetAtLocation(new Point(e.x, e.y));
                    } catch (Exception exception) {
                        /*
                         * Do Nothing - Did not click on text.
                         */
                    }
                    SpellingProblem problem = getProblemInRange(offset);

                    MenuManager menuMgr = new MenuManager();
                    if (menuItems != null) {
                        for (IAction action : menuItems) {
                            menuMgr.add(action);
                        }
                        menuMgr.add(new Separator());
                    }

                    if (!store.getBoolean(
                            SpellingService.PREFERENCE_SPELLING_ENABLED)) {
                        menuMgr.add(new EnableDisableSpellCheckAction(true));

                    } else if (problem != null) {
                        ICompletionProposal[] proposals = filterProposals(
                                problem);
                        for (ICompletionProposal prop : proposals) {
                            menuMgr.add(new SpellCheckProposalAction(prop));
                        }
                    } else {
                        /*
                         * Spell check is enabled but did not click on
                         * mis-spelled word - Allow disabling
                         */
                        menuMgr.add(new EnableDisableSpellCheckAction(false));
                    }

                    Menu menu = menuMgr.createContextMenu(getTextWidget());
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

    @Override
    public void accept(SpellingProblem problem) {
        problems.add(problem);
        addRange(problem);
    }

    /**
     * Get the proposals for the given spelling problem, filtering out any that
     * we don't want due to the blacklisted words. We don't want blacklisted
     * words to be added to the dictionary, ignored, or suggested as fixes.
     *
     * @param problem
     *            the spelling problem to filter proposals for
     * @return the filtered proposals
     */
    protected ICompletionProposal[] filterProposals(SpellingProblem problem) {
        ICompletionProposal[] proposals = problem.getProposals();
        if (proposals != null && proposals.length > 0) {
            List<ICompletionProposal> proposalsList = new ArrayList<>(
                    Arrays.asList(proposals));
            Iterator<ICompletionProposal> proposalsItr = proposalsList
                    .iterator();
            while (proposalsItr.hasNext()) {
                ICompletionProposal proposal = proposalsItr.next();
                String displayString = proposal.getDisplayString();

                /*
                 * Remove this proposal if it is for a blacklisted word
                 */
                for (Pattern pattern : PROPOSAL_PATTERNS) {
                    Matcher match = pattern.matcher(displayString);
                    if (match.matches()) {
                        String word = match.group(1);
                        if (BlacklistedWordsUtil.isBlacklisted(word)) {
                            proposalsItr.remove();
                        }
                        break;
                    }
                }
            }

            if (proposalsList.size() < proposals.length) {
                proposals = proposalsList.toArray(new ICompletionProposal[0]);
            }
        }
        return proposals;
    }

    @Override
    public void beginCollecting() {
        invalidateTextPresentation();
        problems.clear();
        ranges.clear();
    }

    @Override
    public void endCollecting() {
        /*
         * Some blacklisted words may be in the built-in dictionary, so find any
         * blacklisted words that weren't flagged, and flag them.
         */
        Matcher matcher = SpellCheckUtil.WORD_PATTERN
                .matcher(getDocument().get());
        while (matcher.find()) {
            String currentWord = matcher.group();
            if (BlacklistedWordsUtil.isBlacklisted(currentWord)) {
                int offset = matcher.start();

                /*
                 * Determine where to insert the spelling problem for this
                 * blacklisted word, to keep the problems in the order they
                 * appear in the text field
                 */
                int insertIndex = problems.size();
                for (int i = 0; i < problems.size(); ++i) {
                    int problemOffset = problems.get(i).getOffset();
                    if (problemOffset == offset) {
                        // Already flagged, don't insert
                        insertIndex = -1;
                        break;
                    } else if (problemOffset > offset) {
                        insertIndex = i;
                        break;
                    }
                }
                if (insertIndex >= 0) {
                    ISpellEvent spellEvent = new CustomSpellEvent(currentWord,
                            offset);
                    JavaSpellingProblem problem = new JavaSpellingProblem(
                            spellEvent, getDocument());
                    problems.add(insertIndex, problem);
                    addRange(problem);
                }
            }
        }

        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {
                TextPresentation presentation = new TextPresentation();
                int textFieldLength = getDocument().getLength();
                for (SpellingProblem problem : problems) {
                    int offset = problem.getOffset();
                    int length = problem.getLength();
                    if (offset + length > textFieldLength) {
                        /*
                         * When deleting characters from the end of the text
                         * field, the spell checking may lag behind and try to
                         * claim there's a spelling error past the end of the
                         * text, which causes errors
                         */
                        break;
                    }
                    StyleRange range = new StyleRange(problem.getOffset(),
                            problem.getLength(),
                            Display.getCurrent()
                                    .getSystemColor(SWT.COLOR_LIST_FOREGROUND),
                            Display.getCurrent()
                                    .getSystemColor(SWT.COLOR_LIST_BACKGROUND));
                    range.underline = true;
                    range.underlineColor = Display.getCurrent()
                            .getSystemColor(SWT.COLOR_RED);
                    range.underlineStyle = SWT.UNDERLINE_SQUIGGLE;
                    presentation.replaceStyleRange(range);
                }
                changeTextPresentation(presentation, true);
                checkingSpelling = false;
            }
        });
    }

    /**
     * Add an entry to the ranges map for the given spelling problem.
     *
     * @param problem
     *            the problem to add to the ranges map
     */
    protected void addRange(SpellingProblem problem) {
        Range<Integer> range = Range.range(problem.getOffset(),
                BoundType.CLOSED, problem.getOffset() + problem.getLength(),
                BoundType.CLOSED);
        ranges.put(range, problem);
    }

    /**
     * Get the spelling problem at the given position in the text field, if any.
     *
     * @param offset
     *            the position in the text field
     * @return the spelling problem, or null if none
     */
    protected SpellingProblem getProblemInRange(int offset) {
        SpellingProblem problem = null;
        for (Range<Integer> range : ranges.keySet()) {
            if (range.contains(offset)) {
                problem = ranges.get(range);
                break;
            }
        }
        return problem;
    }

    /**
     * Cycles through all the SpellCheckTextViewers and issues a refresh or
     * schedules a EnhancedSpellCheckJob for each one.
     *
     * @param issueSpellCheck
     *            Determines to schedule a spell check or to refresh.
     */
    protected void refreshAllTextViewers(boolean issueSpellCheck) {
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
    protected void scheduleSpellJob(boolean entireDocument) {
        if (store.getBoolean(SpellingService.PREFERENCE_SPELLING_ENABLED)) {
            EnhancedSpellCheckJob job = new EnhancedSpellCheckJob(
                    "Checking spelling...");
            job.setDocument(getDocument());
            if (entireDocument) {
                job.setRegion(new Region(0, getDocument().getLength()));
            } else {
                int lineIndex = getTextWidget()
                        .getLineAtOffset(getTextWidget().getCaretOffset());
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

    /**
     * Add menu items to the context menu.
     *
     * @param menuItems
     *            the menu items to add
     */
    public void addMenuItems(IAction[] menuItems) {
        this.menuItems = menuItems;
    }

    protected class EnableDisableSpellCheckAction extends Action {

        private boolean enable;

        protected EnableDisableSpellCheckAction(boolean enable) {
            String text = enable ? ENABLE_SPELL_CHECK_TEXT
                    : JavaUIMessages.Spelling_disable_label;
            String imageName = enable ? JavaPluginImages.IMG_CORRECTION_ADD
                    : JavaPluginImages.IMG_OBJS_NLS_NEVER_TRANSLATE;
            setText(text);
            setImageDescriptor(ImageDescriptor
                    .createFromImage(JavaPluginImages.get(imageName)));
            this.enable = enable;
        }

        @Override
        public void run() {
            store.setValue(SpellingService.PREFERENCE_SPELLING_ENABLED, enable);
            refreshAllTextViewers(enable);
        }
    }

    protected class SpellCheckProposalAction extends Action {

        private ICompletionProposal proposal;

        protected SpellCheckProposalAction(ICompletionProposal proposal) {
            setText(proposal.getDisplayString());
            setImageDescriptor(
                    ImageDescriptor.createFromImage(proposal.getImage()));
            this.proposal = proposal;
        }

        @Override
        public void run() {
            if (proposal instanceof DisableSpellCheckingProposal) {
                store.setValue(SpellingService.PREFERENCE_SPELLING_ENABLED,
                        false);
            } else {
                proposal.apply(getDocument());

                if (proposal instanceof AddWordProposal) {
                    try {
                        if (lf != null) {
                            lf.save();
                        }
                    } catch (LocalizationException exception) {
                        statusHandler.error(
                                "Unable to save dictionary into localization",
                                exception);
                    }
                }
            }
            scheduleSpellJob(true);
            refreshAllTextViewers(false);
        }
    }

    /*
     * This class only exists because the superclass' constructor is protected,
     * so this allows us to access it.
     */
    protected static class CustomSpellEvent extends SpellEvent {

        public CustomSpellEvent(String word, int begin) {
            super(SpellCheckEngine.getInstance().getSpellChecker(), word, begin,
                    begin + word.length() - 1, false, false);
        }
    }
}
