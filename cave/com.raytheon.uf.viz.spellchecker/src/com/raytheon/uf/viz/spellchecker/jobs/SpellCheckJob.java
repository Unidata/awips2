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
package com.raytheon.uf.viz.spellchecker.jobs;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Deque;
import java.util.Iterator;
import java.util.List;
import java.util.regex.Matcher;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.content.IContentType;
import org.eclipse.core.runtime.content.IContentTypeManager;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.preference.PreferenceStore;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.Region;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.texteditor.spelling.ISpellingProblemCollector;
import org.eclipse.ui.texteditor.spelling.SpellingContext;
import org.eclipse.ui.texteditor.spelling.SpellingProblem;
import org.eclipse.ui.texteditor.spelling.SpellingService;

import com.raytheon.uf.viz.spellchecker.Activator;
import com.raytheon.uf.viz.spellchecker.util.BlacklistedWordsUtil;
import com.raytheon.uf.viz.spellchecker.util.SpellCheckUtil;

/**
 * Spell Checker Job.
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 01Mar2010    4765       MW Fegan    Moved from GFE plug-in.
 * 18Oct2010    11237      rferrel     Created readLine in order to compute
 *                                     offsets for start of line correctly.
 * 25May2016    DR16930    MPorricelli Added flagging of words that are in
 *                                     inappropriateWords.txt blacklist
 * 10May2019    7747       mapeters    Use utility classes, cleanup
 * </pre>
 *
 * @author wldougher
 */
public class SpellCheckJob extends Job implements ISpellingProblemCollector {

    private static final IContentType TEXT_CONTENT_TYPE = Platform
            .getContentTypeManager()
            .getContentType(IContentTypeManager.CT_TEXT);

    private String text;

    private SpellingService service;

    private final Deque<SpellingProblem> problems;

    private BufferedReader reader;

    private int offset;

    private IDocument document;

    private IRegion[] regions;

    private SpellingContext context;

    private ISpellingProblemCollector collector;

    private int separatorLen = 0;

    private String line;

    private String vtecRegex = new String(
            "/[OTEX]\\.([A-Z]{3})\\.([A-Z]{4})\\.([A-Z]{2})\\."
                    + "([WAYSOFN])\\.([0-9]{4})\\.([0-9]{6})T([0-9]{4})Z-"
                    + "([0-9]{6})T([0-9]{4})Z/");

    /**
     * A class for sending SpellingProblems to the caller.
     *
     * @author wldougher
     *
     */
    private class ProblemSender implements Runnable {

        private final SpellingProblem problem;

        ProblemSender(SpellingProblem problem) {
            this.problem = problem;
        }

        @Override
        public void run() {
            collector.accept(problem);
        }
    }

    /**
     * Constructor.
     *
     * @param name
     *            The name of the job.
     */
    public SpellCheckJob(String name) {
        super(name);
        text = "";
        problems = new ArrayDeque<>();
        document = new Document();
        regions = new IRegion[1];
        reader = new BufferedReader(new StringReader(text));
        context = new SpellingContext();
        context.setContentType(TEXT_CONTENT_TYPE);
    }

    /**
     * Set the text the spell check job is supposed to check. The offset is set
     * to zero and any old spelling problems are removed.
     *
     * @param text
     */
    public void setText(String text) {
        this.text = text;
        reset();
    }

    /**
     * Erase any old results and position to the beginning of the text.
     */
    public void reset() {
        problems.clear();
        reader = new BufferedReader(new StringReader(text));
        offset = 0;
        separatorLen = 0;
        line = "";
    }

    /**
     * Set the offset within the document. This should not be called while the
     * job is running.
     *
     * @param offset
     *            The offset to set.
     */
    public void setOffset(int offset) {
        reset();
        try {
            reader.skip(offset);
        } catch (IOException e) {
            // This should never really happen...
            throw new IllegalArgumentException("Error setting offset " + offset,
                    e);
        }
        this.offset = offset;
    }

    @Override
    protected IStatus run(IProgressMonitor monitor) {

        Display display = Display.getDefault();
        if (service == null) {
            PreferenceStore prfStore = new PreferenceStore();
            prfStore.setValue(SpellingService.PREFERENCE_SPELLING_ENABLED,
                    true);
            prfStore.setValue(SpellingService.PREFERENCE_SPELLING_ENGINE,
                    "spellingEngine");
            service = new SpellingService(prfStore);

            display.syncExec(new Runnable() {
                @Override
                public void run() {
                    collector.beginCollecting();
                }
            });
        }

        if (service == null) {
            return new Status(IStatus.ERROR, Activator.PLUGIN_ID,
                    "No spelling checker is available");
        }

        while (problems.isEmpty() && line != null) {
            offset += line.length() + separatorLen;
            try {
                readLine();
            } catch (IOException e) {
                throw new RuntimeException("Error reading text string", e);
            }
            if (line != null) {
                if (line.matches(vtecRegex)) {
                    continue;
                }
                document.set(line);
                regions[0] = new Region(0, line.length());
                synchronized (this) {
                    service.check(document, regions, context, this, monitor);
                }
                // Look in this line for words that are in
                // inappropriateWords.txt
                // If one is found and it has not already been flagged by the
                // Eclipse spell check above, add it to problems
                Matcher matcher = SpellCheckUtil.WORD_PATTERN.matcher(line);
                while (matcher.find()) {
                    String currentWord = matcher.group();
                    if (BlacklistedWordsUtil.isBlacklisted(currentWord)) {
                        int length = currentWord.length();
                        int offset = matcher.start();
                        SpellingProblem blacklistProblem = new AnotherSpellingProblem(
                                offset, length, currentWord);
                        if (problems.isEmpty()) {
                            problems.add(blacklistProblem);
                        } else {
                            Boolean problemAlreadyFlagged = false;
                            Iterator<SpellingProblem> probIter = problems
                                    .iterator();
                            while (probIter.hasNext()) {
                                if (probIter.next()
                                        .getOffset() == blacklistProblem
                                                .getOffset()) {
                                    problemAlreadyFlagged = true;
                                    break;
                                }
                            }
                            if (!problemAlreadyFlagged) {
                                problems.add(blacklistProblem);
                            }
                        }
                    }
                }
            }
        }
        // Sort the problems by position (offset) in this line of text
        List<SpellingProblem> problemsList;
        problemsList = new ArrayList<>();
        if (!problems.isEmpty()) {
            problemsList.addAll(problems);
            Collections.sort(problemsList, new Comparator<SpellingProblem>() {
                @Override
                public int compare(SpellingProblem sp1, SpellingProblem sp2) {
                    return Integer.valueOf(sp1.getOffset())
                            .compareTo(Integer.valueOf(sp2.getOffset()));
                }
            });
            problems.clear();
            problems.addAll(problemsList);
        }

        if (!problems.isEmpty()) {
            SpellingProblem lineProblem = problems.pollFirst();
            SpellingProblem documentProblem = new RevisedProblem(offset,
                    lineProblem);
            display.syncExec(new ProblemSender(documentProblem));
        }

        if (problems.isEmpty() && line == null) {
            display.syncExec(new Runnable() {
                @Override
                public void run() {
                    collector.endCollecting();
                }
            });
        }

        return Status.OK_STATUS;
    }

    /**
     * Read a line terminated by "\r\n" or "\n". This sets separatorLen based on
     * which terminator is found and sets line stripped of the terminator or
     * null when no more lines.
     *
     * @throws IOException
     */
    private void readLine() throws IOException {
        StringBuilder sb = new StringBuilder();
        int c;
        separatorLen = 0;
        while ((c = reader.read()) != -1) {
            if (c == '\n') {
                if (sb.length() > 0 && sb.charAt(sb.length() - 1) == '\r') {
                    separatorLen = 2;
                    sb.setLength(sb.length() - 1);
                } else {
                    separatorLen = 1;
                }
                line = sb.toString();
                return;
            }
            sb.append((char) c);
        }

        if (sb.length() == 0) {
            line = null;
        } else {
            // No line feed at end of the text.
            line = sb.toString();
        }
    }

    @Override
    public void accept(SpellingProblem problem) {
        problems.add(problem);
    }

    @Override
    public void beginCollecting() {
        // no-op
    }

    @Override
    public void endCollecting() {
        // no-op
    }

    /**
     * @return the collector
     */
    public ISpellingProblemCollector getCollector() {
        return collector;
    }

    /**
     * @param collector
     *            the collector to set
     */
    public void setCollector(ISpellingProblemCollector collector) {
        synchronized (this) {
            this.collector = collector;
        }
    }

    /**
     * A SpellingProblem concrete class. This just wraps the spelling problem
     * returned to us for the line, changing its offset from the start of the
     * line to the start of the document.
     *
     * @author wldougher
     *
     */
    private static class RevisedProblem extends SpellingProblem {

        private final int lineOffset;

        private final SpellingProblem lineProblem;

        RevisedProblem(int lineOffset, SpellingProblem lineProblem) {
            super();
            this.lineOffset = lineOffset;
            this.lineProblem = lineProblem;
        }

        @Override
        public int getLength() {
            return lineProblem.getLength();
        }

        @Override
        public String getMessage() {
            return lineProblem.getMessage();
        }

        @Override
        public int getOffset() {
            return lineOffset + lineProblem.getOffset();
        }

        @Override
        public ICompletionProposal[] getProposals() {
            return lineProblem.getProposals();
        }
    }

    private static class AnotherSpellingProblem extends SpellingProblem {

        private int offset;

        private int length;

        private String message;

        AnotherSpellingProblem(int offSet, int wordLength, String word) {
            super();
            this.offset = offSet;
            this.length = wordLength;
            this.message = "The word '" + word + "' is not correctly spelled";
        }

        @Override
        public int getOffset() {
            return this.offset;
        }

        public void setOffset(int offSet) {
            this.offset = offSet;
        }

        @Override
        public int getLength() {
            return this.length;
        }

        public void setLength(int wordLength) {
            this.length = wordLength;
        }

        @Override
        public String getMessage() {
            return this.message;
        }

        public void setMessage(String word) {
            this.message = "The word '" + word + "' is not correctly spelled";
        }

        @Override
        public ICompletionProposal[] getProposals() {
            // TODO Auto-generated method stub
            return null;
        }
    }
}
