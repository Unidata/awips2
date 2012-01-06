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
package com.raytheon.uf.viz.localization.perspective.ui.custom.velocity;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.Stack;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.text.ITypedRegion;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.text.source.IAnnotationModelExtension;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.ui.texteditor.ITextEditor;

/**
 * This class listens for events to redo selection highlighting on the editor
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 14, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class VelocityEditCursorListener implements MouseListener, KeyListener {

    private class Selection {

        int offsetInLine;

        String selectionText;

        int docOffset;

        /**
         * @param offsetInLine
         * @param selectionText
         */
        public Selection(int offsetInLine, String selectionText) {
            this.offsetInLine = offsetInLine;
            this.selectionText = selectionText;
        }

    }

    private static final String IF = "#if";

    private static final String ELSEIF = "#elseif";

    private static final String ELSE = "#else";

    private static final String FOREACH = "#foreach";

    private static final String END = "#end";

    private static final String[] DIRECTIVES = { IF, ELSEIF, ELSE, FOREACH, END };

    private static final Map<String, Set<String>> forwardSearchPushWordMap = new HashMap<String, Set<String>>();

    private static final Map<String, Set<String>> forwardSearchPopWordMap = new HashMap<String, Set<String>>();

    private static final Map<String, Set<String>> reverseSearchPushWordMap = new HashMap<String, Set<String>>();

    private static final Map<String, Set<String>> reverseSearchPopWordMap = new HashMap<String, Set<String>>();

    static {
        Set<String> set = new HashSet<String>(Arrays.asList(new String[] { IF,
                FOREACH }));
        forwardSearchPushWordMap.put(IF, set);
        forwardSearchPushWordMap.put(ELSEIF, set);
        forwardSearchPushWordMap.put(ELSE, set);
        forwardSearchPushWordMap.put(FOREACH, set);
        reverseSearchPopWordMap.put(END, set);

        set = new HashSet<String>(Arrays.asList(new String[] { END }));
        reverseSearchPushWordMap.put(END, set);
        reverseSearchPushWordMap.put(ELSE, set);
        reverseSearchPushWordMap.put(ELSEIF, set);
        forwardSearchPopWordMap.put(IF, set);
        forwardSearchPopWordMap.put(ELSE, set);
        forwardSearchPopWordMap.put(ELSEIF, set);
        forwardSearchPopWordMap.put(FOREACH, set);

        set = new HashSet<String>(Arrays.asList(new String[] { IF, FOREACH }));
        reverseSearchPopWordMap.put(ELSE, set);
        reverseSearchPopWordMap.put(ELSEIF, set);
    }

    private int lastOffset = -1;

    private ITextEditor editor;

    private Map<Annotation, Position> annotations = new HashMap<Annotation, Position>();

    public VelocityEditCursorListener(ITextEditor editor) {
        this.editor = editor;
    }

    /**
     * Notifies clients about a change in the cursor position.
     */
    private void notifyCursorPositionChanged(int offset) {
        if (offset > -1) {
            IDocument doc = editor.getDocumentProvider().getDocument(
                    editor.getEditorInput());
            try {
                ITypedRegion region = doc.getPartition(offset);
                if (IDocument.DEFAULT_CONTENT_TYPE.equals(region.getType())) {
                    int line = doc.getLineOfOffset(offset);
                    int lineOffset = doc.getLineOffset(line);
                    int lineLength = doc.getLineLength(line);
                    String lineText = doc.get(lineOffset, lineLength);
                    int offsetIntoLine = offset - lineOffset;

                    Map<Annotation, Position> newAnnotations = new HashMap<Annotation, Position>();
                    Selection selectedDir = findDirective(lineText,
                            offsetIntoLine, DIRECTIVES);

                    if (selectedDir != null) {
                        int wordOffset = lineOffset + selectedDir.offsetInLine
                                + selectedDir.selectionText.length();

                        selectedDir.docOffset = lineOffset
                                + selectedDir.offsetInLine;
                        addAnnotation(newAnnotations, selectedDir);

                        int startIter = 0;
                        int iterations = 1;

                        if (selectedDir.selectionText.equals(END) == true) {
                            iterations = 2;
                            startIter = 1;
                        } else if (selectedDir.selectionText.equals(ELSE)
                                || selectedDir.selectionText.equals(ELSEIF)) {
                            iterations = 2;
                        }

                        for (int i = startIter; i < iterations; ++i) {
                            boolean reverse = i == 1;
                            Set<String> popWords = reverse ? reverseSearchPopWordMap
                                    .get(selectedDir.selectionText)
                                    : forwardSearchPopWordMap
                                            .get(selectedDir.selectionText);

                            Map<String, Set<String>> pushWordMap = reverse ? reverseSearchPushWordMap
                                    : forwardSearchPushWordMap;

                            Set<String> pushWords = pushWordMap
                                    .get(selectedDir.selectionText);
                            if (pushWords == null) {
                                pushWords = new HashSet<String>();
                            }
                            if (popWords == null) {
                                popWords = new HashSet<String>();
                            }

                            Stack<Set<String>> stack = new Stack<Set<String>>();
                            stack.push(null);

                            String restOfText = doc.get(
                                    reverse ? 0 : wordOffset,
                                    reverse ? selectedDir.docOffset : (doc
                                            .getLength() - wordOffset));

                            Selection end = null;
                            boolean done = false;
                            while (!done) {
                                end = findFirstDirective(restOfText,
                                        DIRECTIVES, reverse);
                                if (end == null) {
                                    done = true;
                                } else {
                                    if (popWords.contains(end.selectionText)) {
                                        pushWords = stack.pop();
                                        if (pushWords == null) {
                                            done = true;
                                        }
                                    } else if (pushWords
                                            .contains(end.selectionText)) {
                                        stack.push(pushWords);
                                        pushWords = pushWordMap
                                                .get(end.selectionText);
                                    } else if (stack.size() == 1) {
                                        end.docOffset = reverse ? end.offsetInLine
                                                : (wordOffset + end.offsetInLine);
                                        addAnnotation(newAnnotations, end);
                                    }

                                    if (!done) {
                                        if (reverse) {
                                            wordOffset -= (restOfText.length() - end.offsetInLine);
                                            restOfText = restOfText.substring(
                                                    0, end.offsetInLine);
                                        } else {
                                            int nextWordOffset = end.offsetInLine
                                                    + end.selectionText
                                                            .length();
                                            restOfText = restOfText
                                                    .substring(nextWordOffset);
                                            wordOffset += nextWordOffset;
                                        }
                                    }
                                }
                            }

                            if (end != null) {
                                end.docOffset = reverse ? end.offsetInLine
                                        : (wordOffset + end.offsetInLine);
                                addAnnotation(newAnnotations, end);
                            }
                        }
                    }

                    IAnnotationModelExtension model = (IAnnotationModelExtension) editor
                            .getDocumentProvider().getAnnotationModel(
                                    editor.getEditorInput());
                    model.replaceAnnotations(
                            annotations.keySet().toArray(new Annotation[0]),
                            newAnnotations);
                    annotations = newAnnotations;
                }
            } catch (BadLocationException e) {
                // Ignore
            }
        }
    }

    /**
     * @param newAnnotations
     * @param selectedDir
     */
    private void addAnnotation(Map<Annotation, Position> newAnnotations,
            Selection selection) {
        newAnnotations.put(new Annotation(
                VelocityTemplateEditor.OCCURANCE_ANNOTATION_TYPE, false,
                "occurance"), new Position(selection.docOffset,
                selection.selectionText.length()));
    }

    private Selection findDirective(String lineOfText, int offsetIntoLine,
            String[] directives) {
        String foundString = null;
        int containingIndex = 0;
        for (String dir : directives) {
            int index = 0;
            boolean search = true;
            boolean match = false;
            while (search && !match) {
                int idx = lineOfText.indexOf(dir, index);
                if (idx == -1) {
                    search = false;
                } else if (offsetIntoLine >= idx
                        && offsetIntoLine <= (idx + dir.length())) {
                    match = true;
                    containingIndex = idx;
                } else {
                    index = idx + 1;
                }
            }

            if (match) {
                foundString = dir;
                return new Selection(containingIndex, foundString);
            }
        }
        return null;
    }

    private Selection findFirstDirective(String lineOfText,
            String[] directives, boolean reverse) {
        String firstDir = null;
        int closestIdx = reverse ? -1 : Integer.MAX_VALUE;

        for (String dir : directives) {
            if (reverse) {
                int idx = lineOfText.lastIndexOf(dir);
                if (idx > -1 && idx > closestIdx) {
                    firstDir = dir;
                    closestIdx = idx;
                }
            } else {
                int idx = lineOfText.indexOf(dir);
                if (idx > -1 && idx < closestIdx) {
                    firstDir = dir;
                    closestIdx = idx;
                }
            }
        }

        if (firstDir != null) {
            return new Selection(closestIdx, firstDir);
        }
        return null;
    }

    public void mouseDoubleClick(MouseEvent e) {
    }

    public void mouseDown(MouseEvent e) {
    }

    /**
     * notify when the user makes a click
     */
    public void mouseUp(MouseEvent e) {
        int offset = getOffset();
        if (lastOffset != offset) {
            lastOffset = offset;
            notifyCursorPositionChanged(offset);
        }
    }

    public void keyPressed(KeyEvent e) {
    }

    private int getOffset() {
        return ((ITextSelection) editor.getSelectionProvider().getSelection())
                .getOffset();
    }

    /**
     * Notify when the user makes an arrow movement which actually changes the
     * cursor position (because while doing code-completion it could make that
     * notification when the cursor was changed in the dialog -- even if it
     * didn't affect the cursor position).
     */
    public void keyReleased(KeyEvent e) {
        if (e.character == '\0') {

            switch (e.keyCode) {
            case SWT.ARROW_DOWN:
            case SWT.ARROW_UP:
            case SWT.ARROW_LEFT:
            case SWT.ARROW_RIGHT:
            case SWT.HOME:
            case SWT.END:
            case SWT.PAGE_UP:
            case SWT.PAGE_DOWN:
                int offset = getOffset();
                if (offset != lastOffset) {
                    lastOffset = offset;
                    notifyCursorPositionChanged(offset);
                }
            default:
                return;
            }
        }
    }
}
