/**
 * This software was developed and / or modified by NOAA/NWS/OCP/ASDT
 */
package com.raytheon.viz.texteditor.dialogs;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.List;

import org.eclipse.swt.custom.ExtendedModifyEvent;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.graphics.Point;

/**
 * Manages undo/redo state for TextEditorDialog.
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 11, 2020 81547       dfriedman   Initial creation.
 * </pre>
 *
 * @author dfriedman
 */
public class TextEditorUndoManager {

    /**
     * Represents a single text replace operation on a text control.
     */
    private static class EditOp {
        private int start;

        private int length;

        private String replacedText;

        private EditOp(int start, int length, String replacedText) {
            super();
            this.start = start;
            this.length = length;
            this.replacedText = replacedText;
        }

        private static EditOp from(ExtendedModifyEvent event) {
            return new EditOp(event.start, event.length, event.replacedText);
        }
    }

    /**
     * Represents an undoable/redoable edit operation and associated coalescing
     * state.
     */
    private static class EditEntry {
        private enum Type {
            NONE, INSERT, DELETE_FORWARD, DELETE_BACKWARD
        }

        /**
         * Type of coalescing edit (INSERT, DELETE_FORWARD, DELETE_BACKWARD) or
         * NONE
         */
        private Type type = Type.NONE;

        /**
         * List of basic edit operations that compose this logical edit
         * operation.
         */
        private List<EditOp> ops = new ArrayList<>();

        /**
         * If this is a simple edit from a sing user keypress, the inserted or
         * deleted character. Used for coalescing logic.
         */
        private char userEditChar;

        private int startCaretPos = -1;

        private int endCaretPos = -1;

        private Point startSel;

        private Point endSel;

        /**
         * If true, this edit may be coalesced with subsequent edits.
         */
        private boolean open;

        /**
         * For a coalescing, true if a non-line-breaking whitespae as been
         * inserted or deleted.
         */
        private boolean seenWordSpace = false;

        /**
         * For a coalescing, true if a word character has been inserted or
         * deleted.
         */
        private boolean seenWordChar = false;

        /**
         * Attempt to coalesce the given edit into this one. If not possible,
         * this edit's open state is set to false.
         *
         * @param other
         * @return true if {@code other} was coalesced.
         */
        private boolean merge(EditEntry other) {
            if (open) {
                if (!ops.isEmpty()) {
                    char userChar = other.userEditChar;
                    if (userChar != '\0' && type == other.type) {
                        /**
                         * <pre>
                         * Coalesce any of the following types of edits:
                         * * Inserting typed text.
                         * * Deleting text forward.
                         * * Deleting text backward.
                         *
                         * The edit consists of any amount of intra-line
                         * white space followed by any amount of word
                         * characters. Currently, any non-whitespace character
                         * is treated as a word character.
                         * </pre>
                         */
                        boolean isWordSpace = Character.isWhitespace(userChar)
                                && userChar != '\r' && userChar != '\n'
                                && userChar != '\f';
                        boolean isWordChar = !Character.isWhitespace(userChar);
                        boolean mergeOk = false;

                        switch (type) {
                        case INSERT:
                            mergeOk = endCaretPos == other.startCaretPos
                                && ((isWordSpace && !seenWordChar)
                                        || isWordChar);
                            break;
                        case DELETE_FORWARD:
                            mergeOk = startCaretPos == other.startCaretPos
                                && ((isWordSpace && !seenWordChar)
                                        || isWordChar);
                            break;
                        case DELETE_BACKWARD:
                            mergeOk = endCaretPos == other.startCaretPos
                                && ((isWordChar && !seenWordSpace)
                                        || (isWordSpace));
                            break;
                        default:
                            mergeOk = false;
                        }
                        seenWordChar |= isWordChar;
                        seenWordSpace |= isWordSpace;
                        if (mergeOk) {
                            endCaretPos = other.endCaretPos;
                            ops.addAll(other.ops);
                            return true;
                        }
                    }
                } else {
                    // This shouldn't happen, but handle it just in case.
                    open = other.open;
                    type = other.type;
                    ops.addAll(other.ops);
                    endCaretPos = other.endCaretPos;
                    return true;
                }
                open = false;
            }
            return false;
        }

        /**
         * Create an edit with start/end state reversed.
         *
         * @return the reversed edit
         */
        private EditEntry createReversed() {
            /*
             * "open", "type", etc. are not preserved, effectively closing the
             * edit represented by this instance.
             */
            EditEntry result = new EditEntry();
            result.startCaretPos = this.endCaretPos;
            result.endCaretPos = this.startCaretPos;
            result.startSel = this.endSel;
            result.endSel = this.startSel;
            return result;
        }

    }

    /**
     * Represents a programmatically grouped sequence of operations. For
     * example, a user-type character and the associated wrapping operations.
     * Created by {@code beginGroup}.
     */
    public class EditGroup {

        private EditEntry entry;

        private EditGroup() {
            // nothing
        }

    }

    private Deque<EditEntry> undoEntries = new ArrayDeque<>();

    private Deque<EditEntry> redoEntries = new ArrayDeque<>();

    /**
     * True while reverting an edit in the undo list.
     */
    private boolean undoInProgress;

    /**
     * True while reverting an edit in the redo list.
     */
    private boolean redoInProgress;

    private TextEditorDialog dialog;

    private StyledText text;

    /**
     * Active edit group or null if there is none.
     */
    private EditGroup group;

    /**
     * Caret position before the start of an edit event.
     */
    private int preEditCaretPos = -1;

    /**
     * Text selection before the start of an edit event.
     */
    private Point preEditSel;

    /**
     * Create an UndoManager for the given TextEditorDialog and text control.
     *
     * @param dialog
     * @param text
     */
    public TextEditorUndoManager(TextEditorDialog dialog, StyledText text) {
        this.dialog = dialog;
        this.text = text;
    }

    /**
     * Return true if edits can be undone.
     *
     * @return
     */
    public boolean canUndo() {
        return !undoEntries.isEmpty();
    }

    /**
     * Return true if edits can be redone.
     *
     * @return
     */
    public boolean canRedo() {
        return !redoEntries.isEmpty();
    }

    /**
     * Reset the state, clearing the undo/redo history.
     */
    public void reset() {
        preEditCaretPos = -1;
        preEditSel = null;
        group = null;
        undoEntries.clear();
        redoEntries.clear();
        undoStateChanged();
    }

    /**
     * Start a programmatically grouped sequence of operations. Nested groups
     * are effectively flattened into the top-level group.
     */
    public EditGroup beginGroup() {
        EditGroup result = new EditGroup();
        if (group == null) {
            group = result;
        }
        return result;
    }

    public void endGroup(EditGroup group) {
        if (this.group == group) {
            endGroup();
        }
    }

    /**
     * End the current edit group, potentially coalescing the group with a prior
     * edit.
     */
    private void endGroup() {
        if (group != null) {
            EditEntry groupOp = group.entry;
            group = null;
            if (groupOp != null && !groupOp.ops.isEmpty()) {
                groupOp.endCaretPos = text.getCaretOffset();
                groupOp.endSel = getTextSelection();
                pushEdit(groupOp);
            }
        }
    }

    /**
     * Store the current caret and selection state prior to a pending text
     * modification.
     */
    public void recordCaretAndSelection() {
        preEditCaretPos = text.getCaretOffset();
        preEditSel = getTextSelection();
    }

    /**
     * Notify the manager of an change to the text control.
     */
    public void modifyTextHook(ExtendedModifyEvent event, boolean userEdit) {
        EditOp op = EditOp.from(event);

        if (undoInProgress) {
            /*
             * If undoing an edit, add the operations to the pending redo edit.
             */
            redoEntries.peek().ops.add(op);
        } else if (redoInProgress) {
            /*
             * If redoing an edit, add the operations to the pending undo edit.
             */
            undoEntries.peek().ops.add(op);
        } else {
            if (group != null) {
                /* If a group is active, add the operation to the group. */
                if (group.entry == null) {
                    group.entry = createEntry(op, userEdit);
                } else {
                    group.entry.ops.add(op);
                    group.entry.endCaretPos = text.getCaretOffset();
                }
            } else {
                /*
                 * Otherwise, add the edit to the undo list, potentially
                 * coalescing it into the current undo edit.
                 */
                pushEdit(createEntry(op, userEdit));
            }
        }
        preEditCaretPos = -1;
        preEditSel = null;
    }

    /**
     * Create an edit from a text modification event, potentially classifying it
     * as an operation that can be coalesced.
     *
     * @param op
     * @param userEdit
     *            true if the modification was directly from a user's keypress
     * @return
     */
    private EditEntry createEntry(EditOp op, boolean userEdit) {
        EditEntry entry = new EditEntry();
        entry.ops.add(op);
        entry.startCaretPos = preEditCaretPos;
        entry.endCaretPos = text.getCaretOffset();
        entry.startSel = preEditSel;
        entry.endSel = getTextSelection();
        if (userEdit && preEditSel == null) {
            entry.open = true;
            if (op.replacedText.length() == 0 && op.length == 1
                    && entry.startCaretPos + 1 == entry.endCaretPos) {
                entry.type = EditEntry.Type.INSERT;
                entry.userEditChar = text.getText(op.start, op.start).charAt(0);
            } else if (op.replacedText.length() == 1 && op.length == 0
                    && entry.startCaretPos == entry.endCaretPos) {
                entry.type = EditEntry.Type.DELETE_FORWARD;
                entry.userEditChar = op.replacedText.charAt(0);
            } else if (op.replacedText.length() == 1 && op.length == 0
                    && entry.startCaretPos - 1 == entry.endCaretPos) {
                entry.type = EditEntry.Type.DELETE_BACKWARD;
                entry.userEditChar = op.replacedText.charAt(0);
            } else {
                entry.open = false;
            }
        }
        return entry;
    }

    /**
     * Adds the given edit to the undo list, potentially coalescing it into the
     * the most recent undo entry. Clears any redo entries.
     *
     * @param newEntry
     */
    private void pushEdit(EditEntry newEntry) {
        EditEntry curEntry;

        if (!undoEntries.isEmpty()) {
            curEntry = undoEntries.peek();
        } else {
            curEntry = null;
        }
        if (curEntry != null && curEntry.merge(newEntry)) {
            // nothing
        } else {
            undoEntries.push(newEntry);
        }
        redoEntries.clear();
        undoStateChanged();
    }

    /**
     * Undo the most recent undoable edit. Does nothing if there are no undoable
     * edits.
     */
    public void undo() {
        if (undoInProgress || redoInProgress) {
            return;
        }
        if (undoEntries.isEmpty()) {
            return;
        }

        EditEntry editToRevert = undoEntries.pop();

        redoEntries.push(editToRevert.createReversed());

        undoInProgress = true;
        try {
            revert(editToRevert);
        } finally {
            undoInProgress = false;
        }
    }

    /**
     * Redo the most recent redoable edit. Does nothing if there are no redoable
     * edits.
     */
    public void redo() {
        if (undoInProgress || redoInProgress) {
            return;
        }
        if (redoEntries.isEmpty()) {
            return;
        }

        EditEntry editToRevert = redoEntries.pop();

        undoEntries.push(editToRevert.createReversed());

        redoInProgress = true;
        try {
            revert(editToRevert);
        } finally {
            redoInProgress = false;
        }
    }

    /**
     * Common code to revert an edit.
     *
     * @param editToRevert
     */
    private void revert(EditEntry editToRevert) {
        // Apply the edit operations in reverse order.
        for (int i = editToRevert.ops.size() - 1; i >= 0; --i) {
            EditOp op = editToRevert.ops.get(i);
            text.replaceTextRange(op.start, op.length, op.replacedText);
        }

        // Restore the caret and selection state.
        int pos = editToRevert.startCaretPos;
        Point sel = editToRevert.startSel;
        if (sel != null) {
            if (pos > editToRevert.startSel.x) {
                text.setSelection(sel.x, sel.y);
            } else {
                text.setSelection(sel.y, sel.x);
            }
        } else if (pos >= 0) {
            text.setCaretOffset(pos);
        }

        undoStateChanged();
    }

    private void undoStateChanged() {
        if (dialog != null) {
            dialog.realizeUndoState();
        }
    }

    private Point getTextSelection() {
        Point sel = text.getSelection();
        return sel.x != sel.y ? sel : null;
    }

}
