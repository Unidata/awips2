package com.raytheon.uf.viz.d2d.ui.dialogs.procedures;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import com.raytheon.uf.common.localization.LocalizationFile;

public class ProcedureTree {
    private LinkedList<ProcedureTree> children = null;

    private String text = null;

    private LocalizationFile file = null;

    public ProcedureTree(String text, LocalizationFile file) {
        this.setText(text);
        this.setFile(file);
    }

    public ProcedureTree addChild(String text, LocalizationFile file) {
        if (children == null) {
            children = new LinkedList<ProcedureTree>();
        }
        ProcedureTree child = new ProcedureTree(text, file);
        children.add(child);
        return child;
    }

    public ProcedureTree findChildByText(String text) {
        if (hasChildren()) {
            Iterator<ProcedureTree> iter = children.iterator();
            while (iter.hasNext()) {
                ProcedureTree child = iter.next();
                if (child.getText().equals(text)) {
                    return child;
                }
            }
            // if we didn't find it travers the children
            iter = children.iterator();
            while (iter.hasNext()) {
                ProcedureTree child = iter.next().findChildByText(text);
                if (child != null) {
                    return child;
                }
            }
        }

        return null;
    }

    public List<ProcedureTree> getChildren() {
        if (this.hasChildren()) {
            return this.children;
        } else {
            return null;
        }
    }

    public boolean hasChildren() {
        if (children != null && children.size() > 0) {
            return true;
        }
        return false;
    }

    /**
     * @param text
     *            the text to set
     */
    public void setText(String text) {
        this.text = text;
    }

    /**
     * @return the text
     */
    public String getText() {
        return text;
    }

    /**
     * @param file
     *            the file to set
     */
    public void setFile(LocalizationFile file) {
        this.file = file;
    }

    /**
     * @return the file
     */
    public LocalizationFile getFile() {
        return file;
    }
}
