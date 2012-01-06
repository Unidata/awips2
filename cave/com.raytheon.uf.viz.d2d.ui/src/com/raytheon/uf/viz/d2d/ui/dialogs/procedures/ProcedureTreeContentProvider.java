package com.raytheon.uf.viz.d2d.ui.dialogs.procedures;

import java.util.List;

import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;

public class ProcedureTreeContentProvider implements ITreeContentProvider {

    private ProcedureTree rootNode = null;

    public ProcedureTreeContentProvider(ProcedureTree tree) {
        rootNode = tree;
    }

    @Override
    public void dispose() {
        // nothing to do here
    }

    @Override
    public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
        // nothing to do here
    }

    @Override
    public Object[] getElements(Object inputElement) {
        if (rootNode != null) {
            if (rootNode.getChildren() != null) {
                return rootNode.getChildren().toArray();
            }
        }
        return new Object[0];
    }

    @Override
    public Object[] getChildren(Object parentElement) {
        if (parentElement instanceof ProcedureTree) {
            ProcedureTree parent = (ProcedureTree) parentElement;
            List<ProcedureTree> children = parent.getChildren();
            if (children != null) {
                return children.toArray();
            } else {
                return new Object[0];
            }
        }
        return null;
    }

    @Override
    public Object getParent(Object element) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public boolean hasChildren(Object element) {
        if (element instanceof ProcedureTree) {
            ProcedureTree elem = (ProcedureTree) element;
            return elem.hasChildren();
        }
        return false;
    }

    public Object findItem(String text) {
        ProcedureTree item = rootNode.findChildByText(text);
        return item;
    }

}
