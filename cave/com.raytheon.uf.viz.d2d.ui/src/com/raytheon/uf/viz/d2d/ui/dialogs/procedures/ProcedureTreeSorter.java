package com.raytheon.uf.viz.d2d.ui.dialogs.procedures;

import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerSorter;

public class ProcedureTreeSorter extends ViewerSorter {

    @Override
    public int compare(Viewer viewer, Object e1, Object e2) {
        if (e1 instanceof ProcedureTree && e2 instanceof ProcedureTree) {
            ProcedureTree lhs = (ProcedureTree) e1;
            ProcedureTree rhs = (ProcedureTree) e2;
            int comp = lhs.getText().compareToIgnoreCase(rhs.getText());
            if (comp < 0) {
                return -1;
            } else if (comp > 0) {
                return 1;
            } else {
                return 0;
            }
        }
        return -1;
    }

}
