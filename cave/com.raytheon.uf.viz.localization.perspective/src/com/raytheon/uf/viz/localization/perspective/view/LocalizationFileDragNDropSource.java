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
package com.raytheon.uf.viz.localization.perspective.view;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.ViewerDropAdapter;
import org.eclipse.swt.dnd.DND;
import org.eclipse.swt.dnd.DragSourceEvent;
import org.eclipse.swt.dnd.DragSourceListener;
import org.eclipse.swt.dnd.TransferData;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.localization.filetreeview.FileTreeEntryData;
import com.raytheon.uf.viz.localization.filetreeview.LocalizationFileEntryData;
import com.raytheon.uf.viz.localization.filetreeview.LocalizationFileGroupData;
import com.raytheon.uf.viz.localization.perspective.view.actions.ImportFileAction;

/**
 * Drag 'N Drop support class for FileTreeView LocalizationFiles
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 1, 2011             mschenke    Initial creation
 * Oct 13, 2015 4410       bsteffen    Allow localization perspective to mix
 *                                     files for multiple Localization Types.
 * Nov 12, 2015 4834       njensen     Changed LocalizationOpFailedException to LocalizationException
 * Jan 11, 2016 5242       kbisanz     Deleted unused dropLocalizationFile(...)
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class LocalizationFileDragNDropSource extends ViewerDropAdapter
        implements DragSourceListener {

    private FileTreeView view;

    LocalizationFileDragNDropSource(FileTreeView view, TreeViewer viewer) {
        super(viewer);
        this.view = view;
    }

    private LocalizationFile toCopy = null;

    private LocalizationFile potentialDelete = null;

    private LocalizationFile toDelete = null;

    // Drag methods

    @Override
    public void dragStart(DragSourceEvent event) {
        event.doit = false;
        Tree tree = view.getTree();
        TreeItem[] selected = tree.getSelection();
        if (selected.length == 1
                && selected[0].getData() instanceof LocalizationFileEntryData) {
            LocalizationFile file = ((LocalizationFileEntryData) selected[0]
                    .getData()).getFile();
            event.doit = file.getContext().getLocalizationLevel()
                    .isSystemLevel() == false;
            event.image = selected[0].getImage();
            toCopy = potentialDelete = file;
        }
    }

    @Override
    public void dragFinished(DragSourceEvent event) {
        if (!event.doit || event.detail == DND.DROP_NONE) {
            return;
        }

        if (toDelete != null) {
            try {
                toDelete.delete();
            } catch (LocalizationException e) {
                UFStatus.getHandler().handle(Priority.PROBLEM,
                        "Error deleting old file", e);
            }
        }
    }

    @Override
    public void dragSetData(DragSourceEvent event) {
        event.data = new String[] { toCopy.getFile().getAbsolutePath() };
    }

    // Drop methods

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.viewers.ViewerDropAdapter#performDrop(java.lang.Object)
     */
    @Override
    public boolean performDrop(Object toDrop) {
        FileTreeEntryData data = (FileTreeEntryData) getCurrentTarget();

        if (toDrop instanceof String[]) {
            String[] paths = (String[]) toDrop;
            File[] files = new File[paths.length];
            for (int i = 0; i < paths.length; ++i) {
                files[i] = new File(paths[i]);
            }
            boolean rval = dropFile(data.getPathData().getTypes(),
                    data.getPath(), files);
            if (rval) {
                toDelete = potentialDelete;
                if (toDelete == null) {
                    // Dragging from somewhere outside of FileTreeView
                    IPathManager pm = PathManagerFactory.getPathManager();
                    view.refresh(pm.getLocalizationFile(pm.getContext(data
                            .getPathData().getTypes().get(0),
                            LocalizationLevel.BASE), data.getPath()));
                }
            }
            return rval;
        } else {
            return false;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.viewers.ViewerDropAdapter#validateDrop(java.lang.Object
     * , int, org.eclipse.swt.dnd.TransferData)
     */
    @Override
    public boolean validateDrop(Object target, int operation,
            TransferData transferType) {
        if (target instanceof FileTreeEntryData) {
            FileTreeEntryData data = (FileTreeEntryData) target;
            if (data instanceof LocalizationFileEntryData == false
                    && data instanceof LocalizationFileGroupData == false) {
                if (operation == DND.DROP_COPY) {
                    potentialDelete = null;
                }
                return true;
            }
        }
        return false;
    }

    private boolean dropFile(List<LocalizationType> types, String dirPath,
            File[] toCopyFiles) {
        boolean oneGood = false;
        List<File> files = new ArrayList<File>(toCopyFiles.length);
        List<File> directories = new ArrayList<File>(toCopyFiles.length);

        for (File file : toCopyFiles) {
            if (file.isHidden() == false) {
                if (file.isDirectory()) {
                    directories.add(file);
                } else {
                    files.add(file);
                }
            }
        }

        if (files.size() > 0) {
            if (ImportFileAction.importFile(types, dirPath,
                    files.toArray(new File[files.size()]))) {
                oneGood = true;
            }
        }

        for (File dir : directories) {
            if (dropFile(types,
                    dirPath + IPathManager.SEPARATOR + dir.getName(),
                    dir.listFiles())) {
                oneGood = true;
            }
        }

        return oneGood;
    }
}
