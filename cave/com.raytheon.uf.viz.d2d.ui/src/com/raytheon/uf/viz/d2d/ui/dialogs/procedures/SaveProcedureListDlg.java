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
package com.raytheon.uf.viz.d2d.ui.dialogs.procedures;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.procedures.Bundle;
import com.raytheon.uf.viz.core.procedures.Procedure;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.d2d.ui.dialogs.procedures.ProcedureComm.BundlePair;

/**
 * This dialog will allow users to open existing procedures.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 19, 2015             mjames	   Copied from OpenProcedureListDlg
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */



public class SaveProcedureListDlg extends ProcedureListDlg {
	
	/** Flag indicating the data will only be at the root level. */
    protected boolean oneLevel = true;
	
    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public SaveProcedureListDlg(Shell parent) {
        super("Save Bundle", parent,  ProcedureListDlg.Mode.SAVE);
    }

    /**
     * populate the data list
     */
    protected ProcedureTree populateDataList() {
        ProcedureTree root = new ProcedureTree("root", null);
        IPathManager mgr = PathManagerFactory.getPathManager();
        LocalizationContext ctx = mgr.getContext(LocalizationType.CAVE_STATIC,
                LocalizationLevel.USER);
        LocalizationFile[] files = mgr.listFiles(ctx,
                ProcedureDlg.PROCEDURES_DIR, null, true, true);
        String[] strings = new String[files.length];
        for (int i = 0; i < strings.length; i++) {
            strings[i] = LocalizationUtil.extractName(files[i].getName());
            root.addChild(strings[i], files[i]);
        }
        this.oneLevel = true;
        return root;
    }
}
