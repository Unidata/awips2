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
package com.raytheon.uf.viz.datadelivery.common.ui;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;

import com.raytheon.uf.viz.datadelivery.filter.FilterImages;
import com.raytheon.uf.viz.datadelivery.filter.FilterImages.ExpandBarControlImage;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 16, 2012            lvenable     Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */

public class ExpandBarControls extends Composite {

    private IExpandControlAction callback;

    private FilterImages filterImgs;

    private Label collapseLbl;

    private Label expandLbl;

    private Label expandSelectedLbl;

    private Label disableLbl;

    private Label previewLbl;

    private ExpandBarControlsConfig config;

    public ExpandBarControls(Composite parent, ExpandBarControlsConfig config, IExpandControlAction callback,
            FilterImages fi) {
        super(parent, SWT.NONE);

        this.config = config;
        this.callback = callback;
        this.filterImgs = fi;

        init();
    }

    private void init() {

        GridLayout gl = new GridLayout(5, false);
        gl.marginWidth = 2;
        gl.marginHeight = 0;
        gl.verticalSpacing = 2;
        this.setLayout(gl);
        this.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        Composite composite = new Composite(this, SWT.NONE);
        GridLayout layout = new GridLayout(5, false);
        layout.marginHeight = 0;
        layout.marginWidth = 2;
        composite.setLayout(layout);
        composite.setLayoutData(new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false));

        if (config.getCollapseAll()) {
            collapseLbl = new Label(composite, SWT.NONE);
            collapseLbl.setImage(filterImgs.getExpandControlImage(ExpandBarControlImage.Collapse));
            collapseLbl.setToolTipText("Collapse Filters");
            collapseLbl.addMouseListener(new MouseAdapter() {
                @Override
                public void mouseDown(MouseEvent e) {
                    callback.collapseAction();
                }
            });
        }

        if (config.getExpandAll()) {
            expandLbl = new Label(composite, SWT.NONE);
            expandLbl.setImage(filterImgs.getExpandControlImage(ExpandBarControlImage.Expand));
            expandLbl.setToolTipText("Expand Filters");
            expandLbl.addMouseListener(new MouseAdapter() {
                @Override
                public void mouseDown(MouseEvent e) {
                    callback.expandAction();
                }
            });
        }

        if (config.getExpandSelected()) {
            expandSelectedLbl = new Label(composite, SWT.NONE);
            expandSelectedLbl.setImage(filterImgs.getExpandControlImage(ExpandBarControlImage.ExpandSelected));
            expandSelectedLbl.setToolTipText("Expand Items with Selections");
            expandSelectedLbl.addMouseListener(new MouseAdapter() {
                @Override
                public void mouseDown(MouseEvent e) {
                    callback.expandSelectedAction();
                }
            });
        }

        if (config.getDisable()) {
            GridData gd = new GridData();
            gd.horizontalIndent = 10;
            disableLbl = new Label(composite, SWT.NONE);
            disableLbl.setImage(filterImgs.getExpandControlImage(ExpandBarControlImage.Disable));
            disableLbl.setToolTipText("Enable/Disable Filters");
            disableLbl.setLayoutData(gd);
            disableLbl.addMouseListener(new MouseAdapter() {
                @Override
                public void mouseDown(MouseEvent e) {
                    callback.disableAction();
                }
            });
        }

        if (config.getClearAll()) {
            GridData gd = new GridData();
            gd.horizontalIndent = 2;
            disableLbl = new Label(composite, SWT.NONE);
            disableLbl.setImage(filterImgs.getExpandControlImage(ExpandBarControlImage.ClearAll));
            disableLbl.setToolTipText("Clear All Filter Entries");
            disableLbl.setLayoutData(gd);
            disableLbl.addMouseListener(new MouseAdapter() {
                @Override
                public void mouseDown(MouseEvent e) {
                    callback.clearAllAction();
                }
            });
        }

        if (config.getPreviewSelected()) {
            GridData gd = new GridData();
            gd.horizontalIndent = 10;
            previewLbl = new Label(composite, SWT.NONE);
            previewLbl.setImage(filterImgs.getExpandControlImage(ExpandBarControlImage.Preview));
            previewLbl.setToolTipText("Preview Selected Items");
            previewLbl.setLayoutData(gd);
            previewLbl.addMouseListener(new MouseAdapter() {
                @Override
                public void mouseDown(MouseEvent e) {
                    callback.previewAction();
                }
            });
        }
    }
}
