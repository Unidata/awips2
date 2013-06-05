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
package com.raytheon.uf.viz.datadelivery.subscription.subset;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.viz.datadelivery.subscription.subset.presenter.IPointDataTimingSubsetView;

/**
 * Point Time Subset Tab. Sets the data retrieval interval.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 29, 2013    223     mpduff      Initial creation.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class PointTimeSubsetTab extends DataTimingSubsetTab implements
        IPointDataTimingSubsetView {

    /** Data Retrieval Intervals */
    private final String[] INTERVALS = new String[] { "5", "10", "15", "30" };

    /** Retrieval Interval Selection Combo */
    private Combo intervalCombo;

    /**
     * Constructor.
     * 
     * @param parentComp
     *            parent composite
     * @param callback
     *            dataSize callback
     * @param shell
     *            Shell
     */
    public PointTimeSubsetTab(Composite parentComp, IDataSize callback,
            Shell shell) {
        super(parentComp, callback, shell);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.datadelivery.subscription.subset.DataTimingSubsetTab
     * #init()
     */
    @Override
    public void init() {
        super.init();

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(2, false);
        Group intervalGroup = new Group(parentComp, SWT.NONE);
        intervalGroup.setText(" Set Data Retrieval Interval ");
        intervalGroup.setLayout(gl);
        intervalGroup.setLayoutData(gd);

        Label intervalLabel = new Label(intervalGroup, SWT.NONE);
        intervalLabel.setText("Retrieval Interval (minutes): ");

        GridData comboData = new GridData(85, SWT.DEFAULT);
        intervalCombo = new Combo(intervalGroup, SWT.READ_ONLY);
        intervalCombo.setLayoutData(comboData);
        intervalCombo.setItems(INTERVALS);
        intervalCombo.select(0);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setDataRetrievalInterval(int interval) {
        int idx = 0;
        for (String s : INTERVALS) {
            if (String.valueOf(interval).equals(s)) {
                intervalCombo.select(idx);
                break;
            }
            idx++;
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getDataRetrievalInterval() {
        return Integer.parseInt(intervalCombo.getItem(intervalCombo
                .getSelectionIndex()));
    }
}
