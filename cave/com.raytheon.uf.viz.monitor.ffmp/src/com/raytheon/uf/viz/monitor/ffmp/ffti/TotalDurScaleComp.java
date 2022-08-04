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
package com.raytheon.uf.viz.monitor.ffmp.ffti;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.widgets.Composite;

import com.raytheon.uf.common.dataplugin.ffmp.request.FFMPGetDefaultPurgeHourRequest;
import com.raytheon.uf.common.serialization.comm.RequestRouter;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.viz.core.mode.CAVEMode;
import com.raytheon.viz.ui.widgets.LabeledDoubleScale;

/**
 *
 * Total duration scale for the FFTI display.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * ????                                 Initial creation
 * Oct 07, 2013 2437       lvenable     Fixed color memory leak
 * Jun 21, 2018 6641       njensen      Fix getting retention time from server
 * Aug 31, 2018 6588       dgilling     Refactor based on LabeledDoubleScale.
 *
 * </pre>
 *
 * @author lvenable
 */

public class TotalDurScaleComp extends LabeledDoubleScale {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    /** Default retention time. */
    private static final int DEFAULT_RETENTION_TIME = 24;

    /**
     * Lower hour range value (always zero).
     */
    private static final double SCALE_LOWER_RANGE_VAL = 0;

    /** Duration interface. */
    private final DurationInterface owner;

    private final int retentionTime;

    public TotalDurScaleComp(Composite parent, DurationInterface owner) {
        super(parent, true);

        this.owner = owner;

        this.retentionTime = getFFMPRetentiontime();
        setFractionalDigits(2);
        setDoubleMinimum(SCALE_LOWER_RANGE_VAL);
        setDoubleMaximum(retentionTime);
        setResolution(0.25);
        setIncrement(1);
        setPageIncrement(1);
        addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                double newTimeDurHours = getDoubleValue();
                setTimeDuration(newTimeDurHours);

                owner.updateQPEDurHour(getDoubleValue());
                owner.updateGuidDurHour(getDoubleValue());
                owner.updateAccumAttrib(getDoubleValue());
            }
        });
    }

    public void setScaleToYellow() {
        changeScaleColor(getDisplay().getSystemColor(SWT.COLOR_YELLOW));
    }

    public void setScaleToGrey() {
        changeScaleColor(CAVEMode.getBackgroundColor());
    }

    private void changeScaleColor(final Color newColor) {
        scale.setForeground(newColor);
        scale.setBackground(newColor);
    }

    /**
     * An equivalent of changeTotalDur in FFTIhandler.C
     *
     * @param val
     */
    public void setTimeDuration(double val) {
        double newTimeDur = owner.adjustTotalDurationHr(val);
        setDoubleValue(newTimeDur);
    }

    /**
     * get the retention time (purge hour)
     *
     * @return retention time
     */
    private int getFFMPRetentiontime() {
        FFMPGetDefaultPurgeHourRequest req = new FFMPGetDefaultPurgeHourRequest();
        try {
            return (Integer) RequestRouter.route(req);
        } catch (Exception e) {
            statusHandler
                    .error("Error getting default FFMP purge rule, defaulting slider to "
                            + DEFAULT_RETENTION_TIME + " hours", e);

        }

        return DEFAULT_RETENTION_TIME;
    }
}
