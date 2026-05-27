/*
 * The following software products were developed by Raytheon:
 *
 * ADE (AWIPS Development Environment) software
 * CAVE (Common AWIPS Visualization Environment) software
 * EDEX (Environmental Data Exchange) software
 * uFrame™ (Universal Framework) software
 *
 * Copyright (c) 2013 Raytheon Co.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/org/documents/epl-v10.php
 *
 *
 * Contractor Name: Raytheon Company
 * Contractor Address:
 * 2120 South 72nd Street
 * Omaha Tower, Suite 900
 * Omaha, NE 68124 USA
 * 402.291.0100
 *
 */
package com.raytheon.uf.viz.dataplugin.nswrc;

import java.awt.Rectangle;

import com.raytheon.uf.common.dataplugin.nswrc.NSWRCConstants;
import com.raytheon.uf.common.dataplugin.nswrc.NSWRCRadialRecord;

/**
 * This class gets the NetCDF radar data ready to display as an image, additionally
 * applying the noise threshold and power threshold filters if provided by the data.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 19, 2013            ekladstrup     Initial creation
 * Apr 22, 2014  3048      mweeks      Updates for peer review and 13.5.4 baseline.
 *
 * </pre>
 *
 * @author ekladstrup
 * @version 1.0
 */

public class FilteredRadarRadialDataRetriever extends RadialDataRetriever {

    /**
     * @param record
     * @param rect
     */
    public FilteredRadarRadialDataRetriever(NSWRCRadialRecord record,
            Rectangle rect) {
        super(record, rect);
    }

    /*
     * (non-Javadoc)
     *
     * @see com.raytheon.uf.viz.dataplugin.nswrcradar.RadarRadialDataRetriever#
     * convertData()
     */
    @Override
    public float[] convertData() {
        float[] data = super.convertData();
        if (this.record.getSignal_to_noise() != null
                && this.record.getNormalized_coherent_power() != null) {
            float[] snr = this.record.getSignal_to_noise();
            float[] ncp = this.record.getNormalized_coherent_power();

            for (int i = 0; i < data.length; ++i) {
                if (snr[i] < NSWRCConstants.NOISE_THRESHOLD || ncp[i] < NSWRCConstants.POWER_THRESHOLD) {
                    data[i] = NSWRCConstants.FILL_VALUE;
                }
            }
        }
        return data;
    }

}
