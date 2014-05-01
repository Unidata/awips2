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
package com.raytheon.uf.viz.monitor.ffmp.ui.dialogs;

import com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.FFMPConfig.TableCellColor;
import com.raytheon.uf.viz.monitor.ffmp.xml.FFMPTableColumnXML;

public class ThresholdManager {
    private FFMPTableColumnXML tcXML;

    public ThresholdManager(FFMPTableColumnXML tcXML) {
        this.tcXML = tcXML;
    }

    public TableCellColor getThresholdColor(double val) {
        if (Double.isNaN(val) == true || Double.isInfinite(val) == true) {
            return TableCellColor.Default;
        }

        if (val >= tcXML.getUpper()) {
            return TableCellColor.Upper;
        } else if (val >= tcXML.getMid()) {
            return TableCellColor.Mid;
        } else if (val >= tcXML.getLow()) {
            return TableCellColor.Lower;
        }

        return TableCellColor.BelowLower;
    }

    public TableCellColor getBasinThresholdColor(double val) {
        if (Double.isNaN(val) == true || Double.isInfinite(val) == true) {
            return TableCellColor.Default;
        }

        if (val >= tcXML.getUpper()) {
            return TableCellColor.Upper;
        } else if (val >= tcXML.getMid()) {
            return TableCellColor.Mid;
        } else {
            return TableCellColor.Lower;
        }
    }

    public double getUpperValue() {
        return tcXML.getUpper();
    }

    public double getMidValue() {
        return tcXML.getMid();
    }

    public double getLowerValue() {
        return tcXML.getLow();
    }

    public String getDisplayUnits() {
        return tcXML.getUnits();
    }
}
