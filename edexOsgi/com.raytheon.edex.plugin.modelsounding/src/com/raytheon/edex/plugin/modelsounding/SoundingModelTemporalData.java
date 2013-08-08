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
package com.raytheon.edex.plugin.modelsounding;

import java.util.Calendar;

import com.raytheon.edex.plugin.modelsounding.common.SoundingModels;
import com.raytheon.uf.common.time.DataTime;

/**
 * Stores temporal information associated with sounding data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 3, 2013        2161 bkowal     Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 * @version 1.0
 */

public class SoundingModelTemporalData {
    private Calendar obsTime;

    private DataTime dt;

    private long refTime;

    private long validTime;

    private int forecastHr;

    private SoundingModels model;

    /**
     * 
     */
    public SoundingModelTemporalData() {
        this.obsTime = null;
        this.dt = null;
        this.refTime = -1L;
        this.validTime = -1L;
        this.forecastHr = -1;
    }

    public Calendar getObsTime() {
        return obsTime;
    }

    public void setObsTime(Calendar obsTime) {
        this.obsTime = obsTime;
    }

    public DataTime getDt() {
        return dt;
    }

    public void setDt(DataTime dt) {
        this.dt = dt;
    }

    public long getRefTime() {
        return refTime;
    }

    public void setRefTime(long refTime) {
        this.refTime = refTime;
    }

    public long getValidTime() {
        return validTime;
    }

    public void setValidTime(long validTime) {
        this.validTime = validTime;
    }

    public int getForecastHr() {
        return forecastHr;
    }

    public void setForecastHr(int forecastHr) {
        this.forecastHr = forecastHr;
    }

    public SoundingModels getModel() {
        return model;
    }

    public void setModel(SoundingModels model) {
        this.model = model;
    }

    @Override
    public String toString() {
        StringBuilder stringBuilder = new StringBuilder(40);
        stringBuilder.append("model = ").append(model.getReportType());
        stringBuilder.append(", refTime = ");
        stringBuilder.append(this.refTime);
        stringBuilder.append(", forecastHr = ");
        stringBuilder.append(this.forecastHr);

        return stringBuilder.toString();
    }
}
