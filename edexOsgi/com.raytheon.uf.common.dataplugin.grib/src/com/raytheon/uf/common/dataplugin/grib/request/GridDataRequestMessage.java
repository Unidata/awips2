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
package com.raytheon.uf.common.dataplugin.grib.request;

import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;
import com.raytheon.uf.common.time.DataTime;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 12, 2010            brockwoo     Initial creation
 * 
 * </pre>
 * 
 * @author brockwoo
 * @version 1.0
 */
@DynamicSerialize
public class GridDataRequestMessage implements IServerRequest {

    public static final long MISSING = -999l;

    @DynamicSerializeElement
    private String modelName;

    @DynamicSerializeElement
    private double levelOne = Level.INVALID_VALUE;

    @DynamicSerializeElement
    private double levelTwo = Level.INVALID_VALUE;

    @DynamicSerializeElement
    private String levelType;

    @DynamicSerializeElement
    private long startTime = -999l;

    @DynamicSerializeElement
    private int forecastTime = 0;

    @DynamicSerializeElement
    private String parameterAbbreviation;

    @DynamicSerializeElement
    private String ensemble;

    @DynamicSerializeElement
    private int version = -999;

    public String getModelName() {
        return modelName;
    }

    public void setModelName(String modelName) {
        this.modelName = modelName;
    }

    public double getLevelOne() {
        return levelOne;
    }

    public void setLevelOne(double levelOne) {
        this.levelOne = levelOne;
    }

    public double getLevelTwo() {
        return levelTwo;
    }

    public void setLevelTwo(double levelTwo) {
        this.levelTwo = levelTwo;
    }

    public String getLevelType() {
        return levelType;
    }

    public void setLevelType(String levelType) {
        this.levelType = levelType;
    }

    public long getStartTime() {
        return startTime;
    }

    public void setStartTime(long startTime) {
        this.startTime = startTime;
    }

    public int getForecastTime() {
        return forecastTime;
    }

    public void setForecastTime(int forecastTime) {
        this.forecastTime = forecastTime;
    }

    public String getParameterAbbreviation() {
        return parameterAbbreviation;
    }

    public void setParameterAbbreviation(String parameterAbbreviation) {
        this.parameterAbbreviation = parameterAbbreviation;
    }

    public String getEnsemble() {
        return ensemble;
    }

    public void setEnsemble(String ensemble) {
        this.ensemble = ensemble;
    }

    public int getVersion() {
        return version;
    }

    public void setVersion(int version) {
        this.version = version;
    }

    public void setInfoFromRecord(GridRecord record) {
        DataTime time = record.getDataTime();
        this.modelName = record.getDatasetId();
        this.levelOne = record.getLevel().getLevelonevalue();
        this.levelTwo = record.getLevel().getLeveltwovalue();
        this.levelType = record.getLevel().getMasterLevel().getName();
        this.parameterAbbreviation = record.getParameter().getAbbreviation();
        if (record.getEnsembleId() != null) {
            this.ensemble = record.getEnsembleId();
        }
        this.startTime = time.getRefTime().getTime();
        this.forecastTime = time.getFcstTime();
    }
}
