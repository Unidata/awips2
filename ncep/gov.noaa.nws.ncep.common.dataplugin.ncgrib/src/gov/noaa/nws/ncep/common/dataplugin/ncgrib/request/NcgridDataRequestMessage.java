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
package gov.noaa.nws.ncep.common.dataplugin.ncgrib.request;

import gov.noaa.nws.ncep.common.dataplugin.ncgrib.NcgribModel;
import gov.noaa.nws.ncep.common.dataplugin.ncgrib.NcgribRecord;
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
 * 3/2012				   T. Lee		Changed perturbation number to String
 * 
 * </pre>
 * 
 * @author brockwoo
 * @version 1.0
 */
@DynamicSerialize
public class NcgridDataRequestMessage implements IServerRequest {

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
    private String pert = "";

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

    public String getPert() {
        return pert;
    }

    public void setPert(String pert) {
        this.pert = pert;
    }

    public int getVersion() {
        return version;
    }

    public void setVersion(int version) {
        this.version = version;
    }

    public void setInfoFromRecord(NcgribRecord record) {
        NcgribModel info = record.getModelInfo();
        DataTime time = record.getDataTime();
        this.modelName = info.getModelName();
        this.levelOne = info.getLevel().getLevelonevalue();
        this.levelTwo = info.getLevel().getLeveltwovalue();
        this.levelType = info.getLevel().getMasterLevel().getName();
        this.parameterAbbreviation = info.getParameterAbbreviation();
        if (info.getPerturbationNumber() != null) {
            this.pert = info.getPerturbationNumber();
        }
        this.version = record.getGridVersion();
        this.startTime = time.getRefTime().getTime();
        this.forecastTime = time.getFcstTime();
    }
}
