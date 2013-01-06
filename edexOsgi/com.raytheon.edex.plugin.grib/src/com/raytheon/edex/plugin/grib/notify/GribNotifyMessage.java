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
package com.raytheon.edex.plugin.grib.notify;

import java.util.Date;

import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.time.DataTime;

/**
 * A message representing a grib record that was ingested, but only the basic
 * human-readable information.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 6, 2009            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

@DynamicSerialize
public class GribNotifyMessage implements ISerializableObject {

    @DynamicSerializeElement
    private String model;

    @DynamicSerializeElement
    private DataTime dataTime;

    @DynamicSerializeElement
    private String paramAbbreviation;

    @DynamicSerializeElement
    private String levelName;

    @DynamicSerializeElement
    private double levelOne;

    @DynamicSerializeElement
    private double levelTwo;

    @DynamicSerializeElement
    private Date insertTime;

    @DynamicSerializeElement
    private String dataURI;

    public String getModel() {
        return model;
    }

    public void setModel(String model) {
        this.model = model;
    }

    public GribNotifyMessage() {

    }

    public GribNotifyMessage(GridRecord grib) {

        this.setInsertTime(grib.getInsertTime().getTime());
        this.setDataTime(grib.getDataTime());
        this.setModel(grib.getDatasetId());
        this.setLevelName(grib.getLevel().getMasterLevel().getName());
        this.setLevelOne(grib.getLevel().getLevelonevalue());
        this.setLevelTwo(grib.getLevel().getLeveltwovalue());
        this.setParamAbbreviation(grib.getParameter().getAbbreviation());
        this.setDataURI(grib.getDataURI());
    }

    public DataTime getDataTime() {
        return dataTime;
    }

    public void setDataTime(DataTime modelTime) {
        dataTime = modelTime;
    }

    public String getParamAbbreviation() {
        return paramAbbreviation;
    }

    public void setParamAbbreviation(String paramAbbreviation) {
        this.paramAbbreviation = paramAbbreviation;
    }

    public String getLevelName() {
        return levelName;
    }

    public void setLevelName(String levelName) {
        this.levelName = levelName;
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

    public Date getInsertTime() {
        return insertTime;
    }

    public void setInsertTime(Date insertTime) {
        this.insertTime = insertTime;
    }

    public void setDataURI(String dataURI) {
        this.dataURI = dataURI;
    }

    public String getDataURI() {
        return dataURI;
    }

}
