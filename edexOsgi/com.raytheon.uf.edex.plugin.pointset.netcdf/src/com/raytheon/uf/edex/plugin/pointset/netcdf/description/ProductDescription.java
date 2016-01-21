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
package com.raytheon.uf.edex.plugin.pointset.netcdf.description;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;

import ucar.nc2.NetcdfFile;

import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.dataplugin.pointset.PointSetRecord;
import com.raytheon.uf.common.parameter.Parameter;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.netcdf.description.AbstractFieldDescription;
import com.raytheon.uf.edex.netcdf.description.AttributeDescription;
import com.raytheon.uf.edex.netcdf.description.LevelDescription;
import com.raytheon.uf.edex.netcdf.description.ValueDescription;
import com.raytheon.uf.edex.netcdf.description.VariableDescription;
import com.raytheon.uf.edex.netcdf.description.date.DataTimeDescription;
import com.raytheon.uf.edex.netcdf.description.exception.InvalidDescriptionException;
import com.raytheon.uf.edex.plugin.pointset.netcdf.PointSetNetcdfDecoder;

/**
 * 
 * Describe a single product in a {@link NetcdfFile}. The fields on this object
 * provide the information necessary for the {@link PointSetNetcdfDecoder} to
 * convert the data in a netCDF file into a single {@link PointSetRecord}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- ----------------------------------
 * Aug 11, 2015  4709     bsteffen  Initial creation
 * Jan 21, 2016  5208     bsteffen  Move parameter persistence to dao
 * 
 * </pre>
 * 
 * @author bsteffen
 */
@XmlAccessorType(XmlAccessType.NONE)
public class ProductDescription {

    /**
     * This is intended to be true only when configuring new products because it
     * will print out information about why this description was not used.
     */
    @XmlAttribute
    private boolean debug;

    @XmlElement(required = true)
    private VariableDescription data;

    /**
     * The variable name of the longitude data, this is only necessary if the
     * data variable does not have a coordinates attribute that references the
     * longitude variable.
     */
    @XmlElement
    private VariableDescription longitude;

    /**
     * The variable name of the latitude data, this is only necessary if the
     * data variable does not have a coordinates attribute that references the
     * latitude variable.
     */
    @XmlElement
    private VariableDescription latitude;

    @XmlElements({
            @XmlElement(name = "datasetIdValue", type = ValueDescription.class),
            @XmlElement(name = "datasetIdAttribute", type = AttributeDescription.class) })
    private AbstractFieldDescription datasetId;

    @XmlElements({
            @XmlElement(name = "parameterValue", type = ValueDescription.class),
            @XmlElement(name = "parameterAttribute", type = AttributeDescription.class) })
    private AbstractFieldDescription parameter;

    @XmlElement
    private LevelDescription level;

    @XmlElement
    private DataTimeDescription dataTime;

    @XmlElement
    private TriangulationDescription triangulation;

    public boolean isDebug() {
        return debug;
    }

    public void setDebug(boolean debug) {
        this.debug = debug;
    }

    public VariableDescription getData() {
        return data;
    }

    public void setData(VariableDescription data) {
        this.data = data;
    }

    public VariableDescription getLongitude() {
        return longitude;
    }

    public void setLongitude(VariableDescription longitude) {
        this.longitude = longitude;
    }

    public VariableDescription getLatitude() {
        return latitude;
    }

    public void setLatitude(VariableDescription latitude) {
        this.latitude = latitude;
    }

    public AbstractFieldDescription getDatasetId() {
        return datasetId;
    }

    public void setDatasetId(AbstractFieldDescription datasetId) {
        this.datasetId = datasetId;
    }

    public AbstractFieldDescription getParameter() {
        return parameter;
    }

    public void setParameter(AbstractFieldDescription parameter) {
        this.parameter = parameter;
    }

    public LevelDescription getLevel() {
        return level;
    }

    public void setLevel(LevelDescription level) {
        this.level = level;
    }

    public DataTimeDescription getDataTime() {
        return dataTime;
    }

    public void setDataTime(DataTimeDescription dataTime) {
        this.dataTime = dataTime;
    }

    public TriangulationDescription getTriangulation() {
        return triangulation;
    }

    public void setTriangulation(TriangulationDescription triangulation) {
        this.triangulation = triangulation;
    }

    /**
     * Extract the the datasetId, parameter, level and datatime from the file
     * using the attributes contained in this description.
     */
    public PointSetRecord getRecord(NetcdfFile file, LevelFactory levelFactory)
            throws InvalidDescriptionException {
        String datasetId = this.datasetId.getString(file);
        if (datasetId == null) {
            return null;
        }
        String parameterAbbrev = this.parameter.getString(file);
        if (parameterAbbrev == null) {
            return null;
        }
        Level level = this.level.getLevel(file, levelFactory);
        if (level == null) {
            return null;
        }
        DataTime dataTime = this.dataTime.getDataTime(file);
        if (dataTime == null) {
            return null;
        }
        PointSetRecord record = new PointSetRecord();
        record.setDatasetId(datasetId);
        record.setParameter(new Parameter(parameterAbbrev));
        record.setLevel(level);
        record.setDataTime(dataTime);
        return record;
    }

    public String getDataVariable() {
        if (data == null) {
            return null;
        }
        return data.getName();
    }

    public String getLongitudeVariable() {
        if (longitude == null) {
            return null;
        }
        return longitude.getName();
    }

    public String getLatitudeVariable() {
        if (latitude == null) {
            return null;
        }
        return latitude.getName();
    }


}
