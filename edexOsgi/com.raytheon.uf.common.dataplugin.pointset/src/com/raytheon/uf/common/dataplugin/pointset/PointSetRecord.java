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
package com.raytheon.uf.common.dataplugin.pointset;

import java.nio.Buffer;
import java.nio.file.Path;
import java.nio.file.Paths;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ManyToOne;
import javax.persistence.PrimaryKeyJoinColumn;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.persist.IHDFFilePathProvider;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.parameter.Parameter;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * 
 * {@link PluginDataObject} implementation for the point set plugin. A
 * {@link PointSetRecord} is uniquely identified by its time, datasetId,
 * parameter and level.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Aug 11, 2015  4709     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 */
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "pointsetseq")
@Table(name = "pointset", uniqueConstraints = { @UniqueConstraint(name = "uk_pointset_datauri_fields", columnNames = {
        "refTime", "forecastTime", "datasetid", "parameter_abbreviation",
        "level_id" }) })
@DynamicSerialize
public class PointSetRecord extends PersistablePluginDataObject {

    private static final long serialVersionUID = 1L;

    @Column(nullable = false)
    @DataURI(position = 1)
    @DynamicSerializeElement
    private String datasetId;

    @ManyToOne(optional = false)
    @PrimaryKeyJoinColumn
    @DataURI(position = 2, embedded = true)
    @DynamicSerializeElement
    private Parameter parameter;

    @ManyToOne(optional = false)
    @PrimaryKeyJoinColumn
    @DynamicSerializeElement
    @DataURI(position = 3, embedded = true)
    private Level level;

    @Column(length = 40)
    @DynamicSerializeElement
    private String locationId;

    private transient Buffer data;

    public String getDatasetId() {
        return datasetId;
    }

    public void setDatasetId(String datasetId) {
        this.datasetId = datasetId;
    }

    public Parameter getParameter() {
        return parameter;
    }

    public void setParameter(Parameter parameter) {
        this.parameter = parameter;
    }

    public Level getLevel() {
        return level;
    }

    public void setLevel(Level level) {
        this.level = level;
    }

    public String getLocationId() {
        return locationId;
    }

    public void setLocationId(String locationId) {
        this.locationId = locationId;
    }

    public Buffer getData() {
        return data;
    }

    public void setData(Buffer data) {
        this.data = data;
    }

    public Path getStoragePath() {
        IHDFFilePathProvider pathProvider = getHDFPathProvider();
        String pluginName = getPluginName();
        String directory = pathProvider.getHDFPath(pluginName, this);
        if (directory.startsWith("/")) {
            directory = directory.substring(1);
        }
        String fileName = pathProvider.getHDFFileName(pluginName, this);
        return Paths.get(pluginName).resolve(directory).resolve(fileName);
    }

    @Override
    public String getPluginName() {
        return PointSetConstants.POINTSET;
    }

}
