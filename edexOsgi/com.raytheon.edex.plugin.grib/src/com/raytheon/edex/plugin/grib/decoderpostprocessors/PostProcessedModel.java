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
package com.raytheon.edex.plugin.grib.decoderpostprocessors;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * A container class to hold which post processors apply to a grib model
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jul 24, 2012  949      bphillip    Initial Creation
 * Oct 15, 2013  2473     bsteffen    Remove deprecated ISerializableObject.
 * Oct 14, 2015  4627     nabowle     Add id attribute.
 * Apr 18, 2016  5182     tjensen     Optimized to store modelNamePattern
 * 
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@XmlRootElement(name = "postProcessedModel")
@XmlAccessorType(XmlAccessType.NONE)
public class PostProcessedModel {

    /**
     * The model name to which the processors apply. May be a regular expression
     */
    @XmlElement
    private String modelName;

    @XmlAttribute
    private String id;

    /**
     * The list of grib decoder post processors. The short class name may be
     * used if the class is in the
     * com.raytheon.edex.plugin.grib.decoderpostprocessors package. A fully
     * qualified name may be used if the grib post processor is defined
     * elsewhere
     */
    @XmlElement(name = "processorName")
    private List<String> processors;

    /**
     * Stores the regex pattern so it only needs to be calculated once.
     */
    private Pattern modelNamePattern;

    public PostProcessedModel() {

    }

    public String getModelName() {
        return modelName;
    }

    public void setModelName(String modelName) {
        this.modelName = modelName;
        this.modelNamePattern = Pattern.compile(modelName);
    }

    /**
     * @return the id
     */
    public String getId() {
        return id;
    }

    /**
     * @param id
     *            the id to set
     */
    public void setId(String id) {
        this.id = id;
    }

    public List<String> getProcessors() {
        if (processors == null) {
            processors = new ArrayList<String>();
        }
        return processors;
    }

    public void setProcessors(List<String> processors) {
        this.processors = processors;
    }

    public String toString() {
        StringBuilder buf = new StringBuilder();
        if (id != null) {
            buf.append(id).append("\n");
        }
        buf.append(modelName).append("\n");
        if (processors != null) {
            for (String proc : processors) {
                buf.append(proc).append("\n");
            }
        }
        return buf.toString();
    }

    public Pattern getModelNamePattern() {
        if (modelName != null && modelNamePattern == null) {
            modelNamePattern = Pattern.compile(modelName);
        }
        return modelNamePattern;
    }
}
