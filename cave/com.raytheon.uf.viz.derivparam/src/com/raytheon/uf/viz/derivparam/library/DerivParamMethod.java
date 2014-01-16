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
package com.raytheon.uf.viz.derivparam.library;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;

import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizCommunicationException;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Metadata about a derived parameter method.
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Nov 21, 2009  3576     rjpeter     Initial version
 * Jan 14, 2014  2661     bsteffen    Remove ISerializableObject
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement
public class DerivParamMethod {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DerivParamMethod.class);

    public enum FrameworkMethod {
        ALIAS, MODELRUN, TIMERANGE, OR, UNION, SUPPLEMENT, IMPORT, NODERIVATION;

        /**
         * Sam functionality as valueOf except will return null if not found
         * rather than throwing an exception.
         * 
         * @param string
         * @return
         */
        public static FrameworkMethod valueOfOrNull(String string) {
            for (FrameworkMethod method : FrameworkMethod.values()) {
                if (method.toString().equals(string)) {
                    return method;
                }
            }
            return null;

        }

    }

    public enum MethodType {
        FRAMEWORK, PYTHON, OTHER;
    }

    @XmlAttribute(required = true)
    private String name;

    @XmlAttribute
    private String displayName;

    @XmlElements({
            @XmlElement(name = "ConstantField", type = DerivParamConstantField.class),
            @XmlElement(name = "Field", type = DerivParamField.class) })
    private List<IDerivParamField> fields;

    @XmlTransient
    private Set<Level> validLevels;

    @XmlAttribute
    private String levels;

    @XmlAttribute(name = "models")
    private List<String> validModels;

    @XmlAttribute
    private boolean dtime;

    @XmlAttribute
    private boolean ftime;

    @XmlTransient
    private MethodType methodType;

    @XmlTransient
    private FrameworkMethod frameworkMethod;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public List<IDerivParamField> getFields() {
        if (fields == null) {
            return Collections.emptyList();
        } else {
            return fields;
        }
    }

    public void setFields(List<IDerivParamField> fields) {
        this.fields = fields;
    }

    public void addField(IDerivParamField field) {
        if (fields == null) {
            fields = new ArrayList<IDerivParamField>();
        }

        fields.add(field);
    }

    public Set<Level> getValidLevels() throws VizCommunicationException {
        if (validLevels == null && levels != null && levels.length() > 0) {
            ValidLevelGenerator lvlGen = new ValidLevelGenerator();
            validLevels = lvlGen.generateLevels(levels);
        }
        return validLevels;
    }

    public List<String> getValidModels() {
        return validModels;
    }

    public void setValidModels(List<String> validModels) {
        this.validModels = validModels;
    }

    public boolean isDtime() {
        return dtime;
    }

    public void setDtime(boolean dtime) {
        this.dtime = dtime;
    }

    public boolean isFtime() {
        return ftime;
    }

    public void setFtime(boolean ftime) {
        this.ftime = ftime;
    }

    public void parseFields(String fields) throws VizException {
        if (fields == null) {
            throw new VizException("method [" + getName()
                    + "] has no field information.");
        }
        String[] pFields = fields.split("\\|");
        for (int j = 0; j < pFields.length; j++) {

            DerivParamField field;
            try {
                field = parseField(pFields[j]);
            } catch (VizException e) {
                throw new VizException("method [" + getName()
                        + "] error occured parsing field information"
                        + pFields[j] + "]", e);
            }
            // dTime/fTime need to grab next item and set to time shift
            if (isDtime() || isFtime()) {
                if (j + 1 < pFields.length) {
                    String timeOffset = pFields[++j];
                    try {
                        field.setTimeShift(new Integer(timeOffset));
                    } catch (NumberFormatException e) {
                        throw new VizException("method [" + getName()
                                + "] is marked as dTime/fTime and field ["
                                + field.getParam()
                                + "] has invalid time information ["
                                + timeOffset + "].", e);
                    }
                } else {
                    throw new VizException("method [" + getName()
                            + "] is marked as dTime/fTime and field ["
                            + field.getParam() + "] has no time information.");
                }

            }
            addField(field);

        }

    }

    /**
     * Parse a field for an alias or method
     * 
     * @param rawField
     * @return A field or null if something bad happened
     */
    private DerivParamField parseField(String rawField) throws VizException {
        DerivParamField field = new DerivParamField();
        String[] pFieldTokens = rawField.split(",");

        int index = 0;
        field.setParam(pFieldTokens[index++]);

        if (pFieldTokens.length > index) {
            String levelToken = pFieldTokens[index++];
            if (!field.setLevel(levelToken)) {
                index--;
            }

            if (pFieldTokens.length > index) {
                // source item
                field.setValidSource(pFieldTokens[index++]);

                if (pFieldTokens.length > index) {
                    // unhandled
                    StringBuilder tmp = new StringBuilder();
                    for (int k = 3; k < pFieldTokens.length; k++) {
                        tmp.append(pFieldTokens[k]);
                    }

                    statusHandler
                            .handle(Priority.SIGNIFICANT,
                                    "Derived parameter ["
                                            + "field ["
                                            + field.getParam()
                                            + "], contains unknown parameter modifier ["
                                            + tmp.toString() + "]");
                }
            }
        }
        return field;
    }

    public void parseValidModels(String validModels) {
        if (validModels != null && validModels.length() > 0) {
            setValidModels(Arrays.asList(validModels.split(",")));
        }
    }

    public void setLevels(String levels) {
        this.levels = levels;
    }

    public String getLevels() {
        return levels;
    }

    public String getDisplayName() {
        return displayName;
    }

    public void setDisplayName(String displayName) {
        this.displayName = displayName;
    }

    @Override
    public String toString() {
        StringBuilder tmp = new StringBuilder();
        tmp.append(name);
        if (fields != null) {
            for (IDerivParamField f : fields) {
                tmp.append(", field[");
                tmp.append(f.toString());
                tmp.append("]");
            }
        }
        return tmp.toString();
    }

    public void setMethodType(MethodType methodType) {
        this.methodType = methodType;
    }

    public MethodType getMethodType() {
        return methodType;
    }

    public void setFrameworkMethod(FrameworkMethod frameworkMethod) {
        this.methodType = MethodType.FRAMEWORK;
        this.frameworkMethod = frameworkMethod;
    }

    public FrameworkMethod getFrameworkMethod() {
        if (methodType == null) {
            frameworkMethod = FrameworkMethod.valueOfOrNull(name.toUpperCase());
            if (frameworkMethod != null) {
                methodType = MethodType.FRAMEWORK;
            }
        }
        return frameworkMethod;
    }
}
