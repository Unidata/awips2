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

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.SI;
import javax.xml.bind.JAXBException;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.edex.plugin.grib.exception.GribException;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.parameter.Parameter;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * 
 * Adjusts temperature values that are mislabled as Celsius or Kelvin when they
 * actually represent the other one. Loads a list of parameters and thresholds
 * from a localization file. Assumes that all values above the threshold for a
 * parameter are in Kelvin and will convert if the declared unit is Celsius.
 * Values below the threshold are assumed to be in Celsius and will be converted
 * if the declared unit is Kelvin
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Mar 28, 2010  2874     bsteffen    Initial creation
 * 
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class TemperatureCorrectionPostProcessor implements
        IDecoderPostProcessor, ILocalizationFileObserver {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(TemperatureCorrectionPostProcessor.class);

    private static final String LOCALIZATON_LOCATION = "grib"
            + IPathManager.SEPARATOR + "temperatureCorrectionParameters.xml";

    private static final UnitConverter k2c = SI.KELVIN
            .getConverterTo(SI.CELSIUS);

    private static final UnitConverter c2k = SI.CELSIUS
            .getConverterTo(SI.KELVIN);

    private Map<String, Double> paramThresholdMap;

    public TemperatureCorrectionPostProcessor() throws GribException {
        LocalizationFile file = readConfiguration();
        if (file != null) {
            file.addFileUpdatedObserver(this);
        }
    }

    protected LocalizationFile readConfiguration() throws GribException {
        LocalizationFile file = PathManagerFactory.getPathManager()
                .getStaticLocalizationFile(LOCALIZATON_LOCATION);
        Map<String, Double> paramThresholdMap = new HashMap<String, Double>(8);
        if (file != null && file.exists()) {
            JAXBManager manager = null;
            try {
                manager = new JAXBManager(TemperatureCorrectionParameters.class);

            } catch (JAXBException e) {
                /* No hope of recovering */
                throw new GribException(
                        "Error occured preparing to load temperate correction parameters.",
                        e);
            }
            TemperatureCorrectionParameters params = null;
            try {
                params = file.jaxbUnmarshal(
                        TemperatureCorrectionParameters.class, manager);
            } catch (LocalizationException e) {
                /* Some hope of recovering with a better file. */
                statusHandler
                        .error("Error occured loading temperate correction parameters, verify the file is formatted correctly.",
                                e);
            }
            if (params != null) {
                for (TemperatureCorrectionParameter param : params
                        .getParameters()) {
                    paramThresholdMap.put(param.getAbbreviation(),
                            param.getThreshold());
                }
            }
        }
        this.paramThresholdMap = paramThresholdMap;
        return file;
    }

    @Override
    public void fileUpdated(FileUpdatedMessage message) {
        try {
            readConfiguration();
        } catch (GribException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
    }

    @Override
    public GridRecord[] process(GridRecord record) throws GribException {
        Parameter parameter = record.getParameter();
        Double thresholdObject = paramThresholdMap.get(parameter
                .getAbbreviation());
        if (thresholdObject != null) {
            Object messageData = record.getMessageData();
            if (messageData instanceof float[]) {
                double threshold = thresholdObject.doubleValue();
                if (parameter.getUnit().equals(SI.CELSIUS)) {
                    float[] data = (float[]) record.getMessageData();
                    for (int i = 0; i < data.length; i++) {
                        if (data[i] > threshold) {
                            data[i] = (float) k2c.convert(data[i]);
                        }
                    }
                } else if (parameter.getUnit().equals(SI.KELVIN)) {
                    float[] data = (float[]) record.getMessageData();
                    for (int i = 0; i < data.length; i++) {
                        if (data[i] < threshold && data[i] > -273.15) {
                            data[i] = (float) c2k.convert(data[i]);
                        }
                    }
                }
            } else {
                statusHandler.warn(this.getClass().getSimpleName()
                        + " is not checking " + record
                        + " because the messageData is " + messageData);
            }

        }
        return new GridRecord[] { record };
    }

    @XmlRootElement
    @XmlAccessorType(XmlAccessType.NONE)
    public static class TemperatureCorrectionParameters {

        @XmlElement(name = "parameter")
        private List<TemperatureCorrectionParameter> parameters;

        public List<TemperatureCorrectionParameter> getParameters() {
            return parameters;
        }

        public void setParameters(
                List<TemperatureCorrectionParameter> parameters) {
            this.parameters = parameters;
        }

    }

    @XmlAccessorType(XmlAccessType.NONE)
    public static class TemperatureCorrectionParameter {

        @XmlAttribute
        private String abbreviation;

        @XmlAttribute
        private double threshold;

        public String getAbbreviation() {
            return abbreviation;
        }

        public void setAbbreviation(String abbreviation) {
            this.abbreviation = abbreviation;
        }

        public double getThreshold() {
            return threshold;
        }

        public void setThreshold(double threshold) {
            this.threshold = threshold;
        }

    }

}
