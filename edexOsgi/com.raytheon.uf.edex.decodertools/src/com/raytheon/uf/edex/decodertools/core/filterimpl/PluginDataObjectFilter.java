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
package com.raytheon.uf.edex.decodertools.core.filterimpl;

import static com.raytheon.uf.common.localization.LocalizationContext.LocalizationType.EDEX_STATIC;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;

import javax.xml.bind.JAXBException;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Filter {@link PluginDataObject}s based off configurable parameters.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jul 16, 2009           jkorman     Initial creation
 * Aug 30, 2013  2298     rjpeter     Make getPluginName abstract
 * Jun 11, 2014  2061     bsteffen    Remove IDecoderGettable
 * Jul 23, 2014 3410      bclement    location changed to floats
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class PluginDataObjectFilter extends AbstractObsFilter {

    private static final String ERROR_1_FMT = "Could not create {%s} context for file \"%s\"";

    private static final String ERROR_2_FMT = "File %s does not exist";

    public static final String FILTERS_DIR = "plugin-filters";

    private final Log logger = LogFactory.getLog(getClass());

    private String filterConfigFile = null;

    public PluginDataObjectFilter() {
    }

    public PluginDataObjectFilter(String configFile, String localContext) {
        filterConfigFile = configFile;
        try {
            File filterDir = null;

            IPathManager manager = PathManagerFactory.getPathManager();
            if (manager != null) {
                LocalizationContext context = manager.getContext(EDEX_STATIC,
                        LocalizationLevel.valueOf(localContext));
                if (context != null) {
                    filterDir = manager.getFile(context, FILTERS_DIR);
                    if (filterDir.exists()) {
                        File srcFile = new File(filterDir, filterConfigFile);

                        byte[] data = new byte[(int) srcFile.length()];

                        InputStream stream = getInputStream(srcFile);
                        try {
                            stream.read(data);
                            stream.close();

                            AbstractObsFilter filter = SerializationUtil
                                    .unmarshalFromXml(AbstractObsFilter.class,
                                            new String(data));

                            setFilterElements(filter.getFilterElements());
                            setFilterName(filter.getFilterName());
                        } catch (IOException e) {
                            logger.error("Unable to read filter config", e);
                        } catch (JAXBException e) {
                            logger.error("Unable to unmarshall filter config",
                                    e);
                        }
                    } else {
                        logger.error(String.format(ERROR_2_FMT,
                                filterDir.getPath()));
                        createDummyFilter();
                    }
                } else {
                    logger.error(String.format(ERROR_1_FMT, localContext,
                            configFile));
                    createDummyFilter();
                }
            } else {
                // Could not create PathManager
            }
        } catch (Exception e) {
            logger.error("Error creating filter.", e);
            createDummyFilter();
        }
        logger.info("Filter name = " + getFilterName());
        for (AbstractFilterElement element : getFilterElements()) {
            logger.info(element);
        }
    }

    /**
     * Apply the list of filters against given input data.
     * 
     */
    @Override
    public PluginDataObject[] filter(PluginDataObject[] reports) {
        int reportCount = 0;
        if (reports != null) {

            for (int i = 0; i < reports.length; i++) {
                PluginDataObject r = null;
                boolean keep = true;
                for (AbstractFilterElement element : filterElements) {
                    r = element.filter(reports[i]);

                    // Only allow keep to be set to true. Once true it stays
                    // that way.
                    if (AbstractObsFilter.INCLUDE_TYPE.equals(element
                            .getFilterType())) {
                        // Did the filter pass?
                        if (r == null) {
                            // If we fail an element, exit now.
                            keep = false;
                            break;
                        }
                    } else if (AbstractObsFilter.EXCLUDE_TYPE.equals(element
                            .getFilterType())) {
                        if (r != null) {
                            // There was a match, so we want to remove this
                            // item.
                            keep = false;
                            // And there's no reason for further checks.
                            break;
                        }
                    }
                }
                if (keep) {
                    reportCount++;
                } else {
                    reports[i] = null;
                }
            }
        }
        if (reportCount == 0) {
            reports = new PluginDataObject[0];
        } else {
            PluginDataObject[] newReports = new PluginDataObject[reportCount];
            int i = 0;
            // Copy in the reports that passed filtering.
            for (PluginDataObject report : reports) {
                if (report != null) {
                    newReports[i++] = report;
                }
            }
            reports = newReports;
        }
        return reports;
    }

    private void createDummyFilter() {
        setFilterName("Created Pass-All filter");
        // Add a dummy element.
        AbstractFilterElement dummy = new AbstractFilterElement() {
            @Override
            public PluginDataObject filter(PluginDataObject report) {
                return report;
            }
        };
        dummy.setFilterType(AbstractObsFilter.INCLUDE_TYPE);
        addFilterElement(dummy);
    }

    /**
     * 
     * @param file
     * @return
     */
    private static FileInputStream getInputStream(File file) {
        FileInputStream fis = null;

        try {
            fis = new FileInputStream(file);
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
        return fis;
    }

    private static class TestObject extends PluginDataObject implements
            ISpatialEnabled {

        private static final long serialVersionUID = 1L;

        SurfaceObsLocation location;

        @Override
        public ISpatialObject getSpatialObject() {
            return location;
        }

        @Override
        public String toString() {
            return location.getStationId() + " Passed";
        }

        @Override
        public String getPluginName() {
            return "test";
        }
    }

    public static final void main(String[] args) {

        RadiusFilterElement element = new RadiusFilterElement(0, 0, 60);
        element.setFilterType("INCLUDE");

        TestObject p = new TestObject();
        p.location = new SurfaceObsLocation("1.1");
        p.location.assignLocation(.8f, .8f);
        p = (TestObject) element.filter(p);
        System.out.println((p == null) ? "passed" : "failed");

        p = new TestObject();
        p.location = new SurfaceObsLocation("1.2");
        p.location.assignLocation(.7f, .7f);
        p = (TestObject) element.filter(p);
        System.out.println((p == null) ? "failed" : p);

        // Southeast corner of OAX WFO
        element = new RadiusFilterElement(40, -94.90, 100);
        element.setFilterType("INCLUDE");

        p = new TestObject();
        p.location = new SurfaceObsLocation("2.1");
        p.location.assignLocation(38.78f, -97.65f); // KSLN 38 47N 097 39W
        p = (TestObject) element.filter(p);
        System.out.println((p == null) ? p : "failed");

        p = new TestObject();
        p.location = new SurfaceObsLocation("2.2");
        p.location.assignLocation(39.13f, -96.68f); // KMHK 39 08N 096 41W
        p = (TestObject) element.filter(p);
        System.out.println((p != null) ? p : "failed");

        // Test set 3
        PluginDataObjectFilter filter = new PluginDataObjectFilter();
        element = new RadiusFilterElement(40, -94.90, 100);
        element.setFilterType("INCLUDE");
        filter.addFilterElement(element);

        RectFilterElement e = new RectFilterElement();
        e.setUpperLeftLat(42.90);
        e.setUpperLeftLon(-98.35);

        e.setLowerRightLat(40.0);
        e.setLowerRightLon(-94.90);
        e.setFilterType("INCLUDE");
        filter.addFilterElement(e);

        PluginDataObject[] pp = new PluginDataObject[3];
        p = new TestObject();
        p.location = new SurfaceObsLocation("KSLN");
        p.location.assignLocation(38.78f, -97.65f); // KSLN 38 47N 097 39W
        pp[0] = p;

        p = new TestObject();
        p.location = new SurfaceObsLocation("KMHK");
        p.location.assignLocation(39.13f, -96.68f); // KMHK 39 08N 096 41W
        pp[1] = p;

        p = new TestObject();
        p.location = new SurfaceObsLocation("KSTJ");
        p.location.assignLocation(41f, -96.00f);
        pp[2] = p;

        pp = filter.filter(pp);

        System.out.println("----------------------------------");
        System.out.println("- Success = KSTJ");
        System.out.println("----------");
        for (PluginDataObject o : pp) {
            System.out.println(o);
        }

        // Test set 4
        pp = new PluginDataObject[4];

        p = new TestObject();
        p.location = new SurfaceObsLocation("KORD");
        p.location.assignLocation(38.78f, -97.65f); // KSLN 38 47N 097 39W
        pp[0] = p;

        p = new TestObject();
        p.location = new SurfaceObsLocation("KSLN");
        p.location.assignLocation(38.78f, -97.65f); // KSLN 38 47N 097 39W
        pp[1] = p;

        p = new TestObject();
        p.location = new SurfaceObsLocation("KMHK");
        p.location.assignLocation(39.13f, -96.68f); // KMHK 39 08N 096 41W
        pp[2] = p;

        p = new TestObject();
        p.location = new SurfaceObsLocation("KSTJ");
        p.location.assignLocation(41f, -96.00f);
        pp[3] = p;

        filter = new PluginDataObjectFilter();

        StationIdFilterElement s = new StationIdFilterElement();
        s = new StationIdFilterElement();
        s.addPattern("K.*");
        s.setFilterType(INCLUDE_TYPE);
        filter.addFilterElement(s);

        // s = new StationIdFilterElement();
        // s.addPattern("KM[HIJ]K");
        // s.setFilterType(EXCLUDE_TYPE);
        // filter.addFilterElement(s);

        e = new RectFilterElement();
        e.setUpperLeftLat(42.90);
        e.setUpperLeftLon(-98.35);

        e.setLowerRightLat(40.0);
        e.setLowerRightLon(-94.90);
        e.setFilterType("INCLUDE");
        filter.addFilterElement(e);

        pp = filter.filter(pp);

        System.out.println("----------------------------------");
        System.out.println("- Success = KSLN, KSTJ");
        System.out.println("----------");
        for (PluginDataObject o : pp) {
            System.out.println(o);
        }
        System.out.println("----------------------------------");
    }
}
