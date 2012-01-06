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
package com.raytheon.uf.edex.plugin.mesowest;

import static com.raytheon.uf.common.localization.LocalizationContext.LocalizationType.EDEX_STATIC;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.edex.decodertools.core.filterimpl.AbstractFilterElement;
import com.raytheon.uf.edex.decodertools.core.filterimpl.AbstractObsFilter;
import com.raytheon.uf.edex.decodertools.core.filterimpl.RectFilterElement;

/**
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 23, 2009            jkorman     Initial creation
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class MESOWestFilter extends AbstractObsFilter {

    private Log logger = LogFactory.getLog(getClass());

    private static final String FILTER_CONFIG = "mesowest_filters.xml";

    public MESOWestFilter() {
    }

    public MESOWestFilter(String site) {
        try {
            IPathManager manager = PathManagerFactory.getPathManager();

            LocalizationContext siteContext = manager.getContextForSite(
                    EDEX_STATIC, site);

            File siteDir = manager.getFile(siteContext, "");
            if (siteDir.exists()) {
                File srcFile = new File(siteDir, FILTER_CONFIG);

                byte[] data = new byte[(int) srcFile.length()];

                InputStream stream = getInputStream(srcFile);
                try {
                    stream.read(data);
                    stream.close();

                    AbstractObsFilter filter = (AbstractObsFilter) SerializationUtil
                            .unmarshalFromXml(new String(data));

                    setFilterElements(filter.getFilterElements());
                    setFilterName(filter.getFilterName());
                } catch (IOException e) {
                    logger.error("Unable to read filter config", e);
                } catch (JAXBException e) {
                    logger.error("Unable to unmarshall filter config", e);
                }
            }

        } catch (Exception e) {
            logger.error("Error creating filter. Creating default empty", e);
            setFilterName("Default");
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
                boolean keep = false;
                for (AbstractFilterElement element : filterElements) {
                    PluginDataObject r = element.filter(reports[i]);
                    if (r != null) {
                        if (AbstractObsFilter.INCLUDE_TYPE.equals(element
                                .getFilterType())) {
                            keep = keep | true;
                        }
                    }
                }
                if (!keep) {
                    reports[i] = null;
                } else {
                    reportCount++;
                }
            }
        }
        if (reportCount == 0) {
            reports = new PluginDataObject[0];
        } else {
            PluginDataObject[] newReports = new PluginDataObject[reportCount];
            int i = 0;
            for (PluginDataObject report : reports) {
                if (report != null) {
                    newReports[i++] = report;
                }
            }
            reports = newReports;
        }
        return reports;
    }

    public static final void main(String[] args) {

        AbstractObsFilter filter = new MESOWestFilter();
        filter.setFilterName("TestFilter");

        RectFilterElement rect = new RectFilterElement();
        rect.setLowerRightLat(40.0);
        rect.setLowerRightLon(-90.0);
        rect.setUpperLeftLat(45.0);
        rect.setUpperLeftLon(-95.0);
        rect.setFilterElementName("OAX.WFO");
        rect.setFilterType(AbstractObsFilter.INCLUDE_TYPE);
        filter.addFilterElement(rect);

        rect = new RectFilterElement();
        rect.setLowerRightLat(46.0);
        rect.setLowerRightLon(-90.0);
        rect.setUpperLeftLat(49.0);
        rect.setUpperLeftLon(-100.0);
        rect.setFilterElementName("Upperbasin");
        rect.setFilterType(AbstractObsFilter.INCLUDE_TYPE);
        filter.addFilterElement(rect);

        String s = null;

        try {
            JAXBContext ctx = JAXBContext.newInstance(MESOWestFilter.class,
                    RectFilterElement.class);

            Marshaller msh = ctx.createMarshaller();

            msh.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);

            ByteArrayOutputStream istrm = new ByteArrayOutputStream();
            msh.marshal(filter, istrm);

            s = istrm.toString();

            System.out.println(s);

            rect = null;
            filter = null;

            Unmarshaller umsh = ctx.createUnmarshaller();

            ByteArrayInputStream ostrm = new ByteArrayInputStream(s.getBytes());
            filter = (AbstractObsFilter) umsh.unmarshal(ostrm);

            System.out.println(filter.getFilterName());
            System.out.println(filter.getClass().getName());

            List<AbstractFilterElement> elements = filter.getFilterElements();
            for (AbstractFilterElement element : elements) {
                System.out.println(element.getFilterElementName());
            }

        } catch (JAXBException e) {

            e.printStackTrace();
        }
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

}
