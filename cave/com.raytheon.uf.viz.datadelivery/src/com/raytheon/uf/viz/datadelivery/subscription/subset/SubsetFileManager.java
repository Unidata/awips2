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
package com.raytheon.uf.viz.datadelivery.subscription.subset;

import java.io.File;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.datadelivery.common.xml.AreaXML;
import com.raytheon.uf.viz.datadelivery.subscription.subset.xml.DateRangeTimeXML;
import com.raytheon.uf.viz.datadelivery.subscription.subset.xml.SpecificDateTimeXML;
import com.raytheon.uf.viz.datadelivery.subscription.subset.xml.SubsetXML;
import com.raytheon.uf.viz.datadelivery.subscription.subset.xml.TimeXML;
import com.raytheon.uf.viz.datadelivery.subscription.subset.xml.VerticalXML;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils;

/**
 * File manager for subsets.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 28, 2012            mpduff     Initial creation.
 * Jun 07, 2012   687      lvenable   Table data refactor.
 * Jun  8, 2012   684      jpiatt     Modify file display.
 * Jul 30, 2012   702      jpiatt     Code cleanup.
 * Aug 22, 2012  0743      djohnson   Add TimeXML subclasses.
 * Nov 19, 2012  1289      bgonzale   Added deleteArea(String) method.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class SubsetFileManager {
    private static SubsetFileManager instance = null;

    private final String AREA_PATH = "dataDelivery" + File.separator + "subset" + File.separator + "area"
            + File.separator;

    private final String SUBSET_PATH = "dataDelivery" + File.separator + "subset" + File.separator;

    /** Status Handler */
    private final IUFStatusHandler statusHandler = UFStatus.getHandler(SubsetFileManager.class);

    /** JAXB context */
    private JAXBContext jax;

    /** Marshaller object */
    private Marshaller marshaller;

    /** Unmarshaller object */
    private Unmarshaller unmarshaller;

    private LocalizationFile[] subsetFiles;

    private SubsetFileManager() {
        createContext();
    }

    /**
     * Get an instance of the SubsetFileManager.
     * 
     * @return instance
     */
    public static SubsetFileManager getInstance() {
        if (instance == null) {
            instance = new SubsetFileManager();
        }

        return instance;
    }

    /**
     * Create the JAXB context
     */
    @SuppressWarnings("rawtypes")
    private void createContext() {
        Class[] classes = new Class[] { AreaXML.class, SubsetXML.class,
                SpecificDateTimeXML.class, DateRangeTimeXML.class,
                TimeXML.class,
                VerticalXML.class };

        try {
            jax = JAXBContext.newInstance(classes);
            this.unmarshaller = jax.createUnmarshaller();
            this.marshaller = jax.createMarshaller();
            this.marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);
        } catch (JAXBException e) {
            statusHandler.handle(Priority.ERROR, e.getLocalizedMessage(), e);
        }
    }

    /**
     * Save the area data.
     * 
     * @param area
     *            The area xml object to save
     * @param shell 
     * @return true if save successful
     */
    public boolean saveArea(AreaXML area, Shell shell) {
        if (!area.getRegionName().endsWith("xml")) {
            area.setRegionName(area.getRegionName() + ".xml");
        }
        IPathManager pm = PathManagerFactory.getPathManager();

        LocalizationContext context = pm.getContext(LocalizationType.CAVE_STATIC, LocalizationLevel.USER);
        LocalizationFile areaLocFile = pm.getLocalizationFile(context, AREA_PATH + area.getRegionName());

        if (areaLocFile.getFile().exists()) {
            String msg = "The file " + areaLocFile.getFile().getName()
                    + " already exists.\n\nWould you like to overwrite the file?";
            int response = DataDeliveryUtils.showMessage(shell, SWT.YES | SWT.NO, "File Exists", msg);
            if (response == SWT.NO) {
                return false;
            }
        }

        try {
            marshaller.marshal(area, areaLocFile.getFile());
            areaLocFile.save();
            return true;
        } catch (JAXBException e) {
            statusHandler.handle(Priority.ERROR, e.getLocalizedMessage(), e);
            e.printStackTrace();
        } catch (LocalizationOpFailedException e) {
            statusHandler.handle(Priority.ERROR, e.getLocalizedMessage(), e);
        }

        return false;
    }

    /**
     * Save the area data.
     * 
     * @param areaName
     *            The name of the area to delete
     * 
     * @return true if successful; false otherwise
     */
    public boolean deleteArea(String areaName) {
        if (!areaName.endsWith("xml")) {
            areaName = areaName + ".xml";
        }
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext context = pm.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.USER);
        LocalizationFile areaLocFile = pm.getLocalizationFile(context,
                AREA_PATH + areaName);

        try {
            if (areaLocFile.getFile().exists()) {
                return areaLocFile.delete();
            }
        } catch (LocalizationOpFailedException e) {
            statusHandler.handle(Priority.ERROR, e.getLocalizedMessage(), e);
        }
        return false;
    }

    /**
     * Get the AreaXML file
     * 
     * @param name
     *            Saved file name
     * @return AreaXML object
     */
    public LocalizationFile[] getAreas() {
        IPathManager pm = PathManagerFactory.getPathManager();

        String[] extensions = new String[] { "xml" };
        return pm.listStaticFiles(AREA_PATH, extensions, false, true);
    }

    /**
     * Get the area xml file
     * 
     * @param locFile
     *            Localization file
     * @return the AreaXML object
     */
    public AreaXML getArea(LocalizationFile locFile) {
        File file = locFile.getFile();
        AreaXML area = new AreaXML();
        try {
            area = (AreaXML)unmarshaller.unmarshal(file);
        } catch (JAXBException e) {
            statusHandler.handle(Priority.ERROR, e.getLocalizedMessage(), e);
            e.printStackTrace();
        }
        return area;
    }

    /**
     * Get all the saved subset files
     * 
     * @return LocalizationFile[]
     */
    public LocalizationFile[] getSubsets() {
        IPathManager pm = PathManagerFactory.getPathManager();

        LocalizationContext context = pm.getContext(LocalizationType.CAVE_STATIC, LocalizationLevel.USER);
        String[] extensions = new String[] { "xml" };
        this.subsetFiles = pm.listFiles(context, SUBSET_PATH, extensions, false, true);

        return subsetFiles;
    }

    /**
     * Save a subset xml object.
     * 
     * @param subset
     *            the object to save
     * @param shell 
     * @return true if successfully saved
     */
    public boolean saveSubset(SubsetXML subset, Shell shell) {
        IPathManager pm = PathManagerFactory.getPathManager();

        LocalizationContext context = pm.getContext(LocalizationType.CAVE_STATIC, LocalizationLevel.USER);
        LocalizationFile subsetLocFile = pm.getLocalizationFile(context, SUBSET_PATH + subset.getSubsetName());

        if (subsetLocFile.getFile().exists()) {
            String msg = "The file " + subsetLocFile.getFile().getName()
                    + " already exists.\n\nWould you like to overwrite the file?";
            int response = DataDeliveryUtils.showMessage(shell, SWT.YES | SWT.NO, "File Exists", msg);
            if (response == SWT.NO) {
                return false;
            }
        }

        try {
            marshaller.marshal(subset, subsetLocFile.getFile());
            subsetLocFile.save();
            System.out.println("Saved " + subsetLocFile.getFile().getPath());
            
            
            return true;
        } catch (JAXBException e) {
            statusHandler.handle(Priority.ERROR, e.getLocalizedMessage(), e);
            e.printStackTrace();
        } catch (LocalizationOpFailedException e) {
            statusHandler.handle(Priority.ERROR, e.getLocalizedMessage(), e);
            e.printStackTrace();
        }

        return false;
    }

    /**
     * Load a saved subset into memory
     * 
     * @param subsetName
     *            The subset name
     * 
     * @return The SubsetXML object
     */
    public SubsetXML<?> loadSubset(String subsetName) {
        // Load the subset files if not already loaded
        if (subsetFiles == null || subsetFiles.length == 0) {
            getSubsets();
        }

        for (LocalizationFile lf : subsetFiles) {
            if (lf.getFile().getName().equals(subsetName)) {
                try {
                    return (SubsetXML<?>) unmarshaller.unmarshal(lf.getFile());
                } catch (JAXBException e) {
                    statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
                }
            }
        }

        return null;
    }

    /**
     * Delete the subset
     * 
     * @param subsetName
     *            The name of the subset to delete
     */
    public void deleteSubset(String subsetName) {
        // Load the subset files if not already loaded
        if (subsetFiles == null || subsetFiles.length == 0) {
            getSubsets();
        }

        for (LocalizationFile lf : subsetFiles) {
            if (lf.getFile().getName().equals(subsetName)) {
                try {
                    lf.delete();
                } catch (LocalizationOpFailedException e) {
                    statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
                }
            }
        }
    }
}
