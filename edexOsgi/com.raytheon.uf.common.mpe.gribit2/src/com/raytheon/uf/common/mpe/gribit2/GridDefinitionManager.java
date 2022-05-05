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
package com.raytheon.uf.common.mpe.gribit2;

import java.io.InputStream;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import javax.xml.bind.JAXBException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.mpe.gribit2.grid.ExtendedLatLonGridDefinition;
import com.raytheon.uf.common.mpe.gribit2.grid.GaussianGridDefinition;
import com.raytheon.uf.common.mpe.gribit2.grid.IGridDefinition;
import com.raytheon.uf.common.mpe.gribit2.grid.LambertConformalGridDefinition;
import com.raytheon.uf.common.mpe.gribit2.grid.LatLonGridDefinition;
import com.raytheon.uf.common.mpe.gribit2.grid.MercatorGridDefinition;
import com.raytheon.uf.common.mpe.gribit2.grid.PolarStereographicGridDefinition;
import com.raytheon.uf.common.mpe.gribit2.grid.StaggeredGridDefinition;
import com.raytheon.uf.common.serialization.JAXBManager;

/**
 * Provides access to the {@link IGridDefinition}s. Will keep a cache of
 * recently accessed grid definitions for ease of access. Based on:
 * /rary.ohd.pproc.gribit/TEXT/w3fi71.f.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 19, 2016 4619       bkowal      Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class GridDefinitionManager {

    private final Logger logger = LoggerFactory.getLogger(getClass());

    private static final String GRIBIT_GRID_FILE_FMT = XmrgToGribConstants.GRIBIT_ROOT
            + IPathManager.SEPARATOR
            + "grid"
            + IPathManager.SEPARATOR
            + "grid%d-Definition.xml";

    private static GridDefinitionManager INSTANCE;

    private static final int CACHE_SIZE = 30;

    private final ConcurrentMap<Integer, IGridDefinition> gridDefinitionCacheMap = new ConcurrentHashMap<>(
            CACHE_SIZE, 1.0f);

    private final JAXBManager jaxbManager;

    protected GridDefinitionManager() throws GridDefinitionException {
        try {
            jaxbManager = new JAXBManager(ExtendedLatLonGridDefinition.class,
                    GaussianGridDefinition.class,
                    LambertConformalGridDefinition.class,
                    LatLonGridDefinition.class, MercatorGridDefinition.class,
                    PolarStereographicGridDefinition.class,
                    StaggeredGridDefinition.class);
        } catch (JAXBException e) {
            throw new GridDefinitionException(
                    "Failed to instantiate the JAXB Manager.", e);
        }
    }

    public static synchronized GridDefinitionManager getInstance()
            throws GridDefinitionException {
        if (INSTANCE == null) {
            INSTANCE = new GridDefinitionManager();
        }
        return INSTANCE;
    }

    /**
     * Retrieves the {@link IGridDefinition} associated with the specified GRIB
     * grid number.
     * 
     * @param gribNum
     *            the specified GRIB grid number
     * @return the retrieved {@link IGridDefinition}
     */
    public IGridDefinition retrieveGridDefinition(final int gribNum)
            throws GridDefinitionNotFoundException, GridDefinitionException {
        IGridDefinition gridDefinition = gridDefinitionCacheMap.get(gribNum);
        if (gridDefinition == null) {
            /*
             * Attempt to read the grid definition from localization.
             */
            final String expectedGridDefFile = String.format(
                    GRIBIT_GRID_FILE_FMT, gribNum);
            IPathManager pathManager = PathManagerFactory.getPathManager();
            ILocalizationFile localizationFile = pathManager
                    .getStaticLocalizationFile(expectedGridDefFile);
            if (localizationFile == null || !localizationFile.exists()) {
                throw new GridDefinitionNotFoundException(gribNum);
            }

            logger.info("Reading Grid Definition: {} ...", expectedGridDefFile);
            try (InputStream is = localizationFile.openInputStream()) {
                gridDefinition = (IGridDefinition) jaxbManager
                        .unmarshalFromInputStream(is);
                logger.info("Successfully read Grid Definition: {}.",
                        expectedGridDefFile);
                /*
                 * Add the retrieved grid definition to the cache.
                 */
                if (gridDefinitionCacheMap.size() == CACHE_SIZE) {
                    /*
                     * Already at capacity. Clear to start caching recently
                     * retrieved grids.
                     */
                    gridDefinitionCacheMap.clear();
                }
                gridDefinitionCacheMap.put(gribNum, gridDefinition);
            } catch (Exception e) {
                throw new GridDefinitionException(
                        "Failed to read grid definition file: "
                                + expectedGridDefFile + ".", e);
            }
        }

        return gridDefinition;
    }
}