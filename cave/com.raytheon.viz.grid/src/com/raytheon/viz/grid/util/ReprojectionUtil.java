package com.raytheon.viz.grid.util;

import java.util.regex.Pattern;

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.referencing.CRS;
import org.geotools.referencing.operation.projection.MapProjection;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.localization.AutoUpdatingLocalizationFile;
import com.raytheon.uf.common.localization.AutoUpdatingLocalizationFile.AutoUpdatingFileChangedListener;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.viz.grid.xml.GridReprojectionRules;
import com.raytheon.viz.grid.xml.GridReprojectionRules.Reproject;
import com.raytheon.viz.grid.xml.GridReprojectionRules.Rule;

/**
 * Determines if a grid should be reprojected based display properties and a
 * configuration file.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer         Description
 * ------------ ---------- ---------------- --------------------------
 * Sep 23, 2013 DR 15972   D. Friedman      Initial creation
 * 
 * </pre>
 * 
 */
public class ReprojectionUtil {
    private static final String RULES_PATH = "styleRules/gridReprojectionRules.xml";

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ReprojectionUtil.class);

    private static ReprojectionUtil instance;

    static ReprojectionUtil getInstance() {
        if (instance == null) {
            synchronized (ReprojectionUtil.class) {
                if (instance == null) {
                    instance = new ReprojectionUtil();
                }
            }
        }
        return instance;
    }

    GridReprojectionRules rules;

    AutoUpdatingLocalizationFile file;

    boolean fileChanged = false;

    public static boolean shouldReproject(GridRecord record,
            GridGeometry2D gridGeometry, DisplayType displayType,
            GeneralGridGeometry dstGridGeometry) {
        GridReprojectionRules rules = getInstance().getRules();
        if (rules != null) {
            for (Rule rule : rules.getRules()) {
                if (matches(rule, record, displayType, dstGridGeometry)) {
                    Reproject reproject = rule.getReproject();
                    if (reproject == Reproject.ALWAYS) {
                        return true;
                    } else if (reproject == Reproject.NEVER) {
                        return false;
                    } else if ((reproject == Reproject.TEST)
                            || (reproject == null)) {
                        return !ConformalityUtil.testConformality(gridGeometry,
                                dstGridGeometry);
                    } else {
                        throw new RuntimeException(
                                String.format("Unknown reprojection behavior "
                                        + reproject));
                    }
                }
            }
        }
        return false;
    }

    private static boolean matches(Rule rule, GridRecord record,
            DisplayType displayType, GeneralGridGeometry dstGridGeometry) {
        return matches(rule.getModelName(), record.getDatasetId())
                && matches(rule.getSrcProjection(),
                        getProjectionName(record.getLocation()))
                && matches(rule.getDstProjection(),
                        getProjectionName(dstGridGeometry))
                && matches(rule.getDisplayType(), displayType.toString());
    }

    private static boolean matches(String pattern, String value) {
        if (pattern == null) {
            return true;
        }
        return Pattern.matches(pattern, value != null ? value : "");
    }

    private static String getProjectionName(GridCoverage coverage) {
        return coverage != null ? getProjectionName(coverage.getCrs()) : null;
    }

    private static String getProjectionName(GeneralGridGeometry gridGeometry) {
        return gridGeometry != null ? getProjectionName(gridGeometry
                .getCoordinateReferenceSystem()) : null;
    }

    private static String getProjectionName(CoordinateReferenceSystem crs) {
        MapProjection projection = CRS.getMapProjection(crs);
        return projection != null ? projection.getName() : null;
    }

    public GridReprojectionRules getRules() {
        try {
            synchronized (this) {
                if (checkFileChanged()) {
                    GridReprojectionRules newRules = file.loadObject(
                            new JAXBManager(GridReprojectionRules.class),
                            GridReprojectionRules.class);
                    if (newRules == null) {
                        throw new LocalizationException("No " + RULES_PATH
                                + "found");
                    }
                    rules = newRules;
                }
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error occurred loading grid reproject rules", e);
        }
        return rules;
    }

    private synchronized boolean checkFileChanged()
            throws LocalizationException {
        if (file == null) {
            file = new AutoUpdatingLocalizationFile(RULES_PATH,
                    LocalizationType.COMMON_STATIC);
            file.addListener(new AutoUpdatingFileChangedListener() {
                @Override
                public void fileChanged(AutoUpdatingLocalizationFile file) {
                    synchronized (ReprojectionUtil.this) {
                        fileChanged = true;
                    }
                    ;
                }
            });
            return true;
        } else {
            /*
             * Only return true once per noticed file changed (and not after the
             * file has been read.) This is done to prevent a flood of AlertViz
             * errors if there is something wrong with the file.
             */
            if (fileChanged) {
                fileChanged = false;
                return true;
            } else {
                return false;
            }
        }
    }
}
