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
package com.raytheon.edex.plugin.grib.util;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementWrapper;

import com.raytheon.edex.plugin.grib.spatial.GribSpatialCache;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.gridcoverage.exception.GridCoverageException;

/**
 * 
 * Contains the grib fields that map to a specific grid dataset.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Oct 15, 2013  2473     bsteffen    Remove deprecated ISerializableObject.
 * Dec 16, 2015  5182     tjensen     Added fileName and support for meta 
 *                                    characters in the model name.
 * Jul 08, 2016  5748     bsteffen    Fix COVERAGE names when name is not in DB.
 * 
 * </pre>
 * 
 * @author unknown
 * @version 1.0
 * @see GribModelLookup
 */
@XmlAccessorType(XmlAccessType.NONE)
public class GridModel {

    /** The title of the model */
    @XmlElement
    private String title;

    /**
     * The model name. May contain certain meta characters to allow simple
     * generation of names based on model information.
     * 
     * <pre>
     * Supported meta characters are:
     *   ${COVERAGE}  Replaced with the grid name of associated with the model
     *   ${REGION}    Replaced with the name of the region for the GridCoverage
     *                associated with the model.
     *   ${RES(:(\d+)?(:([01]))?)?}
     *                Replaced with the resolution of the grid associated with
     *                the model. May provide optional information to specify
     *                the precision (\d+) and whether or not to add the units
     *                to the resolution ([01]) may be provided. Defaults to
     *                precision stored in grid and no units.
     *   ${\d+}       Replaced with corresponding numbered group from the
     *                provided file name regex.
     * </pre>
     */
    @XmlElement
    private String name;

    /** The center associated with this model */
    @XmlElement
    private Integer center;

    /** The NCEP grid associated with this model */
    @XmlElement
    private String grid;

    @XmlElementWrapper(name = "grids")
    @XmlElement(name = "id")
    private ArrayList<String> grids;

    @XmlElement
    private String subcenter;

    /** The generating processes associated with this model */
    @XmlElementWrapper(name = "process")
    @XmlElement(name = "id")
    private ArrayList<Integer> process;

    /** The generating processes associated with this model */
    @XmlElement
    private String processType;

    @XmlElement
    private String alias;

    /**
     * The intrinsic temporal resolution of the data.
     */
    @XmlElement
    private Integer dt;

    @XmlElement
    private String paramInfo;

    @XmlElement
    private boolean analysisOnly;

    /**
     * The regex file names for this model must match.
     */
    @XmlElement
    private String fileNameRegex;

    private Pattern namePattern;

    public String getFileNameRegex() {
        return fileNameRegex;
    }

    public void setFileNameRegex(String fileName) {
        this.fileNameRegex = fileName;
    }

    /**
     * Get the compiled pattern for the file name regex if it is not null.
     * 
     * @return Pattern for file name regex
     */
    public Pattern getNamePattern() {
        if (fileNameRegex != null && namePattern == null) {
            namePattern = Pattern.compile(fileNameRegex);
        }
        return namePattern;
    }

    /**
     * use datasetInfo instead
     */
    @Deprecated
    public String getTitle() {
        return title;
    }

    /**
     * use datasetInfo instead
     */
    @Deprecated
    public void setTitle(String title) {
        this.title = title;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    private static final Pattern metaRegex = Pattern.compile("\\$\\{.*?\\}");

    private static final Pattern covRegex = Pattern
            .compile("\\$\\{COVERAGE\\}");

    private static final Pattern resRegex = Pattern
            .compile("\\$\\{RES(:(\\d+)?(:([01]))?)?\\}");

    private static final Pattern regRegex = Pattern.compile("\\$\\{REGION\\}");

    private static final Pattern groupRegex = Pattern
            .compile("\\$\\{(\\d+)\\}");

    /**
     * Gets the name of the model with all accepted meta characters replaced
     * with the appropriate values.
     * 
     * @param gc
     *            GridCoverage used to get the resolution information.
     * @param filePath
     * @param myGrid
     *            Grid used to get the grid and region information.
     * @return model name with meta characters replaced.
     * @throws GridCoverageException
     */
    public String getTransformedName(GridCoverage gc, String filePath)
            throws GridCoverageException {
        String retval = name;
        /*
         * if name contains "${COVERAGE}", replace it with the appropriate grid
         * coverage name
         */
        Matcher covMatch = covRegex.matcher(retval);
        if (covMatch.find()) {
            Set<String> names = GribSpatialCache.getInstance()
                    .getGribCoverageNames(gc);
            if (names.size() != 1) {
                throw new GridCoverageException(
                        "Unable to determine model name for spatial coverage");
            }
            retval = covMatch.replaceAll(names.iterator().next());
        }

        /*
         * if name contains "${RES(:(d+)?(:([01]))?)?}", replace it with
         * resolution. "d+" is the number of decimal places to set resolution
         * to. "[01]" is a boolean for whether or not to add units.
         */
        Matcher resMatch = resRegex.matcher(retval);
        if (resMatch.find()) {
            resMatch.reset();
            StringBuffer sb = new StringBuffer();
            while (resMatch.find()) {
                String resReplace = "";

                /*
                 * If we have a precision specified in our match, display
                 * resolution in that precision. Else default to the precision
                 * stored in the GridCoverage
                 */
                if (resMatch.group(2) != null) {
                    resReplace += String.format("%." + resMatch.group(2) + "f",
                            gc.getDx());
                } else {
                    resReplace += gc.getDx();
                }

                /*
                 * If we have a boolean for displaying units and its value is
                 * "1", add units to the resolution.
                 */
                if (resMatch.group(4) != null && resMatch.group(4).equals("1")) {
                    resReplace += gc.getSpacingUnit();
                }
                resMatch.appendReplacement(sb, resReplace);
            }
            resMatch.appendTail(sb);
            retval = sb.toString();
        }

        // if name contains ${REGION}, replace it with the appropriate region.
        Matcher regMatch = regRegex.matcher(retval);
        if (regMatch.find()) {
            String region = GridRegionLookup.getInstance().determineRegion(gc);

            if (region != null) {
                retval = regMatch.replaceAll(region);
            } else {
                retval = regMatch.replaceAll("UNK");
            }
        }

        // if name contains "${1}", "${2}", etc and the model has a fileName
        // regex, pull the value of the noted group from the file name and
        // insert it into the model name.
        Matcher groupMatch = groupRegex.matcher(retval);
        if (fileNameRegex != null && groupMatch.find()) {
            groupMatch.reset();
            Path path = Paths.get(filePath);
            String myFileName = path.getFileName().toString();
            Matcher fileMatch = getNamePattern().matcher(myFileName);

            StringBuffer sb = new StringBuffer();
            while (groupMatch.find()) {
                int groupNum = Integer.parseInt(groupMatch.group(1));
                if (fileMatch.groupCount() >= groupNum && fileMatch.find()) {
                    groupMatch.appendReplacement(sb, fileMatch.group(groupNum));
                }
            }
            groupMatch.appendTail(sb);
            retval = sb.toString();
        }

        return retval;
    }

    /**
     * Checks to make sure all meta characters in the name are supported. Any
     * unsupported meta characters will be replaced with empty strings.
     * 
     * @return Error message if name is invalid, else an empty string.
     */
    public String checkValidName() {
        String retval = "";
        Matcher metaMatch = metaRegex.matcher(name);
        StringBuffer sb = new StringBuffer();
        /*
         * Loop over all meta characters found in the name. Keep all that match
         * the supported ones and drop all others from the name.
         */
        while (metaMatch.find()) {
            Matcher covMatch = covRegex.matcher(metaMatch.group(0));
            Matcher resMatch = resRegex.matcher(metaMatch.group(0));
            Matcher regMatch = regRegex.matcher(metaMatch.group(0));
            Matcher groupMatch = groupRegex.matcher(metaMatch.group(0));
            if (covMatch.matches() || resMatch.matches() || regMatch.matches()
                    || groupMatch.matches()) {
                metaMatch.appendReplacement(sb,
                        Matcher.quoteReplacement(metaMatch.group(0)));

            } else {
                metaMatch.appendReplacement(sb, "");
            }
        }
        metaMatch.appendTail(sb);
        String result = sb.toString();
        /*
         * If name doesn't match, that means there was an unsupported meta
         * character found. Update the name to remove it.
         */
        if (!result.equals(name)) {
            retval += "Model name '" + name
                    + "' contains unexpected meta characters. Using name '"
                    + result + "' instead.";
            name = result;
        }
        return retval;
    }

    public String getGrid() {
        return grid;
    }

    public void setGrid(String grid) {
        this.grid = grid;
    }

    public ArrayList<Integer> getProcess() {
        return process;
    }

    public void setProcess(ArrayList<Integer> process) {
        this.process = process;
    }

    public String getProcessType() {
        return processType;
    }

    public void setProcessType(String processType) {
        this.processType = processType;
    }

    public Integer getCenter() {
        return center;
    }

    public void setCenter(Integer center) {
        this.center = center;
    }

    public String getSubCenter() {
        return this.subcenter;
    }

    public void setSubCenter(String subcenter) {
        this.subcenter = subcenter;
    }

    /**
     * use datasetInfo instead
     */
    @Deprecated
    public void setAlias(String alias) {
        this.alias = alias;
    }

    /**
     * use datasetInfo instead
     */
    @Deprecated
    public String getAlias() {
        return alias;
    }

    /**
     * use datasetInfo instead
     */
    @Deprecated
    public Integer getDt() {
        return dt;
    }

    /**
     * use datasetInfo instead
     */
    @Deprecated
    public void setDt(Integer dt) {
        this.dt = dt;
    }

    /**
     * @return the paramInfo
     */
    @Deprecated
    public String getParamInfo() {
        return paramInfo;
    }

    /**
     * @param paramInfo
     *            the paramInfo to set
     */
    @Deprecated
    public void setParamInfo(String paramInfo) {
        this.paramInfo = paramInfo;
    }

    public boolean getAnalysisOnly() {
        return analysisOnly;
    }

    public ArrayList<String> getGrids() {
        return grids;
    }

    public void setGrids(ArrayList<String> grids) {
        this.grids = grids;
    }

    /**
     * Returns a list containing both the grid element if present and the grids
     * element if present
     * 
     * @return
     */
    public List<String> getAllGrids() {
        List<String> grids = new ArrayList<String>();
        if (this.grids != null) {
            grids.addAll(this.grids);
        }
        if (this.grid != null) {
            grids.add(this.grid);
        }
        return grids;
    }
}