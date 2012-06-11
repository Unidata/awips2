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
package com.raytheon.edex.plugin.gfe.server.database;

import java.awt.Rectangle;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import ucar.ma2.ArrayChar;
import ucar.ma2.ArrayFloat;
import ucar.ma2.ArrayInt;
import ucar.ma2.DataType;
import ucar.nc2.Attribute;
import ucar.nc2.Dimension;
import ucar.nc2.NetcdfFile;
import ucar.nc2.Variable;

import com.raytheon.uf.common.dataplugin.gfe.config.ProjectionData;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DFloat;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.TimeRange;

/**
 * Reads and verifies a netCDF grid file.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 14, 2012            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class NetCDFFile {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(NetCDFFile.class);

    public static class ParmAtts {
        private String name;

        private String uiname;

        private String units;

        private String longName;

        private float maxVal, minVal, fillVal;

        private int[] dims;

        private Grid2DBit inventory;

        private List<String> levelNames;

        // private long possibleInventoryBits;

        public ParmAtts() {
            maxVal = 0;
            minVal = 0;
        }

        public ParmAtts(String name, String uiname, String units,
                String longName, float max, float min, int[] dims,
                Grid2DBit inventory, List<String> levelNames, float fillV) {
            this.name = name;
            this.uiname = uiname;
            this.units = units;
            this.longName = longName;

            this.maxVal = max;
            this.minVal = min;
            this.fillVal = fillV;
            this.dims = dims;
            this.inventory = inventory;

            this.levelNames = levelNames;
        }

        public String getName() {
            return name;
        }

        public String getUiname() {
            return uiname;
        }

        public String getUnits() {
            return units;
        }

        public String getLongName() {
            return longName;
        }

        public float getMaxVal() {
            return maxVal;
        }

        public float getMinVal() {
            return minVal;
        }

        public float getFillVal() {
            return fillVal;
        }

        public int[] getDims() {
            return dims;
        }

        public Grid2DBit getInventory() {
            return inventory;
        }

        public List<String> getLevelNames() {
            return levelNames;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            ParmAtts other = (ParmAtts) obj;
            if (!Arrays.equals(dims, other.dims)) {
                return false;
            }
            if (inventory == null) {
                if (other.inventory != null) {
                    return false;
                }
            } else if (!inventory.equals(other.inventory)) {
                return false;
            }
            if (levelNames == null) {
                if (other.levelNames != null) {
                    return false;
                }
            } else if (!levelNames.equals(other.levelNames)) {
                return false;
            }
            if (longName == null) {
                if (other.longName != null) {
                    return false;
                }
            } else if (!longName.equals(other.longName)) {
                return false;
            }
            if (Float.floatToIntBits(maxVal) != Float
                    .floatToIntBits(other.maxVal)) {
                return false;
            }
            if (Float.floatToIntBits(minVal) != Float
                    .floatToIntBits(other.minVal)) {
                return false;
            }
            if (name == null) {
                if (other.name != null) {
                    return false;
                }
            } else if (!name.equals(other.name)) {
                return false;
            }
            if (units == null) {
                if (other.units != null) {
                    return false;
                }
            } else if (!units.equals(other.units)) {
                return false;
            }
            return true;
        }
    };

    /**
     * @param fname
     *            file name to be validated
     * @return true if the supplied string is a valid netCDF file name
     */
    public static boolean validFileName(String fname) {
        return setModelTime(fname).isOkay();
    }

    /**
     * Attempts to determine the models run time. If it fails, an invalid
     * ServerResponse will be returned.
     * 
     * @param fname
     * @param modelTime
     * @return
     */
    static private ServerResponse<Date> setModelTime(String fname) {
        // The D2D "convention" is to use the file name as the base model
        // time. While there are two vars in the cdf file (valtime and reftime),
        // Both of these are just set to the _FillValue :(.
        File file = new File(fname);
        return NetCDFUtils.parseTime(file.getName());
    }

    private boolean valid;

    private String fname;

    private NetcdfFile cdf;

    private Date modelTime;

    private String modelName;

    private List<TimeRange> availableTimes;

    private List<ParmAtts> atts;

    private ProjectionData projection;

    private List<Boolean> tpSubPrev;

    /**
     * Constructor for a NetCDFFile taking the filename of the netcdf file and
     * an optional modelName (which overrides the netcdf model variable)
     * 
     * Attempt to open and read all of the meta data (all but the grids). If any
     * of this fails, then set the invalid flag.
     * 
     * @param fname
     *            netCDF file name
     * @param overrideModelName
     *            optional override filename. Null or empty string for no
     *            override
     */
    public NetCDFFile(String fname, String overrideModelName) {
        this.valid = false;
        this.fname = fname;
        this.cdf = null;

        // NcError nce(NcError::silent_nonfatal);

        CHECK_STATE(setModelTime());
        CHECK_STATE(openCDF());
        if (overrideModelName == null || overrideModelName.length() > 0) {
            this.modelName = overrideModelName;
        } else {
            CHECK_STATE(getModel());
        }
        CHECK_STATE(getProj());
        CHECK_STATE(getTimes());
        getTPDurations();
        CHECK_STATE(getNames());

        closeCDF();
        this.valid = true;
    }

    private void CHECK_STATE(ServerResponse<?> sr) {
        if (!sr.isOkay()) {
            statusHandler.handle(Priority.PROBLEM, "Invalid netCDF file: "
                    + this.fname + " : " + sr.message());
        }
    }

    public boolean isValid() {
        return valid;
    }

    public String getFname() {
        return fname;
    }

    public Date getModelTime() {
        return modelTime;
    }

    public String getModelName() {
        return modelName;
    }

    public List<TimeRange> getAvailableTimes() {
        return availableTimes;
    }

    public List<ParmAtts> getAtts() {
        return atts;
    }

    public ProjectionData getProjection() {
        return projection;
    }

    /**
     * Utility function which reads a float netcdf var attribute.
     * 
     * An invalid ServerResponse will be returned to indicate an error.
     * 
     * @param var
     *            is the netcdf variable to use.
     * @param name
     *            is the name of the attribute
     * @return the value of the attribute.
     */
    private ServerResponse<Float> getFloatVarAtt(Variable var, String name) {
        ServerResponse<Float> sr = new ServerResponse<Float>();
        Attribute att = var.findAttribute(name);
        if (att == null || !att.getDataType().equals(DataType.FLOAT)) {
            sr.addMessage("Missing or invalid attribute: " + name);
            return sr;
        }

        sr.setPayload(att.getNumericValue().floatValue());

        return sr;
    }

    /**
     * Utility function which reads a string netcdf var attribute.
     * 
     * An invalid ServerResponse will be returned to indicate an error.
     * 
     * @param var
     *            is the netcdf variable to use.
     * @param name
     *            is the name of the attribute
     * @return the value of the attribute.
     */
    private ServerResponse<String> getStringVarAtt(Variable var, String name) {
        ServerResponse<String> sr = new ServerResponse<String>();
        Attribute att = var.findAttribute(name);
        if (att == null || !att.getDataType().equals(DataType.STRING)) {
            sr.addMessage("Missing or invalid attribute: " + name);
            return sr;
        }

        sr.setPayload(att.getStringValue());

        return sr;
    }

    private ServerResponse<ProjectionData> getProj() {
        ServerResponse<ProjectionData> sr = NetCDFUtils.getProj(this.cdf);
        if (sr.isOkay()) {
            this.projection = sr.getPayload();
        }
        return sr;
    }

    /**
     * @return a list of parm names found in this netCDF file.
     */
    public List<String> getParmNames() {
        List<String> rval = new ArrayList<String>(this.atts.size());
        for (int i = 0; i < atts.size(); i++) {
            rval.add(atts.get(i).getName());
        }
        return rval;
    }

    /**
     * Returns a modified version of the input str. All non alphanumeric
     * characters are replaced with spaces. Then all spaces are removed.
     * 
     * @param str
     * @return
     */
    private String processName(String str) {
        char[] chars = new char[str.length()];
        for (int i = 0; i < chars.length; i++) {
            char c = str.charAt(i);
            chars[i] = (Character.isLetterOrDigit(c) ? c : ' ');
        }

        StringBuilder rval = new StringBuilder(chars.length);
        for (int j = 0; j < chars.length; j++) {
            if (chars[j] != ' ') {
                rval.append(chars[j]);
            }
        }

        return rval.toString();
    }

    /**
     * Attempts to read all of the needed attributes for a given NcVar. If the
     * attempt fails, then an invalid ServerResponse is returned.
     * 
     * @param var
     * @return
     */
    private ServerResponse<ParmAtts> getParmAtts(Variable var) {
        ServerResponse<ParmAtts> sr = new ServerResponse<ParmAtts>();

        String units, longname;
        float min = -30, max = 10000;

        ServerResponse<String> ssr = getStringVarAtt(var, "units");
        sr.addMessages(ssr);
        if (!ssr.isOkay()) {
            statusHandler.handle(Priority.PROBLEM, "[units] not found for "
                    + var.getFullName() + " " + getFname());
            return sr;
        }
        units = ssr.getPayload();

        ServerResponse<String> ttsr = getStringVarAtt(var, "long_name");
        if (!ttsr.isOkay()) {
            longname = var.getFullName();
            statusHandler.handle(Priority.VERBOSE, "[longname] not found for "
                    + var.getFullName() + " " + getFname());
        } else {
            longname = ttsr.getPayload();
        }

        ServerResponse<Float> tsrmin = getFloatVarAtt(var, "valid_min");
        ServerResponse<Float> tsrmax = getFloatVarAtt(var, "valid_max");

        if (!tsrmin.isOkay() || !tsrmax.isOkay()) {
            Attribute att = var.findAttribute("valid_range");
            if (att != null && att.getLength() == 2
                    && att.getDataType().equals(DataType.FLOAT)) {
                min = att.getNumericValue(0).floatValue();
                max = att.getNumericValue(1).floatValue();
            } else {
                // This is the CDF convention. But we can't use
                // it or the GFE will attempt to create billions and
                // billions of contours.
                // min = MINFLOAT;
                // max = MAXFLOAT;
                min = 0;
                max = 10000;
                if (!var.getFullName().equals("staticTopo")
                        && !var.getFullName().equals("staticSpacing")
                        && !var.getFullName().equals("staticCoriolis")) {
                    statusHandler.handle(Priority.VERBOSE,
                            "[valid_range] or [valid_min] or [valid_max] not found for "
                                    + var.getFullName() + " " + getFname());
                }
            }
        } else {
            min = tsrmin.getPayload();
            max = tsrmax.getPayload();
        }

        float fillV = Float.MAX_VALUE;
        ServerResponse<Float> fsr = getFloatVarAtt(var, "_FillValue");
        if (!fsr.isOkay()) {
            fillV = Float.MAX_VALUE;
            statusHandler.handle(Priority.VERBOSE,
                    "[_FillValue] not found for " + var.getFullName() + " "
                            + getFname());
        } else {
            fillV = fsr.getPayload();
        }

        // eliminate special characters/spaces from name
        String uiname = "";
        if (var.getFullName().equals("uw")) {
            uiname = "uw";
        } else if (var.getFullName().equals("vw")) {
            uiname = "vw";
        } else {
            String uinameOrig = var.getFullName();
            StringBuilder sb = new StringBuilder(uinameOrig.length());
            for (int i = 0; i < uinameOrig.length(); i++) {
                if (Character.isLetterOrDigit(uinameOrig.charAt(i))) {
                    sb.append(uinameOrig.charAt(i));
                }
            }
            uiname = sb.toString();
        }

        // long *tedges = var->edges();
        // SeqOf<long> dims(tedges, var->num_dims()); //dimensions of variable
        // delete[] tedges;

        int[] dims = var.getShape();

        Grid2DBit inventory = new Grid2DBit(1, 1, true);
        String ivarname = var.getFullName() + "Inventory";
        Variable invv = cdf.findVariable(ivarname);
        if (invv != null) {
            // tedges = invv->edges();
            // SeqOf<long> idims(tedges, invv->num_dims()); // dimensions of
            // inventory
            // delete[] tedges;
            int[] idims = invv.getShape();
            if (idims.length == 2) {
                try {
                    ArrayChar dta = (ArrayChar) invv.read();
                    inventory = new Grid2DBit(idims[1], idims[0]);
                    // note that if we have more bits than the main var,
                    // then the rest of the Grid2DBit is empty. That is why
                    // the y goes to dims[0] instead of idims[0]
                    // Y coordinate = time, X coordinate = levels
                    for (int y = 0; y < dims[0]; y++) {
                        for (int x = 0; x < idims[1]; x++) {
                            char c = (char) dta.getByte(y * idims[1] + x);
                            byte b = (byte) (c == '1' ? 1 : 0);
                            inventory.set(x, y, b);
                        }
                    }
                } catch (Exception e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error reading netCDF file inventory " + getFname()
                                    + ": " + e.getLocalizedMessage(), e);
                }
            }
        }

        List<String> levelNames = new ArrayList<String>();
        String lvarname = var.getFullName() + "Levels";
        Variable lvar = cdf.findVariable(lvarname);
        if (lvar != null) {
            if (lvar.getRank() == 2) {
                int[] origin = new int[] { 0, 0 };
                int[] size = new int[] { 1, lvar.getDimension(1).getLength() };
                for (int i = 0; i < lvar.getDimension(0).getLength(); i++) {
                    origin[0] = i;

                    try {
                        ArrayChar chars = (ArrayChar) lvar.read(origin, size)
                                .reduce(0);
                        String tmp = chars.getString();
                        levelNames.add(processName(tmp));
                    } catch (Exception e) {
                        statusHandler.handle(
                                Priority.PROBLEM,
                                "Error reading netCDF file levels "
                                        + getFname() + ": "
                                        + e.getLocalizedMessage(), e);
                    }
                }
            }
        }
        if (levelNames.size() == 0) {
            levelNames.add("Dflt");
        }

        sr.setPayload(new ParmAtts(var.getFullName(), uiname, units, longname,
                max, min, dims, inventory, levelNames, fillV));

        return sr;
    }

    /**
     * Attempts to load all of the usable netCDF grid variables. If the attempt
     * fails an invalid ServerResponse is returned.
     * 
     * @return
     */
    private ServerResponse<Object> getNames() {
        ServerResponse<Object> sr = new ServerResponse<Object>();

        this.atts = new ArrayList<NetCDFFile.ParmAtts>();
        for (Variable var : this.cdf.getVariables()) {
            if (var != null) {
                if (!var.getDataType().equals(DataType.FLOAT)) {
                    continue;
                }

                if (var.getFullName().indexOf(' ') != -1) {
                    continue;
                }

                boolean foundx = false;
                boolean foundy = false;
                for (int j = 0; j < var.getRank(); j++) {
                    Dimension dim = var.getDimension(j);
                    if (dim != null) {
                        String dname = dim.getName();
                        if (dname.equals("x")) {
                            foundx = true;
                        } else if (dname.equals("y")) {
                            foundy = true;
                        }

                        if (foundx && foundy) {
                            break;
                        }
                    }
                }
                if (foundx && foundy) {
                    ServerResponse<ParmAtts> tsr = getParmAtts(var);
                    sr.addMessages(tsr);
                    if (tsr.isOkay()) {
                        this.atts.add(tsr.getPayload());
                    }
                }
            }
        }

        return sr;
    }

    /**
     * Hairy Voodoo magic to get the duration of the tp grids from some netCDF
     * model files.
     * 
     */
    private void getTPDurations() {
        this.tpSubPrev = new ArrayList<Boolean>(getAvailableTimes().size());
        for (int i = 0; i < getAvailableTimes().size(); i++) {
            this.tpSubPrev.add(false);
        }
        if (getAvailableTimes().size() >= 2) {
            long duration = (getAvailableTimes().get(1).getStart().getTime() - getAvailableTimes()
                    .get(0).getStart().getTime()) / 1000;
            String s = String.format("_tp%d", (duration / 3600) * 2);
            Variable tvar = this.cdf.findVariable(s);
            if (tvar != null && tvar.getDataType().equals(DataType.FLOAT)) {
                Dimension d1 = tvar.getDimension(0);
                if (d1 != null) {
                    try {
                        ArrayFloat flags = (ArrayFloat) tvar.read();
                        for (int i = 0; i < d1.getLength(); i++) {
                            this.tpSubPrev.set(i, flags.getFloat(i) != 1e+37f);
                        }
                    } catch (Exception e) {
                        statusHandler.handle(Priority.PROBLEM, "Error reading "
                                + s + "from netCDF file " + getFname() + ": "
                                + e.getLocalizedMessage(), e);
                    }
                }
            }
        }
    }

    /**
     * Attempts to determine the valid grid times. If the attempt fails, then
     * this function returns an invalid ServerResponse.
     * 
     * @return ServerResponse
     */
    private ServerResponse<Object> getTimes() {
        ServerResponse<Object> sr = new ServerResponse<Object>();
        Variable tvar = this.cdf.findVariable("valtimeMINUSreftime");
        if (tvar == null || !tvar.getDataType().equals(DataType.INT)) {
            sr.addMessage("Missing or invalid 'valtimeMINUSreftime' var.");
        } else {
            Dimension d1 = tvar.getDimension(0);
            if (d1 == null) {
                sr.addMessage("Missing or invalid record dimension.");
            } else {
                try {
                    ArrayInt times = (ArrayInt) tvar.read();
                    this.availableTimes = new ArrayList<TimeRange>(
                            d1.getLength());
                    for (int i = 0; i < d1.getLength(); i++) {
                        this.availableTimes.add(new TimeRange(new Date(
                                this.modelTime.getTime() + times.getInt(i)
                                        * 1000L), 3600 * 1000));
                    }
                } catch (IOException e) {
                    statusHandler.handle(
                            Priority.PROBLEM,
                            "Error reading valtimeMINUSreftime from netCDF file "
                                    + getFname() + ": "
                                    + e.getLocalizedMessage(), e);
                }
            }
        }
        return sr;
    }

    /**
     * Attempts to determine the model name. If the attempt fails then an
     * invalid ServerResponse is returned.
     * 
     * @return ServerResponse
     */
    private ServerResponse<Object> getModel() {
        ServerResponse<Object> sr = new ServerResponse<Object>();
        Variable mvar = this.cdf.findVariable("model");
        if (mvar == null || !mvar.getDataType().equals(DataType.CHAR)) {
            sr.addMessage("Missing or invalid 'model' var.");
        } else {
            try {
                String tmp = ((ArrayChar) mvar.read()).getString();
                this.modelName = processName(tmp);
            } catch (IOException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error reading model from netCDF file " + getFname()
                                + ": " + e.getLocalizedMessage(), e);
            }
        }
        return sr;
    }

    /**
     * Attempts to open the netcdf file. If it can not be opened (or is not a
     * valid cdf file) then an invalid ServerResponse is returned.
     * 
     * @return ServerResponse
     */
    private ServerResponse<Object> openCDF() {
        ServerResponse<Object> sr = new ServerResponse<Object>();
        try {
            this.cdf = NetcdfFile.open(this.fname);
            if (this.cdf == null) {
                sr.addMessage("Invalid NetCDF file: " + this.fname);
            }
        } catch (IOException e) {
            statusHandler.handle(Priority.PROBLEM, "Error opening netCDF file "
                    + this.fname + ": " + e.getLocalizedMessage(), e);
        }
        return sr;
    }

    /**
     * Closes the netcdf file.
     * 
     */
    private void closeCDF() {
        try {
            this.cdf.close();
        } catch (IOException e) {
            statusHandler.handle(Priority.PROBLEM, "Error closing netCDF file "
                    + this.fname + ": " + e.getLocalizedMessage(), e);
        }
        this.cdf = null;
    }

    /**
     * Attempts to determine the models run time. If it fails, an invalid
     * ServerResponse will be returned.
     * 
     * @return
     */
    private ServerResponse<Date> setModelTime() {
        ServerResponse<Date> sr = setModelTime(this.fname);
        this.modelTime = sr.getPayload();
        return sr;
    }

    /**
     * Returns a pointer to a ParmAtts instance for the supplied parm name.
     * 
     * @param parmName
     *            desired parm name
     * @return the ParmAtts
     */
    public ParmAtts getAtts(String parmName) {
        for (ParmAtts a : this.atts) {
            if (a.getName().equals(parmName)) {
                return a;
            }
        }
        return null;
    }

    public Grid2DFloat getGrid(String parmName, int index, int level,
            Rectangle subdomain) {
        ParmAtts atts = getAtts(parmName);
        if (atts == null) {
            statusHandler.handle(Priority.PROBLEM, "Unknown parm name: "
                    + parmName);
            return null;
        }

        if (!openCDF().isOkay()) {
            statusHandler.handle(Priority.PROBLEM, "Error opening CDF File: "
                    + this.fname);
            this.valid = false;
            return null;
        }

        for (ParmAtts a : this.atts) {
            if (parmName.equals(a.getName())) {
                Grid2DFloat grid = null;
                ServerResponse<Grid2DFloat> sr = NetCDFUtils.getFloatGrid(
                        this.cdf, parmName, index, level, subdomain);
                if (!sr.isOkay()) {
                    closeCDF();
                    statusHandler.handle(Priority.PROBLEM, sr.message());
                    return null;
                } else {
                    grid = sr.getPayload();
                }

                closeCDF();
                return grid;
            }
        }

        statusHandler
                .handle(Priority.PROBLEM, "unknown parm name: " + parmName);
        closeCDF();
        return null;
    }

    public boolean getTpSubPrev(int index) {
        return this.tpSubPrev.get(index);
    }
}