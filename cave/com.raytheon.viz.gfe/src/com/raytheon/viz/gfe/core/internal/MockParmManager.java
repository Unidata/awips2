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
package com.raytheon.viz.gfe.core.internal;

import java.awt.Point;
import java.io.DataInputStream;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.TimeZone;

import javax.measure.converter.ConversionException;
import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Path;

import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.gfe.config.ProjectionData;
import com.raytheon.uf.common.dataplugin.gfe.config.ProjectionData.ProjectionType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord.GridType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.TimeConstraints;
import com.raytheon.uf.common.dataplugin.gfe.discrete.DiscreteKey;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DByte;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DFloat;
import com.raytheon.uf.common.dataplugin.gfe.slice.DiscreteGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.ScalarGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.VectorGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.WeatherGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.weather.WeatherKey;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.griddata.DiscreteGridData;
import com.raytheon.viz.gfe.core.griddata.IGridData;
import com.raytheon.viz.gfe.core.griddata.ScalarGridData;
import com.raytheon.viz.gfe.core.griddata.VectorGridData;
import com.raytheon.viz.gfe.core.parm.MockParm;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * MockParmManager is a simulated parm manager that can be used for testing
 * 
 * It does not do any communication with the backend. All interactions are
 * simulated.
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02/04/2008              chammack    Initial Creation
 * 03/20/2013     #1774    randerso    Use TimeUtil constants
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class MockParmManager extends AbstractParmManager {

    private static final int HOURS_OF_DATA = 12;

    private static final int TIME_OFFSET = -6;

    private static final ProjectionData grid211 = new ProjectionData("Grid211",
            ProjectionType.LAMBERT_CONFORMAL.ordinal(), new Coordinate(
                    -133.459, 12.190), new Coordinate(-49.385, 57.290),
            new Coordinate(-95.0, 25.0), 25.0f, 25.0f, new Point(1, 1),
            new Point(93, 65), 0.0f, 0.0f, 0.0f);

    private static final GridLocation gloc = new GridLocation("OAX", grid211,
            new Point(145, 145), new Coordinate(45, 30), new Coordinate(9, 9),
            "CST6CDT");

    private static final TimeConstraints TC1 = new TimeConstraints(
            TimeUtil.SECONDS_PER_HOUR, TimeUtil.SECONDS_PER_HOUR, 0);

    private static final TimeConstraints TC2 = new TimeConstraints(
            13 * TimeUtil.SECONDS_PER_HOUR, TimeUtil.SECONDS_PER_DAY, 13);

    protected Set<Parm> fullParmSet;

    protected List<ParmID> parmIDs;

    protected DatabaseID mutableDb;

    protected TimeRange systemTimeRange;

    protected ParmID[] mockD2DGrids;

    public MockParmManager(DataManager dmgr) {
        super(dmgr);
        this.fullParmSet = new LinkedHashSet<Parm>();

        this.parmIDs = new ArrayList<ParmID>();
        this.mutableDb = new DatabaseID("OAX_GRID__Fcst_00000000_0000");

        DatabaseID dbid = new DatabaseID("OAX_GRID__Fcst_00000000_0000");
        DatabaseID mockD2Did = new DatabaseID(
                "OAX_GRID_D2D_NAM12_20080320_0000");

        // Create a bogus set of parms
        GridParmInfo gpi = new GridParmInfo(new ParmID("T", dbid,
                ParmID.defaultLevel()), gloc, GridType.SCALAR, "F",
                "Surface Temperature", -80f, 120f, 0, false, TC1, false);
        addInternal(gpi, true);

        gpi = new GridParmInfo(new ParmID("Td", dbid, ParmID.defaultLevel()),
                gloc, GridType.SCALAR, "F", "Dewpoint", -80f, 120f, 0, false,
                TC1, false);
        addInternal(gpi, true);

        gpi = new GridParmInfo(new ParmID("MaxT", dbid, ParmID.defaultLevel()),
                gloc, GridType.SCALAR, "F", "Maximum Temperature", -80f, 120f,
                0, false, TC2, false);
        addInternal(gpi, true);

        gpi = new GridParmInfo(new ParmID("HeatIndex", dbid,
                ParmID.defaultLevel()), gloc, GridType.SCALAR, "F",
                "Heat Index", -80f, 130f, 0, false, TC1, false);
        addInternal(gpi, true);

        gpi = new GridParmInfo(new ParmID("WindChill", dbid,
                ParmID.defaultLevel()), gloc, GridType.SCALAR, "F",
                "Wind Chill", -120f, 120f, 0, false, TC1, false);
        addInternal(gpi, false);

        gpi = new GridParmInfo(new ParmID("Wind", dbid, ParmID.defaultLevel()),
                gloc, GridType.VECTOR, "kts", "Surface Wind", 0.0f, 125.0f, 0,
                false, TC1, false);
        addInternal(gpi, true);

        gpi = new GridParmInfo(new ParmID("WindGust", dbid,
                ParmID.defaultLevel()), gloc, GridType.VECTOR, "kts",
                "Wind Gust", 0.0f, 125.0f, 0, false, TC1, false);
        addInternal(gpi, false);

        gpi = new GridParmInfo(new ParmID("Discrete", dbid,
                ParmID.defaultLevel()), gloc, GridType.DISCRETE, "",
                "TESTDiscrete", 0.0f, 2.0f, 0, false, TC1, false);
        addInternal(gpi, true);

        gpi = new GridParmInfo(new ParmID("Hazards", dbid,
                ParmID.defaultLevel()), gloc, GridType.DISCRETE, "",
                "TESTHazards", 0.0f, 2.0f, 0, false, TC1, false);
        addInternal(gpi, true);

        gpi = new GridParmInfo(new ParmID("Weather", dbid,
                ParmID.defaultLevel()), gloc, GridType.WEATHER, "",
                "TESTWeather", 0.0f, 2.0f, 0, false, TC1, false);
        addInternal(gpi, true);

        this.mockD2DGrids = new ParmID[4];
        this.mockD2DGrids[0] = new ParmID("T", mockD2Did, "MB650");
        this.mockD2DGrids[1] = new ParmID("T", mockD2Did, "MB750");
        this.mockD2DGrids[2] = new ParmID("T", mockD2Did, "MB850");
        this.mockD2DGrids[3] = new ParmID("T", mockD2Did, ParmID.defaultLevel());

        this.systemTimeRange = recalcSystemTimeRange();
    }

    private void addInternal(GridParmInfo gpi, boolean displayable) {
        ParmID pid = gpi.getParmID();
        Parm parm = new MockParm(pid, gpi, true, displayable, this.dataManager);
        this.parmIDs.add(pid);

        Calendar tmpCal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        tmpCal.set(Calendar.MINUTE, 0);
        tmpCal.set(Calendar.SECOND, 0);
        tmpCal.set(Calendar.MILLISECOND, 0);

        tmpCal.add(Calendar.HOUR_OF_DAY, TIME_OFFSET);

        // Hack for simulated data
        float[][] data = null;
        if (Activator.getDefault() != null
                && (gpi.getDescriptiveName().equals("Surface Temperature") || gpi
                        .getDescriptiveName().equals("Dewpoint"))) {
            UnitConverter conv = SI.KELVIN.getConverterTo(NonSI.FAHRENHEIT);

            data = new float[145][145];

            try {
                String fn = null;
                if (gpi.getDescriptiveName().equals("Surface Temperature")) {
                    fn = FileLocator.resolve(
                            FileLocator.find(
                                    Activator.getDefault().getBundle(),
                                    new Path("T.bin"), null)).getPath();
                } else {
                    fn = FileLocator.resolve(
                            FileLocator.find(
                                    Activator.getDefault().getBundle(),
                                    new Path("Td.bin"), null)).getPath();
                }
                FileInputStream fis = new FileInputStream(fn);
                DataInputStream dis = new DataInputStream(fis);

                for (int m = 0; m < 145; m++) {
                    for (int n = 0; n < 145; n++) {
                        float f = dis.readFloat();
                        f = (float) conv.convert(f);
                        data[m][n] = f;
                    }
                }
                dis.close();
                fis.close();

            } catch (FileNotFoundException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            } catch (ConversionException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            } catch (IOException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }

        Calendar tmpCal2 = (Calendar) tmpCal.clone();

        Calendar tmpCal3 = (Calendar) tmpCal.clone();
        tmpCal3.add(Calendar.HOUR_OF_DAY, HOURS_OF_DATA);
        // TimeRange fullTR = new TimeRange(tmpCal, tmpCal3);

        List<IGridData> mockGD = new ArrayList<IGridData>(HOURS_OF_DATA);

        for (int i = 0; i < HOURS_OF_DATA; i++) {
            tmpCal2.add(Calendar.HOUR_OF_DAY, 1);
            if (gpi.getGridType() == GridType.SCALAR) {
                ScalarGridSlice slice = new ScalarGridSlice(new TimeRange(
                        tmpCal, tmpCal2), gpi, new GridDataHistory[0],
                        new Grid2DFloat(145, 145));
                Grid2DFloat g2dFloat = slice.getScalarGrid();

                if (data != null) {
                    for (int m = 0; m < g2dFloat.getXdim(); m++) {
                        for (int n = 0; n < g2dFloat.getYdim(); n++) {
                            g2dFloat.set(m, n, data[m][n]);
                        }
                    }
                } else {
                    // Fake some data in the right range
                    float step = (gpi.getMaxValue() - gpi.getMinValue())
                            / (g2dFloat.getXdim() + g2dFloat.getYdim());
                    for (int m = 0; m < g2dFloat.getXdim(); m++) {
                        for (int n = 0; n < g2dFloat.getYdim(); n++) {
                            g2dFloat.set(m, n,
                                    ((float) m + n) * step + gpi.getMinValue());
                        }
                    }
                }

                mockGD.add(new ScalarGridData(parm, slice));

            } else if (gpi.getGridType() == GridType.VECTOR) {
                VectorGridSlice slice = new VectorGridSlice(new TimeRange(
                        tmpCal, tmpCal2), gpi, new GridDataHistory[0],
                        new Grid2DFloat(145, 145), new Grid2DFloat(145, 145));
                Grid2DFloat g2dFloat = slice.getDirGrid();
                for (int m = 0; m < g2dFloat.getXdim(); m++) {
                    for (int n = 0; n < g2dFloat.getYdim(); n++) {
                        g2dFloat.set(m, n, 90.0f);
                    }
                }

                g2dFloat = slice.getMagGrid();
                float step = (gpi.getMaxValue() - gpi.getMinValue())
                        / (g2dFloat.getXdim() + g2dFloat.getYdim());
                for (int m = 0; m < g2dFloat.getXdim(); m++) {
                    for (int n = 0; n < g2dFloat.getYdim(); n++) {
                        g2dFloat.set(m, n,
                                ((float) m + n) * step + gpi.getMinValue());
                    }
                }

                mockGD.add(new VectorGridData(parm, slice));

            } else if (gpi.getGridType() == GridType.DISCRETE) {
                ParmID parmId = gpi.getParmID();
                String siteId = parmId.getDbId().getSiteId();

                DiscreteKey tmpKey = DiscreteKey.defaultKey(siteId, parmId);

                DiscreteKey tmpKey2 = new DiscreteKey(siteId, "BZ.A", parmId);

                DiscreteGridSlice slice = new DiscreteGridSlice(new TimeRange(
                        tmpCal, tmpCal2), gpi, new GridDataHistory[0],
                        new Grid2DByte(145, 145), new DiscreteKey[] { tmpKey,
                                tmpKey2 });
                Grid2DByte g2dByte = slice.getDiscreteGrid();
                slice.assign(tmpKey);

                // ADDED TO DISPLAY SOME DISCRETE DATA
                for (int m = 0; m < g2dByte.getXdim(); m++) {
                    for (int n = 0; n < g2dByte.getYdim(); n++) {
                        if (m > 80 && m < 110 && n > 80 && n < 110) {
                            g2dByte.set(m, n, (byte) 1);
                        }
                        if (m > 90 && m < 100 && n > 110 && n < 140) {
                            g2dByte.set(m, n, (byte) 1);
                        }
                    }
                }// ADDED TO DISPLAY SOME DISCRETE DATA

                mockGD.add(new DiscreteGridData(parm, slice));

            } else if (gpi.getGridType() == GridType.WEATHER) {

                String siteId = gpi.getGridLoc().getSiteId();
                WeatherKey tmpKey = new WeatherKey(siteId,
                        "<NoCov>:<NoWx>:<NoInten>:<NoVis>:");

                WeatherKey tmpKey2 = new WeatherKey(siteId,
                        "Sct:T:<NoInten>:6SM:FL");
                //
                // tmpKey2.add(wxDefinition.instanceFromString("T"));

                WeatherGridSlice slice = new WeatherGridSlice(new TimeRange(
                        tmpCal, tmpCal2), gpi, new GridDataHistory[0],
                        new Grid2DByte(145, 145), new WeatherKey[] { tmpKey,
                                tmpKey2 });
                Grid2DByte g2dByte = slice.getWeatherGrid();
                slice.assign(tmpKey);

                // ADDED TO DISPLAY SOME DISCRETE DATA
                for (int m = 0; m < g2dByte.getXdim(); m++) {
                    for (int n = 0; n < g2dByte.getYdim(); n++) {
                        if (m > 80 && m < 110 && n > 80 && n < 110) {
                            g2dByte.set(m, n, (byte) 1);
                        }
                        if (m > 90 && m < 100 && n > 110 && n < 140) {
                            g2dByte.set(m, n, (byte) 1);
                        }
                    }
                }// ADDED TO DISPLAY SOME DISCRETE DATA

                mockGD.add(new DiscreteGridData(parm, slice));

            }
            tmpCal.add(Calendar.HOUR_OF_DAY, 1);
        }
        parm.setGrids(mockGD);

        this.fullParmSet.add(parm);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.parm.IParmManager#addParm(com.raytheon.viz.
     * gfe.core.parm.ParmID, boolean, boolean)
     */
    @Override
    public Parm addParm(ParmID pid, boolean mutableParm, boolean displayable) {
        Parm parm = new MockParm(pid, new GridParmInfo(pid, gloc,
                GridType.SCALAR, "F", "descriptive name", 0f, 100f, 0, false,
                TC1, false), mutableParm, displayable, this.dataManager);
        parms.acquireWriteLock();
        try {
            if (!this.parms.contains(parm)) {
                this.parms.add(parm);
            }
        } finally {
            parms.releaseWriteLock();
        }
        parm.getDisplayAttributes().setDisplayable(displayable);
        return parm;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.parm.IParmManager#createVirtualParm(com.raytheon
     * .viz.gfe.core.parm.ParmID,
     * com.raytheon.edex.plugin.gfe.db.objects.GridParmInfo,
     * com.raytheon.edex.plugin.gfe.slice.IGridSlice, boolean, boolean)
     */
    @Override
    public Parm createVirtualParm(ParmID pid, GridParmInfo gpi,
            IGridSlice[] data, boolean mutableParm, boolean displayable) {
        Parm parm = new MockParm(pid, gpi, mutableParm, displayable,
                this.dataManager);

        IGridData grid = null;
        if (data == null || data.length == 0) {
            grid = null;
        } else if (gpi.getGridType() == GridType.SCALAR) {
            grid = new ScalarGridData(parm, data[0]);
        } else if (gpi.getGridType() == GridType.VECTOR) {
            grid = new VectorGridData(parm, data[0]);
        } else if (gpi.getGridType() == GridType.DISCRETE) {
            grid = new DiscreteGridData(parm, data[0]);
        }
        List<IGridData> grids = Arrays.asList(grid);
        parm.setGrids(grids);
        return parm;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.parm.IParmManager#getAllParms()
     */
    @Override
    public Parm[] getAllParms() {
        parms.acquireReadLock();
        try {
            return this.fullParmSet.toArray(new Parm[parms.size()]);
        } finally {
            parms.releaseReadLock();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.parm.IParmManager#saveParm(com.raytheon.viz
     * .gfe.core.parm.Parm)
     */
    @Override
    public boolean saveParm(Parm parm) {
        return false;

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.parm.IParmManager#saveParm(com.raytheon.viz
     * .gfe.core.parm.Parm, com.raytheon.uf.common.time.TimeRange[])
     */
    @Override
    public boolean saveParm(Parm parm, TimeRange[] timeRanges) {
        return false;

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.parm.IParmManager#isParmInDatabase(com.raytheon
     * .viz.gfe.core.parm.ParmID)
     */
    @Override
    public boolean isParmInDatabase(ParmID parmId) {
        for (Parm parm : this.fullParmSet) {
            if (parm.getParmID().equals(parmId)) {
                return true;
            }
        }

        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.IParmManager#getAvailableDbs()
     */
    @Override
    public List<DatabaseID> getAvailableDbs() {
        return Arrays.asList(new DatabaseID("OAX_GRID__Fcst_00000000_0000"),
                new DatabaseID("OAX_GRID_D2D_NAM12_20080320_0000"));
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.IParmManager#getDisplayedDbs()
     */
    @Override
    public List<DatabaseID> getDisplayedDbs() {
        // TODO Auto-generated method stub
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.IParmManager#getUndisplayedDbs()
     */
    @Override
    public List<DatabaseID> getUndisplayedDbs() {
        // TODO Auto-generated method stub
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.IParmManager#getAvailableParms(com.raytheon
     * .edex.plugin.gfe.db.objects.DatabaseID)
     */
    @Override
    public ParmID[] getAvailableParms(DatabaseID dbID) {

        if (dbID.getModelName().equals("Fcst")) {
            return this.parmIDs.toArray(new ParmID[this.parmIDs.size()]);
        } else {
            return this.mockD2DGrids;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.IParmManager#getUniqueParmID(com.raytheon.edex
     * .plugin.gfe.db.objects.ParmID, java.lang.String, java.lang.String)
     */
    @Override
    public ParmID getUniqueParmID(final ParmID pid, final String nameHint,
            final String categoryHint) {
        return pid;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.IParmManager#getSystemTimeRange()
     */
    @Override
    public TimeRange getSystemTimeRange() {
        return this.systemTimeRange;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.IParmManager#deleteParm(com.raytheon.viz.gfe
     * .core.parm.Parm[])
     */
    @Override
    public void deleteParm(final Parm... parms) {
        if (parms.length == 0) {
            return; // nothing to do
        }

        List<Parm> toBeDeleted = new ArrayList<Parm>();

        this.parms.acquireReadLock();
        try {
            for (int i = 0; i < parms.length; i++) {
                if (!this.parms.contains(parms[i])) {
                    throw new IllegalArgumentException(
                            "Attempt to delete unknown parm: "
                                    + parms[i].getParmID().toString());
                }

                // skip modified parms
                if (!parms[i].isModified()) {
                    toBeDeleted.add(parms[i]);
                } else {
                    throw new IllegalArgumentException("Skipping parm: "
                            + parms[i].getParmID() + " due to modified state.");
                }
            }
        } finally {
            this.parms.releaseReadLock();
        }

        // List<ParmID> ids = new ArrayList<ParmID>();
        this.parms.acquireWriteLock();
        try {
            for (int i = 0; i < toBeDeleted.size(); i++) {
                if (this.parms.contains(toBeDeleted.get(i))) {
                    this.parms.remove(toBeDeleted.get(i));
                    // ids.add(toBeDeleted.get(i).getParmID());
                }
            }
        } finally {
            this.parms.releaseWriteLock();
        }

        // continue the command to remove the specified ids
        // TODO
        // setParms(null, ids);

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.IParmManager#getMutableDatabase()
     */
    @Override
    public DatabaseID getMutableDatabase() {
        return this.mutableDb;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.IParmManager#getOrigMutableDatabase()
     */
    @Override
    public DatabaseID getOrigMutableDatabase() {
        return this.mutableDb;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.internal.AbstractParmManager#getDataManager()
     */
    @Override
    protected DataManager getDataManager() {
        return this.dataManager;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.IParmManager#compositeGridLocation()
     */
    @Override
    public GridLocation compositeGridLocation() {
        // TODO: get this value from the server
        return gloc;
    }

    @Override
    public Parm getParmInExpr(final String exprName, boolean enableTopo) {
        return getParmInExpr(exprName, enableTopo, dataManager
                .getSpatialDisplayManager().getActivatedParm());
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.internal.AbstractParmManager#createParmInternal
     * (com.raytheon.edex.plugin.gfe.db.objects.ParmID, boolean, boolean)
     */
    @Override
    protected Parm createParmInternal(ParmID pid, boolean mutableParm,
            boolean displayable) {

        for (Parm p : this.fullParmSet) {
            if (p.getParmID().equals(pid)) {
                return p;
            }
        }

        return new MockParm(pid, new GridParmInfo(), mutableParm, displayable,
                this.dataManager);
    }

    // -- private
    // ----------------------------------------------------------------
    // ParmMgr::setParms()
    // Adds/removes/displaystatechanges the set of given parms. Handles the
    // bookkeeping and sends out notifications.
    // -- implementation
    // ---------------------------------------------------------
    // This is the complicated routine which handles all of the bookkeeping
    // and the notifications. Should never see a NULL parm* given to this
    // routine.
    // ---------------------------------------------------------------------------
    @Override
    protected void setParms(final Collection<Parm> addParms,
            final Collection<Parm> removeParms,
            final Collection<Parm> displayedStateModParms) {
        // logDebug << "--- setParms(parmsToAdd,parmsToRemov,displayedStateMod):
        // "
        // << printAMR(addParms, displayedStateModParms, removeParms) <<
        // std::endl;

        // update list of parms
        parms.acquireWriteLock();
        try {
            for (Parm addParm : addParms) {
                if (addParm != null && !this.parms.contains(addParm)) {
                    this.parms.add(addParm); // add the additions
                }
            }

            for (Parm removeParm : removeParms) {
                if (removeParm != null && this.parms.contains(removeParm)) {
                    this.parms.remove(removeParm);
                }
            }
        } finally {
            parms.releaseWriteLock();
        }

        // recalculate the system time range changes, send notification
        TimeRange newSysTR = recalcSystemTimeRange();
        if (!newSysTR.equals(this.systemTimeRange)) {
            this.systemTimeRange = newSysTR;
            // TODO
            // SystemTimeRangeChangedMsg::send(_msgHand, _systemTimeRange);
        }

        // send ParmListChanged notification
        if (addParms.size() > 0 || removeParms.size() > 0) {
            parms.acquireReadLock();
            try {
                fireParmListChanged(
                        this.parms.toArray(new Parm[this.parms.size()]),
                        addParms.toArray(new Parm[addParms.size()]),
                        removeParms.toArray(new Parm[removeParms.size()]));
            } finally {
                parms.releaseReadLock();
            }
        }

        List<Parm> addedDisplayed = new ArrayList<Parm>();
        List<Parm> removedDisplayed = new ArrayList<Parm>();
        for (Parm p : addParms) {
            if (p.getDisplayAttributes().isDisplayable()) {
                addedDisplayed.add(p);
            }
        }

        for (Parm p : removeParms) {
            if (p.getDisplayAttributes().isDisplayable()) {
                removedDisplayed.add(p);
            }
        }

        // send DisplayedParmListChanged notification
        if (removedDisplayed.size() > 0 || addedDisplayed.size() > 0) {
            parms.acquireReadLock();
            try {
                fireDisplayedParmListChanged(this.parms
                        .toArray(new Parm[this.parms.size()]), addedDisplayed
                        .toArray(new Parm[addedDisplayed.size()]),
                        removedDisplayed.toArray(new Parm[removedDisplayed
                                .size()]));
            } finally {
                parms.releaseReadLock();
            }
        }

        // send AvailableSourcesChanged notification
        // TODO
        // if (addedDbs.length() || remDbs.length())
        // AvailableSourcesChangedMsg::send(_msgHand, nowDbs, remDbs, addedDbs);

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.IParmManager#enableDisableTopoParm(boolean,
     * boolean)
     */
    @Override
    public void enableDisableTopoParm(boolean wanted, boolean forceVisibility) {
        // TODO Auto-generated method stub

    }

    @Override
    public List<DatabaseID> getIscDatabases() {
        return new ArrayList<DatabaseID>();
    }

    @Override
    public ParmID getISCParmID(ParmID pid) {
        return new ParmID();
    }

    @Override
    public boolean iscMode() {
        return false;
    }

    @Override
    public void purgeDbCacheForSite(String site) {
        // Do nothing

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.IParmManager#createParm(com.raytheon.uf.common
     * .dataplugin.gfe.db.objects.ParmID, boolean, boolean)
     */
    @Override
    public Parm createParm(ParmID pid, boolean mutableParm, boolean displayable) {
        // TODO Auto-generated method stub
        return null;
    }

}
