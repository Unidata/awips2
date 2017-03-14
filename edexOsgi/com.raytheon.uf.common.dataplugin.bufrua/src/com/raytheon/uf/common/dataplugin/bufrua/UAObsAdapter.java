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
package com.raytheon.uf.common.dataplugin.bufrua;

import static com.raytheon.uf.common.sounding.SoundingLayer.MISSING;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.sounding.LayerType;
import com.raytheon.uf.common.sounding.SoundingLayer;
import com.raytheon.uf.common.sounding.VerticalSounding;
import com.raytheon.uf.common.sounding.adapter.AbstractVerticalSoundingAdapter;

/**
 * Adapter for convertung UAObs data into Vertical Soundings.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 19, 2013 1992       bsteffen    Remove redundant time columns from
 *                                     bufrua.
 * Aug 18, 2014 3530       bclement    removed dead code
 * 
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class UAObsAdapter extends AbstractVerticalSoundingAdapter {

    private boolean belowSurface = false;

    private static final Map<String, String> WMO_STN_MAP = new HashMap<String, String>();
    static {
        WMO_STN_MAP.put("72201", "KEYW");
        WMO_STN_MAP.put("72203", "KPBI");
        WMO_STN_MAP.put("72208", "KCHS");
        WMO_STN_MAP.put("72210", "KTBW");
        WMO_STN_MAP.put("72213", "KAYS");
        WMO_STN_MAP.put("72220", "KAQQ");
        WMO_STN_MAP.put("72229", "KCKL");
        WMO_STN_MAP.put("72232", "KBVE");
        WMO_STN_MAP.put("72235", "KJAN");
        WMO_STN_MAP.put("72240", "KLCH");
        WMO_STN_MAP.put("72247", "KGGG");
        WMO_STN_MAP.put("72250", "KBRO");
        WMO_STN_MAP.put("72255", "KVCT");
        WMO_STN_MAP.put("72249", "KFWD"); // 72250 SEP
        WMO_STN_MAP.put("72261", "KDRT");
        WMO_STN_MAP.put("72265", "KMAF");
        WMO_STN_MAP.put("72270", "KELP");
        WMO_STN_MAP.put("72274", "KTUS");
        WMO_STN_MAP.put("72290", "KSAN");
        WMO_STN_MAP.put("72304", "KHAT");
        WMO_STN_MAP.put("72311", "KAHN");
        WMO_STN_MAP.put("72317", "KGSO");
        WMO_STN_MAP.put("72318", "KRNK");
        WMO_STN_MAP.put("72327", "KBNA");
        WMO_STN_MAP.put("72340", "KLIT");
        WMO_STN_MAP.put("72349", "KUMN");
        WMO_STN_MAP.put("72357", "KOUN"); // 72353 OKC
        WMO_STN_MAP.put("72363", "KAMA");
        WMO_STN_MAP.put("72365", "KABQ");
        WMO_STN_MAP.put("72374", "KINW");
        WMO_STN_MAP.put("72393", "KVBG");
        WMO_STN_MAP.put("72403", "KIAD");
        WMO_STN_MAP.put("72407", "KACY");
        WMO_STN_MAP.put("72425", "KHTS");
        WMO_STN_MAP.put("72429", "KDAY");
        WMO_STN_MAP.put("72433", "KSLO");
        WMO_STN_MAP.put("72451", "KDDC");
        WMO_STN_MAP.put("72456", "KTOP");
        WMO_STN_MAP.put("72469", "KDEN");
        WMO_STN_MAP.put("72476", "KGJT");
        WMO_STN_MAP.put("72493", "KOAK");
        WMO_STN_MAP.put("72518", "KALB");
        WMO_STN_MAP.put("72520", "KPIT");
        WMO_STN_MAP.put("72528", "KBUF");
        WMO_STN_MAP.put("72532", "KPIA");
        WMO_STN_MAP.put("72558", "KOAX");
        WMO_STN_MAP.put("72562", "KLBF");
        WMO_STN_MAP.put("72572", "KSLC");
        WMO_STN_MAP.put("72576", "KLND");
        WMO_STN_MAP.put("72583", "KWMC");
        WMO_STN_MAP.put("72597", "KMFR");
        WMO_STN_MAP.put("72606", "KPWM");
        WMO_STN_MAP.put("72637", "KFNT");
        WMO_STN_MAP.put("72645", "KGRB");
        WMO_STN_MAP.put("72654", "KHON");
        WMO_STN_MAP.put("72655", "KSTC");
        WMO_STN_MAP.put("72659", "KABR");
        WMO_STN_MAP.put("72662", "KRAP");
        WMO_STN_MAP.put("72681", "KBOI");
        WMO_STN_MAP.put("72694", "KSLE");
        WMO_STN_MAP.put("72712", "KCAR");
        WMO_STN_MAP.put("72734", "KSSM");
        WMO_STN_MAP.put("72747", "KINL");
        WMO_STN_MAP.put("72764", "KBIS");
        WMO_STN_MAP.put("72768", "KGGW");
        WMO_STN_MAP.put("72775", "KGTF");
        WMO_STN_MAP.put("72785", "KGEG");
        WMO_STN_MAP.put("72797", "KUIL");
    };

    public void setBelowSurface(boolean belowSurface) {
        this.belowSurface = belowSurface;
    }

    @Override
    public VerticalSounding[] createSoundings() {
        UAObs[] uaObs = new UAObs[0];
        if (objects != null) {
            uaObs = new UAObs[objects.length];
            for (int i = 0; i < objects.length; i++) {
                uaObs[i] = (UAObs) objects[i];
            }
        }
        UAObs[] aggregatedObs = UAObsAdapter.aggregateObsData(flatten(uaObs));
        ArrayList<VerticalSounding> soundings = new ArrayList<VerticalSounding>(
                aggregatedObs.length);
        for (UAObs obs : aggregatedObs) {
            VerticalSounding sounding = createVerticalSounding(obs);
            if (sounding != null) {
                soundings.add(sounding);
            }
        }

        return soundings.toArray(new VerticalSounding[soundings.size()]);
    }

    /**
     * Get the data from this upper air data object as a VerticalSounding.
     * 
     * @return The created VerticalSounding.
     * @see com.raytheon.edex.upperair.IVerticalSoundingCreator#createVerticalSounding()
     */
    public VerticalSounding createVerticalSounding(UAObs obsData) {
        VerticalSounding sounding = null;

        if (obsData != null && obsData.getLevels() != null) {
            sounding = new VerticalSounding();
            sounding.setSpatialInfo(obsData.getLocation());
            sounding.setElevation(obsData.getElevation());
            sounding.setStationId(obsData.getStationId());
            String bsn = obsData.getStationId();
            // TODO: quit using this hard coded station map
            // String icao = WMO_STN_MAP.get(bsn);
            // if (icao != null) {
            // sounding.setName(icao);
            // } else {
            // sounding.setName(String.format("%5s", bsn));
            // }
            String icao = obsData.getStationName();
            if (icao == null) {
                icao = String.format("%5s", bsn);
            }
            sounding.setDisplayFormat(icao);
            sounding.setName(icao);

            sounding.setObsTime(obsData.getDataTime().getRefTimeAsCalendar());
            sounding.setDataTime(obsData.getDataTime());

            List<SoundingLayer> layers = interleave(obsData);
            sounding.addLayers(layers);

            sounding.checkSfcLayer();
            if (!belowSurface) {
                sounding.removeBelowSfcLayers();
            }
            // recompute and correct after removing below sfc layers.
            layers = sounding.getLayerData();
            List<SoundingLayer> fixLyrs = new ArrayList<SoundingLayer>();
            fixLyrs.addAll(layers);
            sounding.addLayers(fixLyrs);
            sounding.setInitialLoad(false);
            sounding.invalidate();

            if (sounding.size() == 0) {
                sounding = null;
            }
        }
        return sounding;
    }

    /**
     * 
     * @param level
     * @return
     */
    private SoundingLayer createLayer(UAObsLevel level) {
        SoundingLayer layer = new SoundingLayer();
        layer.setLayerType(convertLayerType(level.getVertSig()));
        // set up default values
        // Double losh = null;
        // Double hish = null;

        Integer press = level.getPressure();
        if (inRange(0.0, press, 120000.0)) {
            // Pa --> hPa
            layer.setPressure(press.floatValue() / 100);
        }
        Integer z = level.getGeoHeight();
        if (inRange(0.0, z, 30000.0)) {
            layer.setGeoHeight(z.floatValue());
        }
        Integer dir = level.getWindDirection();
        if (inRange(0.0, dir, 360.0)) {
            layer.setWindDirection(dir.floatValue());
        }
        Double temp = level.getTemp();
        if (inRange(0.0, temp, 373.0)) {
            // temp = temp;
            layer.setTemperature(temp.floatValue());
        }

        Double dew = level.getDwpt();
        if (inRange(0.0, dew, 373.0)) {
            // dew = dew - 273.13;
            layer.setDewpoint(dew.floatValue());
        }
        Double ws = level.getWindSpeed();
        if (inRange(0.0, ws, 300.0)) {
            layer.setWindSpeed(ws.floatValue());
        }
        // losh = level.getLoShear();
        // if (losh != null) {
        // layer.setLoShear(losh);
        // } else {
        // layer.setLoShear(SoundingLayer.MISSING);
        // }
        //
        // hish = level.getHiShear();
        //
        // if (hish != null) {
        // layer.setHiShear(hish);
        // } else {
        // layer.setHiShear(SoundingLayer.MISSING);
        // }

        return layer;
    }

    /**
     * Convert the vertical significance to LayerType.
     * 
     * @param vertSig
     *            The vertical significance from the observation.
     * @return The created layer type. If a correspondence cannot be found, a
     *         LayerType of GENERIC is returned.
     */
    private LayerType convertLayerType(Integer vertSig) {
        LayerType layerType = LayerType.GENERIC;
        if (vertSig != null) {
            switch (vertSig) {
            case 2: {
                layerType = LayerType.SIG_WIND;
                break;
            }
            case 4: {
                layerType = LayerType.SIG_PRESSURE;
                break;
            }
            case 8: {
                layerType = LayerType.MAX_WIND;
                break;
            }
            case 16: {
                layerType = LayerType.TROPOPAUSE;
                break;
            }
            case 32: {
                layerType = LayerType.MAN_PRESSURE;
                break;
            }
            case 64: {
                layerType = LayerType.SURFACE;
                break;
            }
            default: {
                layerType = LayerType.GENERIC;
                break;
            }
            }

        }
        return layerType;
    }

    /**
     * 
     * @param wmoIndex
     * @return
     */
    public static String getClimoStationName(String wmoIndex) {
        String s = WMO_STN_MAP.get(wmoIndex);
        if (s == null) {
            s = "---";
        }
        return s;
    }

    /**
     * 
     * @param levels
     * @return
     */
    private List<SoundingLayer> interleave(UAObs obsData) {

        List<SoundingLayer> layers = new ArrayList<SoundingLayer>();

        int sfcPres = (int) MISSING;

        List<UAObsLevel> levels = obsData.getLevels();
        if ((levels != null) && (levels.size() > 1)) {

            for (UAObsLevel level : levels) {
                switch (level.getVertSig()) {
                case LayerTools.SIGPRE_LEVEL:
                case LayerTools.TROP_LEVEL:
                case LayerTools.MANPRE_LEVEL:
                case LayerTools.SFC_LEVEL: {
                    SoundingLayer layer = createLayer(level);
                    if (LayerTools.SFC_LEVEL == level.getVertSig()) {
                        sfcPres = obsData.getPressure_station() / 100;
                        layer.setGeoHeight(obsData.getElevation());
                    }
                    layers.add(layer);
                    break;
                }
                default: {
                    // Nothing - Don't do anything with
                    // SIGWND_LEVEL and MAXWND_LEVEL data yet.
                }
                } // switch()
            }
            fixupHeights(layers);
            for (int i = 0; i < layers.size();) {
                SoundingLayer layer = layers.get(i);
                double p = layer.getPressure();
                if (p > sfcPres) {
                    layers.remove(i);
                } else {
                    if (layer.getDewpoint() >= MISSING
                            && layer.getTemperature() < MISSING) {
                        layer.setDewpoint(layer.getTemperature() - 50);
                        layer.setDptInterpolated(true);
                    }
                    i++;
                }
            }

            // now add the significant level wind data.
            for (UAObsLevel level : levels) {
                if (level.getVertSig() == LayerTools.SIGWND_LEVEL) {
                    if (level.getGeoHeight() > 0) {
                        layers.add(createLayer(level));
                    }
                }
                if (level.getVertSig() == LayerTools.MAXWND_LEVEL) {
                    layers.add(createLayer(level));
                }
            }
            // Removed call to method to make data values same as AWIPS I
            // fixupWithWinds(layers);
        }

        return layers;
    }

    /**
     * 
     * @param layers
     */
    private static void fixupHeights(List<SoundingLayer> layers) {
        // Ensure the data is sorted by pressure.
        Collections.sort(layers, SoundingLayer.getPressureComparator());
        for (int i = 0; i < layers.size() - 1;) {
            if (checkLayers(layers, i, i + 1)) {
                continue;
            }
            i++;
        }

        for (int i = 1; i < layers.size(); i++) {
            SoundingLayer layer = layers.get(i);
            // Do we have a valid geoheight?
            if (layer.getGeoHeight() < MISSING) {
                continue;
            }
            // Now we have a level that needs a height.
            SoundingLayer layerblo = layers.get(i - 1);

            float tv1 = layer.getVirtualTemp();
            float tv2 = layerblo.getVirtualTemp();
            if ((tv1 != MISSING) && (tv2 != MISSING)) {
                float mtv = ((tv1 + tv2) / 2);

                double delta = Math.log(layerblo.getPressure())
                        - Math.log(layer.getPressure());
                delta *= 29.2898 * mtv;

                layer.setGeoHeight(layerblo.getGeoHeight() + (float) delta);
                layer.setHgtInterpolated(true);
            } else {

            }
        }
    }

    /**
     * 
     * @param layers
     * @param idxA
     * @param idxB
     * @return
     */
    private static boolean checkLayers(List<SoundingLayer> layers, int idxA,
            int idxB) {
        boolean removed = false;

        SoundingLayer a = layers.get(idxA);
        SoundingLayer b = layers.get(idxB);

        if (a.getPressure() == b.getPressure()) {
            if (LayerType.MAN_PRESSURE.equals(a.getLayerType())) {
                layers.remove(idxB);
                removed = true;
            } else if (LayerType.MAN_PRESSURE.equals(b.getLayerType())) {
                layers.remove(idxA);
                removed = true;
            } else if (LayerType.SURFACE.equals(a.getLayerType())) {
                layers.remove(idxB);
                removed = true;
            } else if (LayerType.SURFACE.equals(b.getLayerType())) {
                layers.remove(idxA);
                removed = true;
            } else if (LayerType.SIG_PRESSURE.equals(a.getLayerType())) {
                layers.remove(idxB);
                removed = true;
            } else if (LayerType.SIG_PRESSURE.equals(b.getLayerType())) {
                layers.remove(idxA);
                removed = true;
            } else if (LayerType.SIG_WIND.equals(b.getLayerType())) {
                layers.remove(idxB);
                removed = true;
            } else if (LayerType.SIG_WIND.equals(b.getLayerType())) {
                layers.remove(idxA);
                removed = true;
            } else if (LayerType.TROPOPAUSE.equals(b.getLayerType())) {
                layers.remove(idxB);
                removed = true;
            } else if (LayerType.TROPOPAUSE.equals(b.getLayerType())) {
                layers.remove(idxA);
                removed = true;
            } else if (LayerType.MAX_WIND.equals(b.getLayerType())) {
                layers.remove(idxB);
                removed = true;
            } else if (LayerType.MAX_WIND.equals(b.getLayerType())) {
                layers.remove(idxA);
                removed = true;
            }
        }
        return removed;
    }

    /**
     * 
     * @param observations
     *            Group of observation data to aggregate.
     * @return
     */
    public static UAObs[] aggregateObsData(UAObs[] observations) {

        ArrayList<UAObs> obsList = new ArrayList<UAObs>();
        HashMap<ObsMapKey, UAObs> obsMap = new HashMap<ObsMapKey, UAObs>();

        for (UAObs obs : observations) {
            if (obs.getReportType() < LayerTools.MANLVL_HI) {
                ObsMapKey key = new ObsMapKey(obs);
                if (obsMap.containsKey(key)) {
                    if (obs.getLevels() != null) {
                        UAObs aggregate = obsMap.get(key);
                        // check to see if another datatype was put into the map
                        // before we found the mandatory level data.
                        if (obs.getReportType() == LayerTools.MANLVL_LO) {
                            // If so, replace the current aggregate with the
                            // manlevel data
                            obsMap.put(key, obs);
                            UAObs t = obs;
                            // and set up so we will swap the aggregate and the
                            // obs.
                            obs = aggregate;
                            aggregate = t;
                            // make sure to update the obsList information also!
                            obsList.remove(obs);
                            obsList.add(aggregate);
                        }
                        List<UAObsLevel> levels = obs.getLevels();
                        if (levels != null) {
                            for (UAObsLevel level : levels) {
                                aggregate.addLevel(level);
                            }
                        }
                    }
                } else {
                    obsMap.put(key, obs);
                    obsList.add(obs);
                }
            }
        }
        UAObs[] data = new UAObs[0];
        if (obsList.size() > 0) {
            data = obsList.toArray(new UAObs[obsList.size()]);
        }
        return data;
    }

    /**
     * 
     * @param data
     * @return
     */
    private static UAObs[] flatten(UAObs[] data) {
        if (data != null) {
            HashMap<String, UAObs> obsMap = new HashMap<String, UAObs>();
            for (UAObs obs : data) {
                if (obs.getReportType() < LayerTools.MANLVL_HI) {
                    String key = getKey(obs);
                    if (obsMap.containsKey(key)) {
                        UAObs mapped = obsMap.get(key);

                        // if compare(mapped,obs) < 0 then mapped has newer data
                        int c = UAObs.getCorComparator().compare(mapped, obs);
                        if (c < 0) {
                            obsMap.put(key, obs);
                        }
                    } else {
                        obsMap.put(key, obs);
                    }
                }
            }
            data = obsMap.values().toArray(new UAObs[obsMap.values().size()]);
        }
        return data;
    }

    /**
     * Create a key for a UAObs by concatenating the station identifier, report
     * type code and the valid time in milliseconds.
     * 
     * @param data
     *            A UAObs instance to key.
     * @return The generated key, returns null if the UAObs reference is null.
     */
    private static String getKey(UAObs data) {
        String key = null;
        if (data != null) {
            key = String.format("%s:%4d:%d", data.getStationId(),
                    data.getReportType(), data.getDataTime().getRefTime()
                            .getTime());
        }
        return key;
    }

    /**
     * 
     * @param lo
     * @param val
     * @param hi
     * @return
     */
    public static boolean inRange(Double lo, Double val, Double hi) {
        boolean retValue = false;
        if (val != null) {
            retValue = (lo <= val) && (val <= hi);
        }
        return retValue;
    }

    /**
     * 
     * 
     * @param lo
     * @param val
     * @param hi
     * @return
     */
    public static boolean inRange(Double lo, Integer val, Double hi) {
        boolean retValue = false;
        if (val != null) {
            retValue = (lo <= val) && (val <= hi);
        }
        return retValue;
    }

    /**
     * 
     * @param args
     */
    public static final void main(String[] args) {

        // Set<UAObsLevel> levels = new HashSet<UAObsLevel>();
        //
        // levels.add(createLevel(85000, 1526, 13.8 + 273.13, 5.8 + 273.13,
        // LayerTools.MANPRE_LEVEL));
        // levels.add(createLevel(84500, -1, 13.6 + 273.13, 5.6 + 273.13,
        // LayerTools.SIGPRE_LEVEL));
        // levels.add(createLevel(81990, -1, 12 + 273.13, 5.5 + 273.13,
        // LayerTools.SIGPRE_LEVEL));
        // levels.add(createLevel(81100, -1, 11.4 + 273.13, 5.4 + 273.13,
        // LayerTools.SIGPRE_LEVEL));
        // levels.add(createLevel(80300, -1, 10.8 + 273.13, 6.5 + 273.13,
        // LayerTools.SIGPRE_LEVEL));
        // levels.add(createLevel(79050, -1, 9.7 + 273.13, 6.2 + 273.13,
        // LayerTools.SIGPRE_LEVEL));
        // levels.add(createLevel(76200, -1, 7.1 + 273.13, 5.4 + 273.13,
        // LayerTools.SIGPRE_LEVEL));
        // levels.add(createLevel(48000, -1, -30 + 273.13, -40 + 273.13,
        // LayerTools.TROP_LEVEL));
        // levels.add(createLevel(76100, -1, 7.0 + 273.13, 5.4 + 273.13,
        // LayerTools.SIGPRE_LEVEL));
        // levels.add(createLevel(74000, -1, 5.6 + 273.13, 2.0 + 273.13,
        // LayerTools.SIGPRE_LEVEL));
        // levels.add(createLevel(73420, -1, 5.0 + 273.13, 1.0 + 273.13,
        // LayerTools.SIGPRE_LEVEL));
        // levels.add(createLevel(41000, -1, -1, -1, LayerTools.MAXWND_LEVEL));
        // levels.add(createLevel(72800, -1, 4.4 + 273.13, 0.0 + 273.13,
        // LayerTools.SIGPRE_LEVEL));
        // levels.add(createLevel(72000, -1, 3.6 + 273.13, 0.3 + 273.13,
        // LayerTools.SIGPRE_LEVEL));
        // levels.add(createLevel(70500, -1, 2.4 + 273.13, -2.6 + 273.13,
        // LayerTools.SIGPRE_LEVEL));
        // levels.add(createLevel(70000, 3131, 2.0 + 273.13, -1.6 + 273.13,
        // LayerTools.MANPRE_LEVEL));
        // levels.add(createLevel(89000, 1526, 16.8 + 273.13, 7.8 + 273.13,
        // LayerTools.SFC_LEVEL));
        // levels.add(createLevel(-1, 1640, -1, -1, LayerTools.SIGWND_LEVEL));
        // levels.add(createLevel(-1, 1526, -1, -1, LayerTools.SIGWND_LEVEL));
        // levels.add(createLevel(-1, 2099, -1, -1, LayerTools.SIGWND_LEVEL));
        // levels.add(createLevel(-1, 2510, -1, -1, LayerTools.SIGWND_LEVEL));
        // levels.add(createLevel(-1, 2830, -1, -1, LayerTools.SIGWND_LEVEL));
        //
        // UAObs obs = new UAObs();
        // SurfaceObsLocation loc = new SurfaceObsLocation();
        // loc.assignLocation(45, -90);
        // loc.setElevation(1400);
        // loc.setStationId("TEST");
        // obs.setLocation(loc);
        // obs.setLevels(levels);
        //
        // UAObsAdapter adapter = new UAObsAdapter();
        // adapter.setObjects(new PluginDataObject[] { obs });
        // VerticalSounding[] v = adapter.createSoundings();
        //
        // Iterator<SoundingLayer> it = v[0].iterator();
        // while (it.hasNext()) {
        // System.out.println(it.next());
        // }

        SurfaceObsLocation loc = new SurfaceObsLocation("72558");
        UAObs[] obs = new UAObs[2];
        obs[0] = new UAObs();
        obs[0].setLocation(loc);
        obs[0].setReportType(2020);
        obs[0].setCorIndicator(null);

        loc = new SurfaceObsLocation("72558");
        obs[1] = new UAObs();
        obs[1].setLocation(loc);
        obs[1].setReportType(2020);
        obs[1].setCorIndicator("CCA");

        obs = flatten(obs);

    }

}
