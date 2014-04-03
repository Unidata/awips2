package com.raytheon.uf.edex.plugin.acarssounding.tools;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.TimeZone;

import org.apache.commons.logging.Log;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.acars.ACARSRecord;
import com.raytheon.uf.common.dataplugin.acarssounding.ACARSSoundingLayer;
import com.raytheon.uf.common.dataplugin.acarssounding.ACARSSoundingRecord;
import com.raytheon.uf.common.pointdata.spatial.AircraftObsLocation;
import com.raytheon.uf.edex.decodertools.time.TimeTools;

/**
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * ??			           ??           Initial creation
 * Feb 24, 2014 DR15038    M.Porricelli Modified 'accept' to
 *                                      not discard sounding data
 *                                      based on altitude here
 */

public final class ACARSSoundingTools {

    // 30 minute offset to apply to observation time.
    public static final String TIMEOFFSET = Long
            .toString(TimeTools.MILLIS_HOUR / 2L);

    public static final String FMT = "%s%2$tY%2$tm%2$td%2$tH";

    public static final String STD_TM_FMT = "%1$tY-%1$tm-%1$td %1$tH:%1$tM:%1$tS";

    public static final String ACARS_PLUGIN_NAME = "acars";

    public static final String ACARS_SNDG_PLUGIN_NAME = "acarssounding";

    public static final String BASE_PATH = "acars/";

    public static final String DATA_PATH = "acftObs/";

    public static final String SNDG_INFO = "soundingInfo";

    public static final String RAW_PATH = "rawdata/";

    public static final String DATAFILE = "acarsdata";

    public static final byte ACARS_OLD_OBS = '+';

    public static final String ACARS_NEW_OBS = "X";

    public static final boolean NO_APPEND = false;

    public static final boolean APPEND = true;

    public static final int CUTOFF_HOURS = 6;

    public static final int OBS_TIME_SIZE = 20;
    
    // Only use 100 for size of the dataURI - ACARS dataURIs don't get much
    // bigger than about 90 at most.
    public static final String OUT_FMT = ACARS_NEW_OBS + "X:%"
            + String.valueOf(OBS_TIME_SIZE) + "d:%-100s\n";

    public static final String OBS_FMT = "%" + String.valueOf(OBS_TIME_SIZE)
            + "d:%s\n";

    public static final TimeZone UTC_TZ = TimeZone.getTimeZone("Zulu");

    public static final int MIN_OBS_FOR_SOUNDING = 6;

    public static final double FEET_TO_METERS = 3.28084;

    // The first layer must be at or below this altitude. When using this
    // value, ensure that the aircraft altitude is expressed as above
    // ground level (agl).
    public static final double MAX_FIRST = 2000 / FEET_TO_METERS;

    // Minimum depth of the sounding data
    public static final double MIN_DEPTH = 10000 / FEET_TO_METERS;

    // Maximum altitude change between observations before a break
    // must be considered.
    public static final double MIN_DELTA_ALT = 2500 / FEET_TO_METERS;

    // Maximum time in minutes between observations before a break
    // must be considered.
    public static final long BREAK_TIME_DIFF = 20 * 60 * 1000;

    // Maximum distance from an airport in kilometers
    public static final double MAX_DISTANCE = 50;

    // 0 3 Level flight, routine observation, unsteady
    // 1 4 Level flight, highest wind encountered, unsteady
    // 2 2 Unsteady (UNS)
    // 3 3 Level flight, routine observation (LVR)
    // 4 4 Level flight, highest wind encountered (LVW)
    // 5 5 Ascending (ASC)
    // 6 6 Descending (DES)
    // 7 5 Ascending, observation intervals selected by time increments
    // 8 5 Ascending, observation intervals selected by time increments,
    // unsteady
    // 9 5 Ascending, observation intervals selected by pressure increments
    // 10 5 Ascending, observation intervals selected by pressure increments,
    // unsteady
    // 11 6 Descending, observation intervals selected by time increments
    // 12 6 Descending, observation intervals selected by time increments,
    // unsteady
    // 13 6 Descending, observation intervals selected by pressure increments
    // 14 6 Descending, observation intervals selected by pressure increments,
    // unsteady
    // 15 7 Missing value

    public static final int UNS_FLGT = 2;

    public static final int LVR_FLGT = 3;

    public static final int LVW_FLGT = 4;

    public static final int ASC_FLGT = 5;

    public static final int DES_FLGT = 6;

    public static final int MIS_FLGT = 7;

    public static final Comparator<ACARSRecord> ACARS_TIME_COMPARATOR = new Comparator<ACARSRecord>() {

        @Override
        public int compare(ACARSRecord o1, ACARSRecord o2) {
            return o1.getTimeObs().compareTo(o2.getTimeObs());
        }
    };

    public static final Comparator<ACARSRecord> ACARS_ASC_COMPARATOR = new Comparator<ACARSRecord>() {

        @Override
        public int compare(ACARSRecord o1, ACARSRecord o2) {
            return o1.getFlightLevel().compareTo(o2.getFlightLevel());
        }
    };

    public static final Comparator<ACARSRecord> ACARS_DES_COMPARATOR = new Comparator<ACARSRecord>() {

        @Override
        public int compare(ACARSRecord o1, ACARSRecord o2) {
            return o2.getFlightLevel().compareTo(o1.getFlightLevel());
        }
    };

    /**
     * Provides a time ordering comparator for ACARSSoundingLayer instances.
     */
    public static final Comparator<ACARSSoundingLayer> ACARS_LAYER_COMPARATOR = new Comparator<ACARSSoundingLayer>() {

        /**
         * 
         * @param layer1
         * @param layer2
         * @return
         */
        @Override
        public int compare(ACARSSoundingLayer layer1, ACARSSoundingLayer layer2) {
            return layer1.getTimeObs().compareTo(layer1.getTimeObs());
        }
    };

    
    /**
     * Don't allow this class to be instantiated.
     */
    private ACARSSoundingTools() {

    }

    /**
     * 
     * @return
     */
    public static final String getFileName() {
        return String.format(FMT, DATAFILE, TimeTools.getSystemCalendar());
    }

    /**
     * 
     * @param baseDir
     * @return An array of File references for files found in baseDir. If no
     *         files are found a non-null empty array is returned.
     */
    public static final File[] getDataFiles(File baseDir) {
        File[] files = null;
        if ((baseDir != null) && (baseDir.isDirectory() && (baseDir.exists()))) {
            // Only our files should be in here so no need to filter.
            files = baseDir.listFiles();
        }
        if (files == null) {
            files = new File[0];
        }
        return files;
    }

    /**
     * 
     * @param tailNumberFile
     * @return
     */
    public static final List<String> getAircraftData(File tailNumberFile,
            Log logger) {

        ArrayList<String> uris = new ArrayList<String>();
        BufferedReader bf = null;
        try {
            bf = new BufferedReader(new FileReader(tailNumberFile));
            String s = null;
            while ((s = bf.readLine()) != null) {
                uris.add(s);
            }
        } catch (IOException ioe) {
            logger.error("Error reading data for tailnumber "
                    + tailNumberFile.getName());
        } finally {
            if (bf != null) {
                try {
                    bf.close();
                } catch (IOException ioe) {
                    logger.error("Error closing tailnumber file "
                            + tailNumberFile.getName());
                }
            }
        }
        return uris;
    }

    /**
     * 
     * @param tailNumberFile
     * @param acarsDataURIs
     * @param logger
     */
    public static final void writeAircraftData(File tailNumberFile,
            List<String> acarsDataURIs, Log logger) {
        if (acarsDataURIs != null) {
            // Check if there is at least one not null data item to write.
            boolean writeData = false;
            for (String s : acarsDataURIs) {
                if (s != null) {
                    writeData = true;
                    break;
                }
            }
            if (writeData) {
                FileWriter writer = null;
                try {
                    writer = new FileWriter(tailNumberFile, false);
                    for (String s : acarsDataURIs) {
                        if (s != null) {
                            writer.write(String.format("%s\n", s));
                        }
                    }
                } catch (Exception e) {
                    logger.error("Error processing " + tailNumberFile.getName());
                } finally {
                    if (writer != null) {
                        try {
                            writer.close();
                        } catch (IOException ioe) {
                            logger.error("Error closing "
                                    + tailNumberFile.getName());
                        }
                    }
                }
            }
        }
    }

    /**
     * 
     * @param tailNumberFile
     * @param acarsDataURIs
     * @param dups
     * @param logger
     * @return
     */
    public static final List<ACARSRecord> readAircraftData(File tailNumberFile,
            List<ACARSRecord> obs, HashSet<String> dups, Log logger) {

        if(obs == null) {
            obs = new ArrayList<ACARSRecord>();
        }

        if(dups == null) {
            dups = new HashSet<String>();
        }
        
        if ((tailNumberFile != null) && (tailNumberFile.exists())) {
            BufferedReader bf = null;
            try {
                bf = new BufferedReader(new FileReader(tailNumberFile));
                String s = null;
                while ((s = bf.readLine()) != null) {
                    s = removeTrailingSpace(s);
                    long obsTime = parseLong(s.substring(0,20),0);
                    // Don't process if bad obstime.
                    if(obsTime > 0) {
                        String uri = s.substring(21);

                        if (!dups.contains(uri)) {
                            dups.add(uri);
                            ACARSRecord r = new ACARSRecord(uri);
                            r.setTimeObs(TimeTools.newCalendar(obsTime));
                            obs.add(r);
                        }
                    }
                }
                // All done reading
            } catch (IOException ioe) {
                logger.error("Error reading " + tailNumberFile.getName());
            } finally {
                if (bf != null) {
                    try {
                        bf.close();
                    } catch (IOException ioe) {
                        logger.error("Error attempting to close file " + tailNumberFile.getName());
                    }
                }
            }
        }
        return obs;
    }

    /**
     * Create an ACARSSoundingLayer from a given ACARSRecord.
     * 
     * @param record
     *            ACARSRecord to copy data from.
     * @return Populated ACARSSoundingLayer data. Returns a null reference if
     *         the supplied ACARSRecord is null.
     */
    public static final ACARSSoundingLayer createLayer(ACARSRecord record) {

        ACARSSoundingLayer layer = null;
        if (record != null) {

            layer = new ACARSSoundingLayer();

            layer.setTimeObs(record.getTimeObs());
            layer.setLocation(copyLocation(record.getLocation()));
            layer.setTailNumber(record.getTailNumber());
            layer.setFlightPhase(record.getFlightPhase());

            layer.setTemp(record.getTemp());
            layer.setDwpt(record.getDwpt());
            layer.setHumidity(record.getHumidity());
            layer.setMixingRatio(record.getMixingRatio());

            layer.setPressure(record.getPressure());

            layer.setWindDirection(record.getWindDirection());
            layer.setWindSpeed(record.getWindSpeed());

            layer.setIceBaseHgt(record.getIceBaseHgt());
            layer.setIceTopHgt(record.getIceTopHgt());
            layer.setIcing(record.getIcing());

            layer.setTurbBaseHgt(record.getTurbBaseHgt());
            layer.setTurbTopHgt(record.getTurbTopHgt());
            layer.setTurbulence(record.getTurbulence());

        }
        return layer;
    }

    /**
     * 
     * @param oldLoc
     * @return
     */
    public static final AircraftObsLocation copyLocation(
            AircraftObsLocation oldLoc) {

        AircraftObsLocation newLoc = null;
        if (oldLoc != null) {
            newLoc = new AircraftObsLocation();
            newLoc.setFlightLevel(oldLoc.getFlightLevel());
            newLoc.setLocation(oldLoc.getLocation());
            newLoc.setStationId(oldLoc.getStationId());
            newLoc.setLatitude(oldLoc.getLatitude());
            newLoc.setLongitude(oldLoc.getLongitude());
        }

        return newLoc;
    }

    /**
     * Find the minimum and maximum flight level altitudes in a list of ACARS
     * observations.
     * 
     * @param soundingList
     * @return
     */
    private static List<Integer> getMinMaxAltitude(
            List<ACARSRecord> soundingList) {
        Integer minAlt = Integer.MAX_VALUE;
        Integer maxAlt = Integer.MIN_VALUE;

        for (ACARSRecord rec : soundingList) {
            if ((rec != null) && (rec.getFlightLevel() != null)) {
                maxAlt = Math.max(rec.getFlightLevel(), maxAlt);
                minAlt = Math.min(rec.getFlightLevel(), minAlt);
            }
        }

        List<Integer> retValue = null;
        if (!maxAlt.equals(Integer.MIN_VALUE)
                && !minAlt.equals(Integer.MAX_VALUE)) {
            retValue = new ArrayList<Integer>();
            retValue.add(minAlt);
            retValue.add(maxAlt);
        }
        return retValue;
    }

    /**
     * Apply a variety of rules to determine if an observation should be
     * considered.
     * 
     * @param soundingList
     * @return The accepted list. If the list is null, the null list is
     *         returned. If the list fails acceptance tests, an empty list is
     *         returned.
     */
    public static final List<ACARSRecord> accept(List<ACARSRecord> soundingList) {
        // Do we have enough layers to make a sounding?
        if (soundingList != null) {
            if (soundingList.size() >= MIN_OBS_FOR_SOUNDING) {
                List<Integer> altitudes = getMinMaxAltitude(soundingList);
                if (altitudes != null) {
                    double minAlt = altitudes.get(0).doubleValue();
                    double maxAlt = altitudes.get(1).doubleValue();
                    // Is the sounding deep enough? If not, clear the list.
                    if ((maxAlt - minAlt) < MIN_DEPTH) {
                        soundingList.clear();
                    }
                }
            } else {
                soundingList.clear();
            }
        }
        return soundingList;
    }

    /**
     * 
     * @return
     */
    public static final long getCutoffTime(int cutOffHours) {
        long cTime = TimeTools.getSystemCalendar().getTimeInMillis();
        cTime -= (TimeTools.MILLIS_HOUR * cutOffHours);

        return cTime;
    }

    /**
     * Does a time tB lie between times tA and tC.
     * @param tA
     * @param tB
     * @param tC
     * @return
     */
    public static final boolean betweenTimes(Calendar tA, Calendar tB, Calendar tC) {
        return (tB.compareTo(tA) > 0)&&(tB.compareTo(tC) < 0);
    }
    
    /**
     * Does a time tB lie between times tA and tC.
     * @param tA
     * @param tB
     * @param tC
     * @return
     */
    public static final boolean betweenFlightLevel(Integer fA, Integer fB, Integer fC) {
        boolean result = (fB.compareTo(fA) > 0)&&(fB.compareTo(fC) < 0); 
        return result |= (fB.compareTo(fA) < 0)&&(fB.compareTo(fC) > 0);
    }

    /**
     * Determine if a target ACARS observation lies between two
     * given layers based on observation time and flight level. 
     * @param layerA First layer.
     * @param rec
     * @param layerB Second layer.
     * @return Does the observation lie between the given layers.
     */
    public static boolean checkBetween(ACARSSoundingLayer layerA,
            ACARSRecord rec, ACARSSoundingLayer layerB) {
        boolean result = false;
        if (ACARSSoundingTools.betweenTimes(layerA.getTimeObs(),
                rec.getTimeObs(), layerB.getTimeObs())) {
            if (ACARSSoundingTools.betweenFlightLevel(layerA.getFlightLevel(),
                    rec.getFlightLevel(), layerB.getFlightLevel())) {
                result = true;
            }
        }
        return result;
    }

    private static boolean test_betweenTimes() {
        boolean result = true;
        
        long t = System.currentTimeMillis();
        
        Calendar cA = TimeTools.newCalendar(t - 1);
        Calendar cB = TimeTools.newCalendar(t);
        Calendar cC = TimeTools.newCalendar(t + 1);

        result &= betweenTimes(cA,cB,cC);

        cA = TimeTools.newCalendar(t);
        cB = TimeTools.newCalendar(t - 1);
        cC = TimeTools.newCalendar(t + 1);
        result &= !betweenTimes(cA,cB,cC);
        
        cA = TimeTools.newCalendar(t-1);
        cB = TimeTools.newCalendar(t + 1);
        cC = TimeTools.newCalendar(t);
        result &= !betweenTimes(cA,cB,cC);

        return result;
    }
    
    private static boolean test_betweenFlightLevel() {
        boolean result = true;

        Integer [] fA = { 2000, 3000,  2500,  3000,  2500,  2000, };
        Integer [] fB = { 2500, 2500,  2000,  2000,  3000,  3000, };
        Integer [] fC = { 3000, 2000,  3000,  2500,  2000,  2500, };
        boolean [] rs = { true, true, false, false, false, false, };
        
        for(int i = 0;i < fA.length;i++) {
            result &= (betweenFlightLevel(fA[i],fB[i],fC[i]) == rs[i]);
        }
        
        return result;
    }
    
    public static final String removeTrailingSpace(String target) {
        String value = null;
        if(target != null) {
            int i = target.length()-1;
            for(;i > -1;i--) {
                if(!Character.isWhitespace(target.charAt(i))) {
                    break;
                }
            }
            if(i == -1) {
                value = new String();
            } else if(i > -1) {
                value = target.substring(0,i+1);
            }
        }
        return value;
    }
    
    /**
     * 
     * @param s
     * @param defValue
     * @return
     */
    public static final long parseLong(String s, long defValue) {
        long value = defValue;
        if(s != null) {
            try {
                value = Long.parseLong(s.trim()); 
            } catch(Exception e) {
                // nothing
            }
            
        }
        return value;
    }
    /**
     * 
     * @param file
     * @param logger
     * @return
     */
    public static final boolean deleteFile(File file, Log logger, String logMessage) {
        boolean deleted = false;
        if(file != null) {
            deleted = file.delete();
            if (!deleted) {
                logger.error(String.format(logMessage,file.getName()));
            }
        }
        return deleted;
    }
    
    /**
     * 
     * @param args
     * @return The number of milliseconds that were delayed.
     */
    public static final long delay(long time) {
        long start = System.currentTimeMillis();
        try {
            Thread.sleep(10000L);
        } catch(Exception e) {
            // nothing.
        }
        return (System.currentTimeMillis() - start);
    } // end

    public static final void main(String [] args) {
        
//        System.out.println("Test betweenTimes() passed test [" + test_betweenTimes() + "]");
//        System.out.println("Test betweenFlightLevel() passed test [" + test_betweenFlightLevel() + "]");
//        
//        String STD_TM_FMT = "%1$tY-%1$tm-%1$td %1$tH:%1$tM:%1$tS";
//        
//        Calendar c = TimeTools.getSystemCalendar();
//        System.out.println(String.format(STD_TM_FMT,c));
        
        System.out.println(removeTrailingSpace("   THIS IS A TEST   "));
        System.out.println(removeTrailingSpace("   THIS IS A TESTXXX"));
        System.out.println(removeTrailingSpace("   THIS IS A TEST  X"));
        System.out.println(removeTrailingSpace(" "));
        System.out.println(removeTrailingSpace("X"));
        
        String s = removeTrailingSpace("       1307539490000:/acars/2011-06-08_13:24:50.0/0HKEILZA/null/14.73/-90.51/2807");
        System.out.println(parseLong(s.substring(0,20),0));
        
    }
}
