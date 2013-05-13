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
package com.raytheon.edex.plugin.shef.ohdlib;

import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.plugin.shef.ohdlib.GagePPOptions.upd_action;
import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.dataplugin.shef.tables.Dailypp;
import com.raytheon.uf.common.dataplugin.shef.tables.DailyppId;
import com.raytheon.uf.common.dataplugin.shef.tables.Hourlypc;
import com.raytheon.uf.common.dataplugin.shef.tables.HourlypcId;
import com.raytheon.uf.common.dataplugin.shef.tables.Hourlypp;
import com.raytheon.uf.common.dataplugin.shef.tables.HourlyppId;
import com.raytheon.uf.common.dataplugin.shef.tables.IHourlyTS;
import com.raytheon.uf.common.dataplugin.shef.util.SHEFTimezone;
import com.raytheon.uf.common.dataplugin.shef.util.ShefConstants;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.decodertools.time.TimeTools;

/**
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 5, 2008   1649     snaples     Initial creation
 * May 7, 2013   15880    lbousaidi   changed minute_offset to offset in
 *                                    in write_1_HourValue routine.
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public final class GagePPWrite {
    private static Log logger = LogFactory.getLog(GagePPWrite.class);
    
    private char hourly_qc[] = new char[PrecipUtils.NUM_HOURLY_SLOTS];

    private char minute_offset[] = new char[PrecipUtils.NUM_HOURLY_SLOTS];

    private char sixhr_qc[] = new char[PrecipUtils.NUM_6HOURLY_SLOTS];

    private char sixhr_offset[] = new char[PrecipUtils.NUM_6HOURLY_SLOTS];

    private SimpleDateFormat sdf = null;

    /**
     * Empty constructor.
     */
    public GagePPWrite() {
        // sdf = new SimpleDateFormat(ShefConstants.POSTGRES_DATE_STRING);
        sdf = new SimpleDateFormat("yyyy-MM-dd");
        sdf.setTimeZone(SHEFTimezone.GMT_TIMEZONE);
    }

    /**
     * Returns a new hour slot based on current data. This is used to insert or
     * update an existing Hourlypp record.
     * 
     * @param datetime
     * @param id
     * @param ts
     * @param new_hourly_value
     * @param obsdate
     * @param zero_offset_code
     * @param manual_qc_code
     * @return int hour_slot numeric value of the hour slot for current value.
     */
    public int gage_pp_init(Date dtime, String id, String ts,
            double new_hourly_value, String obsdate, char zero_offset_code,
            char manual_qc_code) {


        Calendar dt = TimeTools.newCalendar(dtime.getTime());

        int hour_slot = dt.get(Calendar.HOUR_OF_DAY);
        Arrays.fill(hourly_qc, '-');
        Arrays.fill(minute_offset, '-');
        Arrays.fill(sixhr_qc, '-');
        Arrays.fill(sixhr_offset, '-');

        return hour_slot;
    }

    /**
     * Creates a new Hourlypp record or Updates existing record based on data
     * passed to it.
     * 
     * @param id
     *            gage id
     * @param ts
     *            type source
     * @param obsdate
     *            observation date
     * @param options
     *            GagePP Options
     * @param revision
     *            hourly revision code
     * @param revision_6hour
     *            six hour revision code
     * @param precip1Hour
     *            new hourly precip value
     * @param precip6Hour
     *            new six hour precip value
     */
    public int gage_pp_write_rec(Hourlypp pHourlyPP, String pe, Date obsDate,
            GagePPOptions options, short revision, short revision_6hour[],
            Short precip1Hour, Short precip6Hour, long quality_code, String qual) {

        int status = 0;

        if(precip1Hour != null) {
            status = write_1_HourValue(pHourlyPP, pe, obsDate,
                    options, revision, precip1Hour,
                    quality_code, qual);
        } else if(precip6Hour != null) {
            status = write_6_HourValue(pHourlyPP, pe, obsDate,
                    options, revision_6hour, precip6Hour,
                    quality_code, qual);
        }
        return status;
    }

    /**
     * TODO : Write_1
     * @return
     */
    private int write_1_HourValue(Hourlypp pHourlyPP, String pe, Date obsDate,
            GagePPOptions options, short revision, Short newHourlyValue,
            long qualityCode, String qual) {
        int status = 0;

        String id = pHourlyPP.getId().getLid();
        String ts = pHourlyPP.getId().getTs();

        Calendar c = getCalendar(obsDate);
        int min = c.get(Calendar.MINUTE);
        int hr = c.get(Calendar.HOUR_OF_DAY);
        
        String where = String.format(
                "WHERE lid='%s' AND ts='%s' AND obsdate ='%s'", id, ts,
                sdf.format(obsDate));

        char minOff = PrecipUtils.get_offset_code(min);
        char qcc = 'Z'; // ShefQC.buildQcSymbol(qualityCode).charAt(0);
        if((qual != null)&&(qual.length() > 0)) {
            qcc = qual.charAt(0);
        }

        PrecipUtils.setMinOffset(minute_offset, hr, minOff);
        PrecipUtils.setHourlyQC(hourly_qc, hr, qcc);

        boolean isPC = ("PC".equalsIgnoreCase(pe));

        IHourlyTS hourlyRec = getHourlyRecord(isPC, where);
        if(hourlyRec == null) {
            PersistableDataObject rec = null;
            String offset = new String(minute_offset);
            String qc = new String(hourly_qc);
            
            if (isPC) {
                Hourlypc pHourpc = new Hourlypc();
                HourlypcId pHid = new HourlypcId();
                pHid.setLid(id);
                pHid.setTs(ts);
                pHid.setObsdate(obsDate);
                pHourpc.setId(pHid);
                pHourpc.setMinuteOffset(offset);
                pHourpc.setHourlyQc(qc);
                PrecipUtils.set_hour_slot_value(pHourpc, hr, newHourlyValue);
                rec = pHourpc;
            } else {
                Hourlypp pHourpp = new Hourlypp();
                HourlyppId pHid = new HourlyppId();
                pHid.setLid(id);
                pHid.setTs(ts);
                pHid.setObsdate(obsDate);
                pHourpp.setId(pHid);
                pHourpp.setMinuteOffset(offset);
                pHourpp.setHourlyQc(qc);
                PrecipUtils.set_hour_slot_value(pHourpp, hr, newHourlyValue);
                
                pHourpp.setSixhroffset(new String(sixhr_offset));
                pHourpp.setSixhrqc(new String(sixhr_qc));
                
                rec = pHourpp;
            }
            status = update_gage_rec(rec);
        } else {
            boolean useValue = true;
            
            Short oldDataValue = PrecipUtils.get_hour_slot_value(hourlyRec, hr);
            Short newDataValue = null;
            
            char [] oldOffset = hourlyRec.getMinuteOffset().toCharArray();
            char [] offset = new char [oldOffset.length];
            System.arraycopy(oldOffset, 0, offset, 0, offset.length);
            
            char [] oldQC = hourlyRec.getHourlyQc().toCharArray();
            char [] qc = new char [oldQC.length];
            System.arraycopy(oldQC, 0, qc, 0, qc.length);
            
            if(newHourlyValue != null) {
                if (PrecipUtils.get_hour_slot_value(hourlyRec, hr) != null) {

                    int use_value = PrecipUtils.use_precip_value(newHourlyValue,
                            oldDataValue, qcc, oldQC[hr], minOff,
                            oldOffset[hr], options.getShef_duplicate().name(),
                            revision);
                    useValue = (use_value == 1);
                }
            }
            
            if(useValue) {
                newDataValue = newHourlyValue;

                PrecipUtils.setMinOffset(offset, hr, minOff);
                PrecipUtils.setHourlyQC(qc, hr, qcc);

                PersistableDataObject rec = null;
                if (isPC) {
                    Hourlypc pHourpc = (Hourlypc) hourlyRec;
                    HourlypcId pHid = new HourlypcId();
                    pHid.setLid(id);
                    pHid.setTs(ts);
                    pHid.setObsdate(obsDate);
                    pHourpc.setId(pHid);
                    pHourpc.setMinuteOffset(new String(offset));
                    pHourpc.setHourlyQc(new String(qc));
                    PrecipUtils.set_hour_slot_value(pHourpc, hr, newDataValue);
                    rec = pHourpc;
                } else {
                    Hourlypp pHourpp = (Hourlypp) hourlyRec;
                    HourlyppId pHid = new HourlyppId();
                    pHid.setLid(pHourpp.getLid());
                    pHid.setTs(pHourpp.getTs());
                    pHid.setObsdate(obsDate);
                    pHourpp.setId(pHid);

                    PrecipUtils.set_hour_slot_value(pHourpp, hr, newDataValue);
                    pHourpp.setMinuteOffset(new String(offset));
                    pHourpp.setHourlyQc(new String(qc));
                    
                    rec = pHourpp;
                }
                status = update_gage_rec(rec);
            }
        }
        return status;
    }

    /**
     * TODO : Write_6
     * @return
     */
    private int write_6_HourValue(Hourlypp pHourlyPP, String pe, Date obsDate,
            GagePPOptions options, short revision[], Short value,
            long qualityCode, String qual) {
        int status = 0;

        String id = pHourlyPP.getId().getLid();
        String ts = pHourlyPP.getId().getTs();
        

        Calendar c = getCalendar(obsDate);
        int min = c.get(Calendar.MINUTE);
        int hr = c.get(Calendar.HOUR_OF_DAY);
        
        String where = String.format(
                "WHERE lid='%s' AND ts='%s' AND obsdate ='%s'", id, ts,
                sdf.format(obsDate));
        
        char minOff = PrecipUtils.get_offset_code(min);
        char qcc = 'Z'; // ShefQC.buildQcSymbol(qualityCode).charAt(0);
        if((qual != null)&&(qual.length() > 0)) {
            qcc = qual.charAt(0);
        }
        

        // hr = PrecipUtils.getSixHourSelector(hr);
        // find out which slot actually holds the data.
        for(int i = 0;i < 4;i++) {
            if(PrecipUtils.get_6hour_slot_value(pHourlyPP,i) != null) {
                hr = i;
                break;
            }
        }
        
        sixhr_offset[hr] = minOff;
        sixhr_qc[hr] = qcc;

        boolean isPC = ("PC".equalsIgnoreCase(pe));

        IHourlyTS hourlyRec = getHourlyRecord(isPC, where);
        if (hourlyRec == null) {
            PersistableDataObject rec = null;
            if (isPC) {
                // This is an error!
                logger.error("Attempt to write six hour PC data");
            } else {
                Hourlypp pHourpp = new Hourlypp();
                HourlyppId pHid = new HourlyppId();
                pHid.setLid(id);
                pHid.setTs(ts);
                pHid.setObsdate(obsDate);
                pHourpp.setId(pHid);
                
                pHourpp.setMinuteOffset(new String(minute_offset));
                pHourpp.setHourlyQc(new String(hourly_qc));
                
                pHourpp.setSixhroffset(new String(sixhr_offset));
                pHourpp.setSixhrqc(new String(sixhr_qc));
                PrecipUtils.set_6hour_slot_value(pHourpp, hr,
                        value);
                rec = pHourpp;
            }
            status = update_gage_rec(rec);
        } else {
            Hourlypp pHourpp = (Hourlypp) hourlyRec;
            
            Short oldDataValue = PrecipUtils.get_6hour_slot_value(pHourpp, hr);
            Short newDataValue = null;
            
            char [] oldOffset = pHourpp.getSixhroffset().toCharArray();
            char [] offset = new char [oldOffset.length];
            System.arraycopy(oldOffset, 0, offset, 0, offset.length);
            
            char [] oldQC = pHourpp.getSixhrqc().toCharArray();
            char [] qc = new char [oldQC.length];
            System.arraycopy(oldQC, 0, qc, 0, qc.length);
            
            boolean useValue = true;
            // If the old value has been manually edited do not overwrite!
            if(PrecipUtils.isManualEdit(oldQC[hr]) && !PrecipUtils.isManualEdit(qcc)) {
                useValue = false;
            } else {
                // if the existing offset on record is '-' we haven't written anything yet
                if(oldOffset[hr] == '-') {
                    useValue = true;
                } else {
                    status = PrecipUtils.compare_offset_codes(oldOffset[hr], minOff);
                    if(status > 0) {
                        useValue = false;
                    } else if (status == 0) {
                        if(minOff == oldOffset[hr]) {
                            boolean rev = true;
                            if(revision[hr] == 1) {
                                rev = false;
                            }
                            String updateAction = PrecipUtils
                            .determine_update_action(
                                    options.getShef_duplicate()
                                            .name(), rev);

                            if ((upd_action.DONT_UPDATE_ACTION
                                    .isAction(updateAction))
                                    || ((upd_action.IF_DIFFERENT_UPDATE_ACTION
                                            .isAction(updateAction)) && (value.equals(oldDataValue)))) {
                                useValue = false;
                            }
                        }
                    }
                }

                if(useValue) {
                    newDataValue = value;
                    offset[hr] = minOff;
                    qc[hr] = qcc;

                    PersistableDataObject rec = null;
                    if (isPC) {
                        // This is an error!
                        logger.error("Attempt to write six hour PC data");
                    } else {
                        HourlyppId pHid = new HourlyppId();
                        pHid.setLid(pHourpp.getLid());
                        pHid.setTs(pHourpp.getTs());
                        pHid.setObsdate(obsDate);
                        pHourpp.setId(pHid);

                        PrecipUtils.set_6hour_slot_value(pHourpp, hr, newDataValue);
                        pHourpp.setSixhroffset(new String(offset));
                        pHourpp.setSixhrqc(new String(qc));
                        
                        rec = pHourpp;
                    }
                    status = update_gage_rec(rec);
                }
            }
        }        
        return status;
    }
    
    private Calendar getCalendar(Date obsDate) {
        Calendar dt = TimeTools.getSystemCalendar();
        dt.setTime(obsDate);


//        int hr = dt.get(Calendar.HOUR_OF_DAY);
//        if (hr == 0) {
//            hr = 24;
//            dt.add(Calendar.HOUR_OF_DAY, -1);
//            obsDate = dt.getTime();
//        }
        return dt;
    }

    
    private IHourlyTS getHourlyRecord(boolean isPC, String where) {
        IHourlyTS rec = null;
        
        if (isPC) {
            List<Hourlypc> hour_PC_old = PrecipUtils.getHourlyPC(where);
            if (hour_PC_old.size() >= 1) {
                rec = hour_PC_old.get(0);
            }
        } else {
            List<Hourlypp> hour_PP_old = PrecipUtils.getHourlyPP(where);
            if (hour_PP_old.size() >= 1) {
                rec = hour_PP_old.get(0);
            }
        }
        
        return rec;
    }

    // *********************************************************
    // These static methods are ok
    // *********************************************************

    /**
     * 
     * @param pDailyPP
     * @param pOptions
     * @param obsDate
     * @param rev_24hour_code
     * @param quality_code
     * @return
     */
    public static int gage_pp_write_daily_rec(Dailypp pDailyPP,
            GagePPOptions pOptions, Date obsDate, boolean rev_24hour_code,
            long quality_code, String qual) {

        SimpleDateFormat sdf = new SimpleDateFormat(
                ShefConstants.POSTGRES_DATE_STRING);
        sdf.setTimeZone(SHEFTimezone.GMT_TIMEZONE);

        int status = 0;
        boolean record_exists;

        Calendar dt = TimeTools.getSystemCalendar();
        dt.setTime(obsDate);
        dt.set(Calendar.HOUR_OF_DAY, 0);
        dt.set(Calendar.MINUTE, 0);
        dt.set(Calendar.SECOND, 0);
        dt.set(Calendar.MILLISECOND, 0);

        Date starttime = dt.getTime();
        dt.add(Calendar.DAY_OF_MONTH, 1);
        Date endtime = dt.getTime();

        char qcc = 'Z'; // ShefQC.buildQcSymbol(qualityCode).charAt(0);
        if(qual != null) {
            qcc = qual.charAt(0);
        }

        String qcsym = "Z"; // ShefQC.buildQcSymbol(quality_code);
        if((qual != null)&&(qual.length() > 0)) {
            qcsym = qual.substring(0,1);
        }
        String id = pDailyPP.getId().getLid();
        String ts = pDailyPP.getId().getTs();
        pDailyPP.setQc(qcsym);

        DailyppId pid = new DailyppId();
        pid.setLid(id);
        pid.setObstime(obsDate);
        pid.setTs(ts);
        pDailyPP.setId(pid);

        /*
         * This routine writes a record out to the DailyPP table. It tests the
         * shef_duplicate token to determine how it should handle duplicate
         * reports and revisions.
         */

        /*
         * The possible update actions are: DONT_UPDATE_ACTION, UPDATE_ACTION,
         * IF_DIFFERENT_UPDATE_ACTION
         */
        String update_action = PrecipUtils.determine_update_action(pOptions
                .getShef_duplicate().name(), rev_24hour_code);

        /*
         * Check if there is already a record in the DailyPP table for this
         * record.
         */
        /* Construct the where clause. */
        String where = ("WHERE lid='" + id + "' and ts='" + ts
                + "' and obstime >='" + starttime + "' and obstime <='"
                + endtime + "'");

        List<Dailypp> pRecord = PrecipUtils.getDailyPP(where);
        Dailypp daily_rec = null;

        record_exists = (pRecord.size() >= 1);

        if (!record_exists || upd_action.UPDATE_ACTION.isAction(update_action)) {
            status = update_gage_rec(pDailyPP);
        } else {
            if (upd_action.IF_DIFFERENT_UPDATE_ACTION.isAction(update_action)) {
                daily_rec = pRecord.get(0);
                if (daily_rec.getValue() != pDailyPP.getValue()) {
                    /* Update the record. */
                    status = update_gage_rec(pDailyPP);
                }
            }
        }

        return status;
    }

    /**
     * Updates record in the Hourlypc/pp table with new value.
     * 
     * @param obj
     *            Data object to store
     * 
     */
    public static int update_gage_rec(PersistableDataObject rec) {
        int status = -1;
        if(rec != null) {
            try {
                CoreDao dao = new CoreDao(DaoConfig.forDatabase("ihfs"));
                try {
                    dao.saveOrUpdate(rec);
                    status = GagePP.GPP_OK;
                } catch (Exception e) {
                    status = -2;
                }
            } catch (Exception e) {
                status = -1;
            }
        }
        return status;
    }

    // ------------------ exit here
    
    


//  int is_pc = 0;
//
//  String id = pHourlyPP.getId().getLid();
//  String ts = pHourlyPP.getId().getTs();
//
//  
//  
//  // Date dto = obsdate;
//  Calendar dt = TimeTools.getSystemCalendar();
//  dt.setTime(obsDate);
//  String obstime = sdf.format(obsDate);
//
//  int hr = dt.get(Calendar.HOUR_OF_DAY);
//  if (hr == 0) {
//      hr = 24;
//      dt.add(Calendar.HOUR_OF_DAY, -1);
//      obstime = sdf.format(dt.getTime());
//      obsDate = dt.getTime();
//  }
//  int min = dt.get(Calendar.MINUTE);
//
//  GagePPOptions opts = options;
//
//  char sixhroffset = PrecipUtils.get_offset_code(min);
//  char sixhrqc = ShefQC.buildQcSymbol(quality_code).charAt(0);
//  char minoff = sixhroffset;
//  char qcc = sixhrqc;
//  
//  hourly_rec = null;
//
//  String where = String
//          .format("WHERE lid='%s' AND ts='%s' AND obsdate ='%s'", id, ts,
//                  obstime);
//
//  int six = PrecipUtils.getSixHourSelector(hr);
//
//  if ("PC".equalsIgnoreCase(pe)) {
//      is_pc = 1;
//  }
//  if (is_pc == 1) {
//      List<Hourlypc> hour_PC_old = PrecipUtils.getHourlyPC(where);
//      if (hour_PC_old.size() >= 1) {
//          hourly_rec = hour_PC_old.get(0);
//      }
//  } else {
//      List<Hourlypp> hour_PP_old = PrecipUtils.getHourlyPP(where);
//      if (hour_PP_old.size() >= 1) {
//          hourly_rec = hour_PP_old.get(0);
//      }
//  }

//  if (hourly_rec == null) {
//      minute_offset[hr - 1] = minoff;
//      hourly_qc[hr - 1] = qcc;
//      sixhr_offset[six] = sixhroffset;
//      sixhr_qc[six] = sixhrqc;
//
//      if (is_pc == 1) {
//          Hourlypc pHourpc = new Hourlypc();
//          HourlypcId pHid = new HourlypcId();
//          pHid.setLid(id);
//          pHid.setTs(ts);
//          pHid.setObsdate(obsDate);
//          pHourpc.setId(pHid);
//          if(new_hourly_value != null) {
//              pHourpc.setMinuteOffset(new String(minute_offset));
//              pHourpc.setHourlyQc(new String(hourly_qc));
//              PrecipUtils.set_hour_slot_value(pHourpc, hr, new_hourly_value);
//              status = update_gage_rec(pHourpc);
//          }
//
//      } else {
//          Hourlypp pHourpp = new Hourlypp();
//          HourlyppId pHid = new HourlyppId();
//          pHid.setLid(id);
//          pHid.setTs(ts);
//          pHid.setObsdate(obsDate);
//          pHourpp.setId(pHid);
//          boolean performWrite = false;
//          if(new_hourly_value != null) {
//              pHourpp.setMinuteOffset(String.valueOf(minute_offset));
//              pHourpp.setHourlyQc(String.valueOf(hourly_qc));
//              PrecipUtils.set_hour_slot_value(pHourpp, hr, new_hourly_value);
//              performWrite = true;
//          }
//          if(pp_value != null) {
//              pHourpp.setSixhroffset(String.valueOf(sixhr_offset));
//              pHourpp.setSixhrqc(String.valueOf(sixhr_qc));
//              PrecipUtils.set_6hour_slot_value(pHourpp, six + 1, pp_value);
//              performWrite = true;
//          }
//          if(performWrite) {
//              status = update_gage_rec(pHourpp);
//          }
//      }
//
//  } else {
//
//      Short old_hr_value = -7700;
//      Short hr_value = -7700;
//      
//      char offset[] = new char[PrecipUtils.NUM_HOURLY_SLOTS];
//      System.arraycopy(minute_offset, 0, offset, 0, PrecipUtils.NUM_HOURLY_SLOTS);
//      
//      char old_offset[] = new char[PrecipUtils.NUM_HOURLY_SLOTS];
//      String mOff = hourly_rec.getMinuteOffset();
//      if(mOff != null) {
//          old_offset = mOff.toCharArray();
//      } else {
//          Arrays.fill(old_offset, '-');
//      }
//      char prev_offset = old_offset[hr - 1];
//      
//      char qc[] = new char[PrecipUtils.NUM_HOURLY_SLOTS];
//      qc = hourly_qc;
//      
//      char old_qc[] = hourly_rec.getHourlyQc().toCharArray();
//      char prev_qc = old_qc[hr - 1];
//      
//      Short six_hr_slot_val = -7700;
//      Short old_six_hr_val = -7700;
//      
//      char old_sixhroffset[] = new char[PrecipUtils.NUM_6HOURLY_SLOTS];
//      Arrays.fill(old_sixhroffset, '-');
//
//      char old_six_qc[] = new char[PrecipUtils.NUM_6HOURLY_SLOTS];
//      Arrays.fill(old_six_qc, '-');
//
//      char six_hr_qc[] = new char[PrecipUtils.NUM_6HOURLY_SLOTS];
//      System.arraycopy(sixhr_qc, 0, six_hr_qc, 0, PrecipUtils.NUM_6HOURLY_SLOTS);
//
//      
//      char prev_sixhroff = ' ';
//      char prev_sixqc = ' ';
//
//      int use_value = 1;
//      if(new_hourly_value != null) {
//          if (PrecipUtils.get_hour_slot_value(hourly_rec, hr) != null) {
//              old_hr_value = PrecipUtils.get_hour_slot_value(hourly_rec, hr);
//
//              use_value = PrecipUtils.use_precip_value(new_hourly_value,
//                      old_hr_value, qcc, prev_qc, minute_offset[hr - 1],
//                      prev_offset, opts.getShef_duplicate().name(),
//                      revision[hr - 1]);
//          }
//      }
//
//      if (use_value == 1) {
//          hr_value = new_hourly_value;
//          offset = old_offset;
//          offset[hr - 1] = minoff;
//          qc = old_qc;
//          qc[hr - 1] = qcc;
//      } else {
//          hr_value = old_hr_value;
//          offset = old_offset;
//          qc = old_qc;
//      }
//
//      Hourlypp hpp = null;
//      
//      if (use_value == 1) {
//          if (is_pc == 0) {
//              if (hourly_rec instanceof Hourlypp) {
//                  hpp = (Hourlypp) hourly_rec;
//              }
//              if (hpp != null) {
//                  if (hpp.getSixhroffset() != null) {
//                      old_sixhroffset = hpp.getSixhroffset()
//                              .toCharArray();
//                  }
//                  if (hpp.getSixhrqc() != null) {
//                      old_six_qc = hpp.getSixhrqc().toCharArray();
//                  }
//                  Short sixval = PrecipUtils.get_6hour_slot_value(hpp,
//                          six + 1);
//                  prev_sixhroff = old_sixhroffset[six];
//                  prev_sixqc = old_six_qc[six];
//
//                  if (sixval != null) {
//                      if (PrecipUtils.isManualEdit(prev_sixqc)
//                              && PrecipUtils.isManualEdit(sixhrqc)) {
//                          use_value = 0;
//                      } else {
//                          status = PrecipUtils.compare_offset_codes(
//                                  sixhr_offset[six], prev_sixhroff);
//
//                          if (status > 0) {
//                              use_value = 0;
//                          } else if ((status == 0)
//                                  && (sixhroffset == prev_sixhroff)) {
//                              boolean rev = true;
//                              if (revision_6hour[six] == 0) {
//                                  rev = false;
//                              }
//                              String update_action = PrecipUtils
//                                      .determine_update_action(
//                                              options.getShef_duplicate()
//                                                      .name(), rev);
//
//                              if ((upd_action.DONT_UPDATE_ACTION
//                                      .isAction(update_action))
//                                      || ((upd_action.IF_DIFFERENT_UPDATE_ACTION
//                                              .isAction(update_action)) && (pp_value == old_six_hr_val))) {
//                                  use_value = 0;
//                              }
//                          }
//                      }
//                  }
//                  if (use_value == 1) {
//                      six_hr_slot_val = pp_value;
//                      sixhr_offset = old_sixhroffset;
//                      sixhr_offset[six] = sixhroffset;
//                      six_hr_qc = old_six_qc;
//                      six_hr_qc[six] = sixhrqc;
//
//                  } else {
//                      if (old_six_hr_val == -7700) {
//                          old_six_hr_val = null;
//                      }
//                      six_hr_slot_val = old_six_hr_val;
//                      sixhr_offset = old_sixhroffset;
//                      six_hr_qc = old_six_qc;
//                  }
//              }
//          }
//
//          if (is_pc == 1) {
//              Hourlypc pHourpc = (Hourlypc) hourly_rec;
//              HourlypcId pHid = new HourlypcId();
//              pHid.setLid(pHourpc.getLid());
//              pHid.setTs(pHourpc.getTs());
//              pHid.setObsdate(obsDate);
//              pHourpc.setId(pHid);
//              // Only write out the data if there is a value!
//              if(hr_value != null) {
//                  PrecipUtils.set_hour_slot_value(pHourpc, hr, hr_value);
//                  pHourpc.setMinuteOffset(String.valueOf(offset));
//                  pHourpc.setHourlyQc(String.valueOf(qc));
//
//                  status = update_gage_rec(pHourpc);
//              }
//
//          } else {
//
//              Hourlypp pHourpp = hpp;
//              HourlyppId pHid = new HourlyppId();
//              pHid.setLid(pHourpp.getLid());
//              pHid.setTs(pHourpp.getTs());
//              pHid.setObsdate(obsDate);
//              pHourpp.setId(pHid);
//              boolean performWrite = false;
//              if(hr_value != null) {
//                  PrecipUtils.set_hour_slot_value(pHourpp, hr, hr_value);
//                  pHourpp.setMinuteOffset(String.valueOf(offset));
//                  pHourpp.setHourlyQc(String.valueOf(qc));
//                  performWrite = true;
//              }
//              if(six_hr_slot_val != null) {
//                  PrecipUtils.set_hour_slot_value(pHourpp, six + 1, six_hr_slot_val);
//                  pHourpp.setSixhroffset(String.valueOf(sixhr_offset));
//                  pHourpp.setSixhrqc(String.valueOf(six_hr_qc));
//                  performWrite = true;
//              }
//              if(performWrite) {
//                  status = update_gage_rec(pHourpp);
//              }
//          }
//      }
//  }

    
    
}